;;; menu-spec.el --- org-graph menu module tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural tests for the org-graph menu module (menu-module task;
;; design D4 / D6 / D7 / D8; specs/org-graph-menu):
;;
;;   - the `org-graph-menu' transient prefix exists with all four groups
;;     and every entry bound to its command -- and, because transient
;;     layout membership checks pass even for VOID symbols, every
;;     dispatched suffix command is additionally asserted `fboundp' +
;;     `commandp' (the green-on-empty guard),
;;   - `org-graph-menu--note-id-at-point' resolves the enclosing
;;     heading :ID: first, then the file-level :ID:, and signals a
;;     `user-error' when nothing at point carries one
;;     (register/boundary/note-at-point-resolution),
;;   - the edge-query commands render stubbed query rows into the shared
;;     read-only org buffer "*org-graph-edges*" as
;;     "- <rel> :: [[id:<uuid>][<title>]]" items, titles from the edge
;;     plist's already-resolved :note slot with the raw far-end id as
;;     fallback (register/shape/edge-results-buffer),
;;   - the validate front door delegates to the schemas module
;;     (note-at-point path and prompt-for-type path; no validation logic
;;     of its own),
;;   - the SPC v install is wrapped in `with-eval-after-load' 'evil so an
;;     evil-less load stays clean
;;     (register/invariant/spc-v-binding-evil-guarded).
;;
;; All DB and UI boundaries are stubbed function-scoped via `cl-letf'
;; (`org-graph-query/*', `vulpea-db-get-by-id', `pop-to-buffer',
;; `completing-read'); no SQLite DB and no window management is touched.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'seq)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  ;; helpers add vulpea to load-path and require it.
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  ;; The menu dispatches commands owned by schemas, query, finders,
  ;; authoring, and discovery; load them by path (basename != feature)
  ;; in canonical-order-compatible sequence so the commandp assertions
  ;; hold even in a lean process.  Under the `make' runner init.el has
  ;; already loaded all of org-graph, making these requires no-ops.
  (require 'org-graph-schemas (expand-file-name "schemas.el" module-dir))
  (require 'org-graph-query (expand-file-name "query.el" module-dir))
  (require 'org-graph-finders (expand-file-name "finders.el" module-dir))
  (require 'org-graph-authoring (expand-file-name "authoring.el" module-dir))
  (require 'org-graph-discovery (expand-file-name "discovery.el" module-dir))
  (require 'org-graph-menu (expand-file-name "menu.el" module-dir)))

;; Owned by the loader defcustom; declared special so a lean process
;; falls back cleanly (the schemas module guards with boundp).
(defvar org-graph-note-types)

(defconst org-graph-menu-spec--module-dir
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of config/org-graph/, for source-level assertions.")

(defconst org-graph-menu-spec--bindings
  '(;; Find
    ("t" . org-graph/find-topic)
    ("d" . org-graph/find-debug)
    ("l" . org-graph/find-log)
    ("r" . org-graph/find-reference)
    ("p" . org-graph/find-project)
    ("a" . org-graph/find-any)
    ("D" . org-graph/find-agent-drafts)
    ;; Author
    ("f" . org-graph/find-or-create)
    ("i" . org-graph/insert-link)
    ;; Edges (at point)
    ("o" . org-graph/edges-outgoing-at-point)
    ("n" . org-graph/edges-incoming-at-point)
    ("c" . org-graph/edges-connected-at-point)
    ;; Maintain
    ("s" . org-graph/configure-sync)
    ("v" . org-graph/validate-note-at-point-or-prompt)
    ("h" . vulpea-doctor))
  "Every (KEY . COMMAND) entry the org-graph-menu prefix must carry.")

(defun org-graph-menu-spec--suffix-command (suffix)
  "Extract the :command symbol from a transient SUFFIX spec.
Tolerates both layout shapes transient has used: the current
\(CLASS . PLIST) cons and the legacy (LEVEL CLASS PLIST) list."
  (or (plist-get (cdr-safe suffix) :command)
      (and (listp suffix)
           (plist-get (nth 2 suffix) :command))))

(defmacro org-graph-menu-spec--in-org-buffer (text &rest body)
  "Evaluate BODY in a temp `org-mode' buffer containing TEXT.
Point ends up at `point-max'.  Mode hooks are suppressed so the test
process's org hooks (evil-org etc.) cannot perturb the buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (let ((org-mode-hook nil)
           (org-element-use-cache nil))
       (delay-mode-hooks (org-mode)))
     (goto-char (point-max))
     ,@body))

(defun org-graph-menu-spec--edges-buffer-string ()
  "Return the current contents of the shared edges buffer."
  (with-current-buffer org-graph-menu-edges-buffer-name
    (buffer-string)))

(describe "org-graph menu module"

  (describe "org-graph-menu transient prefix"

    (it "is an interactive command with a transient layout"
      (expect (commandp 'org-graph-menu) :to-be-truthy)
      (expect (get 'org-graph-menu 'transient--layout) :to-be-truthy))

    (it "shows the four groups: Find, Author, Edges (at point), Maintain"
      (let ((layout (format "%S" (get 'org-graph-menu 'transient--layout))))
        (dolist (group '("Find" "Author" "Edges (at point)" "Maintain"))
          (expect layout :to-match (regexp-quote group)))))

    (it "binds every entry key to its command"
      (pcase-dolist (`(,key . ,command) org-graph-menu-spec--bindings)
        (let ((suffix (transient-get-suffix 'org-graph-menu key)))
          (expect suffix :to-be-truthy)
          (expect (org-graph-menu-spec--suffix-command suffix)
                  :to-be command))))

    (it "dispatches only commands that are fbound and interactive (green-on-empty guard)"
      ;; Layout membership alone passes for void symbols; assert every
      ;; dispatched target actually exists as an interactive command.
      (pcase-dolist (`(,_key . ,command) org-graph-menu-spec--bindings)
        (expect (fboundp command) :to-be-truthy)
        (expect (commandp command) :to-be-truthy))))

  (describe "org-graph-menu--note-id-at-point"

    (it "returns the enclosing heading's :ID: first"
      (org-graph-menu-spec--in-org-buffer
          (concat ":PROPERTIES:\n:ID: file-id\n:END:\n"
                  "#+title: Subject\n"
                  "* Heading\n:PROPERTIES:\n:ID: head-id\n:END:\nbody text\n")
        (expect (org-graph-menu--note-id-at-point) :to-equal "head-id")))

    (it "falls back to the file-level :ID: (inherited lookup)"
      (org-graph-menu-spec--in-org-buffer
          (concat ":PROPERTIES:\n:ID: file-id\n:END:\n"
                  "#+title: Subject\n"
                  "* Heading without id\nbody text\n")
        (expect (org-graph-menu--note-id-at-point) :to-equal "file-id")))

    (it "signals user-error when nothing at point carries an :ID:"
      (org-graph-menu-spec--in-org-buffer "* Heading\nplain body, no ids\n"
        (expect (org-graph-menu--note-id-at-point)
                :to-throw 'user-error '("No note with an :ID: at point"))))

    (it "returns nil instead of signaling with NOERROR"
      (org-graph-menu-spec--in-org-buffer "plain text, no ids\n"
        (expect (org-graph-menu--note-id-at-point 'noerror) :to-be nil))))

  (describe "edge-query commands"

    (after-each
      (when (get-buffer org-graph-menu-edges-buffer-name)
        (kill-buffer org-graph-menu-edges-buffer-name)))

    (it "renders outgoing edges as one followable list item per edge"
      (let* ((resolved (org-graph-test/note-fixture
                        :id "far-1" :title "Graph spike"))
             (edges (list (list :from "subj" :rel 'implements
                                :to "far-1" :note resolved)
                          ;; unresolvable far end: raw id as description
                          (list :from "subj" :rel 'relates-to
                                :to "far-2" :note nil)))
             queried)
        (cl-letf (((symbol-function 'org-graph-query/outgoing)
                   (lambda (id &optional _rel) (setq queried id) edges))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buf &rest _) buf)))
          (org-graph-menu-spec--in-org-buffer
              ":PROPERTIES:\n:ID: subj\n:END:\n#+title: Subject\n"
            (org-graph/edges-outgoing-at-point)))
        (expect queried :to-equal "subj")
        (let ((rendered (org-graph-menu-spec--edges-buffer-string)))
          (expect rendered :to-match (regexp-quote "* Outgoing"))
          (expect rendered
                  :to-match
                  (regexp-quote "- implements :: [[id:far-1][Graph spike]]"))
          (expect rendered
                  :to-match
                  (regexp-quote "- relates-to :: [[id:far-2][far-2]]")))))

    (it "leaves the buffer read-only with org-mode active (links followable)"
      (cl-letf (((symbol-function 'org-graph-query/outgoing)
                 (lambda (&rest _) nil))
                ((symbol-function 'pop-to-buffer)
                 (lambda (buf &rest _) buf)))
        (org-graph-menu-spec--in-org-buffer
            ":PROPERTIES:\n:ID: subj\n:END:\n"
          (org-graph/edges-outgoing-at-point)))
      (with-current-buffer org-graph-menu-edges-buffer-name
        (expect buffer-read-only :to-be-truthy)
        (expect (derived-mode-p 'org-mode) :to-be-truthy)))

    (it "renders incoming edges with the SOURCE as the far end"
      (let ((edges (list (list :from "src-1" :rel 'supersedes
                               :to "subj"
                               :note (org-graph-test/note-fixture
                                      :id "src-1" :title "Older note")))))
        (cl-letf (((symbol-function 'org-graph-query/incoming)
                   (lambda (_id &optional _rel) edges))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buf &rest _) buf)))
          (org-graph-menu-spec--in-org-buffer
              ":PROPERTIES:\n:ID: subj\n:END:\n"
            (org-graph/edges-incoming-at-point)))
        (let ((rendered (org-graph-menu-spec--edges-buffer-string)))
          (expect rendered :to-match (regexp-quote "* Incoming"))
          (expect rendered
                  :to-match
                  (regexp-quote "- supersedes :: [[id:src-1][Older note]]")))))

    (it "renders connected as one section per direction"
      (cl-letf (((symbol-function 'org-graph-query/outgoing)
                 (lambda (_id &optional _rel)
                   (list (list :from "subj" :rel 'implements :to "out-1"
                               :note (org-graph-test/note-fixture
                                      :id "out-1" :title "Out")))))
                ((symbol-function 'org-graph-query/incoming)
                 (lambda (_id &optional _rel)
                   (list (list :from "in-1" :rel 'contradicts :to "subj"
                               :note (org-graph-test/note-fixture
                                      :id "in-1" :title "In")))))
                ((symbol-function 'pop-to-buffer)
                 (lambda (buf &rest _) buf)))
        (org-graph-menu-spec--in-org-buffer
            ":PROPERTIES:\n:ID: subj\n:END:\n"
          (org-graph/edges-connected-at-point)))
      (let ((rendered (org-graph-menu-spec--edges-buffer-string)))
        (expect rendered :to-match (regexp-quote "* Outgoing"))
        (expect rendered :to-match (regexp-quote "* Incoming"))
        (expect rendered
                :to-match (regexp-quote "- implements :: [[id:out-1][Out]]"))
        (expect rendered
                :to-match (regexp-quote "- contradicts :: [[id:in-1][In]]"))))

    (it "replaces the previous render (single shared buffer)"
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buf &rest _) buf)))
        (cl-letf (((symbol-function 'org-graph-query/outgoing)
                   (lambda (&rest _)
                     (list (list :from "a" :rel 'implements :to "old-far"
                                 :note nil)))))
          (org-graph-menu-spec--in-org-buffer
              ":PROPERTIES:\n:ID: a\n:END:\n"
            (org-graph/edges-outgoing-at-point)))
        (cl-letf (((symbol-function 'org-graph-query/outgoing)
                   (lambda (&rest _) nil)))
          (org-graph-menu-spec--in-org-buffer
              ":PROPERTIES:\n:ID: b\n:END:\n"
            (org-graph/edges-outgoing-at-point))))
      (let ((rendered (org-graph-menu-spec--edges-buffer-string)))
        (expect rendered :not :to-match (regexp-quote "old-far"))
        (expect rendered :to-match (regexp-quote "No edges."))))

    (it "signals user-error (no query) when point has no :ID:"
      (let (queried)
        (cl-letf (((symbol-function 'org-graph-query/outgoing)
                   (lambda (&rest args) (setq queried args) nil)))
          (org-graph-menu-spec--in-org-buffer "no ids here\n"
            (expect (org-graph/edges-outgoing-at-point)
                    :to-throw 'user-error)))
        ;; the guard fired BEFORE the query: never query a nil/empty id
        (expect queried :to-be nil))))

  (describe "org-graph/validate-note-at-point-or-prompt"

    (it "validates the note at point via org-graph/validate-note-type"
      (let* ((note (org-graph-test/note-fixture :id "subj" :title "Subject"))
             validated messaged)
        (cl-letf (((symbol-function 'vulpea-db-get-by-id)
                   (lambda (id) (and (equal id "subj") note)))
                  ((symbol-function 'org-graph/validate-note-type)
                   (lambda (n) (setq validated n) nil))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq messaged (apply #'format fmt args)))))
          (org-graph-menu-spec--in-org-buffer
              ":PROPERTIES:\n:ID: subj\n:END:\n#+title: Subject\n"
            (org-graph/validate-note-at-point-or-prompt)))
        (expect validated :to-be note)
        (expect messaged :to-match "conforms")))

    (it "reports violations in the summary"
      (let* ((note (org-graph-test/note-fixture :id "subj" :title "Subject"))
             messaged)
        (cl-letf (((symbol-function 'vulpea-db-get-by-id)
                   (lambda (_id) note))
                  ((symbol-function 'org-graph/validate-note-type)
                   (lambda (_n) '(violation-1 violation-2)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq messaged (apply #'format fmt args)))))
          (org-graph-menu-spec--in-org-buffer
              ":PROPERTIES:\n:ID: subj\n:END:\n"
            (org-graph/validate-note-at-point-or-prompt)))
        (expect messaged :to-match (regexp-quote "2 violation(s)"))))

    (it "prompts for a type and validates all of it when point has no note"
      (let (validated-type)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "topic"))
                  ((symbol-function 'org-graph/validate-all-of-type)
                   (lambda (type) (setq validated-type type) nil))
                  ((symbol-function 'message) #'ignore))
          (org-graph-menu-spec--in-org-buffer "no ids here\n"
            (org-graph/validate-note-at-point-or-prompt)))
        (expect validated-type :to-be 'topic)))

    (it "signals user-error when the :ID: at point is not in the index"
      (cl-letf (((symbol-function 'vulpea-db-get-by-id)
                 (lambda (_id) nil)))
        (org-graph-menu-spec--in-org-buffer
            ":PROPERTIES:\n:ID: ghost\n:END:\n"
          (expect (org-graph/validate-note-at-point-or-prompt)
                  :to-throw 'user-error)))))

  (describe "SPC v leader binding (register/invariant/spc-v-binding-evil-guarded)"

    (it "wraps the install in with-eval-after-load 'evil (source guard)"
      ;; The guard is what keeps an evil-less load clean: the form is
      ;; inert until evil loads.  Assert it at source level so the
      ;; invariant holds regardless of whether THIS process has evil.
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name "menu.el" org-graph-menu-spec--module-dir))
        (expect (buffer-string)
                :to-match "with-eval-after-load 'evil")))

    (it "binds SPC v to org-graph-menu when evil is present, M-x otherwise"
      (if (featurep 'evil)
          ;; evil-define-key 'normal 'global lands in the global normal
          ;; state map.
          (expect (lookup-key evil-normal-state-map (kbd "<SPC> v"))
                  :to-be 'org-graph-menu)
        ;; evil absent: the module still loaded cleanly (we are running)
        ;; and the menu degrades to M-x org-graph-menu.
        (expect (commandp 'org-graph-menu) :to-be-truthy)))))

(provide 'menu-spec)
;;; menu-spec.el ends here
