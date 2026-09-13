;;; session-keywords-and-hooks-spec.el --- Session head keywords + lifecycle hooks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural specs for change workspace-notes-in-vault, task
;; gptel-session-keywords-and-hooks:
;;
;; 1. Session file head (register/shape/session-file-head).  A fresh
;;    `session.org' is drawer -> `#+title: <session-id>' ->
;;    `#+filetags: :gptel_session:' -> blank line -> empty user block.
;;    The keywords are emitted by `jf/gptel--initial-session-body' and
;;    nowhere else; the drawer stays the first non-blank content, so
;;    the `magic-mode-alist' signature is unaffected
;;    (register/invariant/signature-anchored-to-point-min-drawer); the
;;    chat-mode save path rewrites only the property drawer, so a
;;    user-edited title survives real `save-buffer' round-trips.
;;
;; 2. Session-created hook (register/boundary/session-lifecycle-hooks,
;;    register/shape/session-created-hook-payload).
;;    `jf/gptel-session-created-hook' fires exactly once, as the last
;;    step of `jf/gptel--create-session-core', AFTER `session.org' (and
;;    the sibling system-prompt file) exist on disk with the org `:ID:'
;;    stamped, and BEFORE any buffer visits the file or the registry
;;    knows about it.  The payload carries `:session-file',
;;    `:session-id', `:branch', `:session-dir', `:branch-dir' and
;;    `:project-root' (nil when the caller supplied none).
;;
;; 3. Error guard.  A hook function that signals is logged through
;;    `jf/gptel--log' at level `error', does not stop the remaining
;;    functions, and does not abort creation: the session is still
;;    created, opened, and registered normally.
;;
;; 4. Direction.  The sessions package subscribes nothing on its own
;;    hooks.
;;
;; 5. `jf/gptel-branch-created-hook' is DECLARED but INERT this cycle:
;;    its runner lands with the branch identity rewrite.  The specs
;;    under that heading assert only inertness -- a green run there is
;;    NOT coverage of branch-hook behaviour.
;;
;; The creation-path scenarios (2, 3) run the REAL
;; `jf/gptel--create-session-core' against a temp root with nothing on
;; the creation path stubbed.  Because the guard under test swallows
;; every `error' -- and a failing buttercup `expect' is an error -- the
;; hook functions only RECORD what they observe; all assertions run
;; after the core returns.  Keyword-shape scenarios that need no real
;; disk use `with-captured-io' (Emacs write primitives mocked only).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Shared `with-captured-io' helper.
(add-to-list 'load-path
             (expand-file-name
              "../../../test"
              (file-name-directory (or load-file-name buffer-file-name))))
(require 'persistence-test-helpers)

;; Chat subsystem: `gptel-chat-mode' registers the content signature on
;; `magic-mode-alist' and installs the drawer-writing save hook that the
;; save-path scenario exercises for real.
(add-to-list 'load-path
             (expand-file-name
              "../../../chat"
              (file-name-directory (or load-file-name buffer-file-name))))
(require 'gptel-chat-parser)
(require 'gptel-chat-mode)

;; Production modules.
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-scope-profiles)
(require 'gptel-session-commands)

;;; ---------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------

(defvar jf-gptel-kw-test--registry-keys nil
  "Registry keys to remove in `after-each'.")

(defun jf-gptel-kw-test--count-matches (regexp string)
  "Return how many times REGEXP matches in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun jf-gptel-kw-test--file-string (file)
  "Return FILE's contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun jf-gptel-kw-test--drawer-id (file)
  "Return the `:ID:' value in FILE's head, or nil."
  (let ((content (jf-gptel-kw-test--file-string file)))
    (and (string-match "^:ID:[ \t]+\\(\\S-+\\)" content)
         (match-string 1 content))))

(defmacro jf-gptel-kw-test--with-temp-root (var &rest body)
  "Bind VAR to a fresh temp directory around BODY; delete it afterwards."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file "gptel-kw-hooks-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defmacro jf-gptel-kw-test--with-session-buffer (file buffer-var &rest body)
  "Visit FILE with `find-file-noselect', bind BUFFER-VAR, run BODY, clean up.
Binds `enable-local-variables' nil the way a user open would not be
influenced by a stray cookie.  The buffer is marked unmodified and
killed afterwards, even on error."
  (declare (indent 2) (debug (form symbolp body)))
  `(let ((enable-local-variables nil)
         (,buffer-var nil))
     (unwind-protect
         (progn
           (setq ,buffer-var (find-file-noselect ,file))
           (with-current-buffer ,buffer-var
             ,@body))
       (when (buffer-live-p ,buffer-var)
         (with-current-buffer ,buffer-var
           (set-buffer-modified-p nil))
         (kill-buffer ,buffer-var)))))

(defvar jf-gptel-kw-test--unit-hook nil
  "A throwaway hook symbol for `jf/gptel--run-lifecycle-hook' unit specs.")

;;; ---------------------------------------------------------------
;;; 1. Session file head keywords
;;; ---------------------------------------------------------------

(describe "session.org head keywords (register/shape/session-file-head)"

  (describe "jf/gptel--initial-session-body"

    (it "emits title, filetags, a blank line, then the empty user block, in that order"
      (expect (jf/gptel--initial-session-body "notes-20260913120000")
              :to-equal (concat "#+title: notes-20260913120000\n"
                                "#+filetags: :gptel_session:\n"
                                "\n"
                                "#+begin_user\n\n#+end_user\n")))

    (it "still parses to exactly one empty user turn (keywords sit outside every block)"
      (with-temp-buffer
        (insert (jf/gptel--initial-session-body "notes-20260913120000"))
        (let ((turns (gptel-chat-parse-buffer)))
          (expect (length turns) :to-equal 1)
          (expect (plist-get (car turns) :role) :to-equal 'user)
          (expect (plist-get (car turns) :content) :to-equal "\n")))))

  (describe "jf/gptel--create-session-core"

    (it "writes the keywords directly after the :ID:-bearing drawer and before the first user block"
      ;; Delta spec sessions-persistence, scenario "New session carries
      ;; title and filetag".
      (with-captured-io
        (jf/gptel--create-session-core
         "notes-20260913120000"
         "/sessions/notes-20260913120000"
         'executor)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/notes-20260913120000/branches/main/session.org")))
          (expect content :to-be-truthy)
          ;; Region 1 first, then regions 3-6 verbatim to end of file.
          (expect content :to-match "\\`:PROPERTIES:\n")
          (expect content
                  :to-match
                  (concat ":END:\n"
                          "#\\+title: notes-20260913120000\n"
                          "#\\+filetags: :gptel_session:\n"
                          "\n"
                          "#\\+begin_user\n\n#\\+end_user\n\\'"))
          ;; Exactly one of each keyword, both at column 0.
          (expect (jf-gptel-kw-test--count-matches "^#\\+title: " content)
                  :to-equal 1)
          (expect (jf-gptel-kw-test--count-matches "^#\\+filetags: " content)
                  :to-equal 1)
          ;; The keywords come after the stamped :ID: and before the
          ;; first #+begin_ line.
          (let ((id-pos (string-match "^:ID:" content))
                (title-pos (string-match "^#\\+title:" content))
                (block-pos (string-match "^#\\+begin_" content)))
            (expect id-pos :to-be-truthy)
            (expect (< id-pos title-pos) :to-be t)
            (expect (< title-pos block-pos) :to-be t)))))

    (it "leaves caller-supplied INITIAL-CONTENT byte-verbatim (no keywords injected)"
      (with-captured-io
        (jf/gptel--create-session-core
         "notes-override-20260913120000"
         "/sessions/notes-override-20260913120000"
         'executor
         "custom\nseed\n")
        (expect (captured-file-content
                 captured-files
                 "/sessions/notes-override-20260913120000/branches/main/session.org")
                :to-equal "custom\nseed\n"))))

  (describe "the signature stays anchored to the point-min drawer"

    ;; register/invariant/signature-anchored-to-point-min-drawer: the
    ;; bounded scan stops at the first :END:, so nothing after it --
    ;; the keywords, or an EDGES drawer a subscriber may insert -- is
    ;; ever read, and nothing above the drawer is tolerated.

    (it "recognises the new head and reads only the drawer's GPTEL keys"
      (with-temp-buffer
        (insert ":PROPERTIES:\n"
                ":GPTEL_PRESET: executor\n"
                ":GPTEL_SESSION_ID: s\n"
                ":GPTEL_BRANCH: main\n"
                ":ID: AAA\n"
                ":END:\n"
                "#+title: s\n"
                "#+filetags: :gptel_session:\n"
                "\n"
                "#+begin_user\n\n#+end_user\n")
        (expect (jf/gptel--session-signature-p) :to-be t)
        (expect (mapcar #'car (jf/gptel--scan-session-drawer-keys))
                :to-equal '("GPTEL_PRESET" "GPTEL_SESSION_ID" "GPTEL_BRANCH"))))

    (it "still recognises the head when an EDGES drawer sits between :END: and #+title:"
      (with-temp-buffer
        (insert ":PROPERTIES:\n"
                ":GPTEL_SESSION_ID: s\n"
                ":GPTEL_BRANCH: main\n"
                ":ID: BBB\n"
                ":END:\n"
                ":EDGES:\n"
                "- part-of :: [[id:P]]\n"
                ":END:\n"
                "#+title: s\n"
                "#+filetags: :gptel_session:\n"
                "\n"
                "#+begin_user\nhi\n#+end_user\n")
        (expect (jf/gptel--session-signature-p) :to-be t)
        (expect (mapcar #'car (jf/gptel--scan-session-drawer-keys))
                :to-equal '("GPTEL_SESSION_ID" "GPTEL_BRANCH"))))

    (it "does NOT recognise a file whose #+title: precedes the drawer"
      ;; The org-roam / vulpea habit of a line-1 title would break
      ;; activation for every session -- the keywords MUST stay below.
      (with-temp-buffer
        (insert "#+title: s\n"
                ":PROPERTIES:\n"
                ":GPTEL_SESSION_ID: s\n"
                ":END:\n")
        (expect (jf/gptel--session-signature-p) :to-be nil))))

  (describe "chat-mode save path leaves the keywords alone"

    (after-each
      (dolist (key jf-gptel-kw-test--registry-keys)
        (remhash key jf/gptel--session-registry))
      (setq jf-gptel-kw-test--registry-keys nil))

    (it "a user-edited title and the filetag survive two real save-buffer round-trips"
      ;; Delta spec sessions-persistence, scenario "User-edited title
      ;; survives saves".  Real creation, real `find-file-noselect'
      ;; (magic-mode-alist -> gptel-chat-mode -> before-save drawer
      ;; writer), real `save-buffer', re-read from disk.  Sending a
      ;; turn is replaced by a body edit between the two saves.
      (jf-gptel-kw-test--with-temp-root root
        (let* ((session-id "notes-20260913120000")
               (info (jf/gptel--create-session-core
                      session-id
                      (expand-file-name session-id root)
                      'executor))
               (session-file (plist-get info :session-file)))
          (push (jf/gptel--registry-key session-id "main")
                jf-gptel-kw-test--registry-keys)
          (jf-gptel-kw-test--with-session-buffer session-file buf
            (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
            ;; First save: user edits the title.
            (goto-char (point-min))
            (expect (re-search-forward "^#\\+title: .*$" nil t) :to-be-truthy)
            (replace-match "#+title: Retry design" t t)
            (save-buffer)
            (let ((on-disk (jf-gptel-kw-test--file-string session-file)))
              (expect on-disk :to-match "^#\\+title: Retry design$")
              (expect on-disk :to-match "^#\\+filetags: :gptel_session:$")
              (expect on-disk :to-match "\\`:PROPERTIES:\n"))
            ;; Second save: a body edit inside the user block (the
            ;; stand-in for sending a turn), then save again -- the
            ;; drawer writer runs a second time.
            (goto-char (point-min))
            (expect (re-search-forward "^#\\+begin_user\n" nil t) :to-be-truthy)
            (insert "  hello\n")
            (save-buffer)
            (let ((on-disk (jf-gptel-kw-test--file-string session-file)))
              ;; Drawer still first and still carries its identity keys
              ;; (the save path rewrites snapshot keys; identity keys
              ;; are never touched).
              (expect on-disk :to-match "\\`:PROPERTIES:\n")
              (expect on-disk :to-match "^:GPTEL_SESSION_ID: notes-20260913120000$")
              ;; Keywords intact, exactly once, still after :END: and
              ;; before the first block.
              (expect on-disk :to-match "^#\\+title: Retry design$")
              (expect on-disk :to-match "^#\\+filetags: :gptel_session:$")
              (expect (jf-gptel-kw-test--count-matches "^#\\+title: " on-disk)
                      :to-equal 1)
              (expect (jf-gptel-kw-test--count-matches "^#\\+filetags: " on-disk)
                      :to-equal 1)
              (let ((end-pos (string-match "^:END:$" on-disk))
                    (title-pos (string-match "^#\\+title:" on-disk))
                    (tags-pos (string-match "^#\\+filetags:" on-disk))
                    (block-pos (string-match "^#\\+begin_" on-disk)))
                (expect (< end-pos title-pos tags-pos block-pos) :to-be t))
              ;; And the file still activates: the signature holds.
              (with-temp-buffer
                (insert on-disk)
                (expect (jf/gptel--session-signature-p) :to-be t)))))))))

;;; ---------------------------------------------------------------
;;; 2. Session-created hook
;;; ---------------------------------------------------------------

(describe "jf/gptel-session-created-hook (register/boundary/session-lifecycle-hooks)"

  (after-each
    (dolist (key jf-gptel-kw-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-kw-test--registry-keys nil))

  (describe "un-stubbed creation path"

    (it "fires once, after session.org exists with its :ID:, before display and registration"
      ;; Delta spec scenario "Session-created hook fires with file
      ;; identity".  Nothing on the creation path is stubbed; the hook
      ;; function only records what it sees at call time.
      (jf-gptel-kw-test--with-temp-root root
        (let* ((session-id "notes-20260913120000")
               (session-dir (expand-file-name session-id root))
               (seen nil)
               (recorder
                (lambda (payload)
                  (let ((file (plist-get payload :session-file)))
                    (push (list
                           :payload payload
                           :file-exists (file-exists-p file)
                           :id (and (file-exists-p file)
                                    (jf-gptel-kw-test--drawer-id file))
                           :drawer (jf/gptel--read-session-drawer-head file)
                           :sibling-exists
                           (let ((drawer (jf/gptel--read-session-drawer-head file)))
                             (when-let* ((basename
                                          (cdr (assoc "GPTEL_SYSTEM_PROMPT_FILE"
                                                      drawer))))
                               (file-exists-p
                                (expand-file-name
                                 basename (file-name-directory file)))))
                           :visiting (get-file-buffer file)
                           :registered
                           (gethash (jf/gptel--registry-key
                                     (plist-get payload :session-id) "main")
                                    jf/gptel--session-registry))
                          seen))))
               (result nil))
          (let ((jf/gptel-session-created-hook (list recorder)))
            (setq result (jf/gptel--create-session-core
                          session-id session-dir 'executor)))
          ;; Exactly once.
          (expect (length seen) :to-equal 1)
          (let* ((obs (car seen))
                 (payload (plist-get obs :payload)))
            ;; ORDER: the file (and sibling, when the preset has one)
            ;; existed with identity + :ID: stamped ...
            (expect (plist-get obs :file-exists) :to-be t)
            (expect (plist-get obs :id) :to-match "\\S-")
            (expect (cdr (assoc "GPTEL_SESSION_ID" (plist-get obs :drawer)))
                    :to-equal session-id)
            (expect (cdr (assoc "GPTEL_BRANCH" (plist-get obs :drawer)))
                    :to-equal "main")
            (when (assoc "GPTEL_SYSTEM_PROMPT_FILE" (plist-get obs :drawer))
              (expect (plist-get obs :sibling-exists) :to-be t))
            ;; ... and nothing had displayed or registered it yet.
            (expect (plist-get obs :visiting) :to-be nil)
            (expect (plist-get obs :registered) :to-be nil)
            ;; PAYLOAD (register/shape/session-created-hook-payload):
            ;; absolute paths, identity, and the same values the core
            ;; returns.
            (expect (plist-get payload :session-file)
                    :to-equal (plist-get result :session-file))
            (expect (file-name-absolute-p (plist-get payload :session-file))
                    :to-be t)
            (expect (plist-get payload :session-id) :to-equal session-id)
            (expect (plist-get payload :branch) :to-equal "main")
            (expect (plist-get payload :session-dir)
                    :to-equal (plist-get result :session-dir))
            (expect (file-name-absolute-p (plist-get payload :session-dir))
                    :to-be t)
            (expect (plist-get payload :branch-dir)
                    :to-equal (plist-get result :branch-dir))
            ;; :project-root is PRESENT and nil when none was supplied.
            (expect (plist-member payload :project-root) :to-be-truthy)
            (expect (plist-get payload :project-root) :to-be nil))
          ;; The core still returned its plist normally.
          (expect (plist-get result :session-id) :to-equal session-id))))

    (it "passes :project-root as the canonical absolute path the drawer's GPTEL_WORK_ROOT carries"
      ;; Delta spec scenario "Programmatic creation with an explicit
      ;; project root".  A NON-canonical input proves the payload
      ;; carries the core's single expand-file-name, byte-identical to
      ;; the drawer value (register/invariant/cwd-scope-agreement).
      (with-captured-io
        (let* ((input "proj")
               (expected (expand-file-name input))
               (payloads nil))
          (let ((jf/gptel-session-created-hook
                 (list (lambda (p) (push p payloads)))))
            (jf/gptel--create-session-core
             "notes-root-20260913120000"
             "/sessions/notes-root-20260913120000"
             'executor nil nil input))
          (expect (length payloads) :to-equal 1)
          (expect (plist-get (car payloads) :project-root) :to-equal expected)
          (let ((content (captured-file-content
                          captured-files
                          "/sessions/notes-root-20260913120000/branches/main/session.org")))
            (expect content
                    :to-match (concat "^:GPTEL_WORK_ROOT: "
                                      (regexp-quote expected) "$"))))))

    (it "also fires for a caller-supplied INITIAL-CONTENT creation"
      (with-captured-io
        (let ((payloads nil))
          (let ((jf/gptel-session-created-hook
                 (list (lambda (p) (push p payloads)))))
            (jf/gptel--create-session-core
             "notes-override-20260913120000"
             "/sessions/notes-override-20260913120000"
             'executor
             "custom\nseed\n"))
          (expect (length payloads) :to-equal 1)
          (expect (plist-get (car payloads) :session-file)
                  :to-equal "/sessions/notes-override-20260913120000/branches/main/session.org")))))

  (describe "error guard"

    (it "a signalling function is logged, the next function still runs, and the session is created, opened, and registered"
      ;; Delta spec scenario "Failing hook does not break creation".
      ;; Real creation path; the "opened and registered normally" half
      ;; is the caller-side find-file the interactive command performs.
      (spy-on 'jf/gptel--log :and-call-through)
      (jf-gptel-kw-test--with-temp-root root
        (let* ((session-id "notes-20260913120000")
               (session-dir (expand-file-name session-id root))
               (second-seen nil)
               (boom (lambda (_payload) (error "boom from subscriber")))
               (recorder (lambda (payload) (push payload second-seen)))
               (result nil))
          (let ((jf/gptel-session-created-hook (list boom recorder)))
            (setq result (jf/gptel--create-session-core
                          session-id session-dir 'executor)))
          ;; Creation completed and the file is on disk.
          (expect result :to-be-truthy)
          (expect (file-exists-p (plist-get result :session-file)) :to-be t)
          ;; The second function still ran, with the same payload.
          (expect (length second-seen) :to-equal 1)
          (expect (plist-get (car second-seen) :session-file)
                  :to-equal (plist-get result :session-file))
          ;; The failure was reported at level `error' through the
          ;; module logger (which writes to *Messages*).
          (expect (cl-some
                   (lambda (args)
                     (and (eq (car args) 'error)
                          (string-match-p "boom from subscriber"
                                          (apply #'format (cdr args)))))
                   (spy-calls-all-args 'jf/gptel--log))
                  :to-be-truthy)
          ;; Opened and registered normally (caller side, stage 7).
          (push (jf/gptel--registry-key session-id "main")
                jf-gptel-kw-test--registry-keys)
          (jf-gptel-kw-test--with-session-buffer
              (plist-get result :session-file) buf
            (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
            (expect (gethash (jf/gptel--registry-key session-id "main")
                             jf/gptel--session-registry)
                    :to-be-truthy)))))

    (it "jf/gptel--run-lifecycle-hook runs every function, passes the payload, and returns nil"
      (spy-on 'jf/gptel--log)
      (let* ((calls nil)
             (payload (list :session-file "/abs/session.org" :branch "main")))
        (setq jf-gptel-kw-test--unit-hook
              (list (lambda (p) (push (cons 'first p) calls))
                    (lambda (_p) (error "middle fails"))
                    (lambda (p) (push (cons 'third p) calls))))
        (unwind-protect
            (progn
              (expect (jf/gptel--run-lifecycle-hook
                       'jf-gptel-kw-test--unit-hook payload)
                      :to-be nil)
              (expect (mapcar #'car (reverse calls)) :to-equal '(first third))
              (expect (cdr (assq 'third calls)) :to-be payload)
              (expect 'jf/gptel--log :to-have-been-called)
              (let ((args (spy-calls-args-for 'jf/gptel--log 0)))
                (expect (car args) :to-equal 'error)
                (expect (apply #'format (cdr args))
                        :to-match "jf-gptel-kw-test--unit-hook")
                (expect (apply #'format (cdr args))
                        :to-match "middle fails")))
          (setq jf-gptel-kw-test--unit-hook nil)))))

  (describe "direction"

    (it "the sessions package registers nothing on its own hooks"
      ;; Only the integration layer may subscribe.  Whatever is on the
      ;; global hooks at test time (possibly nothing) must not be a
      ;; function defined under config/gptel/sessions/.
      (dolist (hook '(jf/gptel-session-created-hook
                      jf/gptel-branch-created-hook))
        (dolist (fn (default-value hook))
          (when (and (symbolp fn) (not (eq fn t)))
            (let ((file (symbol-file fn 'defun)))
              (when file
                (expect file :not :to-match "/gptel/sessions/")))))))))

;;; ---------------------------------------------------------------
;;; 3. Branch-created hook: declared, inert this cycle
;;; ---------------------------------------------------------------

(describe "jf/gptel-branch-created-hook (declared but NOT yet run)"

  ;; GREEN-ON-EMPTY WARNING.  The variable is declared this cycle so
  ;; the public hook surface lives in one place; its runner lands in
  ;; `jf/gptel--create-branch-session' with the branch identity rewrite
  ;; (task gptel-branch-identity).  These specs assert INERTNESS only.
  ;; A passing run here says nothing about branch-hook behaviour --
  ;; the positive specs belong in config/gptel/sessions/test/branching/
  ;; once the runner exists.

  (it "is declared with a nil default"
    (expect (boundp 'jf/gptel-branch-created-hook) :to-be t)
    (expect (default-value 'jf/gptel-branch-created-hook) :to-be nil))

  (it "is NOT run by session creation"
    (with-captured-io
      (let ((called nil))
        (let ((jf/gptel-branch-created-hook
               (list (lambda (_p) (setq called t)))))
          (jf/gptel--create-session-core
           "notes-inert-20260913120000"
           "/sessions/notes-inert-20260913120000"
           'executor))
        (expect called :to-be nil)))))

(provide 'session-keywords-and-hooks-spec)
;;; session-keywords-and-hooks-spec.el ends here
