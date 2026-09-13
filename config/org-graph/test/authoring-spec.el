;;; authoring-spec.el --- Authoring command delegation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Specs for the org-graph authoring module.
;;
;; 1. Delegation tests for the human-side authoring commands
;;    (authoring-module task, design D3 / D9,
;;    `register/boundary/authoring-commands-delegation').  The commands
;;    are THIN wrappers over vulpea 2.4 built-ins -- no custom
;;    completion, creation, or link-insertion logic -- so these specs
;;    assert exactly that: each command hands off to its vulpea entry
;;    point with the right arguments, and nothing more.
;;
;; 2. Behavioural specs for the programmatic edge writer
;;    `org-graph/write-edge' (workspace-notes-in-vault task
;;    og-edge-writer-and-project-note; `register/boundary/edge-writer-api',
;;    `register/invariant/edge-drawer-immediately-after-property-drawer',
;;    `register/invariant/write-edge-idempotent',
;;    `register/invariant/coordinator-lock-contract',
;;    `register/invariant/edge-drawer-discriminator').  These run the
;;    real writer over real files in a temp directory; only the vulpea
;;    DB boundary (`vulpea-db-update-file', and `emacsql' for the
;;    round-trip) is stubbed, and the coordinator's acquire path is
;;    spied on (call-through) so the lock contract is asserted, not
;;    mocked away.
;;
;; 3. Specs for `org-graph/create-project-note'
;;    (`register/boundary/project-note-creation'): the delegation shape
;;    via a captured `vulpea-create', and the on-disk layout via the REAL
;;    `vulpea-create' with the DB / org-id side effects stubbed -- then
;;    parsed back with vulpea's real parser to prove the `project' schema
;;    predicate and validator accept the note (the `status' META
;;    decision).
;;
;; Pattern: `cl-letf' for scoped stubs (mirroring finders-spec.el),
;; buttercup spies for call assertions, temp directories for files.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  ;; helpers-spec adds vulpea to `load-path' and requires it, which
  ;; authoring.el's hard `(require 'vulpea)' needs in a lean process.
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  ;; authoring.el requires the extractor (drawer name + normaliser) and the
  ;; coordinator (file lock) features; load them by filename first so this
  ;; spec also runs in a standalone process.  schemas + finders back the
  ;; project-note predicate assertions.
  (require 'org-graph-extractor (expand-file-name "extractor.el" module-dir))
  (require 'org-graph-coordinator (expand-file-name "coordinator.el" module-dir))
  (require 'org-graph-schemas (expand-file-name "schemas.el" module-dir))
  (require 'org-graph-finders (expand-file-name "finders.el" module-dir))
  (require 'org-graph-authoring (expand-file-name "authoring.el" module-dir)))

;; Owned by the loader's defcustoms (not loaded standalone); declared
;; special HERE so this file's `let' bindings of them are dynamic.  A
;; value-less `defvar' only marks the symbol special for the rest of the
;; file it appears in, so helpers-spec's own declarations do not carry
;; over -- each spec file must declare what it binds.
(defvar org-graph-roam-root)
(defvar org-graph-vault-root)
(defvar org-graph-note-types)
(defvar org-graph-edge-drawer)

;;; Thin vulpea wrappers ---------------------------------------------------

(describe "org-graph authoring commands (thin vulpea wrappers)"

  (describe "org-graph/find-or-create"

    (it "is an interactive command"
      (expect (commandp #'org-graph/find-or-create) :to-be-truthy))

    (it "delegates to vulpea-find with :require-match nil"
      (let* ((capture (org-graph-test/capture-call
                       'vulpea-find #'org-graph/find-or-create))
             (args (cdr capture)))
        (expect (car capture) :to-be-truthy)
        (expect (plist-member args :require-match) :to-be-truthy)
        (expect (plist-get args :require-match) :to-be nil)))

    (it "passes no :filter-fn -- completion covers every indexed note"
      (let ((args (cdr (org-graph-test/capture-call
                        'vulpea-find #'org-graph/find-or-create))))
        (expect (plist-member args :filter-fn) :to-be nil))))

  (describe "org-graph/insert-link"

    (it "is an interactive command"
      (expect (commandp #'org-graph/insert-link) :to-be-truthy))

    (it "delegates to vulpea-insert with no arguments"
      (let ((capture (org-graph-test/capture-call
                      'vulpea-insert #'org-graph/insert-link)))
        (expect (car capture) :to-be-truthy)
        (expect (cdr capture) :to-be nil)))))

;;; Scaffolding for the file-based specs ----------------------------------

(defvar org-graph-authoring-test--dir nil
  "Per-spec temp directory holding the fixture files.")

(defun org-graph-authoring-test--write (name content)
  "Write CONTENT to NAME under the spec temp dir; return the absolute path."
  (let ((path (expand-file-name name org-graph-authoring-test--dir)))
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) path nil 'silent))
    path))

(defun org-graph-authoring-test--read (path)
  "Return PATH's decoded contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun org-graph-authoring-test--bytes (path)
  "Return PATH's raw bytes (no decoding) as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defmacro org-graph-authoring-test--with-edges (&rest body)
  "Run BODY with the edge-drawer name bound to the default \"EDGES\"."
  (declare (indent 0) (debug (body)))
  `(let ((org-graph-edge-drawer "EDGES"))
     ,@body))

;; A multi-node fixture: file-level node f1 (with a title carrying
;; non-ASCII text so byte identity is meaningful), a body id: link that
;; must NOT count as an edge, and an ID'd heading h1.
(defconst org-graph-authoring-test--head
  ":PROPERTIES:\n:ID:       f1\n:END:\n"
  "The file-level property drawer of the fixture.")

(defconst org-graph-authoring-test--tail
  (concat "#+title: Note — ü\n"
          "#+filetags: :topic:\n"
          "\n"
          "Body with [[id:P]] link.\n"
          "\n"
          "* Heading\n"
          ":PROPERTIES:\n:ID:       h1\n:END:\n"
          "Heading body.\n")
  "Everything after the file-level property drawer of the fixture.")

(defconst org-graph-authoring-test--note
  (concat org-graph-authoring-test--head org-graph-authoring-test--tail)
  "The complete fixture note.")

;;; org-graph/write-edge ---------------------------------------------------

(describe "org-graph/write-edge"
  :var (file)

  (before-each
    (setq org-graph-authoring-test--dir
          (make-temp-file "org-graph-authoring-" t))
    (setq file (org-graph-authoring-test--write
                "note.org" org-graph-authoring-test--note))
    ;; DB boundary: reindex is asserted, never run.
    (spy-on 'vulpea-db-update-file :and-return-value 1)
    ;; Lock contract: call through so the real lock is taken and released.
    (spy-on 'org-graph-coordinator--acquire :and-call-through))

  (after-each
    (when (and org-graph-authoring-test--dir
               (file-directory-p org-graph-authoring-test--dir))
      (delete-directory org-graph-authoring-test--dir t))
    (setq org-graph-authoring-test--dir nil))

  (describe "placement (edge-drawer-immediately-after-property-drawer)"

    (it "creates the EDGES drawer on the line after the file-level :END:, before #+title:, rest byte-identical"
      (let ((before (org-graph-authoring-test--bytes file)))
        (org-graph-authoring-test--with-edges
          (org-graph/write-edge file "part-of" "P"))
        (let ((drawer ":EDGES:\n- part-of :: [[id:P]]\n:END:\n")
              (head-len (length org-graph-authoring-test--head)))
          (expect (org-graph-authoring-test--bytes file)
                  :to-equal (concat (substring before 0 head-len)
                                    drawer
                                    (substring before head-len))))))

    (it "targets the heading's property drawer when NODE-ID names a heading"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "part-of" "P" "h1"))
      (let* ((heading-drawer "* Heading\n:PROPERTIES:\n:ID:       h1\n:END:\n")
             (split (+ (string-search heading-drawer org-graph-authoring-test--note)
                       (length heading-drawer))))
        (expect (org-graph-authoring-test--read file)
                :to-equal (concat (substring org-graph-authoring-test--note 0 split)
                                  ":EDGES:\n- part-of :: [[id:P]]\n:END:\n"
                                  (substring org-graph-authoring-test--note split)))))

    (it "resolves the file-level ID itself to the file-level node"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "part-of" "P" "f1"))
      (expect (org-graph-authoring-test--read file)
              :to-equal (concat org-graph-authoring-test--head
                                ":EDGES:\n- part-of :: [[id:P]]\n:END:\n"
                                org-graph-authoring-test--tail)))

    (it "extends an existing drawer in place, after its last item"
      (let ((existing (concat org-graph-authoring-test--head
                              ":EDGES:\n- relates-to :: [[id:Q]]\n:END:\n"
                              org-graph-authoring-test--tail)))
        (setq file (org-graph-authoring-test--write "existing.org" existing))
        (org-graph-authoring-test--with-edges
          (org-graph/write-edge file "part-of" "P"))
        (expect (org-graph-authoring-test--read file)
                :to-equal (concat org-graph-authoring-test--head
                                  ":EDGES:\n- relates-to :: [[id:Q]]\n- part-of :: [[id:P]]\n:END:\n"
                                  org-graph-authoring-test--tail))))

    (it "matches an existing drawer name case-insensitively"
      (let ((existing (concat org-graph-authoring-test--head
                              ":edges:\n- relates-to :: [[id:Q]]\n:END:\n"
                              org-graph-authoring-test--tail)))
        (setq file (org-graph-authoring-test--write "lower.org" existing))
        (org-graph-authoring-test--with-edges
          (org-graph/write-edge file "part-of" "P"))
        (expect (org-graph-authoring-test--read file)
                :to-equal (concat org-graph-authoring-test--head
                                  ":edges:\n- relates-to :: [[id:Q]]\n- part-of :: [[id:P]]\n:END:\n"
                                  org-graph-authoring-test--tail))))

    (it "takes the drawer name from the extractor's resolver, not a literal"
      (let ((org-graph-edge-drawer "LINKS"))
        (org-graph/write-edge file "part-of" "P"))
      (expect (org-graph-authoring-test--read file)
              :to-equal (concat org-graph-authoring-test--head
                                ":LINKS:\n- part-of :: [[id:P]]\n:END:\n"
                                org-graph-authoring-test--tail)))

    (it "handles a property drawer whose :END: is the last line without a newline"
      (setq file (org-graph-authoring-test--write
                  "bare.org" ":PROPERTIES:\n:ID: f1\n:END:"))
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "part-of" "P"))
      (expect (org-graph-authoring-test--read file)
              :to-equal ":PROPERTIES:\n:ID: f1\n:END:\n:EDGES:\n- part-of :: [[id:P]]\n:END:\n")))

  (describe "idempotency (write-edge-idempotent)"

    (it "returns the appended item on the first call and nil on the duplicate"
      (org-graph-authoring-test--with-edges
        (expect (org-graph/write-edge file "part-of" "P")
                :to-equal "- part-of :: [[id:P]]\n")
        (expect (org-graph/write-edge file "part-of" "P") :to-be nil)))

    (it "leaves the file byte-identical and does not reindex on the second call"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "part-of" "P")
        (let ((after-first (org-graph-authoring-test--bytes file)))
          (org-graph/write-edge file "part-of" "P")
          (expect (org-graph-authoring-test--bytes file) :to-equal after-first)
          (expect (spy-calls-count 'vulpea-db-update-file) :to-equal 1))))

    (it "treats \"Part_of\" and part-of as the same edge (normalised comparison)"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "part-of" "P")
        (org-graph/write-edge file "Part_of" "P")
        (org-graph/write-edge file 'part-of "P"))
      (expect (org-graph-authoring-test--read file)
              :to-equal (concat org-graph-authoring-test--head
                                ":EDGES:\n- part-of :: [[id:P]]\n:END:\n"
                                org-graph-authoring-test--tail)))

    (it "ignores the link description when comparing"
      (let ((existing (concat org-graph-authoring-test--head
                              ":EDGES:\n- part-of :: [[id:P][Project P]]\n:END:\n"
                              org-graph-authoring-test--tail)))
        (setq file (org-graph-authoring-test--write "described.org" existing))
        (org-graph-authoring-test--with-edges
          (expect (org-graph/write-edge file "part-of" "P") :to-be nil))
        (expect (org-graph-authoring-test--read file) :to-equal existing)))

    (it "appends a different target or relation to the same node"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "part-of" "P")
        (org-graph/write-edge file "part-of" "P2")
        (org-graph/write-edge file "branched-from" "P"))
      (expect (org-graph-authoring-test--read file)
              :to-equal (concat org-graph-authoring-test--head
                                ":EDGES:\n- part-of :: [[id:P]]\n- part-of :: [[id:P2]]\n- branched-from :: [[id:P]]\n:END:\n"
                                org-graph-authoring-test--tail)))

    (it "only de-duplicates against the node's own edge drawer -- body links and other drawers do not count"
      (let ((existing (concat org-graph-authoring-test--head
                              ":NOTES:\n- part-of :: [[id:P]]\n:END:\n"
                              org-graph-authoring-test--tail)))
        (setq file (org-graph-authoring-test--write "other-drawer.org" existing))
        (org-graph-authoring-test--with-edges
          (expect (org-graph/write-edge file "part-of" "P") :not :to-be nil))
        (expect (org-graph-authoring-test--read file)
                :to-equal (concat org-graph-authoring-test--head
                                  ":EDGES:\n- part-of :: [[id:P]]\n:END:\n"
                                  ":NOTES:\n- part-of :: [[id:P]]\n:END:\n"
                                  org-graph-authoring-test--tail)))))

  (describe "relation normalisation (relation-types)"

    (it "emits the normalised tag for free-text and symbol TYPE"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "  Follows Up " "P")
        (org-graph/write-edge file 'BRANCHED_FROM "Q"))
      (expect (org-graph-authoring-test--read file)
              :to-equal (concat org-graph-authoring-test--head
                                ":EDGES:\n- follows-up :: [[id:P]]\n- branched-from :: [[id:Q]]\n:END:\n"
                                org-graph-authoring-test--tail))))

  (describe "lock contract (coordinator-lock-contract)"

    (it "acquires the coordinator lock on the canonical path and releases it"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "part-of" "P"))
      (let ((key (org-graph-coordinator--canonical file)))
        (expect 'org-graph-coordinator--acquire :to-have-been-called-with key)
        (expect (gethash key org-graph-coordinator--locks) :to-be nil)))

    (it "releases the lock when the node is missing (error path)"
      (org-graph-authoring-test--with-edges
        (expect (org-graph/write-edge file "part-of" "P" "missing")
                :to-throw 'user-error))
      (expect 'org-graph-coordinator--acquire :to-have-been-called)
      (expect (gethash (org-graph-coordinator--canonical file)
                       org-graph-coordinator--locks)
              :to-be nil)))

  (describe "reindex"

    (it "calls vulpea-db-update-file with the expanded file path after writing"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "part-of" "P"))
      (expect 'vulpea-db-update-file
              :to-have-been-called-with (expand-file-name file))))

  (describe "visit discipline"

    (it "never runs org-mode hooks and never visits the file"
      (let ((fired nil))
        (let ((org-mode-hook (list (lambda () (setq fired t)))))
          (org-graph-authoring-test--with-edges
            (org-graph/write-edge file "part-of" "P")))
        (expect fired :to-be nil)
        (expect (get-file-buffer file) :to-be nil))))

  (describe "argument and node errors"

    (it "signals user-error for a NODE-ID not in the file and leaves the file untouched"
      (let ((before (org-graph-authoring-test--bytes file)))
        (org-graph-authoring-test--with-edges
          (expect (org-graph/write-edge file "part-of" "P" "nope")
                  :to-throw 'user-error))
        (expect (org-graph-authoring-test--bytes file) :to-equal before)
        (expect 'vulpea-db-update-file :not :to-have-been-called)))

    (it "signals user-error when the file-level node has no property drawer"
      (setq file (org-graph-authoring-test--write
                  "no-drawer.org" "#+title: T\n\nbody\n"))
      (org-graph-authoring-test--with-edges
        (expect (org-graph/write-edge file "part-of" "P")
                :to-throw 'user-error))
      (expect (org-graph-authoring-test--read file) :to-equal "#+title: T\n\nbody\n"))

    (it "signals user-error when the file-level property drawer has no :ID: and leaves the file byte-identical"
      (let ((content ":PROPERTIES:\n:CATEGORY: x\n:END:\n#+title: T\n\nbody\n"))
        (setq file (org-graph-authoring-test--write "no-id.org" content))
        (let ((before (org-graph-authoring-test--bytes file)))
          (org-graph-authoring-test--with-edges
            (expect (org-graph/write-edge file "part-of" "P")
                    :to-throw 'user-error))
          (expect (org-graph-authoring-test--bytes file) :to-equal before)
          (expect 'vulpea-db-update-file :not :to-have-been-called))))

    (it "signals user-error for an empty relation type before taking the lock"
      (org-graph-authoring-test--with-edges
        (expect (org-graph/write-edge file "   " "P")
                :to-throw 'user-error)
        (expect (org-graph/write-edge file nil "P")
                :to-throw 'user-error))
      (expect 'org-graph-coordinator--acquire :not :to-have-been-called))

    (it "signals user-error for an empty target before taking the lock"
      (org-graph-authoring-test--with-edges
        (expect (org-graph/write-edge file "part-of" "")
                :to-throw 'user-error)
        (expect (org-graph/write-edge file "part-of" nil)
                :to-throw 'user-error))
      (expect 'org-graph-coordinator--acquire :not :to-have-been-called))

    (it "signals user-error when no edge-drawer name is configured"
      (let ((org-graph-edge-drawer nil))
        (expect (org-graph/write-edge file "part-of" "P")
                :to-throw 'user-error))
      (expect 'org-graph-coordinator--acquire :not :to-have-been-called)))

  (describe "round-trip through the real extractor (edge-drawer-discriminator)"

    (it "reads back as exactly one edge tuple with the normalised type"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "Part_of" "P")
        (let* ((ctx (vulpea-db--parse-file file))
               (tuples (org-graph-extractor/parse-drawer-edges
                        (vulpea-parse-ctx-ast ctx))))
          (expect tuples :to-equal '(("f1" part-of "P"))))))

    (it "yields exactly one typed_edges row through the vulpea extractor wrapper"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "Part_of" "P")
        (let* ((dir (file-name-as-directory org-graph-authoring-test--dir))
               (org-graph-roam-root dir)
               (org-graph-vault-root dir)
               (ctx (vulpea-db--parse-file file))
               (rows nil))
          (cl-letf (((symbol-function 'vulpea-db)
                     (lambda (&rest _) 'stub-db))
                    ((symbol-function 'emacsql)
                     (lambda (_db _sql &rest args)
                       (setq rows (append rows (car (last args))))
                       nil)))
            (org-graph-extractor/extract ctx (vulpea-parse-ctx-file-node ctx)))
          (expect rows :to-equal (list (vector "f1" 'part-of "P"))))))

    (it "attributes a heading-node edge to the heading, not the file-level node"
      (org-graph-authoring-test--with-edges
        (org-graph/write-edge file "part-of" "P" "h1")
        (let* ((ctx (vulpea-db--parse-file file))
               (tuples (org-graph-extractor/parse-drawer-edges
                        (vulpea-parse-ctx-ast ctx))))
          (expect tuples :to-equal '(("h1" part-of "P"))))))))

;;; org-graph/create-project-note -------------------------------------------

(defmacro org-graph-authoring-test--with-real-create (&rest body)
  "Run BODY with the REAL `vulpea-create' but its side effects stubbed.
Placement is redirected to the spec temp dir; `org-id-add-location' is
captured into the lexical `created' (ID . PATH) instead of touching
`org-id-locations'; `vulpea-db-update-file' is the spy from the suite;
`vulpea-db-get-by-id' returns a fixture for the captured ID so
`vulpea-create' can finish.  BODY sees `created'."
  (declare (indent 0) (debug (body)))
  `(let ((vulpea-default-notes-directory
          (file-name-as-directory org-graph-authoring-test--dir))
         (vulpea-create-default-function nil)
         (created nil))
     (cl-letf (((symbol-function 'org-id-add-location)
                (lambda (id path) (setq created (cons id path))))
               ((symbol-function 'vulpea-db-get-by-id)
                (lambda (id)
                  (org-graph-test/note-fixture
                   :id id :title "myproj" :tags '("project")
                   :path (cdr created)))))
       ,@body)))

(defun org-graph-authoring-test--parsed-note (path)
  "Parse PATH with vulpea's real parser; return its file-level `vulpea-note'."
  (let* ((ctx (vulpea-db--parse-file path))
         (node (vulpea-parse-ctx-file-node ctx)))
    (make-vulpea-note
     :id (plist-get node :id)
     :title (plist-get node :title)
     :tags (plist-get node :tags)
     :meta (plist-get node :meta)
     :properties (plist-get node :properties)
     :path path
     :level 0)))

(describe "org-graph/create-project-note"

  (before-each
    (setq org-graph-authoring-test--dir
          (make-temp-file "org-graph-project-" t))
    (spy-on 'vulpea-db-update-file :and-return-value 1))

  (after-each
    (when (and org-graph-authoring-test--dir
               (file-directory-p org-graph-authoring-test--dir))
      (delete-directory org-graph-authoring-test--dir t))
    (setq org-graph-authoring-test--dir nil))

  (describe "delegation shape"

    (it "calls vulpea-create with the project tag, status META, the block skeleton, and returns the id"
      (let (args)
        (cl-letf (((symbol-function 'vulpea-create)
                   (lambda (&rest a)
                     (setq args a)
                     (org-graph-test/note-fixture :id "pid" :title "myproj"))))
          (expect (org-graph/create-project-note "myproj") :to-equal "pid"))
        (expect (nth 0 args) :to-equal "myproj")
        (expect (nth 1 args) :to-be nil)
        (let ((kw (nthcdr 2 args)))
          (expect (plist-get kw :tags) :to-equal '("project"))
          (expect (plist-get kw :meta) :to-equal '(("status" . "active")))
          (expect (plist-get kw :body)
                  :to-equal "#+BEGIN: org-graph-connected\n#+END:\n")
          (expect (plist-get kw :context) :to-be nil)
          (expect (plist-member kw :properties) :to-be nil))))

    (it "passes BODY as a literal :context value behind a ${body} placeholder, never as template text"
      (let (args)
        (cl-letf (((symbol-function 'vulpea-create)
                   (lambda (&rest a)
                     (setq args a)
                     (org-graph-test/note-fixture :id "pid" :title "myproj"))))
          (org-graph/create-project-note "myproj" "Legacy %(+ 1 2) ${title}\n"))
        (let ((kw (nthcdr 2 args)))
          (expect (plist-get kw :body)
                  :to-equal "#+BEGIN: org-graph-connected\n#+END:\n\n${body}\n")
          (expect (plist-get kw :context)
                  :to-equal '(:body "Legacy %(+ 1 2) ${title}"))))))

  (describe "on-disk note (real vulpea-create, side effects stubbed)"

    (it "writes drawer, title, filetags, status META, and the empty block in the pinned order"
      (org-graph-authoring-test--with-real-create
        (let ((id (org-graph/create-project-note "myproj")))
          (expect id :to-equal (car created))
          (expect (org-graph-authoring-test--read (cdr created))
                  :to-equal (concat ":PROPERTIES:\n"
                                    (format org-property-format ":ID:" id) "\n"
                                    ":END:\n"
                                    "#+title: myproj\n"
                                    "#+filetags: :project:\n"
                                    "\n"
                                    "- status :: active\n"
                                    "\n"
                                    "#+BEGIN: org-graph-connected\n"
                                    "#+END:\n")))))

    (it "appends BODY verbatim after the block without expanding template directives"
      (org-graph-authoring-test--with-real-create
        (org-graph/create-project-note "myproj" "Legacy %(+ 1 2) and ${title} text\n")
        (expect (org-graph-authoring-test--read (cdr created))
                :to-match (regexp-quote
                           "#+BEGIN: org-graph-connected\n#+END:\n\nLegacy %(+ 1 2) and ${title} text\n"))))

    (it "places the note under vulpea-default-notes-directory with a timestamped slug filename"
      (org-graph-authoring-test--with-real-create
        (org-graph/create-project-note "myproj")
        (expect (file-in-directory-p (cdr created) org-graph-authoring-test--dir)
                :to-be-truthy)
        (expect (file-name-nondirectory (cdr created))
                :to-match "\\`[0-9]\\{14\\}[-_]myproj\\.org\\'")))

    (it "indexes immediately through vulpea-create's birth path"
      (org-graph-authoring-test--with-real-create
        (org-graph/create-project-note "myproj")
        (expect 'vulpea-db-update-file :to-have-been-called-with (cdr created))))

    (it "is accepted by the project schema predicate and validator when parsed by vulpea"
      (org-graph-authoring-test--with-real-create
        (org-graph/create-project-note "myproj" "Some body.\n")
        (let ((note (org-graph-authoring-test--parsed-note (cdr created))))
          (expect (vulpea-note-id note) :to-equal (car created))
          (expect (vulpea-note-tags note) :to-equal '("project"))
          (expect (vulpea-note-meta-get note "status" 'symbol) :to-be 'active)
          (expect (org-graph/note-of-type-p note 'project) :to-be-truthy)
          (expect (vulpea-schema-validate note 'org-graph-project) :to-be nil))))))

;;; authoring-spec.el ends here
