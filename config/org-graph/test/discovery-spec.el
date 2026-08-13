;;; discovery-spec.el --- Registry-driven discovery tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Tests for the registry-driven discovery layer (`registry-discovery'
;; task; roots widened by `vault-root-discovery').  Covers the
;; bounded-discovery-roots invariant
;; (`register/invariant/bounded-discovery-roots'): the explicit root set
;; is `org-graph-vault-root' plus the mocked workspace homes only, and
;; the wider work tree is NEVER walked (negative assertion on
;; `directory-files-recursively').  Also covers the `org-id-locations'
;; startup seed (one `org-id-add-location' per DB note),
;; `org-graph/configure-sync' wiring, and the note-placement settings
;; (`vulpea-default-notes-directory' derived from the vault root;
;; `vulpea-create-default-template' pinning the dash filename separator,
;; `register/invariant/note-filename-template-dash').
;;
;; All external dependencies are mocked at the API boundary via `cl-letf'
;; (function-scoped, never global): vulpea sync/query, `org-id', and the
;; workspaces registry accessors.  No real DB and no filesystem walk.
;; The workspaces registry accessors and `org-id-add-location' are NOT in
;; `org-graph-test/with-stubbed-vulpea', so this spec installs its own
;; scoped stubs for them.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Pull in the shared helpers (which also adds vulpea to `load-path' and
;; requires it), then load the module under test.
(defvar org-graph-discovery-spec--module-file nil
  "Absolute path of discovery.el, stashed at load time for reload specs.")

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (setq org-graph-discovery-spec--module-file
        (expand-file-name "discovery.el" module-dir))
  (require 'org-graph-discovery org-graph-discovery-spec--module-file))

;; `org-graph-vault-root' / `org-graph-watch-workspace-homes' are defined
;; by the loader (org-graph.org), which is not loaded in this isolated
;; test process.  Declare them so the discovery functions read concrete
;; values; individual specs `let'-bind them as needed.
(defvar org-graph-vault-root "~/org/")
(defvar org-graph-watch-workspace-homes t)

;; `workspace--registry' lives in `workspace-tabs' (not loaded here).
;; Declare it special so specs can dynamically `let'-bind a fixture
;; registry that the discovery helper reads via `gethash'.
(defvar workspace--registry nil)

(defmacro org-graph-test--with-workspaces (homes &rest body)
  "Run BODY with the `workspaces' feature and a fixture registry present.
HOMES is a list of absolute home-directory strings.  Installs a fixture
`workspace--registry' plus scoped stubs for the registry accessors so
`org-graph--active-workspace-homes' reads HOMES.  `sessions/' is derived
as <HOME>/sessions/ to mirror `workspace--sessions-dir'."
  (declare (indent 1) (debug (form body)))
  `(let ((workspace--registry (make-hash-table :test 'equal))
         (features (cons 'workspaces features)))
     (dolist (h ,homes)
       (puthash h (list :name h :home h) workspace--registry))
     (cl-letf (((symbol-function 'workspace--registered-names)
                (lambda () (copy-sequence ,homes)))
               ((symbol-function 'workspace--home)
                (lambda (ws) (plist-get ws :home)))
               ((symbol-function 'workspace--sessions-dir)
                (lambda (home) (expand-file-name "sessions" home))))
       ,@body)))

(describe "org-graph/index-roots"

  (it "always includes the canonicalised vault root first"
    (let ((org-graph-vault-root "~/org/")
          (org-graph-watch-workspace-homes nil))
      (expect (car (org-graph/index-roots))
              :to-equal (file-name-as-directory
                         (expand-file-name "~/org/")))))

  (it "does NOT carry ~/org/roam/ as a separate root (vault root covers it)"
    (let ((org-graph-vault-root "~/org/")
          (org-graph-watch-workspace-homes nil))
      (expect (org-graph/index-roots)
              :not :to-contain
              (file-name-as-directory (expand-file-name "~/org/roam/")))))

  (it "includes each active workspace home and its sessions/ subdir"
    (org-graph-test--with-workspaces '("/ws/alpha/" "/ws/beta/")
      (let ((org-graph-watch-workspace-homes t)
            (org-graph-vault-root "~/org/"))
        (let ((roots (org-graph/index-roots)))
          (expect roots :to-contain "/ws/alpha/")
          (expect roots :to-contain "/ws/beta/")
          (expect roots :to-contain
                  (file-name-as-directory (expand-file-name "sessions" "/ws/alpha/")))
          (expect roots :to-contain
                  (file-name-as-directory (expand-file-name "sessions" "/ws/beta/")))))))

  (it "omits workspace homes when watching is disabled"
    (org-graph-test--with-workspaces '("/ws/alpha/")
      (let ((org-graph-watch-workspace-homes nil)
            (org-graph-vault-root "~/org/"))
        (let ((roots (org-graph/index-roots)))
          (expect roots :not :to-contain "/ws/alpha/")
          (expect roots :to-equal
                  (list (file-name-as-directory
                         (expand-file-name "~/org/"))))))))

  (it "degrades to the vault root alone when workspaces is absent"
    ;; `features' deliberately omits `workspaces' here.
    (let ((features (remq 'workspaces features))
          (org-graph-watch-workspace-homes t)
          (org-graph-vault-root "~/org/"))
      (expect (org-graph/index-roots)
              :to-equal (list (file-name-as-directory
                               (expand-file-name "~/org/"))))))

  (it "NEVER walks the wider work tree to discover roots"
    ;; register/invariant/bounded-discovery-roots: the negative assertion.
    ;; Roots come from the registry + vault root only; no wholesale walk.
    (spy-on 'directory-files-recursively)
    (spy-on 'directory-files :and-call-through)
    (org-graph-test--with-workspaces '("/ws/alpha/")
      (let ((org-graph-watch-workspace-homes t)
            (org-graph-vault-root "~/org/"))
        (org-graph/index-roots)))
    (expect 'directory-files-recursively :not :to-have-been-called)
    (expect 'directory-files :not :to-have-been-called)))

(describe "note placement settings (Default notes directory section)"

  (it "falls back to ~/org when org-graph-vault-root is unbound at load"
    ;; The module-load setq ran before `org-graph-vault-root' was
    ;; bound in this test process, so the boundp guard's "~/org"
    ;; fallback applied — which expands to the same directory as the
    ;; vault-root default.  Placement and index root stay aligned.
    (expect vulpea-default-notes-directory
            :to-equal (file-name-as-directory
                       (expand-file-name "~/org/"))))

  (it "derives vulpea-default-notes-directory from a bound vault root"
    ;; Production (boundp) branch: re-run the module's load-time setq
    ;; with `org-graph-vault-root' bound to a distinctive value.  The
    ;; let-bound placement vars confine the reload's setqs to this
    ;; spec; function redefinitions are idempotent.
    (let ((org-graph-vault-root "/tmp/org-graph-spec-vault/")
          (vulpea-default-notes-directory nil)
          (vulpea-create-default-template nil))
      (load org-graph-discovery-spec--module-file nil t)
      (expect vulpea-default-notes-directory
              :to-equal (file-name-as-directory
                         (expand-file-name "/tmp/org-graph-spec-vault/")))))

  (it "pins the dash filename template (timestamp-slug.org)"
    ;; register/invariant/note-filename-template-dash: vulpea's own
    ;; default uses an underscore separator; the corpus convention is
    ;; the dash.
    (expect (plist-get vulpea-create-default-template :file-name)
            :to-equal "${timestamp}-${slug}.org")))

(describe "org-graph/seed-org-id-locations"

  (it "registers one org-id location per DB note"
    (let ((calls nil))
      (org-graph-test/with-stubbed-vulpea
          (:query (list (org-graph-test/note-fixture
                         :id "a" :title "A" :path "/n/a.org")
                        (org-graph-test/note-fixture
                         :id "b" :title "B" :path "/n/b.org")))
        (cl-letf (((symbol-function 'org-id-add-location)
                   (lambda (id path) (push (cons id path) calls))))
          (org-graph/seed-org-id-locations)))
      (expect (length calls) :to-equal 2)
      (expect (assoc "a" calls) :to-equal '("a" . "/n/a.org"))
      (expect (assoc "b" calls) :to-equal '("b" . "/n/b.org"))))

  (it "skips notes missing an id or a path"
    (let ((calls nil))
      (org-graph-test/with-stubbed-vulpea
          (:query (list (org-graph-test/note-fixture
                         :id "ok" :title "Ok" :path "/n/ok.org")
                        (org-graph-test/note-fixture
                         :id nil :title "NoId" :path "/n/x.org")
                        (org-graph-test/note-fixture
                         :id "noPath" :title "NoPath" :path nil)))
        (cl-letf (((symbol-function 'org-id-add-location)
                   (lambda (id path) (push (cons id path) calls))))
          (org-graph/seed-org-id-locations)))
      (expect (length calls) :to-equal 1)
      (expect (caar calls) :to-equal "ok"))))

(describe "org-graph/configure-sync"

  (it "points vulpea at the index roots, enables autosync, and scans"
    (let ((set-dirs nil)
          (autosync-arg 'unset)
          (scanned nil)
          (vulpea-db-sync-directories nil)
          (org-graph-watch-workspace-homes nil)
          (org-graph-vault-root "~/org/"))
      (cl-letf (((symbol-function 'vulpea-db-autosync-mode)
                 (lambda (&optional arg) (setq autosync-arg arg)))
                ((symbol-function 'vulpea-db-sync-full-scan)
                 (lambda (&optional _arg) (setq scanned t))))
        (org-graph/configure-sync)
        (setq set-dirs vulpea-db-sync-directories))
      (expect set-dirs :to-equal (list (file-name-as-directory
                                        (expand-file-name "~/org/"))))
      (expect autosync-arg :to-equal 1)
      (expect scanned :to-be t))))

;;; discovery-spec.el ends here
