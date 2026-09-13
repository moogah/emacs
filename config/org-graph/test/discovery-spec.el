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
;;
;; Parser hardening (og-parser-hardening): the two `:around' advices on
;; vulpea's per-file parser and per-file sync update
;; (`register/invariant/index-parser-never-evaluates-file-locals',
;; `register/invariant/index-sync-per-file-errors-contained').  Asserts
;; the advice is installed (`advice-member-p', so a vulpea rename fails
;; loudly) and idempotent, that the parse runs with
;; `enable-local-variables' nil (unit + through the advised dispatcher
;; with the inner parser stubbed + the real, un-stubbed parse of a temp
;; file carrying an `eval:' local, with a control proving the fixture
;; would otherwise fire), and that a per-file sync error is contained
;; (returns nil, logged to *org-graph*, the rest of a batch proceeds).

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

;;; Parser hardening (og-parser-hardening)

;; Probe target for the `eval:' file-local fixture.  If the parser ever
;; evaluates the Local Variables block, this counter moves.
(defvar org-graph-test--must-not-run-calls 0
  "Number of times `org-graph-test--must-not-run' was called.")

(defun org-graph-test--must-not-run ()
  "Record a call.  Named in the `eval:' local of the parse fixture."
  (setq org-graph-test--must-not-run-calls
        (1+ org-graph-test--must-not-run-calls)))

(defun org-graph-test--write-eval-local-fixture (dir)
  "Write an org file under DIR whose Local Variables block calls the probe.
Returns the file path."
  (let ((file (expand-file-name "eval-local.org" dir)))
    (with-temp-file file
      (insert ":PROPERTIES:\n"
              ":ID: 11111111-2222-3333-4444-555555555555\n"
              ":END:\n"
              "#+title: Eval local fixture\n\n"
              "* A heading\n\nBody.\n\n"
              "# Local Variables:\n"
              "# eval: (org-graph-test--must-not-run)\n"
              "# End:\n"))
    file))

(defun org-graph-test--advice-count (symbol)
  "Return how many pieces of advice are installed on SYMBOL."
  (let ((n 0))
    (advice-mapc (lambda (&rest _) (setq n (1+ n))) symbol)
    n))

(describe "parser hardening: file-local variables (register/invariant/index-parser-never-evaluates-file-locals)"

  (before-each
    (setq org-graph-test--must-not-run-calls 0))

  (it "installs the parse advice on vulpea-db--parse-file at module load"
    ;; A vulpea rename of the seam fails here loudly.
    (expect (advice-member-p #'org-graph--vulpea-parse-safely
                             'vulpea-db--parse-file)
            :to-be-truthy))

  (it "re-installing is idempotent (exactly one copy of each advice)"
    (org-graph--install-vulpea-hardening)
    (org-graph--install-vulpea-hardening)
    (expect (org-graph-test--advice-count 'vulpea-db--parse-file)
            :to-equal 1)
    (expect (org-graph-test--advice-count
             'vulpea-db-sync--update-file-if-changed)
            :to-equal 1))

  (it "binds enable-local-variables and enable-dir-local-variables to nil around the parse"
    ;; Unit: drive the wrapper with a recording ORIG while the globals are t.
    (let ((enable-local-variables t)
          (enable-dir-local-variables t)
          (seen 'unset)
          (seen-dir 'unset))
      (org-graph--vulpea-parse-safely
       (lambda (&rest _)
         (setq seen enable-local-variables
               seen-dir enable-dir-local-variables)
         'parsed)
       "/vault/note.org")
      (expect seen :to-be nil)
      (expect seen-dir :to-be nil)))

  (it "passes the arguments and result through unchanged"
    (let ((got nil))
      (expect (org-graph--vulpea-parse-safely
               (lambda (&rest args) (setq got args) 'ctx)
               "/vault/note.org")
              :to-be 'ctx)
      (expect got :to-equal '("/vault/note.org"))))

  (it "the advised dispatcher hands the inner temp-buffer parser a nil binding"
    ;; Through the real advised `vulpea-db--parse-file' with the inner
    ;; method function stubbed to record the binding it observes.
    (let ((enable-local-variables t)
          (vulpea-db-parse-method 'temp-buffer)
          (seen 'unset)
          (seen-args nil))
      (cl-letf (((symbol-function 'vulpea-db--parse-with-temp-buffer)
                 (lambda (&rest args)
                   (setq seen enable-local-variables
                         seen-args args)
                   'stub-ctx)))
        (expect (vulpea-db--parse-file "/vault/note.org") :to-be 'stub-ctx))
      (expect seen :to-be nil)
      ;; temp-buffer method re-runs org-mode: RERUN-ORG-MODE is t
      (expect seen-args :to-equal '("/vault/note.org" t))))

  (it "never evaluates an eval: local through the real, un-stubbed parse path"
    ;; Spec scenario "eval local variable is not evaluated during indexing".
    ;; Global `enable-local-variables' is :all (the most permissive value,
    ;; the only one under which batch Emacs would evaluate an unsafe eval
    ;; without prompting) so the assertion proves the advice overrides
    ;; the user's setting rather than relying on it.
    (let* ((dir (make-temp-file "org-graph-parser-hardening" t))
           (file (org-graph-test--write-eval-local-fixture dir)))
      (unwind-protect
          (let ((enable-local-variables :all)
                (vulpea-db-parse-method 'temp-buffer)
                (org-mode-hook nil))
            (let ((ctx (vulpea-db--parse-file file)))
              ;; parse completed and produced a context (file was indexed)
              (expect (vulpea-parse-ctx-p ctx) :to-be t)
              (expect (plist-get (vulpea-parse-ctx-file-node ctx) :id)
                      :to-equal "11111111-2222-3333-4444-555555555555"))
            (expect org-graph-test--must-not-run-calls :to-equal 0))
        (delete-directory dir t))))

  (it "control: the un-advised inner parser DOES evaluate the same fixture"
    ;; Stub-bounded regression guard: proves the fixture really exercises
    ;; `hack-local-variables' (org-mode re-run with `buffer-file-name'
    ;; set), so the negative assertion above is not vacuous.  If vulpea
    ;; stops re-running org-mode this way, this control fails and the
    ;; hardening needs re-evaluation.
    (let* ((dir (make-temp-file "org-graph-parser-hardening" t))
           (file (org-graph-test--write-eval-local-fixture dir)))
      (unwind-protect
          (let ((enable-local-variables :all)
                (org-mode-hook nil))
            (vulpea-db--parse-with-temp-buffer file t)
            (expect org-graph-test--must-not-run-calls :to-equal 1))
        (delete-directory dir t)))))

(describe "parser hardening: per-file sync containment (register/invariant/index-sync-per-file-errors-contained)"

  (before-each
    (when (get-buffer org-graph--log-buffer-name)
      (kill-buffer org-graph--log-buffer-name)))

  (after-each
    (when (get-buffer org-graph--log-buffer-name)
      (kill-buffer org-graph--log-buffer-name)))

  (it "installs the sync advice on vulpea-db-sync--update-file-if-changed at module load"
    (expect (advice-member-p #'org-graph--vulpea-update-safely
                             'vulpea-db-sync--update-file-if-changed)
            :to-be-truthy))

  (it "returns nil and logs (path . error) to *org-graph* when the update signals"
    (let ((inhibit-message t)
          (result 'unset))
      (setq result
            (org-graph--vulpea-update-safely
             (lambda (&rest _) (error "Malformed drawer at line 7"))
             "/vault/bad.org"))
      (expect result :to-be nil)
      (let ((log (with-current-buffer org-graph--log-buffer-name
                   (buffer-string))))
        (expect log :to-match (regexp-quote "/vault/bad.org"))
        (expect log :to-match (regexp-quote "Malformed drawer at line 7")))))

  (it "echoes the skip with message"
    (spy-on 'message)
    (org-graph--vulpea-update-safely
     (lambda (&rest _) (error "Boom"))
     "/vault/bad.org")
    (expect 'message :to-have-been-called)
    (expect (apply #'format (spy-calls-args-for 'message 0))
            :to-match (regexp-quote "/vault/bad.org")))

  (it "passes the result through and logs nothing when the update succeeds"
    (expect (org-graph--vulpea-update-safely
             (lambda (path &optional cache) (list path cache))
             "/vault/ok.org" 'cache)
            :to-equal '("/vault/ok.org" cache))
    (expect (get-buffer org-graph--log-buffer-name) :to-be nil))

  (it "processes the other files of a three-file batch when one signals"
    ;; Through the real advised `vulpea-db-sync--update-file-if-changed'
    ;; on real temp files, with the DB boundary (`vulpea-db-update-file')
    ;; stubbed to signal for the middle path only.  Mirrors the per-path
    ;; loop of vulpea's queue processor / full scan.
    (let* ((dir (make-temp-file "org-graph-sync-containment" t))
           (paths (mapcar (lambda (n)
                            (let ((f (expand-file-name n dir)))
                              (with-temp-file f (insert "#+title: " n "\n"))
                              f))
                          '("a.org" "bad.org" "c.org")))
           (hash-cache (make-hash-table :test 'equal))
           (updated nil)
           (results nil)
           (inhibit-message t))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'vulpea-db-update-file)
                       (lambda (path)
                         (when (string-suffix-p "bad.org" path)
                           (error "Extractor exploded on %s" path))
                         (push path updated)
                         1)))
              (dolist (p paths)
                (push (vulpea-db-sync--update-file-if-changed p hash-cache)
                      results)))
            (setq results (nreverse results))
            ;; the bad file returned nil; the good ones were updated (t)
            (expect results :to-equal '(t nil t))
            (expect (length updated) :to-equal 2)
            (expect (with-current-buffer org-graph--log-buffer-name
                      (buffer-string))
                    :to-match (regexp-quote "bad.org")))
        (delete-directory dir t)))))

;;; discovery-spec.el ends here
