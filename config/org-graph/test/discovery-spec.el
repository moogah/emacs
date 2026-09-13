;;; discovery-spec.el --- Vault discovery tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Tests for the vault discovery layer (`og-vault-only-discovery' task,
;; change workspace-notes-in-vault).  Covers the bounded-discovery-roots
;; invariant (`register/invariant/bounded-discovery-roots'): the sync
;; root set is EXACTLY ONE entry -- the expanded `org-graph-vault-root'
;; -- no other package is consulted (a fake `workspaces' feature changes
;; nothing), the `boundp' fallback pins ~/org/, and the wider work tree
;; is NEVER walked (negative assertion on `directory-files-recursively').
;; Also pressure-tests the invariant's "hidden directories are excluded
;; by vulpea's own rule" clause against vulpea's REAL file enumerator on
;; a throwaway tree (no DB), covers the `org-id-locations' startup seed
;; (one `org-id-add-location' per DB note), `org-graph/configure-sync'
;; wiring, and the note-placement settings
;; (`vulpea-default-notes-directory' derived from the vault root;
;; `vulpea-create-default-template' pinning the dash filename separator,
;; `register/invariant/note-filename-template-dash').
;;
;; All external dependencies are mocked at the API boundary via `cl-letf'
;; (function-scoped, never global): vulpea sync/query and `org-id'.  No
;; real DB.  `org-id-add-location' is NOT in
;; `org-graph-test/with-stubbed-vulpea', so this spec installs its own
;; scoped stub for it.
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

;; `org-graph-vault-root' is defined by the loader (org-graph.org), which
;; is not loaded in this isolated test process.  Declare it so the
;; discovery functions read a concrete value; individual specs
;; `let'-bind it as needed.
(defvar org-graph-vault-root "~/org/")

(defun org-graph-discovery-spec--expanded-vault-root ()
  "Return the canonicalised form of the current `org-graph-vault-root'."
  (file-name-as-directory (expand-file-name org-graph-vault-root)))

(describe "org-graph/index-roots"

  (it "returns exactly one root: the canonicalised vault root"
    ;; register/invariant/bounded-discovery-roots (re-stated
    ;; cycle-1789334430): a single-element list, no second entry.
    (let ((org-graph-vault-root "~/org/"))
      (expect (org-graph/index-roots)
              :to-equal (list (file-name-as-directory
                               (expand-file-name "~/org/"))))))

  (it "canonicalises a non-default vault root (expanded, trailing slash)"
    (let ((org-graph-vault-root "~/elsewhere/vault"))
      (expect (org-graph/index-roots)
              :to-equal (list (file-name-as-directory
                               (expand-file-name "~/elsewhere/vault"))))))

  (it "does NOT carry ~/org/roam/ as a separate root (vault root covers it)"
    (let ((org-graph-vault-root "~/org/"))
      (expect (org-graph/index-roots)
              :not :to-contain
              (file-name-as-directory (expand-file-name "~/org/roam/")))))

  (it "falls back to ~/org/ when org-graph-vault-root is unbound"
    ;; The `boundp' guard keeps discovery.el loadable without the loader
    ;; defcustom; the fallback must be the same directory as the
    ;; defcustom's default so placement and index root stay aligned.
    (let ((saved org-graph-vault-root))
      (unwind-protect
          (progn
            (makunbound 'org-graph-vault-root)
            (expect (boundp 'org-graph-vault-root) :to-be nil)
            (expect (org-graph/index-roots)
                    :to-equal (list (file-name-as-directory
                                     (expand-file-name "~/org/")))))
        (setq org-graph-vault-root saved))))

  (it "consults no other package: a fake `workspaces' feature changes nothing"
    ;; register/invariant/packages-independent-no-cross-naming: even
    ;; with the `workspaces' feature present, the root set is still
    ;; exactly the vault root -- nothing reads any registry.
    (let ((features (cons 'workspaces features))
          (org-graph-vault-root "~/org/"))
      (expect (featurep 'workspaces) :to-be-truthy)
      (expect (org-graph/index-roots)
              :to-equal (list (file-name-as-directory
                               (expand-file-name "~/org/"))))))

  (it "NEVER walks any tree to discover roots"
    ;; register/invariant/bounded-discovery-roots: the negative assertion.
    ;; The root is a configured value, never the product of a walk.
    (spy-on 'directory-files-recursively)
    (spy-on 'directory-files :and-call-through)
    (let ((org-graph-vault-root "~/org/"))
      (org-graph/index-roots))
    (expect 'directory-files-recursively :not :to-have-been-called)
    (expect 'directory-files :not :to-have-been-called)))

(describe "hidden subdirectories are excluded by vulpea's own rule"

  ;; The invariant's "org-graph adds no predicate of its own" clause rests
  ;; on vulpea rejecting any path containing "/." -- which is what lets a
  ;; hidden `.agents/' transcript tree live INSIDE the vault without being
  ;; indexed.  Pressure-test that rule against vulpea's real enumerator
  ;; (`vulpea-db-sync--list-org-files', the sync-time file source) on a
  ;; throwaway tree.  No DB is opened.

  (defvar org-graph-discovery-spec--tree nil)

  (before-each
    (setq org-graph-discovery-spec--tree
          (file-name-as-directory
           (make-temp-file "org-graph-discovery-spec-vault" t)))
    (let ((visible (expand-file-name "sessions/S/branches/main/session.org"
                                     org-graph-discovery-spec--tree))
          (hidden (expand-file-name "sessions/S/branches/main/.agents/x/session.org"
                                    org-graph-discovery-spec--tree))
          (top (expand-file-name "note.org" org-graph-discovery-spec--tree)))
      (dolist (f (list visible hidden top))
        (make-directory (file-name-directory f) t)
        (with-temp-file f
          (insert ":PROPERTIES:\n:ID: " (file-name-base f) "-id\n:END:\n#+title: t\n")))))

  (after-each
    (when (and org-graph-discovery-spec--tree
               (file-directory-p org-graph-discovery-spec--tree))
      (delete-directory org-graph-discovery-spec--tree t)))

  (it "vulpea-db-sync--org-file-p rejects any path with a hidden segment"
    (expect (vulpea-db-sync--org-file-p "/v/sessions/S/branches/main/.agents/x/session.org")
            :to-be nil)
    (expect (vulpea-db-sync--org-file-p "/v/sessions/S/branches/main/session.org")
            :to-be-truthy))

  (it "the real enumerator lists visible ID-bearing files but not the .agents/ transcript"
    (let ((found (vulpea-db-sync--list-org-files org-graph-discovery-spec--tree)))
      (expect (length found) :to-equal 2)
      (expect (seq-some (lambda (f) (string-suffix-p "/branches/main/session.org" f))
                        found)
              :to-be-truthy)
      (expect (seq-some (lambda (f) (string-suffix-p "/note.org" f)) found)
              :to-be-truthy)
      (expect (seq-some (lambda (f) (string-match-p "/\\.agents/" f)) found)
              :to-be nil))))

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
