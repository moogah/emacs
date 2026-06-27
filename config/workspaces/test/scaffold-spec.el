;;; scaffold-spec.el --- Tests for workspace directory scaffolder -*- lexical-binding: t; -*-

;; Covers register/boundary/workspace-scaffold-pipeline (five-stage
;; ordered pipeline with INIT-AND-COMMIT? gating) and
;; register/invariant/scaffold-leave-partial-on-failure (no rollback
;; on stage failure; no delete-* primitives in scaffold.el body).
;;
;; Uses REAL git against tmpdirs (design.md §D9 — mocking subprocess
;; output is brittle and would miss real-git behaviour).  The four
;; git invocations are small and deterministic.

(require 'buttercup)
(require 'cl-lib)

(defconst scaffold-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(defconst scaffold-spec--module-path
  (expand-file-name "../scaffold.el" scaffold-spec--this-dir)
  "Absolute path to the scaffold module under test.")

(load scaffold-spec--module-path)

(defmacro scaffold-spec--with-tmp-home (var &rest body)
  "Bind VAR to a fresh temp HOME directory and run BODY, cleaning up after."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file "ws-scaffold-spec-" t)))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun scaffold-spec--read-file (path)
  "Return the full contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun scaffold-spec--git (home &rest args)
  "Run `git ARGS' in HOME, returning trimmed stdout (ignores exit status)."
  (let ((default-directory (file-name-as-directory home)))
    (string-trim
     (with-output-to-string
       (with-current-buffer standard-output
         (apply #'call-process "git" nil t nil args))))))

;;; Stage-1..5 default-path acceptance

(describe "workspace-scaffold default path (INIT-AND-COMMIT? t)"
  (it "returns HOME on success"
    (scaffold-spec--with-tmp-home home
      (expect (workspace-scaffold home "alpha" :init-and-commit? t)
              :to-equal home)))

  (it "creates .git/ (stage 2)"
    (scaffold-spec--with-tmp-home home
      (workspace-scaffold home "alpha" :init-and-commit? t)
      (expect (file-directory-p (expand-file-name ".git" home))
              :not :to-be nil)))

  (it "writes home.org with a `#+TITLE: <name>' line the reader can find (stage 3)"
    (scaffold-spec--with-tmp-home home
      (workspace-scaffold home "alpha" :init-and-commit? t)
      (let ((homeorg (expand-file-name "home.org" home)))
        (expect (file-exists-p homeorg) :not :to-be nil)
        ;; RE-2a / register/invariant/indexable-requires-id: the
        ;; skeleton now carries a file-level org `:ID:' so the home is
        ;; an indexable vulpea node.  A file-level property drawer must
        ;; be the document's first element, so the `:ID:' drawer
        ;; precedes `#+TITLE:'.  The home-org-read-pipeline reader scans
        ;; for the title with a start-of-LINE regex (`^#\+TITLE:'), not
        ;; start-of-file, so the title remains discoverable below the
        ;; drawer.  Pin the reader's actual contract (a `#+TITLE: alpha'
        ;; line), not the stricter "title on line 1" the pre-ID skeleton
        ;; happened to satisfy.
        (let ((content (scaffold-spec--read-file homeorg)))
          (expect content :to-match "^#\\+TITLE: alpha$")
          ;; The file begins with the file-level `:ID:' property drawer.
          (expect content :to-match "\\`:PROPERTIES:\n:ID:[ \t]+\\S-")))))

  (it "creates sessions/ as a directory (stage 4)"
    (scaffold-spec--with-tmp-home home
      (workspace-scaffold home "alpha" :init-and-commit? t)
      (expect (file-directory-p (expand-file-name "sessions" home))
              :not :to-be nil)))

  (it "leaves sessions/ empty (no initial session file)"
    ;; The scaffolder creates sessions/ but never populates it.  An
    ;; initial session file, when wanted, is created by the
    ;; gptel-sessions integration's :on-create handler (fired from
    ;; tabs.el after registration), NOT by the scaffold pipeline —
    ;; register/boundary/gptel-sessions-workspace-consult forbids the
    ;; scaffolder from naming any gptel-sessions symbol.
    (scaffold-spec--with-tmp-home home
      (workspace-scaffold home "alpha" :init-and-commit? t)
      (let* ((sessions (expand-file-name "sessions" home))
             (files (directory-files sessions nil "\\`[^.]")))
        (expect files :to-equal nil))))

  (it "produces exactly one commit named `Initial workspace' (stage 5)"
    (scaffold-spec--with-tmp-home home
      (workspace-scaffold home "alpha" :init-and-commit? t)
      (let* ((log (scaffold-spec--git home "log" "--oneline"))
             (lines (split-string log "\n" t)))
        (expect (length lines) :to-equal 1)
        (expect (car lines) :to-match "Initial workspace\\'")))))

;;; Anchor-without-commit branch (INIT-AND-COMMIT? nil)

(describe "workspace-scaffold anchor branch (INIT-AND-COMMIT? nil)"
  (it "writes home.org and creates an empty sessions/ but does not init or commit"
    (scaffold-spec--with-tmp-home home
      ;; Pre-init the directory as a git repo so stage 2 is not relevant.
      (let ((default-directory (file-name-as-directory home)))
        (call-process "git" nil nil nil "init"))
      (workspace-scaffold home "beta" :init-and-commit? nil)
      (expect (file-exists-p (expand-file-name "home.org" home))
              :not :to-be nil)
      (expect (file-directory-p (expand-file-name "sessions" home))
              :not :to-be nil)
      ;; home.org is present but UNTRACKED — the function did not run
      ;; `git add' or `git commit'.  (sessions/ is empty, so git's
      ;; porcelain status does not list it — git does not track empty
      ;; directories.)
      (let ((porcelain (scaffold-spec--git home "status" "--porcelain")))
        (expect porcelain :to-match "^\\?\\? home\\.org$"))
      ;; No commits exist on the anchor branch.
      (let ((default-directory (file-name-as-directory home))
            (out-buf (generate-new-buffer " *scaffold-spec-log-status*")))
        (unwind-protect
            (expect (call-process "git" nil out-buf nil "log" "--oneline")
                    :not :to-equal 0)
          (kill-buffer out-buf))))))

;;; Stage-3 idempotency

(describe "workspace-scaffold stage 3 (home.org writer)"
  (it "is idempotent: pre-existing home.org content is preserved"
    (scaffold-spec--with-tmp-home home
      (let* ((homeorg (expand-file-name "home.org" home))
             (custom "#+TITLE: hand-written\n* My content\n"))
        (with-temp-file homeorg (insert custom))
        (workspace-scaffold home "gamma" :init-and-commit? nil)
        (expect (scaffold-spec--read-file homeorg) :to-equal custom)))))

;;; Indexable-at-birth (register/invariant/indexable-requires-id)

(defun scaffold-spec--home-org-id (home)
  "Return the file-level org `:ID:' value from HOME/home.org, or nil."
  (let ((content (scaffold-spec--read-file (expand-file-name "home.org" home))))
    (when (string-match ":ID:[ \t]+\\(\\S-+\\)" content)
      (match-string 1 content))))

(describe "workspace-scaffold home.org org-id stamp (indexable-requires-id)"
  (it "stamps a stable file-level :ID: into a freshly-created home.org"
    ;; RE-2a: vulpea only indexes notes carrying an :ID:, so a fresh
    ;; workspace home is stamped at birth to become a graph node.
    (scaffold-spec--with-tmp-home home
      (workspace-scaffold home "alpha" :init-and-commit? t)
      (expect (scaffold-spec--home-org-id home) :to-be-truthy)))

  (it "captures the :ID: in the initial commit (stamped before git add)"
    ;; The stamp lands before stage 5 (git add . && commit), so the id
    ;; is part of the very first committed home.org.
    (scaffold-spec--with-tmp-home home
      (workspace-scaffold home "alpha" :init-and-commit? t)
      (let ((committed (scaffold-spec--git
                        home "show" "HEAD:home.org")))
        (expect committed :to-match ":ID:[ \t]+\\S-"))))

  (it "is idempotent: re-running scaffold leaves an existing :ID: unchanged"
    ;; org-id-get-create is a no-op when an :ID: already exists, and the
    ;; stage-3 guard never rewrites an existing home.org — so a re-run
    ;; never churns the id (register/invariant/indexable-requires-id).
    (scaffold-spec--with-tmp-home home
      (workspace-scaffold home "alpha" :init-and-commit? t)
      (let ((id-before (scaffold-spec--home-org-id home)))
        (expect id-before :to-be-truthy)
        ;; Re-run with INIT-AND-COMMIT? nil: a second commit on an
        ;; unchanged tree would fail ("nothing to commit") — that is
        ;; pre-existing scaffold behaviour, orthogonal to id stamping.
        ;; The stage-3 guard + org-id-get-create idempotency are what we
        ;; assert here.
        (workspace-scaffold home "alpha" :init-and-commit? nil)
        (expect (scaffold-spec--home-org-id home) :to-equal id-before)))))

;;; Failure-path invariants (scaffold-leave-partial-on-failure)

(describe "workspace-scaffold mid-pipeline failure"
  (it "signals user-error when git commit fails (simulated)"
    (scaffold-spec--with-tmp-home home
      (let ((original-git (symbol-function 'workspace--scaffold-git)))
        (cl-letf (((symbol-function 'workspace--scaffold-git)
                   (lambda (h &rest args)
                     (if (and args (member "commit" args))
                         (user-error "git commit failed (simulated)")
                       (apply original-git h args)))))
          (expect (workspace-scaffold home "alpha" :init-and-commit? t)
                  :to-throw 'user-error)))))

  (it "leaves home.org, sessions/, and .git/ in place after mid-pipeline failure"
    (scaffold-spec--with-tmp-home home
      (let ((original-git (symbol-function 'workspace--scaffold-git)))
        (cl-letf (((symbol-function 'workspace--scaffold-git)
                   (lambda (h &rest args)
                     (if (and args (member "commit" args))
                         (user-error "git commit failed (simulated)")
                       (apply original-git h args)))))
          (ignore-errors
            (workspace-scaffold home "alpha" :init-and-commit? t)))
        (expect (file-directory-p home) :not :to-be nil)
        (expect (file-directory-p (expand-file-name ".git" home))
                :not :to-be nil)
        (expect (file-exists-p (expand-file-name "home.org" home))
                :not :to-be nil)
        (let* ((sessions (expand-file-name "sessions" home)))
          ;; sessions/ was created (stage 4 ran before the simulated
          ;; commit failure) and is left in place; it is empty because
          ;; the scaffolder never populates it.
          (expect (file-directory-p sessions) :not :to-be nil)
          (expect (directory-files sessions nil "\\`[^.]")
                  :to-equal nil)))))

  (it "the user-error message names the HOME path"
    (scaffold-spec--with-tmp-home home
      ;; Force the inner `call-process' to non-zero so the real
      ;; `workspace--scaffold-git' formats its own error.  The message
      ;; includes the HOME path (see scaffold.org §git-subprocess-helper).
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest _args) 1)))
        (condition-case err
            (workspace-scaffold home "alpha" :init-and-commit? t)
          (user-error
           (expect (error-message-string err)
                   :to-match (regexp-quote home))))))))

(describe "workspace-scaffold git-on-path failure"
  (it "signals user-error mentioning git when git exits non-zero"
    (scaffold-spec--with-tmp-home home
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest _args) 1)))
        (condition-case err
            (workspace-scaffold home "alpha" :init-and-commit? t)
          (user-error
           (expect (error-message-string err) :to-match "git")))))))

;;; Structural invariant: scaffold.el contains no rollback primitives.

(describe "Invariant: scaffold-leave-partial-on-failure (structural)"
  (it "config/workspaces/scaffold.el contains no delete-directory or delete-file primitives"
    ;; If a future change adds an auto-cleanup path on git-init
    ;; failure, this lint surfaces it.  The contract is explicit:
    ;; leave partial scaffold in place (design.md §D2).
    (expect (file-readable-p scaffold-spec--module-path) :not :to-be nil)
    (with-temp-buffer
      (insert-file-contents scaffold-spec--module-path)
      (dolist (sym '(delete-directory delete-file))
        (goto-char (point-min))
        (expect (re-search-forward
                 (concat "(" (regexp-quote (symbol-name sym)) "\\_>")
                 nil t)
                :to-be nil)))))

(provide 'scaffold-spec)
;;; scaffold-spec.el ends here
