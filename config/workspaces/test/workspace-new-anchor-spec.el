;;; workspace-new-anchor-spec.el --- Tests for workspace-new anchor-existing prefix-arg flow -*- lexical-binding: t; -*-

;; Cycle-4 task `workspace-new-anchor-existing' covers:
;;
;;   - `workspace-new' (with prefix arg) dispatches to
;;     `workspace--new-anchor-existing', which prompts for an
;;     existing directory via `read-directory-name' (mustmatch=t);
;;   - three sub-cases by directory state:
;;       1. repo + home.org     → register only; no scaffold
;;       2. repo, no home.org   → scaffold WITHOUT git ops
;;                                (=workspace-scaffold ... :init-and-commit? nil=)
;;       3. non-repo            → full scaffold (mkdir, git init,
;;                                files, initial commit);
;;   - double-registration guard: refuse to anchor a directory that is
;;     already a registered workspace's :home (`workspace--same-file-p'
;;     check: device and inode, trailing-slash- and `~'-insensitive);
;;   - the workspace name is derived from the chosen directory's
;;     basename (=register/invariant/registry-name-equals-basename=);
;;   - the workspace plist is constructed via `workspace--make'
;;     (=register/shape/workspace-plist-v3=);
;;   - all paths land absolute via `read-directory-name' interactive
;;     completion (=register/invariant/home-required-no-floating-
;;     workspaces=).

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../home-org.el"   dir))
  (load (expand-file-name "../scaffold.el"   dir))
  (load (expand-file-name "../tabs.el"       dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../workspaces.el"        dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar wna-spec--tmp-dir nil
  "Per-test temp directory used as the anchor target.")

(defvar wna-spec--saved-default-parent-directory 'unset
  "Captured value of `workspaces-default-parent-directory' for restore.")

(defun wna-spec--git (dir &rest args)
  "Run git with the named ARGS inside DIR; signal on non-zero exit."
  (let* ((default-directory (file-name-as-directory dir))
         (status (apply #'call-process "git" nil nil nil args)))
    (unless (zerop status)
      (error "git %s failed in %s (status %d)"
             (mapconcat #'identity args " ") dir status))))

(defun wna-spec--init-repo (dir)
  "Make DIR a fresh git repo with an initial commit so HEAD exists."
  (wna-spec--git dir "init" "-q")
  ;; Set local identity for the commit so test runs do not depend on
  ;; the runner's global git config.
  (wna-spec--git dir "config" "user.email" "test@example.com")
  (wna-spec--git dir "config" "user.name"  "Test User")
  ;; Make an initial empty commit so `git log' has a HEAD to compare
  ;; against in case-1 scenarios where we must assert "no new commits."
  (wna-spec--git dir "commit" "--allow-empty" "-q" "-m" "initial"))

(defun wna-spec--reset ()
  "Reset tab-bar, registry, and allocate a fresh tmpdir."
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  ;; Capture defcustom for restoration in cleanup.
  (when (eq wna-spec--saved-default-parent-directory 'unset)
    (setq wna-spec--saved-default-parent-directory
          workspaces-default-parent-directory))
  (setq wna-spec--tmp-dir (make-temp-file "ws-anchor-" t)))

(defun wna-spec--cleanup ()
  "Remove the per-test tmpdir tree and restore mutated defcustoms."
  (when (and wna-spec--tmp-dir
             (file-directory-p wna-spec--tmp-dir))
    (delete-directory wna-spec--tmp-dir t))
  (setq wna-spec--tmp-dir nil)
  ;; Restore `workspaces-default-parent-directory' to whatever value
  ;; it held before this spec file ran.  Mirrors the cycle-3 cleanup
  ;; convention in workspace-new-default-spec.el.
  (unless (eq wna-spec--saved-default-parent-directory 'unset)
    (setq workspaces-default-parent-directory
          wna-spec--saved-default-parent-directory)
    (setq wna-spec--saved-default-parent-directory 'unset)))

(defun wna-spec--git-log-oneline (dir)
  "Return the output of `git log --oneline' run inside DIR, as a list."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (call-process "git" nil t nil "log" "--oneline"))
    (split-string (buffer-string) "\n" t)))

(defun wna-spec--call-anchor (home)
  "Invoke `workspace-new' with prefix arg, stubbing `read-directory-name'
to return HOME.  Returns the freshly-registered workspace plist."
  (cl-letf (((symbol-function 'read-directory-name)
             (lambda (&rest _) home)))
    (workspace-new "ignored" t)))

(describe "workspace-new anchor-existing — case 1: repo + home.org"
  (before-each
    (wna-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through)
    (spy-on 'workspace-scaffold :and-call-through))
  (after-each (wna-spec--cleanup))

  (it "registers only — no scaffolding, no new files, no new commits"
    ;; Build a repo with a pre-existing home.org owned by the user.
    (let* ((home (file-name-as-directory
                  (expand-file-name "alpha" wna-spec--tmp-dir))))
      (make-directory home t)
      (wna-spec--init-repo home)
      ;; User-authored home.org content; the package must NOT
      ;; overwrite this.
      (let ((path (expand-file-name "home.org" home))
            (user-content "#+TITLE: User-authored alpha\n* Pre-existing\n"))
        (with-temp-file path (insert user-content))
        ;; Capture commits before the anchor call.
        (let ((commits-before (wna-spec--git-log-oneline home)))
          (wna-spec--call-anchor home)
          ;; Registry has the workspace; name == basename(home).
          (let ((ws (gethash "alpha" workspace--registry)))
            (expect ws :not :to-be nil)
            (expect (workspace--name ws) :to-equal "alpha")
            (expect (workspace--home ws) :to-equal home))
          ;; `workspace-scaffold' was NOT called.
          (expect 'workspace-scaffold :not :to-have-been-called)
          ;; home.org content is unchanged (user authorship preserved).
          (with-temp-buffer
            (insert-file-contents (expand-file-name "home.org" home))
            (expect (buffer-string) :to-equal user-content))
          ;; sessions/ was NOT created (the scaffolder is the only
          ;; producer of sessions/ and it did not run).
          (expect (file-directory-p (expand-file-name "sessions" home))
                  :to-be nil)
          ;; No new commits.
          (expect (wna-spec--git-log-oneline home)
                  :to-equal commits-before)
          ;; Tab was created.
          (expect 'tab-bar-new-tab :to-have-been-called))))))

(describe "workspace-new anchor-existing — delete then re-anchor (spec scenario 20)"
  ;; "Deleted workspace can be re-anchored": workspace-delete is
  ;; unregister-only, so the directory survives; re-running the
  ;; prefix-arg anchor flow on that surviving directory re-registers it
  ;; and recreates the tab, with no scaffolding side effects (case-1
  ;; path: repo + home.org already present).
  (before-each
    (wna-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through)
    (spy-on 'workspace-scaffold :and-call-through))
  (after-each (wna-spec--cleanup))

  (it "re-registers a deleted workspace from its surviving home directory"
    (let ((home (file-name-as-directory
                 (expand-file-name "gamma" wna-spec--tmp-dir))))
      (make-directory home t)
      (wna-spec--init-repo home)
      (let ((user-content "#+TITLE: gamma\n* Pre-existing\n"))
        (with-temp-file (expand-file-name "home.org" home)
          (insert user-content))
        ;; First anchor: register + tab.
        (wna-spec--call-anchor home)
        (expect (gethash "gamma" workspace--registry) :not :to-be nil)
        ;; Delete (unregister-only): registry entry gone, dir intact.
        (workspace-delete "gamma")
        (expect (gethash "gamma" workspace--registry) :to-be nil)
        (expect (file-directory-p home) :to-be t)
        (expect (file-exists-p (expand-file-name "home.org" home)) :to-be t)
        ;; Re-anchor the surviving directory.
        (let ((commits-before (wna-spec--git-log-oneline home)))
          (wna-spec--call-anchor home)
          ;; Re-registered under the same basename, same :home.
          (let ((ws (gethash "gamma" workspace--registry)))
            (expect ws :not :to-be nil)
            (expect (workspace--name ws) :to-equal "gamma")
            (expect (workspace--home ws) :to-equal home))
          ;; No scaffolding ran on re-anchor (case-1: home.org present).
          (expect (with-temp-buffer
                    (insert-file-contents (expand-file-name "home.org" home))
                    (buffer-string))
                  :to-equal user-content)
          ;; No new commits; user's git state untouched.
          (expect (wna-spec--git-log-oneline home)
                  :to-equal commits-before))))))

(describe "workspace-new anchor-existing — case 2: repo, no home.org"
  (before-each
    (wna-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through)
    (spy-on 'workspace-scaffold :and-call-through))
  (after-each (wna-spec--cleanup))

  (it "scaffolds files but does NOT touch the user's git state"
    (let ((home (file-name-as-directory
                 (expand-file-name "beta" wna-spec--tmp-dir))))
      (make-directory home t)
      (wna-spec--init-repo home)
      (let ((commits-before (wna-spec--git-log-oneline home)))
        (wna-spec--call-anchor home)
        ;; Scaffold ran with :init-and-commit? nil.
        (expect 'workspace-scaffold :to-have-been-called)
        ;; home.org and an empty sessions/ directory now exist.
        (expect (file-exists-p (expand-file-name "home.org" home))
                :to-be t)
        (expect (file-directory-p (expand-file-name "sessions" home))
                :to-be t)
        ;; No new commits (the user owns this repo).
        (expect (wna-spec--git-log-oneline home)
                :to-equal commits-before)
        ;; Registry has the workspace.
        (let ((ws (gethash "beta" workspace--registry)))
          (expect ws :not :to-be nil)
          (expect (workspace--name ws) :to-equal "beta")
          (expect (workspace--home ws) :to-equal home))))))

(describe "workspace-new anchor-existing — case 3: non-repo"
  (before-each
    (wna-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through)
    (spy-on 'workspace-scaffold :and-call-through))
  (after-each (wna-spec--cleanup))

  (it "performs the full scaffold including git init and an initial commit"
    ;; Pre-create an empty directory (nothing in it: no .git, no home.org).
    (let ((home (file-name-as-directory
                 (expand-file-name "gamma" wna-spec--tmp-dir))))
      (make-directory home t)
      (wna-spec--call-anchor home)
      ;; Full scaffold ran.
      (expect 'workspace-scaffold :to-have-been-called)
      (expect (file-directory-p (expand-file-name ".git" home)) :to-be t)
      (expect (file-exists-p (expand-file-name "home.org" home)) :to-be t)
      (expect (file-directory-p (expand-file-name "sessions" home))
              :to-be t)
      ;; Exactly one commit (the scaffold's initial commit).
      (expect (length (wna-spec--git-log-oneline home)) :to-equal 1)
      ;; Registry has the workspace.
      (let ((ws (gethash "gamma" workspace--registry)))
        (expect ws :not :to-be nil)
        (expect (workspace--name ws) :to-equal "gamma")
        (expect (workspace--home ws) :to-equal home)))))

(describe "workspace-new anchor-existing — double-registration guard"
  (before-each
    (wna-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'workspace-scaffold :and-call-through))
  (after-each (wna-spec--cleanup))

  (it "signals user-error when the directory is already a registered :home"
    (let ((home (file-name-as-directory
                 (expand-file-name "delta" wna-spec--tmp-dir))))
      (make-directory home t)
      ;; Pre-register a workspace at this home.
      (puthash "delta"
               (workspace--make "delta" home)
               workspace--registry)
      ;; Reset spies so the assertions below measure ONLY the guarded
      ;; second call.
      (spy-on 'tab-bar-new-tab :and-call-through)
      (spy-on 'workspace-scaffold :and-call-through)
      (let ((tabs-before (length (funcall tab-bar-tabs-function)))
            (registry-size-before (hash-table-count workspace--registry)))
        (expect (wna-spec--call-anchor home) :to-throw 'user-error)
        ;; Registry unchanged.
        (expect (hash-table-count workspace--registry)
                :to-equal registry-size-before)
        ;; No tab created, no scaffold call.
        (expect 'tab-bar-new-tab :not :to-have-been-called)
        (expect 'workspace-scaffold :not :to-have-been-called)
        (expect (length (funcall tab-bar-tabs-function))
                :to-equal tabs-before)))))

(describe "workspace-new anchor-existing — trailing-slash equivalence"
  (before-each
    (wna-spec--reset)
    (spy-on 'workspace-scaffold :and-call-through))
  (after-each (wna-spec--cleanup))

  (it "guard fires when registered home has trailing slash and prompt returns the unslashed form"
    (let* ((home-with-slash (file-name-as-directory
                             (expand-file-name "epsilon" wna-spec--tmp-dir)))
           ;; Drop the trailing slash for the prompt return value.
           (home-no-slash (directory-file-name home-with-slash)))
      (make-directory home-with-slash t)
      ;; Register under the slashed form.
      (puthash "epsilon"
               (workspace--make "epsilon" home-with-slash)
               workspace--registry)
      ;; Anchor-existing prompt returns the un-slashed form.  Note that
      ;; the real `read-directory-name' with mustmatch=t would canonicalize
      ;; this, but the guard MUST cope with the un-canonicalized form too
      ;; (defensive: any non-canonical input must not produce a false
      ;; negative on the uniqueness invariant).
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) home-no-slash)))
        (expect (workspace-new "ignored" t) :to-throw 'user-error)
        ;; Registry still has exactly one entry.
        (expect (hash-table-count workspace--registry) :to-equal 1)
        (expect 'workspace-scaffold :not :to-have-been-called)))))

;;; Registry-name guard (anchoring a same-basename directory)

(describe "workspace-new anchor-existing — registry-name guard"
  ;; The registry is keyed by basename, so anchoring a DIFFERENT
  ;; directory with an existing workspace's basename must be refused:
  ;; registering it would replace that workspace's entry (its layouts,
  ;; membership and recent-layout pointer).
  (before-each
    (wna-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'workspace-scaffold :and-call-through))
  (after-each (wna-spec--cleanup))

  (it "refuses a different local directory with the same basename and keeps the existing entry"
    (let* ((home-a (file-name-as-directory
                    (expand-file-name "a/proj" wna-spec--tmp-dir)))
           (home-b (file-name-as-directory
                    (expand-file-name "b/proj" wna-spec--tmp-dir)))
           (original (workspace--set-recent-group
                      (workspace--make "proj" home-a) "home")))
      (make-directory home-a t)
      (make-directory home-b t)
      (puthash "proj" original workspace--registry)
      (let ((tabs-before (length (funcall tab-bar-tabs-function))))
        (expect (wna-spec--call-anchor home-b)
                :to-throw 'user-error
                (list (format "A workspace named proj already exists (home %s)"
                              home-a)))
        ;; The existing entry is untouched: the very same plist.
        (expect (gethash "proj" workspace--registry) :to-be original)
        (expect (hash-table-count workspace--registry) :to-equal 1)
        ;; Refused before any tab or scaffold work.
        (expect 'tab-bar-new-tab :not :to-have-been-called)
        (expect 'workspace-scaffold :not :to-have-been-called)
        (expect (length (funcall tab-bar-tabs-function)) :to-equal tabs-before)
        (expect (file-exists-p (expand-file-name ".git" home-b)) :to-be nil)
        (expect (file-exists-p (expand-file-name "home.org" home-b)) :to-be nil)))))


;;; Remote (TRAMP) workspace homes are refused (user decision 2026-09-27)

(defconst wna-spec--remote-prefix "/wsna-remote:"
  "Prefix of the stand-in remote file names below.")

(defvar wna-spec--remote-touches nil
  "Filesystem operations the stand-in remote handler was asked, newest first.")

(defun wna-spec--remote-handler (operation &rest args)
  "A stand-in for TRAMP: names under `wna-spec--remote-prefix' are remote.
`file-remote-p' answers the prefix, as TRAMP does, without connecting.
Pure file-NAME operations are answered by the default implementation.
Every other operation is a filesystem touch: it is recorded in
`wna-spec--remote-touches' and signals, so a spec that reaches the
filesystem with a remote home fails loudly instead of connecting."
  (let ((inhibit-file-name-handlers
         (cons 'wna-spec--remote-handler inhibit-file-name-handlers))
        (inhibit-file-name-operation operation))
    (pcase operation
      ('file-remote-p wna-spec--remote-prefix)
      ((or 'expand-file-name 'file-name-as-directory 'directory-file-name
           'file-name-directory 'file-name-nondirectory
           'file-name-sans-versions 'substitute-in-file-name
           'file-name-case-insensitive-p 'abbreviate-file-name)
       (apply operation args))
      (_
       (push operation wna-spec--remote-touches)
       (error "Remote filesystem touched: %S %S" operation args)))))

(defun wna-spec--remote-refusal (home)
  "The `user-error' data `workspace--refuse-remote-home' signals for HOME.
Every remote spec asserts THIS message, so each one fails when the
remote refusal is removed, rather than passing on another guard."
  (list (concat "Workspace homes must be local; remote (TRAMP)"
                " directories are not supported: " home)))

(defmacro wna-spec--with-remote-names (&rest body)
  "Run BODY with `wna-spec--remote-handler' serving remote names."
  (declare (indent 0))
  `(let ((file-name-handler-alist
          (cons (cons (concat "\\`" (regexp-quote wna-spec--remote-prefix))
                      #'wna-spec--remote-handler)
                file-name-handler-alist)))
     (setq wna-spec--remote-touches nil)
     ,@body))

(describe "remote (TRAMP) workspace homes are refused"
  (before-each
    (wna-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'workspace-scaffold :and-call-through)
    (spy-on 'workspace--registered-for-home-p :and-call-through))
  (after-each (wna-spec--cleanup))

  (it "names the path in the refusal, for a real TRAMP name, without connecting"
    (spy-on 'file-attributes :and-call-through)
    (spy-on 'file-exists-p :and-call-through)
    (let ((remote "/ssh:wsna-nohost.invalid:/srv/proj/"))
      (expect (workspace--refuse-remote-home remote)
              :to-throw 'user-error (wna-spec--remote-refusal remote))
      (expect (workspace--refuse-remote-home wna-spec--tmp-dir) :to-be nil))
    (expect 'file-attributes :not :to-have-been-called)
    (expect 'file-exists-p :not :to-have-been-called))

  (it "C-u workspace-new refuses a remote directory before any filesystem work"
    (wna-spec--with-remote-names
      (let ((home (concat wna-spec--remote-prefix "/srv/proj/")))
        (expect (file-remote-p home) :to-be-truthy)
        (expect (wna-spec--call-anchor home)
                :to-throw 'user-error (wna-spec--remote-refusal home))
        (expect wna-spec--remote-touches :to-be nil)
        ;; Refused before the duplicate-home guard, whose nil for a remote
        ;; name would mean "cannot tell", not "not registered".
        (expect 'workspace--registered-for-home-p :not :to-have-been-called)
        (expect 'workspace-scaffold :not :to-have-been-called)
        (expect 'tab-bar-new-tab :not :to-have-been-called)
        (expect (hash-table-count workspace--registry) :to-equal 0))))

  (it "C-u workspace-new refuses a remote ALIAS of a registered remote home, keeping the entry"
    ;; The case only the remote refusal catches.  The alias (another mount
    ;; path of the same remote directory) has a DIFFERENT basename, so the
    ;; registry-name guard does not fire, and the duplicate-home guard
    ;; cannot compare remote names.  Without the refusal the anchor would
    ;; go on to the filesystem (here the stand-in handler signals) and
    ;; register a second workspace on the registered one's directory.
    (wna-spec--with-remote-names
      (let* ((home (concat wna-spec--remote-prefix "/srv/proj/"))
             (alias (concat wna-spec--remote-prefix "/mnt/proj-alias/"))
             (original (workspace--make "proj" home)))
        (puthash "proj" original workspace--registry)
        (expect (wna-spec--call-anchor alias)
                :to-throw 'user-error (wna-spec--remote-refusal alias))
        (expect (gethash "proj" workspace--registry) :to-be original)
        (expect (gethash "proj-alias" workspace--registry) :to-be nil)
        (expect (hash-table-count workspace--registry) :to-equal 1)
        (expect wna-spec--remote-touches :to-be nil)
        (expect 'workspace--registered-for-home-p :not :to-have-been-called)
        (expect 'tab-bar-new-tab :not :to-have-been-called))))

  (it "workspace-new refuses a remote default parent before any filesystem work"
    (wna-spec--with-remote-names
      (setq workspaces-default-parent-directory
            (concat wna-spec--remote-prefix "/srv/"))
      (expect (workspace-new "proj")
              :to-throw 'user-error
              (wna-spec--remote-refusal
               (concat wna-spec--remote-prefix "/srv/proj")))
      (expect wna-spec--remote-touches :to-be nil)
      (expect 'workspace-scaffold :not :to-have-been-called)
      (expect 'tab-bar-new-tab :not :to-have-been-called)
      (expect (hash-table-count workspace--registry) :to-equal 0)))

  (it "workspace-re-anchor refuses a remote new home before any filesystem work"
    (let* ((old-home (file-name-as-directory
                      (expand-file-name "gone/proj" wna-spec--tmp-dir)))
           (original (workspace--mark-broken
                      (workspace--make "proj" old-home))))
      (puthash "proj" original workspace--registry)
      (spy-on 'workspace--flush-state)
      (wna-spec--with-remote-names
        (expect (workspace-re-anchor
                 "proj" (concat wna-spec--remote-prefix "/srv/proj/"))
                :to-throw 'user-error
                (wna-spec--remote-refusal
                 (concat wna-spec--remote-prefix "/srv/proj/")))
        (expect wna-spec--remote-touches :to-be nil))
      (expect (gethash "proj" workspace--registry) :to-be original)
      (expect 'workspace--flush-state :not :to-have-been-called))))

(provide 'workspace-new-anchor-spec)
;;; workspace-new-anchor-spec.el ends here
