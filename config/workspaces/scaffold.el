;;; scaffold.el --- Workspaces directory scaffolder -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'workspace-data-model)

(defun workspace--scaffold-git (home &rest args)
  "Run `git ARGS' with `default-directory' bound to HOME.
Signal `user-error' with stderr on non-zero exit."
  (let* ((default-directory (file-name-as-directory home))
         (out-buf (generate-new-buffer " *workspace-scaffold-git*"))
         (status (apply #'call-process "git" nil out-buf nil args)))
    (unwind-protect
        (unless (zerop status)
          (user-error "git %s failed in %s: %s"
                      (mapconcat #'identity args " ")
                      home
                      (with-current-buffer out-buf
                        (string-trim (buffer-string)))))
      (kill-buffer out-buf))))

(defun workspace--scaffold-write-home-org (home name)
  "Write the initial `home.org' skeleton into HOME, using NAME for #+TITLE:.
Does nothing if `home.org' already exists.

This function is the SOLE writer of `home.org' in the workspaces
package — see `register/invariant/home-org-user-authored-after-creation'."
  (let ((path (workspace-home-org-path home)))
    (unless (file-exists-p path)
      (with-temp-file path
        (insert (format "#+TITLE: %s\n\n" name))
        (insert "* Description\n\n")
        (insert "* Notes\n")))))

(cl-defun workspace-scaffold (home name &key init-and-commit?)
  "Scaffold a workspace directory at HOME with display NAME.

When INIT-AND-COMMIT? is non-nil, run `git init' (stage 2) and an
initial `git commit' (stage 6).  When nil, skip those stages —
caller is anchoring an existing git repo without home.org and we
never touch the user's git state.

Stages (always | INIT-AND-COMMIT?):
  1. make-directory HOME                       (always)
  2. git init HOME                             (INIT-AND-COMMIT?)
  3. write HOME/home.org skeleton              (always — idempotent)
  4. make-directory HOME/sessions/             (always — left empty)
  5. git add . && git commit                   (INIT-AND-COMMIT?)

On any failure, signal `user-error'.  Partially-created files are
LEFT IN PLACE (design.md §D2) — the error message names the path
so the user can inspect/remediate manually.

Returns HOME on success.

This function does NOT register the workspace or create a tab —
that is the caller's responsibility (tabs.el's workspace-new)."
  (make-directory home t)
  (when init-and-commit?
    (workspace--scaffold-git home "init"))
  (workspace--scaffold-write-home-org home name)
  (make-directory (workspace--sessions-dir home) t)
  (when init-and-commit?
    (workspace--scaffold-git home "add" ".")
    (workspace--scaffold-git home "commit" "-m" "Initial workspace"))
  home)

(provide 'workspace-scaffold)
;;; scaffold.el ends here
