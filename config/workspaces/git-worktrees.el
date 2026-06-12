;;; git-worktrees.el --- Git worktree workspace integration -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun jf/workspace--worktree-repo-candidates ()
  "Return a de-duped list of git-repo roots from available project sources.
Collects candidate directories from `projectile-known-projects'
\(when projectile is loaded), `(magit-list-repos)' (when magit is
loaded), and `project-known-project-roots' (when project.el is
available).  Each source is guarded so none is required.  Paths are
expanded to absolute form and de-duped with `delete-dups', then
filtered to directories that are actual git repositories (via
`locate-dominating-file' on \".git\", or `magit-toplevel' when magit
is loaded).  Returns the surviving list; does not prompt."
  (let ((raw '()))
    (when (and (featurep 'projectile)
               (boundp 'projectile-known-projects))
      (setq raw (append raw projectile-known-projects)))
    (when (and (featurep 'magit) (fboundp 'magit-list-repos))
      (setq raw (append raw (magit-list-repos))))
    (when (fboundp 'project-known-project-roots)
      (setq raw (append raw (project-known-project-roots))))
    (let ((expanded (delete-dups
                     (mapcar (lambda (d) (expand-file-name d))
                             (delq nil raw)))))
      (seq-filter #'jf/workspace--worktree-git-repo-p expanded))))

(defun jf/workspace--worktree-git-repo-p (dir)
  "Return non-nil when DIR is the root (or inside) a git repository.
Prefers `magit-toplevel' when magit is loaded; otherwise falls back to
`locate-dominating-file' looking for a \".git\" entry."
  (or (and (featurep 'magit) (fboundp 'magit-toplevel)
           (magit-toplevel dir))
      (locate-dominating-file dir ".git")))

(defun jf/workspace--worktree-read-repo ()
  "Prompt for a source git repository, returning an absolute directory.
Offers `jf/workspace--worktree-repo-candidates' via `completing-read'
with `require-match' nil, so a free-form path is always accepted.  If
the entered path is not an existing directory, falls back to a
`read-directory-name' floor — guaranteeing a repo present in no project
source is still reachable.  Returns the chosen directory, expanded."
  (let* ((cands (jf/workspace--worktree-repo-candidates))
         (choice (completing-read "Source git repo: " cands nil nil)))
    (if (and choice (file-directory-p (expand-file-name choice)))
        (expand-file-name choice)
      (expand-file-name (read-directory-name "Source git repo: ")))))

(with-eval-after-load 'workspaces
  (with-eval-after-load 'magit
    (workspace-register-integration 'git-worktree
      :label "git worktree"
      :menu (cons "w" #'jf/workspace--add-worktree)
      :on-purge #'jf/workspace--worktree-on-purge)))

(provide 'workspace-git-worktrees)
