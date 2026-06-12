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

(defun jf/workspace--worktree-sanitize-branch (name)
  "Return NAME sanitized into a valid git ref-component default.
Replaces whitespace and git-illegal ref characters (space, tilde,
caret, colon, question-mark, asterisk, open-bracket, backslash,
control chars, and the at-brace sequence) with \"-\", collapses
repeated \"-\", and trims leading/trailing \"-\".  Intended to seed
an editable prompt default, not to be authoritative."
  (let* ((s (or name ""))
         (s (replace-regexp-in-string "@{" "-" s))
         (s (replace-regexp-in-string "[[:space:][:cntrl:]~^:?*\\[\\\\]" "-" s))
         (s (replace-regexp-in-string "-+" "-" s))
         (s (replace-regexp-in-string "\\`-+\\|-+\\'" "" s)))
    s))

(defun jf/workspace--add-worktree (payload)
  "Create a git worktree off main for the workspace described by PAYLOAD.
PAYLOAD is the workspace-integration anchor plist; this reads `:name'
\(the workspace name, used as the default shared feature branch) and
`:home' (the workspace container directory, used as the worktree
parent).  Operates solely on PAYLOAD, never on the current tab.

Prompts for a source repo (`jf/workspace--worktree-read-repo'),
detects its main branch (`magit-main-branch' with `default-directory'
bound to the repo), and prompts for a branch name defaulting to the
sanitized workspace name.  The worktree directory is a child of
`:home' named for the source repo's basename.

If the target directory already exists, returns
\(failed . REASON) and makes NO magit call — never overwriting.
Otherwise creates the worktree on a NEW branch off main via
`magit-worktree-branch' (again with `default-directory' bound to the
repo).  Returns `ok' on success, or (failed . REASON) on any git
error."
  (let* ((repo (jf/workspace--worktree-read-repo))
         (main (let ((default-directory repo))
                 (magit-main-branch)))
         (default-branch (jf/workspace--worktree-sanitize-branch
                          (plist-get payload :name)))
         (branch (read-string "Worktree branch: " default-branch))
         (dir (expand-file-name
               (file-name-nondirectory (directory-file-name repo))
               (plist-get payload :home))))
    (if (file-exists-p dir)
        (cons 'failed (format "worktree dir already exists: %s" dir))
      (condition-case err
          (progn
            (let ((default-directory repo))
              (magit-worktree-branch dir branch main))
            'ok)
        (error (cons 'failed (error-message-string err)))))))

(defun jf/workspace--worktree-children (home)
  "Return a list of linked-worktree descriptors under HOME.
Scans the immediate child directories of HOME and keeps those whose
`.git' entry is a FILE (a linked worktree's gitfile), not a directory.
For each, binds `default-directory' into the child and calls
`magit-list-worktrees' to resolve the source repo's worktree list,
whose FIRST element is the source repo's primary worktree.  Returns a
list of plists (:dir CHILD :branch BRANCH :source SOURCE-REPO), where
BRANCH is the child worktree's branch and SOURCE-REPO is the source
repo's primary worktree directory.  Children that do not resolve are
skipped."
  (let ((acc '()))
    (when (and home (file-directory-p home))
      (dolist (child (directory-files home t directory-files-no-dot-files-regexp))
        (when (and (file-directory-p child)
                   (file-regular-p (expand-file-name ".git" child)))
          (let* ((default-directory (file-name-as-directory child))
                 (worktrees (magit-list-worktrees))
                 (source (car (car worktrees)))
                 (self (seq-find
                        (lambda (wt)
                          (file-equal-p (car wt) child))
                        worktrees))
                 (branch (and self (nth 2 self))))
            (when source
              (push (list :dir child :branch branch :source source) acc))))))
    (nreverse acc)))

(defun jf/workspace--worktree-on-purge (payload)
  "Tear down git worktrees living under PAYLOAD's `:home' before purge.
PAYLOAD is the workspace-integration anchor plist; this reads `:home'
\(the container whose child worktrees are removed)
\(register/shape/workspace-integration-anchor-payload).  Runs BEFORE
the purge deletes `:home', so the worktrees still exist on disk.

Enumerates the linked worktrees under `:home' via
`jf/workspace--worktree-children'.  For each, binds `default-directory'
into that worktree's SOURCE repo and calls `magit-worktree-delete' to
remove it, wrapped in `condition-case'.  If a removal signals (e.g. a
dirty tree magit refuses), its path is accumulated into a failure
reason and the remaining worktrees are still processed — never aborts.

After a successful removal, offers GUARDED branch deletion: only when
`magit-branch-merged-p' reports the branch merged into the source
repo's main branch is `magit-branch-delete' called, WITHOUT force.
Unmerged branches are kept and never force-deleted.

Returns `skipped' when there are NO worktrees under `:home', `ok' when
every worktree was removed cleanly, or (failed . REASON) naming the
worktree(s) that could not be removed
\(register/vocabulary/workspace-integration-outcome)."
  (let ((children (jf/workspace--worktree-children (plist-get payload :home))))
    (if (null children)
        'skipped
      (let ((failures '()))
        (dolist (wt children)
          (let ((dir (plist-get wt :dir))
                (branch (plist-get wt :branch))
                (source (plist-get wt :source)))
            (condition-case err
                (progn
                  (let ((default-directory source))
                    (magit-worktree-delete dir))
                  (when branch
                    (let* ((default-directory source)
                           (main (magit-main-branch)))
                      (when (and main
                                 (magit-branch-merged-p branch main))
                        (magit-branch-delete (list branch))))))
              (error (push (format "%s (%s)" dir (error-message-string err))
                           failures)))))
        (if failures
            (cons 'failed
                  (concat "could not remove worktree(s): "
                          (mapconcat #'identity (nreverse failures) "; ")))
          'ok)))))

(with-eval-after-load 'workspaces
  (with-eval-after-load 'magit
    (workspace-register-integration 'git-worktree
      :label "git worktree"
      :menu (cons "w" #'jf/workspace--add-worktree)
      :on-purge #'jf/workspace--worktree-on-purge)))

(provide 'workspace-git-worktrees)
