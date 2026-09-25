;;; git-worktrees.el --- Git worktree workspace integration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

(defun jf/workspace--worktree-warn-discovery (what err)
  "Display one warning that WHAT failed with ERR."
  (display-warning
   'workspace-git-worktrees
   (format "%s unavailable (portable-bookmarks discovery failed): %s"
           what (error-message-string err))
   :warning))

(defun jf/workspace--worktree-dedupe-dirs (dirs)
  "Return DIRS as absolute directory names, without duplicates.
Each entry is expanded and given a trailing slash.  Two entries are
duplicates when they name the same directory: local entries are
compared by `file-truename', so spellings through a symlinked prefix
collapse; remote entries are compared by their expanded name.  The
first occurrence's spelling is kept, in order.  nil entries are
dropped."
  (let ((seen (make-hash-table :test #'equal))
        (out '()))
    (dolist (d dirs)
      (when d
        (let* ((dir (file-name-as-directory (expand-file-name d)))
               (key (if (file-remote-p dir)
                        dir
                      (condition-case nil
                          (file-name-as-directory (file-truename dir))
                        (error dir)))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push dir out)))))
    (nreverse out)))

(defun jf/workspace--worktree-git-derived-candidates ()
  "Return every checkout of the repositories in view, or nil.
Asks `workspace-bookmark--checkouts-in-view' for the checkouts already
in view and expands each to every checkout of its repository with
`workspace-bookmark--checkouts-of'.  The result is NOT yet classified;
`jf/workspace--worktree-canonical-only' does that for every source.
Those helpers belong to the portable-bookmarks module, which this module
does not require.  When any of them, or `workspace-bookmark--classify',
is unbound, the result is nil.  Never signals: if a helper signals, one
warning naming the error is displayed and the result is nil."
  (when (and (fboundp 'workspace-bookmark--checkouts-in-view)
             (fboundp 'workspace-bookmark--checkouts-of)
             (fboundp 'workspace-bookmark--classify))
    (condition-case err
        (delete-dups
         (seq-mapcat (lambda (checkout)
                       (funcall 'workspace-bookmark--checkouts-of checkout))
                     (funcall 'workspace-bookmark--checkouts-in-view)))
      (error
       (jf/workspace--worktree-warn-discovery "Git-derived repo candidates" err)
       nil))))

(defun jf/workspace--worktree-canonical-only (dirs)
  "Return the members of DIRS classified `canonical'.
Classification is by `workspace-bookmark--classify'.  When the classifier is unbound (portable-bookmarks not loaded), DIRS is
returned unchanged.  Never signals: if the classifier signals, one
warning naming the error is displayed and the result is nil, so nothing
unchecked is offered and the caller's floor prompt still works."
  (if (not (fboundp 'workspace-bookmark--classify))
      dirs
    (condition-case err
        (seq-filter
         (lambda (dir)
           (eq 'canonical (funcall 'workspace-bookmark--classify dir)))
         dirs)
      (error
       (jf/workspace--worktree-warn-discovery "Repo candidate classification" err)
       nil))))

(defun jf/workspace--worktree-repo-candidates ()
  "Return the canonical git-repo roots offered as worktree sources.
The sources, in order, are the git-derived candidates
\(`jf/workspace--worktree-git-derived-candidates') and then
`(magit-list-repos)' when magit is loaded.  Projectile and project.el
lists are deliberately not consulted.  Paths are de-duped by
`jf/workspace--worktree-dedupe-dirs' (same directory by truename, first
spelling kept) and filtered to actual git repositories (via
`locate-dominating-file' on \".git\", or `magit-toplevel' when magit is
loaded).  Every survivor is then classified by
`jf/workspace--worktree-canonical-only'.  Returns the list; does not
prompt."
  (let ((raw (copy-sequence (jf/workspace--worktree-git-derived-candidates))))
    (when (and (featurep 'magit) (fboundp 'magit-list-repos))
      (setq raw (append raw (magit-list-repos))))
    (jf/workspace--worktree-canonical-only
     (seq-filter #'jf/workspace--worktree-git-repo-p
                 (jf/workspace--worktree-dedupe-dirs raw)))))

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
