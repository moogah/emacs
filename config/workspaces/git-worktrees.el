;;; git-worktrees.el --- Git worktree workspace integration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

(declare-function workspace--same-file-p "data-model" (a b))

(defvar jf/workspace--worktree-warning-state nil
  "Whether a discovery warning may be shown.
nil outside a candidate collection: every failure is reported.
`jf/workspace--worktree-repo-candidates' binds it to `collecting', and
the first warning inside that collection sets it to `warned', which
suppresses the rest.")

(defun jf/workspace--worktree-warn-discovery (what err)
  "Display a warning that WHAT failed with ERR.
Within one candidate collection only the first failure is shown; see
`jf/workspace--worktree-warning-state'."
  (unless (eq jf/workspace--worktree-warning-state 'warned)
    (when (eq jf/workspace--worktree-warning-state 'collecting)
      (setq jf/workspace--worktree-warning-state 'warned))
    (display-warning
     'workspace-git-worktrees
     (format "%s unavailable (portable-bookmarks discovery failed): %s"
             what (error-message-string err))
     :warning)))

(defun jf/workspace--worktree-local-only (dirs)
  "Return the entries of DIRS that are local directory names.
Non-strings and remote (TRAMP) names are dropped.  The only question
asked of an entry is `file-remote-p' without its CONNECTED argument,
which is answered from the name alone, so this touches no network."
  (seq-filter (lambda (dir) (and (stringp dir) (not (file-remote-p dir))))
              dirs))

(defun jf/workspace--worktree-git-derived-candidates ()
  "Return the canonical checkouts of the repositories in view, or nil.
Calls `workspace-bookmark-canonical-checkouts-in-view' with no argument:
the checkouts already in view, each resolved to its repository's main
worktree, which is kept only when portable-bookmarks' one canonical
predicate admits it.  That public function is one of the two public
portable-bookmarks symbols this module calls (see the Overview), and the
module is never required.  When it is unbound, the result is nil.  Never signals: if it signals, a warning
naming the error is displayed and the result is nil."
  (when (fboundp 'workspace-bookmark-canonical-checkouts-in-view)
    (condition-case err
        (funcall 'workspace-bookmark-canonical-checkouts-in-view)
      (error
       (jf/workspace--worktree-warn-discovery "Git-derived repo candidates" err)
       nil))))

(defun jf/workspace--worktree-canonical-only (dirs)
  "Return the canonical checkouts of the repositories of DIRS.
Calls `workspace-bookmark-canonical-checkouts-in-view' with DIRS, so a
candidate from another source is admitted by the same predicate as the
git-derived ones; a directory in DIRS that is a checkout of a repository
yields that repository's main worktree only when the predicate admits
it.  DIRS nil yields nil.  When the function is unbound (portable-bookmarks
not loaded), DIRS is returned unchanged.  Never signals: if it signals,
a warning naming the error is displayed and the result is nil, so
nothing unchecked is offered and the caller's floor prompt still
works."
  (cond
   ((null dirs) nil)
   ((not (fboundp 'workspace-bookmark-canonical-checkouts-in-view)) dirs)
   (t
    (condition-case err
        (funcall 'workspace-bookmark-canonical-checkouts-in-view dirs)
      (error
       (jf/workspace--worktree-warn-discovery "Repo candidate classification" err)
       nil)))))

(defun jf/workspace--worktree-in-one-discovery-call (function)
  "Call FUNCTION with no arguments as one portable-bookmarks discovery call.
Returns FUNCTION's value.  When `workspace-bookmark-call-with-discovery-memo'
is bound, FUNCTION runs inside it, so every call of
`workspace-bookmark-canonical-checkouts-in-view' FUNCTION makes shares
one memo of git's answers; otherwise FUNCTION is simply called.  Like
that function, the scope is reached through a call-time `fboundp' guard
and portable-bookmarks is never required.  FUNCTION is called at most
once.

The scope is error-barriered like every portable-bookmarks call, in two
cases: when the call into the scope signals before FUNCTION has
started, a warning is displayed (`jf/workspace--worktree-warn-discovery')
and FUNCTION is called unscoped; once FUNCTION has started, any error
is FUNCTION's own and propagates unchanged, with no warning."
  (if (not (fboundp 'workspace-bookmark-call-with-discovery-memo))
      (funcall function)
    (let ((started nil))
      (condition-case err
          (funcall 'workspace-bookmark-call-with-discovery-memo
                   (lambda ()
                     (setq started t)
                     (funcall function)))
        (error
         (if started
             (signal (car err) (cdr err))
           (jf/workspace--worktree-warn-discovery "Discovery scope" err)
           (funcall function)))))))

(defun jf/workspace--worktree-repo-candidates ()
  "Return the canonical git-repo roots offered as worktree sources.
The sources, in order, are the git-derived candidates
\(`jf/workspace--worktree-git-derived-candidates', canonical already)
and then `(magit-list-repos)' when magit is loaded, classified by
`jf/workspace--worktree-canonical-only'.  Both are built in one
portable-bookmarks discovery call
\(`jf/workspace--worktree-in-one-discovery-call').  Projectile and
project.el lists are deliberately not consulted.  Magit's remote
\(TRAMP) entries are dropped where its list enters
\(`jf/workspace--worktree-local-only'), before classification.

The appended sources are de-duplicated in this one place: nil entries
are dropped, each entry is expanded to a directory name with a trailing
slash, and two entries are one when `workspace--same-file-p' says they
are the same directory (device and inode), the first occurrence kept
at its position and in its spelling.  That `seq-uniq' is quadratic in
the number of candidates, which is tens at most.  The result is then
filtered to actual git repositories (via `locate-dominating-file' on
\".git\", or `magit-toplevel' when magit is loaded).  At most one
discovery warning is shown per call.  Returns the list; does not
prompt."
  (let* ((jf/workspace--worktree-warning-state 'collecting)
         (sources
          (jf/workspace--worktree-in-one-discovery-call
           (lambda ()
             (let ((git-derived (jf/workspace--worktree-git-derived-candidates)))
               (append git-derived
                       (and (featurep 'magit) (fboundp 'magit-list-repos)
                            (jf/workspace--worktree-canonical-only
                             (jf/workspace--worktree-local-only
                              (magit-list-repos))))))))))
    (seq-filter #'jf/workspace--worktree-git-repo-p
                (seq-uniq (mapcar (lambda (dir)
                                    (file-name-as-directory
                                     (expand-file-name dir)))
                                  (seq-remove #'null sources))
                          #'workspace--same-file-p))))

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
source is still reachable.  An empty or blank answer is no choice: it
goes straight to the floor, never probing `default-directory' (which
may be remote).  Returns the chosen directory, expanded and slash-ended
so it can be bound as `default-directory'."
  (let* ((cands (jf/workspace--worktree-repo-candidates))
         (choice (completing-read "Source git repo: " cands nil nil)))
    (file-name-as-directory
     (if (and choice
              (not (string-blank-p choice))
              (file-directory-p (expand-file-name choice)))
         (expand-file-name choice)
       (expand-file-name (read-directory-name "Source git repo: "))))))

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

The source repo is PAYLOAD's `:source-repo' when present — no source
prompt, no candidate discovery — and otherwise prompted for
\(`jf/workspace--worktree-read-repo').  A `:source-repo' that is not an
existing local directory returns (failed . REASON) without prompting.  Then
detects the repo's main branch (`magit-main-branch' with
`default-directory' bound to the repo), and prompts for a branch name
defaulting to the sanitized workspace name.  The worktree directory is
a child of `:home' named for the source repo's basename.

After the prompts, immediately before any worktree is made, `:home'
is checked (the callers' health gates ran before the prompts): when
it is not an existing LOCAL directory (deleted while a prompt was up,
remote, or absent) returns (failed . REASON) and makes NO magit call,
because `git worktree add' would recreate a missing home.
`file-remote-p' is asked first, without its CONNECTED argument, so a
remote home is never contacted.  If the target directory already exists, returns (failed . REASON) and makes NO magit
call — never overwriting.  Otherwise creates the worktree on a NEW
branch off main via `magit-worktree-branch' (again with
`default-directory' bound to the repo).  Returns `ok' on success, or
\(failed . REASON) on any git error."
  (let ((source (plist-get payload :source-repo)))
    (if (and (plist-member payload :source-repo)
             (not (and (stringp source)
                       (not (file-remote-p source))
                       (file-directory-p source))))
        (cons 'failed (format "source repository is not a directory: %s" source))
      (let* ((repo (if source
                       (file-name-as-directory (expand-file-name source))
                     (jf/workspace--worktree-read-repo)))
             (main (let ((default-directory repo))
                     (magit-main-branch)))
             (default-branch (jf/workspace--worktree-sanitize-branch
                              (plist-get payload :name)))
             (branch (read-string "Worktree branch: " default-branch))
             (home (plist-get payload :home)))
        (if (not (and (stringp home)
                      (not (file-remote-p home))
                      (file-directory-p home)))
            (cons 'failed (format "workspace home is not a local directory: %s" home))
          (let ((dir (expand-file-name
                      (file-name-nondirectory (directory-file-name repo))
                      home)))
            (if (file-exists-p dir)
                (cons 'failed (format "worktree dir already exists: %s" dir))
              (condition-case err
                  (progn
                    (let ((default-directory repo))
                      (magit-worktree-branch dir branch main))
                    'ok)
                (error (cons 'failed (error-message-string err)))))))))))

(defun jf/workspace--worktree-children (home)
  "Return a list of linked-worktree descriptors under HOME.
Scans the immediate child directories of HOME and keeps those whose
`.git' entry is a FILE (a linked worktree's gitfile), not a directory.
For each, binds `default-directory' into the child and calls
`magit-list-worktrees' to resolve the source repo's worktree list,
whose FIRST element is the source repo's primary worktree.  Returns a
list of plists (:dir CHILD :branch BRANCH :source SOURCE-REPO), where
BRANCH is the child worktree's branch and SOURCE-REPO is the source
repo's primary worktree directory.  The child's own entry in magit's
list is found with `workspace--same-file-p' (device and inode), so a
child written to while it is looked up still finds its branch.
Children that do not resolve are skipped."
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
                          (workspace--same-file-p (car wt) child))
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
