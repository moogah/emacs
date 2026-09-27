;;; portable-bookmarks-fixtures.el --- Buttercup fixtures for portable bookmarks -*- lexical-binding: t; -*-

;; Shared fixtures for the `portable-bookmarks' spec files.  This file is
;; NOT a spec: the runner discovers `*-spec.el' only
;; (`jf/test-find-buttercup-test-files'), so nothing here executes unless a
;; spec loads it.
;;
;; Everything the portable-bookmarks suite needs to build a REAL git
;; situation on disk lives here: throwaway repositories, linked worktrees,
;; a fake workspace registry entry, and an isolated bookmark file.  The
;; design's testing approach (design.md D11) is "both real git and mocks",
;; and these fixtures are the real-git half — behavioral specs run actual
;; `git' subprocesses so identity normalization and worktree
;; classification are exercised against git's own output rather than a
;; hand-written imitation of it.
;;
;; Usage from a spec:
;;
;;   (before-each (workspace-bookmark-fixtures-setup))
;;   (after-each  (workspace-bookmark-fixtures-teardown))
;;
;; or, when the specs must not see the developer's registered workspaces,
;; at `describe' level in place of that pair:
;;
;;   (workspace-bookmark-fixtures-with-empty-registry
;;     (setq repo (workspace-bookmark-fixtures-make-repo)))
;;
;; `workspace-bookmark-fixtures-teardown' removes every temp directory,
;; registry entry and bookmark-variable override the fixtures created, and
;; puts the live registry back when the fixtures swapped it out.

(require 'cl-lib)
(require 'bookmark)

;; `workspace--make' (the workspace plist constructor) and
;; `workspace--registry' come from the workspaces package.  Under the test
;; harness `init.el' has already loaded them; load the data model by
;; relative path anyway so a spec can be run against this file in
;; isolation.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (unless (fboundp 'workspace--make)
    (load (expand-file-name "../data-model.el" dir))))

;; Owned by tabs.el.  Declared defensively so the registry fixtures work
;; even when only data-model.el is loaded; tabs.el's own `defvar' is a
;; no-op once the symbol is bound, so both files see the SAME hash table.
(defvar workspace--registry (make-hash-table :test 'equal))

(defvar workspace-bookmark-fixtures--temp-dirs nil
  "Temp directories created by the fixtures, newest first.
`workspace-bookmark-fixtures-teardown' deletes these.  Newest-first
ordering matters: a linked worktree created inside a repo must be
removable before (or harmlessly after) its parent.")

(defvar workspace-bookmark-fixtures--registered-names nil
  "Workspace names the fixtures pushed into `workspace--registry'.")

(defvar workspace-bookmark-fixtures--bookmark-state nil
  "Saved bookmark globals, or nil when bookmarks are not isolated.
A plist of the pre-isolation values restored by
`workspace-bookmark-fixtures-teardown'.")

(defvar workspace-bookmark-fixtures--live-registry nil
  "The live `workspace--registry' while it is set aside, or nil.
Non-nil only between `workspace-bookmark-fixtures-isolate-registry' and
the restore in `workspace-bookmark-fixtures-teardown'.")


;;; Lifecycle

(defun workspace-bookmark-fixtures-setup ()
  "Start a fixture scope.  Call from `before-each'.

Isolates the bookmark globals so no spec can read or write the real
bookmark file, and resets the bookkeeping the teardown walks.

Re-entrant, on the same terms as
`workspace-bookmark-fixtures-isolate-bookmarks': when a scope is already
open, the tracking lists ACCUMULATE rather than reset, so a nested or
repeated setup cannot orphan the outer scope\'s temp directories or leave
its names in `workspace--registry\' — the one hash table the other specs
in this directory share."
  (unless workspace-bookmark-fixtures--bookmark-state
    (setq workspace-bookmark-fixtures--temp-dirs nil
          workspace-bookmark-fixtures--registered-names nil))
  (workspace-bookmark-fixtures-isolate-bookmarks))

(defun workspace-bookmark-fixtures-reset-jump-state ()
  "Cancel a pending restore-then-jump finish step and clear the jump state.
A spec that drives an off-workspace jump without draining the event
loop leaves `workspace-bookmark--finish-restore' queued and the
module's jump globals set; left alone they would fire, or be read, in
whatever spec file runs next.  Every symbol is guarded, so this is safe
before the module is loaded."
  (when (fboundp 'workspace-bookmark--finish-restore)
    (cancel-function-timers #'workspace-bookmark--finish-restore))
  (when (boundp 'workspace-bookmark--awaiting-jump-target)
    (set 'workspace-bookmark--awaiting-jump-target nil))
  (when (boundp 'workspace-bookmark--jump-target)
    (set 'workspace-bookmark--jump-target nil)))

(defun workspace-bookmark-fixtures-teardown ()
  "End a fixture scope.  Call from `after-each'.

Cancels a pending restore-then-jump finish step and clears the jump
state (`workspace-bookmark-fixtures-reset-jump-state'), unregisters
every workspace the fixtures registered, restores the bookmark globals,
recursively deletes every temp directory the fixtures created and, when
`workspace-bookmark-fixtures-isolate-registry' swapped the registry out,
puts the live table back.  The registry comes back even if an earlier
step signals, so a temp directory that cannot be deleted never leaves
an empty registry behind for the next spec.  Safe to call when nothing
was created."
  (unwind-protect
      (progn
        (workspace-bookmark-fixtures-reset-jump-state)
        (dolist (name workspace-bookmark-fixtures--registered-names)
          (remhash name workspace--registry))
        (setq workspace-bookmark-fixtures--registered-names nil)
        (workspace-bookmark-fixtures-restore-bookmarks)
        (dolist (dir workspace-bookmark-fixtures--temp-dirs)
          (when (file-directory-p dir)
            (delete-directory dir t)))
        (setq workspace-bookmark-fixtures--temp-dirs nil))
    (workspace-bookmark-fixtures-restore-registry)))


;;; Registry isolation

(defun workspace-bookmark-fixtures-isolate-registry ()
  "Swap `workspace--registry' for a fresh, empty table until teardown.

The live table is set aside and put back by
`workspace-bookmark-fixtures-teardown' (or, on its own, by
`workspace-bookmark-fixtures-restore-registry').  The fresh table uses
the `equal' test, as tabs.el's does, because workspace names are
strings.  Re-entrant: when the registry is already isolated the outer
save is kept and the current table stays.  Returns the table now bound
to `workspace--registry'."
  (unless workspace-bookmark-fixtures--live-registry
    (setq workspace-bookmark-fixtures--live-registry workspace--registry
          workspace--registry (make-hash-table :test 'equal)))
  workspace--registry)

(defun workspace-bookmark-fixtures-live-registry ()
  "Return the live `workspace--registry'.

While the fixtures have the registry isolated this is the table they
set aside, NOT the one currently bound; otherwise it is the current
table.  Specs use it to assert that nothing leaked into the developer's
registry, without keeping a copy of their own."
  (or workspace-bookmark-fixtures--live-registry workspace--registry))

(defun workspace-bookmark-fixtures-restore-registry ()
  "Put the live `workspace--registry' back, if the fixtures isolated it.
A no-op otherwise."
  (when workspace-bookmark-fixtures--live-registry
    (setq workspace--registry workspace-bookmark-fixtures--live-registry
          workspace-bookmark-fixtures--live-registry nil)))

(defun workspace-bookmark-fixtures-setup-with-empty-registry (&optional setup)
  "Isolate the registry, start a fixture scope, then call SETUP.

SETUP is a function of no arguments that builds the per-spec fixtures,
or nil.  If SETUP signals, the scope is left OPEN: the registry stays
isolated and the fixtures created so far stay registered, so the spec
body buttercup runs anyway after a failed `before-each' still sees the
fresh table and the scratch bookmark file, never the live ones.  The
`after-each' that `workspace-bookmark-fixtures-with-empty-registry'
installs (buttercup runs it even after a failed hook) tears the scope
down and puts the live registry back."
  (workspace-bookmark-fixtures-isolate-registry)
  (workspace-bookmark-fixtures-setup)
  (when setup (funcall setup)))

(defmacro workspace-bookmark-fixtures-with-empty-registry (&rest setup-body)
  "Run every spec of the current `describe' against an empty registry.

Use at `describe' level in place of a `before-each' / `after-each' pair.
Expands to a `before-each' that sets the live `workspace--registry'
aside, installs a fresh `equal' table, starts a fixture scope and runs
SETUP-BODY (the per-spec fixture building, which may `setq' the
describe's `:var' variables), and an `after-each' that tears the scope
down and puts the live table back.

The live table comes back on every exit because buttercup runs the
`after-each' even when the spec body or an earlier hook fails; a signal
in SETUP-BODY deliberately leaves the scope open until then, so the
failing spec's body still runs isolated (see
`workspace-bookmark-fixtures-setup-with-empty-registry').
Needs buttercup loaded at the expansion site, as any spec has."
  (declare (indent 0) (debug (&rest form)))
  `(progn
     (before-each
       (workspace-bookmark-fixtures-setup-with-empty-registry
        (lambda () ,@setup-body)))
     (after-each
       (workspace-bookmark-fixtures-teardown))))


;;; Temp directories

(defun workspace-bookmark-fixtures-temp-dir (&optional prefix)
  "Create and track a temp directory, returning its absolute path.

PREFIX defaults to \"wsbm-\".  The path is run through `file-truename'
because on macOS `make-temp-file' hands back a /var/... path whose real
name is /private/var/...; `git' reports the real name, so truenaming here
keeps fixture paths and git output `equal'-comparable."
  (let ((dir (file-name-as-directory
              (file-truename (make-temp-file (or prefix "wsbm-") t)))))
    (push dir workspace-bookmark-fixtures--temp-dirs)
    dir))

(defun workspace-bookmark-fixtures-track-dir (dir)
  "Track DIR for deletion at teardown and return it."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (push dir workspace-bookmark-fixtures--temp-dirs)
    dir))


;;; git

(defun workspace-bookmark-fixtures-git (dir &rest args)
  "Run git with ARGS in DIR and return its trimmed stdout.

Signals an error naming the command and its output on non-zero exit, so
a broken fixture fails loudly at the point of breakage instead of
producing a subtly wrong repository."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil args)))
        (unless (eq status 0)
          (error "Fixture git %s failed in %s (status %s): %s"
                 (string-join args " ") dir status
                 (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(cl-defun workspace-bookmark-fixtures-make-repo
    (&key remote (branch "main") (file "README.md") (contents "seed\n")
          (prefix "wsbm-repo-"))
  "Create a throwaway git repository and return its absolute root path.

The repository has one commit containing FILE with CONTENTS, on BRANCH.
When REMOTE is non-nil it is added as the URL of `origin', which is how
SSH-versus-HTTPS identity normalization gets exercised against real
remotes rather than against strings.

Identity (`user.email'/`user.name') is set LOCALLY: batch git inherits no
committer identity from the test environment, and commit signing is
disabled locally for the same reason — the developer's global
`commit.gpgsign' must not be able to make the suite prompt or fail."
  (let* ((root (workspace-bookmark-fixtures-temp-dir prefix))
         (git (lambda (&rest args)
                (apply #'workspace-bookmark-fixtures-git root args))))
    (funcall git "init" "--quiet")
    ;; `git init -b' needs git >= 2.28; symbolic-ref works on any version
    ;; and is safe before the first commit.
    (funcall git "symbolic-ref" "HEAD" (concat "refs/heads/" branch))
    (funcall git "config" "user.email" "fixtures@example.invalid")
    (funcall git "config" "user.name" "Portable Bookmarks Fixtures")
    (funcall git "config" "commit.gpgsign" "false")
    (let ((path (expand-file-name file root)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path (insert contents)))
    (funcall git "add" file)
    (funcall git "commit" "--quiet" "-m" "seed")
    (when remote
      (funcall git "remote" "add" "origin" remote))
    root))

(cl-defun workspace-bookmark-fixtures-add-worktree
    (repo path &key (branch "feature"))
  "Add a linked worktree of REPO at PATH on a new BRANCH; return PATH.

PATH must not already exist — `git worktree add' creates it.  Pass a
child of a tracked temp directory (or of REPO's parent) so teardown
reclaims it."
  (let ((target (directory-file-name (expand-file-name path))))
    (workspace-bookmark-fixtures-git repo "worktree" "add" "--quiet"
                                     "-b" branch target "HEAD")
    (workspace-bookmark-fixtures-track-dir target)))

(defun workspace-bookmark-fixtures-write-file (root relpath &optional contents)
  "Create RELPATH under ROOT holding CONTENTS; return its absolute name.

Parent directories are created.  CONTENTS defaults to \"x\\n\".  The
file is NOT added to git: this is for the untracked-file cases, and
`workspace-bookmark-fixtures-commit-file' for the tracked ones."
  (let ((path (expand-file-name relpath root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert (or contents "x\n")))
    path))

(defun workspace-bookmark-fixtures-commit-file (repo relpath &optional contents)
  "Create RELPATH in REPO with CONTENTS, add and commit it; return its path.

The add is forced (`git add -f'), so a name an ignore rule would skip —
`.DS_Store' under a global excludesfile, say — still ends up tracked,
which is what the specs asking for a tracked OS artifact rely on.  REPO
may be a main worktree or a linked one; the commit lands on whatever
branch it has checked out.  Committer identity is whatever REPO's
configuration provides: repositories from
`workspace-bookmark-fixtures-make-repo', and their linked worktrees,
carry a local one.  A `git clone' of one does NOT inherit it; give clones
their own local identity before committing there."
  (let ((path (workspace-bookmark-fixtures-write-file repo relpath contents)))
    (workspace-bookmark-fixtures-git repo "add" "-f" "--" relpath)
    (workspace-bookmark-fixtures-git repo "commit" "--quiet" "-m"
                                     (concat "track " relpath))
    path))


;;; The identity questions, asked the way the live paths ask them

(defun workspace-bookmark-fixtures-root-of (dir)
  "Return the root of the checkout containing DIR, or nil.
The root the live paths use: the first element of DIR's position,
`workspace-bookmark--checkout-position' — the one `rev-parse' that
places a directory for the stamp seam and for discovery alike."
  (car (workspace-bookmark--checkout-position dir)))

(defun workspace-bookmark-fixtures-identity-of (dir)
  "Return the identity of the repository containing DIR, or nil.
Composed as the live paths compose it: DIR's position names the root
\(`workspace-bookmark-fixtures-root-of'), and the identity is that
root's (`workspace-bookmark--root-identity').  The identity memo is
bypassed, so one spec's answer never depends on another's."
  (let ((root (workspace-bookmark-fixtures-root-of dir)))
    (and root (workspace-bookmark--root-identity root))))


;;; Workspace registry

(defun workspace-bookmark-fixtures-register-workspace (name home)
  "Register a workspace NAME anchored at HOME in `workspace--registry'.

The plist is built with `workspace--make', the data layer's constructor,
rather than written out by hand — it canonicalises `:home' and owns the
plist shape, so a fixture that hand-rolled the shape would drift from
production the moment the shape changed.  Returns the plist."
  (let ((ws (workspace--make name home)))
    (puthash name ws workspace--registry)
    (push name workspace-bookmark-fixtures--registered-names)
    ws))

(defun workspace-bookmark-fixtures-make-workspace (name &optional home)
  "Create a temp home directory for workspace NAME and register it.

HOME defaults to a fresh tracked temp directory.  Returns the home path."
  (let ((home (or home (workspace-bookmark-fixtures-temp-dir
                        (format "wsbm-%s-" name)))))
    (workspace-bookmark-fixtures-register-workspace name home)
    home))


;;; Bookmark isolation

(defun workspace-bookmark-fixtures-isolate-bookmarks ()
  "Point the bookmark globals at a throwaway file with an empty alist.

`bookmark-default-file' is set to a path inside a tracked temp directory
that does NOT exist yet, `bookmark-alist' to nil and
`bookmark-bookmarks-timestamp' to nil.  Together these make
`bookmark-maybe-load-default-file' a no-op (its first arm is gated on
`file-readable-p'), so no spec can read — or, via `bookmark-save', write
— the developer's real bookmark list.  Returns the temp file path."
  (unless workspace-bookmark-fixtures--bookmark-state
    (setq workspace-bookmark-fixtures--bookmark-state
          (list :default-file bookmark-default-file
                :alist bookmark-alist
                :timestamp bookmark-bookmarks-timestamp)))
  (let ((path (expand-file-name
               "bookmarks"
               (workspace-bookmark-fixtures-temp-dir "wsbm-bookmarks-"))))
    (setq bookmark-default-file path
          bookmark-alist nil
          bookmark-bookmarks-timestamp nil)
    path))

(defun workspace-bookmark-fixtures-restore-bookmarks ()
  "Restore the bookmark globals saved by the isolation helper."
  (when workspace-bookmark-fixtures--bookmark-state
    (setq bookmark-default-file
          (plist-get workspace-bookmark-fixtures--bookmark-state :default-file)
          bookmark-alist
          (plist-get workspace-bookmark-fixtures--bookmark-state :alist)
          bookmark-bookmarks-timestamp
          (plist-get workspace-bookmark-fixtures--bookmark-state :timestamp))
    (setq workspace-bookmark-fixtures--bookmark-state nil)))

(provide 'portable-bookmarks-fixtures)
;;; portable-bookmarks-fixtures.el ends here
