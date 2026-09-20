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
;; `workspace-bookmark-fixtures-teardown' removes every temp directory,
;; registry entry and bookmark-variable override the fixtures created.

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


;;; Lifecycle

(defun workspace-bookmark-fixtures-setup ()
  "Start a fixture scope.  Call from `before-each'.

Isolates the bookmark globals so no spec can read or write the real
bookmark file, and resets the bookmkeeping the teardown walks."
  (setq workspace-bookmark-fixtures--temp-dirs nil
        workspace-bookmark-fixtures--registered-names nil)
  (workspace-bookmark-fixtures-isolate-bookmarks))

(defun workspace-bookmark-fixtures-teardown ()
  "End a fixture scope.  Call from `after-each'.

Unregisters every workspace the fixtures registered, restores the
bookmark globals, and recursively deletes every temp directory the
fixtures created.  Safe to call when nothing was created."
  (dolist (name workspace-bookmark-fixtures--registered-names)
    (remhash name workspace--registry))
  (setq workspace-bookmark-fixtures--registered-names nil)
  (workspace-bookmark-fixtures-restore-bookmarks)
  (dolist (dir workspace-bookmark-fixtures--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq workspace-bookmark-fixtures--temp-dirs nil))


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
