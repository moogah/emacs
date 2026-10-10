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
(require 'seq)
(require 'bookmark)
(require 'dired)
;; The jump harness below reports a missing decline with `expect'.
(require 'buttercup)

;; `workspace--make' (the workspace plist constructor) and
;; `workspace--registry' come from the workspaces package.  Under the test
;; harness `init.el' has already loaded them; load the data model by
;; relative path anyway so a spec can be run against this file in
;; isolation.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (unless (fboundp 'workspace--make)
    (load (expand-file-name "../data-model.el" dir)))
  ;; The core test helpers (the fake remote handler).  Core specs load that
  ;; file themselves; the portable-bookmarks specs reach it through here.
  (load (expand-file-name "workspaces-test-helpers.el" dir)))

;; Owned by tabs.el.  Declared defensively so the registry fixtures work
;; even when only data-model.el is loaded; tabs.el's own `defvar' is a
;; no-op once the symbol is bound, so both files see the SAME hash table.
(defvar workspace--registry (make-hash-table :test 'equal))

;; Owned by tabs.el too, which cannot be loaded on its own (it requires the
;; rest of the package by feature name).  When a spec runs outside the
;; harness, supply the same registry walk; under the harness the real
;; definition is already bound and this is skipped.  The ONE copy: the
;; counting validator of portable-bookmarks-spec.el ("finds no inline
;; stand-in in any spec file") fails the suite when a spec file defines
;; its own.
(unless (fboundp 'workspace--registered-names)
  (defun workspace--registered-names ()
    "Return the list of workspace names currently in the registry."
    (let (names)
      (maphash (lambda (k _v) (push k names)) workspace--registry)
      (nreverse names))))

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
    (set 'workspace-bookmark--jump-target nil))
  (when (boundp 'workspace-bookmark--jump-target-display)
    (set 'workspace-bookmark--jump-target-display nil)))

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

(defun workspace-bookmark-fixtures-mark-broken (name)
  "Tag the registered workspace NAME broken, as persistence does at load.

The plist is re-registered through `workspace--mark-broken', the data
layer's tagger, so the fixture never writes the flag by hand.  The home
directory is left as it is: a spec that wants the load-time picture
\(a missing home behind the tag) deletes it first; teardown tolerates
that.  Signals when NAME is not registered.  Returns the plist."
  (let ((ws (or (gethash name workspace--registry)
                (error "Not a registered fixture workspace: %S" name))))
    (puthash name (workspace--mark-broken ws) workspace--registry)
    (gethash name workspace--registry)))


;;; The bookmark-jump harness
;;
;; The ONE harness of the jump specs (portable-bookmarks-jump-spec.el,
;; -missing-repo-spec.el, -off-workspace-spec.el): build the world, set a
;; bookmark through the real stamp seam, jump through the real entry
;; points with only the prompts stood in for, and ask what a jump left
;; visited.  The counting validator of portable-bookmarks-spec.el ("finds
;; no inline stand-in in any spec file") fails the suite when a spec file
;; defines its own `--hermetic' macro or `--visits-p' predicate instead of
;; using these.

(defconst workspace-bookmark-fixtures-jump-contents
  "line one\nline two\nline three\n"
  "Contents of the README `workspace-bookmark-fixtures-jump-world' seeds.
Identical in every checkout of the world's repository.")

(defconst workspace-bookmark-fixtures-jump-position 10
  "A position inside `workspace-bookmark-fixtures-jump-contents' (on line 2).
Where `workspace-bookmark-fixtures-set-bookmark' sets a bookmark in a file.")

(defvar workspace-bookmark-fixtures-chooser-prompts nil
  "(PROMPT CHOICES) pairs the chooser stand-in received, newest first.
Reset at the start of every `workspace-bookmark-fixtures-jump'.")

(defvar workspace-bookmark-fixtures-workspace-prompts nil
  "(PROMPT . NAMES) of the workspace prompts a jump made, newest first.
NAMES are the candidates in completion order.  Reset at the start of
every `workspace-bookmark-fixtures-jump'.")

(defmacro workspace-bookmark-fixtures-hermetic (&rest body)
  "Run one spec's BODY with nothing but the fixtures' world in view.

`jf/emacs-dir' is nil unless BODY rebinds it, so the configuration
checkout running the suite is not a canonical checkout in view; the
persistence file lives in a fixtures temp directory (made on first
use), so an autosave BODY triggers never writes the real one; the
identity memo is cleared on exit; every buffer BODY leaves behind is
killed before the `after-each' teardown deletes the directories those
buffers visit.  Registry isolation is NOT this macro's business: the
describe-level `workspace-bookmark-fixtures-with-empty-registry' owns it.

BODY may begin with keyword options:

  :integrations FORM       `workspace--integrations' is bound to FORM's
                           value for BODY.
  :current-workspace FORM  `workspace-bookmark--current-workspace'
                           answers FORM's value for BODY."
  (declare (indent 0) (debug (&rest form)))
  (let ((options nil))
    (while (keywordp (car body))
      (push (cons (pop body) (pop body)) options))
    (let ((before (make-symbol "before"))
          (state-dir (make-symbol "state-dir"))
          (state-directory (make-symbol "state-directory"))
          (integrations (assq :integrations options))
          (current (assq :current-workspace options)))
      (dolist (option options)
        (unless (memq (car option) '(:integrations :current-workspace))
          (error "Unknown hermetic option %S" (car option))))
      `(let* ((jf/emacs-dir nil)
              ,@(and integrations `((workspace--integrations ,(cdr integrations))))
              (,before (buffer-list))
              (,state-dir nil)
              (,state-directory
               (lambda ()
                 (or ,state-dir
                     (setq ,state-dir (workspace-bookmark-fixtures-temp-dir
                                       "wsbm-state-"))))))
         (cl-letf (((symbol-function 'workspace--state-directory)
                    ,state-directory)
                   ((symbol-function 'workspace--state-file)
                    (lambda ()
                      (expand-file-name "workspaces.eld"
                                        (funcall ,state-directory))))
                   ,@(and current
                          `(((symbol-function 'workspace-bookmark--current-workspace)
                             (let ((value ,(cdr current))) (lambda () value))))))
           (unwind-protect (progn ,@body)
             (dolist (buffer (buffer-list))
               (unless (memq buffer ,before)
                 (with-current-buffer buffer (set-buffer-modified-p nil))
                 (kill-buffer buffer)))
             (workspace-bookmark-clear-identity-cache)))))))

(cl-defun workspace-bookmark-fixtures-jump-world
    (prefix remote &key (holding '("a" "b")) empty files)
  "Build a jump spec's world and return it as a plist.

One repository whose `origin' is REMOTE, seeded on `main' with a
README.md holding `workspace-bookmark-fixtures-jump-contents', then with
FILES, a list of (RELPATH . CONTENTS) committed one at a time.  For each
LETTER of HOLDING (strings), workspace PREFIX-LETTER is registered with a
fresh home holding a linked worktree of the repository at HOME/proj, on
branch LETTER; for each LETTER of EMPTY, workspace PREFIX-LETTER is
registered with a home holding nothing.  Workspaces are registered in
that order.  Keys: :repo, :home-LETTER for every workspace and
:wt-LETTER for every holder.

Call it from the describe's `workspace-bookmark-fixtures-with-empty-registry'
body, so every spec gets a fresh world against an empty registry."
  (let* ((repo (workspace-bookmark-fixtures-make-repo
                :remote remote :contents workspace-bookmark-fixtures-jump-contents))
         (homes (mapcar (lambda (letter)
                          (cons letter (workspace-bookmark-fixtures-make-workspace
                                        (concat prefix "-" letter))))
                        (append holding empty)))
         (world (list :repo repo)))
    (dolist (file files)
      (workspace-bookmark-fixtures-commit-file repo (car file) (cdr file)))
    (dolist (home homes)
      (setq world (append world (list (intern (concat ":home-" (car home)))
                                      (cdr home)))))
    (dolist (letter holding)
      (setq world
            (append world
                    (list (intern (concat ":wt-" letter))
                          (workspace-bookmark-fixtures-add-worktree
                           repo (expand-file-name
                                 "proj" (cdr (assoc letter homes)))
                           :branch letter)))))
    world))

(defun workspace-bookmark-fixtures-set-bookmark (path name)
  "Set bookmark NAME on PATH through `bookmark-set'; return the stored record.

PATH is visited (a directory in Dired), point is put at
`workspace-bookmark-fixtures-jump-position' in a file, the bookmark is
set by the real API — so the real stamp seam runs — and the buffer is
killed so a later jump has to visit PATH afresh."
  (let ((buffer (if (file-directory-p path)
                    (dired-noselect path)
                  (find-file-noselect path))))
    (with-current-buffer buffer
      (unless (file-directory-p path)
        (goto-char workspace-bookmark-fixtures-jump-position))
      (let ((inhibit-message t))
        (bookmark-set name)))
    (kill-buffer buffer)
    (bookmark-get-bookmark name)))

(defun workspace-bookmark-fixtures-prompted! (&rest args)
  "Fail the spec: a prompt was reached that the spec did not provide for.
ARGS are the prompt's arguments, named in the error."
  (error "Prompted: %S" args))

(defun workspace-bookmark-fixtures-answering (keys &optional before)
  "Return a `read-multiple-choice' stand-in answering the first of KEYS offered.

KEYS is one key, or a list of keys in order of preference: a spec that
wants add-worktree takes it whenever it is on the menu, so a gate that
wrongly offers it is caught by the command being CALLED, not only by
the option set.  BEFORE, a function of no arguments, runs while the
prompt is up: what the world does between the offer and the user's
answer, e.g. deleting a workspace home.  Records (PROMPT CHOICES) in
`workspace-bookmark-fixtures-chooser-prompts' and returns the offered
entry; signals when none of KEYS is offered, so a spec cannot pass by
answering something else."
  (let ((keys (if (listp keys) keys (list keys))))
    (lambda (prompt choices &rest _)
      (push (list prompt choices) workspace-bookmark-fixtures-chooser-prompts)
      (when before (funcall before))
      (or (seq-some (lambda (key) (assq key choices)) keys)
          (error "Option %s is not offered; offered: %S"
                 (mapconcat #'char-to-string keys " or ")
                 (mapcar #'cadr choices))))))

(defun workspace-bookmark-fixtures-offered-keys ()
  "Return the keys the most recent chooser prompt offered, in order."
  (mapcar #'car (cadr (car workspace-bookmark-fixtures-chooser-prompts))))

(defun workspace-bookmark-fixtures-prompt-text ()
  "Return the most recent chooser prompt's text."
  (car (car workspace-bookmark-fixtures-chooser-prompts)))

(cl-defun workspace-bookmark-fixtures-jump
    (name &key (workspace nil workspace-p) (choose nil choose-p)
          (pick nil pick-p) jumper)
  "Jump to bookmark NAME through the real jump path; return the buffer left current.

  :workspace W   `workspace--current-name' answers W (a name, or nil)
                 for the jump.  Without it the current workspace is the
                 real one, the selected tab's.
  :choose KEYS   the missing-repo chooser (`read-multiple-choice') is
                 answered: KEYS is a key or a list of keys in order of
                 preference (`workspace-bookmark-fixtures-answering'), or
                 a function to use as `read-multiple-choice' itself.
  :pick ANSWER   the off-workspace workspace prompt (`completing-read')
                 answers ANSWER, a workspace name, or `quit' to quit it;
                 its candidates are recorded in
                 `workspace-bookmark-fixtures-workspace-prompts'.
  :jumper FN     the entry point; `bookmark-jump' by default.

A jump that answers a prompt (:choose or :pick) runs with the prompt
guard answering as an interactive Emacs would: whatever
`workspace-bookmark--can-prompt-p' is at call time (the real one, or a
spec's stub) is asked with `noninteractive' masked, so it still refuses
under `inhibit-interaction' and while a layout leaf is being restored.
With :pick and no :choose, reaching the chooser fails the spec.  With
neither, the guard and both prompts are left as they are, so a spec's
own stubs of them hold.  `y-or-n-p' and `yes-or-no-p' always fail the
spec: the seam must neither ask nor let the host's relocate offer
through.  Both prompt records are reset first."
  (setq workspace-bookmark-fixtures-chooser-prompts nil
        workspace-bookmark-fixtures-workspace-prompts nil)
  (let ((guard (symbol-function 'workspace-bookmark--can-prompt-p))
        (current (symbol-function 'workspace--current-name))
        (chooser (symbol-function 'read-multiple-choice))
        (reader (symbol-function 'completing-read)))
    (cl-letf (((symbol-function 'workspace--current-name)
               (if workspace-p (lambda () workspace) current))
              ((symbol-function 'workspace-bookmark--can-prompt-p)
               (if (or choose-p pick-p)
                   (lambda () (let ((noninteractive nil)) (funcall guard)))
                 guard))
              ((symbol-function 'read-multiple-choice)
               (cond (choose-p (if (functionp choose)
                                   choose
                                 (workspace-bookmark-fixtures-answering choose)))
                     (pick-p #'workspace-bookmark-fixtures-prompted!)
                     (t chooser)))
              ((symbol-function 'completing-read)
               (if pick-p
                   (lambda (prompt collection &rest _)
                     (push (cons prompt (all-completions "" collection))
                           workspace-bookmark-fixtures-workspace-prompts)
                     (if (eq pick 'quit) (signal 'quit nil) pick))
                 reader))
              ((symbol-function 'y-or-n-p)
               #'workspace-bookmark-fixtures-prompted!)
              ((symbol-function 'yes-or-no-p)
               #'workspace-bookmark-fixtures-prompted!))
      (let ((inhibit-message t))
        (funcall (or jumper #'bookmark-jump) name))
      (current-buffer))))

(defun workspace-bookmark-fixtures-decline (name &rest options)
  "Jump to NAME as `workspace-bookmark-fixtures-jump' does, expecting a `user-error'.
OPTIONS are that function's keyword arguments.  Returns the error's
message; fails the spec when nothing was signalled."
  (let ((message
         (condition-case err
             (progn (apply #'workspace-bookmark-fixtures-jump name options) nil)
           (user-error (cadr err)))))
    (expect message :not :to-be nil)
    message))

(defun workspace-bookmark-fixtures-visits-p (path)
  "Non-nil when some live buffer visits PATH (a file or a directory).
A directory counts as visited by a Dired buffer of it, compared with
`workspace--same-file-p', so a directory bookmark's jump is checked as
really as a file's."
  (and (or (find-buffer-visiting path)
           (and (file-directory-p path)
                (seq-some (lambda (buffer)
                            (with-current-buffer buffer
                              (and (derived-mode-p 'dired-mode)
                                   (workspace--same-file-p default-directory path))))
                          (buffer-list))))
       t))


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



;;; Stand-ins for the seam's collaborators
;;
;; The shared forms of "a record with no stamp", "a collaborator that
;; fails" and "the seam's openers replaced by markers".  Specs use these
;; rather than an inline copy: the counting validator of
;; portable-bookmarks-spec.el ("finds no inline stand-in in any spec
;; file") fails the suite when a spec file binds a signalling collaborator
;; of its own.

(defun workspace-bookmark-fixtures-strip-stamp (record)
  "Remove the three stamped properties from bookmark RECORD, in place; return RECORD.

RECORD may be (NAME . ALIST) or (NAME ALIST).  What remains is the
stock record `bookmark-set' stores before seam 1 stamps it, so
`workspace-bookmark--stamped-coordinates' answers nil for it.  Signals
when the stamp is still readable afterwards."
  (let* ((alist (bookmark-get-bookmark-record record))
         (props (list workspace-bookmark-repo-property
                      workspace-bookmark-relpath-property
                      workspace-bookmark-format-property))
         (stripped (seq-remove (lambda (cell) (memq (car-safe cell) props))
                               alist)))
    (if (eq alist (cdr record))
        (setcdr record stripped)
      (setcar (cdr record) stripped))
    (when (workspace-bookmark--stamped-coordinates record)
      (error "Fixture could not strip the stamp from %S" (car record)))
    record))

(defmacro workspace-bookmark-fixtures-signalling (function message &rest body)
  "Run BODY with FUNCTION (a quoted symbol) replaced by one that signals MESSAGE.
Every call of FUNCTION inside BODY signals an `error' whose message is
MESSAGE, whatever the arguments — the stand-in for a collaborator that
fails.  Scoped to BODY with `cl-letf'."
  (declare (indent 2) (debug (sexp sexp body)))
  `(cl-letf (((symbol-function ,function)
              (lambda (&rest _) (error "%s" ,message))))
     ,@body))

(defconst workspace-bookmark-fixtures-copy-marker 'workspace-bookmark-fixtures-copy
  "What the marker `workspace-bookmark--plan-to-open' hands the host instead of a copy.")

(defconst workspace-bookmark-fixtures-offer-marker 'workspace-bookmark-fixtures-offered
  "The COPY of the (view COPY . CHECKOUT) plan the marker chooser returns.")

(defmacro workspace-bookmark-fixtures-with-marker-plans (&rest body)
  "Run BODY with the seam's two opening collaborators replaced by markers.

`workspace-bookmark--plan-to-open' returns (KIND . MARKER) with
`workspace-bookmark-fixtures-copy-marker', pre-flighting and copying
nothing; `workspace-bookmark--offer-missing-repo' returns
\(view MARKER . \"canonical\") with
`workspace-bookmark-fixtures-offer-marker', prompting for nothing.  For
specs of the outcome-to-plan mapping alone, which need to see WHICH
collaborator a cell reached, not what it does."
  (declare (indent 0) (debug (body)))
  `(cl-letf (((symbol-function 'workspace-bookmark--plan-to-open)
              (lambda (_record _resolution kind)
                (cons kind workspace-bookmark-fixtures-copy-marker)))
             ((symbol-function 'workspace-bookmark--offer-missing-repo)
              (lambda (_record _resolution)
                (cons 'view (cons workspace-bookmark-fixtures-offer-marker
                                  "canonical")))))
     ,@body))

(provide 'portable-bookmarks-fixtures)
;;; portable-bookmarks-fixtures.el ends here
