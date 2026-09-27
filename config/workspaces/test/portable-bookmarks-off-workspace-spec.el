;;; portable-bookmarks-off-workspace-spec.el --- Off-workspace restore-then-jump specs -*- lexical-binding: t; -*-

;; Specs for the off-workspace hand-off of seam 2: a stamped bookmark
;; jumped to from a tab that is not a workspace prompts over the
;; REGISTRY, restores the chosen workspace through `workspace-restore',
;; then resolves against it; and for the layout reincarnation chain's
;; decline gate in layouts.el, which the same fixtures reach end to end.
;;
;; * BEHAVIORAL — real `git' (one repository with a remote, a linked
;;   worktree in each of two registered workspace homes), records stamped
;;   by the real stamp seam through `bookmark-set', REAL TABS — unlike the
;;   jump spec, nothing here binds `workspace--current-name': every spec
;;   starts on the frame's first, unregistered tab, and `workspace-restore'
;;   selects or creates the workspace's tab for real — the real restore
;;   path with its deferred `window-state-put', and the real autosave
;;   trigger (a real `tab-bar-select-tab').  Only the prompt is stubbed
;;   (`completing-read'), scoped per spec with `cl-letf'; any other
;;   prompt fails the spec.  Batch has no one to ask, so the jump helper
;;   answers the prompt guard as an interactive Emacs would: the real
;;   `workspace-bookmark--can-prompt-p' with `noninteractive' masked,
;;   which still refuses while a layout leaf is being restored.
;;
;; * UNIT — candidate ordering and exclusion, C-g and the prompt guard at
;;   the workspace prompt, and the fallback when no workspace will serve
;;   the jump.
;;
;; Registry isolation goes through the fixtures module's describe-level
;; macro `workspace-bookmark-fixtures-with-empty-registry'
;; (register/invariant/spec-registry-isolation-via-fixtures); this file
;; never touches `workspace--registry' itself.  The persistence file is
;; pointed at a fixtures temp directory for every spec, because the
;; autosaves this file triggers on purpose flush to disk; the real
;; workspaces.eld is never written.

(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'bookmark)
(require 'tab-bar)
(require 'map)

(defconst portable-bookmarks-off-workspace-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

;; Under the harness init.el has loaded the workspaces package.  Load the
;; core modules by relative path only when it has not, so this file can
;; be run against the tangled sources alone.
(unless (fboundp 'workspace-restore)
  (let ((dir portable-bookmarks-off-workspace-spec--this-dir))
    (load (expand-file-name "../data-model.el" dir))
    (load (expand-file-name "../tabs.el" dir))
    (unless (featurep 'bufferlo)
      (defalias 'bufferlo-mode (lambda (&optional _) nil))
      (provide 'bufferlo))
    (load (expand-file-name "../buffer-membership.el" dir))
    (load (expand-file-name "../layouts.el" dir))
    (load (expand-file-name "../persistence.el" dir))
    (load (expand-file-name "../workspaces.el" dir))))

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-off-workspace-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-off-workspace-spec--this-dir))

(defvar jf/emacs-dir)
(defvar workspace-home-builder)
(defvar workspace-anti-save-predicates)
(defvar workspace--restoring-layout-leaf)


;;; Helpers

(defconst portable-bookmarks-off-workspace-spec--remote "git@example.com:fixtures/off.git"
  "Remote URL given to the fixture repository.")

(defconst portable-bookmarks-off-workspace-spec--identity "example.com/fixtures/off"
  "The identity `portable-bookmarks-off-workspace-spec--remote' normalizes to.")

(defconst portable-bookmarks-off-workspace-spec--contents "line one\nline two\nline three\n"
  "Contents of the seeded README, identical in every checkout.")

(defconst portable-bookmarks-off-workspace-spec--position 10
  "A position inside `portable-bookmarks-off-workspace-spec--contents' (on line 2).")

(defvar portable-bookmarks-off-workspace-spec--prompted nil
  "The workspace prompts a spec's jump made, newest first.
Each entry is (PROMPT . NAMES), NAMES the candidates in completion order.")

(defun portable-bookmarks-off-workspace-spec--reset-frame ()
  "Leave the frame on its first tab with one window and no pending restore.
Run from the fixtures macro's setup body, so the registry is already
the fresh, empty one when the tab switch consults it."
  (tab-bar-select-tab 1)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (delete-other-windows)
  (workspace-bookmark-fixtures-reset-jump-state)
  (setq portable-bookmarks-off-workspace-spec--prompted nil))

(defmacro portable-bookmarks-off-workspace-spec--hermetic (&rest body)
  "Run one spec's BODY with nothing but the fixtures' world in view.

`jf/emacs-dir' is nil unless BODY rebinds it, so the configuration
checkout running the suite is not a canonical checkout in view; the
persistence file lives in a fixtures temp directory, so the autosaves
BODY triggers never touch the real one; the identity memo is cleared on
exit; every buffer BODY leaves behind is killed before the `after-each'
teardown deletes the directories those buffers visit."
  (declare (indent 0))
  `(let* ((jf/emacs-dir nil)
          (before (buffer-list))
          (state-dir (workspace-bookmark-fixtures-temp-dir "wsbm-off-state-")))
     (cl-letf (((symbol-function 'workspace--state-directory)
                (lambda () state-dir))
               ((symbol-function 'workspace--state-file)
                (lambda () (expand-file-name "workspaces.eld" state-dir))))
       (unwind-protect (progn ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))
         (workspace-bookmark-clear-identity-cache)))))

(defun portable-bookmarks-off-workspace-spec--world ()
  "Build the standard world and return it as a plist.

One repository with a remote, seeded with README.md on `main';
workspace A (\"wsbm-off-a\") and workspace B (\"wsbm-off-b\"), each
holding a linked worktree of it under its home.  Neither has a live
tab or a saved layout.  Keys: :repo :home-a :home-b :wt-a :wt-b."
  (let* ((repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-off-workspace-spec--remote
                :contents portable-bookmarks-off-workspace-spec--contents))
         (home-a (workspace-bookmark-fixtures-make-workspace "wsbm-off-a"))
         (home-b (workspace-bookmark-fixtures-make-workspace "wsbm-off-b")))
    (list :repo repo :home-a home-a :home-b home-b
          :wt-a (workspace-bookmark-fixtures-add-worktree
                 repo (expand-file-name "proj" home-a) :branch "a")
          :wt-b (workspace-bookmark-fixtures-add-worktree
                 repo (expand-file-name "proj" home-b) :branch "b"))))

(defun portable-bookmarks-off-workspace-spec--set (path name)
  "Set bookmark NAME on PATH through `bookmark-set'; return the stored record.
PATH is visited, point is put at the spec position, the bookmark is set
by the real API — so the real stamp seam runs — and the buffer is
killed so a later jump has to visit PATH afresh."
  (let ((buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (goto-char portable-bookmarks-off-workspace-spec--position)
      (let ((inhibit-message t))
        (bookmark-set name)))
    (kill-buffer buffer)
    (bookmark-get-bookmark name)))

(defun portable-bookmarks-off-workspace-spec--prompted! (&rest args)
  "Fail the spec: a prompt was reached that the spec did not provide for."
  (error "Prompted: %S" args))

(defun portable-bookmarks-off-workspace-spec--jump (name answer &optional chooser)
  "Jump to bookmark NAME from the current tab; return the buffer left current.

ANSWER is what the workspace prompt answers: a workspace name, or the
symbol `quit' to quit it.  The prompt's candidates are recorded in
`portable-bookmarks-off-workspace-spec--prompted'.  CHOOSER, when
given, stands in for `read-multiple-choice' (the missing-repo
chooser's prompt).  Any other prompt fails the spec.

The prompt guard answers as in an interactive Emacs: whatever
`workspace-bookmark--can-prompt-p' is at call time (the real one, or a
spec's stub) is asked with `noninteractive' masked, so it still refuses
under `inhibit-interaction' and while a layout leaf is being restored."
  (cl-letf* ((guard (symbol-function 'workspace-bookmark--can-prompt-p))
             ((symbol-function 'workspace-bookmark--can-prompt-p)
              (lambda () (let ((noninteractive nil)) (funcall guard))))
             ((symbol-function 'completing-read)
             (lambda (prompt collection &rest _)
               (push (cons prompt (all-completions "" collection))
                     portable-bookmarks-off-workspace-spec--prompted)
               (if (eq answer 'quit) (signal 'quit nil) answer)))
            ((symbol-function 'y-or-n-p)
             #'portable-bookmarks-off-workspace-spec--prompted!)
            ((symbol-function 'yes-or-no-p)
             #'portable-bookmarks-off-workspace-spec--prompted!)
            ((symbol-function 'read-multiple-choice)
             (or chooser #'portable-bookmarks-off-workspace-spec--prompted!)))
    (bookmark-jump name)
    (current-buffer)))

(defun portable-bookmarks-off-workspace-spec--decline (name answer &optional chooser)
  "Jump to NAME answering ANSWER at the prompt, expecting a `user-error'.
CHOOSER is passed on to `portable-bookmarks-off-workspace-spec--jump'.
Returns the error's message.  Fails the spec if nothing was signalled."
  (let ((message
         (condition-case err
             (progn (portable-bookmarks-off-workspace-spec--jump name answer chooser) nil)
           (user-error (cadr err)))))
    (expect message :not :to-be nil)
    message))

(defmacro portable-bookmarks-off-workspace-spec--recording-messages (var &rest body)
  "Run BODY with every `message' pushed onto VAR (a list) instead of shown."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'message)
              (lambda (format-string &rest args)
                (when format-string
                  (push (condition-case nil
                            (apply #'format-message format-string args)
                          (error format-string))
                        ,var))
                nil)))
     ,@body))

(defun portable-bookmarks-off-workspace-spec--layout-beside-scratch (path)
  "Capture a two-window state: PATH visited in the selected window, *scratch* beside it.
The leaf record for PATH is made by the real stamp seam.  PATH's buffer
is killed and the split undone before returning, so a restore has to
reincarnate it."
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (split-window-right)
  (find-file path)
  (prog1 (workspace--capture-frameset)
    (kill-buffer (find-buffer-visiting path))
    (delete-other-windows)))

(defun portable-bookmarks-off-workspace-spec--layout-of-two (left right)
  "Capture a two-window state: LEFT visited in the selected window, RIGHT beside it.
Both leaf records are made by the real stamp seam.  Both buffers are
killed and the split undone before returning, so a restore has to
reincarnate them through the seam."
  (delete-other-windows)
  (find-file left)
  (let ((window (selected-window)))
    (select-window (split-window-right))
    (find-file right)
    (select-window window))
  (prog1 (workspace--capture-frameset)
    (kill-buffer (find-buffer-visiting left))
    (kill-buffer (find-buffer-visiting right))
    (delete-other-windows)))

(defun portable-bookmarks-off-workspace-spec--leaf-files (state)
  "Return the saved filenames of window-state STATE's leaves, in walk order."
  (let (files)
    (workspace--window-state-walk-leaves
     state
     (lambda (leaf)
       (let ((ws-buffer (map-elt (map-elt (cdr leaf) 'parameters) 'workspace-buffer)))
         (push (and ws-buffer (workspace-buffer-filename ws-buffer)) files))
       leaf))
    (nreverse files)))

(defun portable-bookmarks-off-workspace-spec--window-files ()
  "Return the file each window of the selected frame shows, selected window first."
  (mapcar (lambda (window)
            (let ((file (buffer-file-name (window-buffer window))))
              (and file (file-truename file))))
          (window-list nil 'no-mini (selected-window))))

(defun portable-bookmarks-off-workspace-spec--drain-timers ()
  "Run the event loop briefly so pending immediate timers fire.
`workspace--restore-frameset' defers `window-state-put' with
`run-at-time' nil, and the off-workspace flow's finish step is queued
`workspace-bookmark--finish-delay' seconds after it; the wait covers
both."
  (sit-for 0)
  (sit-for 0)
  (accept-process-output nil 0.05))

(defun portable-bookmarks-off-workspace-spec--visits-p (path)
  "Non-nil when some live buffer visits PATH."
  (and (find-buffer-visiting path) t))

(defun portable-bookmarks-off-workspace-spec--layout-for (name)
  "Return the recent layout plist of the registered workspace NAME."
  (let* ((ws (gethash name workspace--registry))
         (group-name (workspace--recent-group ws))
         (group (and group-name (workspace--find-group ws group-name))))
    (and group (workspace--group-recent-layout group))))

(defun portable-bookmarks-off-workspace-spec--give-saved-layout (name state)
  "Store window-state STATE as workspace NAME's saved \"home\" layout."
  (let* ((ws (gethash name workspace--registry))
         (ws (workspace--upsert-group ws "home" (workspace--layout-make state)))
         (ws (workspace--set-recent-group ws "home")))
    (puthash name ws workspace--registry)))

(defun portable-bookmarks-off-workspace-spec--two-window-state ()
  "Capture a two-window window-state of the current tab, then undo the split."
  (delete-other-windows)
  (split-window-right)
  (prog1 (workspace--capture-frameset)
    (delete-other-windows)))

(defun portable-bookmarks-off-workspace-spec--window-showing (name)
  "Return the window showing the buffer named NAME on the selected frame, or nil."
  (seq-find (lambda (window)
              (equal (buffer-name (window-buffer window)) name))
            (window-list nil 'no-mini)))


;;; Behavioral — restore, then resolve

(describe "off-workspace jump: restore, then resolve (real git, real tabs)"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (portable-bookmarks-off-workspace-spec--reset-frame)
    (setq world (portable-bookmarks-off-workspace-spec--world)))

  (it "prompts over the registry before resolving, restores the chosen workspace, and opens its copy in the selected window — not the stored path"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (portable-bookmarks-off-workspace-spec--set b-path "readme"))
             (snapshot (copy-tree stored)))
        ;; Preconditions: off-workspace, and the target has no live tab.
        (expect (workspace--current-name) :to-be nil)
        (expect (workspace--tab-index-for "wsbm-off-a") :to-be nil)
        (let ((buffer (portable-bookmarks-off-workspace-spec--jump "readme" "wsbm-off-a")))
          (expect (length portable-bookmarks-off-workspace-spec--prompted) :to-equal 1)
          (expect (cdr (car portable-bookmarks-off-workspace-spec--prompted))
                  :to-equal '("wsbm-off-a" "wsbm-off-b"))
          (expect (workspace--current-name) :to-equal "wsbm-off-a")
          (expect (workspace--tab-index-for "wsbm-off-a") :not :to-be nil)
          (expect (workspace--same-file-p (buffer-file-name buffer) a-path) :to-be-truthy)
          (expect (with-current-buffer buffer (point))
                  :to-equal portable-bookmarks-off-workspace-spec--position))
        (expect (portable-bookmarks-off-workspace-spec--visits-p b-path) :to-be nil)
        (portable-bookmarks-off-workspace-spec--drain-timers)
        ;; The selected window of the restored tab shows the file; no split.
        (expect (workspace--same-file-p (buffer-file-name (window-buffer (selected-window))) a-path)
                :to-be-truthy)
        (expect (length (window-list nil 'no-mini)) :to-equal 1)
        (expect workspace-bookmark--awaiting-jump-target :to-be nil)
        (expect workspace-bookmark--jump-target :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)
        (expect (bookmark-get-filename "readme") :to-equal b-path))))

  (it "offers a saved workspace that has no live tab beside one that has, and materializes it when chosen"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b))))
        (portable-bookmarks-off-workspace-spec--set a-path "readme")
        ;; A gets a live tab; B stays registry-only.  Back to the plain tab.
        (workspace-restore "wsbm-off-a")
        (portable-bookmarks-off-workspace-spec--drain-timers)
        (tab-bar-select-tab 1)
        (expect (workspace--current-name) :to-be nil)
        (expect (workspace--tab-index-for "wsbm-off-b") :to-be nil)
        (let ((tabs-before (length (frame-parameter nil 'tabs)))
              (buffer (portable-bookmarks-off-workspace-spec--jump "readme" "wsbm-off-b")))
          (expect (cdr (car portable-bookmarks-off-workspace-spec--prompted))
                  :to-equal '("wsbm-off-a" "wsbm-off-b"))
          (expect (workspace--current-name) :to-equal "wsbm-off-b")
          (expect (length (frame-parameter nil 'tabs)) :to-equal (1+ tabs-before))
          (expect (workspace--same-file-p (buffer-file-name buffer) b-path) :to-be-truthy))
        (portable-bookmarks-off-workspace-spec--drain-timers)
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "continues through the seam's normal path: a chosen workspace that lacks the file declines not-on-branch, and the pending jump state is still cleared"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-only (workspace-bookmark-fixtures-commit-file
                      (plist-get world :wt-a) "only-a.el"))
             (b-only (expand-file-name "only-a.el" (plist-get world :wt-b))))
        (portable-bookmarks-off-workspace-spec--set a-only "only-a")
        (expect (file-exists-p b-only) :to-be nil)
        (let ((message (portable-bookmarks-off-workspace-spec--decline "only-a" "wsbm-off-b")))
          (expect message :to-match "not on this branch")
          (expect message :to-match (regexp-quote "only-a.el")))
        ;; The restore happened (the seam resolved against B) ...
        (expect (workspace--current-name) :to-equal "wsbm-off-b")
        ;; The declined jump never reached the host, so nothing noted it.
        (expect workspace-bookmark--awaiting-jump-target :to-be t)
        (expect workspace-bookmark--jump-target :to-be nil)
        ;; ... nothing was opened, in either checkout ...
        (expect (portable-bookmarks-off-workspace-spec--visits-p a-only) :to-be nil)
        (expect (portable-bookmarks-off-workspace-spec--visits-p b-only) :to-be nil)
        (expect (file-exists-p b-only) :to-be nil)
        ;; ... and the finish step still clears the pending state.
        (portable-bookmarks-off-workspace-spec--drain-timers)
        (expect workspace-bookmark--awaiting-jump-target :to-be nil))))

  (it "paints the saved layout first, then shows the file in its selected window; nothing suppresses autosave, and a later tab switch records the jump's arrangement as :working-state"
    ;; User decision 2026-09-27: a jump-triggered restore is remembered
    ;; like any find-file in the workspace.  The autosave is the real
    ;; one: `tab-bar-select-tab' leaving the workspace.
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b))))
        (portable-bookmarks-off-workspace-spec--give-saved-layout
         "wsbm-off-a" (portable-bookmarks-off-workspace-spec--two-window-state))
        (portable-bookmarks-off-workspace-spec--set b-path "readme")
        (let ((saved-before (workspace--layout-saved-state
                             (portable-bookmarks-off-workspace-spec--layout-for "wsbm-off-a")))
              (events nil))
          (expect saved-before :not :to-be nil)
          (cl-letf* ((put (symbol-function 'window-state-put))
                     ((symbol-function 'window-state-put)
                      (lambda (&rest args) (push 'paint events) (apply put args)))
                     (finish (symbol-function 'workspace-bookmark--finish-restore))
                     ((symbol-function 'workspace-bookmark--finish-restore)
                      (lambda () (push 'finish events) (funcall finish))))
            (portable-bookmarks-off-workspace-spec--jump "readme" "wsbm-off-a")
            (expect (workspace--current-name) :to-equal "wsbm-off-a")
            ;; Nothing of this module's stands between the jump and an autosave.
            (expect (run-hook-with-args-until-success 'workspace-anti-save-predicates)
                    :to-be nil)
            (portable-bookmarks-off-workspace-spec--drain-timers))
          (expect (reverse events) :to-equal '(paint finish))
          ;; D9: the layout is painted (two windows), THEN the file replaces
          ;; the selected window — plain same-window display, no split.
          (expect (length (window-list nil 'no-mini)) :to-equal 2)
          (expect (workspace--same-file-p (buffer-file-name (window-buffer (selected-window))) a-path)
                  :to-be-truthy)
          ;; No autosave has run yet.
          (expect (workspace--layout-working-state
                   (portable-bookmarks-off-workspace-spec--layout-for "wsbm-off-a"))
                  :to-be nil)
          ;; The next ordinary autosave — leaving the workspace's tab —
          ;; records the arrangement on screen; :saved-state is untouched.
          (tab-bar-select-tab 1)
          (expect (workspace--current-name) :to-be nil)
          (let* ((layout (portable-bookmarks-off-workspace-spec--layout-for "wsbm-off-a"))
                 (files (portable-bookmarks-off-workspace-spec--leaf-files
                         (workspace--layout-working-state layout))))
            (expect (length files) :to-equal 2)
            (expect (seq-some (lambda (file) (and file (workspace--same-file-p file a-path))) files)
                    :to-be-truthy)
            (expect (workspace--layout-saved-state layout) :to-equal saved-before))))))

  (it "keeps the paint-then-show order when the two timers read the same clock value"
    ;; timer.el fires equal-time timers NEWEST first.  Every `run-at-time'
    ;; call with a nil TIME is given one fixed time, so the restore's paint
    ;; and anything else queued "now" tie; the finish step must still fire
    ;; after the paint.
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (fixed (current-time)))
        (portable-bookmarks-off-workspace-spec--give-saved-layout
         "wsbm-off-a" (portable-bookmarks-off-workspace-spec--two-window-state))
        (portable-bookmarks-off-workspace-spec--set b-path "readme")
        (cl-letf* ((run-at (symbol-function 'run-at-time))
                   ((symbol-function 'run-at-time)
                    (lambda (time &rest args) (apply run-at (or time fixed) args))))
          (portable-bookmarks-off-workspace-spec--jump "readme" "wsbm-off-a"))
        (portable-bookmarks-off-workspace-spec--drain-timers)
        (expect (length (window-list nil 'no-mini)) :to-equal 2)
        ;; The selected window (listed first) shows the file.
        (expect (car (portable-bookmarks-off-workspace-spec--window-files))
                :to-equal (file-truename a-path)))))

  (it "leaves the painted layout alone when the jump declines after restoring a layout of bookmarked leaves: the restore's own leaf jumps are never the jump's target"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-readme (expand-file-name "README.md" (plist-get world :wt-a)))
             (a-other (workspace-bookmark-fixtures-commit-file
                       (plist-get world :wt-a) "other.el" "(other)\n"))
             (b-only (workspace-bookmark-fixtures-commit-file
                      (plist-get world :wt-b) "only-b.el" "(only-b)\n")))
        ;; A's layout: README (selected) | other.el — both bookmarked leaves
        ;; that the restore reincarnates through the seam.
        (portable-bookmarks-off-workspace-spec--give-saved-layout
         "wsbm-off-a" (portable-bookmarks-off-workspace-spec--layout-of-two a-readme a-other))
        (portable-bookmarks-off-workspace-spec--set b-only "only-b")
        (let ((message (portable-bookmarks-off-workspace-spec--decline "only-b" "wsbm-off-a")))
          (expect message :to-match "not on this branch"))
        (expect (workspace--current-name) :to-equal "wsbm-off-a")
        (portable-bookmarks-off-workspace-spec--drain-timers)
        ;; Exactly the layout as saved: README in the selected window, other.el beside it.
        (expect (portable-bookmarks-off-workspace-spec--window-files)
                :to-equal (list (file-truename a-readme) (file-truename a-other)))
        (expect (portable-bookmarks-off-workspace-spec--visits-p
                 (expand-file-name "only-b.el" (plist-get world :wt-a)))
                :to-be nil))))

  (it "shows the file when the jump starts from a buffer visiting it, which the new tab inherits"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (portable-bookmarks-off-workspace-spec--give-saved-layout
         "wsbm-off-a" (portable-bookmarks-off-workspace-spec--two-window-state))
        (portable-bookmarks-off-workspace-spec--set a-path "readme")
        ;; On the plain tab, looking at A's README; jump to its bookmark.
        (find-file a-path)
        (let ((tab-bar-new-tab-choice t))
          (portable-bookmarks-off-workspace-spec--jump "readme" "wsbm-off-a"))
        (expect (workspace--current-name) :to-equal "wsbm-off-a")
        (portable-bookmarks-off-workspace-spec--drain-timers)
        (expect (length (window-list nil 'no-mini)) :to-equal 2)
        ;; The selected window (listed first) shows the file.
        (expect (car (portable-bookmarks-off-workspace-spec--window-files))
                :to-equal (file-truename a-path)))))

  (it "declines, never falling through to the stored path, when the restore itself fails — and still clears the pending jump state"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (stored (portable-bookmarks-off-workspace-spec--set a-path "readme"))
             (snapshot (copy-tree stored)))
        (cl-letf (((symbol-function 'workspace-restore)
                   (lambda (&rest _) (error "Boom"))))
          (let ((message (portable-bookmarks-off-workspace-spec--decline "readme" "wsbm-off-b")))
            (expect message :to-match "could not establish a workspace")
            (expect message :to-match "Boom")))
        (expect (portable-bookmarks-off-workspace-spec--visits-p a-path) :to-be nil)
        (expect (workspace--current-name) :to-be nil)
        (portable-bookmarks-off-workspace-spec--drain-timers)
        (expect workspace-bookmark--awaiting-jump-target :to-be nil)
        (expect workspace-bookmark--jump-target :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)))))


;;; Unit — candidates

(describe "off-workspace jump: candidates"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (portable-bookmarks-off-workspace-spec--reset-frame)
    (setq world (portable-bookmarks-off-workspace-spec--world)))

  (it "excludes a broken workspace (its :home gone) from the candidates and from the prompt"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (home (workspace-bookmark-fixtures-make-workspace "wsbm-off-broken")))
        ;; What persistence does when a :home is missing at load: the
        ;; entry stays registered, flagged broken.
        (delete-directory home t)
        (puthash "wsbm-off-broken"
                 (workspace--mark-broken (gethash "wsbm-off-broken" workspace--registry))
                 workspace--registry)
        (expect (member "wsbm-off-broken" (workspace--registered-names)) :to-be-truthy)
        (expect (mapcar #'car (workspace-bookmark--offerable-workspaces
                               portable-bookmarks-off-workspace-spec--identity))
                :to-equal '("wsbm-off-a" "wsbm-off-b"))
        (portable-bookmarks-off-workspace-spec--set a-path "readme")
        (portable-bookmarks-off-workspace-spec--jump "readme" "wsbm-off-a")
        (expect (cdr (car portable-bookmarks-off-workspace-spec--prompted))
                :to-equal '("wsbm-off-a" "wsbm-off-b"))
        (portable-bookmarks-off-workspace-spec--drain-timers))))

  (it "puts workspaces holding a checkout of the identity first, in a legible, un-re-sortable order"
    (portable-bookmarks-off-workspace-spec--hermetic
      ;; Registry order becomes A, B, none, late; only `late' holds a checkout.
      (workspace-bookmark-fixtures-make-workspace "wsbm-off-none")
      (let ((home-late (workspace-bookmark-fixtures-make-workspace "wsbm-off-late")))
        (workspace-bookmark-fixtures-add-worktree
         (plist-get world :repo) (expand-file-name "proj" home-late) :branch "late"))
      (expect (workspace--registered-names)
              :to-equal '("wsbm-off-a" "wsbm-off-b" "wsbm-off-none" "wsbm-off-late"))
      (let* ((identity portable-bookmarks-off-workspace-spec--identity)
             (candidates (workspace-bookmark--offerable-workspaces identity))
             (table (workspace-bookmark--workspace-table candidates identity))
             (metadata (cdr (funcall table "" nil 'metadata)))
             (annotate (alist-get 'annotation-function metadata)))
        (expect candidates
                :to-equal '(("wsbm-off-a" . t) ("wsbm-off-b" . t)
                            ("wsbm-off-late" . t) ("wsbm-off-none")))
        (expect (all-completions "" table)
                :to-equal '("wsbm-off-a" "wsbm-off-b" "wsbm-off-late" "wsbm-off-none"))
        (expect (alist-get 'display-sort-function metadata) :to-be #'identity)
        (expect (alist-get 'cycle-sort-function metadata) :to-be #'identity)
        (expect (funcall annotate "wsbm-off-late") :to-match "holds a checkout of")
        (expect (funcall annotate "wsbm-off-late") :to-match (regexp-quote identity))
        (expect (funcall annotate "wsbm-off-none") :to-match "no checkout")
        (expect (funcall annotate "wsbm-off-none") :not :to-match "holds"))))

  (it "ends the jump on C-g at the workspace prompt: nothing restored, nothing opened, and no second prompt even where the fallback would ask"
    ;; With the canonical checkout in view the no-workspace fallback
    ;; would be the missing-repo chooser.  C-g is not "no workspace".
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (stored (portable-bookmarks-off-workspace-spec--set a-path "readme"))
             (snapshot (copy-tree stored))
             (tabs-before (length (frame-parameter nil 'tabs)))
             (jf/emacs-dir (plist-get world :repo))
             (offered nil)
             (quit-p nil))
        (spy-on 'workspace-restore :and-call-through)
        (cl-letf (((symbol-function 'workspace-bookmark--can-prompt-p) (lambda () t)))
          (condition-case nil
              (portable-bookmarks-off-workspace-spec--jump
               "readme" 'quit
               (lambda (prompt choices &rest _)
                 (push prompt offered)
                 (assq ?c choices)))
            (quit (setq quit-p t))))
        (expect quit-p :to-be t)
        (expect (length portable-bookmarks-off-workspace-spec--prompted) :to-equal 1)
        (expect offered :to-be nil)
        (expect 'workspace-restore :not :to-have-been-called)
        (expect (length (frame-parameter nil 'tabs)) :to-equal tabs-before)
        (expect (workspace--current-name) :to-be nil)
        (expect (portable-bookmarks-off-workspace-spec--visits-p a-path) :to-be nil)
        (expect (portable-bookmarks-off-workspace-spec--visits-p canonical-path) :to-be nil)
        (expect workspace-bookmark--awaiting-jump-target :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "puts no workspace prompt where no one can answer: declines with a message, restores nothing, opens nothing"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (stored (portable-bookmarks-off-workspace-spec--set a-path "readme"))
             (snapshot (copy-tree stored)))
        (spy-on 'workspace-restore :and-call-through)
        (cl-letf (((symbol-function 'workspace-bookmark--can-prompt-p) (lambda () nil)))
          (let ((message (portable-bookmarks-off-workspace-spec--decline "readme" "wsbm-off-a")))
            (expect message :to-match "no one can be asked")
            (expect message :to-match "nothing opened")))
        (expect portable-bookmarks-off-workspace-spec--prompted :to-be nil)
        (expect 'workspace-restore :not :to-have-been-called)
        (expect (workspace--current-name) :to-be nil)
        (expect (portable-bookmarks-off-workspace-spec--visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)))))


;;; Unit — the fallback with an empty registry

(describe "off-workspace jump: an empty answer at the workspace prompt"
  ;; The REAL `completing-read' (stock `completing-read-default'); only the
  ;; minibuffer read under it is answered, with empty input, as RET on an
  ;; empty minibuffer returns it.  `completing-read' then substitutes its
  ;; DEF, so "no workspace" is an empty answer only when no default is
  ;; offered — when no candidate holds a checkout.
  (workspace-bookmark-fixtures-with-empty-registry)

  (it "restores the default, the first holder, when a workspace holds a checkout"
    (let ((reads 0))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (setq reads (1+ reads)) "")))
        (let ((completing-read-function #'completing-read-default))
          (expect (workspace-bookmark--choose-workspace
                   '(("wsbm-off-holder" . t) ("wsbm-off-other"))
                   portable-bookmarks-off-workspace-spec--identity)
                  :to-equal "wsbm-off-holder")))
      (expect reads :to-equal 1)))

  (it "answers no workspace (nil) when no candidate holds a checkout, so no default is offered"
    (let ((reads 0))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (setq reads (1+ reads)) "")))
        (let ((completing-read-function #'completing-read-default))
          (expect (workspace-bookmark--choose-workspace
                   '(("wsbm-off-a") ("wsbm-off-b"))
                   portable-bookmarks-off-workspace-spec--identity)
                  :to-be nil)))
      (expect reads :to-equal 1))))

(describe "off-workspace jump: empty registry"
  :var (repo)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (portable-bookmarks-off-workspace-spec--reset-frame)
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-off-workspace-spec--remote
                :contents portable-bookmarks-off-workspace-spec--contents)))

  (it "asks for no workspace and hands the canonical checkout in view to the missing-repo chooser, offering only view read-only and cancel; cancel opens nothing"
    ;; The chooser is portable-bookmarks-missing-repo-spec.el's; here it
    ;; is reached from the off-workspace fallback with NO workspace
    ;; current, so `add-worktree' is withheld.  As in that spec, the
    ;; batch guard is lifted and the prompt itself is stubbed.
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((path (expand-file-name "README.md" repo))
             (stored (portable-bookmarks-off-workspace-spec--set path "readme"))
             (snapshot (copy-tree stored))
             (jf/emacs-dir repo)
             (offered nil)
             (quit-p nil))
        (expect (workspace--registered-names) :to-be nil)
        (spy-on 'workspace-restore :and-call-through)
        (cl-letf (((symbol-function 'workspace-bookmark--can-prompt-p) (lambda () t)))
          (condition-case nil
              (portable-bookmarks-off-workspace-spec--jump
               "readme" "unused"
               (lambda (prompt choices &rest _)
                 (push (cons prompt (mapcar #'cadr choices)) offered)
                 (assq ?c choices)))
            (quit (setq quit-p t))))
        (expect quit-p :to-be t)
        (expect portable-bookmarks-off-workspace-spec--prompted :to-be nil)
        (expect (length offered) :to-equal 1)
        (expect (cdr (car offered)) :to-equal '("view read-only" "cancel"))
        (expect (car (car offered)) :to-match "workspace (none)")
        (expect 'workspace-restore :not :to-have-been-called)
        (expect (workspace--current-name) :to-be nil)
        (expect (portable-bookmarks-off-workspace-spec--visits-p path) :to-be nil)
        (expect workspace-bookmark--awaiting-jump-target :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "opens the canonical copy read-only, marked, when that is chosen at the fallback"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((path (expand-file-name "README.md" repo))
             (stored (portable-bookmarks-off-workspace-spec--set path "readme"))
             (snapshot (copy-tree stored))
             (jf/emacs-dir repo))
        (cl-letf (((symbol-function 'workspace-bookmark--can-prompt-p) (lambda () t)))
          (let ((buffer (portable-bookmarks-off-workspace-spec--jump
                         "readme" "unused"
                         (lambda (_prompt choices &rest _) (assq ?v choices)))))
            (expect (workspace--same-file-p (buffer-file-name buffer) path) :to-be-truthy)
            (with-current-buffer buffer
              (expect buffer-read-only :to-be-truthy)
              (expect view-mode :to-be-truthy)
              (expect workspace-bookmark--canonical-view :to-be-truthy)
              ;; `format-mode-line' renders nothing in batch; the marker
              ;; is a buffer-local value, asserted as the missing-repo
              ;; spec asserts it.
              (expect (local-variable-p 'mode-line-buffer-identification) :to-be-truthy)
              (expect (format "%S" mode-line-buffer-identification)
                      :to-match "canonical"))))
        (expect (workspace--current-name) :to-be nil)
        (expect workspace-bookmark--awaiting-jump-target :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "withholds the read-only view when the relpath is absent under the canonical checkout"
    (portable-bookmarks-off-workspace-spec--hermetic
      ;; Bookmark a file that exists only on a worktree branch, then jump
      ;; with only the canonical checkout in view: missing-repo, :path nil.
      (let* ((wt (workspace-bookmark-fixtures-add-worktree
                  repo (expand-file-name "wt" (workspace-bookmark-fixtures-temp-dir "wsbm-off-wt-"))
                  :branch "elsewhere"))
             (only-wt (workspace-bookmark-fixtures-commit-file wt "only-wt.el" "(wt)\n"))
             (jf/emacs-dir repo)
             (offered nil))
        (portable-bookmarks-off-workspace-spec--set only-wt "only-wt")
        (cl-letf (((symbol-function 'workspace-bookmark--can-prompt-p) (lambda () t)))
          (condition-case nil
              (portable-bookmarks-off-workspace-spec--jump
               "only-wt" "unused"
               (lambda (prompt choices &rest _)
                 (push (cons prompt (mapcar #'cadr choices)) offered)
                 (assq ?c choices)))
            (quit nil)))
        (expect (cdr (car offered)) :to-equal '("cancel"))
        (expect (car (car offered)) :to-match "only-wt.el is absent there")
        (expect (portable-bookmarks-off-workspace-spec--visits-p only-wt) :to-be nil)
        (expect (portable-bookmarks-off-workspace-spec--visits-p
                 (expand-file-name "only-wt.el" repo))
                :to-be nil))))

  (it "asks for no workspace and reports no-clone when nothing is in view; the stored path is never opened"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((path (expand-file-name "README.md" repo))
             (stored (portable-bookmarks-off-workspace-spec--set path "readme"))
             (snapshot (copy-tree stored)))
        (let ((message (portable-bookmarks-off-workspace-spec--decline "readme" "unused")))
          (expect message :to-match "no checkout of")
          (expect message :to-match "in view"))
        (expect portable-bookmarks-off-workspace-spec--prompted :to-be nil)
        (expect (portable-bookmarks-off-workspace-spec--visits-p path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)))))


;;; Behavioral — the layout reincarnation chain honours a decline

(describe "layout reincarnation chain: a leaf whose bookmark declines"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (portable-bookmarks-off-workspace-spec--reset-frame)
    (setq world (portable-bookmarks-off-workspace-spec--world)))

  (it "restores a layout whose leaf resolves not-on-branch WITHOUT visiting the stored path: the window shows why, one report, no prompt"
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-only (workspace-bookmark-fixtures-commit-file
                      (plist-get world :wt-a) "only-a.el" "(only-a)\n"))
             (b-only (expand-file-name "only-a.el" (plist-get world :wt-b)))
             (reports nil))
        ;; Capture a layout on the plain tab: only-a.el (A's copy) beside
        ;; *scratch*, then hand it to B, which holds a checkout WITHOUT
        ;; only-a.el.
        (portable-bookmarks-off-workspace-spec--give-saved-layout
         "wsbm-off-b" (portable-bookmarks-off-workspace-spec--layout-beside-scratch a-only))
        (expect (file-exists-p b-only) :to-be nil)
        (portable-bookmarks-off-workspace-spec--recording-messages reports
          (cl-letf (((symbol-function 'y-or-n-p)
                     #'portable-bookmarks-off-workspace-spec--prompted!)
                    ((symbol-function 'read-multiple-choice)
                     #'portable-bookmarks-off-workspace-spec--prompted!))
            (workspace-restore "wsbm-off-b")
            (portable-bookmarks-off-workspace-spec--drain-timers)))
        (expect (workspace--current-name) :to-equal "wsbm-off-b")
        (expect (length (window-list nil 'no-mini)) :to-equal 2)
        ;; Neither the stored path (A's copy) nor B's missing path was visited.
        (expect (portable-bookmarks-off-workspace-spec--visits-p a-only) :to-be nil)
        (expect (portable-bookmarks-off-workspace-spec--visits-p b-only) :to-be nil)
        (expect (file-exists-p b-only) :to-be nil)
        ;; The leaf's window shows the error buffer carrying the reason.
        (let ((window (portable-bookmarks-off-workspace-spec--window-showing
                       "*workspace-restore: only-a.el*")))
          (expect window :not :to-be nil)
          (with-current-buffer (window-buffer window)
            (expect (buffer-string) :to-match "declined")
            (expect (buffer-string) :to-match "not on this branch")
            (expect (buffer-string) :to-match "NOT opened")))
        ;; One aggregated report, naming the leaf and the reason.
        (let ((declines (seq-filter (lambda (report)
                                      (string-match-p "not restored" report))
                                    reports)))
          (expect (length declines) :to-equal 1)
          (expect (car declines) :to-match "1 buffer not restored")
          (expect (car declines) :to-match (regexp-quote "only-a.el"))
          (expect (car declines) :to-match "not on this branch")))))

  (it "restores a layout whose leaf resolves missing-repo without reaching the chooser: no prompt, the stored path not visited, one report"
    ;; The chain binds `workspace--restoring-layout-leaf' around each
    ;; leaf; the missing-repo chooser's guard
    ;; (`workspace-bookmark--can-prompt-p') reads it and declines instead
    ;; of asking.  The guard answers as in an interactive Emacs (the real
    ;; predicate, `noninteractive' masked) so that it is the leaf signal,
    ;; not batch mode, that keeps the prompt away; a prompt that is
    ;; reached fails loudly.
    (portable-bookmarks-off-workspace-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (home-c (workspace-bookmark-fixtures-make-workspace "wsbm-off-c"))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (reports nil)
             (asked nil))
        (ignore home-c)
        ;; Capture a layout holding A's README beside *scratch*; give it
        ;; to C, which holds no checkout at all.
        (portable-bookmarks-off-workspace-spec--give-saved-layout
         "wsbm-off-c" (portable-bookmarks-off-workspace-spec--layout-beside-scratch a-path))
        (portable-bookmarks-off-workspace-spec--recording-messages reports
          (cl-letf* ((guard (symbol-function 'workspace-bookmark--can-prompt-p))
                     ((symbol-function 'workspace-bookmark--can-prompt-p)
                      (lambda () (let ((noninteractive nil)) (funcall guard))))
                     ((symbol-function 'read-multiple-choice)
                     (lambda (&rest args) (setq asked args) (error "Prompted: %S" args)))
                    ((symbol-function 'y-or-n-p)
                     #'portable-bookmarks-off-workspace-spec--prompted!))
            ;; The canonical checkout is in view through `jf/emacs-dir'.
            (let ((jf/emacs-dir (plist-get world :repo)))
              (workspace-restore "wsbm-off-c")
              (portable-bookmarks-off-workspace-spec--drain-timers))))
        (expect asked :to-be nil)
        (expect (workspace--current-name) :to-equal "wsbm-off-c")
        (expect (length (window-list nil 'no-mini)) :to-equal 2)
        (expect (portable-bookmarks-off-workspace-spec--visits-p a-path) :to-be nil)
        (expect (portable-bookmarks-off-workspace-spec--visits-p canonical-path) :to-be nil)
        (let ((window (portable-bookmarks-off-workspace-spec--window-showing
                       "*workspace-restore: README.md*")))
          (expect window :not :to-be nil)
          (with-current-buffer (window-buffer window)
            (expect (buffer-string) :to-match "declined")
            (expect (buffer-string) :to-match "holds no checkout")))
        (let ((declines (seq-filter (lambda (report)
                                      (string-match-p "not restored" report))
                                    reports)))
          (expect (length declines) :to-equal 1)
          (expect (car declines) :to-match "1 buffer not restored")
          (expect (car declines) :to-match "holds no checkout")
          (expect (car declines) :not :to-match "choice failed"))))))


;;; Unit — the chooser's prompt guard, which the chain relies on

(describe "workspace-bookmark--can-prompt-p"
  (it "is nil while core restores a layout leaf, even in an interactive Emacs"
    (let ((noninteractive nil))
      (expect (workspace-bookmark--can-prompt-p) :to-be-truthy)
      (let ((workspace--restoring-layout-leaf t))
        (expect (workspace-bookmark--can-prompt-p) :to-be nil))))
  (it "is nil while inhibit-interaction is bound, even in an interactive Emacs"
    (let ((noninteractive nil))
      (expect (workspace-bookmark--can-prompt-p) :to-be-truthy)
      (let ((inhibit-interaction t))
        (expect (workspace-bookmark--can-prompt-p) :to-be nil))))
  (it "is nil in a non-interactive Emacs"
    (let ((noninteractive t))
      (expect (workspace-bookmark--can-prompt-p) :to-be nil))))

(provide 'portable-bookmarks-off-workspace-spec)
;;; portable-bookmarks-off-workspace-spec.el ends here
