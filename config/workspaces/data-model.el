;;; data-model.el --- Workspaces pure data layer -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct workspace-buffer
  "Per-window-leaf reincarnation record.
Stored under the leaf's `parameters' map by `workspace--capture-frameset',
consumed by `workspace--deserialize-buffer' on restore.

Slots:
- `bookmark'        :: result of `bookmark-make-record', or nil for
                       non-bookmarkable buffers.
- `filename'        :: file path for file-backed buffers, else nil.
- `name'            :: `buffer-name' at save time.
- `narrowed-p'      :: t if the buffer was narrowed at save time.
- `indirect-p'      :: t if the buffer was an indirect buffer.
- `local-variables' :: reserved (always nil in v2); forward-compat hook.
- `etc'             :: alist; forward-compat slot."
  (bookmark nil)
  (filename nil)
  (name nil)
  (narrowed-p nil)
  (indirect-p nil)
  (local-variables nil)
  (etc nil))

(defun workspace--layout-make (saved-state &optional timestamp)
  "Build a layout plist wrapping SAVED-STATE.
TIMESTAMP defaults to current time as an integer.  The freshly-built
layout has =:working-state= nil (no autosave drift yet) and =:etc= nil
(forward-compat alist)."
  (list :timestamp (or timestamp (time-convert nil 'integer))
        :saved-state saved-state
        :working-state nil
        :etc nil))

(defun workspace--layout-saved-state (layout)
  "Return the :saved-state window-state stored in LAYOUT.
This is the explicit-save slot; written only by =workspace-save= and
its variants.  Never written by autosave paths."
  (plist-get layout :saved-state))

(defun workspace--layout-working-state (layout)
  "Return the :working-state window-state stored in LAYOUT.
This is the autosave slot; may be nil.  Written by tab-switch advice,
the idle-save timer, and =workspace--kill-emacs-flush=.  Cleared to nil
by every explicit =workspace-save=."
  (plist-get layout :working-state))

(defun workspace--layout-effective-state (layout)
  "Return the window-state to apply on restore for LAYOUT.
Precedence: =:working-state= when non-nil, else =:saved-state=.
Returns nil when both slots are nil (no state ever captured).

This is the single dispatch point for the working-over-saved
precedence rule (register/invariant/restore-precedence-working-over-
saved); direct reads of either slot from the restore path are a
regression risk."
  (or (workspace--layout-working-state layout)
      (workspace--layout-saved-state layout)))

(defun workspace--layout-timestamp (layout)
  "Return the integer timestamp stored in LAYOUT."
  (plist-get layout :timestamp))

(defconst workspace--reserved-group-names '("home")
  "Layout-group names that cannot be deleted by user commands.")

(defun workspace--group-name-reserved-p (name)
  "Return non-nil if NAME is a reserved layout-group name."
  (and (stringp name)
       (member name workspace--reserved-group-names)
       t))

(defun workspace--group-make (name layout)
  "Build a layout-group plist named NAME containing LAYOUT as its sole entry."
  (list :name name
        :layouts (list layout)))

(defun workspace--group-name (group)
  "Return the name of GROUP."
  (plist-get group :name))

(defun workspace--group-layouts (group)
  "Return the list of layouts in GROUP."
  (plist-get group :layouts))

(defun workspace--group-recent-layout (group)
  "Return the most recent layout in GROUP.
MVP keeps a single layout per group; this returns the head of the
:layouts list, which is also the most recently saved one."
  (car (workspace--group-layouts group)))

(defun workspace--group-add-layout (group layout)
  "Return a new group like GROUP with LAYOUT prepended to its :layouts.
Non-destructive: GROUP is not modified.  In MVP this replaces rather
than appends; the deferred history feature can change this without
disturbing callers."
  (list :name (workspace--group-name group)
        :layouts (list layout)))

(defun workspace--make (name home)
  "Build an empty workspace plist named NAME anchored at HOME.

NAME is the workspace name (and the registry key under which it is
stored).  HOME is the workspace's filesystem anchor — an absolute
path string.  By convention, =basename(HOME)= equals NAME; the data
layer does not enforce this here, but callers in the command layer
do (the canonical construction site derives NAME from HOME).

Both arguments are required: floating workspaces (without a home
directory) are structurally unrepresentable."
  (list :name name
        :home home
        :recent-layout-group nil
        :buffer-files nil
        :layout-groups nil))

(defun workspace--name (ws)
  "Return the name of workspace WS."
  (plist-get ws :name))

(defun workspace--home (ws)
  "Return the absolute home directory path of workspace WS.

Returns nil if WS has no =:home= slot (defensive: handles
hand-constructed or stale plists from a corrupted persistence file)."
  (plist-get ws :home))

(defun workspace--set-home (ws home)
  "Return a new workspace like WS with its =:home= slot set to HOME.
Non-destructive: WS is not modified."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :home home)))

(defun workspace--layout-groups (ws)
  "Return the layout-groups of workspace WS."
  (plist-get ws :layout-groups))

(defun workspace--recent-group (ws)
  "Return the name of WS's most recently activated layout-group, or nil."
  (plist-get ws :recent-layout-group))

(defun workspace--set-recent-group (ws name)
  "Return a new workspace like WS with its recent-layout-group set to NAME.
Non-destructive: WS is not modified."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :recent-layout-group name)))

(defun workspace--find-group (ws name)
  "Return the layout-group named NAME in WS, or nil if none."
  (seq-find (lambda (g) (equal (workspace--group-name g) name))
            (workspace--layout-groups ws)))

(defun workspace--upsert-group (ws name layout)
  "Return a new workspace like WS with a group named NAME holding LAYOUT.
If a group with NAME already exists, it is replaced; otherwise the new
group is appended.  Non-destructive."
  (let* ((existing (workspace--layout-groups ws))
         (filtered (seq-remove (lambda (g) (equal (workspace--group-name g) name))
                               existing))
         (replaced? (not (equal (length filtered) (length existing))))
         (new-group (workspace--group-make name layout))
         (next (if replaced?
                   ;; Preserve original position: walk existing, swap matching slot.
                   (mapcar (lambda (g)
                             (if (equal (workspace--group-name g) name)
                                 new-group
                               g))
                           existing)
                 (append existing (list new-group))))
         (copy (copy-sequence ws)))
    (plist-put copy :layout-groups next)))

(defun workspace--remove-group (ws name)
  "Return a new workspace like WS with the layout-group named NAME removed.
Non-destructive.  Reserved names are not specially handled here; callers
in the command layer must consult `workspace--group-name-reserved-p'
before invoking this on a user-supplied name."
  (let* ((next (seq-remove (lambda (g) (equal (workspace--group-name g) name))
                           (workspace--layout-groups ws)))
         (copy (copy-sequence ws)))
    (plist-put copy :layout-groups next)))

(defun workspace--buffer-files (ws)
  "Return the buffer-file membership list of WS."
  (plist-get ws :buffer-files))

(defun workspace--add-buffer-file (ws path)
  "Return a new workspace like WS with PATH added to its buffer-files.
Dedupes: adding the same PATH twice yields one entry.  Non-destructive."
  (let* ((files (workspace--buffer-files ws))
         (next (if (member path files) files (append files (list path))))
         (copy (copy-sequence ws)))
    (plist-put copy :buffer-files next)))

(defun workspace--remove-buffer-file (ws path)
  "Return a new workspace like WS with PATH removed from its buffer-files.
Non-destructive."
  (let* ((next (remove path (workspace--buffer-files ws)))
         (copy (copy-sequence ws)))
    (plist-put copy :buffer-files next)))

(defun workspace--broken-p (ws)
  "Return non-nil if WS is in a broken state.

A workspace is broken when its =:home= directory was missing at
persistence-load time.  The flag is runtime-only (never serialized);
on next save it is implicitly dropped."
  (plist-get ws :broken))

(defun workspace--mark-broken (ws)
  "Return a new workspace like WS marked broken.
Non-destructive: WS is not modified.  Sets =:broken t= on the copy."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :broken t)))

(defun workspace--clear-broken (ws)
  "Return a new workspace like WS with the broken tag cleared.
Non-destructive: WS is not modified.  Sets =:broken nil= on the copy."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :broken nil)))

(defun workspace--restore-pending-p (ws)
  "Return non-nil if WS is awaiting lazy frameset restoration.

The tag is set by the persistence loader and cleared by
=workspace--apply-saved-layout= after first activation.  Runtime-
only; never serialized."
  (plist-get ws :restore-pending))

(defun workspace--mark-restore-pending (ws)
  "Return a new workspace like WS marked restore-pending.
Non-destructive: WS is not modified.  Sets =:restore-pending t= on
the copy."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :restore-pending t)))

(defun workspace--clear-restore-pending (ws)
  "Return a new workspace like WS with the restore-pending tag cleared.
Non-destructive: WS is not modified.  Sets =:restore-pending nil=
on the copy."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :restore-pending nil)))

(defun workspace--sessions-dir (home)
  "Return =<HOME>/sessions/= as an absolute path.
Pure function: does not touch the filesystem.  HOME is the
workspace's =:home= directory."
  (expand-file-name "sessions" home))

(provide 'workspace-data-model)
;;; data-model.el ends here
