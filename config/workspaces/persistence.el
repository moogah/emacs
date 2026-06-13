;;; persistence.el --- Workspaces persistence -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'workspace-data-model)
(require 'workspace-tabs)
(require 'workspace-buffer-membership)
(require 'workspace-layouts)
(require 'tab-bar)

(defvar workspace-state-directory-override nil
  "When non-nil, an absolute directory used for persistence I/O in place
of the per-machine default.  The explicit hermetic knob: a sandbox or a
scripted batch session sets this to redirect every read/write away from
the developer's real state file.  See `workspace--state-directory'.")

(defun workspace--state-directory ()
  "Return the directory where workspace state is persisted.
Resolves in three tiers (see the module commentary):

1. `workspace-state-directory-override', when non-nil.
2. A per-process sandbox under `temporary-file-directory' whenever
   `noninteractive' is set and no override is given — so a batch/test
   run can never write the real per-machine state file (the
   `kill-emacs-hook' flush fires after any `cl-letf' state-dir rebinding
   has unwound).
3. The per-machine default, keyed by `jf/machine-role' (falling back to
   a `default' subdirectory when the role is unset)."
  (cond
   (workspace-state-directory-override
    (file-name-as-directory
     (expand-file-name workspace-state-directory-override)))
   (noninteractive
    (file-name-as-directory
     (expand-file-name (format "workspaces-batch-state-%d" (emacs-pid))
                       temporary-file-directory)))
   (t
    (let ((subdir (if (and (boundp 'jf/machine-role) jf/machine-role)
                      (concat "state/workspaces/" jf/machine-role "/")
                    "state/workspaces/default/")))
      (expand-file-name subdir (or (and (boundp 'jf/emacs-dir) jf/emacs-dir)
                                   user-emacs-directory))))))

(defun workspace--state-file ()
  "Return the absolute path of the persistence file."
  (expand-file-name "workspaces.eld" (workspace--state-directory)))

(defconst workspace--state-version 3
  "Schema version for the workspaces persistence file.
v3 (current): adds the required =:home= filesystem-anchor slot to every
workspace plist.  Inherits v2's per-layout =:saved-state= /
=:working-state= split, per-leaf =workspace-buffer= reincarnation struct,
=:etc= forward-compat alist.  The reader emits an *Messages* notice and
returns nil for any file whose =:version= is not 3 (no migration: v1 and
v2 files are rejected; delete the file to start fresh, design.md §D5).")

(defun workspace--persistence-serialize-workspace (ws)
  "Return WS reduced to its persistable shape.
Filters out runtime-only tags — currently =:broken= (set by the
deserializer on missing-home detection) and =:restore-pending= (set on
deserialize to gate lazy frameset activation).  Both are observations
about the *current session* and must be re-derived on next load, never
persisted (register/invariant/broken-tag-runtime-only, design.md §D5)."
  (list :name (workspace--name ws)
        :home (workspace--home ws)
        :recent-layout-group (workspace--recent-group ws)
        :buffer-files (workspace--buffer-files ws)
        :layout-groups (workspace--layout-groups ws)))

(defun workspace--serialize-registry ()
  "Walk `workspace--registry' and produce its on-disk plist form.
Each workspace is reduced to its persistable shape via
`workspace--persistence-serialize-workspace' — runtime-only tags like
=:broken= and =:restore-pending= are stripped at this boundary."
  (let (workspaces)
    (maphash
     (lambda (_name ws)
       (push (workspace--persistence-serialize-workspace ws) workspaces))
     workspace--registry)
    (list :version workspace--state-version
          :workspaces (nreverse workspaces))))

(defun workspace--deserialize-state (state)
  "Read STATE (the form read from disk) into `workspace--registry'.
Returns the list of workspaces that were inserted into the registry.
Does NOT create tabs; `workspace--restore-tabs' does that.  Assumes
STATE has already been version-checked by `workspace--read-state'.

Three filtering rules apply per entry (design.md §D5, §D6; cycle-2
architect finding =arch-cycle-20260525-213500-04= for the
absolute-path arm):

- Entries lacking the required =:home= slot are structural corruption
  (the writer should have produced one); emit an *Messages* notice
  naming the entry and skip it.  Sibling entries in the same file are
  still loaded — one bad entry should not tank the whole session.

- Entries whose =:home= is a relative path violate
  =register/invariant/home-required-no-floating-workspaces= (which
  requires =:home= to be an absolute filesystem anchor).  Such
  entries are skipped with a notice naming the offending value; this
  is the deserializer-side enforcement of the absolute-path
  requirement (the constructor =workspace--make= is the other entry
  point, but we cannot rely on it for hand-edited or corrupted
  persistence files).

- Entries whose =:home= directory does not exist on disk are tagged
  via `workspace--mark-broken' before insertion.  The broken tag is
  runtime-only (never serialized; the next load re-derives it from the
  current filesystem state)."
  (clrhash workspace--registry)
  (let ((ws-list (plist-get state :workspaces))
        (restored nil))
    (dolist (ws ws-list)
      (let ((name (workspace--name ws))
            (home (workspace--home ws)))
        (cond
         ((or (null home) (not (stringp home)))
          (message
           "Workspaces: skipping persisted entry %S — missing :home slot"
           name))
         ((not (file-name-absolute-p home))
          (message
           "Workspaces: skipping persisted entry %S — :home %S is not absolute"
           name home))
         (t
          (let* ((broken-p (not (file-directory-p home)))
                 (with-broken (if broken-p
                                  (workspace--mark-broken ws)
                                ws))
                 (tagged (workspace--mark-restore-pending with-broken)))
            (when broken-p
              (message
               "Workspaces: workspace %S has missing :home directory %s — tagged broken"
               name home))
            (puthash name tagged workspace--registry)
            (push tagged restored))))))
    (nreverse restored)))

(defun workspace--write-state (form)
  "Write FORM to the persistence file.
Creates the parent directory if missing."
  (make-directory (workspace--state-directory) t)
  (with-temp-file (workspace--state-file)
    (let ((print-length nil)
          (print-level nil))
      (prin1 form (current-buffer)))))

(defun workspace--read-state ()
  "Read and return the persisted state form, or nil if missing/unreadable.
Emits an *Messages* notice and returns nil for files whose =:version= is
not =workspace--state-version=.  v1 and v2 files on disk are expected to
be deleted by the user before running v3 code (design.md §D5 — no
migration during pre-alpha)."
  (let ((file (workspace--state-file)))
    (when (file-exists-p file)
      (condition-case err
          (let ((form (with-temp-buffer
                        (insert-file-contents file)
                        (read (current-buffer)))))
            (let ((v (plist-get form :version)))
              (if (eql v workspace--state-version)
                  form
                (message
                 "Workspaces: persistence file at %s is schema :version %S; v%d required — file ignored. Delete the file to start fresh."
                 file v workspace--state-version)
                nil)))
        (error
         (message "Workspaces: failed to read state file %s: %s"
                  file (error-message-string err))
         nil)))))

(defcustom workspace-save-idle-delay 2
  "Idle delay (seconds) before the auto-save trigger writes to disk.
Coalesces bursts of context switches into a single disk write."
  :type 'number
  :group 'workspaces)

(defvar workspace--save-timer nil
  "Pending idle timer for debounced disk save.")

(defun workspace--flush-state ()
  "Write the registry to disk now (cancelling any pending debounce)."
  (when workspace--save-timer
    (cancel-timer workspace--save-timer)
    (setq workspace--save-timer nil))
  (workspace--write-state (workspace--serialize-registry)))

(defun workspace-save-state ()
  "Schedule a debounced save of the workspaces registry to disk.
Cancels any pending timer and re-arms it; the actual disk write
happens after `workspace-save-idle-delay' seconds of idle time."
  (interactive)
  (when workspace--save-timer
    (cancel-timer workspace--save-timer)
    (setq workspace--save-timer nil))
  (setq workspace--save-timer
        (run-with-idle-timer workspace-save-idle-delay nil
                             #'workspace--flush-state)))

(defun workspace--persistence-after-autosave (&rest _args)
  "Schedule a debounced disk save after an autosave snapshot."
  (workspace-save-state))

(advice-add 'workspace--autosave-current-layout :after
            #'workspace--persistence-after-autosave)

(defun workspace--restore-tabs ()
  "Create a tab per restored workspace; do not change the active tab.

Each new tab is named after its workspace; that name is the
discriminator (see `workspace--tab-workspace-name' in tabs.el).
Layout restore is deferred until the user first selects the tab."
  (let ((preserved (workspace--current-name)))
    (maphash
     (lambda (name _ws)
       (unless (workspace--tab-for name)
         (tab-bar-new-tab)
         (tab-bar-rename-tab name)))
     workspace--registry)
    ;; If we just changed the current tab while creating new tabs,
    ;; navigate back to where we were.
    (when preserved
      (let ((idx (workspace--tab-index-for preserved)))
        (when idx (tab-bar-select-tab idx))))))

(defun workspace--apply-saved-layout (name)
  "Apply NAME's recent effective window-state to the current frame.
Also clears any `:restore-pending' flag on the workspace.  No-op when
no saved layout exists.

The effective state is `workspace--layout-effective-state', which
prefers =:working-state= when non-nil, else =:saved-state= (register/
invariant/restore-precedence-working-over-saved).  Direct reads of
either slot from this code path would silently regress the precedence
on a future refactor.

The leaf walker in `workspace--restore-frameset' reincarnates each
saved buffer via the four-step fallback chain (bookmark → filename
→ name → error buffer), so no separate pre-load of file buffers is
needed; the legacy `:buffer-files' slot is left untouched (design.md
§D7).

The restore generation counter (race guard against concurrent
restores; design.md §D2 Gotcha 3) is incremented inside
`workspace--restore-frameset' itself — every restore entry path
participates in the guard, including direct callers like
`workspace-switch-layout'."
  (let ((ws (gethash name workspace--registry)))
    (when ws
      (let* ((recent (workspace--recent-group ws))
             (group (and recent (workspace--find-group ws recent)))
             (layout (and group (workspace--group-recent-layout group)))
             (state (and layout (workspace--layout-effective-state layout))))
        (when state
          (condition-case err
              (workspace--restore-frameset state)
            (error (message "Workspaces: layout restore failed: %s"
                            (error-message-string err))))))
      (when (workspace--restore-pending-p ws)
        (puthash name (workspace--clear-restore-pending ws)
                 workspace--registry)))))

(defun workspace--activate-pending-workspace (name)
  "Restore NAME's recent layout if the workspace is `:restore-pending'.
Called from the tab-switch hook on first selection of a restored
workspace.  No-op when the flag is not set."
  (let ((ws (gethash name workspace--registry)))
    (when (and ws (workspace--restore-pending-p ws))
      (workspace--apply-saved-layout name))))

(defun workspace--persistence-after-tab-switch (&rest _args)
  "Hook installed after `tab-bar-switch-to-tab' for lazy activation."
  (let ((name (workspace--current-name)))
    (when name
      (workspace--activate-pending-workspace name))))

(advice-add 'tab-bar-switch-to-tab :after
            #'workspace--persistence-after-tab-switch)
(advice-add 'tab-bar-select-tab    :after
            #'workspace--persistence-after-tab-switch)

(defun workspace--kill-emacs-flush ()
  "Capture working-state then write the registry to disk synchronously.
Cancels any pending debounce.  The working-state capture is wrapped in
`ignore-errors' so a capture failure (e.g. unusual frame state during
shutdown) does not prevent the flush itself.

Consults `workspace-anti-save-predicates' before the working-state
capture; the registry flush itself is unconditional (the user has
explicitly initiated shutdown)."
  (when workspace--save-timer
    (cancel-timer workspace--save-timer)
    (setq workspace--save-timer nil))
  (unless (run-hook-with-args-until-success 'workspace-anti-save-predicates)
    (ignore-errors (workspace--autosave-current-layout :working-state)))
  (ignore-errors (workspace--write-state (workspace--serialize-registry))))

(add-hook 'kill-emacs-hook #'workspace--kill-emacs-flush)

(defun workspace--persistence-before-tab-switch (&rest _args)
  "Snapshot the outgoing workspace's :working-state before the tab switch.
No-op when the current tab is not a workspace-managed tab.  Consults
`workspace-anti-save-predicates'; if any predicate returns non-nil,
the autosave is silently skipped."
  (when (workspace--current-name)
    (unless (run-hook-with-args-until-success 'workspace-anti-save-predicates)
      (workspace--autosave-current-layout :working-state))))

(advice-add 'tab-bar-select-tab :before
            #'workspace--persistence-before-tab-switch)
(advice-add 'tab-bar-switch-to-tab :before
            #'workspace--persistence-before-tab-switch)

(defun workspace--restore ()
  "Read the persistence file and restore tabs (lazy frameset activation).
Safe to call at startup; no-ops cleanly when the file is absent."
  (let ((state (workspace--read-state)))
    (when state
      (workspace--deserialize-state state)
      (workspace--restore-tabs))))

(defun workspace-save ()
  "Save the current workspace's window configuration to disk.
Captures the current frame into the recent layout-group's `:saved-state'
slot, clears any `:working-state' drift on the same layout (the
explicit save is the new clean baseline; register/invariant/explicit-
save-clears-working-state), syncs the workspace's `:buffer-files' from
bufferlo, and writes the registry to disk synchronously.  Errors when
not on a workspaces-managed tab.

The bufferlo sync is intentionally limited to this explicit save path
(and not done by the tab-switch autosave) so that transient session
events — killing a buffer, closing a tab — never wipe the workspace's
saved file list."
  (interactive)
  (let ((ws-name (workspace--current-name)))
    (unless ws-name
      (user-error "Not on a workspaces-managed tab"))
    (workspace--autosave-current-layout :saved-state)
    ;; Clear any accumulated :working-state drift now that we have a
    ;; new explicit baseline.  Without this, the next restart would
    ;; prefer the stale :working-state over the just-written
    ;; :saved-state and the user's save would feel ignored.
    (let* ((ws (gethash ws-name workspace--registry))
           (group-name (workspace--recent-group ws))
           (group (and group-name (workspace--find-group ws group-name)))
           (layout (and group (workspace--group-recent-layout group))))
      (when layout
        (plist-put layout :working-state nil)))
    (workspace--sync-registry-from-bufferlo ws-name)
    (workspace--flush-state)
    ws-name))

(defun workspace-revert ()
  "Discard the current workspace's :working-state and re-apply :saved-state.
The user-facing affordance for \"throw away the autosave drift and
return to my last explicit save\".

Clears the current workspace's recent layout-group's `:working-state'
slot, flushes the registry to disk, then re-applies the layout via
`workspace--apply-saved-layout' (which now picks `:saved-state' because
`:working-state' is nil; register/invariant/restore-precedence-working-
over-saved).

Errors when not on a workspaces-managed tab."
  (interactive)
  (let ((name (workspace--current-name)))
    (unless name
      (user-error "Not on a workspaces-managed tab"))
    (let* ((ws (gethash name workspace--registry))
           (group-name (workspace--recent-group ws))
           (group (and group-name (workspace--find-group ws group-name)))
           (layout (and group (workspace--group-recent-layout group))))
      (when layout
        (plist-put layout :working-state nil)
        (workspace--flush-state)
        (workspace--apply-saved-layout name))
      name)))

(defvar workspace--restore-history nil
  "Minibuffer history for `workspace-restore'.")

(defun workspace-restore (name)
  "Materialize saved workspace NAME by creating or switching to its tab.

If a tab for NAME already exists, switch to it.  Otherwise create a
tab and apply the workspace's saved window configuration.

Interactively, completes over every workspace in the registry —
including saved workspaces that have no live tab.

Signals `user-error' if NAME is in a broken state (its `:home' no
longer exists on disk).  Use `workspace-re-anchor' to point it at a
new path or `workspace-purge' to remove the registry entry."
  (interactive
   (let ((names (workspace--registered-names)))
     (unless names
       (user-error "No saved workspaces"))
     (list (completing-read "Restore workspace: " names
                            nil t nil 'workspace--restore-history))))
  (unless (gethash name workspace--registry)
    (user-error "No saved workspace named %s" name))
  (let ((ws (gethash name workspace--registry)))
    (when (and ws (workspace--broken-p ws))
      (user-error
       "Workspace %s is broken: :home %s no longer exists. Use `workspace-re-anchor' or `workspace-purge'."
       name (workspace--home ws))))
  (if-let ((idx (workspace--tab-index-for name)))
      (tab-bar-select-tab idx)
    (tab-bar-new-tab)
    (tab-bar-rename-tab name)
    (workspace--apply-saved-layout name))
  name)

(provide 'workspace-persistence)
;;; persistence.el ends here
