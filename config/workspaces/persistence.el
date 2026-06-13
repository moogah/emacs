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
The persisted-slot whitelist is =(:name :home :recent-layout-group
:buffer-files :layout-groups)=; any runtime-only tag not in that set is
stripped at this boundary.  Currently that is =:broken= (set by the
deserializer on missing-home detection) — an observation about the
*current session* that must be re-derived on next load, never persisted
(register/invariant/broken-tag-runtime-only, design.md §D5)."
  (list :name (workspace--name ws)
        :home (workspace--home ws)
        :recent-layout-group (workspace--recent-group ws)
        :buffer-files (workspace--buffer-files ws)
        :layout-groups (workspace--layout-groups ws)))

(defun workspace--serialize-registry ()
  "Walk `workspace--registry' and produce its on-disk plist form.
Each workspace is reduced to its persistable shape via
`workspace--persistence-serialize-workspace' — runtime-only tags like
=:broken= are stripped at this boundary."
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
Does NOT create tabs — startup hydrates the registry only; tabs are
materialized later by the explicit `workspace-restore' command.  Assumes
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
                                ws)))
            (when broken-p
              (message
               "Workspaces: workspace %S has missing :home directory %s — tagged broken"
               name home))
            (puthash name with-broken workspace--registry)
            (push with-broken restored))))))
    (nreverse restored)))

(defvar workspace--persistence-blocked nil
  "Non-nil when the startup load found a present-but-unreadable file.
Suppresses all persistence writes for the rest of the session so an
autosave cannot clobber the backed-up corrupt original with an empty
registry.  Set by `workspace--read-state' on a parse error; never
auto-cleared (the in-memory registry after a failed load is
untrustworthy — fix/remove the file and restart).")

(defvar workspace--persistence-blocked-warned nil
  "Non-nil once the suppressed-write warning has been emitted.
Keeps the block warning to one message per session rather than one
per debounce tick.")

(defun workspace--state-readable-p (form)
  "Return non-nil if FORM round-trips through `prin1' then `read'.
The write-time readable assert: a single live Emacs object renders as
the unreadable =#<…>= syntax and would make the whole on-disk file
unparseable, so we refuse to write any FORM that cannot be read back.
Total over any input (never signals)."
  (let ((print-length nil)
        (print-level nil))
    (condition-case _err
        (progn (read (prin1-to-string form)) t)
      (error nil))))

(defun workspace--write-state (form)
  "Write FORM to the persistence file atomically, or refuse to write.
No-op (with a one-time warning) while `workspace--persistence-blocked'
is set, so an autosave cannot clobber a backed-up corrupt file.

Aborts without touching disk when FORM fails the write-time readable
assert (`workspace--state-readable-p'), leaving any prior good file
intact.

Otherwise writes FORM to a sibling temp file in the state directory and
`rename-file's it over the target, so a crash mid-write can never
truncate the live file.  Creates the parent directory if missing."
  (cond
   (workspace--persistence-blocked
    (unless workspace--persistence-blocked-warned
      (setq workspace--persistence-blocked-warned t)
      (display-warning
       'workspaces
       (format
        "Persistence is suppressed this session: the state file %s could not be read at startup and was backed up. Writes are disabled to protect the backup. Fix or remove the file and restart."
        (workspace--state-file))
       :warning)))
   ((not (workspace--state-readable-p form))
    (display-warning
     'workspaces
     (format
      "Refusing to write workspaces state: the serialized form is not readable back (contains a live/unreadable object). The previous file at %s is left intact. This is a bug in the serializer; please report it."
      (workspace--state-file))
     :error))
   (t
    (let* ((dir (workspace--state-directory))
           (target (workspace--state-file))
           (_ (make-directory dir t))
           (tmp (make-temp-file
                 (expand-file-name "workspaces.eld.tmp-" dir))))
      (with-temp-file tmp
        (let ((print-length nil)
              (print-level nil))
          (prin1 form (current-buffer))))
      (rename-file tmp target t)))))

(defun workspace--read-state ()
  "Read and return the persisted state form.
Three outcomes (design.md §D2):

- File absent → nil (fresh start; saving stays permitted).
- File present and readable: a v3 plist → that plist; any other
  =:version= → nil with a *Messages* notice (recognized \"ignore and
  start fresh\", not corruption — no backup, no block).
- File present but unreadable (a `read'/parse error): the corrupt bytes
  are renamed aside to a =workspaces.eld.corrupt-<ts>= sibling, a loud
  `display-warning' names both paths, `workspace--persistence-blocked'
  is set, and the sentinel symbol =workspace--unreadable= is returned
  (NOT nil) so `workspace--restore' can tell corruption from absence."
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
         (let ((backup (concat file ".corrupt-"
                               (format-time-string "%Y%m%d-%H%M%S"))))
           (ignore-errors (rename-file file backup t))
           (setq workspace--persistence-blocked t)
           (display-warning
            'workspaces
            (format
             "Could not read the workspaces state file %s (%s). It has been backed up to %s and persistence is suppressed for this session to protect it. Fix or remove the backup and restart."
             file (error-message-string err) backup)
            :error)
           'workspace--unreadable))))))

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
happens after `workspace-save-idle-delay' seconds of idle time.

No-op while `workspace--persistence-blocked' is set: the eventual
`workspace--flush-state' → `workspace--write-state' would refuse the
write anyway, so we do not even arm a useless idle timer."
  (interactive)
  (unless workspace--persistence-blocked
    (when workspace--save-timer
      (cancel-timer workspace--save-timer)
      (setq workspace--save-timer nil))
    (setq workspace--save-timer
          (run-with-idle-timer workspace-save-idle-delay nil
                               #'workspace--flush-state))))

(defun workspace--persistence-after-autosave (&rest _args)
  "Schedule a debounced disk save after an autosave snapshot."
  (workspace-save-state))

(advice-add 'workspace--autosave-current-layout :after
            #'workspace--persistence-after-autosave)

(defun workspace--apply-saved-layout (name)
  "Apply NAME's recent effective window-state to the current frame.
No-op when no saved layout exists.

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
                            (error-message-string err)))))))))

(defun workspace--kill-emacs-flush ()
  "Capture working-state then write the registry to disk synchronously.
Cancels any pending debounce.  The working-state capture is wrapped in
`ignore-errors' so a capture failure (e.g. unusual frame state during
shutdown) does not prevent the flush itself.

Consults `workspace-anti-save-predicates' before the working-state
capture; the registry flush itself is unconditional (the user has
explicitly initiated shutdown).

Short-circuits entirely when `workspace--persistence-blocked' is set:
the startup load found a corrupt file (now backed up) and the in-memory
registry is untrustworthy, so shutdown must NOT write over the backup.
The `workspace--write-state' choke point would refuse the write anyway;
the explicit early-return avoids the wasted working-state capture and
documents the boundary at the shutdown path itself."
  (unless workspace--persistence-blocked
    (when workspace--save-timer
      (cancel-timer workspace--save-timer)
      (setq workspace--save-timer nil))
    (unless (run-hook-with-args-until-success 'workspace-anti-save-predicates)
      (ignore-errors (workspace--autosave-current-layout :working-state)))
    (ignore-errors (workspace--write-state (workspace--serialize-registry)))))

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
  "Read the persistence file and hydrate the registry only (no tabs).
Restored workspaces are saved-but-not-materialized: reachable via
`workspace-restore' but with no live tab until the user restores one.
Safe to call at startup.

Distinguishes the three `workspace--read-state' outcomes (design.md §D2):

- nil (file absent) → no-op; the registry stays empty and persistence
  is NOT blocked, so a fresh-start save works normally.
- the `workspace--unreadable' sentinel (present-but-corrupt file) →
  no-op; the registry stays empty and `workspace--read-state' has
  already backed the corrupt file up and set
  `workspace--persistence-blocked'.  The sentinel is NOT deserialized.
- a real state plist → deserialize into the registry as usual."
  (let ((state (workspace--read-state)))
    (when (and state (not (eq state 'workspace--unreadable)))
      (workspace--deserialize-state state))))

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
tab and materialize the workspace.

When the workspace has a saved layout (its recent layout-group has a
non-nil effective state — `:working-state' if set, else `:saved-state';
register/invariant/restore-precedence-working-over-saved), apply that
layout via `workspace--apply-saved-layout'.

When the workspace has NO saved layout (a recovered/registry-only
workspace whose `:layout-groups' are empty or carry no effective
state), fall back to the home builder instead — the same
`workspace-home-builder' indirection `workspace-new' uses — so the new
tab opens <home>/home.org.  No `window-state-put' is attempted in this
case (design.md §D3).

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
    ;; Decide whether a saved layout exists using the SAME effective-state
    ;; derivation `workspace--apply-saved-layout' uses, so "has a layout"
    ;; cannot diverge from the restore-precedence invariant.
    (let* ((ws (gethash name workspace--registry))
           (recent (and ws (workspace--recent-group ws)))
           (group (and recent (workspace--find-group ws recent)))
           (layout (and group (workspace--group-recent-layout group)))
           (state (and layout (workspace--layout-effective-state layout))))
      (if state
          (workspace--apply-saved-layout name)
        ;; No saved layout: open <home>/home.org via the home builder,
        ;; mirroring `workspace-new'.  Do NOT window-state-put.
        (when (and (boundp 'workspace-home-builder)
                   (functionp workspace-home-builder))
          (funcall workspace-home-builder name)))))
  name)

(provide 'workspace-persistence)
;;; persistence.el ends here
