;;; layouts.el --- Workspace layout commands -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'bookmark)
(require 'map)
(require 'workspace-data-model)
(require 'workspace-tabs)

;; `workspace--flush-state' lives in persistence.el, which `require's this
;; module (not the reverse).  The deliberate layout commands below call it
;; synchronously at runtime, well after both modules are loaded, so the
;; forward reference is safe; declare it to silence the byte-compiler.
(declare-function workspace--flush-state "workspace-persistence")

(defcustom workspace-window-persistent-parameters
  '((header-line-format . writable)
    (mode-line-format . writable)
    (tab-line-format . writable)
    (no-other-window . writable)
    (no-delete-other-windows . writable)
    (window-preserved-size . writable)
    (window-side . writable)
    (window-slot . writable))
  "Window parameters preserved across workspace capture/restore.
Extends `window-persistent-parameters' for the duration of
`workspace--capture-frameset' and `workspace--restore-frameset'.

Each entry is `(PARAMETER . writable)' where `writable' tells
`window-state-get' to serialize the value via `prin1' so it survives
the round trip through the on-disk eld form."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'workspaces)

(defun workspace--unreadable-object-p (obj)
  "Return non-nil if OBJ cannot be reconstructed by `read'.
Covers the live-object classes that `prin1' renders as the
unreadable =#<…>= syntax: buffers, markers, overlays, frames,
windows, processes, and non-symbol functions (subrs and closures).
Symbols that happen to be `functionp' (named functions) are
readable and return nil.  Total over any input."
  (or (bufferp obj)
      (markerp obj)
      (overlayp obj)
      (framep obj)
      (windowp obj)
      (processp obj)
      (and (not (symbolp obj)) (functionp obj))))

(defvar workspace-window-parameter-translators
  `((window-preserved-size
     (serialize . ,(pcase-lambda (`(,buffer ,dir ,size))
                     (list (and (bufferp buffer) (buffer-name buffer))
                           dir size)))
     (deserialize . ,(pcase-lambda (`(,name ,dir ,size))
                       (list (and name (get-buffer name)) dir size)))))
  "Per-window-parameter serialize/deserialize translators.
An alist `(PARAM . ((serialize . FN) (deserialize . FN)))'.  FN
receives a single argument — the parameter's value as stored in a
window-state leaf's `parameters' map — and returns a replacement
value.  Modelled on `activities-window-parameters-translators' and
`burly-window-parameters-translators'.

`window-preserved-size' carries a `(BUFFER DIR SIZE)' triple; the
serializer replaces BUFFER with its name (readable), the deserializer
resolves the name back to a live buffer.  Add an entry here for any
future window parameter whose value embeds a live, unreadable object.")

(defun workspace--window-state-walk-leaves (state func)
  "Return a fresh window-state like STATE with FUNC applied to each leaf.
FUNC is called with a `(leaf . attrs)' form and must return a
replacement form (typically a fresh leaf with translated attrs).

Handles three cases per node:
- `(leaf . _attrs)' — apply FUNC.
- atom — return as-is.
- `(_key . atom)' — return as-is (split-direction keywords etc).
- proper list — recurse on its members.

The one-window-frame edge case (top-level form is itself a leaf,
not wrapped in a tree) is detected by `cl-position 'leaf' on the
form; if non-nil, the leaf-bearing sublist is translated and the
prefix before it preserved."
  (cl-labels
      ((walk (node)
         (pcase node
           (`(leaf . ,_attrs) (funcall func node))
           ((pred atom) node)
           (`(,_key . ,(pred atom)) node)
           ((pred proper-list-p)
            (if-let ((leaf-pos (cl-position 'leaf node)))
                ;; One-window-frame: prefix elements + translated leaf.
                (append (cl-subseq node 0 leaf-pos)
                        (funcall func (cl-subseq node leaf-pos)))
              (mapcar #'walk node)))
           (_ node))))
    (walk state)))

(defconst workspace--valid-reincarnation-steps
  '(bookmark filename name error-buffer)
  "Closed, ordered set of reincarnation-chain step symbols.
The order is load-bearing in production via the `or' chain in
`workspace--deserialize-buffer'.  Adding a fifth member is a
register-level change (see register/vocabulary/buffer-
reincarnation-fallback-chain in =interfaces.org=), not a
code-level addition.")

(defun workspace--reincarnation-step-p (step)
  "Return non-nil if STEP names a valid reincarnation-chain step."
  (memq step workspace--valid-reincarnation-steps))

(defun workspace--scrub-bookmark-record (record)
  "Return RECORD with any unreadable prop value nulled.
A bookmark record is `(NAME . PROPS)' where PROPS is an alist of
`(KEY . VALUE)'.  This nulls — keeps the KEY, sets VALUE to nil — any
VALUE satisfying `workspace--unreadable-object-p' (a marker, buffer,
overlay, frame, window, process, or non-symbol function a mode may have
embedded; the `help-mode' bug#56643 class).  The record's KEYS are
preserved so it remains a valid `bookmark-make-record'-shaped alist;
only offending values are dropped.  RECORD is mutated in place and
returned; nil passes through unchanged."
  (when record
    (dolist (prop (bookmark-get-bookmark-record record))
      (when (and (consp prop)
                 (workspace--unreadable-object-p (cdr prop)))
        (setcdr prop nil))))
  record)

(defun workspace--serialize-buffer (buffer)
  "Return a `workspace-buffer' struct capturing BUFFER's state, or nil if dead.
The struct carries enough to reincarnate the buffer across restart:
a `bookmark-make-record' result (the standard Emacs primitive for
major-mode-specific restorable state), the filename if file-backed,
the buffer name, and the narrowed/indirect flags.

The bookmark record is scrubbed of unreadable objects via
`workspace--scrub-bookmark-record' before being stored: a mode may
embed a marker or natively-compiled subr in its record (bug#56643),
which would make the whole on-disk state unreadable.  Nulling those
values keeps the record's keys intact, so the bookmark step of the
reincarnation chain still fires for the common case; a record whose
load-bearing value was nulled simply falls through to the file/name
fallback."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (make-workspace-buffer
       :bookmark (condition-case _err
                     (workspace--scrub-bookmark-record
                      (bookmark-make-record))
                   ;; Non-bookmarkable buffers (e.g. minibuffers, fresh
                   ;; *scratch* without a file) raise; fall through.
                   (error nil))
       :filename (buffer-file-name buffer)
       :name (buffer-name buffer)
       :narrowed-p (buffer-narrowed-p)
       :indirect-p (and (buffer-base-buffer buffer) t)))))

(defun workspace--error-buffer (ws-buffer)
  "Return a live, named buffer explaining that WS-BUFFER could not be reincarnated.
Modelled on `activities--error-buffer' (activities.el:855-862)."
  (let* ((orig-name (or (workspace-buffer-name ws-buffer) "<unknown>"))
         (name (format "*workspace-restore: %s*" orig-name))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         "Workspaces could not reincarnate this buffer.\n\n"
         (format "Original buffer name : %s\n" orig-name)
         (format "Original filename    : %s\n"
                 (or (workspace-buffer-filename ws-buffer) "<none>"))
         (format "Bookmark record      : %s\n\n"
                 (if (workspace-buffer-bookmark ws-buffer)
                     "present (failed to restore)"
                   "absent"))
         "All four fallback steps failed:\n"
         "  1. bookmark restore\n"
         "  2. find-file-noselect on filename\n"
         "  3. get-buffer by name\n"
         "  4. (this buffer is step 4)\n\n"
         "It is likely that the buffer's major mode does not support\n"
         "the bookmark system, or the underlying resource (file, repo,\n"
         "process) is no longer available.\n"))
      (visual-line-mode 1))
    buf))

(defun workspace--bookmark-buffer (ws-buffer)
  "Return the buffer produced by jumping to WS-BUFFER's bookmark record, or nil.
Uses a temp-buffer probe to detect whether `bookmark-jump' actually
moved off the temp buffer — `bookmark-handle-bookmark' swallows the
\"file no longer exists\" error and returns nil, so the only reliable
signal that the jump succeeded is observing a buffer change.

NOTE: Be aware of the following note from burly.el:
NOTE: Due to changes in help-mode.el which serialize natively
compiled subrs in the bookmark props, which cannot be read
back (which actually break the entire bookmark system when
such a props is saved in the bookmarks file), we have to
workaround a failure to read here.  See bug#56643."
  (let ((bookmark (workspace-buffer-bookmark ws-buffer)))
    (when bookmark
      (with-temp-buffer
        (let* ((temp-buffer (current-buffer))
               (jumped-to-buffer
                (save-window-excursion
                  (condition-case _err
                      (bookmark-jump bookmark)
                    ;; bug#56643 et al.: read-back failures, missing
                    ;; handlers, anything else.  Fall through.
                    (error nil))
                  (current-buffer))))
          (unless (eq temp-buffer jumped-to-buffer)
            jumped-to-buffer))))))

(defun workspace--deserialize-buffer (ws-buffer)
  "Reincarnate the buffer represented by WS-BUFFER.
Returns a live buffer.  Walks the four-step fallback chain:
bookmark → filename → name → error buffer.  Never returns nil; the
error buffer is the floor."
  (or
   ;; 1. Bookmark restore.
   (and (workspace-buffer-bookmark ws-buffer)
        (condition-case-unless-debug _err
            (workspace--bookmark-buffer ws-buffer)
          (error nil)))
   ;; 2. Filename fallback.
   (and (workspace-buffer-filename ws-buffer)
        (ignore-errors
          (find-file-noselect (workspace-buffer-filename ws-buffer))))
   ;; 3. Name fallback.
   (and (workspace-buffer-name ws-buffer)
        (get-buffer (workspace-buffer-name ws-buffer)))
   ;; 4. Error buffer.  Always live.
   (workspace--error-buffer ws-buffer)))

(defun workspace--window-state-serialize (state)
  "Return a fresh window-state like STATE with `workspace-buffer' embedded.
Walks STATE; on each leaf, looks up the saved buffer-or-buffer-name,
builds a `workspace-buffer' struct, and stores it in the leaf's
`parameters' map under key `workspace-buffer'."
  (workspace--window-state-walk-leaves
   state
   (lambda (leaf)
     (pcase-let* ((`(leaf . ,attrs) leaf)
                  ((map parameters
                        ('buffer
                         `(,buffer-or-buffer-name . ,_buffer-attrs)))
                   attrs))
       (setf (map-elt parameters 'workspace-buffer)
             (workspace--serialize-buffer
              (get-buffer buffer-or-buffer-name)))
       (pcase-dolist (`(,param . ,translators)
                      workspace-window-parameter-translators)
         (when (map-contains-key parameters param)
           (setf (map-elt parameters param)
                 (funcall (alist-get 'serialize translators)
                          (map-elt parameters param)))))
       (setf (map-elt attrs 'parameters) parameters)
       (cons 'leaf attrs)))))

(defun workspace--window-state-deserialize (state)
  "Return a fresh window-state like STATE with each leaf's buffer reincarnated.
Walks STATE; on each leaf, reads the `workspace-buffer' struct from
its `parameters' map (if present), reincarnates the buffer via the
four-step fallback chain, and rewrites the leaf's `buffer' slot to
point at the live buffer."
  (workspace--window-state-walk-leaves
   state
   (lambda (leaf)
     (pcase-let* ((`(leaf . ,attrs) leaf)
                  ((map parameters
                        ('buffer
                         `(,_buffer-name . ,buffer-attrs)))
                   attrs)
                  ((map workspace-buffer) parameters))
       (when workspace-buffer
         (let ((live (workspace--deserialize-buffer workspace-buffer)))
           (setf (map-elt attrs 'buffer) (cons live buffer-attrs))))
       (pcase-dolist (`(,param . ,translators)
                      workspace-window-parameter-translators)
         (when (map-contains-key parameters param)
           (condition-case-unless-debug _err
               (setf (map-elt parameters param)
                     (funcall (alist-get 'deserialize translators)
                              (map-elt parameters param)))
             (error
              (setq parameters (map-delete parameters param))))))
       (setf (map-elt attrs 'parameters) parameters)
       (cons 'leaf attrs)))))

(defvar workspace--restore-generation 0
  "Monotonic counter incremented on every restore entry.
The deferred restore closure captures the value at scheduling time
and compares it on fire; a mismatch indicates a newer restore was
queued and the closure should no-op.")

(defun workspace--capture-frameset ()
  "Return a window-state capturing the selected frame's root window.
Extends `window-persistent-parameters' with
`workspace-window-persistent-parameters' so side windows, preserved
sizes, and explicit modeline overrides survive the round trip.

Each leaf in the returned form carries a `workspace-buffer' struct
in its `parameters' map, encoding enough to reincarnate the buffer
across restart via the four-step fallback chain."
  (let* ((window-persistent-parameters
          (append workspace-window-persistent-parameters
                  window-persistent-parameters))
         (state (window-state-get (frame-root-window) 'writable)))
    (workspace--window-state-serialize state)))

(defun workspace--restore-frameset (state)
  "Apply window-state STATE to the selected frame's root window.
STATE is expected to carry a `workspace-buffer' struct in each
leaf's `parameters' map (see `workspace--capture-frameset').

The state is `copy-tree'-ed before mutation so the in-memory
registry is not affected by buffer-replacement side effects.  The
`window-state-put' call is deferred via `run-at-time' nil nil to
avoid racing against `bookmark--jump-via''s buffer-display call,
which fires synchronously from within bookmark handlers.

A generation counter (`workspace--restore-generation') gates the
deferred closure: if a newer restore is queued while this one is
pending, the older closure no-ops.  The counter is incremented
HERE — at the single restore choke point — so every entry path
(`workspace--apply-saved-layout', `workspace-switch-layout', and
any future caller of `workspace--restore-frameset') participates
in the race guard."
  ;; Note: only the INNER let binding of `window-persistent-parameters'
  ;; (inside the deferred lambda) is load-bearing — that is the binding
  ;; in scope when `window-state-put' runs.  An earlier draft also
  ;; bound it around `workspace--window-state-deserialize', but that
  ;; deserializer reads the per-leaf `workspace-buffer' struct from the
  ;; saved parameters map; it does not consult the dynamic variable.
  (let* ((bufferized (workspace--window-state-deserialize
                      (copy-tree state)))
         (gen (cl-incf workspace--restore-generation))
         (frame (selected-frame)))
    (run-at-time nil nil
                 (lambda ()
                   (when (= gen workspace--restore-generation)
                     (let ((window-persistent-parameters
                            (append workspace-window-persistent-parameters
                                    window-persistent-parameters)))
                       (window-state-put bufferized
                                         (frame-root-window frame)
                                         'safe)))))))

(defun workspace--update-recent-group (ws-name group-name)
  "Set WS-NAME's recent-layout-group to GROUP-NAME in the registry."
  (let ((ws (gethash ws-name workspace--registry)))
    (when ws
      (puthash ws-name
               (workspace--set-recent-group ws group-name)
               workspace--registry))))

(defconst workspace--valid-state-slots '(:saved-state :working-state)
  "Closed vocabulary of layout state slots.
See =register/vocabulary/workspace-state-slot= in =interfaces.org=.
The two members map to: =:saved-state= (explicit-save writes;
authoritative across restarts) and =:working-state= (autosave
writes; cleared by explicit save).  Adding a third member is a
register-level change, not a code-level addition.")

(defun workspace--state-slot-p (slot)
  "Return non-nil if SLOT is a valid workspace state-slot keyword."
  (memq slot workspace--valid-state-slots))

(defun workspace--autosave-current-layout (slot)
  "Snapshot the current frame into the current workspace's recent layout SLOT.
SLOT MUST be one of `:saved-state' (explicit save) or
`:working-state' (autosave path).  Routing is by direct caller
specification (no trigger → slot indirection at this site).

There is no default for SLOT: the load-bearing invariant
`autosave-never-writes-saved-state' (see =register/invariant/= in
=interfaces.org=) is enforced structurally — a caller that forgets
to pass a slot triggers `cl-assert' rather than silently defaulting
to the explicit-save slot.  This is the structural fix for v1 MVP
gap D8.

Captures only the window-state; does NOT update `:buffer-files'.
That sync is intentionally limited to explicit user save paths
(`workspace-save'); doing it here would wipe the file list whenever
a user switched away from a tab whose buffers had been killed since
the last explicit save.  No-op when not on a workspace tab or when
the workspace has no recent layout-group yet."
  (unless (workspace--state-slot-p slot)
    (error "workspace--autosave-current-layout: invalid SLOT %S \
(must be one of %S)" slot workspace--valid-state-slots))
  (let ((ws-name (workspace--current-name)))
    (when ws-name
      (let* ((ws (gethash ws-name workspace--registry))
             (group-name (and ws (workspace--recent-group ws))))
        (when (and ws group-name)
          (let* ((captured (workspace--capture-frameset))
                 (group (workspace--find-group ws group-name))
                 (existing (and group (workspace--group-recent-layout group)))
                 (layout (cond
                          ((and existing (eq slot :working-state))
                           ;; Preserve :saved-state; update timestamp +
                           ;; :working-state in place.
                           (let ((copy (copy-sequence existing)))
                             (plist-put copy :timestamp
                                        (time-convert nil 'integer))
                             (plist-put copy :working-state captured)))
                          ((and existing (eq slot :saved-state))
                           ;; Replace :saved-state; preserve :etc and
                           ;; any prior :working-state (the caller —
                           ;; typically `workspace-save' — decides
                           ;; whether to clear it as a separate step).
                           (let ((copy (copy-sequence existing)))
                             (plist-put copy :timestamp
                                        (time-convert nil 'integer))
                             (plist-put copy :saved-state captured)))
                          (t
                           ;; No existing layout — construct a fresh one.
                           ;; A :working-state-only first write would
                           ;; leave :saved-state nil, which is
                           ;; structurally valid (effective-state falls
                           ;; through correctly) and matches the v2
                           ;; semantics: explicit save defines the
                           ;; baseline, autosaves layer on top.
                           (if (eq slot :working-state)
                               (let ((l (workspace--layout-make nil)))
                                 (plist-put l :working-state captured))
                             (workspace--layout-make captured)))))
                 (updated (workspace--upsert-group ws group-name layout)))
            (puthash ws-name updated workspace--registry)))))))

(defvar workspace--save-layout-history nil
  "Minibuffer history for `workspace-save-layout'.")

(defun workspace--current-group-names ()
  "Return the layout-group names of the current workspace."
  (let* ((name (workspace--current-name))
         (ws (and name (gethash name workspace--registry))))
    (mapcar #'workspace--group-name (workspace--layout-groups ws))))

(defun workspace-save-layout (name)
  "Save the current window configuration as layout NAME in the current workspace.

Re-saving an existing layout (including the reserved `home' layout)
overwrites the layout's :saved-state while preserving :etc and any
prior :working-state is then cleared (explicit-save-clears-working-
state invariant).

Funnels through `workspace--autosave-current-layout :saved-state' so
all three explicit-save variants (`workspace-save', this command, and
`workspace--capture-home-layout' via `workspace-new') share the same
canonical layout-construction path.  Resolves
register/shape/layout-v2-plist producer fragmentation."
  (interactive
   (list
    (completing-read "Save layout as: "
                     (workspace--current-group-names)
                     nil nil nil 'workspace--save-layout-history)))
  (let ((ws-name (workspace--current-name)))
    (unless ws-name
      (user-error "Not on a workspaces-managed tab"))
    ;; Re-point recent to NAME *before* invoking the canonical helper
    ;; so the helper writes into the target group.  For an existing
    ;; group, the helper's :saved-state cond arm copies and preserves
    ;; :etc; for a new group, it falls through to the fresh-construction
    ;; arm.  Either way: one canonical construction path.
    (let* ((ws (gethash ws-name workspace--registry))
           (with-recent (workspace--set-recent-group ws name)))
      (puthash ws-name with-recent workspace--registry))
    (workspace--autosave-current-layout :saved-state)
    ;; Clear any prior :working-state drift on the re-saved layout to
    ;; honour register/invariant/explicit-save-clears-working-state.
    ;; For a brand-new group the working-state is already nil (helper
    ;; constructs via `workspace--layout-make'); for an existing group
    ;; the helper preserved any prior :working-state, which this clears.
    (let* ((ws (gethash ws-name workspace--registry))
           (group (workspace--find-group ws name))
           (layout (and group (workspace--group-recent-layout group))))
      (when layout
        (plist-put layout :working-state nil)))
    ;; Deliberate command — flush synchronously (no debounce).
    (workspace--flush-state)
    name))

(defun workspace-switch-layout (name)
  "Switch to layout NAME within the current workspace.
The outgoing layout's `:working-state' is updated with a fresh snapshot
of the current frame and flushed to disk synchronously (a layout switch
is a discrete user action; no debounce).  The destination layout's
effective state (`:working-state' if non-nil else `:saved-state') is
restored."
  (interactive
   (list
    (completing-read "Switch to layout: "
                     (workspace--current-group-names)
                     nil t)))
  (let ((ws-name (workspace--current-name)))
    (unless ws-name
      (user-error "Not on a workspaces-managed tab"))
    (let* ((ws (gethash ws-name workspace--registry))
           (group (workspace--find-group ws name)))
      (unless group
        (user-error "No layout named %s in workspace %s" name ws-name))
      ;; Snapshot the outgoing layout into its :working-state slot
      ;; before restoring.  Writing :working-state (not :saved-state)
      ;; preserves the outgoing layout's last explicit save unaltered
      ;; (register/invariant/autosave-never-writes-saved-state).
      (workspace--autosave-current-layout :working-state)
      ;; Deliberate command — flush the outgoing layout's snapshot
      ;; synchronously (no debounce) before restoring the destination.
      (workspace--flush-state)
      (let ((layout (workspace--group-recent-layout group)))
        (when layout
          (when-let ((state (workspace--layout-effective-state layout)))
            (workspace--restore-frameset state))))
      (workspace--update-recent-group ws-name name)
      name)))

(defun workspace-switch-to-recent-layout ()
  "Switch to the current workspace's most recently activated layout."
  (interactive)
  (let* ((ws-name (workspace--current-name))
         (ws (and ws-name (gethash ws-name workspace--registry)))
         (recent (and ws (workspace--recent-group ws))))
    (unless recent
      (user-error "No recent layout for this workspace"))
    (workspace-switch-layout recent)))

(defun workspace-delete-layout (name)
  "Delete layout NAME from the current workspace.

The reserved layout `home' cannot be deleted.  If the deleted layout
was the workspace's recent pointer, it is reassigned to the first
remaining group (or `home' if none)."
  (interactive
   (list
    (completing-read "Delete layout: "
                     (workspace--current-group-names)
                     nil t)))
  (when (workspace--group-name-reserved-p name)
    (user-error "Cannot delete reserved layout: %s" name))
  (let ((ws-name (workspace--current-name)))
    (unless ws-name
      (user-error "Not on a workspaces-managed tab"))
    (let* ((ws (gethash ws-name workspace--registry))
           (existed (workspace--find-group ws name)))
      (unless existed
        (user-error "No layout named %s" name))
      (let* ((updated (workspace--remove-group ws name))
             (remaining (workspace--layout-groups updated))
             (new-recent
              (cond
               ;; Pointer was something else; keep it.
               ((and (workspace--recent-group ws)
                     (not (equal (workspace--recent-group ws) name)))
                (workspace--recent-group ws))
               ;; Pointer was the deleted layout; pick a survivor.
               (remaining
                (workspace--group-name (car remaining)))
               (t "home"))))
        (puthash ws-name
                 (workspace--set-recent-group updated new-recent)
                 workspace--registry)
        name))))

(defun workspace--capture-home-layout ()
  "Stamp the current frame's window config as the current workspace's `home'.

Funnels through `workspace--autosave-current-layout :saved-state' (the
canonical layout-construction path), keeping shape parity with
`workspace-save' and `workspace-save-layout'.  Resolves
register/shape/layout-v2-plist producer fragmentation.

Called as an `:after' advice on `workspace-new', which has just put a
freshly-made workspace into the registry with `:recent-layout-group'
nil and no layout-groups.  Pointing recent to \"home\" before the
helper runs causes the helper's no-existing-layout branch to construct
a fresh layout via `workspace--layout-make' — same shape as the
explicit-save variants on first save."
  (let ((ws-name (workspace--current-name)))
    (when ws-name
      ;; Set the recent pointer to "home" before invoking the helper so
      ;; the helper writes into the home group (creating it via its
      ;; no-existing-layout branch).
      (let* ((ws (gethash ws-name workspace--registry))
             (with-recent (workspace--set-recent-group ws "home")))
        (puthash ws-name with-recent workspace--registry))
      (workspace--autosave-current-layout :saved-state)
      ;; `workspace-new' is a deliberate action — flush the home stamp
      ;; synchronously (no debounce).
      (workspace--flush-state))))

(advice-add 'workspace-new :after
            (lambda (&rest _)
              (workspace--capture-home-layout)))

(provide 'workspace-layouts)
;;; layouts.el ends here
