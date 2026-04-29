;;; scope-expansion.el --- GPTEL Scope Expansion UI -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Interactive UI for handling scope violations with 3-choice menu.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'transient)
(require 'jf-gptel-scope-validation)

(defvar-local jf/gptel-scope--expansion-queue nil
  "Queue of pending expansion prompts waiting to be shown.
Each entry is a plist with :violation, :callback, :patterns, :tool-name,
:chat-buffer.  Buffer-local so concurrent gptel sessions don't interfere.")

(defvar-local jf/gptel-scope--expansion-active nil
  "Non-nil when an expansion transient menu is currently displayed.
Used to decide whether to show transient immediately or queue.")

(defun jf/gptel-scope--process-expansion-queue ()
  "Process the next queued expansion prompt, or clear the active flag.
Called after each transient suffix action (deny, allow-once, add-to-scope).
If queue has items, pops the next and shows its transient.
If queue is empty, clears the active flag."
  (if jf/gptel-scope--expansion-queue
      ;; Pop next item and show transient
      (let* ((next (pop jf/gptel-scope--expansion-queue))
             (violation (plist-get next :violation))
             (callback (plist-get next :callback))
             (patterns (plist-get next :patterns))
             (tool-name (plist-get next :tool-name))
             (chat-buffer (plist-get next :chat-buffer)))
        (transient-setup 'jf/gptel-scope-expansion-menu nil nil
                         :scope (list :violation violation
                                      :callback callback
                                      :patterns patterns
                                      :tool-name tool-name
                                      :chat-buffer chat-buffer)))
    ;; Queue empty — clear active flag
    (setq jf/gptel-scope--expansion-active nil)))

(defun jf/gptel-scope--parent-wildcard-for (resource)
  "Return parent-directory wildcard pattern for a RESOURCE file path.
Examples: ~/foo/bar.txt → ~/foo/** | /a/b/src/init.el → /a/b/src/**"
  (concat (string-remove-suffix "/" (file-name-directory resource)) "/**"))

(defun jf/gptel-scope--current-chat-buffer ()
  "Return the chat buffer associated with the active expansion, or nil.
Reads `:chat-buffer' from the transient scope plist captured at
expansion-trigger time. Falls back to `(current-buffer)' when no transient
scope is available (defensive — production callers always go through the
transient).  Returns nil when the captured buffer has been killed."
  (let* ((scope (and (fboundp 'transient-scope) (transient-scope)))
         (buffer (or (and scope (plist-get scope :chat-buffer))
                     (current-buffer))))
    (and (bufferp buffer) (buffer-live-p buffer) buffer)))

(defun jf/gptel-scope--write-pattern-to-scope (pattern validation-type tool denied-operation)
  "Route PATTERN to the drawer writer based on VALIDATION-TYPE.
TOOL is the tool name (passed for diagnostic context only).
DENIED-OPERATION is the operation keyword from the validation error
(e.g. :read, :write) collapsed by the writer to the matching drawer key
via `jf/gptel-scope--map-operation-to-drawer-key'.

Returns the result of the underlying drawer writer (the pattern string
when the buffer was modified, nil on dedup short-circuit, or nil on the
bare-command branch of bash routing — see `--add-bash-to-scope')."
  (pcase validation-type
    ('path
     (jf/gptel-scope--add-path-to-scope pattern tool denied-operation))
    ('bash
     (jf/gptel-scope--add-bash-to-scope pattern tool denied-operation))
    (_
     (user-error "Unknown validation type: %s" validation-type))))

(defun jf/gptel-scope--map-operation-to-drawer-key (operation)
  "Map a denied OPERATION keyword to the matching drawer key string.

The input domain is the ten `:operation' values declared by
`register/vocabulary/operation-to-drawer-key' (read-like collapse to
GPTEL_SCOPE_READ; `:read-metadata' → GPTEL_SCOPE_READ_METADATA;
write-like collapse to GPTEL_SCOPE_WRITE; `:modify' → MODIFY;
`:execute' → EXECUTE).  Returns the bare key form (e.g.
\"GPTEL_SCOPE_READ\") suitable for the `org-entry-*' property API; the
colonised drawer literal \":GPTEL_SCOPE_READ:\" only appears in drawer
text.

Signals an error on any operation outside the closed input set, including
nil and `:match-pattern' — silent fall-through would grant read access on
every typo or upstream nil-operation case, and routing a glob pattern
into GPTEL_SCOPE_READ would grant reads on every filesystem match.
Callers receiving violations with `:operation nil' (cloud-auth,
parse-incomplete) or `:match-pattern' must intercept at the action layer
rather than relying on this function to choose a default — see
`harden-add-to-scope-action-handler' (cycle-3)."
  (cond
   ;; Read-like ops — :read-directory still collapses to READ since the
   ;; whole directory is the resource and granting it as a read scope is
   ;; the user's intent. :read-metadata gets its own bucket per ask 10A.
   ((memq operation '(:read :read-directory))
    "GPTEL_SCOPE_READ")
   ((eq operation :read-metadata)
    "GPTEL_SCOPE_READ_METADATA")
   ;; Write-like ops — :delete is intentionally kept under WRITE rather
   ;; than getting its own bucket (cycle-2 ask 10C disposition).
   ((memq operation '(:write :create :create-or-modify :append :delete))
    "GPTEL_SCOPE_WRITE")
   ((eq operation :modify)  "GPTEL_SCOPE_MODIFY")
   ((eq operation :execute) "GPTEL_SCOPE_EXECUTE")
   ;; Action-handler-only operations.  :match-pattern (cycle-2 ask 10B)
   ;; must be redirected to its sibling :read-directory before reaching
   ;; the writer; if it lands here, the upstream redirect is broken.
   ((eq operation :match-pattern)
    (error "scope-expansion: :match-pattern reached the writer — action handler should have redirected to :read-directory"))
   ((null operation)
    (error "scope-expansion: cannot map nil :operation to a drawer key — handle at the action layer"))
   (t
    (error "scope-expansion: unmapped :operation %S — extend the mapping or use the deny action"
           operation))))

(defun jf/gptel-scope--write-pattern-to-drawer (buffer operation pattern)
  "Append PATTERN to BUFFER's drawer key for OPERATION; save the buffer.
Idempotent: returns nil and does not modify the buffer when PATTERN is
already present under the target key. Returns the pattern string when the
buffer was modified.

OPERATION is collapsed to a drawer key via
`jf/gptel-scope--map-operation-to-drawer-key'.  The drawer at point-min
must already exist; this is true for any session created by
`jf/gptel-scope-profile--apply-to-drawer'."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let* ((key (jf/gptel-scope--map-operation-to-drawer-key operation))
               (existing (org-entry-get-multivalued-property (point) key)))
          (if (member pattern existing)
              nil
            (let ((updated (append existing (list pattern))))
              (apply #'org-entry-put-multivalued-property
                     (point) key updated)
              (save-buffer)
              pattern)))))))

(transient-define-prefix jf/gptel-scope-expansion-menu ()
  "Handle scope violation with 3-choice UI."
  [:description
   (lambda ()
     (let* ((scope (transient-scope))
            (violation (plist-get scope :violation))
            (tool (plist-get violation :tool))
            (resource (plist-get violation :resource))
            (reason (plist-get violation :reason))
            (metadata (plist-get violation :metadata))
            (git-tracked (when metadata (plist-get metadata :git-tracked)))
            (exists (when metadata (plist-get metadata :exists))))
       (concat
        (format "Scope Violation: %s\n  Tool: %s\n  Resource: %s"
                (propertize "Access Denied" 'face 'error)
                (propertize tool 'face 'font-lock-function-name-face)
                (propertize resource 'face 'font-lock-string-face))
        ;; Display git status if metadata available
        (when metadata
          (format "\n  Git Status: %s"
                  (propertize (if git-tracked "TRACKED" "NOT TRACKED")
                              'face (if git-tracked 'success 'warning))))
        ;; Display file status if metadata available
        (when metadata
          (format "\n  File Status: %s"
                  (propertize (if exists "exists" "does not exist")
                              'face (if exists 'success 'warning))))
        ;; Display reason if present (defensive: handle nil gracefully)
        (when reason
          (format "\n  Reason: %s"
                  (propertize reason 'face 'warning))))))
   [("d" "Deny (reject tool call)" jf/gptel-scope--deny-expansion
     :transient nil)
    ("a" "Add to scope (permanent)" jf/gptel-scope--add-to-scope
     :transient nil)
    ("w" (lambda ()
           (let* ((resource (plist-get (plist-get (transient-scope) :violation) :resource))
                  (pattern (jf/gptel-scope--parent-wildcard-for resource)))
             (format "Add %s to scope" pattern)))
     jf/gptel-scope--add-wildcard-to-scope
     :if (lambda ()
           (not (file-directory-p
                 (plist-get (plist-get (transient-scope) :violation) :resource))))
     :transient nil)
    ("c" "Add custom pattern to scope" jf/gptel-scope--add-custom-to-scope
     :transient nil)
    ("o" "Allow once (temporary)" jf/gptel-scope--allow-once-action
     :transient nil)]
   [""
    ("e" "Edit scope manually" jf/gptel-scope--edit-scope)
    ("q" "Cancel" transient-quit-one)]])

(defun jf/gptel-scope--deny-expansion ()
  "Reject the tool call completely."
  (interactive)
  (condition-case err
      (let* ((scope (transient-scope))
             (callback (plist-get scope :callback)))
        (if callback
            (condition-case callback-err
                (funcall callback
                         (json-serialize
                          (list :success nil
                                :user_denied t
                                :message "User denied scope expansion request.")))
              (error
               (message "Error invoking callback: %s" (error-message-string callback-err))))
          (message "Warning: No callback provided for scope expansion"))
        (transient-quit-one)
        (jf/gptel-scope--process-expansion-queue))
    (error
     (message "Error in deny-expansion: %s" (error-message-string err))
     (transient-quit-one)
     (jf/gptel-scope--process-expansion-queue))))

(defun jf/gptel-scope--add-to-scope ()
  "Add violated resource to the chat buffer's scope drawer permanently."
  (interactive)
  (let* ((scope (transient-scope))
         (violation (plist-get scope :violation))
         (callback (plist-get scope :callback))
         (validation-type (plist-get violation :validation-type))
         (resource (plist-get violation :resource))
         (tool (plist-get violation :tool))
         (denied-operation (plist-get violation :operation)))

    ;; Route to the drawer writer. denied-operation is collapsed to a
    ;; :GPTEL_SCOPE_<KEY>: drawer key by --map-operation-to-drawer-key.
    (jf/gptel-scope--write-pattern-to-scope resource validation-type tool denied-operation)

    ;; Notify callback with JSON response
    (condition-case err
        (if callback
            (let* ((patterns (plist-get scope :patterns))
                   (tool-name (plist-get scope :tool-name)))
              (funcall callback
                       (json-serialize
                        (list :success t
                              :patterns_added (vconcat patterns)  ; Convert list to vector for JSON array
                              :message (format "Scope expanded. Added %d pattern(s) to %s"
                                             (length patterns) tool-name)))))
          (message "Warning: No callback provided for scope expansion"))
      (error
       (message "Error invoking callback: %s" (error-message-string err))))

    (message "Added %s to scope" resource)
    (transient-quit-one)
    (jf/gptel-scope--process-expansion-queue)))

(defun jf/gptel-scope--allow-once-action ()
  "Authorize the pending tool invocation without modifying the scope drawer.
Signals success to the wrapper's callback. The wrapper treats this the
same as a passed validation and runs the tool body once. No state is
persisted — a subsequent invocation of the same resource will prompt
again."
  (interactive)
  (let* ((scope (transient-scope))
         (violation (plist-get scope :violation))
         (callback (plist-get scope :callback))
         (resource (plist-get violation :resource)))
    (condition-case err
        (if callback
            (funcall callback
                     (json-serialize
                      (list :success t
                            :allowed_once t
                            :message "Permission granted for this invocation only.")))
          (message "Warning: No callback provided for scope expansion"))
      (error
       (message "Error invoking callback: %s" (error-message-string err))))

    (message "Allowed %s once" resource)
    (transient-quit-one)
    (jf/gptel-scope--process-expansion-queue)))

(defun jf/gptel-scope--add-wildcard-to-scope ()
  "Add parent-directory wildcard pattern to the scope drawer permanently."
  (interactive)
  (let* ((scope (transient-scope))
         (violation (plist-get scope :violation))
         (callback (plist-get scope :callback))
         (validation-type (plist-get violation :validation-type))
         (resource (plist-get violation :resource))
         (tool (plist-get violation :tool))
         (denied-operation (plist-get violation :operation))
         (pattern (jf/gptel-scope--parent-wildcard-for resource)))

    (jf/gptel-scope--write-pattern-to-scope pattern validation-type tool denied-operation)

    (condition-case err
        (if callback
            (funcall callback
                     (json-serialize
                      (list :success t
                            :patterns_added (vector pattern)
                            :message (format "Scope expanded. Added wildcard %s" pattern))))
          (message "Warning: No callback provided for scope expansion"))
      (error
       (message "Error invoking callback: %s" (error-message-string err))))

    (message "Added %s to scope" pattern)
    (transient-quit-one)
    (jf/gptel-scope--process-expansion-queue)))

(defun jf/gptel-scope--add-custom-to-scope ()
  "Prompt for a custom pattern and add it to the scope drawer permanently."
  (interactive)
  (let* ((scope (transient-scope))
         (violation (plist-get scope :violation))
         (callback (plist-get scope :callback))
         (validation-type (plist-get violation :validation-type))
         (resource (plist-get violation :resource))
         (tool (plist-get violation :tool))
         (denied-operation (plist-get violation :operation))
         (custom-pattern (condition-case nil
                             (read-string (format "Add pattern [%s]: " resource) resource)
                           (quit nil))))

    (if (null custom-pattern)
        ;; User pressed C-g — deny without error
        (condition-case err
            (if callback
                (funcall callback
                         (json-serialize
                          (list :success nil
                                :user_denied t)))
              (message "Warning: No callback provided for scope expansion"))
          (error
           (message "Error invoking callback: %s" (error-message-string err))))

      ;; User provided a pattern — write it
      (jf/gptel-scope--write-pattern-to-scope custom-pattern validation-type tool denied-operation)

      (condition-case err
          (if callback
              (funcall callback
                       (json-serialize
                        (list :success t
                              :patterns_added (vector custom-pattern)
                              :message (format "Scope expanded. Added custom pattern %s" custom-pattern))))
            (message "Warning: No callback provided for scope expansion"))
        (error
         (message "Error invoking callback: %s" (error-message-string err))))

      (message "Added %s to scope" custom-pattern))

    (transient-quit-one)
    (jf/gptel-scope--process-expansion-queue)))

(defun jf/gptel-scope--edit-scope ()
  "Bring the chat buffer's session.org into focus and unfold the
:PROPERTIES: drawer at point-min, then quit the transient."
  (interactive)
  (let ((buffer (jf/gptel-scope--current-chat-buffer)))
    (unless buffer
      (user-error "No chat buffer associated with this expansion"))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (when (looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$")
      (org-cycle))
    (transient-quit-one)))

(defun jf/gptel-scope--add-path-to-scope (path tool &optional denied-operation)
  "Add PATH to the chat buffer's scope drawer.
TOOL is passed for diagnostic context only (the writer routes by
DENIED-OPERATION, not by tool category).
DENIED-OPERATION is the operation keyword from the validation error
(e.g., :read, :read-metadata, :write). The drawer writer collapses it
to the matching :GPTEL_SCOPE_<KEY>: drawer key via
`jf/gptel-scope--map-operation-to-drawer-key'.

When DENIED-OPERATION is nil, defaults to :read (the safest choice for
filesystem tools whose category the caller did not pass through).
Returns the result of the underlying writer (the pattern when the
buffer was modified, nil on dedup short-circuit)."
  (ignore tool)
  (let* ((operation (or denied-operation :read))
         ;; Trailing-slash → /** suffix preserves the v3 normalization
         ;; behavior (a directory grant means \"this dir and below\").
         (normalized-path (if (string-suffix-p "/" path)
                              (concat (directory-file-name path) "/**")
                            path))
         (buffer (jf/gptel-scope--current-chat-buffer)))
    (unless buffer
      (user-error "No chat buffer associated with this expansion"))
    (jf/gptel-scope--write-pattern-to-drawer buffer operation normalized-path)))

(defun jf/gptel-scope--add-bash-to-scope (resource tool &optional denied-operation)
  "Add bash command RESOURCE to the chat buffer's scope drawer.
TOOL is passed for diagnostic context only.
DENIED-OPERATION, when non-nil, is the denied operation keyword (e.g.,
:read-metadata) collapsed by the writer to the matching drawer key.

Path-shaped RESOURCE values (absolute, tilde-prefixed, glob, or directory)
delegate to `jf/gptel-scope--add-path-to-scope'. Bare command names
emit a user message and return nil — they are not expandable in the
operation-first model."
  ;; Check if resource is a path-like pattern or a bare command name.
  ;; Path-like: absolute (/..., ~...), directory, or contains a glob wildcard
  ;; (match-pattern operations produce globs like "*.txt").
  ;; Command patterns are bare names like "brew" or "tree" — not expandable.
  (if (or (file-directory-p resource)
          (string-prefix-p "/" resource)
          (string-prefix-p "~" resource)
          (string-match-p "[*?]" resource))
      (jf/gptel-scope--add-path-to-scope resource tool denied-operation)

    ;; Bare command name — not expandable in the operation-first model
    ;; (commands are validated by their file operations, not by name).
    (message "Cannot add command '%s' to scope — use path-based expansion instead" resource)
    nil))

(defun jf/gptel-scope-prompt-expansion (violation-info callback patterns tool-name)
  "Show expansion UI for VIOLATION-INFO, or queue if one is already active.
CALLBACK is the gptel async callback to invoke with JSON result.
PATTERNS is the list of patterns to add if approved.
TOOL-NAME is the tool requesting expansion.
VIOLATION-INFO is a plist with :tool, :resource, :reason, :validation-type.

The current buffer is captured at expansion-trigger time and stored on
the transient scope plist as `:chat-buffer'.  Action handlers retrieve
it via `jf/gptel-scope--current-chat-buffer' and mutate the chat
buffer's `:PROPERTIES:' drawer in place — the writer no longer round-
trips through a YAML file.

When multiple async tools trigger expansion simultaneously (via gptel's mapc),
the first call shows the transient immediately and subsequent calls are queued.
After the user responds to each prompt, `jf/gptel-scope--process-expansion-queue'
shows the next queued prompt or clears the active flag.

This is a public API function used by scope-shell-tools and other modules."
  (let ((chat-buffer (current-buffer)))
    (if jf/gptel-scope--expansion-active
        ;; Queue this prompt — a transient is already showing
        (setq jf/gptel-scope--expansion-queue
              (append jf/gptel-scope--expansion-queue
                      (list (list :violation violation-info
                                  :callback callback
                                  :patterns patterns
                                  :tool-name tool-name
                                  :chat-buffer chat-buffer))))
      ;; No active expansion — show transient immediately
      (setq jf/gptel-scope--expansion-active t)
      (transient-setup 'jf/gptel-scope-expansion-menu nil nil
                       :scope (list :violation violation-info
                                    :callback callback
                                    :patterns patterns
                                    :tool-name tool-name
                                    :chat-buffer chat-buffer)))))

(provide 'jf-gptel-scope-expansion)
;;; scope-expansion.el ends here
