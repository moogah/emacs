;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; Configuration for gptel session auto-save
(defcustom jf/gptel-autosave-enabled t
  "Whether to automatically save gptel sessions after each response."
  :type 'boolean
  :group 'gptel)

(defcustom jf/gptel-sessions-directory "~/gptel-sessions/"
  "Directory for storing gptel sessions.
Will be created if it doesn't exist."
  :type 'directory
  :group 'gptel)

(defcustom jf/gptel-session-filename-format "%Y%m%d-%H%M%S"
  "Format string for timestamp portion of gptel session filenames.
Uses `format-time-string' syntax."
  :type 'string
  :group 'gptel)

(defvar jf/gptel--session-registry (make-hash-table :test 'equal)
  "Global registry mapping session-id to session data plist.
Each entry: session-id -> (:directory path
                           :metadata plist
                           :parent-buffer buffer
                           :trace-stack list
                           :trace-counter int
                           :created time)

This allows subagents running in separate buffers to access
session context via session-id lookup.")

(defun jf/gptel--generate-session-id ()
  "Generate unique session ID using timestamp and random component."
  (format "%s-%04x" (format-time-string "%Y%m%d%H%M%S") (random 65536)))

(defun jf/gptel--register-session (session-dir session-metadata parent-buffer)
  "Register new session in global registry. Returns session-id."
  (let ((session-id (jf/gptel--generate-session-id)))
    (puthash session-id
             (list :directory session-dir
                   :metadata session-metadata
                   :parent-buffer parent-buffer
                   :trace-stack nil
                   :trace-counter 0
                   :created (current-time))
             jf/gptel--session-registry)
    session-id))

(defun jf/gptel--unregister-session (session-id)
  "Remove session from registry (manual cleanup)."
  (remhash session-id jf/gptel--session-registry))

(defun jf/gptel--get-session-data (session-id)
  "Lookup session data from registry. Returns plist or nil."
  (gethash session-id jf/gptel--session-registry))

(defun jf/gptel--update-session-data (session-id key value)
  "Update specific key in session data."
  (when-let ((session-data (gethash session-id jf/gptel--session-registry)))
    (plist-put session-data key value)
    (puthash session-id session-data jf/gptel--session-registry)))

(defun jf/gptel-session-directory (session-id-or-dirname)
  "Get full path to session directory.
SESSION-ID-OR-DIRNAME can be either:
- A session ID (looks up directory from registry)
- A directory name (expands relative to sessions base directory)
- An absolute path (returned as-is)"
  (cond
   ;; If it's an absolute path, return it
   ((file-name-absolute-p session-id-or-dirname)
    session-id-or-dirname)
   ;; Try lookup in registry first (if it looks like a session ID)
   ((and (stringp session-id-or-dirname)
         (string-match-p "^[0-9]\\{14\\}-[0-9a-f]\\{4\\}$" session-id-or-dirname))
    (when-let ((session-data (jf/gptel--get-session-data session-id-or-dirname)))
      (plist-get session-data :directory)))
   ;; Otherwise treat as directory name
   (t
    (expand-file-name session-id-or-dirname
                      (expand-file-name jf/gptel-sessions-directory)))))

(defun jf/gptel-session-trace-directory (session-id trace-id)
  "Get full path to trace directory within session.
SESSION-ID identifies the session (looked up in registry).
TRACE-ID is the trace identifier (e.g., \"trace-1\")."
  (when-let* ((session-data (jf/gptel--get-session-data session-id))
              (session-dir (plist-get session-data :directory)))
    (expand-file-name trace-id
                      (expand-file-name "traces" session-dir))))

(defun jf/gptel-session-trace-tool-results-directory (session-id trace-id)
  "Get full path to tool results directory within trace.
SESSION-ID identifies the session (looked up in registry).
TRACE-ID is the trace identifier (e.g., \"trace-1\")."
  (when-let* ((trace-dir (jf/gptel-session-trace-directory session-id trace-id)))
    (expand-file-name "tool-results" trace-dir)))

(defun jf/gptel--overlay-get-session-id (overlay)
  "Get session ID from OVERLAY.
Returns the session ID string or nil if not set.
This property is used to track session context across buffers,
particularly for subagents that run in separate buffers."
  (overlay-get overlay 'jf/session-id))

(defun jf/gptel--overlay-set-session-id (overlay session-id)
  "Set SESSION-ID on OVERLAY.
SESSION-ID should be a string returned by `jf/gptel--generate-session-id'.
This property enables subagents running in separate buffers to access
the parent session's context via registry lookup."
  (overlay-put overlay 'jf/session-id session-id))

(defun jf/gptel--sanitize-model-name (model)
  "Sanitize MODEL symbol for use in filename.
Converts to lowercase, replaces special chars with hyphens."
  (let ((name (symbol-name model)))
    (replace-regexp-in-string
     "-+" "-"  ; collapse multiple hyphens
     (replace-regexp-in-string
      "[^a-z0-9-]" "-"  ; replace special chars
      (downcase name)))))

(defun jf/gptel--generate-session-dirname ()
  "Generate directory name for current gptel session.
Format: TIMESTAMP-MODELNAME"
  (let* ((timestamp (format-time-string jf/gptel-session-filename-format))
         (model-name (jf/gptel--sanitize-model-name gptel-model)))
    (format "%s-%s" timestamp model-name)))

(defun jf/gptel--get-file-extension ()
  "Get file extension based on current major-mode."
  (cond
   ((derived-mode-p 'org-mode) "org")
   ((derived-mode-p 'markdown-mode) "md")
   (t "txt")))

(defun jf/gptel--initialize-session ()
  "Initialize a new session directory, metadata, and register in global registry."
  (let* ((dirname (jf/gptel--generate-session-dirname))
         (sessions-base (expand-file-name jf/gptel-sessions-directory))
         (session-dir (expand-file-name dirname sessions-base))
         (backend-name (gptel-backend-name gptel-backend)))
    ;; Create session directory
    (make-directory session-dir t)

    ;; Create initial metadata (now includes :agent_traces [])
    (setq jf/gptel--session-metadata
          (jf/gptel--create-metadata session-dir dirname gptel-model backend-name))

    ;; Write metadata
    (jf/gptel--write-metadata session-dir jf/gptel--session-metadata)

    ;; Register in global registry
    (let ((session-id (jf/gptel--register-session session-dir jf/gptel--session-metadata
                                                  (current-buffer))))
      ;; Store session-id as buffer-local variable
      (setq jf/gptel--session-id session-id)
      ;; Keep old vars for backwards compatibility
      (setq jf/gptel--session-dir session-dir)

      (message "Created session: %s (ID: %s)" dirname session-id))))

;; Buffer-local variables to track session state
;; NOTE: Only session-id is buffer-local now. All other state lives in global registry.
(defvar-local jf/gptel--session-id nil
  "Session ID for this buffer. Used to lookup data from global registry.
All other session state (directory, metadata, traces) stored in registry.")

(defvar-local jf/gptel--session-dir nil
  "Directory where current session is stored.
DEPRECATED: Kept for backwards compatibility. Use registry lookup instead.")

(defvar-local jf/gptel--session-metadata nil
  "Metadata plist for current session.")

(defvar-local jf/gptel--message-counter 0
  "Counter for message/response pairs in current session.")

(defvar-local jf/gptel--branching-next nil
  "Flag indicating next message should be a branch.")

(defvar-local jf/gptel--branch-id nil
  "Branch ID to use for next message when branching.")

(defvar-local jf/gptel--agent-name nil
  "Name of the agent currently active in this buffer.
Set when delegating to a subagent via gptel-agent.")
