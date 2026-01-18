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
