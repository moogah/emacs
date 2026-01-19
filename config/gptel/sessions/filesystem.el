;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defvar-local jf/gptel--current-node-path nil
  "Current path in the conversation tree.
List of node names from root to current position.
Example: '(\"msg-1\" \"response-1\" \"msg-2\" \"response-2\")")

(defun jf/gptel--find-max-node-id (session-dir pattern extract-number-fn)
  "Recursively find maximum node ID in SESSION-DIR matching PATTERN.
EXTRACT-NUMBER-FN is a function that extracts the number from a node name.
Returns the maximum number found, or 0 if none found."
  (let ((max-num 0))
    (cl-labels ((scan-dir (dir)
                  (when (file-directory-p dir)
                    (dolist (entry (directory-files dir nil "^[^.]"))
                      (let ((full-path (expand-file-name entry dir)))
                        (when (file-directory-p full-path)
                          (when (string-match pattern entry)
                            (let ((num (funcall extract-number-fn entry)))
                              (when (> num max-num)
                                (setq max-num num))))
                          ;; Recurse into subdirectories
                          (scan-dir full-path)))))))
      (scan-dir session-dir))
    max-num))

(defun jf/gptel--next-message-id (session-dir)
  "Generate next message ID by scanning entire SESSION-DIR tree.
Finds highest msg-N across all levels and returns next sequential ID."
  (let ((max-num (jf/gptel--find-max-node-id
                  session-dir
                  "^msg-\\([0-9]+\\)\\(-alt[0-9]+\\)?$"
                  (lambda (name)
                    (if (string-match "^msg-\\([0-9]+\\)" name)
                        (string-to-number (match-string 1 name))
                      0)))))
    (format "msg-%d" (1+ max-num))))

(defun jf/gptel--next-response-id (session-dir)
  "Generate next response ID by scanning entire SESSION-DIR tree.
Finds highest response-N across all levels and returns next sequential ID."
  (let ((max-num (jf/gptel--find-max-node-id
                  session-dir
                  "^response-\\([0-9]+\\)\\(-alt[0-9]+\\)?$"
                  (lambda (name)
                    (if (string-match "^response-\\([0-9]+\\)" name)
                        (string-to-number (match-string 1 name))
                      0)))))
    (format "response-%d" (1+ max-num))))

(defun jf/gptel--next-branch-id (parent-dir base-name)
  "Generate next branch ID for BASE-NAME in PARENT-DIR.
BASE-NAME is like 'msg-2', returns 'msg-2-alt1', 'msg-2-alt2', etc."
  (let* ((pattern (format "^%s-alt\\([0-9]+\\)$" (regexp-quote base-name)))
         (existing (directory-files parent-dir nil pattern))
         (alt-numbers (mapcar (lambda (name)
                               (if (string-match pattern name)
                                   (string-to-number (match-string 1 name))
                                 0))
                             existing))
         (max-alt (if alt-numbers (apply 'max alt-numbers) 0)))
    (format "%s-alt%d" base-name (1+ max-alt))))

(defun jf/gptel--build-context-from-history (history)
  "Build markdown context from HISTORY.
HISTORY is a list of plists with :role and :content.
Returns markdown string suitable for context.md file."
  (with-temp-buffer
    (dolist (entry history)
      (let ((role (plist-get entry :role))
            (content (plist-get entry :content)))
        ;; Add markdown heading for role
        (insert (format "# %s\n\n" (capitalize (symbol-name role))))
        ;; Add content
        (insert content)
        ;; Ensure content ends with newline
        (unless (string-suffix-p "\n" content)
          (insert "\n"))
        ;; Add blank line between messages
        (insert "\n")))
    (buffer-string)))

(defun jf/gptel--extract-conversation-history (&optional end-pos)
  "Extract conversation history from current gptel buffer.
If END-POS is provided, only extract history up to that position.
Returns list of plists: (:role user|assistant :content string)."
  (let ((history nil)
        (scan-pt (point-min))
        (end-point (or end-pos (point-max))))
    (save-excursion
      (while (< scan-pt end-point)
        (goto-char scan-pt)
        (let ((next-change (next-single-property-change scan-pt 'gptel nil end-point)))
          (when next-change
            (let* ((prop (get-char-property scan-pt 'gptel))
                   (content (buffer-substring-no-properties scan-pt next-change))
                   ;; Trim gptel's prefix markers
                   (trimmed (gptel--trim-prefixes content))
                   (role (if (eq prop 'response) 'assistant 'user)))
              (when (and trimmed (not (string-empty-p trimmed)))
                (push (list :role role :content trimmed) history)))
            (setq scan-pt next-change))
          (unless next-change
            (setq scan-pt end-point)))))
    (nreverse history)))

(defun jf/gptel--create-message-node (session-dir node-path response-start)
  "Create message node in conversation tree.
SESSION-DIR is the session root directory.
NODE-PATH is list of parent node names (e.g., '(\"msg-1\" \"response-1\")).
RESPONSE-START marks where the response begins (extract message before this point).
Returns the new node ID."
  (let* ((parent-path (jf/gptel--resolve-node-path session-dir node-path))
         (node-id (jf/gptel--next-message-id session-dir))
         (node-dir (expand-file-name node-id parent-path))
         ;; Extract conversation history UP TO (but not including) the response
         (history (jf/gptel--extract-conversation-history response-start)))
    ;; Create node directory
    (make-directory node-dir t)
    ;; Write context.md with conversation up to this message
    (let ((context-file (expand-file-name "context.md" node-dir))
          (context-md (jf/gptel--build-context-from-history history)))
      (with-temp-file context-file
        (insert context-md)))
    node-id))

(defun jf/gptel--create-response-node (session-dir node-path)
  "Create response node in conversation tree.
SESSION-DIR is the session root directory.
NODE-PATH is list of parent node names (e.g., '(\"msg-1\")).
Returns the new node ID."
  (let* ((parent-path (jf/gptel--resolve-node-path session-dir node-path))
         (node-id (jf/gptel--next-response-id session-dir))
         (node-dir (expand-file-name node-id parent-path))
         ;; Extract complete conversation history including the response
         (history (jf/gptel--extract-conversation-history)))
    ;; Create node directory
    (make-directory node-dir t)
    ;; Write context.md with full conversation
    (let ((context-file (expand-file-name "context.md" node-dir))
          (context-md (jf/gptel--build-context-from-history history)))
      (with-temp-file context-file
        (insert context-md)))
    node-id))

(defun jf/gptel--resolve-node-path (session-dir node-path)
  "Resolve NODE-PATH to absolute directory path.
SESSION-DIR is the session root.
NODE-PATH is list of node names.
Returns absolute path to the deepest node."
  (if (null node-path)
      session-dir
    (cl-reduce (lambda (parent node)
                (expand-file-name node parent))
              node-path
              :initial-value session-dir)))

(defun jf/gptel--save-tools-file (node-dir tool-calls)
  "Save TOOL-CALLS to tools.md in NODE-DIR.
TOOL-CALLS is a list of tool call plists with :name, :input, :result, etc."
  (when tool-calls
    (let ((tools-file (expand-file-name "tools.md" node-dir)))
      (with-temp-file tools-file
        (insert "## Tool Calls\n\n")
        (dolist (tool-call tool-calls)
          (let ((name (plist-get tool-call :name))
                (id (plist-get tool-call :id))
                (input (plist-get tool-call :input))
                (result (plist-get tool-call :result)))
            ;; Tool header
            (insert (format "### %s\n" name))
            (when id
              (insert (format "**ID**: %s\n\n" id)))
            ;; Input
            (when input
              (insert "**Input**:\n")
              (insert "```json\n")
              (insert (json-encode input))
              (insert "\n```\n\n"))
            ;; Result
            (when result
              (insert "**Result**:\n")
              (insert "```\n")
              (insert (if (stringp result)
                         result
                       (format "%S" result)))
              (insert "\n```\n\n"))))))))

(defun jf/gptel--update-current-symlink (session-dir node-path)
  "Update current symlink to point to context.md at NODE-PATH.
SESSION-DIR is the session root directory.
NODE-PATH is list of node names."
  (let* ((symlink-path (expand-file-name "current" session-dir))
         (target-dir (jf/gptel--resolve-node-path session-dir node-path))
         (target-file (expand-file-name "context.md" target-dir))
         ;; Make relative path for more robust symlinks
         (relative-target (file-relative-name target-file session-dir)))
    ;; Remove old symlink if exists
    (when (file-symlink-p symlink-path)
      (delete-file symlink-path))
    ;; Create new symlink
    (make-symbolic-link relative-target symlink-path)))

(defun jf/gptel--create-session-directory (session-id metadata)
  "Create session directory with METADATA.
SESSION-ID is the session identifier.
METADATA is a plist with :model, :backend, :created.
Returns the session directory path."
  (let* ((sessions-base (expand-file-name jf/gptel-sessions-directory))
         (session-dir (expand-file-name session-id sessions-base)))
    ;; Create session directory
    (make-directory session-dir t)
    ;; Write metadata.json
    (let ((metadata-file (expand-file-name "metadata.json" session-dir))
          (json-data (list :session_id session-id
                          :model (plist-get metadata :model)
                          :backend (plist-get metadata :backend)
                          :created (plist-get metadata :created))))
      (with-temp-file metadata-file
        (insert (json-encode json-data))))
    session-dir))

(defun jf/gptel--read-context-file (context-path)
  "Read and return contents of context.md at CONTEXT-PATH."
  (when (file-exists-p context-path)
    (with-temp-buffer
      (insert-file-contents context-path)
      (buffer-string))))

(defun jf/gptel--list-child-nodes (parent-dir)
  "List child node directories in PARENT-DIR.
Returns list of node names (e.g., '(\"msg-1\" \"response-1\"))."
  (when (file-directory-p parent-dir)
    (let ((entries (directory-files parent-dir nil "^\\(msg\\|response\\)-[0-9]+")))
      (cl-remove-if-not (lambda (name)
                         (file-directory-p (expand-file-name name parent-dir)))
                       entries))))

(provide 'jf/gptel-session-filesystem)
