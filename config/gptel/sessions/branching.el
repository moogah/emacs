;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun jf/gptel-branch-session ()
  "Create new branch by selecting which messages to include.
Presents a list of message/response pairs and lets user choose
where to branch from."
  (interactive)
  (unless (and jf/gptel--session-dir jf/gptel--session-metadata)
    (user-error "No active session to branch"))

  ;; Get all messages and build candidates
  (let* ((all-messages (jf/gptel--get-all-messages))
         (candidates (jf/gptel--build-branch-candidates all-messages))
         (choice (completing-read "Branch before: " candidates nil t))
         (choice-index (cdr (assoc choice candidates)))
         ;; Take first N messages (up to but NOT including the chosen point)
         (branch-context (seq-take all-messages choice-index))
         ;; Get the next message text (the one we're branching before)
         (next-message-text (when (< choice-index (length all-messages))
                             (let ((next-msg (nth choice-index all-messages)))
                               (when (eq (plist-get next-msg :type) 'message)
                                 (plist-get next-msg :content)))))
         (context-length (length branch-context))
         ;; Get the parent path
         (current-path (plist-get jf/gptel--session-metadata :current_path))
         (desired-path-length (1+ context-length))
         (actual-path-length (length current-path))
         (parent-path (if (and (> context-length 0)
                              (>= actual-path-length desired-path-length))
                         (seq-subseq current-path 0 desired-path-length)
                       ["root"]))
         (next-msg-num (1+ (/ context-length 2)))
         (branch-id (jf/gptel--get-next-branch-id next-msg-num)))

    (message "DEBUG branch: choice=%s, context-length=%d, parent-path=%S"
             choice context-length parent-path)

    ;; Create new buffer with branch context
    (jf/gptel--create-branch-buffer branch-context branch-id parent-path next-message-text)))

(defun jf/gptel--build-branch-candidates (messages)
  "Build completing-read candidates from MESSAGES list.
Returns alist of (display-string . message-count).
Each option represents branching BEFORE that message."
  (let ((candidates '())
        (count 0))
    ;; Add option to branch before first message (fresh start)
    (push (cons "[Beginning] - Start fresh conversation" 0) candidates)

    ;; Add option for each message (branch before this message)
    (while (< count (length messages))
      (let* ((msg (nth count messages))
             (msg-content (plist-get msg :content))
             (msg-preview (substring msg-content 0
                                    (min 50 (length msg-content))))
             (msg-num (/ (+ count 2) 2)))
        ;; Only add option if this is a message (not response)
        (when (eq (plist-get msg :type) 'message)
          (push (cons (format "Before message %d: \"%s...\""
                             msg-num msg-preview)
                      count)
                candidates))
        (setq count (1+ count))))

    (nreverse candidates)))

(defun jf/gptel--get-next-branch-id (message-num)
  "Get next branch ID for MESSAGE-NUM by scanning directory.
Returns 'message-N' if no branches exist, or 'message-N-altM' for next alt."
  (let* ((base-pattern (format "message-%d" message-num))
         (alt-pattern (format "%s-alt\\([0-9]+\\)" base-pattern))
         (files (directory-files jf/gptel--session-dir nil base-pattern))
         (max-alt 0))
    ;; Scan for existing alts
    (dolist (file files)
      (when (string-match alt-pattern file)
        (let ((alt-num (string-to-number (match-string 1 file))))
          (setq max-alt (max max-alt alt-num)))))
    (if (> max-alt 0)
        (format "message-%d-alt%d" message-num (1+ max-alt))
      ;; Check if base message exists
      (if (seq-find (lambda (f) (string-match (format "^%s\\." base-pattern) f)) files)
          (format "message-%d-alt1" message-num)
        (format "message-%d" message-num)))))

(defun jf/gptel--create-branch-buffer (context branch-id parent-path &optional next-message-text)
  "Create new gptel buffer with CONTEXT loaded, ready for branching at BRANCH-ID.
If NEXT-MESSAGE-TEXT is provided, insert it as the starting prompt for editing."
  (let* ((metadata jf/gptel--session-metadata)
         (session-id (plist-get metadata :session_id))
         (buffer-name (format "*gptel-%s-branch-%s*" session-id branch-id))
         (buf (get-buffer-create buffer-name))
         (parent-session-dir jf/gptel--session-dir))

    (with-current-buffer buf
      ;; Set up mode
      (markdown-mode)
      (gptel-mode 1)

      ;; Reconstruct conversation with proper formatting
      (erase-buffer)
      (jf/gptel--insert-context context t) ; t = include final prompt prefix

      ;; If we have next message text, insert it as editable content
      (when next-message-text
        (insert next-message-text))

      ;; Set up session state (shared with parent)
      (setq jf/gptel--session-dir parent-session-dir)
      (setq jf/gptel--session-metadata (copy-sequence metadata))

      ;; Update current path to branch parent
      (plist-put jf/gptel--session-metadata :current_path parent-path)
      (message "DEBUG branch-buffer: set current-path=%S" parent-path)

      ;; Set message counter for next message
      (setq jf/gptel--message-counter (/ (length context) 2))

      ;; Mark that next message is a branch
      (setq-local jf/gptel--branching-next t)
      (setq-local jf/gptel--branch-id branch-id)

      ;; Restore backend/model
      (jf/gptel--restore-backend-model metadata)

      (goto-char (point-max))
      (message "Created branch at %s. Edit and send message to continue." branch-id))

    ;; Display buffer
    (switch-to-buffer buf)))
