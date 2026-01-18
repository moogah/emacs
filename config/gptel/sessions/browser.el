;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun jf/gptel-browse-sessions ()
  "Browse and open saved gptel sessions from directory.
Shows leaf nodes (endpoints) of each branch."
  (interactive)
  (let* ((sessions-dir (expand-file-name jf/gptel-sessions-directory))
         (session-dirs (when (file-directory-p sessions-dir)
                         (seq-filter #'file-directory-p
                                    (directory-files sessions-dir t "^[^.]"))))
         (candidates (jf/gptel--build-session-candidates session-dirs)))

    (message "DEBUG browse: candidates=%S" candidates)
    (message "DEBUG browse: candidates type=%s" (type-of candidates))

    (if (null candidates)
        (message "No gptel sessions found in %s" sessions-dir)
      (let* ((choice-list (mapcar #'car candidates)))
        (message "DEBUG browse: choice-list=%S" choice-list)
        (let* ((choice (completing-read "Open session branch: " choice-list nil t))
               (session-info (cdr (assoc choice candidates))))
          (message "DEBUG browse: selected=%s, info=%S" choice session-info)
          (jf/gptel--open-session-branch session-info))))))

(defun jf/gptel--build-session-candidates (session-dirs)
  "Build list of session branch candidates from SESSION-DIRS.
Returns alist of (display-string . (session-dir path leaf-node))."
  (let (candidates)
    (dolist (dir session-dirs)
      (let ((metadata (jf/gptel--read-metadata dir)))
        (when metadata
          (let* ((tree (plist-get metadata :tree))
                 (leaves (jf/gptel--find-leaf-nodes tree))
                 (session-id (plist-get metadata :session_id)))
            ;; Add candidate for each leaf
            (dolist (leaf leaves)
              (let* ((path (car leaf))
                     (node (cdr leaf))
                     (depth (1- (length path))) ; subtract root
                     (last-id (aref path (1- (length path))))
                     (is-alt (string-match "alt\\([0-9]+\\)" (format "%s" last-id)))
                     (branch-label (if is-alt
                                      (format " [alt%s, depth:%d]"
                                             (match-string 1 (format "%s" last-id))
                                             depth)
                                    (format " [main, depth:%d]" depth)))
                     (preview (plist-get node :preview))
                     (display (format "%-35s %-20s | %s"
                                     session-id
                                     branch-label
                                     (if preview
                                         (substring preview 0 (min 60 (length preview)))
                                       "..."))))
                (push (cons display (list :dir dir :path path :node node))
                      candidates)))))))
    (nreverse candidates)))

(defun jf/gptel--open-session-branch (session-info)
  "Open a session branch and reconstruct conversation.
SESSION-INFO is a plist with :dir, :path, and :node."
  (let* ((dir (plist-get session-info :dir))
         (path (plist-get session-info :path))
         (metadata (jf/gptel--read-metadata dir))
         (tree (plist-get metadata :tree)))
    ;; Reconstruct conversation from root to this leaf
    (jf/gptel--reconstruct-conversation dir tree path)))

(defun jf/gptel--reconstruct-conversation (session-dir tree path)
  "Reconstruct conversation from root to PATH endpoint in TREE."
  (let* ((metadata (jf/gptel--read-metadata session-dir))
         (session-id (plist-get metadata :session_id))
         (buffer-name (format "*gptel-%s*" session-id))
         (context (jf/gptel--load-context-from-path session-dir tree path)))

    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        (markdown-mode)
        (gptel-mode 1)
        (erase-buffer)

        ;; Insert conversation with final prompt
        (jf/gptel--insert-context context t)

        ;; Set up session state
        (setq jf/gptel--session-dir session-dir)
        (setq jf/gptel--session-metadata metadata)
        (plist-put jf/gptel--session-metadata :current_path path)
        (jf/gptel--write-metadata session-dir jf/gptel--session-metadata)

        ;; Set counter from path depth
        (setq jf/gptel--message-counter (/ (length context) 2))

        ;; Restore backend/model
        (jf/gptel--restore-backend-model metadata)

        (goto-char (point-max)))

      (switch-to-buffer buf)
      (message "Loaded session: %s" session-id))))

(defun jf/gptel--load-context-from-path (session-dir tree path)
  "Load message/response pairs from SESSION-DIR following PATH in TREE.
Returns list of plists suitable for jf/gptel--insert-context."
  (let ((context '())
        (node tree))
    ;; Walk path (skip root at index 0)
    (cl-loop for i from 1 below (length path)
             for target-id = (aref path i)
             do
             (let ((children (plist-get node :children)))
               ;; Find matching child
               (setq node
                     (seq-find (lambda (child)
                                 (equal (plist-get child :id) target-id))
                               children))
               (when node
                 (let* ((file (plist-get node :file))
                        (type (intern (plist-get node :type)))
                        (full-path (expand-file-name file session-dir))
                        (content (when (file-exists-p full-path)
                                  (with-temp-buffer
                                    (insert-file-contents full-path)
                                    (buffer-string)))))
                   (when content
                     (push (list :type type :content content) context))))))
    (nreverse context)))

(defun jf/gptel--insert-context (context &optional include-final-prompt)
  "Insert CONTEXT (list of message/response plists) with proper formatting.
Adds text properties, prefixes, and separators.
If INCLUDE-FINAL-PROMPT is non-nil, adds a final prompt prefix for user input."
  (let ((prompt-prefix (gptel-prompt-prefix-string))
        (response-prefix (gptel-response-prefix-string))
        (separator gptel-response-separator))
    (dolist (item context)
      (let* ((type (plist-get item :type))
             (content (plist-get item :content))
             (is-response (eq type 'response))
             (prefix (if is-response response-prefix prompt-prefix))
             (start-pos (point)))

        ;; Insert separator before responses
        (when is-response
          (insert separator))

        ;; Insert prefix and content
        (insert prefix content "\n")

        ;; Add text properties for responses
        (when is-response
          (add-text-properties start-pos (point)
                              '(gptel response rear-nonsticky t)))))

    ;; Add final prompt prefix if requested
    (when include-final-prompt
      (insert "\n\n" prompt-prefix))))

(defun jf/gptel--restore-backend-model (metadata)
  "Restore backend and model from METADATA."
  (let ((backend-name (plist-get metadata :backend))
        (model-name (plist-get metadata :model)))

    ;; Restore backend
    (when backend-name
      (let ((backend (alist-get backend-name gptel--known-backends
                               nil nil #'equal)))
        (when backend
          (setq-local gptel-backend backend))))

    ;; Restore model (convert string to symbol)
    (when model-name
      (setq-local gptel-model (intern model-name)))))

(defun jf/gptel--get-session-context ()
  "Extract gptel session context from current buffer.
Returns plist with :session-id, :model, :backend, :agent-name, :timestamp.
Returns nil if not in a gptel session."
  (when (and gptel-mode jf/gptel--session-dir)
    (list :session-id (when jf/gptel--session-metadata
                        (plist-get jf/gptel--session-metadata :session_id))
          :model (when gptel-model (symbol-name gptel-model))
          :backend (when gptel-backend (gptel-backend-name gptel-backend))
          :agent-name jf/gptel--agent-name
          :timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))))
