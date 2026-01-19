;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun jf/gptel-resume-from-context (&optional context-file)
  "Resume gptel session from CONTEXT-FILE.
If called interactively in dired, uses context.md at point.
If CONTEXT-FILE is nil, prompts for file selection.
Creates a gptel buffer with the conversation loaded and ready to continue."
  (interactive)
  (let* ((context-path (or context-file
                          (when (derived-mode-p 'dired-mode)
                            (let ((file-at-point (dired-get-filename nil t)))
                              (if (and file-at-point (file-directory-p file-at-point))
                                  (expand-file-name "context.md" file-at-point)
                                (when (and file-at-point
                                          (string= (file-name-nondirectory file-at-point)
                                                  "context.md"))
                                  file-at-point))))
                          (read-file-name "Context file: " nil nil t nil
                                         (lambda (f) (string= (file-name-nondirectory f)
                                                            "context.md")))))
         (session-dir (jf/gptel--find-session-root context-path))
         (node-path (jf/gptel--determine-node-path session-dir context-path))
         (metadata (jf/gptel--read-metadata session-dir)))

    (unless (file-exists-p context-path)
      (user-error "Context file not found: %s" context-path))

    (unless session-dir
      (user-error "Could not find session root for: %s" context-path))

    (unless metadata
      (user-error "Could not read session metadata from: %s" session-dir))

    ;; Create buffer and load context
    (let* ((session-id (file-name-nondirectory session-dir))
           (buffer-name (format "*gptel-%s*" session-id))
           (buf (get-buffer-create buffer-name)))

      (with-current-buffer buf
        ;; Set up mode
        (markdown-mode)
        (gptel-mode 1)
        (erase-buffer)

        ;; Insert context from file
        (insert-file-contents context-path)

        ;; Set up session state
        (setq jf/gptel--session-dir session-dir)
        (setq jf/gptel--session-id (file-name-nondirectory session-dir))
        (setq jf/gptel--session-metadata metadata)
        (setq jf/gptel--current-node-path node-path)

        ;; Restore backend/model from metadata
        (jf/gptel--restore-backend-model metadata)

        (goto-char (point-max))
        (message "Resumed session %s at %s"
                 session-id
                 (string-join node-path "/")))

      (switch-to-buffer buf))))

(defun jf/gptel-send-from-context ()
  "Send current buffer (context.md) to API to get response.
Buffer should contain markdown-formatted conversation history.
Creates response node in session directory tree."
  (interactive)
  (unless (and jf/gptel--session-dir jf/gptel--current-node-path)
    (user-error "No active session state. Use jf/gptel-resume-from-context first"))

  ;; Use gptel's normal send mechanism
  ;; The autosave hook will create the appropriate response node
  (gptel-send))

(defun jf/gptel-branch-from-point ()
  "Create a branch by copying the node at point.
If point is on a msg-N or response-N directory, copies it to create an alt branch.
The copied directory can then be edited before resuming."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Must be in dired/dirvish to create branches"))

  (let* ((node-dir (dired-get-filename nil t))
         (node-name (file-name-nondirectory node-dir)))

    (unless (and node-dir (file-directory-p node-dir))
      (user-error "Point must be on a directory"))

    (unless (string-match "^\\(msg\\|response\\)-\\([0-9]+\\)$" node-name)
      (user-error "Directory must be a msg-N or response-N node"))

    (let* ((parent-dir (file-name-directory (directory-file-name node-dir)))
           (branch-name (jf/gptel--next-branch-id parent-dir node-name))
           (branch-dir (expand-file-name branch-name parent-dir)))

      ;; Copy directory
      (copy-directory node-dir branch-dir)

      ;; Refresh dired
      (revert-buffer)

      ;; Navigate to new branch
      (goto-char (point-min))
      (search-forward branch-name nil t)
      (beginning-of-line)

      (message "Created branch: %s (edit context.md and resume)" branch-name))))

(defun jf/gptel--find-session-root (path)
  "Find session root directory by walking up from PATH.
Returns directory containing metadata.json or nil if not found."
  (let ((current-dir (if (file-directory-p path)
                        path
                      (file-name-directory path))))
    (while (and current-dir
                (not (file-exists-p (expand-file-name "metadata.json" current-dir)))
                (not (string= current-dir "/")))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    (when (file-exists-p (expand-file-name "metadata.json" current-dir))
      current-dir)))

(defun jf/gptel--determine-node-path (session-dir context-file)
  "Determine node path from SESSION-DIR to CONTEXT-FILE.
Returns list of node names representing the path.
Example: '(\"msg-1\" \"response-1\" \"msg-2\")"
  (let* ((context-dir (file-name-directory context-file))
         (rel-path (file-relative-name context-dir session-dir))
         (path-parts (split-string rel-path "/" t)))
    ;; Filter out current directory marker
    (seq-filter (lambda (part) (not (string= part "."))) path-parts)))

(defun jf/gptel--restore-backend-model (metadata)
  "Restore gptel backend and model from METADATA.
Sets buffer-local gptel-backend and gptel-model variables."
  (let* ((backend-name (plist-get metadata :backend))
         (model-name (plist-get metadata :model))
         (backend (cdr (assoc backend-name gptel--known-backends)))
         (model (when backend
                 (seq-find (lambda (m)
                            (string= (gptel--model-name m) model-name))
                          (gptel-backend-models backend)))))
    (when backend
      (setq-local gptel-backend backend))
    (when model
      (setq-local gptel-model model))
    (when (or backend model)
      (message "Restored: %s / %s" backend-name model-name))))

(provide 'jf/gptel-session-branching)
