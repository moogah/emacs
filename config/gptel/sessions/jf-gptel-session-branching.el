;;; branching.el --- GPTEL Session Branching -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Conversation branching for gptel sessions.
;; Create alternate paths, resume from specific points, and manage branches.

;;; Code:

(require 'cl-lib)
(require 'jf-gptel-session-constants)
(require 'jf-gptel-session-logging)
(require 'jf-gptel-session-filesystem)
(require 'jf-gptel-session-registry)
(require 'jf-gptel-session-metadata)

(defun jf/gptel-resume-from-context ()
  "Resume gptel session from context.md file at point in dired.
Opens the context file in a gptel buffer with proper session state."
  (interactive)
  (let* ((file-at-point (dired-get-filename nil t))
         (context-file (if (and file-at-point (file-directory-p file-at-point))
                          (expand-file-name "context.md" file-at-point)
                        (when (and file-at-point
                                  (string-match-p "context\\.md$" file-at-point))
                          file-at-point))))
    (unless (and context-file (file-exists-p context-file))
      (user-error "No context.md file found at point"))

    (let* ((session-dir (file-name-directory context-file))
           (session-id (jf/gptel--session-id-from-directory session-dir))
           (metadata (jf/gptel--read-metadata session-dir)))

      (unless metadata
        (user-error "No metadata found for session"))

      ;; Open context file
      (find-file context-file)

      ;; Enable gptel-mode
      (unless gptel-mode
        (gptel-mode 1))

      ;; Set session variables
      (setq-local jf/gptel--session-id session-id)
      (setq-local jf/gptel--session-dir session-dir)

      ;; Restore backend/model from metadata
      (when-let ((backend-name (plist-get metadata :backend)))
        (setq-local gptel-backend (alist-get backend-name gptel--known-backends
                                            nil nil #'equal)))
      (when-let ((model-name (plist-get metadata :model)))
        (setq-local gptel-model (if (stringp model-name)
                                   (intern model-name)
                                 model-name)))

      ;; Enable autosave
      (setq-local jf/gptel-autosave-enabled t)
      (jf/gptel--enable-autosave)

      (jf/gptel--log 'info "Resumed session: %s" session-id)
      (message "Resumed gptel session: %s" session-id))))

(defun jf/gptel-branch-from-point ()
  "Create a conversation branch from the context at point.
Prompts for branch name and creates a new context file."
  (interactive)
  (let* ((file-at-point (dired-get-filename nil t))
         (context-file (if (and file-at-point (file-directory-p file-at-point))
                          (expand-file-name "context.md" file-at-point)
                        (when (and file-at-point
                                  (string-match-p "context\\.md$" file-at-point))
                          file-at-point))))
    (unless (and context-file (file-exists-p context-file))
      (user-error "No context.md file found at point"))

    (let* ((session-dir (file-name-directory context-file))
           (branch-name (read-string "Branch name: "))
           (branch-file (expand-file-name
                        (format "context-%s.md" branch-name)
                        session-dir)))

      (when (file-exists-p branch-file)
        (user-error "Branch already exists: %s" branch-name))

      ;; Copy context file to branch file
      (copy-file context-file branch-file)

      ;; Open branch file
      (find-file branch-file)

      ;; Optionally prompt to truncate at a specific point
      (when (y-or-n-p "Truncate branch at specific line? ")
        (let ((line (read-number "Line number to truncate after: ")))
          (goto-char (point-min))
          (forward-line (1- line))
          (delete-region (point) (point-max))
          (save-buffer)))

      (jf/gptel--log 'info "Created branch: %s" branch-name)
      (message "Created branch: %s" branch-name))))

(defun jf/gptel--list-branches (session-dir)
  "List all context files (branches) in SESSION-DIR.
Returns list of (branch-name . file-path) cons cells."
  (let ((context-files (directory-files session-dir t "^context.*\\.md$")))
    (mapcar (lambda (file)
              (let ((name (if (string= (file-name-nondirectory file) "context.md")
                            "main"
                          (replace-regexp-in-string
                           "^context-\\(.*\\)\\.md$" "\\1"
                           (file-name-nondirectory file)))))
                (cons name file)))
            context-files)))

(defun jf/gptel-switch-branch ()
  "Switch to a different branch in the current session.
Prompts for branch name using completing-read."
  (interactive)
  (unless jf/gptel--session-dir
    (user-error "Not in a gptel session buffer"))

  (let* ((branches (jf/gptel--list-branches jf/gptel--session-dir))
         (branch-names (mapcar #'car branches))
         (choice (completing-read "Switch to branch: " branch-names nil t))
         (branch-file (cdr (assoc choice branches))))

    (when branch-file
      ;; Save current buffer if modified
      (when (buffer-modified-p)
        (save-buffer))

      ;; Open branch file
      (find-file branch-file)

      (jf/gptel--log 'info "Switched to branch: %s" choice)
      (message "Switched to branch: %s" choice))))

(defun jf/gptel-send-from-context ()
  "Resume context at point and send to API.
Useful for editing a saved context and continuing the conversation."
  (interactive)
  ;; First resume the context
  (jf/gptel-resume-from-context)

  ;; Prompt to send
  (when (y-or-n-p "Send this context to the API? ")
    (call-interactively #'gptel-send)))

(provide 'jf-gptel-session-branching)
;;; branching.el ends here
