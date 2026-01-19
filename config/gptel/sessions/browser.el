;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun jf/gptel-browse-sessions ()
  "Browse saved gptel sessions in dirvish.
Opens the sessions directory where each subdirectory is a session.
Use dirvish navigation to explore conversation trees."
  (interactive)
  (let ((sessions-dir (expand-file-name jf/gptel-sessions-directory)))
    (unless (file-directory-p sessions-dir)
      (make-directory sessions-dir t))
    (if (fboundp 'dirvish)
        (dirvish sessions-dir)
      (dired sessions-dir))
    (message "Browse sessions: Use RET to enter, TAB to expand tree")))

(defun jf/gptel-open-session ()
  "Select and open a specific gptel session.
Provides completing-read interface to choose from available sessions,
then opens the selected session in dirvish."
  (interactive)
  (let* ((sessions-dir (expand-file-name jf/gptel-sessions-directory))
         (session-dirs (when (file-directory-p sessions-dir)
                        (seq-filter
                         (lambda (f)
                           (and (file-directory-p (expand-file-name f sessions-dir))
                                (not (string-prefix-p "." f))))
                         (directory-files sessions-dir))))
         (candidates (mapcar
                     (lambda (dirname)
                       (let* ((full-path (expand-file-name dirname sessions-dir))
                              (metadata (jf/gptel--read-metadata full-path))
                              (model (if metadata
                                       (plist-get metadata :model)
                                     "unknown"))
                              (created (if metadata
                                         (plist-get metadata :created)
                                       "unknown")))
                         (cons (format "%-30s  [%s] %s" dirname model created)
                               full-path)))
                     session-dirs)))
    (if (null candidates)
        (message "No gptel sessions found in %s" sessions-dir)
      (let* ((choice (completing-read "Open session: " (mapcar #'car candidates) nil t))
             (session-path (cdr (assoc choice candidates))))
        (if (fboundp 'dirvish)
            (dirvish session-path)
          (dired session-path))
        (message "Opened session: %s" (file-name-nondirectory session-path))))))

(defun jf/gptel-show-current-position ()
  "Navigate to current conversation position in session directory.
Follows the 'current' symlink to show the active node in the tree."
  (interactive)
  (if (not jf/gptel--session-dir)
      (message "No active gptel session in this buffer")
    (let ((current-link (expand-file-name "current" jf/gptel--session-dir)))
      (if (not (file-symlink-p current-link))
          (message "No current position marker found")
        (let ((target (file-truename current-link)))
          (if (fboundp 'dirvish)
              (progn
                (dirvish (file-name-directory target))
                (goto-char (point-min))
                (search-forward (file-name-nondirectory target) nil t)
                (beginning-of-line))
            (dired (file-name-directory target))))))))

(defun jf/gptel-view-context-at-point ()
  "View context.md file in the directory at point.
If point is on a msg-N or response-N directory, opens its context.md file."
  (interactive)
  (let* ((file-at-point (dired-get-filename nil t))
         (context-file (if (and file-at-point (file-directory-p file-at-point))
                          (expand-file-name "context.md" file-at-point)
                        file-at-point)))
    (if (and context-file (file-exists-p context-file))
        (progn
          (find-file-read-only-other-window context-file)
          (goto-char (point-min))
          (message "Viewing context: %s" (file-name-nondirectory (file-name-directory context-file))))
      (message "No context.md file found at point"))))

(defun jf/gptel-view-tools-at-point ()
  "View tools.md file in the directory at point.
If point is on a msg-N or response-N directory with tools.md, opens it."
  (interactive)
  (let* ((file-at-point (dired-get-filename nil t))
         (tools-file (if (and file-at-point (file-directory-p file-at-point))
                        (expand-file-name "tools.md" file-at-point)
                      nil)))
    (if (and tools-file (file-exists-p tools-file))
        (progn
          (find-file-read-only-other-window tools-file)
          (goto-char (point-min))
          (message "Viewing tools: %s" (file-name-nondirectory (file-name-directory tools-file))))
      (message "No tools.md file found at point"))))

(defvar jf/gptel-session-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") #'jf/gptel-view-context-at-point)
    (define-key map (kbd "C-c C-t") #'jf/gptel-view-tools-at-point)
    (define-key map (kbd "C-c C-p") #'jf/gptel-show-current-position)
    (define-key map (kbd "C-c C-r") #'jf/gptel-resume-from-context)
    map)
  "Keymap for gptel-session-tree-mode.")

(define-minor-mode jf/gptel-session-tree-mode
  "Minor mode for navigating gptel session trees.
Provides keybindings for viewing context files and navigating conversation trees.

\\{jf/gptel-session-tree-mode-map}"
  :lighter " GPTel-Tree"
  :keymap jf/gptel-session-tree-mode-map)

(defun jf/gptel--enable-tree-mode-if-session-dir ()
  "Enable gptel-session-tree-mode if current directory is a gptel session."
  (when (and (derived-mode-p 'dired-mode)
             (string-prefix-p (expand-file-name jf/gptel-sessions-directory)
                            default-directory))
    (jf/gptel-session-tree-mode 1)))

;; Hook into dired-mode
(add-hook 'dired-mode-hook #'jf/gptel--enable-tree-mode-if-session-dir)

(defun jf/gptel--is-session-directory-p (dir)
  "Return t if DIR is a gptel session directory.
Checks for presence of metadata.json file."
  (file-exists-p (expand-file-name "metadata.json" dir)))

(defun jf/gptel--get-session-info (session-dir)
  "Get session information from SESSION-DIR.
Returns plist with :id, :model, :backend, :created."
  (when-let ((metadata (jf/gptel--read-metadata session-dir)))
    (list :id (file-name-nondirectory session-dir)
          :model (plist-get metadata :model)
          :backend (plist-get metadata :backend)
          :created (plist-get metadata :created))))

(provide 'jf/gptel-session-browser)
