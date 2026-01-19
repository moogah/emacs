;; -*- lexical-binding: t; -*-
(require 'transient)

(defun jf/gptel-session--current-session-info ()
  "Return formatted string with current session information.
Shows session ID and node type if in a session directory."
  (let* ((in-session-dir (and (derived-mode-p 'dired-mode)
                             (boundp 'jf/gptel-sessions-directory)
                             (string-prefix-p (expand-file-name jf/gptel-sessions-directory)
                                            default-directory)))
         (session-id (when in-session-dir
                      (let ((rel-path (file-relative-name
                                      default-directory
                                      (expand-file-name jf/gptel-sessions-directory))))
                        (car (split-string rel-path "/")))))
         (node-type (jf/gptel-session--node-type-at-point)))
    (concat
     (if session-id
         (propertize (format "Session: %s" session-id)
                    'face 'transient-value)
       (propertize "Not in session directory"
                  'face 'transient-inactive-value))
     (when node-type
       (concat " | " (propertize (format "Node: %s" node-type)
                                'face 'transient-value))))))

(defun jf/gptel-session--node-type-at-point ()
  "Return node type at point in dired: 'msg', 'response', or nil.
Checks if point is on a msg-N or response-N directory."
  (when (derived-mode-p 'dired-mode)
    (let* ((file-at-point (dired-get-filename nil t))
           (node-name (when (and file-at-point
                                (file-directory-p file-at-point))
                       (file-name-nondirectory
                        (directory-file-name file-at-point)))))
      (cond
       ((and node-name (string-match "^msg-[0-9]+" node-name)) 'msg)
       ((and node-name (string-match "^response-[0-9]+" node-name)) 'response)
       (t nil)))))

(defun jf/gptel-session--count-sessions ()
  "Return the number of saved gptel sessions."
  (if (not (boundp 'jf/gptel-sessions-directory))
      0
    (let ((sessions-dir (expand-file-name jf/gptel-sessions-directory)))
      (if (not (file-directory-p sessions-dir))
          0
        (length (seq-filter
                (lambda (f)
                  (and (file-directory-p (expand-file-name f sessions-dir))
                       (not (string-prefix-p "." f))))
                (directory-files sessions-dir)))))))

(defun jf/gptel-session--session-count-info ()
  "Return formatted string with session count."
  (let ((count (jf/gptel-session--count-sessions)))
    (if (> count 0)
        (propertize (format "%d session%s available"
                           count
                           (if (= count 1) "" "s"))
                   'face 'transient-value)
      (propertize "No sessions found"
                 'face 'transient-inactive-value))))

;;;###autoload
(transient-define-prefix jf/gptel-session-browser-menu ()
  "GPTEL Session Browser commands.

Provides organized access to session browsing, viewing, and manipulation
operations. Only active when browsing gptel session directories.

Commands are organized into three categories:
- Browse: Navigate and open sessions
- View: Inspect context and tools
- Actions: Resume, branch, and send operations"
  [:description "GPTEL Session Browser"
   [""
    (:info (lambda () (jf/gptel-session--current-session-info))
           :format " %d")]
   [""
    (:info (lambda () (jf/gptel-session--session-count-info))
           :format " %d")]
   [""
    ("q" "Quit" transient-quit-one)]]
  ["Browse"
   ("b" "Browse all sessions" jf/gptel-browse-sessions
    :if (lambda () (fboundp 'jf/gptel-browse-sessions)))
   ("o" "Open session..." jf/gptel-open-session
    :if (lambda () (fboundp 'jf/gptel-open-session)))
   ("p" "Show current position" jf/gptel-show-current-position
    :if (lambda () (fboundp 'jf/gptel-show-current-position)))]
  ["View"
   ("v" "View context" jf/gptel-view-context-at-point
    :if (lambda () (fboundp 'jf/gptel-view-context-at-point)))
   ("t" "View tools" jf/gptel-view-tools-at-point
    :if (lambda () (fboundp 'jf/gptel-view-tools-at-point)))]
  ["Actions"
   ("r" "Resume from context" jf/gptel-resume-from-context
    :if (lambda () (fboundp 'jf/gptel-resume-from-context)))
   ("B" "Branch from point" jf/gptel-branch-from-point
    :if (lambda () (fboundp 'jf/gptel-branch-from-point)))
   ("s" "Send context" jf/gptel-send-from-context
    :if (lambda () (fboundp 'jf/gptel-send-from-context)))]
  (interactive)
  ;; Check if we're in a session directory
  (unless (and (derived-mode-p 'dired-mode)
               (boundp 'jf/gptel-sessions-directory)
               (string-prefix-p (expand-file-name jf/gptel-sessions-directory)
                              default-directory))
    (user-error "Session browser menu only available in gptel session directories"))
  (transient-setup 'jf/gptel-session-browser-menu))

;; Add keybinding to gptel-session-tree-mode-map
(with-eval-after-load 'jf/gptel-session-browser
  (when (boundp 'jf/gptel-session-tree-mode-map)
    (define-key jf/gptel-session-tree-mode-map (kbd "?")
      'jf/gptel-session-browser-menu)))

(provide 'jf/gptel-session-transient)
