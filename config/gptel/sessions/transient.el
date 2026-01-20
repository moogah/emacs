;;; transient.el --- GPTEL Session Transient Menu -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Transient menu for gptel session management.
;; Provides unified interface for session commands.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-branching)

(transient-define-prefix jf/gptel-session-browser-menu ()
  "Transient menu for gptel session browser commands."
  ["GPTEL Session Browser"
   ["Browse"
    ("b" "Browse sessions" jf/gptel-browse-sessions)
    ("o" "Open session" jf/gptel-open-session)
    ("p" "Show current position" jf/gptel-show-current-position)]
   ["View"
    ("v" "View context at point" jf/gptel-view-context-at-point)
    ("t" "View tools at point" jf/gptel-view-tools-at-point)]
   ["Actions"
    ("r" "Resume from context" jf/gptel-resume-from-context)
    ("B" "Branch from point" jf/gptel-branch-from-point)
    ("s" "Send from context" jf/gptel-send-from-context)
    ("S" "Switch branch" jf/gptel-switch-branch)]
   ["Info"
    ("i" "Session info" jf/gptel-session-show-info)
    ("?" "Help" jf/gptel-session-help)]
   ["Quit"
    ("q" "Quit" transient-quit-one)]])

(defun jf/gptel-session-show-info ()
  "Show information about the current session or session at point."
  (interactive)
  (let* ((session (if jf/gptel--session-id
                     (jf/gptel-session-find jf/gptel--session-id)
                   ;; Try to get session from dired point
                   (when (derived-mode-p 'dired-mode)
                     (let* ((dir (dired-get-filename nil t))
                            (session-dir (if (file-directory-p dir)
                                           dir
                                         (file-name-directory dir))))
                       (when (jf/gptel--valid-session-directory-p session-dir)
                         (let ((session-id (jf/gptel--session-id-from-directory session-dir)))
                           (jf/gptel-session-find session-id)))))))
         (metadata (when session (plist-get session :metadata))))

    (if (not session)
        (message "No session found")
      (let ((info-buffer (get-buffer-create "*GPTEL Session Info*")))
        (with-current-buffer info-buffer
          (erase-buffer)
          (insert "GPTEL Session Information\n")
          (insert "=========================\n\n")
          (insert (format "Session ID: %s\n" (plist-get session :session-id)))
          (insert (format "Directory: %s\n" (plist-get session :directory)))
          (insert (format "Created: %s\n" (plist-get session :created)))
          (insert (format "Backend: %s\n" (plist-get metadata :backend)))
          (insert (format "Model: %s\n" (plist-get metadata :model)))
          (when-let ((buffer (plist-get session :buffer)))
            (insert (format "Active Buffer: %s\n" (buffer-name buffer))))
          (insert "\n")
          (when-let ((branches (jf/gptel--list-branches (plist-get session :directory))))
            (insert "Branches:\n")
            (dolist (branch branches)
              (insert (format "  - %s\n" (car branch))))))
        (display-buffer info-buffer)))))

(defun jf/gptel-session-help ()
  "Display help for gptel session browser."
  (interactive)
  (let ((help-buffer (get-buffer-create "*GPTEL Session Help*")))
    (with-current-buffer help-buffer
      (erase-buffer)
      (insert "GPTEL Session Browser Help\n")
      (insert "===========================\n\n")
      (insert "Keybindings:\n")
      (insert "  ? - Show this menu\n")
      (insert "  b - Browse sessions directory\n")
      (insert "  o - Open specific session\n")
      (insert "  v - View context.md at point\n")
      (insert "  t - View tools.md at point\n")
      (insert "  r - Resume session from context\n")
      (insert "  B - Create branch from point\n")
      (insert "  S - Switch between branches\n")
      (insert "  p - Show current position in tree\n")
      (insert "  q - Quit window\n\n")
      (insert "Session Structure:\n")
      (insert "  Each session is a directory containing:\n")
      (insert "    - context.md: Main conversation\n")
      (insert "    - metadata.json: Session configuration\n")
      (insert "    - tools.md: Tool call log\n")
      (insert "    - system-prompts.md: System prompt history\n")
      (insert "    - subagents/: Nested agent sessions\n\n")
      (insert "Branching:\n")
      (insert "  Create alternate conversation paths by branching.\n")
      (insert "  Branches are stored as context-<name>.md files.\n"))
    (display-buffer help-buffer)))

(provide 'gptel-session-transient)
;;; transient.el ends here
