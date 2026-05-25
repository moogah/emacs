;;; workspaces-mode.el --- Idle-timer crash-safety mode -*- lexical-binding: t; -*-

(require 'cl-lib)

(defcustom workspaces-mode-idle-frequency 60
  "Seconds of idle time before `workspaces-mode' captures :working-state.
Set to nil to disable the idle-save trigger while keeping the mode on
\(rare; the explicit save and tab-switch autosave remain active)."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'workspaces)

(defvar workspaces-mode--idle-timer nil
  "Active idle timer for `workspaces-mode'.
Set when the mode is enabled, cleared when disabled.  Internal — do
not bind directly; toggle the mode instead.")

(defun workspaces-mode--idle-tick ()
  "Idle-timer body for `workspaces-mode'.
Captures the current workspace's :working-state, no-op when no
workspace is current (e.g. transient state between
`workspace-close' and `workspace-switch', or a frame that has never
been on a workspaces-managed tab)."
  (when (workspace--current-name)
    (workspace--autosave-current-layout :working-state)))

(define-minor-mode workspaces-mode
  "Global minor mode for periodic background save of workspace working state.
When enabled, runs an idle timer (interval
`workspaces-mode-idle-frequency') that captures the current
workspace's :working-state, providing a crash-safety net atop the
explicit save and tab-switch autosave.  Off by default."
  :global t
  :group 'workspaces
  (when workspaces-mode--idle-timer
    (cancel-timer workspaces-mode--idle-timer)
    (setq workspaces-mode--idle-timer nil))
  (when (and workspaces-mode workspaces-mode-idle-frequency)
    (setq workspaces-mode--idle-timer
          (run-with-idle-timer workspaces-mode-idle-frequency
                               t
                               #'workspaces-mode--idle-tick))))

(provide 'workspaces-mode)
;;; workspaces-mode.el ends here
