;;; workspaces-mode.el --- Idle-timer crash-safety mode -*- lexical-binding: t; -*-

(require 'cl-lib)

(declare-function workspace--flush-state "workspace-persistence")

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
Captures the current workspace's :working-state and flushes it to disk
synchronously.  No-op when no workspace is current (e.g. transient state
between `workspace-close' and `workspace-switch', or a frame that has
never been on a workspaces-managed tab).  Also no-op when any predicate
in `workspace-anti-save-predicates' returns non-nil — e.g. during
minibuffer activity or when a *Backtrace* window is visible.

The flush is synchronous (no debounce): the tick fires at most once per
idle period, so there is no burst to coalesce, and a debounced write
armed from within an already-long idle period would be deferred to a
future idle period (the `run-with-idle-timer' already-idle trap),
defeating the idle save's crash-safety purpose."
  (when (workspace--current-name)
    (unless (run-hook-with-args-until-success 'workspace-anti-save-predicates)
      (workspace--autosave-current-layout :working-state)
      (workspace--flush-state))))

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
