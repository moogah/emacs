;;; idle-tick-no-op-off-workspace.test.el --- speculated invariant -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/idle-tick-no-op-off-workspace
;; generated-at: 2026-05-25T00:30:00Z
;; license: implementor-may-revise
;;
;; The enforcement-locus check for the idle-tick callback:
;; (workspace--current-name) → if nil, return without touching
;; workspace--autosave-current-layout. Cited design.md section: §D6
;; ("Idle save is specifically the 'crash safety' net, not the
;; primary persistence trigger") — the tick is not authoritative on
;; "is there a workspace to save"; it must check.

(require 'buttercup)

(describe "Invariant: idle-tick-no-op-off-workspace"

  (it "workspaces-mode--idle-tick does not call workspace--autosave-current-layout when (workspace--current-name) is nil"
    (error "speculated; not implemented — cl-letf bind workspace--current-name \
to (lambda () nil) and workspace--autosave-current-layout to a spy that \
records every invocation; funcall workspaces-mode--idle-tick; assert the spy \
was never called."))

  (it "workspaces-mode--idle-tick DOES call workspace--autosave-current-layout :working-state when a current workspace name exists"
    (error "speculated; not implemented — cl-letf workspace--current-name to \
return \"alpha\" and workspace--autosave-current-layout to a spy; funcall \
workspaces-mode--idle-tick; assert exactly one call with arg :working-state. \
This is the positive-path companion to scenario 1; pinning both directions \
catches a future refactor that inverts the guard."))

  (it "workspaces-mode--idle-tick passes :working-state explicitly, never :saved-state and never a default"
    (error "speculated; not implemented — same fixture as scenario 2; assert \
the recorded arg is :working-state by EQ, not just by presence. This is the \
cycle-1 inline-fix b70a5b4 lesson: required-slot signatures pin the call site \
shape; an off-by-one keyword would silently corrupt the saved baseline."))

  (it "the (when (workspace--current-name) ...) guard is the FIRST form of the callback body"
    (error "speculated; not implemented — structural assertion. Read the \
tangled config/workspaces/workspaces-mode.el via find-file; navigate to the \
defun workspaces-mode--idle-tick; assert the body's first form matches the \
shape (when (workspace--current-name) ...) or equivalent. Catches a refactor \
that moves the guard below a side-effecting call.")))

(provide 'idle-tick-no-op-off-workspace.test)
;;; idle-tick-no-op-off-workspace.test.el ends here
