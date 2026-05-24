;;; explicit-save-bypasses-anti-save.test.el --- speculated invariant -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/explicit-save-bypasses-anti-save
;; generated-at: 2026-05-24T20:06:31Z
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: explicit-save-bypasses-anti-save"

  (it "workspace-save proceeds even when an always-non-nil predicate is registered"
    (error "speculated; not implemented — let-bind \
workspace-anti-save-predicates to '((lambda () t)), call workspace-save, \
assert :saved-state was populated and the state file was written. Restore \
the predicate list after the spec."))

  (it "tab-switch autosave IS suppressed by the same predicate"
    (error "speculated; not implemented — same predicate fixture as above, \
fire the tab-switch advice, assert :working-state is unchanged. This is the \
complement that confirms the predicates are wired and effective on the \
guarded path."))

  (it "workspace-save's entry point does not call run-hook-with-args-until-success"
    (error "speculated; not implemented — structural assertion: \
(grep '(run-hook-with-args-until-success ...workspace-anti-save-predicates' \
       config/workspaces/persistence.org) \
must NOT match inside workspace-save. Acceptable to encode as a \
buttercup-spec via `find-file' + `re-search-forward' guarded by `point-min'.")))

(provide 'explicit-save-bypasses-anti-save.test)
;;; explicit-save-bypasses-anti-save.test.el ends here
