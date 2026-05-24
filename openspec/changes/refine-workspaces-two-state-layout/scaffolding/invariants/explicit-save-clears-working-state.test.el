;;; explicit-save-clears-working-state.test.el --- speculated invariant -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/explicit-save-clears-working-state
;; generated-at: 2026-05-24T20:06:31Z
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: explicit-save-clears-working-state"

  (it "workspace-save clears :working-state on the affected layout"
    (error "speculated; not implemented — populate the layout with a non-nil \
:working-state, call workspace-save, assert the post-save layout has \
:working-state = nil and :saved-state = newly-captured form."))

  (it "workspace-save-layout NAME clears :working-state on the named layout"
    (error "speculated; not implemented — same assertion against \
workspace-save-layout invoked on a non-recent named layout-group; the named \
layout's :working-state must be nil after the call."))

  (it "workspace-switch-layout's save-on-switch clears :working-state on the destination"
    (error "speculated; not implemented — switch into a layout whose \
:working-state is populated, assert the post-switch slot is nil. If the \
design says the switch-on-save also propagates the clear, assert here; \
otherwise revise the scaffold and note the design choice in `## Discoveries`."))

  (it "workspace-new home stamp leaves :working-state nil (new layout, no prior drift)"
    (error "speculated; not implemented — fresh workspace, assert the \
home layout-group is constructed with :working-state nil from the start.")))

(provide 'explicit-save-clears-working-state.test)
;;; explicit-save-clears-working-state.test.el ends here
