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

  ;; REVISED post-implementation (on-touch architect Finding 3,
  ;; cycle-20260524-200631): per design.md §D4 (two-state semantics),
  ;; workspace-switch-layout is NAVIGATION, not an explicit-save
  ;; variant.  It writes :working-state of the OUTGOING layout, never
  ;; clears the destination's :working-state.  The original
  ;; "save-on-switch clears destination" scenario contradicted the
  ;; design table; dropping the scenario from this invariant.  The
  ;; alternate (and correct) behaviour is pinned by the shipped
  ;; layouts-spec.el test "workspace-switch-layout routes outgoing
  ;; capture to :working-state".

  (it "workspace-new home stamp leaves :working-state nil (new layout, no prior drift)"
    (error "speculated; not implemented — fresh workspace, assert the \
home layout-group is constructed with :working-state nil from the start.")))

(provide 'explicit-save-clears-working-state.test)
;;; explicit-save-clears-working-state.test.el ends here
