;;; restore-precedence-working-over-saved.test.el --- speculated invariant -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/restore-precedence-working-over-saved
;; generated-at: 2026-05-24T20:06:31Z
;; license: implementor-may-revise
;;
;; Architect-generated failing-stub spec. The Implementor must make
;; this pass (by ensuring workspace--apply-saved-layout reads via
;; workspace--layout-effective-state, with :working-state preferred
;; over :saved-state), or revise this scaffold with justification in
;; the task's `## Discoveries` section.

(require 'buttercup)

(describe "Invariant: restore-precedence-working-over-saved"

  (it "applies :working-state when both slots are populated"
    (error "speculated; not implemented — make workspace--apply-saved-layout \
prefer :working-state over :saved-state and assert the chosen state by \
populating both slots with distinguishable markers"))

  (it "applies :saved-state when :working-state is nil"
    (error "speculated; not implemented — populate :saved-state only, \
leave :working-state nil, assert the restored frame matches :saved-state"))

  (it "uses workspace--layout-effective-state as the single dispatch point"
    (error "speculated; not implemented — assert that workspace--apply-saved-layout \
does NOT read :saved-state or :working-state directly; only via \
workspace--layout-effective-state. A direct slot read would silently regress \
the precedence on a future refactor.")))

(provide 'restore-precedence-working-over-saved.test)
;;; restore-precedence-working-over-saved.test.el ends here
