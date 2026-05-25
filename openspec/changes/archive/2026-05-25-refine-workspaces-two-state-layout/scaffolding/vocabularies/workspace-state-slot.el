;;; workspace-state-slot.el --- speculated vocabulary -*- lexical-binding: t; -*-

;; scaffolding-of: register/vocabulary/workspace-state-slot
;; generated-at: 2026-05-24T20:06:31Z
;; license: implementor-may-revise
;;
;; Canonical mapping function shell: trigger symbol → state-slot
;; keyword. The closed value set is asserted by the error arms on
;; unhandled inputs — any new trigger added by the implementor must
;; either land in the pcase or be rejected loudly. The Implementor
;; replaces each (error ...) body with the routing decision documented
;; in the register entry (and in the canonical_mapping_function field
;; of that entry).

(defun workspace--slot-for-trigger (trigger)
  "Return the layout state-slot keyword to write for TRIGGER.

Speculation: every autosave trigger writes :working-state; every
explicit-user trigger writes :saved-state. The Implementor MUST replace
each `error' arm with the speculated return value (or revise the
speculation in `## Discoveries')."
  (pcase trigger
    ('explicit-save
     (error "TODO: workspace--slot-for-trigger 'explicit-save → :saved-state"))
    ('save-layout
     (error "TODO: workspace--slot-for-trigger 'save-layout → :saved-state"))
    ('new-home-stamp
     (error "TODO: workspace--slot-for-trigger 'new-home-stamp → :saved-state"))
    ('switch-layout
     (error "TODO: workspace--slot-for-trigger 'switch-layout → :working-state"))
    ('tab-switch
     (error "TODO: workspace--slot-for-trigger 'tab-switch → :working-state"))
    ('idle
     (error "TODO: workspace--slot-for-trigger 'idle → :working-state"))
    ('kill-emacs
     (error "TODO: workspace--slot-for-trigger 'kill-emacs → :working-state"))
    (_
     (error "workspace--slot-for-trigger: unknown trigger %S \
(closed vocabulary; add to register/vocabulary/workspace-state-slot if \
genuinely a new trigger, not a typo)" trigger))))

;; Validator helper — confirms the closed-set membership.
(defun workspace--state-slot-p (slot)
  "Return non-nil if SLOT is a valid workspace state-slot keyword."
  (memq slot '(:saved-state :working-state)))

(provide 'workspace-state-slot)
;;; workspace-state-slot.el ends here
