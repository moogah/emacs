;;; autosave-never-writes-saved-state.test.el --- speculated invariant -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/autosave-never-writes-saved-state
;; generated-at: 2026-05-24T20:06:31Z
;; license: implementor-may-revise
;;
;; The structural fix for v1 MVP gap D8 — re-introducing tab-switch
;; advice must be clobber-impossible. Each autosave trigger gets its
;; own pinned scenario; the property is "for every trigger, post-fire
;; :saved-state equals pre-fire :saved-state".

(require 'buttercup)

(describe "Invariant: autosave-never-writes-saved-state"

  (it "tab-bar-select-tab advice does not modify :saved-state"
    (error "speculated; not implemented — populate :saved-state with marker \
S-MARK, fire the tab-switch advice (mocked at the tab-bar-select-tab boundary), \
assert post-fire :saved-state is eq to S-MARK and :working-state is non-nil."))

  (it "tab-bar-switch-to-tab advice does not modify :saved-state"
    (error "speculated; not implemented — same assertion via the named-switch \
entry point. Both advice forms must be tested separately; a future contributor \
could fix one and miss the other."))

  (it "workspaces-mode idle timer does not modify :saved-state"
    (error "speculated; not implemented — register the idle timer (or call \
the timer's callback directly via `funcall' on the registered function), \
assert :saved-state is unchanged. Mock at run-with-idle-timer."))

  (it "workspace--kill-emacs-flush does not modify :saved-state"
    (error "speculated; not implemented — set :saved-state to S-MARK, call \
workspace--kill-emacs-flush, assert :saved-state is still S-MARK and \
:working-state was populated from the live frame just before the flush."))

  (it "workspace--slot-for-trigger returns :working-state for every autosave trigger"
    (error "speculated; not implemented — assert (workspace--slot-for-trigger \
t) returns :working-state for t in (tab-switch idle kill-emacs switch-layout), \
and returns :saved-state for t in (explicit-save save-layout new-home-stamp). \
This locks the vocabulary's canonical mapping function.")))

(provide 'autosave-never-writes-saved-state.test)
;;; autosave-never-writes-saved-state.test.el ends here
