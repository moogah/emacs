;;; scope-coverage-threshold-is-1.test.el --- Invariant scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/invariant/scope-coverage-threshold-is-1
;; generated-at: 2026-04-29T11:05:33Z
;; license: implementor-may-revise

(require 'buttercup)

(describe "invariant: scope-coverage-threshold-is-1"

  (it "module-level constant equals 1.0"
    (error
     "speculated; not implemented — bind jf/gptel-scope--coverage-threshold to 1.0 in scope-validation.el and assert here"))

  (it "no caller reads (plist-get config :security :max-coverage-threshold)"
    (error
     "speculated; not implemented — assert grep on scope-validation.el (and any caller) finds zero :max-coverage-threshold reads after rewire-validator-config-load"))

  (it "Stage 5 emits a warn (non-blocking) when coverage-ratio < 1.0"
    (error
     "speculated; not implemented — exercise validate-bash with a coverage-ratio < 1.0 fixture and assert exactly one display-warning call"))

  (it "Stage 5 is silent when coverage-ratio = 1.0"
    (error
     "speculated; not implemented — exercise validate-bash with coverage-ratio 1.0 and assert no warning emitted")))

(provide 'scope-coverage-threshold-is-1)
;;; scope-coverage-threshold-is-1.test.el ends here
