;;; scope-parse-complete-is-true.test.el --- Invariant scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/invariant/scope-parse-complete-is-true
;; generated-at: 2026-04-29T11:05:33Z
;; license: implementor-may-revise
;;
;; This file is a speculative scaffold. The Implementor may revise it
;; (and explain the revision in their `## Discoveries`) or escalate to
;; the orchestrator if the speculation is wrong.

(require 'buttercup)

(describe "invariant: scope-parse-complete-is-true"

  (it "module-level constant is exactly t (not nil, not 1, not 'yes)"
    ;; This stub fails loudly until an Implementor binds the constant
    ;; in scope-validation.el AND wires this scaffold to it.
    (error
     "speculated; not implemented — bind jf/gptel-scope--enforce-parse-complete to t in scope-validation.el and require it from this spec"))

  (it "no caller reads (plist-get config :security :enforce-parse-complete)"
    ;; Structural assertion: grep scope-validation.el (and any caller)
    ;; for :security plist reads after the rewire-validator-config-load
    ;; task lands.
    (error
     "speculated; not implemented — replace with a grep-style assertion that no .el file under config/gptel/scope/ contains the substring \":security\" after the cleanup task"))

  (it "Stage 1 of bash validation refuses parse-incomplete unconditionally"
    ;; Invariant the constant exists to guarantee.
    (error
     "speculated; not implemented — exercise validate-bash with a parse-incomplete command and assert :error \"parse_incomplete\" with no warn-only branch")))

(provide 'scope-parse-complete-is-true)
;;; scope-parse-complete-is-true.test.el ends here
