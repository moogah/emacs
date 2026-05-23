;;; expansion-queue-always-progresses.test.el --- Invariant scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/invariant/expansion-queue-always-progresses
;; generated-at: 2026-04-29T17:30:00Z (cycle-3 plan)
;; license: implementor-may-revise
;;
;; Statement: every terminal action handler of
;; `jf/gptel-scope-expansion-menu' calls
;; `jf/gptel-scope--process-expansion-queue' after `transient-quit-one'.
;; The five terminal handlers are --deny-expansion, --add-to-scope,
;; --allow-once-action, --add-wildcard-to-scope, --add-custom-to-scope.
;;
;; Cycle-2 origin: rewire-expansion-writer (commit 18e290a) added the
;; queue-pump call to --add-wildcard-to-scope and --add-custom-to-scope,
;; closing a latent bug. Cycle-3 harden-add-to-scope-action-handler
;; must preserve the invariant in its new refusal / redirect / no-op
;; branches (Stage 1 + Stage 4).

(require 'buttercup)

(describe "invariant: expansion-queue-always-progresses (L1 structural)"

  (it "--deny-expansion contains a call to --process-expansion-queue"
    (error
     "speculated; not implemented — load scope-expansion.el; locate the body of jf/gptel-scope--deny-expansion; grep its source for `process-expansion-queue` and assert the count is >= 1"))

  (it "--add-to-scope contains a call to --process-expansion-queue"
    (error
     "speculated; not implemented — same pattern: function-source --add-to-scope ; assert process-expansion-queue is called"))

  (it "--allow-once-action contains a call to --process-expansion-queue"
    (error
     "speculated; not implemented — same pattern for --allow-once-action"))

  (it "--add-wildcard-to-scope contains a call to --process-expansion-queue"
    (error
     "speculated; not implemented — same pattern for --add-wildcard-to-scope (regression coverage for the cycle-2 bug fix)"))

  (it "--add-custom-to-scope contains a call to --process-expansion-queue"
    (error
     "speculated; not implemented — same pattern for --add-custom-to-scope (regression coverage for the cycle-2 bug fix)"))

  (it "every terminal handler that calls the wrapper callback also pumps the queue"
    (error
     "speculated; not implemented — across each terminal handler, assert the structural pairing: any branch that invokes (funcall callback ...) is followed by a call to --process-expansion-queue (after transient-quit-one). Detects future regressions where a refusal/no-op branch returns early without pumping. Particularly relevant after harden-add-to-scope-action-handler's Stage 1 + Stage 4 land")))

(describe "invariant: expansion-queue-always-progresses (L2 runtime)"

  (it "a three-element queue drains monotonically across three menu actions"
    (error
     "speculated; not implemented — fixture three synthetic violations queued via --prompt-expansion; spy on transient-setup; for each action handler exercised (deny / add / allow-once), assert queue length AFTER < queue length BEFORE; final queue length is 0"))

  (it "a refusal branch does not strand the queue"
    (error
     "speculated; not implemented — once harden-add-to-scope-action-handler lands its Stage 1 refusal branches, queue a violation that the new branch refuses (e.g. nil :operation cloud-auth without provider list); assert the queue still drains, the wrapper callback fires with :success nil, and the next queued violation surfaces"))

  (it "a dedup short-circuit does not strand the queue"
    (error
     "speculated; not implemented — once harden-add-to-scope-action-handler lands Stage 4, queue a violation whose pattern is already in the chat buffer's drawer; assert the action handler emits :success t :patterns_added [] :message \"Pattern already in scope\" AND that the next queued violation is processed (queue length decrements)")))

(provide 'expansion-queue-always-progresses)
;;; expansion-queue-always-progresses.test.el ends here
