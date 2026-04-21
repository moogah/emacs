---
name: auto-init-simplify-org-guards
description: Drop the redundant string-suffix-p ".org" guard in jf/gptel--auto-init-session-buffer; inner string= file-name "session.org" check is stricter
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:sessions-auto-init
---

## Files to modify
- `config/gptel/sessions/commands.org` (`jf/gptel--auto-init-session-buffer`
  fast-path guards at `:276-289`)
- `config/gptel/sessions/commands.el` (re-tangled)

## Implementation steps
1. Remove the outer `(string-suffix-p ".org" (buffer-file-name))` check.
   The inner `(string= file-name "session.org")` is stricter (exact
   match, not suffix) and already short-circuits files named
   `my-session.org` that the outer suffix check would admit.
2. Keep the inner `string=` check in both the branch-path and
   agent-path regex branches — it's the real gate.
3. Re-tangle.

## Design rationale
Cosmetic cleanup. No behavior change expected — the inner check
dominates the outer. Smaller guard surface is easier to reason about
and slightly cheaper on every `.org` open.

Non-blocking — does not gate downstream tasks.

## Verification
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` passes
  (existing suite exercises both branch and agent paths and unrelated
  `.org` files).
- `grep -n "string-suffix-p.*\\.org" config/gptel/sessions/commands.el`
  returns no hits in `jf/gptel--auto-init-session-buffer`.

## Context
- Review of `sessions-auto-init` (2026-04-21, orch-review-1776774164),
  Finding #5 (non-blocking code-quality).
