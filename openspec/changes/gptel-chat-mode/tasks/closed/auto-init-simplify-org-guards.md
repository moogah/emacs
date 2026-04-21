---
name: auto-init-simplify-org-guards
description: Drop the redundant string-suffix-p ".org" guard in jf/gptel--auto-init-session-buffer; inner string= file-name "session.org" check is stricter
change: gptel-chat-mode
status: done
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

## Review
- **Session:** orch-review-1776789773 (2026-04-21), agent `a5138c0dcde9e4c3b`
- **Verdict:** clean
- **Findings:** none
- **Checked and ruled out:**
  - **Guard subsumption**: inner `(string= file-name "session.org")` is
    strictly stricter than outer `(string-suffix-p ".org" ...)` across
    symlinks, case-insensitive filesystems, `.org_archive`, trailing
    slashes, and `my-session.org`. Inner was already the real gate.
  - **Re-tangle fidelity**: `.org` and `.el` diffs align line-for-line;
    lexical-binding header preserved.
  - **Comment history**: trivial `;; Is .org file?` comment replaced
    with useful rationale; no historical context lost.
  - **Downstream impact**: no open task references the removed guard.
- **Verification:** `config/gptel/sessions/test/commands/test-report.txt`
  reports 8/8 Buttercup passing. Commit message cited 10/10; discrepancy
  is a pre-snapshot-regeneration count, both green.
- **Follow-ups:** none
- **Dependents repointed:** none
