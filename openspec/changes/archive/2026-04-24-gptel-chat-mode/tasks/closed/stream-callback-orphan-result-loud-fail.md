---
name: stream-callback-orphan-result-loud-fail
description: Fail loudly (cl-assert or visible marker) when a tool-result arrives without a pending tool-marker, instead of silently dropping
change: gptel-chat-mode
status: done
relations:
  - discovered-from:stream-callback
---

## Files to modify
- `config/gptel/chat/stream.org` (`:707-714` tool-result arm)
- `config/gptel/chat/stream.el` (re-tangled)
- `config/gptel/chat/test/stream/tool-call-spec.el` (add orphan-result
  spec)

## Implementation steps
1. Replace the current `(when marker ...)` guard with either:
   - `(cl-assert marker nil "Orphan tool-result with no pending marker")`
     for a loud failure, or
   - `(unless marker (insert (format "<!-- orphan tool-result: %S -->\n"
     result)))` for a user-visible diagnostic that survives into the
     buffer for debugging.
   Pick whichever matches the rest of the file's error-handling style
   (the `(_ (error ...))` defensive guard at the end of the pcase
   suggests loud-fail is preferred).
2. Re-tangle.
3. Add a spec that drives the callback with a `(tool-result . ...)` but
   no prior `(tool-call . ...)`; assert the chosen behavior.

## Design rationale
The rest of `stream.org` fails loudly on unexpected shapes (defensive
`(_ (error ...))` arm). Silently dropping an orphan tool-result is
inconsistent and makes any future regression that desynchronises the
FIFO invisible. Loud fail keeps the contract strict.

Non-blocking — does not gate downstream tasks, but worth landing
before `verify-change` runs end-to-end.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes with
  the new orphan-result spec.
- `grep -n "when marker" config/gptel/chat/stream.el` returns no hits
  (or only inside the expected assertion).

## Context
- Review of `stream-callback` (2026-04-21, orch-review-1776774164),
  Finding #5 (non-blocking code-quality).

## Review (orch-review-1776792000, 2026-04-21)

Reviewer verified:
- **Post-error state:** `error` fires before `gptel-chat--stream-close-tool-block`, so no partial tool-block write happens for the orphan element. The FIFO is already empty (that's the precondition that triggered the fail), so no leftover markers leak. Silent drop was not safer.
- **Spec quality:** The new spec at `tool-call-spec.el:359-378` calls the real `gptel-chat--stream-callback` factory and feeds a genuine upstream-shaped `(tool-result . ((TOOL ARGS RESULT)))` triple-list. Real code path, not a stripped-down surrogate.
- **Style match:** The new `(error "gptel-chat: orphan tool-result with no pending tool-marker: %S" result)` matches the form of the existing catch-all `(_ (error "gptel-chat: unexpected response shape %S" response))` at `stream.el:678-679` — same prefix, same `%S`.
- **Regression protection:** Four existing positive specs (`tool-call-spec.el:153, 194, 225, 266`) exercise normal tool-call→tool-result pairing; any over-eager rejection would break them. All pass (89 specs, 0 failed).
- **cl-assert rationale:** No other `cl-assert` in `config/` to compare against. The pragmatic `error` choice matches the file's existing defensive-arm style; the Buttercup-throw claim is not load-bearing since the plain-`error` form is arguably the better outcome regardless.
- **Scope:** One pcase arm (org + tangled el) plus one new `describe` block. No related-logic churn.

### Findings
None blocking. One minor style observation (the spec's exact-string match on the formatted signal message at `tool-call-spec.el:378` would break on any reword; a looser regex match would be more maintenance-friendly) — judgment call, did not meet the signal-vs-noise bar for a follow-up task.
