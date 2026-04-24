---
name: design-chaining-rationale-temp-file-note
description: Extend design.md Decision 3 chaining rationale to note upstream's own temp-file cleanup rides :post
change: gptel-chat-mode
status: done
relations:
  - discovered-from:fsm-handlers-upstream-integration
---

## Files to modify
- `openspec/changes/gptel-chat-mode/design.md` (Decision 3 rationale)

## Implementation steps
1. In `design.md:102-104`, find the paragraph explaining why
   `gptel--handle-post` is chained on DONE/ERRS/ABRT.
2. Add one sentence noting that upstream itself rides `:post` for temp-file
   cleanup (curl's large-data path pushes a `cleanup-fn` onto
   `info[:post]` at `gptel-request.el:2539-2541`), so replacing rather
   than chaining would leak a temp file per large request — not just drop
   caller-supplied hooks.

## Design rationale
The current rationale frames the "chain vs replace" decision around
caller-supplied hooks (session export, budget tracking, activities).
That understates the case: the very first consumer of `:post` is
upstream's own cleanup machinery. Strengthens the "must chain" argument
for future readers reconsidering the design.

## Verification
- `grep -n "temp-file" openspec/changes/gptel-chat-mode/design.md` shows
  the new note.
- `grep -n "gptel-request.el:2539" openspec/changes/gptel-chat-mode/design.md`
  shows the upstream reference.

## Context
- Review of `fsm-handlers-upstream-integration` (2026-04-21,
  orch-review-1776774164), Finding #1 (non-blocking spec-signal).
- Upstream: `runtime/straight/build/gptel/gptel-request.el:2539-2541`.

## Review
- **Session:** orch-review-1776785000 (2026-04-21), agent `a60e3535129eccb77`
- **Verdict:** clean
- **Findings:** none
- **Checked and ruled out:**
  - Line-number citation accurate: `gptel-request.el:2539-2541`
    corresponds exactly to the `cleanup-fn` lambda and the
    `plist-put info :post (cons cleanup-fn ...)` call in the
    curl-large-data branch.
  - Upstream code unconditionally pushes `cleanup-fn` onto `info[:post]`
    when `data-json` exceeds `gptel-curl-file-size-threshold`; the
    "replacing rather than chaining would leak one temp file per large
    request" claim is literally correct.
  - Sentence materially strengthens Decision 3 — supplies a concrete
    already-shipping consumer to replace hypothetical future ones;
    "not merely drop caller-supplied hooks" ties back without
    contradicting prior sentence.
  - Verification greps from the task body succeed against the
    committed file; no scope creep.
- **Follow-ups:** none
- **Dependents repointed:** none
