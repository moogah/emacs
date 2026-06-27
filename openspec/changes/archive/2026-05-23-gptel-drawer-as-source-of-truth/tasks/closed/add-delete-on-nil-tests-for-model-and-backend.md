---
name: add-delete-on-nil-tests-for-model-and-backend
description: The chat-save snapshot writer's task brief required write-on-non-nil and delete-on-nil for each scalar key. The unit suite covers delete-on-nil for `:GPTEL_TEMPERATURE:` and `:GPTEL_MAX_TOKENS:` but not for `:GPTEL_MODEL:` or `:GPTEL_BACKEND:`. Add the missing scenarios so a future regression that leaves a stale `:GPTEL_MODEL:` after `gptel-model` is cleared cannot silently break the WYSIWYG contract.
change: gptel-drawer-as-source-of-truth
status: done
relations:
  - discovered-from:replace-chat-save-with-full-snapshot-writer
---

## Files to modify

- `config/gptel/chat/test/menu/save-state-spec.el` — extend the "deletes scalar keys when their source variables are nil" describe (around line 138)

## Why

**Finding 1 (advisory).** The existing delete-on-nil test covers temperature and max-tokens but not model or backend. The implementation (`gptel-chat--put-or-delete` → `org-entry-delete` when the value is nil) is correct; the gap is in coverage. A future change to the writer that accidentally writes `:GPTEL_MODEL:` even when `gptel-model` is nil — or fails to delete a stale entry — would produce on-disk drift without test signal. The WYSIWYG contract (drawer reflects current state) depends on the delete path firing reliably; testing it for half the scalar keys gives partial assurance.

## Implementation steps

1. In `config/gptel/chat/test/menu/save-state-spec.el`, locate the "deletes scalar keys when their source variables are nil" `it` block (around line 138).
2. Add two `it`-blocks (or extend the existing one) mirroring the existing pattern:
   - **"deletes GPTEL_MODEL when gptel-model is nil"** — pre-populate the drawer with a `:GPTEL_MODEL:` entry, set `gptel-model` to nil buffer-locally, run the writer, assert the entry is gone via `(org-entry-get nil "GPTEL_MODEL")` returning nil.
   - **"deletes GPTEL_BACKEND when gptel-backend is nil"** — same pattern for backend.
3. Re-run `./bin/run-tests.sh -d config/gptel/chat` and confirm both new scenarios pass.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/chat
grep -nE "deletes GPTEL_MODEL|deletes GPTEL_BACKEND" config/gptel/chat/test/menu/save-state-spec.el
```

Expect: both new it-blocks present; suite green.

## Context

Full reviewer findings: `.orchestrator/cycles/cycle-1777625426/reviews/replace-chat-save-with-full-snapshot-writer.md` (Finding 1).

Cited register entries: `interfaces.org#register-shape-drawer-text-block`, `interfaces.org#register-invariant-drawer-system-key-write-exclusion`.

## Observations

- Both new specs were placed immediately after the existing "deletes scalar keys when their source variables are nil" `it` block in the `gptel-chat--write-config-drawer (unit)` describe, mirroring its pattern (pre-populated drawer, `setq-local` to nil, run writer, assert `org-entry-get` returns nil).
- Suite size went from 377 to 379 specs; both new specs pass; full chat suite green (`Ran 379 specs, 0 failed, in 2.38s`).
- Verification grep confirms both `it`-block titles are present in `save-state-spec.el`.

## Discoveries

- The writer routes both keys through the shared `gptel-chat--put-or-delete` helper (`config/gptel/chat/menu.el` lines 446-457), so the new specs cover the same delete codepath as the existing temperature/max-tokens spec — they protect against a regression that would bypass `put-or-delete` for these specific keys (e.g., a future inline `org-entry-put` call), not against a regression in `put-or-delete` itself.
- No `.org` source for the spec file — `save-state-spec.el` is hand-written. Edited the `.el` directly per project literate-programming policy (only `.org` sources tangle to `.el`).
