---
name: harden-system-prompt-save-against-missing-chat-heading
description: The heading-present branch of gptel-chat--write-system-prompt-heading computes the System Prompt subtree end by searching forward for the next `* ` heading and falling back to point-max. When a session.org has a `* System Prompt` heading but no following `* Chat` heading, the save deletes everything to end-of-buffer — silently losing any turn blocks below the orphaned heading. Add a defensive guard so an off-nominal layout cannot cause silent conversation data loss.
change: gptel-drawer-as-source-of-truth
status: ready
relations:
  - discovered-from:make-system-prompt-heading-authoritative
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — guard the heading-present branch of `gptel-chat--write-system-prompt-heading`
- `config/gptel/chat/test/menu/save-state-spec.el` (modify) — add an off-nominal-layout scenario

## Why

Author-blind Reviewer finding on `make-system-prompt-heading-authoritative` (advisory). In the heading-present save branch, `subtree-end` is the next `^\* ` heading or `point-max` when none follows. For a document with a `* System Prompt` heading but **no** `* Chat` heading, `subtree-end` is `point-max`: the save deletes from the heading body to end-of-buffer — including any turn blocks living below `* System Prompt` with no `* Chat` between them — and writes only the prompt body. The materialise branch is not reached (a `* System Prompt` heading *is* present), so no `* Chat` is recreated. Result: a document with `* System Prompt`, no `* Chat`, no turn blocks — a violation of `register/shape/session-document-layout`'s "exactly one `* Chat`" / "turn blocks under `* Chat`" invariants, and silent loss of the conversation.

This is an off-nominal layout — the creation renderer and the materialise path both always emit `* Chat` alongside `* System Prompt`, and every test fixture pairs them, which is why the suite is green. But a user who hand-deletes the `* Chat` heading, or a partially materialised document from an interrupted save, would lose turn content on the next save with no warning. The save path is a contract surface; a defensive guard belongs in this change.

## Implementation steps

1. In the heading-present branch of `gptel-chat--write-system-prompt-heading`, detect the "`* System Prompt` present, `* Chat` absent" condition.
2. Choose one of: (a) bound `subtree-end` at the first `^#\+begin_\(user\|assistant\)` turn-block marker rather than blindly at `point-max`; or (b) treat the missing-`* Chat` layout as a layout error — skip the in-place rewrite (or materialise the missing `* Chat` and move turn blocks under it, reusing `jf/gptel--session-headings-block`). Option (b) is preferred since it restores the canonical layout rather than just avoiding the deletion.
3. Re-tangle `config/gptel/chat/menu.org`.
4. Add a `save-state-spec.el` scenario: a buffer with `* System Prompt` + turn blocks but no `* Chat`, save, assert no turn content is lost and the resulting document satisfies `register/shape/session-document-layout`.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/run-tests.sh -d config/gptel/chat
grep -n 'subtree-end\|write-system-prompt-heading' config/gptel/chat/menu.el
```

Expect: saving an orphaned-`* System Prompt` document preserves turn blocks; no path deletes to `point-max` unconditionally.

## Context

Provenance: author-blind Reviewer finding on `make-system-prompt-heading-authoritative` (cycle-7 execute), `discovered_by: reviewer`, `discovered_class: deviation`. Review file: `.orchestrator/cycles/cycle-1779477564/reviews/make-system-prompt-heading-authoritative.md`, Finding 1.

Cited register entries: `interfaces.org#register-shape-session-document-layout` (the layout this guard protects), `interfaces.org#register-invariant-system-prompt-heading-authoritative` (the save path being hardened).
