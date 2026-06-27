---
name: harden-system-prompt-save-against-missing-chat-heading
description: The heading-present branch of gptel-chat--write-system-prompt-heading computes the System Prompt subtree end by searching forward for the next `* ` heading and falling back to point-max. When a session.org has a `* System Prompt` heading but no following `* Chat` heading, the save deletes everything to end-of-buffer — silently losing any turn blocks below the orphaned heading. Add a defensive guard so an off-nominal layout cannot cause silent conversation data loss.
change: gptel-drawer-as-source-of-truth
status: done
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

## Observations

- Implemented option (b) of the task body: detect the orphaned-`* System Prompt` layout and re-materialise `* Chat`, moving any captured turn blocks under it. The heading-present branch of `gptel-chat--write-system-prompt-heading` now computes `chat-heading-present` (whole-buffer search for `^\\* Chat[ \t]*$`) and, when false, locates the first `#+begin_user`/`#+begin_assistant` marker in (body-start, subtree-end) and bounds the in-place rewrite at that position rather than at `point-max`. The captured turn-block text is re-inserted after a literal `* Chat\n` heading appended just after the system-prompt body.
- The literal `"* Chat\n"` is hard-coded in the recovery branch rather than routed through `jf/gptel--session-headings-block`. The helper emits *both* `* System Prompt` and `* Chat` together; in the orphan-recovery case the `* System Prompt` heading already exists (and carries its own preserved `:VISIBILITY: folded` drawer), so reusing the helper would duplicate it. The `* Chat` literal is intentionally bare in the helper as well (no properties drawer), so the two forms agree by inspection. Cross-referenced in the docstring and comment.
- Introduced `gptel-chat--turn-block-marker-re` as a named constant. The same regexp appears in the validator inside `register/shape/session-document-layout` (line ~488) and the new recovery branch; centralising it inside `menu.org` keeps the chat-mode writer self-contained without reaching into the sessions module, and the docstring cites the parser's heading-indifferent marker contract (Decision 12) so a future reader knows which authority the regexp tracks.
- Tests added: one new `describe` block ("off-nominal layout: `* System Prompt` without `* Chat`") with 7 specs — turn-block preservation, singleton `* Chat` materialisation, turn-blocks-under-`* Chat` ordering, recovered prompt body, full session-document-layout validator-style assertion, drawer write-exclusion composition, idempotence, and the "no turn blocks at all" edge case. Built on the existing `gptel-chat-save-test--has-line` helper and the `with-temp-buffer` / `gptel-chat-mode` pattern already in use; no new test infrastructure required.
- Verification: directory-scoped buttercup run goes from 410 → 418 passing (8 new specs, no regressions). The cycle-8 baseline failure load (ERT 10 unexpected + buttercup 79 failed) is for the global suite — within `config/gptel/chat` itself the suite is fully green before and after, so set-based regression detection is trivially satisfied.

## Discoveries

- class: shape-defense-gap
  finding: |
    The orphaned-`* System Prompt` layout (heading present, `* Chat`
    absent, turn blocks below the orphaned subtree) was a documented
    silent-data-loss path in `gptel-chat--write-system-prompt-heading`:
    the body-region helper's SUBTREE-END falls through to `point-max`
    when no next `^\\* ` heading exists, and the writer's
    `delete-region body-start subtree-end` then deletes every turn
    block in the orphaned subtree. No test fixture exercised this layout
    (every fixture pairs `* System Prompt` with `* Chat`), so the suite
    was green while the contract was violable. The fix bounds the
    rewrite at the first turn-block marker and re-materialises `* Chat`
    above the captured turns.
  affected_entries:
    - register/shape/session-document-layout
    - register/invariant/system-prompt-heading-authoritative
  resolution: |
    Fixed in this task. `register/shape/session-document-layout`'s
    validator is now defended on the save path against all four
    structural invariants in the producer code itself, not only via
    fixture coverage. No register edit required — both cited entries
    are `reconciled` and `load_bearing`; the fix strengthens the save
    path's compliance with them rather than changing their contracts.

- class: implicit-coupling
  finding: |
    The turn-block marker regexp `^#\\+begin_\\(user\\|assistant\\)`
    now appears in three places: the chat parser (the heading-
    indifferent locator, per design.md §Decision 12), the
    `register/shape/session-document-layout` validator (lines ~487-488
    in interfaces.org), and the new recovery branch in
    `gptel-chat--write-system-prompt-heading`. The new constant
    `gptel-chat--turn-block-marker-re` names the writer-side
    occurrence, but the three sites are still text-duplicated rather
    than single-sourced.
  affected_entries:
    - register/shape/session-document-layout
  resolution: |
    Not addressed in this task — out of scope (the task body is
    "guard the save path", not "single-source the parser regexp").
    Naming the constant and citing the parser contract in the
    docstring is the minimum that keeps the three sites discoverable
    via grep. Promoting the regexp to a shared constant in the parser
    module (chat/parser.org) would be a sensible follow-up if a
    future change touches all three sites.

## Review

Author-blind review at `.orchestrator/cycles/cycle-1779522837/reviews/harden-system-prompt-save-against-missing-chat-heading.md` (merge_commit `d774f43`): **clean review, 0 findings**. Captured region correctly delimited; idempotence is structural (save #2 takes the canonical branch); nominal-layout byte-equivalence preserved; hard-coded `"* Chat\n"` is the right call (routing through `jf/gptel--session-headings-block` would duplicate `* System Prompt`); turn-block-marker false-positive is the documented `known_limitation` of `register/shape/session-document-layout`, not a regression.
