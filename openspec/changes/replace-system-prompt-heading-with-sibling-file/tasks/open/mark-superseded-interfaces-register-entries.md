---
name: mark-superseded-interfaces-register-entries
description: Mark `register/shape/session-document-layout` and `register/invariant/system-prompt-heading-authoritative` as superseded in `interfaces.org`, with pointers to the new `:GPTEL_SYSTEM_PROMPT_FILE:` contract. Update or remove cross-references in other entries.
change: replace-system-prompt-heading-with-sibling-file
status: ready
relations: []
---

## Files to modify

- `interfaces.org` (modify) — supersede two register entries; reconcile cross-references at the lines listed in Implementation steps

## Why

The prior change confirmed two interfaces register entries as load-bearing under the `* System Prompt` heading shape:

- `register/shape/session-document-layout` (entry_id at `interfaces.org:336`) — described the four structural invariants (config drawer at `point-min`, one `* System Prompt` heading, one `* Chat` heading, no turn block before `* Chat`).
- `register/invariant/system-prompt-heading-authoritative` (entry_id at `interfaces.org:2508`) — described the three-tier precedence with the heading body at the top.

This change makes both obsolete:

- The shape collapses to "config drawer at `point-min`, then turn blocks." No headings. The four-invariant validator no longer applies.
- The precedence collapses to "sibling file > legacy `:GPTEL_SYSTEM:` drawer entry > preset `:system`." The heading-body tier is removed.

Marking them superseded (rather than deleting them) preserves the historical record and lets future readers trace the evolution.

## Implementation steps

1. **`register/shape/session-document-layout` (line 336)**:
   - Add a `superseded_by:` field pointing at `register/shape/session-sibling-system-prompt-file` (which this task creates) or, if we prefer not to spawn a new register entry, a textual note `superseded_by: openspec/changes/replace-system-prompt-heading-with-sibling-file/specs/gptel/sessions-persistence.md`.
   - Flip `status:` to `superseded`.
   - Add a one-paragraph `supersession_note:` summarizing the change: the canonical session shape no longer carries `* System Prompt` or `* Chat` headings; the system prompt lives in a sibling file referenced by `:GPTEL_SYSTEM_PROMPT_FILE:` in the configuration drawer.
2. **`register/invariant/system-prompt-heading-authoritative` (line 2508)**:
   - Add `superseded_by:` and a `supersession_note:` describing the new precedence: sibling file > legacy `:GPTEL_SYSTEM:` drawer entry > preset `:system`. The heading-body tier no longer exists.
   - Flip `status:` to `superseded`.
3. **Optional but recommended**: add a new register entry `register/invariant/system-prompt-sibling-file-authoritative` capturing the new precedence and pre-send refresh contract. (Defer to follow-up if the new spec text in `openspec/specs/gptel/chat-mode.md` is judged sufficient; the bar for a new register entry is "load-bearing invariant referenced from multiple specs.") A new shape entry `register/shape/session-sibling-system-prompt-file` is similarly optional. Default: do not add new entries in this task; let `/opsx-sync` and the next round of cycles spawn them if needed.
4. **Cross-reference cleanup**. Update or remove references to the superseded entries at these lines in `interfaces.org`:
   - line 404: appears in commentary cross-reference
   - line 453: appears in commentary cross-reference
   - line 522: appears in a `composes_with:` or `depends_on:` list — drop or update
   - line 1795: appears in another entry's relation list — drop or update
   - line 2155: same — drop or update
   - line 2464: appears in commentary cross-reference
   - line 2614: appears in a `composes_with:` or `related:` list — drop or update
5. Validate `interfaces.org` parses cleanly (re-tangle if it has tangling rules; otherwise visual check).

## Verification

```bash
./bin/tangle-org.sh interfaces.org 2>/dev/null || true   # may or may not have tangling
grep -n 'register/shape/session-document-layout\|register/invariant/system-prompt-heading-authoritative' interfaces.org
grep -n 'status: superseded\|superseded_by:' interfaces.org
```

Expect: both register entries show `status: superseded` with a `superseded_by:` pointer. The cross-references at the listed lines either point at the new contract or are removed. No stale `composes_with:` / `depends_on:` lists reference the superseded entries.

## Context

architecture.md §Boundaries — the interfaces register is the bookkeeping surface for cross-spec invariants; the rear-guard cleanup here is what lets `/opsx-archive` close cleanly without dangling load-bearing references.

design.md §Decision 7 — wholesale heading deletion. Marking the register entries superseded is the formal closure of the heading-shape contract.

The prior change's discoveries (`disc-make-system-prompt-heading-authoritative-2`, `disc-make-system-prompt-heading-authoritative-3`) flipped these entries from `speculated` to `confirmed`. This task flips `confirmed` to `superseded`.
