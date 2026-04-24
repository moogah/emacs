---
name: sessions-persistence-spec-tighten-auto-init
description: Tighten sessions-persistence.md auto-init MODIFIED requirement to lock in mode-before-preset ordering and split agent-path detection into nested + flat scenarios
change: gptel-chat-mode
status: done
relations:
  - discovered-from:auto-init-metadata-preset-precedence
  - discovered-from:auto-init-agent-path-handling
---

## Files to modify
- `openspec/changes/gptel-chat-mode/specs/gptel/sessions-persistence.md`
  (both the MODIFIED Auto-initialization requirement block and its
  Scenarios — lines ~55-84)

## Implementation steps

This task groups two spec-level findings from the Batch D reviews —
both land on the same "Auto-initialization enables `gptel-chat-mode`"
MODIFIED requirement block and adjacent scenarios.

1. **(Review: `auto-init-metadata-preset-precedence`) Scenario step
   ordering is a trap.** The MODIFIED requirement's numbered steps
   currently order step 5 (apply preset from metadata.yml) before
   step 6 (ensure `gptel-chat-mode`). The implementation is the
   opposite — mode first so `kill-all-local-variables` inside mode
   activation doesn't wipe the setq-local session vars, and so the
   chat-mode hook's `:GPTEL_PRESET:` drawer re-apply runs first,
   letting the metadata.yml apply run last and win. Fix:
   - Renumber steps so "Ensure the major mode is `gptel-chat-mode`"
     precedes "Apply the preset named in `metadata.yml`", or add an
     explicit note at the end of the numbered list: "Ordering is
     load-bearing. `gptel-chat-mode` activation runs first so that
     any `:GPTEL_PRESET:` drawer is re-applied by the chat-mode
     hook before the authoritative `metadata.yml` preset is applied
     last; otherwise the hook silently clobbers the metadata
     values."
   - Cross-reference design.md §Decision 16 point 2.

2. **(Review: `auto-init-agent-path-handling`) Agent-path scenario
   under-specifies shape.** The existing "Scenario: Agent file
   detection" (lines 80-84) treats all `*/agents/<agent-name>/...`
   paths identically and says "sets branch-name to `"main"`". The
   implementation now distinguishes two layouts:
   - **Nested**:
     `*/<session-id>/branches/<branch>/agents/<agent>/session.org`
     — session-id and branch-name captured from the path.
   - **Flat (legacy)**:
     `*/<session-id>/agents/<agent>/session.org` — session-id from
     the path; branch-name defaults to `"main"`; the
     `jf/gptel--update-current-symlink` side-effect is suppressed
     because there is no `branches/` directory.

   Fix: split the single Scenario into two scenarios with the two
   shapes, and add a third scenario asserting
   `jf/gptel--parent-session-id` is populated from `metadata.yml`
   `parent_session_id` when present (nil when absent, per
   `defvar-local` default).

3. **Stale file extension.** Lines 80-84 still mention `session.md`
   in the flat-layout path; the code matches `session.org`. Sweep
   the whole file for any remaining `session.md` references that
   aren't inside the "Legacy `session.md` branches" removal/
   invisibility clauses.

## Design rationale

Both findings were non-blocking against the shipped code but load-
bearing against the spec: a future reader who implements to the
current spec letter would either reintroduce the preset-clobbering
bug (step-ordering trap) or collapse the nested/flat split back into
one regex (agent-path scenario). Grouping them is correct — they
sit in the same MODIFIED block and in the scenario cluster
immediately below it, and the diff will read as one coherent patch.

## Verification

- `grep -n "session.md" openspec/changes/gptel-chat-mode/specs/gptel/sessions-persistence.md`
  — remaining hits only inside the "Legacy `session.md`" invisibility
  block (expected) and the REMOVED section (expected); no hits in the
  auto-init / agent-path scenarios.
- `grep -n "branches/<branch>/agents" openspec/changes/gptel-chat-mode/specs/gptel/sessions-persistence.md`
  has at least one hit inside the new nested-agent scenario.
- `grep -n "parent_session_id" openspec/changes/gptel-chat-mode/specs/gptel/sessions-persistence.md`
  has at least one hit inside the new parent-session-id scenario.
- `grep -nE "Ensure the major mode|mode activation runs first"
  openspec/changes/gptel-chat-mode/specs/gptel/sessions-persistence.md`
  shows the ordering-is-load-bearing note.

No code or test changes. Spec-only task.

## Context
- Review of `auto-init-metadata-preset-precedence` (orch session
  `orch-1776779279`, reviewer agent `aac6c9b7311b25e15`, 2026-04-21),
  Spec-level finding on steps 1-7 ordering.
- Review of `auto-init-agent-path-handling` (orch session
  `orch-1776779279`, reviewer agent `a0d72897d76051461`, 2026-04-21),
  Spec-level finding on agent-path shape + stale `session.md` drift.
- design.md §Decision 16 point 2 (metadata.yml is authoritative).
- design.md §Decision 17 step 2 (parent-session-id buffer-local).

## Review (2026-04-21, orch-review session)

- Reviewer agent `a73b5b7f85cfad109`. Verdict: CLEAN.
- Findings: none above the bar.
- Steps renumbered so mode-ensure precedes preset-apply AND an
  explicit "Ordering is load-bearing" note spells out the failure
  mode (`kill-all-local-variables` wipe + chat-mode-hook clobber).
  Cross-refs to design.md §Decision 16 point 2 / §Decision 17 step 2
  present.
- Nested and flat agent scenarios are genuinely distinct: nested
  captures session-id + branch, flat asserts branch defaults to
  `"main"` AND names the `jf/gptel--update-current-symlink`
  suppression. parent-session-id scenario covers present-populated
  and absent-nil cases.
- Remaining `session.md` hits are all inside the legacy-invisibility
  scenario or the transitional "replaces the previous" sentence, per
  the Verification clause.
- Spec-level nit ("five buffer-local session variables" — code sets
  4 core + 1 conditional): predates this task, parenthetical
  clarifies, below the bar. Not worth a follow-up.
- No follow-up tasks. Flipped to `done`.
