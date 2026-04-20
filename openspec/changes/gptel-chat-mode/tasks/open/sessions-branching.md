---
name: sessions-branching
description: Branch-point selection and truncation on chat-mode turn list
change: gptel-chat-mode
status: blocked
relations:
  - blocked-by:parser
  - blocked-by:sessions-auto-init
---

## Files to modify
- `config/gptel/sessions/branching.org` (modify)
- `config/gptel/sessions/branching.el` (tangled)
- `config/gptel/sessions/test/branching/branch-point-selection-spec.el` (new
  or rewrite)
- `config/gptel/sessions/test/branching/context-truncation-spec.el` (new or
  rewrite)
- `config/gptel/sessions/test/branching/branching-integration-spec.el` (new
  or rewrite)

## Implementation steps
1. Rewrite branch-point selection:
   - Call `gptel-chat--parse-buffer` on the source session buffer to get
     the turn list.
   - Filter for **outer** `#+begin_user` blocks only. Assistant blocks,
     tool blocks, and non-block content are NOT valid branch points.
   - Present an interactive numbered list showing the first line of each
     user block as the label.
   - Ask the user to include or exclude the selected turn.
   - Compute the branch-point position:
     - **Include** → position immediately after the `#+end_user` line
       that closes the selected user turn (new branch ends with the
       selected user turn complete, awaiting an assistant response).
     - **Exclude** → position immediately before the `#+begin_user`
       line that opens the selected user turn (new branch ends before
       the selected user turn; the user can author a different prompt).
2. Rewrite context truncation:
   - Copy buffer content from `point-min` up to the branch-point position,
     verbatim. No filtering, rewriting, or normalization — chat-mode block
     structure in the source is already the canonical form.
   - Write to the new branch's `session.org`.
   - Do NOT invoke any `gptel--bounds` filtering. Chat-mode has no such
     property — this is the main simplification from the old path.
3. Handle edge cases:
   - No valid branch points (empty conversation or only assistant
     blocks) → report to user, do not allow branch creation.
   - First user turn with EXCLUDE → new branch contains only whatever
     was before the first `#+begin_user` (typically nothing, or just
     commentary).
4. Everything else is **unchanged**: registry updates, `metadata.yml`
   writing, `branch-metadata.yml` writing, `current` symlink update, new-
   branch directory creation.
5. Tests (scenarios from sessions-branching delta spec):
   - Interactive turn selection presents a numbered list of user blocks.
   - Include → branch point is after `#+end_user` of selected turn.
   - Exclude → branch point is before `#+begin_user` of selected turn.
   - Assistant blocks, tool blocks, headings, prose do NOT appear in the
     selection list.
   - No valid branch points → reports "no available branch points", no
     branch created.
   - Copying content up to a sample position (e.g. 5420) writes exactly
     bytes 1–5419 to `session.org`; result is parseable chat-mode.
   - Empty branch from first-turn exclude is valid chat-mode (may be
     empty of turns, may contain commentary).
   - Branch preserves org commentary verbatim.

## Design rationale
The old branching path scanned for "user prompts" as regions *without*
the `gptel` text property — a `gptel-mode` convention. Chat-mode has no
such property; the structural `#+begin_user` block is the authoritative
user-turn marker (sessions-branching delta spec). This simplifies the
implementation: one parser call replaces text-property walking and bounds
filtering.

Bounds filtering is removed **entirely** — chat-mode has no
`gptel--bounds` text properties. Block-delimiter integrity is guaranteed
by construction: the branch point is always on a line boundary outside
any open block. No half-open blocks can appear at the truncation point
because the branch point is defined in terms of complete block
boundaries.

## Design pattern
Parser-driven: parse the source buffer once, derive both the selection
list and the branch-point position from the same turn data. This makes
the include/exclude semantics unambiguous: include = end-of-user-block
marker position; exclude = start-of-user-block marker position.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/branching.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/branching` passes
  all three suites.
- `grep -n "gptel--bounds" config/gptel/sessions/branching.org` returns
  nothing (bounds filtering fully removed).
- Manual smoke: create a session with several turns, invoke
  `jf/gptel-branch-session`, choose a mid-conversation user turn with
  include, confirm the new branch's `session.org` is well-formed and
  contains everything through that user turn's `#+end_user`.

## Discovered during review of sessions-filesystem (2026-04-20)
`config/gptel/sessions/branching.el:286` (and the corresponding `.org`)
still references `session.md` in a docstring or comment after the
filesystem rename. Update that reference to `session.org` as part of
this task. (Sessions-filesystem review Finding #9.)

## Context
- design.md §Decision 18 (session file format — `.org` everywhere)
- specs/gptel/sessions-branching.md §"Branch point selection" (MODIFIED)
- specs/gptel/sessions-branching.md §"Context truncation" (MODIFIED)
- specs/gptel/sessions-branching.md §REMOVED — `gptel--bounds` filtering,
  text-property detection
- architecture.md §`sessions/branching` (modified)
- Review of sessions-filesystem (orchestrator session 2026-04-20) Finding #9
