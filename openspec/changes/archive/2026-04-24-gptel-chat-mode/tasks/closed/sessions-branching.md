---
name: sessions-branching
description: Branch-point selection and truncation on chat-mode turn list
change: gptel-chat-mode
status: done
relations:
  - blocked-by:parser
  - blocked-by:auto-init-metadata-preset-precedence
  - blocked-by:auto-init-agent-path-handling
---

## Review (2026-04-21, orch session `orch-review-1776796835`)

Reviewer agent (`a30f5c8f9dad2e358`) read the full `branching.{org,
el}`, all three new spec files (`branch-point-selection-spec.el`,
`context-truncation-spec.el`, `branching-integration-spec.el`),
traced the parser's `:start`/`:end` contract
(`config/gptel/chat/parser.el:280-403`) against
`jf/gptel--branching-turn-branch-point`'s arithmetic, and hand-
verified the byte math on the "realistic branch point" sample
(`#+begin_user\nQ1\n#+end_user\n` → INCLUDE position 28, EXCLUDE
on turn 1 → position 1). Confirmed the integration tests use a
real temp `jf/gptel-sessions-directory`, invoke
`jf/gptel--create-branch-session`, re-parse the resulting
`session.org`, and verify the `current` symlink target via
`file-truename`. Grepped for leftover `gptel--bounds`,
`find-user-prompts`, and `session.md`: all gone from production
code; old bounds machinery fully retired;
`(require 'gptel-chat-parser)` replaces `(require 'gptel)`; the
`gptel-mode` guard is replaced with
`(derived-mode-p 'gptel-chat-mode)`.

**Findings:**

1. **`branching.el:209-243` — live-buffer positions used against
   on-disk file.** `jf/gptel-branch-session` derives
   `branch-position` from the live source buffer via
   `jf/gptel--branching-select-branch-point`, then passes it to
   `jf/gptel--copy-truncated-context`, which reads
   `parent-context` from disk via `insert-file-contents`. If the
   user has unsaved edits, positions computed against the live
   buffer diverge silently from the disk file's byte layout.
   Pre-existing in the old bounds implementation — not a
   regression — but the rewrite was a natural opportunity to fix
   it. → **Follow-up:**
   `openspec/changes/gptel-chat-mode/tasks/open/branching-dirty-
   buffer-handling.md`.

2. **`branching.el:46-65` — EOF INCLUDE edge case.** When the
   selected `#+end_user` is the buffer's last line with no
   trailing newline, `forward-line 1` lands at `point-max`
   instead of "after the closing newline"; the whole file is
   copied. Behaviour is correct (INCLUDE semantics preserved at
   EOF) but neither documented nor tested. → Grouped into the
   test-hardening follow-up below.

3. **`context-truncation-spec.el:159-177` — redundant
   "no filter/rewrite/normalize" test.** Given the new
   `jf/gptel--copy-truncated-context` is a verbatim byte copy
   by construction, this assertion is logically equivalent to
   the earlier "writes bytes 1..(POS-1) verbatim" assertion and
   adds no independent coverage. Reviewer flagged as low-value
   rather than broken. Judgement call; per the signal/noise
   bar, not raised as a follow-up.

4. **`branching-integration-spec.el` (all describes) — no
   registry-update assertion.** Integration tests verify
   `current` symlink, `branch-metadata.yml`, and `session.org`
   parseability, but do not assert that the session registry
   was updated for the new branch. Inspection shows
   `jf/gptel--create-branch-session` does not register
   directly — registry entry is populated lazily by the
   auto-init hook when `find-file` opens the new branch. The
   test omission is technically correct, but this asymmetry
   ("registry update is a side effect of open, not of create")
   is worth a test or a prominent comment. → Grouped into the
   test-hardening follow-up below.

Ruled out:
- **Decision 18 alignment** — `session.md` appears only in test
  fixtures/comments; production branching writes `session.org`
  via `jf/gptel--context-file-path`. Verified.
- **Dead code** — old `jf/gptel--find-user-prompts`,
  `jf/gptel--filter-bounds-before-position`,
  `jf/gptel--validate-bounds` are all gone.
- **Arithmetic correctness** — INCLUDE and EXCLUDE positions
  match the parser's `:start`/`:end` contract on the verified
  sample. Spec scenarios matched.
- **Tool-block / mid-block branch points** — parser only
  surfaces outer user turns; `seq-filter` receives no inner
  blocks to misclassify. No additional tool-block test needed.

**Spec-level signals:** none. The delta spec
(`specs/gptel/sessions-branching.md`) composed cleanly with the
parser's turn shape; include/exclude semantics map onto
`:start`/`:end` markers with a single `forward-line 1` on one
side, zero on the other. No friction that signals a spec
problem.

**Follow-ups (both `ready`; grouped per skill guidance to avoid
finding-per-task fragmentation):**

- `tasks/open/branching-dirty-buffer-handling.md` — Finding #1
  (behavioural; picks a policy for unsaved edits and pins it
  with a regression spec).
- `tasks/open/branching-edge-case-test-hardening.md` — Findings
  #2 and #4 (both are small under-tested edges in the same
  test suite).

Neither follow-up blocks any currently-ready open task in this
change (`menu-integration` does not depend on branching;
`verify-change` already `blocked-by: sessions-branching`
directly, and its verification sweep naturally covers both
follow-ups when they land). No downstream `blocked-by:`
repoint required.

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
