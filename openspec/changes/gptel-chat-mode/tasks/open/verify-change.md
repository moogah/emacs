---
name: verify-change
description: Full test sweep, manual smoke test, spec scenario validation
change: gptel-chat-mode
status: blocked
relations:
  - blocked-by:parser
  - blocked-by:messages
  - blocked-by:sanitize-chunks
  - blocked-by:mode-definition
  - blocked-by:stream-callback-tool-element-shape-and-tests
  - blocked-by:stream-callback-multi-round-t-signal
  - blocked-by:send-command
  - blocked-by:nav-commands
  - blocked-by:display-layer
  - blocked-by:preset-wiring
  - blocked-by:menu-integration
  - blocked-by:sessions-filesystem
  - blocked-by:sessions-persistent-create
  - blocked-by:auto-init-metadata-preset-precedence
  - blocked-by:auto-init-agent-path-handling
  - blocked-by:sessions-branching
  - blocked-by:sessions-activities
---

## Files to modify
None (verification-only). This task produces artefacts (test reports,
smoke-test log) but makes no source-code changes unless regressions are
found — in which case the regression-fixing work is filed as
`discovered-from:verify-change` tasks.

## Implementation steps
1. **Full test suite**: `make test` (runs both Buttercup and ERT). Must
   pass end to end. Capture output; if any test fails, triage:
   - Regression in touched code → file a discovered-from task.
   - Incidental pre-existing failure → note and ignore; pre-existing
     failures are out of scope for this change.
2. **Chat-mode subsystem scoped run**: `./bin/run-tests.sh -d
   config/gptel/chat`. Must pass independently (sanity check for
   localisation of regressions).
3. **Sessions subsystem scoped run**: `./bin/run-tests.sh -d
   config/gptel/sessions`. Must pass; confirms the four modified session
   modules integrate correctly.
4. **Spec scenario checklist**. Walk every scenario in these three spec
   files and confirm at least one passing test covers it:
   - `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
   - `openspec/changes/gptel-chat-mode/specs/gptel/sessions-persistence.md`
   - `openspec/changes/gptel-chat-mode/specs/gptel/sessions-branching.md`
   Mapping table lives in `architecture.md` §"Scenario Mapping" — use it
   as the primary reference.
5. **Manual smoke test** (the "hour-of-use" goal in design.md §Goals):
   - `M-x gptel-chat-new` → empty user block; type a prompt; `C-c C-c`.
     Confirm response streams into `#+begin_assistant` block; a new empty
     user block appears after completion; point lands inside it.
   - Create a persistent session via `M-x jf/gptel-persistent-session`;
     confirm `session.org` opens in chat-mode; send a few turns; quit
     and reopen — registry picks it up, preset applied.
   - Branch the session via `M-x jf/gptel-branch-session`; choose a
     user turn (include) and again (exclude); confirm both new branches
     are well-formed `session.org` files readable by chat-mode.
   - Resume via an activity hook (if activities are configured); confirm
     chat-mode activates and session vars are set.
   - Trigger a tool call through an assistant response; confirm a nested
     `#+begin_tool`/`#+end_tool` block appears inside the assistant
     block.
   - Abort a streaming response (`M-x gptel-abort` or `C-c C-k`); confirm
     the assistant block closes cleanly with an interruption marker and
     `#+end_assistant` on its own line.
6. **Legacy check**: pick a pre-existing `session.md` file (if one
   exists) and confirm:
   - It is NOT auto-recognized by the new session commands.
   - It opens cleanly in upstream `gptel-mode` (`M-x gptel-mode` on
     the buffer) — nothing about upstream `gptel-mode` should have
     changed.
7. **Dead-code sweep**: `grep` for references to removed code paths:
   ```
   grep -rn "gptel--save-state\|gptel--restore-state" config/gptel/sessions/
   grep -rn "session\.md" config/gptel/sessions/
   grep -rn "(gptel-mode 1)" config/gptel/sessions/
   ```
   All should return zero results in live code (only archival comments
   or none at all).
8. Record results in a brief summary (commit message or PR body). If
   any scenario is uncovered, file a discovered-from task rather than
   marking this verification complete.

## Design rationale
Decision 19 (clean break) means legacy `session.md` files remain on disk
but are invisible to the new workflow. The legacy check in step 6 is the
empirical confirmation of this — if the new session commands accidentally
recognize `session.md` files, Decision 19 has been violated.

The hour-of-use manual smoke test is design.md's explicit v1 completeness
bar: "a human can use it for an hour (including creating sessions,
branching, and resuming via activities) without hitting a structural
bug." Automated tests cover component behaviour; the smoke test covers
integration across components and buffers.

## Design pattern
Verification is **read-only by default**. Any source-code changes found
necessary during verification are new tasks (with `discovered-from:
verify-change` relation), not in-line fixes. This keeps the verification
task's scope bounded and traceable.

## Verification
- `make test` exits 0.
- `./bin/run-tests.sh -d config/gptel/chat` exits 0.
- `./bin/run-tests.sh -d config/gptel/sessions` exits 0.
- Spec scenario checklist — every scenario has a passing test (mapping
  per architecture.md §"Scenario Mapping").
- Manual smoke-test checklist above — every step succeeds.
- Dead-code sweep (`grep`) confirms legacy references are gone.

## Context
- design.md §Goals — hour-of-use completeness bar
- design.md §Migration Plan — rollback semantics, clean-break intent
- architecture.md §"Scenario Mapping" — spec→test coverage table
- CLAUDE.md §Testing Infrastructure — test commands and organisation
