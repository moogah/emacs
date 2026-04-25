---
name: scope-and-bash-tool-test-failures-followup
description: Investigate and fix 24 pre-existing buttercup failures in gptel/scope (parallel-callback FSM, bash-tool integration, add-to-scope) and one bash-parser corpus case, surfaced by the gptel-chat-state-persistence regression sweep.
status: ready
source: openspec/changes/gptel-chat-state-persistence
relations:
  - "discovered-from:regression-sweep-and-manual-smoke"
---

## Files to modify

Investigation-driven; expected hot spots (not exhaustive — confirm during triage):

- `config/gptel/scope/parallel-tool-callback.{org,el}`
- `config/gptel/scope/bash-tool.{org,el}` and the `run_bash_command` macro/dispatcher
- `config/gptel/scope/expansion-ui.{org,el}` (for "transient collision" failures)
- `config/gptel/scope/test/integration/parallel-tool-callback-spec.el`
- `config/gptel/scope/test/integration/bug4-end-to-end-add-to-scope-spec.el` (or wherever those Bug 4 / multi-violation specs live)
- `config/gptel/scope/test/validation/run-bash-command-*-spec.el`
- `config/bash-parser/test/corpus/runners/...` (integration-002 pipeline-with-pattern-substitution-and-redirect case)

## Implementation steps

1. Confirm scope. Run `./bin/run-tests.sh -d config/gptel/scope` and `./bin/run-tests.sh -d config/bash-parser` and capture exact failing spec names + messages. The 24 failures observed during the regression sweep cluster as:
   - **`run_bash_command` integration** (12 specs): no-op macro execution, denied-command UI, file-op-out-of-scope (read / allow-once / add-to-scope), gptel callback contract.
   - **Parallel tool callback** (5 specs): single-deny callback, two-mapc both-deny, transient collision (clobber + queue), mixed allowed/denied.
   - **Bug 4: end-to-end add-to-scope** (2 specs).
   - **Multi-violation add-to-scope leaks** (2 specs).
   - **`run_bash_command`: Timeout and resource limits** (2 specs): "more specific filters" warning regex; truncation "head" regex.
   - **Bash-parser corpus** (1 spec): `integration-002: Pipeline with pattern substitution and redirect`.
2. Bisect age. Use `git log --oneline --reverse <test-file>` and run the affected suites at a few historical points to pinpoint when each cluster started failing — they likely have different root causes.
3. Triage by cluster. Each cluster (parallel-callback, bash-tool integration, add-to-scope, timeout messages, bash-parser corpus case) is probably an independent fix. Decide whether to split this task per-cluster once root causes are known. If splitting, the children stay in `.tasks/` as siblings (not in an openspec change) until / unless the work is large enough to warrant promotion to a change of its own.
4. Fix and add regressions. Prefer fixing the production code; only adjust tests if the spec was wrong (justify in commit). For "transient collision" / "session stuck" failures, characterize the bug first (a related historical note may exist — search drawer-corruption notes and parallel-callback specs).
5. Re-run `./bin/run-tests.sh` and confirm 0 unexpected failures + 0 aborted + 0 unknown.

## Design rationale

These failures pre-date the gptel-chat-state-persistence change and are out-of-scope for it (the change touched session-creation / metadata-removal code paths, not scope/bash-tool dispatching). The regression sweep surfaced them because the task body asked for a green full-suite run as the verification gate. Tracking the cleanup as a `.tasks/`-level item (rather than inside the gptel-chat-state-persistence change) keeps the parent change's archive path clear while preserving the audit trail via `source:` and `discovered-from:`.

The cluster naming above reflects how Buttercup grouped them in the failure listing on 2026-04-25; once bisected, splitting into per-cluster tasks is appropriate if root causes diverge. If any cluster turns out large enough to need its own design / spec / multi-task plan, promote it to an openspec change at that point and link back here via this file's `name`.

## Verification

- `./bin/run-tests.sh` reports 0 unexpected, 0 aborted, 0 unknown.
- `./bin/run-tests.sh -d config/gptel/scope` and `./bin/run-tests.sh -d config/bash-parser` both green in isolation.
- Each affected spec is exercised by name and passes deterministically over 3 consecutive runs (stability check; some parallel-callback specs are timing-sensitive).

## Context

- Surfaced by: `regression-sweep-and-manual-smoke` in change `gptel-chat-state-persistence`, on 2026-04-25.
- Pre-existing evidence: `test-report.txt` snapshot from 2026-04-24 21:10 already lists the same 24 failures, before any of the gptel-chat-state-persistence merges that landed on 2026-04-24 21:29 onward.
- Related historical notes: `MEMORY.md → reference_drawer_corruption_notes` may overlap with the "transient collision" / "session stuck" failures — check `~/org/roam/20260419111957-gptel_preset_property_corruption.org` before re-deriving.
- Originally created at `openspec/changes/gptel-chat-state-persistence/tasks/open/scope-and-bash-tool-test-failures-followup.md`; relocated to `.tasks/` once we recognised it was external to the parent change's scope.
