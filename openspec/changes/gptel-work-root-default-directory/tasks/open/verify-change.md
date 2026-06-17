---
name: verify-change
description: Tangle+validate both .org files, full affected-area test run, walk spec scenarios
change: gptel-work-root-default-directory
status: blocked
relations:
  - blocked-by:session-write-work-root
  - blocked-by:binder-default-directory
  - blocked-by:agent-build-scope-plist-split
  - blocked-by:scope-relative-resolution-tests
  - blocked-by:agent-workroot-and-paths
  - blocked-by:docs-allowed-paths-rename
---

## Files to modify
- None (verification only). May add a small end-to-end spec if a gap is found.

## Implementation steps
1. Tangle + validate both touched literate files:
   ```bash
   ./bin/tangle-org.sh config/gptel/sessions/commands.org
   ./bin/tangle-org.sh config/gptel/tools/persistent-agent.org
   ```
2. Run the affected suites and confirm all green:
   ```bash
   ./bin/run-tests.sh -d config/gptel/sessions
   ./bin/run-tests.sh -d config/gptel/tools
   ./bin/run-tests.sh -d config/gptel/scope
   ```
   (Or `make test-report DIR=config/gptel` for a concise count.)
3. End-to-end walk of the four exploration flow scenarios, confirming each spec
   scenario is exercised by a test or a manual check:
   - **Created chat session**: drawer carries `GPTEL_WORK_ROOT`; on open,
     `default-directory` == project root; a relative-path tool call resolves into scope.
   - **Keyless session**: no key ⇒ `default-directory` == `branch-dir` (legacy behavior).
   - **Parent calls a tool**: relative path judged against the parent's work root
     (`with-current-buffer info:buffer` context).
   - **Parent spawns an agent that calls tools**: agent drawer carries its own
     `GPTEL_WORK_ROOT` (passed or parent-default) + `read_paths`/`write_paths` (+`/tmp`);
     agent's relative paths resolve into the agent's work root and are validated against
     the agent's scope.
4. Confirm the cwd↔scope agreement invariant holds: for a freshly created session, the
   `GPTEL_WORK_ROOT` value equals the root that expanded `GPTEL_SCOPE_*`.
5. Confirm no stale `allowed_paths` references remain (re-run the grep from
   `docs-allowed-paths-rename`).
6. Run `openspec validate "gptel-work-root-default-directory"`.

## Design rationale
Final integration gate. The change spans two literate files and three capabilities; a
single pass that tangles, runs every affected suite, and walks the spec scenarios end to
end is the acceptance criterion before archive. The fail-closed absolute-path baseline is
already green and verified, so this focuses on the new relative-path/work-root behavior.

## Design pattern
Pure verification task — the last task in the dependency chain. No production edits unless
a scenario reveals a gap (then file a `discovered-from:verify-change` task).

## Verification
- All three suites green; both files tangle clean.
- Every spec scenario across the three delta specs maps to a passing test or a recorded
  manual check.
- `openspec validate` reports the change valid.

## Context
design.md § Testing approach; § Goals / Non-Goals
specs/sessions-persistence/spec.md, specs/scope/spec.md, specs/persistent-agent/spec.md
