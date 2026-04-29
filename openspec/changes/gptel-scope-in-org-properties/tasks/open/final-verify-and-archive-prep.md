---
name: final-verify-and-archive-prep
description: Run full test suite, manual smoke tests, and opsx-verify; produce an archive-ready change
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:migrate-validation-tests
  - blocked-by:migrate-expansion-tests
  - blocked-by:migrate-session-creation-tests
  - blocked-by:migrate-persistent-agent-tests
  - blocked-by:add-drawer-corruption-regression
  - blocked-by:delete-yaml-and-security-residue
---

## Cites register entries

This task is the omnibus verification before integrate. It cites every register entry the cycle touched so the integrate-phase reconciliation can enumerate dispositions:

- Shape: `scope-config-plist`, `drawer-text-block`, `violation-info`
- Vocabulary: `drawer-key-set`, `operation-to-drawer-key`
- Boundary: `scope-config-loader`, `scope-profile-applicator`, `scope-pattern-writer`
- Invariant: `scope-parse-complete-is-true`, `scope-coverage-threshold-is-1`, `scope-no-security-key-in-plist`, `scope-drawer-no-duplication`, `scope-add-pattern-idempotent`

Verify by running the full suite (`./bin/run-tests.sh -d config/gptel/scope`) and grepping for any remaining `error "speculated; not implemented"` strings — every scaffolded stub must be either satisfied (test passes) or dispositioned (promoted / archived / rejected).

## Files to modify
- (no source code changes — verification and documentation only)
- Possibly snapshot files: `config/gptel/scope/test-results.txt`, `config/gptel/scope-profiles/test-results.txt` if those exist and are git-tracked.

## Implementation steps

1. Run the full test suite:

   ```bash
   ./bin/run-tests.sh
   make test
   ```

   All green. Capture the report:

   ```bash
   ./bin/run-tests.sh --report > /tmp/scope-drawer-final-report.txt
   ```

2. Refresh snapshots if any were tracked:

   ```bash
   ./bin/run-tests.sh -d config/gptel/scope --snapshot
   ./bin/run-tests.sh -d config/gptel/scope-profiles --snapshot
   ```

3. Manual smoke tests in `./bin/emacs-isolated.sh`:

   - **Create a fresh persistent session.** `M-x jf/gptel-persistent-session "smoke-drawer"`. Confirm the resulting `~/.gptel/sessions/smoke-drawer-*/branches/main/session.org` carries a `:PROPERTIES:` drawer with `:GPTEL_PRESET:` and the resolved `:GPTEL_SCOPE_*` keys. Confirm no `scope.yml` exists in the branch directory.

   - **Trigger a scope violation and add-to-scope.** Run a tool call that lands outside the initial scope. The expansion UI fires. Choose "Add to Scope". Confirm the chat buffer's drawer was updated, the buffer is now saved, and the next tool call against the same resource succeeds. Press `C-_` (undo) to confirm the drawer mutation is in the undo ring.

   - **Reopen the file.** Close the buffer and `find-file` the same `session.org`. Confirm the drawer round-trips and the validator sees the same scope.

   - **Create a sub-agent.** Invoke PersistentAgent from the chat buffer with `allowed_paths`. Confirm the agent's `session.org` carries the right drawer keys and the agent dir contains no `scope.yml`.

   - **Edit Manually.** From the expansion UI, choose "Edit Manually". Confirm focus shifts to the chat buffer's `session.org` with the drawer unfolded.

4. Run `openspec verify gptel-scope-in-org-properties` to validate spec scenarios are covered:

   ```bash
   openspec verify gptel-scope-in-org-properties
   ```

   If any scenarios are flagged as uncovered, file follow-up tasks rather than blocking the archive. (Per project convention, "every scenario has at least one test" is the goal; gaps go in `.tasks/` if cross-cutting or in `tasks/open/` if the change owns them.)

5. Update the repo's `CLAUDE.md` if the test directory layout reference (`config/gptel/scope/test/yaml/`) needs to be updated to reflect the deleted directory and the new `drawer/` directory. The existing mention is around `CLAUDE.md` § "Scope validation test organization".

6. Tag the change as ready to archive: confirm `tasks/open/` is empty (all tasks closed), `tasks/closed/` has 16 entries.

7. Run `openspec status --change gptel-scope-in-org-properties` and confirm `isComplete: true`.

8. Commit a final integration commit if changes accumulated across the change's lifecycle haven't already been committed task-by-task.

9. Run `/opsx-archive gptel-scope-in-org-properties` (or note that the change is archive-ready).

## Design rationale

The final-verify task is the explicit gate between "implementation done" and "archived". It catches integration gaps that per-task verification can't see (cross-cutting hook ordering, byte-compile warnings, real-world session lifecycle bugs). Manual smoke tests cover the user-visible surfaces — drawer round-trip, undo affordance, expansion UI, agent creation — that automated tests fixture in isolation.

The CLAUDE.md update keeps onboarding documentation aligned with the codebase. The `openspec verify` step is the formal scenario-coverage check.

## Design pattern

Treat this task as a checklist, not a coding task. Each step is a verification gate; if any step fails, file a fix task (or fold the fix into an existing task and reopen it) rather than band-aiding here.

## Verification

- `./bin/run-tests.sh` passes with no failures.
- All five manual smoke tests succeed (drawer creation, add-to-scope round-trip, reopen, agent creation, edit-manually).
- `openspec verify gptel-scope-in-org-properties` reports no spec-scenario coverage gaps (or any gaps are documented as follow-up tasks).
- `openspec status --change gptel-scope-in-org-properties` reports `isComplete: true`.
- `tasks/open/` is empty (all tasks closed).
- CLAUDE.md test-directory-layout reference matches reality (no `yaml/`; mentions `drawer/`).

## Context

design.md § Migration Plan step 11 (manual smoke)
architecture.md § Testing Approach (Running Tests)
proposal.md § Impact (User impact, Code removed)
