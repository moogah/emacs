---
name: fix-bash-violation-resource-routes-as-command
description: Bash `path_out_of_scope` violations surface the whole command string as `:resource`, so `--add-bash-to-scope` rejects it as "bare command name" instead of expanding the offending path. The expansion-callback then reports success, the tool re-validates, finds the same violation, and the spy loop triggers `max-lisp-eval-depth`.
source: migrate-integration-tests-to-drawer-helpers
status: ready
relations:
  discovered-from: migrate-bash-multi-violation-expansion-spec
---

## Bug in one sentence

For a bash command with denied file ops, the expansion UI receives `:resource = "<full command>"` (e.g. `"ls /foo 2>/dev/null"`) instead of the offending path (`/foo`), so the writer's path-shaped-vs-bare-name heuristic in `jf/gptel-scope--add-bash-to-scope` rejects the resource and the violation never clears.

## Evidence

Migrating `config/gptel/scope/test/integration/bash-multi-violation-expansion-spec.el` from YAML helpers to drawer fixtures uncovered the failure mode in console output:

```
Cannot add command 'ls /foo 2>/dev/null' to scope — use path-based expansion instead
Cannot add command 'ls /foo 2>/dev/null' to scope — use path-based expansion instead
Cannot add command 'ls /foo 2>/dev/null' to scope — use path-based expansion instead
Error loading scope config: Lisp nesting exceeds 'max-lisp-eval-depth': 1601
```

Meanwhile `jf/gptel-scope--validate-command-semantics` (called directly in the file's "validate-command-semantics returns only the first denial" characterization test) returns the correct `:resource` value of `/dev/null` for the same input. The drift is somewhere between Stage-3 violation detection and the call to `jf/gptel-scope-prompt-expansion` — likely in `scope-shell-tools` where `violation-info` is constructed from the validation result.

The related test `Bug 4: end-to-end add-to-scope should enable retry success gptel callback receives success after add-to-scope correctly expands scope` (`config/gptel/scope/test/integration/bash-add-to-scope-bug-spec.el:~325`) documents three chained bugs in its own comments and is currently the single named buttercup failure in the harness summary — almost certainly the same root cause.

## Files likely involved

- `config/gptel/scope/scope-shell-tools.el` — constructs `violation-info` for `jf/gptel-scope-prompt-expansion`. Grep for `:validation-type 'bash` and check the `:resource` value.
- `config/gptel/scope/scope-expansion.el:593,618` — current `--add-path-to-scope` and `--add-bash-to-scope` signatures (3-arg, no buffer).
- `config/gptel/scope/scope-validation.el` — `jf/gptel-scope--validate-command-semantics` returns the canonical violation plist with the correct `:resource`.

## Implementation hint

Probably folds into the existing `openspec/changes/scope-rearch-followups/` change as Bug 4 (the plan already calls this out: see Workstream B in `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md`). Promote this `.tasks/` item to that openspec change when WS-A is fully landed and WS-B is ready to pick up.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/integration 2>&1 | grep -E "multi-violation|Bug 4" | head
```

After the fix, the two `Bug: multi-violation` specs should pass with the offending path(s) appearing in the appropriate drawer key, and the `Bug 4` end-to-end test should also pass (or surface a distinct sub-bug).

## Context

- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream B, Bug 4 fold-in
- Sibling pre-existing failure: `config/gptel/scope/test/integration/bash-add-to-scope-bug-spec.el:~325`
