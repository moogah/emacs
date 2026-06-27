---
name: scrub-denied-paths-references
description: Remove denied_paths references from in-tree agent prompts and any remaining docs; delete the stale legacy persistent-agent spec file
change: persistent-agent-rebuild
status: done
relations:
  - blocked-by:rebuild-persistent-agent-module
---

## Files to modify

- Anywhere `denied_paths` (or `denied-paths` as a kebab-case variant) appears outside the persistent-agent module rewrite scope. Likely candidates:
  - `config/gptel/presets/*.md` (preset definition files — instructions to the LLM may tell it to set `denied_paths` when calling `PersistentAgent`). Note: the legacy directory `config/gptel/agents/` was consolidated into `presets/` in commit `f9ca5be` (Jan 25); the `presets/*.md` files are the in-tree agent/preset prompts now.
  - `config/gptel/tools/persistent-agent.org` itself (any leftover references in docstrings or commentary that the rewrite missed)
  - `config/gptel/tools/README.org` (if present and discusses tool args)
  - Top-level docs (CLAUDE.md, README.md) — verify with grep
- `config/gptel/test/persistent-agent-spec.el` — DELETE. This legacy Buttercup spec asserts on removed symbols (`*gptel-agent:` buffer naming, `jf/gptel--auto-save-session-buffer` hook, `jf/gptel-persistent-agent--create-overlay`); produces 24 failures after the rebuild. The replacement test suite lives in `config/gptel/tools/test/persistent-agent/` (introduced by `add-persistent-agent-test-fixtures` and tasks 5-8). Keep `config/gptel/test/persistence-test-helpers.{el,org}` — they're shared with `session-creation-spec.el` which is unrelated to this change.

The exact list comes from the grep in step 1.

## Implementation steps

1. **Inventory references**:
   ```
   grep -rn '\bdenied[_-]paths\b' . \
     --exclude-dir=runtime \
     --exclude-dir=.git \
     --exclude-dir=openspec/changes/persistent-agent-rebuild
   ```
   Exclude the change's own openspec directory (the proposal/specs/design intentionally reference `denied_paths` as the parameter being removed).

2. **For each hit, decide**:
   - **In-tree preset prompt files** (`config/gptel/presets/*.md`): if the prompt tells the LLM to consider `denied_paths` when calling `PersistentAgent`, remove that instruction. The argument no longer exists.
   - **In source code** (`.org` / `.el`): if the rewrite somehow left a reference, delete it. The grep should be empty here after task 4 lands; if not, that's a bug in task 4.
   - **In docs** (READMEs, CLAUDE.md, etc.): update wording to reflect the new tool surface (4 args, not 5). For CLAUDE.md, since it's project-level guidance, only update if a specific section discusses the tool's argument schema.
   - **In `openspec/specs/persistent-agent/spec.md`** (the *main* spec, not the delta): leave it alone. The main spec is updated when this change archives, not in this task.
   - **In `.tasks/` or `.beads/`**: leave it alone. Those are historical records.

3. **Delete the legacy spec file**:
   ```
   rm config/gptel/test/persistent-agent-spec.el
   ```
   Verify the deletion fixes the failure count: `./bin/run-tests.sh -d config/gptel/test` should now run only `session-creation-spec.el` (and any other unrelated specs). Do NOT delete `persistence-test-helpers.{el,org}`; those are shared with `session-creation-spec.el`.

4. **Re-run the grep** after edits. The non-openspec hits should all be resolved.

5. **Sanity-check preset prompt files**: `config/gptel/presets/*.md` are loaded at init by `jf/gptel-preset-register-all` (in `config/gptel/preset-registration.el`), which populates `gptel--known-presets`. Removing `denied_paths` from instructions doesn't change file load semantics, but verify by running the gptel test suite (which exercises init):
   ```
   ./bin/run-tests.sh -d config/gptel
   ```
   The pre-existing baseline failure count (10 ERT in bash-parser, 24 Buttercup in scope/expansion) is unaffected by preset prompt edits — any new failure relative to baseline indicates a load problem.

   For a more targeted check, count the registered presets in batch mode:
   ```
   make emacs-test-eval EVAL_CMD='(message "presets: %d" (length gptel--known-presets))'
   ```
   The number should match `ls config/gptel/presets/*.md | wc -l` (currently 8).

   Do NOT add `runtime/straight/build/gptel-agent` to the load path — `gptel-agent` is intentionally not a project dependency (commit `eebbc18`, Feb 27).

## Design rationale

The proposal explicitly removes `denied_paths` from the tool's argument surface (proposal.md "What Changes" — BREAKING). The rebuilt persistent-agent module no longer accepts the argument. If in-tree agent prompts continue telling the LLM to pass `denied_paths`, the LLM will attempt to pass it, the tool call will fail validation, and the user-facing experience is broken.

This is a small grep-and-edit cleanup, separated from the main rebuild task (task 4) because:
1. It touches files outside the agent module.
2. It depends on the rebuild having landed (verifying the argument is actually gone from the registered tool).
3. Splitting it gives a clean diff per area.

## Design pattern

`grep -rn` with sensible excludes is sufficient. No tooling beyond standard `find`/`grep`. The change is purely textual.

## Verification

- `grep -rn '\bdenied[_-]paths\b' . --exclude-dir=runtime --exclude-dir=.git --exclude-dir=openspec/changes/persistent-agent-rebuild` returns no hits OR only hits in `openspec/specs/persistent-agent/spec.md` (the main spec, updated at archive time) or `.tasks/` / `.beads/` (historical).
- Preset prompt files in `config/gptel/presets/` load without error after the edit (verified via `./bin/run-tests.sh -d config/gptel` matching the established baseline failure count).
- No new test failures in `./bin/run-tests.sh -d config/gptel/tools`.
- `config/gptel/test/persistent-agent-spec.el` is gone; `./bin/run-tests.sh -d config/gptel/test` no longer reports the 24 stale-symbol failures.

**Done means**: grep is clean (modulo the explicitly-excluded targets), legacy spec file deleted, agents still load, no test regressions.

## Context

proposal.md § "What Changes" — `denied_paths` removal flagged BREAKING
specs/persistent-agent/spec.md (delta) § "Tool invocation and validation" → "Tool argument schema"

## Review

Reviewed 2026-04-26 (orchestrator). Reviewer agent reported clean
work: legacy spec deleted; inventory grep shows only the expected
hit in `openspec/specs/gptel/persistent-agent.md:77` (main spec,
deferred to archive-time sync). No active code or tests reference
the removed symbols. `persistence-test-helpers.{el,org}` correctly
preserved (still required by `session-creation-spec.el`).

One spec-signal worth recording (no inline action needed): this
task body refers to the main spec at
`openspec/specs/persistent-agent/spec.md`, but the actual main spec
lives at `openspec/specs/gptel/persistent-agent.md` (gptel specs are
in their own subdirectory per the change's spec organization). At
archive time, the sync target needs to be the `gptel/` location.
