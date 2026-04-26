---
name: scrub-denied-paths-references
description: Remove denied_paths references from in-tree agent prompts and any remaining docs
change: persistent-agent-rebuild
status: blocked
relations:
  - blocked-by:rebuild-persistent-agent-module
---

## Files to modify

- Anywhere `denied_paths` (or `denied-paths` as a kebab-case variant) appears outside the persistent-agent module rewrite scope. Likely candidates:
  - `config/gptel/agents/*.md` (5 agent definition files — the prompts may instruct the LLM to set `denied_paths`)
  - `config/gptel/tools/persistent-agent.org` itself (any leftover references in docstrings or commentary that the rewrite missed)
  - `config/gptel/tools/README.org` (if present and discusses tool args)
  - Top-level docs (CLAUDE.md, README.md) — verify with grep

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
   - **In-tree agent prompt files** (`config/gptel/agents/*.md`): if the prompt tells the LLM to consider `denied_paths` when calling `PersistentAgent`, remove that instruction. The argument no longer exists.
   - **In source code** (`.org` / `.el`): if the rewrite somehow left a reference, delete it. The grep should be empty here after task 4 lands; if not, that's a bug in task 4.
   - **In docs** (READMEs, CLAUDE.md, etc.): update wording to reflect the new tool surface (4 args, not 5). For CLAUDE.md, since it's project-level guidance, only update if a specific section discusses the tool's argument schema.
   - **In `openspec/specs/persistent-agent/spec.md`** (the *main* spec, not the delta): leave it alone. The main spec is updated when this change archives, not in this task.
   - **In `.tasks/` or `.beads/`**: leave it alone. Those are historical records.

3. **Re-run the grep** after edits. The non-openspec hits should all be resolved.

4. **Sanity-check agent prompt files**: `config/gptel/agents/*.md` are loaded by the `gptel-agent` package's `gptel-agent-update` to register agent definitions. Removing `denied_paths` from instructions doesn't change file load semantics, but verify by tangling and loading the agents directory in a fresh Emacs:
   ```
   emacs --batch -L runtime/straight/build/gptel \
                 -L runtime/straight/build/gptel-agent \
                 -l gptel-agent \
                 --eval "(progn (gptel-agent-update) (message \"agents loaded ok\"))"
   ```
   The output should be `agents loaded ok` without errors.

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
- Agent prompt files in `config/gptel/agents/` load without error after the edit.
- No new test failures in `./bin/run-tests.sh -d config/gptel/tools`.

**Done means**: grep is clean (modulo the explicitly-excluded targets), agents still load, no test regressions.

## Context

proposal.md § "What Changes" — `denied_paths` removal flagged BREAKING
specs/persistent-agent/spec.md (delta) § "Tool invocation and validation" → "Tool argument schema"
