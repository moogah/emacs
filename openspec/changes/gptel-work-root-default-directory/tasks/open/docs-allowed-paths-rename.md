---
name: docs-allowed-paths-rename
description: Sweep and update docs/preset/README/agent references to the old allowed_paths param
change: gptel-work-root-default-directory
status: blocked
relations:
  - blocked-by:agent-workroot-and-paths
---

## Files to modify
- TBD by grep — candidates: `config/gptel/agents/*.md`, `config/gptel/presets/*.md`,
  `openspec/specs/gptel/persistent-agent.md` (sync handled at archive), README/docs,
  any preset guidance that names `allowed_paths`.

## Implementation steps
1. Grep the repo for stale references to the renamed/removed params:
   ```bash
   grep -rn "allowed_paths\|allowed-paths\|denied_paths" config/ openspec/ \
     --include="*.md" --include="*.org" | grep -v "openspec/changes/"
   ```
   (Exclude this change's own dir; the delta specs intentionally describe the rename.)
2. For each hit that is human-facing guidance (agent system prompts, preset docs,
   READMEs, tool-usage examples), update `allowed_paths` → `read_paths` and, where the
   text describes write access, mention `write_paths` and the auto-`/tmp` scratch.
   Update any example that shows passing paths to PersistentAgent.
3. Leave code identifiers that legitimately still exist (none should remain after
   `agent-workroot-and-paths`) — this task is the residual doc sweep; the tool
   `:description` itself is updated in that task.
4. Do NOT edit `openspec/specs/gptel/*` directly — main-spec sync happens via
   `/opsx-sync` / archive from the delta specs.
5. Tangle any `.org` docs that are tangled config:
   `./bin/tangle-org.sh <file>` (skip pure-prose `.md`).

## Design rationale
The `allowed_paths` → `read_paths`/`write_paths` rename is BREAKING at the tool-schema
level. There is no persisted-data migration (the model re-reads the schema each session),
but human-facing guidance and agent prompts that still say `allowed_paths` would mislead
both readers and the model. This sweep is the documented mitigation for that risk
(design Risks/Trade-offs).

## Design pattern
Pure find-and-replace with judgment; one commit. Good janitorial follow-up — no behavior
change.

## Verification
- The grep from step 1 returns no human-facing `allowed_paths` references outside
  `openspec/changes/`.
- Any tangled `.org` doc still validates: `./bin/tangle-org.sh <file>`.
- Spot-check one agent `.md` / preset to confirm guidance reads correctly.

## Context
design.md § Risks / Trade-offs '[BREAKING schema rename allowed_paths → read_paths/write_paths]'
specs/persistent-agent/spec.md § 'Requirement: Tool invocation and validation'
