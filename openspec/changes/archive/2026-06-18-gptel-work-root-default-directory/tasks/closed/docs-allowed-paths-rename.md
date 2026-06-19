---
name: docs-allowed-paths-rename
description: Sweep and update docs/preset/README/agent references to the old allowed_paths param
change: gptel-work-root-default-directory
status: done
relations:
  - blocked-by:agent-workroot-and-paths
---

## Cycle 2 updates (cycle-1781718724)
> UNBLOCKED: agent-workroot-and-paths is DONE (merge 614b95a7). The tool surface
> rename (`allowed_paths` → `{work_root, read_paths, write_paths}`) and the tool
> `:description` are landed; `register/vocabulary/agent-path-params` is now CONFIRMED.
> Status flipped blocked → **ready**. This is now a pure residual prose sweep: the
> only remaining `allowed_paths` references should be in human-facing docs / agent
> prompts / preset guidance (the code identifier is gone). Run the step-1 grep and
> update guidance to `read_paths`/`write_paths` (+ the auto-`/tmp` scratch note). When
> updating write-access guidance, mention `work_root` too. Last in-change task before
> verify-change.

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

## Observations

The step-1 grep (config/ + openspec/, *.md/*.org, excluding openspec/changes/)
returned **no human-facing prose references to `allowed_paths`/`denied_paths`
left to rename.** The prior cycle (merge 614b95a7, agent-workroot-and-paths)
that landed the code rename and tool `:description` evidently also cleared the
agent-prompt / preset / README prose. This residual sweep is effectively a
no-op against the prose surface; the only commit content is these
Observations/Discoveries annotations on the task file.

Breakdown of every surviving hit and its disposition:

1. `openspec/specs/gptel/persistent-agent.md` (lines 15, 23, 24, 27, 108, 125,
   126, 129, 132, 158, 232, 377, 390, 396, 401) — **LEFT INTENTIONALLY.** These
   are main-spec hits. Per the task and CLAUDE.md, main-spec sync from the
   delta specs happens at `/opsx-sync` / archive, not in this task. The delta
   spec under `openspec/changes/gptel-work-root-default-directory/specs/` is
   the source of truth for the rename; these will be reconciled when the change
   archives. NOT edited here.

2. `config/gptel/tools/persistent-agent.org` (lines 537, 564) — **LEFT, CORRECT
   AS-IS.** These are historical code comments that deliberately name the
   *removed* `allowed_paths` / `allowed-paths-list` to explain WHY the new
   `read-paths`/`write-paths` exist ("Replaces the read role of the removed
   `allowed_paths' (design.md D6)"; "mirroring the old `allowed-paths-list'
   coercion"). They describe a removed identifier as provenance, not as live
   guidance, and renaming them would erase the rationale. No prose change
   warranted.

3. `config/gptel/presets/test-agent-fs-scope.md` (line 31) — **NOT A HIT.**
   "Call `request_scope_expansion` for the denied path" is plain English ("the
   path that was denied"), not the `denied_paths` parameter. The same preset
   already describes the new model correctly in prose ("your write scope is
   /tmp only"). No change.

4. `config/gptel/tools/persistent-agent-trace.org` (lines 197, 202, 214) — see
   ## Discoveries. This is a CODE identifier (around-advice parameter), not
   human-facing prose, and it is a genuine arity/interface drift against the
   current `--task` signature. Out of scope for a prose-only sweep (fixing it
   would change debug-trace behavior); recorded as a discovery for the
   persistent-agent code owner.

No `config/gptel/agents/` directory exists at this tree; the agent-facing
prompts live as preset `.md` files under `config/gptel/presets/`, and none of
them name `allowed_paths`. The path-param vocabulary in all surviving prose
matches `register/vocabulary/agent-path-params` = {work_root, read_paths,
write_paths}.

## Discoveries

- discovery_id: disc-docs-allowed-paths-rename-1
  class: interface-drift
  description: |
    `config/gptel/tools/persistent-agent-trace.org` defines the around-advice
    `jf/gptel-pa-trace--around-task` with the stale signature
    `(orig main-cb preset description prompt &optional allowed-paths)` and
    forwards `allowed-paths` to ORIG at line 214. The current advised function
    `jf/gptel-persistent-agent--task` now has the signature
    `(main-cb preset description prompt &optional work-root read-paths write-paths)`
    (persistent-agent.org:522-524). The advice's single `allowed-paths` arg no
    longer matches: when active, this advice would bind `work-root` to nil and
    drop `read-paths`/`write-paths` entirely on the forwarded call — a latent
    behavioral defect in the trace harness, not just a naming nit. It is CODE,
    not human-facing prose, so it is outside this task's pure-prose scope and
    was NOT edited here.
    affected_register_entry: register/vocabulary/agent-path-params
  recommendation: |
    File a small code task (persistent-agent subsystem / trace harness) to
    update `jf/gptel-pa-trace--around-task` to
    `(orig main-cb preset description prompt &optional work-root read-paths write-paths)`,
    forward all three optional args to ORIG, and update the trace format string
    (currently `allowed=%S`) to log work-root/read-paths/write-paths. Tangle
    persistent-agent-trace.org afterward. This restores trace fidelity and
    completes the vocabulary alignment in the one remaining code site.
