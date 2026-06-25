---
name: agent-work-root-into-write-scope
description: Prepend the parent-passed work_root (<work_root>/**) to the agent's WRITE scope in --task, so relative writes under the work root are in scope by construction (not just reads). Folds the now-redundant read-only auto-include into the write prepend. Fixes a spec/implementation divergence — the shipped auto-include landed on the READ axis while the silent-DENY problem it was meant to solve is on the WRITE axis.
status: ready
source: openspec/changes/archive/2026-06-18-gptel-work-root-default-directory
relations:
  - discovered-from:agent-work-root-auto-read
  - surfaced-by:.tasks/explore-working-directory-scoping.org
---

> Surfaced 2026-06-19 in the `/opsx-explore` working-directory-scoping session
> (see `.tasks/explore-working-directory-scoping.org`, RESOLVED E "Assumption
> taken for this work"). The dynamic-preamble change assumes agents have
> appropriate WRITE permissions to their work root; this task makes that true.
> Kept separate from the preamble change per user disposition (boundary
> decision, 2026-06-19): distinct bug, distinct spec scenario already written.

## The bug in one sentence

The PersistentAgent work-root change auto-included `work_root/**` in the agent's
**READ** scope, but the silent-DENY problem it set out to kill is on the
**WRITE** axis — a relative write under `work_root` (e.g. the model writes
`out.txt` while `default-directory` = the assigned worktree) still resolves to a
path that is in no write glob and silently DENIES.

## Why the implementation is on the wrong axis (drift diagnosis)

The intent (exploration-log Thread E) was: "a relative write resolves under
`work_root` → must fall inside a write glob, else silent DENY." That is a
write-scope alignment problem. The fix drifted:

1. The cycle-2 D7 guardrail was implemented as a check of `work_root` against the
   **read** list (`jf/gptel-scope--path-matches-any-pattern-p resolved-work-root
   read-paths-list`).
2. Task `agent-work-root-auto-read` (commit `b62b7e38`) replaced that guardrail
   by **prepending `work_root/**` to the READ list** — consolidating the drift.
3. Net result: `work_root` readable by construction; relative WRITES still
   silently deny. The original problem is unfixed.

## Two confirmations that WRITE is the correct axis

1. **Validator permission hierarchy** (`config/gptel/scope/scope-validation.org:
   188-193`): read-like ops are allowed if the path matches `read OR write`;
   write-like ops require `write`. So placing `work_root/**` in **write** scope
   makes the work root **both writable AND readable** — it subsumes the existing
   read prepend. The read-axis choice can never enable writes.
2. **The work-root change's own spec already requires it**
   (`openspec/changes/archive/2026-06-18-gptel-work-root-default-directory/specs/
   scope/spec.md:28-33`): *"Scenario: A relative write inside the work root is in
   scope by construction … the resolved path falls inside `paths.write` … the
   write is allowed without scope expansion."* This holds for **chat** sessions
   (the coding profile expands `write: ${project_root}/**` and chat's
   `work_root == project_root`) but is **FALSE for agents** today — agents have
   no profile, so their write scope is `write_paths + /tmp/**` only. The agent
   path does not meet its own spec.

## Files to modify

- `config/gptel/tools/persistent-agent.org` — in `jf/gptel-persistent-agent--task`,
  prepend the work-root pattern to `write-paths-list` (mirroring the existing
  read prepend at `:598-603`) BEFORE calling `build-scope-plist` (`:616-618`).
  The pattern is the same `(concat (directory-file-name resolved-work-root)
  "/**")`. Dedup with `member`. Once `work_root/**` is in write scope, the
  separate **read** prepend (`:598-603`) becomes redundant (write implies read
  via the hierarchy) — fold it out, or leave it as a harmless cosmetic explicit.
  Re-tangle + validate.
- `config/gptel/tools/test/persistent-agent/work-root-spec.el` — add/adjust specs:
  `work_root/**` appears in `:GPTEL_SCOPE_WRITE:`; a relative WRITE under the
  work root validates as ALLOWED with NO caller-supplied `write_paths`; a
  relative READ under the work root is still ALLOWED (now via write⊇read).
- `config/gptel/tools/persistent-agent.org` tool `:description` — update the
  guidance that currently tells the model to pass `write_paths` for the work
  tree; the work tree is now writable by construction (the model still passes
  `write_paths` for paths OUTSIDE the work root).

## Design note / decision to confirm at implementation

This flips the agent default to "the work tree is writable" (the `/tmp`-only-by-
default era ends), matching how chat sessions already behave via the coding
profile. `standard-deny-paths` (`**/.git/**`, `**/runtime/**`, `**/.env`,
`**/node_modules/**`) still protects the dangerous paths under the work root. If
a genuinely read-only agent is ever needed, that becomes a separate opt-out
rather than the default — out of scope here. (User decision 2026-06-19:
"assume agents will have appropriate write permissions.")

## Verification

```bash
./bin/tangle-org.sh config/gptel/tools/persistent-agent.org
./bin/run-tests.sh -d config/gptel/tools
grep -n "write-paths-list\|work-root-write\|directory-file-name resolved-work-root" \
  config/gptel/tools/persistent-agent.org
```

A `--task` spawn with `work_root=/p` and no `write_paths` must yield a drawer
`:GPTEL_SCOPE_WRITE:` containing `/p/**` (plus `/tmp/**`), and a relative write
of `out.txt` in that agent must validate ALLOWED.
