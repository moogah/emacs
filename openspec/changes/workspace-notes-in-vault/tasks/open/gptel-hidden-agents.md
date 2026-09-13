---
name: gptel-hidden-agents
description: Rename the sub-agent directory from agents/ to .agents/ across path helpers, enumerators, and PersistentAgent creation.
change: workspace-notes-in-vault
status: ready
relations:
  - enables:gptel-branch-identity
  - enables:int-migration-command
---

## Files to modify
- config/gptel/sessions/filesystem.org (modify; tangle — `jf/gptel--agents-dir-path` ~:348, `jf/gptel--find-all-branches-with-agents` ~:382, `jf/gptel--create-agent-directory` ~:440, `jf/gptel--list-agent-directories` ~:459)
- config/gptel/tools/persistent-agent*.org (modify if it hardcodes `agents`; grep)
- config/gptel/sessions/branching.org (modify; tangle — `jf/gptel--copy-branch-agents` uses the helper; confirm no literal)
- config/gptel/sessions/test/filesystem/*-spec.el, branching/*-spec.el (modify fixtures)
- openspec/specs/gptel/sessions-persistence.md layout diagram (read only; synced at archive)

## Implementation steps
1. `grep -rn '"agents"\|/agents\b\|agents/' config/gptel --include='*.org'` and
   change every literal to `.agents`. The single source should be
   `jf/gptel--agents-dir-path`; make every other site call it.
2. `jf/gptel--find-all-branches-with-agents` walks
   `<root>/*/branches/*/agents/*` — update the glob/`directory-files`
   pattern to the hidden name (note `directory-files` with `"^[^.]"` filters
   would now *exclude* `.agents`; adjust the regexp where directories are
   enumerated by name).
3. No compatibility shim for `agents/`: the migration command renames
   existing directories. State that in the prose.
4. Fixtures: temp session trees in specs create `.agents/`.
5. Spec: creating an agent under `branches/main/` yields
   `branches/main/.agents/<preset>-<ts>-<slug>/`; the enumerator finds it;
   listing agent directories does not return `.` / `..`.

## Design rationale
Sessions will live in the vault and be indexed. Branching copies agent
directories wholesale, so their transcripts would carry duplicate org IDs
and clutter the finder. vulpea excludes any path containing a hidden
directory component, so the rename keeps agent transcripts out of the
index without the sessions package naming any indexer — the layout is
"pure storage convention" per the persistence spec, and identity is
drawer-first, so the rename carries no identity meaning.

## Design pattern
Single path helper (`jf/gptel--agents-dir-path`) as the only place the
name appears; every enumerator composes from it, mirroring
`jf/gptel--branches-dir-path`.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/filesystem.org` validates (and any other touched file).
- `./bin/run-tests.sh -d config/gptel/sessions` and `./bin/run-tests.sh -d config/gptel/tools` — green.
- `grep -n '"agents"' config/gptel/sessions/filesystem.el config/gptel/sessions/branching.el` → no matches; `grep -n '\.agents' config/gptel/sessions/filesystem.el` → the helper.

## Context
design.md § D5 ('.agents/' bullet); § Risks (indexing chats)
specs/sessions-persistence/spec.md § 'Hidden agents directory'
specs/sessions-branching/spec.md § 'Agent replication'
