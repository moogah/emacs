---
name: gptel-hidden-agents
description: Rename the sub-agent directory from agents/ to .agents/ across path helpers, enumerators, and PersistentAgent creation.
change: workspace-notes-in-vault
status: ready
task_class: refactor
on_critical_path: true
cycle: cycle-1789334430
relations:
  - enables:gptel-branch-identity
  - enables:int-migration-command
cites_register_entries:
  - register/vocabulary/session-tree-directory-names
  - register/boundary/session-dir-marker-walk
created_at: 2026-09-13T00:00:00Z
planned_at: 2026-09-13T21:20:30Z
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
- Whole-tree grep: `grep -rn '"agents"\|/agents/\|agents/' config/gptel --include='*.org' --include='*.el'` → only `jf/gptel--agents-dir-path` and prose.
- One un-stubbed scenario: create a session in a temp root, create an agent under it through the real PersistentAgent directory helper, then walk from the agent transcript up via the marker walk and enumerate — no stubs on the path helpers.

## Context
design.md § D5 ('.agents/' bullet); § Risks (indexing chats)
specs/sessions-persistence/spec.md § 'Hidden agents directory'
specs/sessions-branching/spec.md § 'Agent replication'

## Cited register entries

Register entries are reference material to pressure-test, not authority to defer to. Speculated entries carry explicit licence to push back via Observations / Discoveries.

- `register/vocabulary/session-tree-directory-names` — NEW, speculated. The single-source rule (`.agents` appears only in `jf/gptel--agents-dir-path`) is the contract; every literal you find elsewhere is a member of the Observations, and any enumerator using a `"^[^.]"` filter is a Discovery.
- `register/boundary/session-dir-marker-walk` — Confirmed; annotated. The walk from an agent transcript up to the nearest `branches/` must still resolve with the hidden directory in the path — add one spec for it.

## Out-of-scope

See sibling tasks in this change; do not fold in cycle-2 work (int-migration-command (renames existing agents/ dirs), gptel-branch-identity).

## Plan-phase notes (architect forward-mode, cycle-1789334430)

- The `"agents"` literal lives in `jf/gptel-session--agents-dir` (`config/gptel/sessions/constants.org:85`), not only in `jf/gptel--agents-dir-path` as D5 says. Change it there and make the path helper the sole composer; treat any other literal as a duplication to remove.

## Observations

- Production code was already single-sourced: every path site in
  `filesystem.org`, `branching.org`, and `persistent-agent.org` composed
  through `jf/gptel--agents-dir-path`, and the only production literal was
  the defconst `jf/gptel-session--agents-dir` (`constants.org`), exactly as
  the plan-phase note said. The rename is therefore a one-token change in
  `constants.org` plus prose/docstrings; the enumerators needed no regexp
  adjustment because none of them discovers the agents directory by listing
  a branch dir — they all list *children* of a directory reached through the
  helper, where `"^[^.]"` is correct.
- A second literal did exist outside the register's producer/consumer map:
  `config/gptel/drawer-trace.org` (`jf/gptel-drawer--session-p`) matched
  session files with the regex `/\(?:branches\|agents\)/…`. It is a
  standalone diagnostic module (its loader line in `gptel.org` is commented
  out) but it lives under `config/gptel/`, so under the single-source rule it
  now composes the alternation from `(regexp-quote jf/gptel-session--agents-dir)`
  and requires `gptel-session-constants`. The task's whole-tree grep
  (`'"agents"\|/agents/\|agents/'`) could not have caught it because the
  regex alternation separates the token from its slash — grep-based
  single-source verification has a blind spot for regexes.
- Fourteen test fixture sites across nine spec files hardcoded
  `(expand-file-name "agents" …)` or `branches/main/agents/…` paths. They now
  spell `.agents` as a *literal* on purpose (rather than calling
  `jf/gptel--agents-dir-path`), so the specs pin the on-disk name and would
  catch the constant regressing; calling the helper would make them
  tautological. The ERT file `filesystem-test.el` was updated in place, not
  migrated.
- Delta spec `sessions-branching` scenario "Empty agents directory" says the
  child branch SHALL have an empty `.agents/` when the parent has none, but
  the pre-existing `jf/gptel--copy-branch-agents` only creates the child
  `.agents/` when the parent's exists. Out of scope for a rename; the new spec
  asserts only the "does not fail, child lists no agents" half. See
  disc-gptel-hidden-agents-3.
- The main specs `openspec/specs/gptel/sessions-persistence.md` (layout
  diagram, agent-creation scenario, "pure storage convention" sentence) and
  `sessions-branching.md` still say `agents/`; left for the archive sync per
  the task's files list. The delta specs already carry `.agents/`.
- `config/gptel/sessions/test-report.txt` and `config/gptel/tools/test-report.txt`
  were regenerated by the `--report` baseline runs; restored to HEAD so the
  commit carries no snapshot noise (the orchestrator's post-merge full run
  refreshes them).
- New spec `config/gptel/sessions/test/filesystem/hidden-agents-dir-spec.el`
  calls `jf/gptel--create-session-directory` with one argument, so it is
  unaffected by the sibling task removing the `force-global` parameter.
  `jf/gptel--copy-branch-agents` is exercised directly rather than through
  `jf/gptel--create-branch-session`, keeping the spec clear of the branch
  hook / identity-rewrite code the `gptel-session-keywords-and-hooks` and
  `gptel-branch-identity` tasks own.

## Discoveries

- discovery_id: disc-gptel-hidden-agents-1
  class: duplication
  description: |
    The vocabulary entry lists the defconst in constants.org as the ONE
    place the agents-directory name is spelled and maps four consumers
    (filesystem.org, branching.org, persistent-agent.org, registry.org).
    Implementation found a fifth site: config/gptel/drawer-trace.org's
    jf/gptel-drawer--session-p hardcoded the name inside a regex
    alternation ("/\(?:branches\|agents\)/[^/]+/session\.org\'").
    It is a diagnostic module, currently not loaded, but it would have
    silently stopped matching agent transcripts after the rename. It now
    builds the alternation from (regexp-quote jf/gptel-session--agents-dir)
    and requires gptel-session-constants.
  affected_register_entry: register/vocabulary/session-tree-directory-names
  recommendation: |
    Add config/gptel/drawer-trace.org (jf/gptel-drawer--session-p) to
    consumer_mapping with routing_function "regexp-quote of
    jf/gptel-session--agents-dir". Extend the entry's verification note:
    a grep for the bare token `agents` inside regex strings is needed in
    addition to the "agents"/agents/ greps, since alternation separates
    the token from its slash.
- discovery_id: disc-gptel-hidden-agents-2
  class: spec-signal
  description: |
    The speculated single-source rule and the enumerator_rule both held
    under implementation pressure. Changing the defconst to ".agents" was
    sufficient for jf/gptel--agents-dir-path, jf/gptel--create-agent-directory,
    jf/gptel--list-agent-directories, jf/gptel--find-all-branches-with-agents,
    jf/gptel--copy-branch-agents, and the PersistentAgent directory creation
    to land under the hidden directory with no other code change; the
    sessions (7 ERT + 172 buttercup) and tools (13 ERT + 84 buttercup)
    suites are green. No enumerator discovers the agents directory by
    listing a branch dir with "^[^.]" — a spec now pins that a "^[^.]"
    listing of a branch dir does NOT show .agents while the helper does
    reach it. Line references in the entry drift: the defconst is now at
    constants.org:~96 (prose was added above it), not :85.
  affected_register_entry: register/vocabulary/session-tree-directory-names
  recommendation: |
    Move status speculated -> reconciled. Keep canonical_mapping_function
    and enumerator_rule as written; refresh the constants.org line
    reference; add the drawer-trace consumer from disc-1.
- discovery_id: disc-gptel-hidden-agents-3
  class: spec-signal
  description: |
    Delta spec sessions-branching § "Agent replication", scenario "Empty
    agents directory", requires the child branch to have an EMPTY .agents/
    when the parent has none. The pre-existing jf/gptel--copy-branch-agents
    (unchanged by this task apart from docstring) creates the child
    .agents/ only inside (when (file-directory-p parent-agents-dir) ...),
    so a parent with no agents yields a child with no .agents/ at all.
    Behaviourally harmless (every lister treats "absent" and "empty" the
    same) but the scenario and the code disagree, and this predates the
    rename.
  affected_register_entry: register/vocabulary/session-tree-directory-names
  recommendation: |
    Either soften the scenario to "the new branch SHALL have no agent
    directories and branch creation SHALL NOT fail", or add an
    unconditional make-directory of the child .agents/ in
    jf/gptel--copy-branch-agents as a one-line follow-up owned by whichever
    task next touches branching.org. Not folded in here (rename-only task).
- discovery_id: disc-gptel-hidden-agents-4
  class: spec-signal
  description: |
    Confirmed the boundary is unaffected by the hidden component: from
    <session>/branches/main/.agents/<agent>/session.org the drawer-first
    type resolution yields `agent' and jf/gptel--session-dir-from-branch-dir
    returns the agent's own directory; run with type `branch' from the same
    path, the locate-dominating-file walk still resolves to <session> (the
    branches/ ancestor encloses the start). The existing agent-layout case
    in session-dir-walk-spec.el is re-pointed at .agents/, and
    hidden-agents-dir-spec.el adds the un-stubbed end-to-end walk +
    enumeration scenario the task asked for.
  affected_register_entry: register/boundary/session-dir-marker-walk
  recommendation: |
    No contract change. Update the status_note's spec pointer to
    config/gptel/sessions/test/filesystem/hidden-agents-dir-spec.el
    ("marker walk from an agent transcript under .agents/").
