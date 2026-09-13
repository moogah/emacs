---
name: og-vault-only-discovery
description: Index the vault root only; delete workspace-home discovery, its toggle, and org-graph's workspace-integration module and loader slot.
change: workspace-notes-in-vault
status: ready
task_class: module-load
on_critical_path: true
cycle: cycle-1789334430
relations:
  - enables:int-scaffold-and-lint
cites_register_entries:
  - register/invariant/bounded-discovery-roots
  - register/invariant/org-graph-loader-ordered-sequence
  - register/invariant/packages-independent-no-cross-naming
created_at: 2026-09-13T00:00:00Z
planned_at: 2026-09-13T21:20:30Z
---

## Files to modify
- config/org-graph/discovery.org (modify; tangle)
- config/org-graph/org-graph.org (modify; tangle — defcustom removal, loader list, prose)
- config/org-graph/workspace-integration.org and .el (delete)
- config/org-graph/test/discovery-spec.el, module-load-spec.el, workspace-integration-spec.el (modify / delete)
- config/org-graph/docs/spike-eval.org (modify the Discovery section expectations)

## Implementation steps
1. `discovery.org`: `org-graph/index-roots` returns
   `(list (file-name-as-directory (expand-file-name org-graph-vault-root)))`
   (keep the `boundp` guard with the `"~/org/"` fallback). Delete
   `org-graph--active-workspace-homes` and the soft-dependency
   `defvar`/`declare-function` lines for `workspace--registry`,
   `workspace--registered-names`, `workspace--home`, `workspace--sessions-dir`.
   Rewrite the section prose: one root, no other package consulted, hidden
   directories excluded by vulpea.
2. `org-graph.org`: delete `org-graph-watch-workspace-homes`; remove the
   `workspace-integration.el` load line (loader is now ten modules); update
   the module description string (`init.org` entry text can drop "plugged
   into workspaces" — do that in `int-scaffold-and-lint` when touching
   init.org, or here; either is fine, note which).
3. Delete `workspace-integration.org` (both handlers, the menu entry, and
   `--populate-assistant-tools`). Tool injection into the
   `workspace-assistant` preset reappears in `int-workspaces-gptel`.
4. Specs: `org-graph/index-roots` equals the single expanded vault root even
   when a fake `workspaces` feature is provided; `module-load-spec.el`
   asserts the ten-module order; delete `workspace-integration-spec.el`.
5. Runbook: the Discovery section's "workspace home is indexed" checks are
   replaced by "only the vault root is a sync root" and "hidden
   `.agents/` transcript is absent after a full scan".

## Design rationale
Recursive indexing of workspace homes swept up thousands of third-party
org files under a worktree's `runtime/` and evaluated their file-local
variables, dropping into the debugger from the sync timer. vulpea has no
file predicate and its cleanup enforces "in DB ⇔ under a sync root", so a
filter would fight the tool. Notes no longer live in homes, so the only
root is the vault. Deleting the on-create handler and the toggle also
removes org-graph's last references to workspaces, which the
package-independence rule requires.

## Design pattern
Pure deletion plus one simplified function. Keep the `boundp` guard shape
(`discovery.org` § Index roots) and the register invariant
`bounded-discovery-roots` text updated to "exactly one root".

## Verification
- `./bin/tangle-org.sh config/org-graph/discovery.org` and `org-graph.org` validate.
- `./bin/run-tests.sh -d config/org-graph` — green.
- `grep -n 'workspace' config/org-graph/discovery.el config/org-graph/org-graph.el` → no matches.
- `ls config/org-graph/workspace-integration.*` → no such file.
- Whole-tree grep: `grep -rn 'watch-workspace-homes\|active-workspace-homes\|org-graph-workspace' config/ init.org --include='*.org' --include='*.el'` → no matches.
- `grep -rn 'workspace' config/org-graph/*.el` → no matches (whole directory; `tools.el` was the neighbour missed twice in the spike lineage).

## Context
design.md § D6 (Discovery bullet)
specs/org-graph/spec.md § 'Vault Discovery', REMOVED 'Workspace-Substrate Discovery', REMOVED 'Workspace Integration'

## Cited register entries

Register entries are reference material to pressure-test, not authority to defer to. Speculated entries carry explicit licence to push back via Observations / Discoveries.

- `register/invariant/bounded-discovery-roots` — RE-STATED to exactly one root. Pressure-test the `boundp` fallback: with `org-graph-vault-root` unbound the entry expects `~/org/`; confirm and pin in the spec.
- `register/invariant/org-graph-loader-ordered-sequence` — RE-STATED to ten modules. `module-load-spec.el` must assert the new order; note in Discoveries whether the three deferred emacs-startup-hook ops are unchanged by the slot removal.
- `register/invariant/packages-independent-no-cross-naming` — NEW, speculated. Your grep of tangled `config/org-graph/*.el` for `workspace-` is the interim enforcement; the spike's lineage recorded `tools.el` as a neighbour missed twice — grep the whole directory, not the two files you edit.

## Out-of-scope

See sibling tasks in this change; do not fold in cycle-2 work (og-extraction-root-and-agent-dir, int-scaffold-and-lint).

## Observations

(empty — implementor appends during execution)

## Discoveries

(empty — implementor appends structured discoveries; see task-body template)
