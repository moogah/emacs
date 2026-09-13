---
name: og-vault-only-discovery
description: Index the vault root only; delete workspace-home discovery, its toggle, and org-graph's workspace-integration module and loader slot.
change: workspace-notes-in-vault
status: ready
relations:
  - enables:int-scaffold-and-lint
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

## Context
design.md § D6 (Discovery bullet)
specs/org-graph/spec.md § 'Vault Discovery', REMOVED 'Workspace-Substrate Discovery', REMOVED 'Workspace Integration'
