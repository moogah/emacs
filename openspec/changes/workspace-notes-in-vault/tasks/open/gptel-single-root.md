---
name: gptel-single-root
description: Remove the workspaces consult, force-global threading, the -global command, and the in-package workspace integration so gptel sessions target one root and name no other package.
change: workspace-notes-in-vault
status: ready
relations:
  - enables:int-scaffold-and-lint
  - enables:int-workspaces-gptel
---

## Files to modify
- config/gptel/sessions/filesystem.org (modify; tangle)
- config/gptel/sessions/commands.org (modify; tangle)
- config/gptel/sessions/workspace-integration.org and .el (delete)
- config/gptel/gptel.org (modify; tangle — drop the load line ~:372; keep or move the `jf/gptel-workspace-initial-preset` defcustom, see step 4)
- config/gptel/sessions/test/workspace-integration-spec.el (delete)
- config/gptel/sessions/test/filesystem/*, commands/* specs that exercise `force-global` (modify)
- config/workspaces/test/gptel-integration-spec.el, workspace-routing-spec.el (delete — they asserted the consult from the workspaces side)

## Implementation steps
1. `filesystem.org`: replace `jf/gptel--target-sessions-root` (~:82-101) with
   a direct call to `jf/gptel--ensure-sessions-root`; remove the
   `force-global` parameter from `jf/gptel--create-session-directory`
   (~:103-116) and from `jf/gptel--create-session-core` in `commands.org`
   (find every caller: `grep -n 'force-global' config/gptel/**/*.org`).
   Delete the `featurep 'workspaces` / `fboundp 'workspace-sessions-dir`
   consult and its prose (~:33-53, :76-101).
2. `commands.org`: delete `jf/gptel-persistent-session-global` (~:879-903)
   and the routing prose (~:785-800). `jf/gptel-persistent-session` keeps
   its prefix-arg preset selection unchanged.
3. Delete `config/gptel/sessions/workspace-integration.org` (birth session,
   `g` menu entry, `jf/gptel--workspace-read-preset`, registration). These
   behaviours reappear in `int-workspaces-gptel`.
4. `jf/gptel-workspace-initial-preset` (defcustom, currently in the deleted
   module) is needed by the integration layer; keep the defcustom name.
   Simplest: leave a one-line defcustom in `commands.org` under a
   "Public knobs" heading with no consumer in the package, OR let
   `int-workspaces-gptel` define it. Choose the latter (the package should
   not carry a workspace-flavoured knob) and note it in Discoveries.
5. `jf/gptel-session-status` (~:973) already prints only the global root —
   verify its wording no longer mentions workspaces.
6. Update `config/gptel/sessions/commands.org` intro prose (~:20-23) that
   references the activities/workspaces history.
7. Tests: creating a session from any context writes under
   `jf/gptel-sessions-directory`; `jf/gptel--init-registry` in a temp root
   lists it; `grep` of tangled `config/gptel/sessions/*.el` for
   `workspace-` yields nothing.

## Design rationale
The consult violated the package-independence rule and produced a latent
bug: sessions created under workspace homes were invisible to the registry
and every listing command because enumeration only walks the one root.
Removing the alternative root fixes both. The default root stays
`~/.gptel/sessions/`; pointing it into the vault is a configuration line
(`configure-migrate-and-verify`) the package need not understand.

## Design pattern
Deletion task: prefer removing parameters over defaulting them, so the
tangled `.el` has no dead `force-global` plumbing. Keep
`jf/gptel--ensure-sessions-root` as the single root resolver.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/filesystem.org` and `commands.org` validate; `config/gptel/gptel.org` tangles.
- `./bin/run-tests.sh -d config/gptel/sessions` — green.
- `./bin/run-tests.sh -d config/workspaces` — green after deleting the two consult specs.
- `grep -n 'workspace' config/gptel/sessions/filesystem.el config/gptel/sessions/commands.el` → no matches.
- `ls config/gptel/sessions/workspace-integration.*` → no such file.

## Context
design.md § D5 'gptel sessions: root, hooks, keywords, .agents/, branch identity' (Root bullet)
specs/sessions-persistence/spec.md § 'Single sessions root'
specs/workspaces/spec.md § REMOVED 'Workspace-aware gptel session creation'
