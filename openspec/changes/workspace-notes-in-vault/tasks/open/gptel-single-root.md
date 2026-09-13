---
name: gptel-single-root
description: Remove the workspaces consult, force-global threading, the -global command, and the in-package workspace integration so gptel sessions target one root and name no other package.
change: workspace-notes-in-vault
status: ready
task_class: module-load
on_critical_path: true
cycle: cycle-1789334430
relations:
  - enables:int-scaffold-and-lint
  - enables:int-workspaces-gptel
cites_register_entries:
  - register/invariant/sessions-single-root
  - register/invariant/packages-independent-no-cross-naming
  - register/boundary/gptel-sessions-workspace-consult
created_at: 2026-09-13T00:00:00Z
planned_at: 2026-09-13T21:20:30Z
---

## Files to modify
- config/gptel/sessions/filesystem.org (modify; tangle)
- config/gptel/sessions/commands.org (modify; tangle)
- config/gptel/sessions/workspace-integration.org and .el (delete)
- config/gptel/gptel.org (modify; tangle — drop the load line ~:372; keep or move the `jf/gptel-workspace-initial-preset` defcustom, see step 4)
- config/gptel/sessions/test/workspace-integration-spec.el (delete)
- config/gptel/sessions/test/filesystem/*, commands/* specs that exercise `force-global` (modify)
- config/workspaces/test/gptel-integration-spec.el (delete — asserted the consult from the workspaces side)
- config/gptel/sessions/test/workspace-routing-spec.el (delete — asserted the consult from the gptel side; note it lives under gptel/sessions/test, not workspaces/test as design.md D10 says)

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
- Whole-tree grep: `grep -rn 'force-global\|persistent-session-global\|target-sessions-root\|workspace-sessions-dir' config/ init.org --include='*.org'` → no matches.
- `grep -rn 'workspace' config/gptel/sessions/*.el` → no matches (whole directory, not only the files edited).

## Context
design.md § D5 'gptel sessions: root, hooks, keywords, .agents/, branch identity' (Root bullet)
specs/sessions-persistence/spec.md § 'Single sessions root'
specs/workspaces/spec.md § REMOVED 'Workspace-aware gptel session creation'

## Cited register entries

Register entries are reference material to pressure-test, not authority to defer to. Speculated entries carry explicit licence to push back via Observations / Discoveries.

- `register/invariant/sessions-single-root` — NEW, speculated. The statement says creation AND enumeration walk one root. Verify enumeration already does (registry init, branch/agent discovery) — if any enumerator still special-cases a workspace root, that is a Discovery.
- `register/invariant/packages-independent-no-cross-naming` — NEW, speculated; the lint lands in cycle 2. Your whole-tree grep of tangled `config/gptel/sessions/*.el` for `workspace-` / `org-graph` is the interim enforcement. Report any residual match you cannot remove in this task (e.g. prose in a docstring) as an Observation.
- `register/boundary/gptel-sessions-workspace-consult` — SUPERSEDED by this task. Read it to see every consumer the old consult had; each must be gone or re-homed, not defaulted.

## Out-of-scope

See sibling tasks in this change; do not fold in cycle-2 work (int-workspaces-gptel, int-scaffold-and-lint).

## Observations

- Step 1 names `jf/gptel--create-session-core` as a `force-global` carrier;
  it never had that parameter. The parameter only threaded
  `jf/gptel-persistent-session` → `jf/gptel--create-session-directory`.
  Both are now parameter-free on that axis (arity checks in the new spec);
  nothing to remove in `create-session-core`.
- Step 5: `jf/gptel-session-status` does not exist anywhere under
  `config/gptel/` (grep `session-status` → no matches). No-op.
- Step 4: took the "let `int-workspaces-gptel` define it" option — no
  defcustom named `jf/gptel-workspace-initial-preset` remains in the
  sessions package. Prose at `config/gptel/presets/workspace-assistant/preset.org:10`
  still points at that symbol; it is untangled commentary, left for
  `int-workspaces-gptel` to re-point when it defines the knob in
  `config/integrations/workspaces-gptel.org`.
- The whole-tree `--include='*.org'` verification grep cannot be fully
  clean from this task alone. Residual matches, all sibling-owned
  (`ws-node-id-and-payload` deletes the producer): `config/workspaces/workspaces.org:399,415,425`
  (`workspace-sessions-dir` itself), `config/workspaces/data-model.org:421`
  (prose), `config/workspaces/docs/README.org:196,209` (the "gptel sessions"
  README section that documents the consult and the now-deleted `-global`
  command). That README section should be rewritten or removed by the
  sibling; not touched here. Every match under `config/gptel/` is gone.
- Stale prose pointers to the deleted module path remain in
  workspaces-owned files: `config/workspaces/git-worktrees.org:9`,
  `config/workspaces/tabs.org:315` (and the tangled `tabs.el:166`
  docstring). The D1 workspaces pattern (`jf/gptel-\|gptel-\|org-graph`)
  does not match the path string `config/gptel/sessions/...`, so it will
  not trip the cycle-2 lint, but it is misleading and should be re-pointed
  at `config/integrations/workspaces-gptel.org` by whichever sibling next
  touches those files.
- Deleting `config/workspaces/test/gptel-integration-spec.el` also removed
  its "directionality: workspaces never references gptel" glob lint. Until
  `int-scaffold-and-lint` lands `boundary-lint-spec.el`, the workspaces →
  gptel direction has no automated enforcement. The gptel-sessions →
  workspaces/org-graph direction IS enforced now, by the grep `it` in
  `config/gptel/sessions/test/filesystem/single-root-spec.el`.
- Also cleaned two stale historical parentheticals in `config/gptel/gptel.org`
  prose ("activities integration removed … a workspace-scoped equivalent
  may land later"), same class as step 6 but in the loader file. Prose only;
  `gptel.el` diff is exactly the dropped load line.
- Adjacent nit, untouched: `jf/gptel--find-session-directory` binds an
  unused `root` and re-expands `jf/gptel-sessions-directory` inline.
  Harmless; out of scope.
- The new spec runs the real creation path (`jf/gptel--create-session-core`)
  with only `jf/gptel-scope-profile--create-for-session` stubbed (it needs
  a registered preset, orthogonal to root resolution). `org-id` stamping
  runs for real and logs "Could not read org-id-locations … setting it to
  nil" in the batch process — the same benign noise the pre-existing ERT
  `filesystem-test.el` produces.
- design.md D10 lists `workspace-routing-spec.el` under
  `config/workspaces/test/`; it lived at
  `config/gptel/sessions/test/workspace-routing-spec.el` (as the task body
  already notes) and was deleted from there. D10 should be corrected at sync.

## Discoveries

- discovery_id: disc-gptel-single-root-1
  class: interface-drift
  description: |
    The entry statement says "jf/gptel--ensure-sessions-root is the one
    resolver both the creation and the inventory side call". Verified the
    enumerators: jf/gptel--init-registry (via jf/gptel--list-session-
    directories), jf/gptel--find-session-directory and
    jf/gptel--find-all-branches-with-agents all walk
    (expand-file-name jf/gptel-sessions-directory) — the same root — but
    they read the defcustom inline and do NOT call
    jf/gptel--ensure-sessions-root (deliberately: inventory must not
    mkdir). Only the creation side calls the resolver. The invariant
    holds (one defcustom, no alternative root, no consult) and the new
    spec proves created ⇒ enumerable in-process; only the "one resolver
    both sides call" wording is inaccurate. No enumerator special-cases a
    workspace root.
  affected_register_entry: register/invariant/sessions-single-root
  recommendation: |
    Reconcile statement to: "both sides resolve (expand-file-name
    jf/gptel-sessions-directory); creation goes through
    jf/gptel--ensure-sessions-root, which additionally creates the root."
    Set enforcement_mechanism.location to
    config/gptel/sessions/test/filesystem/single-root-spec.el (8 specs:
    creation under bound root, resolver called once, arities (1 . 1) /
    (1 . 4), init-registry + all three enumerators list the created
    session, target-sessions-root / persistent-session-global unbound,
    grep of tangled sessions/*.el for "workspace-\\|org-graph" is empty).
    speculated → reconciled-recommended.

- discovery_id: disc-gptel-single-root-2
  class: responsibility-leakage
  description: |
    Task step 4 offered two homes for the defcustom
    jf/gptel-workspace-initial-preset; chose "int-workspaces-gptel
    defines it". The knob is workspace-flavoured (it names the workspace
    birth session) and the sessions package now has no consumer for it,
    so carrying it here would be exactly the cross-naming the invariant
    forbids (the symbol contains "workspace-", which the tangled-.el grep
    would flag). It leaves the package with this task; the name must be
    kept when the integration layer defines it (design D8).
  affected_register_entry: register/invariant/packages-independent-no-cross-naming
  recommendation: |
    int-workspaces-gptel defines (defcustom jf/gptel-workspace-initial-preset
    'workspace-assistant …) in config/integrations/workspaces-gptel.org and
    re-points the prose at config/gptel/presets/workspace-assistant/preset.org:10.
    Note in the entry that a jf/gptel- prefixed symbol containing
    "workspace-" is legal ONLY under config/integrations/.

- discovery_id: disc-gptel-single-root-3
  class: interface-drift
  description: |
    Consumer inventory of the superseded boundary, as found: (1)
    jf/gptel--target-sessions-root — deleted; (2) the force-global
    parameter on jf/gptel--create-session-directory and
    jf/gptel-persistent-session — removed, not defaulted; (3)
    jf/gptel-persistent-session-global — deleted; (4)
    config/gptel/sessions/workspace-integration.org — deleted, including
    the jf/gptel-workspace-initial-preset defcustom, jf/gptel--workspace-
    on-create, jf/gptel--workspace-read-preset, jf/gptel--workspace-add-
    session and the with-eval-after-load registration. Note (4) did NOT
    consume workspace-sessions-dir at all: both handlers read
    :sessions-dir straight off the anchor payload and built
    (expand-file-name id sdir) themselves — a second consumer path the
    entry's consumer list never named. The producer workspace-sessions-dir
    (config/workspaces/workspaces.org:425) still exists pending
    ws-node-id-and-payload. The entry's specs (workspace-routing-spec.el,
    gptel-integration-spec.el) are deleted.
  affected_register_entry: register/boundary/gptel-sessions-workspace-consult
  recommendation: |
    Record consumer side as fully removed at this commit and add the
    payload-:sessions-dir path to the supersession note so
    register/shape/workspace-integration-anchor-payload reconciliation
    (ws-node-id-and-payload drops :sessions-dir) knows its last consumer
    is gone.

- discovery_id: disc-gptel-single-root-4
  class: spec-signal
  description: |
    Interim enforcement of the three-package rule after this task is
    asymmetric: the gptel-sessions side is lint-enforced by the grep `it`
    in single-root-spec.el (tangled config/gptel/sessions/*.el, pattern
    "workspace-\\|org-graph", zero matches), while the workspaces side lost
    its only lint when config/workspaces/test/gptel-integration-spec.el
    was deleted per the task body — the register note anticipated a
    replace-not-delete but the task and D10 both say delete. The gap is
    open until int-scaffold-and-lint lands boundary-lint-spec.el.
  affected_register_entry: register/invariant/packages-independent-no-cross-naming
  recommendation: |
    Update enforcement_mechanism.notes: "gptel-sessions side enforced by
    config/gptel/sessions/test/filesystem/single-root-spec.el as of
    gptel-single-root; workspaces side unenforced until cycle-2
    boundary-lint-spec.el". Make int-scaffold-and-lint's lint a hard
    requirement of cycle 2's gate, not optional.
