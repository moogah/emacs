---
name: broken-home-tolerance
description: Activation guards in workspace-switch/workspace-restore; add workspace-re-anchor; bind C-x w R
change: add-workspace-home-directory
status: ready
relations: []
---

## Files to modify
- `config/workspaces/tabs.org` (modify — add broken-state guard to `workspace-switch`)
- `config/workspaces/persistence.org` (modify — add broken-state guard to `workspace-restore`)
- `config/workspaces/workspaces.org` (modify — add `workspace-re-anchor` command, `C-x w R` binding)
- `config/workspaces/test/broken-home-runtime-spec.el` (new — Buttercup)

## Implementation steps

1. In `tabs.org`, add a guard at the top of `workspace-switch`:

   ```elisp
   (defun workspace-switch (name)
     "Select the tab for workspace NAME.

   Signals `user-error' if NAME is in a broken state (its :home no
   longer exists on disk). Use `workspace-re-anchor' to point it at a
   new path or `workspace-purge' to remove the registry entry."
     (interactive
      (list (completing-read
             "Switch to workspace: "
             (seq-filter #'workspace--tab-for (workspace--registered-names))
             nil t)))
     (let ((ws (gethash name workspace--registry)))
       (when (and ws (workspace--broken-p ws))
         (user-error
          "Workspace %s is broken: :home %s no longer exists. Use `workspace-re-anchor' or `workspace-purge'."
          name (workspace--home ws))))
     (let ((idx (workspace--tab-index-for name)))
       (unless idx
         (user-error "No tab found for workspace %s" name))
       (tab-bar-select-tab idx)))
   ```

2. In `persistence.org`, add the same guard at the top of
   `workspace-restore` (locate the existing command, which handles
   live-tab-already-exists and create-new-tab branches):

   ```elisp
   (let ((ws (gethash name workspace--registry)))
     (when (and ws (workspace--broken-p ws))
       (user-error
        "Workspace %s is broken: :home %s no longer exists. Use `workspace-re-anchor' or `workspace-purge'."
        name (workspace--home ws))))
   ```

   Insert near the top of `workspace-restore`, after the
   `unless (gethash name ...)` "unknown name" guard.

3. In `workspaces.org`, add `workspace-re-anchor`:

   ```elisp
   (defun workspace-re-anchor (name new-home)
     "Point broken workspace NAME at NEW-HOME on the filesystem.
   Clears the broken-state tag and updates `:home'. If
   `(file-name-nondirectory (directory-file-name NEW-HOME))' differs
   from NAME, the registry entry is renamed to match the new basename
   (per the registry-name-equals-basename invariant).

   The new path must exist and must be a directory."
     (interactive
      (list (completing-read "Re-anchor workspace: "
                             (workspace--registered-names) nil t)
            (file-name-as-directory
             (read-directory-name "New home directory: " nil nil t))))
     (let ((ws (gethash name workspace--registry)))
       (unless ws
         (user-error "No workspace named %s" name))
       (unless (file-directory-p new-home)
         (user-error "Not a directory: %s" new-home))
       (let* ((new-name (file-name-nondirectory
                         (directory-file-name new-home)))
              (next-ws (workspace--clear-broken
                        (workspace--set-home ws new-home))))
         ;; If basename changed, rename the registry key too.
         (cond
          ((string= name new-name)
           (puthash name next-ws workspace--registry))
          (t
           (when (gethash new-name workspace--registry)
             (user-error
              "Cannot rename to %s — a workspace with that name already exists"
              new-name))
           (remhash name workspace--registry)
           (puthash new-name next-ws workspace--registry)
           ;; Update tab label if a live tab exists.
           (let ((tab-idx (workspace--tab-index-for name)))
             (when tab-idx
               (tab-bar-rename-tab new-name (1+ tab-idx))))))
         (workspace--persistence-save)
         (message "Workspace re-anchored: %s → %s" name new-home))))
   ```

4. Add the binding to the workspaces keymap:

   ```elisp
   (define-key workspace-prefix-map (kbd "R") #'workspace-re-anchor)
   ```

5. Create `test/broken-home-runtime-spec.el`:

   - **`workspace-switch` refuses broken**: build a registry entry
     and mark it broken via `workspace--mark-broken`. Create a tab
     (or skip the tab requirement by also stubbing
     `workspace--tab-index-for` to a number — the broken-state guard
     fires before the tab lookup). Invoke `(workspace-switch
     "broken-ws")`. Assert: `user-error` mentioning the missing path
     and naming both remediation commands. Assert no tab selection
     occurred.

   - **`workspace-restore` refuses broken**: similar setup; invoke
     `workspace-restore` on the broken name; assert the same
     `user-error` and no new tab created.

   - **`workspace-re-anchor` happy path (same basename)**: setup
     broken workspace `foo` with `:home /tmp/old-foo/` (gone). Create
     `/tmp/new-foo/` and `/tmp/foo/`. Stub `read-directory-name` to
     return `/tmp/foo/`. Invoke `(workspace-re-anchor "foo")`.
     Assert: registry contains `foo` with `:home` `/tmp/foo/`;
     broken tag cleared; persistence file flushed.

   - **`workspace-re-anchor` rename basename**: broken workspace
     `foo`. Stub `read-directory-name` to return `/tmp/renamed/`.
     Assert registry key is now `renamed` (not `foo`); old key removed.

   - **`workspace-re-anchor` rename collision**: pre-populate `foo`
     (broken) and `bar` (healthy). Try to re-anchor `foo` to a path
     basenamed `bar`. Assert `user-error` and registry unchanged.

   - **`workspace-re-anchor` non-existent target**: stub
     `read-directory-name` to return a path that does not exist.
     Assert `user-error` and registry unchanged.

6. Tangle and test:
   ```bash
   ./bin/tangle-org.sh config/workspaces/tabs.org
   ./bin/tangle-org.sh config/workspaces/persistence.org
   ./bin/tangle-org.sh config/workspaces/workspaces.org
   ./bin/run-tests.sh -d config/workspaces -p broken-home-runtime-spec
   ```

## Design rationale

Broken-state is a runtime tag (per D5/D6): set on load when `:home`
does not exist, cleared on `workspace-re-anchor`, never persisted.
Activation guards fail fast — better a clear error than a silently
mis-restored workspace pointing at the wrong files.

The rename-on-re-anchor branch upholds the registry-name-equals-basename
invariant from the spec. Otherwise a user could re-anchor `foo` at
`/tmp/bar/` and end up with registry entry `foo` whose `:home`
basename is `bar` — confusing and inconsistent.

The "rename collision" guard prevents accidentally clobbering a
different existing workspace by re-anchoring through its name.

## Design pattern

Error-message-naming-remediation: both guards name the two recovery
commands (`workspace-re-anchor` and `workspace-purge`). This is good
ergonomics — the user sees what's wrong AND how to fix it without
opening the manual.

`tab-bar-rename-tab` is destructive on the index; pair it with the
registry update so the user-visible state is consistent.

## Verification

- Tangle: all three .org files
- Spec: `./bin/run-tests.sh -d config/workspaces -p broken-home-runtime-spec`
- Manual:
  - Create workspace `test-broken` via `workspace-new`.
  - From a shell: `rm -rf ~/emacs-workspaces/test-broken/`.
  - Restart Emacs. Verify `*Messages*` notice naming `test-broken`
    and the missing path.
  - `M-x workspace-switch test-broken` → assert user-error with
    remediation commands named.
  - `M-x workspace-re-anchor test-broken` → pick a fresh dir → verify
    re-anchoring works.

## Context

design.md § Decisions / D5 — broken tag is runtime-only
design.md § Decisions / D6 — broken-state semantics in commands
design.md § Decisions / D10 — Key bindings (C-x w R)
specs/workspaces/spec.md § ADDED "Broken home directory tolerated on restore"
specs/workspaces/spec.md § ADDED "Required home directory and identity coupling" (basename invariant for re-anchor renames)


## Cycle 2 updates (cycle-20260525-213500)

### Status

- `status: blocked` → `status: ready`. Blocker `persistence-schema-v3`
  closed at merge `f21f592`. The persistence-side broken-tag work is
  live: `workspace--persistence-deserialize-state` now sets `:broken`
  when `(file-directory-p :home)` is nil and logs to `*Messages*`. Your
  task's job is the COMMAND-LAYER guards (workspace-switch /
  workspace-restore refuse; workspace-re-anchor / workspace-purge /
  workspace-delete permit) plus the user-facing `workspace-re-anchor`
  command — the persistence half is done.

### Cycle-2 register-diff hits relevant to this task

- `register/invariant/broken-tag-runtime-only`: speculated →
  **reconciled**. Mutations of `:broken` now ALWAYS route through
  `workspace--mark-broken` / `workspace--clear-broken` (cycle 1) and
  the parallel `workspace--mark-restore-pending` /
  `workspace--clear-restore-pending` (cycle 2). Your
  `workspace-re-anchor` implementation MUST use
  `workspace--clear-broken` on success — never inline `(plist-put ws
  :broken nil)`. Cycle-2 architect finding
  `arch-cycle-20260525-213500-03` formalises this contract for ALL
  runtime-only tags. See
  `.orchestrator/cycles/cycle-20260525-213500/reconciliations/invariant-broken-tag-runtime-only.md`.

- `register/invariant/home-required-no-floating-workspaces`:
  re-confirmed cycle 2. **NEW WORK ABSORBED HERE**: architect finding
  `arch-cycle-20260525-213500-04` (advisory, invariant-gap) flagged
  that the invariant requires `:home` be absolute, but no code path
  currently enforces `(file-name-absolute-p home)`. Add an `it`
  clause to your activation-guard spec asserting that a workspace
  whose `:home` is a relative path is treated as broken (or rejected
  at workspace--make / deserializer with a notice). Extend the
  deserializer filter at `persistence.el:83-87` with a
  `(not (file-name-absolute-p home))` arm emitting:
  ```
  "Workspaces: skipping persisted entry %S — :home %S is not absolute"
  ```
  Optionally add a defensive `cl-check-type` or `error` in
  `workspace--make` so the constructor enforces it at every entry
  point. See finding file at
  `.orchestrator/cycles/cycle-20260525-213500/findings/arch-cycle-20260525-213500-04.md`.

- `register/shape/workspace-plist-v3`: re-confirmed cycle 2 (now
  enumerates `:restore-pending` alongside `:broken` per finding 01).
  Your guards consult `workspace--broken-p`; the new
  `workspace--restore-pending-p` predicate is also available but is
  consumed by the activation-restore hook, not by your re-anchor
  command.

### Cycle-2 inline-fix hits

The persistence-side deserializer was inline-fixed at commit
`cd1d721` to route `:restore-pending` through helpers. Your task
does not touch the deserializer directly, but if you read it for
reference your inspiration patterns now show `(workspace--mark-broken ws)`
and `(workspace--mark-restore-pending ws)` instead of inline
`plist-put`.

## Observations

- Task body's example code for `workspace-re-anchor` placed the
  tab-rename branch's `(workspace--tab-index-for name)` lookup AFTER
  `remhash`. The lookup walks `frame-parameter 'tabs` but qualifies
  matches by `(gethash name workspace--registry)` for ownership — so
  once the old key has been removed the index lookup returns nil and
  the live tab is never relabeled. Reproduced once empirically (one
  test failure on the live-tab-rename arm of the rename-basename spec),
  fixed by capturing `tab-idx` before mutating the registry. This is a
  small drift between the task body's prescriptive example and the
  actual implementation contract of `workspace--tab-index-for`;
  reviewer should confirm the ordering.

- Task body also wrote `(tab-bar-rename-tab new-name (1+ tab-idx))`.
  `workspace--tab-index-for` returns a 1-based index already (its
  docstring says so), and Emacs's `tab-bar-rename-tab` expects a
  1-based tab number. Passing `(1+ tab-idx)` would target the wrong
  tab. Used `tab-idx` directly; matches existing call sites
  (`tabs.org` line 157, `persistence.org` line 528 use
  `workspace--tab-index-for` directly with `tab-bar-select-tab`).

- The keybinding for `C-x w R` already existed and pointed at
  `workspace-switch-to-recent-layout`. Per design.md §D10 the spot
  goes to `workspace-re-anchor`; relocated the previous occupant to
  `C-x w T` ("to recent") to avoid losing the affordance silently.
  Updated both the keybindings table and the `global-set-key` block
  in `workspaces.org`. Reviewer should sanity-check that the
  displacement does not violate any spec assertion about the old
  binding; I did not find one in the change's specs.

- `workspaces.org` is a loader, not a command home. Per task body the
  new `workspace-re-anchor` defun lives there. Placed it AFTER the
  `Submodules` section so all dependencies (`workspace--registry`,
  `workspace--clear-broken`, `workspace--set-home`,
  `workspace--tab-index-for`, `workspace--flush-state`) are loaded
  by the time the defun's body runs. The defun itself is just a
  definition at load time; only its body executes at call time.

- The test for the rename-basename branch intentionally relies on
  `workspace--name` continuing to hold the OLD name after the
  rename — the implementation does NOT rewrite the `:name` slot of
  the workspace plist on re-anchor; only the registry KEY changes.
  This matches the design's distinction between "registry name"
  (the key) and the plist `:name` slot. Worth noting because a
  future refactor that propagates the rename into the plist would
  change this test's assertion; the test documents the current
  contract.

- Cycle-2 architect finding `arch-cycle-20260525-213500-04` flagged
  the absolute-path gap on
  `register/invariant/home-required-no-floating-workspaces`.
  Absorbed the deserializer arm here (with a spec) but did NOT add
  a defensive `cl-check-type` / `error` in `workspace--make`. The
  task body listed that as optional; chose not to add it because
  there are still in-tree call sites synthesising paths through
  `wire-home-into-callsites--synthesize-home`
  (`tabs.org:135-143`) and `scaffold.el` whose contracts I did
  not want to tighten in a parallel cycle. Recommend a follow-up
  to enforce at the constructor once cycle-3's
  `workspace-new-default-path` task lands.

## Discoveries

- discovery_id: disc-broken-home-tolerance-1
  class: invariant-gap
  description: |
    `register/invariant/home-required-no-floating-workspaces`
    requires that every workspace's `:home` be an absolute filesystem
    path. The previous deserializer enforced `(file-directory-p
    :home)` (the broken-state arm) but did NOT enforce
    `(file-name-absolute-p :home)`. This task added the
    relative-path skip arm to `workspace--deserialize-state` with a
    notice `"Workspaces: skipping persisted entry %S — :home %S is
    not absolute"` and a spec it-clause that asserts the entry is
    not loaded into the registry. The absolute-path requirement is
    now structurally enforced at the persistence boundary.
    `workspace--make` still does NOT defensively assert absolute
    paths; see Observations for the rationale and the
    recommended follow-up.
  affected_register_entry: register/invariant/home-required-no-floating-workspaces
  recommendation: |
    Reconcile entry: status confirmed → reconciled. Add the
    deserializer absolute-path arm to the enforcement
    locus list. Open a follow-up task (cycle-3 or later) to add
    a constructor-side `cl-check-type` / `error` in
    `workspace--make` so the invariant has two
    independent enforcement points (persistence + constructor),
    matching the broken-tag pattern.

- discovery_id: disc-broken-home-tolerance-2
  class: interface-drift
  description: |
    `design.md` §D5 and §D6 use the name `workspace--home-broken-p`
    for the predicate (cited verbatim in cycle-1 open ask
    `ask-cycle-20260525-200459-1`). The actual implementation
    (and `register/shape/workspace-plist-v3`,
    `register/vocabulary/workspace-broken-disposition`,
    `register/invariant/broken-tag-runtime-only`) uses
    `workspace--broken-p`. Implementor used `workspace--broken-p`
    (matches code + register; the cycle-1 ask resolved by name in
    favor of the register). design.md is the only stale reference.
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    No register reconciliation needed — register already uses
    the correct name. Integrate phase: open a `.tasks/`
    follow-up to update design.md §D5/§D6 to reference
    `workspace--broken-p`. Out-of-scope for this task per the
    brief ("do not modify design.md").

- discovery_id: disc-broken-home-tolerance-3
  class: deviation
  description: |
    Task body's `workspace-re-anchor` example listed the registry
    rename in the wrong order: it called `(remhash name
    workspace--registry)` BEFORE looking up
    `(workspace--tab-index-for name)`. Since
    `workspace--tab-index-for` qualifies matches by
    `(gethash name workspace--registry)` for ownership, the lookup
    after `remhash` returns nil and the live tab is never
    relabeled. Implementor captured `tab-idx` BEFORE the mutation;
    a buttercup test on the live-tab-rename arm reproduced the
    failure once, then passed after the fix.

    Also: task body's example wrote
    `(tab-bar-rename-tab new-name (1+ tab-idx))`. The
    `workspace--tab-index-for` helper returns a 1-based index
    already and `tab-bar-rename-tab` expects a 1-based number, so
    `(1+ tab-idx)` targets the wrong tab. Used `tab-idx` directly;
    consistent with all other call sites in
    `tabs.org` / `persistence.org`.
  affected_register_entry: register/invariant/registry-name-equals-basename
  recommendation: |
    No register entry update needed — the invariant itself is
    intact (registry key DOES equal `:home` basename after the
    rename). The discoveries are about prescription accuracy in
    the task body, not about the contract. Integrate phase may
    want to update future task templates to flag the
    tab-rename ordering and the tab-number argument convention.

- discovery_id: disc-broken-home-tolerance-4
  class: scope-question
  description: |
    The keybinding `C-x w R` was previously bound to
    `workspace-switch-to-recent-layout` (existing) and is reassigned
    here to `workspace-re-anchor` per design.md §D10. The previous
    occupant was relocated to `C-x w T` to avoid losing the
    affordance silently. design.md §D10 does NOT specify what to
    do with the displaced binding; this is a local decision. No
    spec or register entry references `C-x w R` or
    `workspace-switch-to-recent-layout` as a load-bearing pair.
  recommendation: |
    Reviewer should confirm the relocation (`R` → `T` for the
    displaced command) is acceptable and is documented adequately
    in `workspaces.org`'s keybindings note. If not acceptable,
    options include dropping the displaced binding entirely or
    finding another letter; no test will catch the choice.
