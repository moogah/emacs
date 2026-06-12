---
name: add-home-slot-to-data-model
description: Add :home slot, accessor, and broken-state runtime tag to data-model.org
change: add-workspace-home-directory
status: done
relations: []
---

## Files to modify
- `config/workspaces/data-model.org` (modify)
- `config/workspaces/test/data-model-home-spec.el` (new — Buttercup)

## Implementation steps

1. In `data-model.org`, locate the `workspace--make` helper (currently
   `(list :name name :recent-layout-group nil :buffer-files nil
   :layout-groups nil)`) and change its signature to
   `(workspace--make name home)`. The resulting plist gains
   `:home HOME` (absolute path string). Update the docstring to note
   that `:home` is required and is the workspace's filesystem anchor.

2. Add accessors:

   ```elisp
   (defun workspace--home (ws)
     "Return the absolute home directory path of workspace WS."
     (plist-get ws :home))

   (defun workspace--set-home (ws home)
     "Return a new workspace like WS with :home set to HOME.
   Non-destructive."
     (let ((copy (copy-sequence ws)))
       (plist-put copy :home home)))
   ```

3. Add the runtime-only broken-state tag (NOT serialized — handled by
   `persistence.org` in task `persistence-schema-v3`):

   ```elisp
   (defun workspace--broken-p (ws)
     "Return non-nil if WS is in a broken state (its :home does not exist)."
     (plist-get ws :broken))

   (defun workspace--mark-broken (ws)
     "Return a new workspace like WS marked broken. Non-destructive."
     (let ((copy (copy-sequence ws)))
       (plist-put copy :broken t)))

   (defun workspace--clear-broken (ws)
     "Return a new workspace like WS with the broken tag cleared.
   Non-destructive."
     (let ((copy (copy-sequence ws)))
       (plist-put copy :broken nil)))
   ```

4. Create `test/data-model-home-spec.el` exercising:
   - `(workspace--make "foo" "/tmp/foo")` produces a plist with both
     `:name` and `:home` set.
   - `(workspace--home ws)` returns the path.
   - `(workspace--set-home ws path)` returns a new ws with updated
     `:home` and leaves the original unchanged.
   - `(workspace--broken-p ws)` is nil by default, t after
     `(workspace--mark-broken ws)`, nil after
     `(workspace--clear-broken ws)`.
   - Non-destructive guarantees: assert the original ws is unchanged
     after each mutating helper.

5. Run `./bin/tangle-org.sh config/workspaces/data-model.org` and fix
   any `check-parens` issues.

6. The existing `workspace--make` is called from `tabs.org` at
   workspace creation. That call site WILL break until task
   `workspace-new-default-path` updates it; for this task, ensure the
   data-model itself is correct and well-tested. Note in the commit
   message that downstream call sites are updated in dependent tasks.

## Design rationale

`:home` is the foundational data-model change for the entire feature.
Every other task either reads or writes this slot. Adding it as a
required positional argument (rather than an optional keyword) forces
all call sites to be updated and makes "floating workspaces" (the
removed concept) structurally impossible.

The `:broken` tag lives at the runtime layer because it represents an
ephemeral observation ("the directory was missing when we loaded this
entry"), not a persistent property. Keeping it out of the serialized
form means the persistence file stays clean and "broken" gets
re-evaluated on every load — desirable, because the user may have
restored the directory between sessions.

The data-model module is the side-effect-free layer
(`data-model.org` has no file or process I/O); `:home` is just a string,
no validation here. Validation (does the dir exist?) lives in
persistence.org's loader (task `persistence-schema-v3`) and in
`scaffold.el` (task `scaffold-module`).

## Design pattern

Follow the existing pure-helper pattern in `data-model.org`: every
mutating helper returns a *new* plist via `copy-sequence` +
`plist-put`. Never destructively modify input. See the existing
`workspace--set-recent-group`, `workspace--upsert-group`, etc., for the
pattern.

Buttercup spec style: top-level `(describe "workspace-data-model" ...)`
matching the production module name; inner `describe` blocks per
concept (`":home accessor"`, `"broken-state tag"`); `it` clauses in
user-facing prose.

## Verification

- Tangle succeeds: `./bin/tangle-org.sh config/workspaces/data-model.org`
- New spec passes: `./bin/run-tests.sh -d config/workspaces -p data-model-home-spec`
- Existing data-model behavior unchanged: `./bin/run-tests.sh -d config/workspaces`
  (other tests may fail because `workspace--make` signature changed; that
  is expected and resolved by dependent tasks — note this in the PR/commit)

## Context

design.md § Decisions / D1 — module layout (data-model.org changes)
design.md § Decisions / D5 — `:broken` tag is runtime-only
specs/workspaces/spec.md § ADDED "Required home directory and identity coupling"
specs/workspaces/spec.md § ADDED "Broken home directory tolerated on restore"

## Observations

- Followed the task body exactly: `workspace--make` is now
  `(name home)`, the docstring notes both args are required and `:home`
  is the filesystem anchor; added `workspace--home`, `workspace--set-home`,
  `workspace--broken-p`, `workspace--mark-broken`, `workspace--clear-broken`.
  All five mutating helpers use the `(copy-sequence ws)` + `plist-put`
  pattern already established by `workspace--set-recent-group`.
- New spec file `config/workspaces/test/data-model-home-spec.el` adds
  15 specs grouped as `:home accessor`, `name-equals-basename
  construction contract`, and `broken-state runtime tag`. All 15 pass.
- Existing data-model-spec.el and other suites have 76 failing specs
  that all trace to a single root cause: `workspace--make` is called
  with one positional arg. The production call site is
  `config/workspaces/tabs.org` line 145 (`(workspace--make name)`); the
  task body and design.md §D6 / workspace-new-default-path explicitly
  defer fixing that to cycle 2+. Existing test files
  (`data-model-spec.el`, `home-spec.el`, `tabs-spec.el`, etc.) likewise
  exercise the one-arg shape — they will need the same update in their
  fixtures when downstream tasks land.
- The defensive-accessor scaffold clause from
  `home-required-no-floating-workspaces.el` (the 5th `it` clause) is
  covered without bloat: `workspace--home` is just `(plist-get ws :home)`,
  which already returns nil on missing slot. The spec asserts that
  contract directly.
- The 5th `registry-name-equals-basename` scaffold clause (the
  `workspace-re-anchor` rename round-trip) is forward-pinned to cycle 3+
  and was NOT pulled into this task's spec — `workspace-re-anchor` does
  not exist yet.
- The vocabulary dispatcher scaffold
  (`workspace-broken-disposition.el`) was NOT modified. Per the scaffold
  header, the cycle-1 implementor's job is the runtime predicate
  (`workspace--broken-p`), which is in place. The dispatcher is reference
  vocabulary for the end-of-cycle Architect audit and exists outside the
  test runner's discovery path.
- An existing `home-spec.el` already lives in `config/workspaces/test/`
  and exercises a *different* concept — the reserved layout-group name
  `"home"` and the `workspace-home-builder` defcustom. My new spec is
  named `data-model-home-spec.el` to disambiguate (it tests the new
  `:home` slot on the workspace plist, not the `"home"` group name).

## Discoveries

- discovery_id: disc-add-home-slot-1
  class: deviation
  description: |
    The task body's `workspace--set-home` signature only updates `:home`
    and explicitly does not touch `:name`. That matches the principle
    that the data layer is non-policy, but it leaves a sharp edge:
    `workspace--set-home` alone is insufficient to honour
    register/invariant/registry-name-equals-basename when HOME's
    basename differs from the current `:name`. The integrate phase /
    cycle 3 `workspace-re-anchor` implementor needs to remember that
    re-anchoring is a *two-step* dance: `workspace--set-home` plus an
    explicit registry rekey (`remhash`/`puthash`) plus a `:name` update
    (either via a new `workspace--set-name` helper, or by re-constructing
    via `workspace--make`). The current data-model intentionally leaves
    that orchestration to the command layer.
  affected_register_entry: register/invariant/registry-name-equals-basename
  recommendation: |
    Either (a) add a `workspace--set-name` accessor in cycle 2/3 when
    `workspace-re-anchor` lands, mirroring the `workspace--set-home`
    shape, or (b) have `workspace-re-anchor` construct a fresh plist
    via `workspace--make` and re-attach the existing groups/buffer-files.
    Option (b) is more honest because it forces the constructor's name+
    home contract to hold for every workspace ever placed in the
    registry. Note this as an integrate-phase signal.

- discovery_id: disc-add-home-slot-2
  class: interface-drift
  description: |
    `config/workspaces/tabs.org` calls `(workspace--make name)` with a
    single argument. After this task, every code path through
    `workspace-new` and `workspace-switch` errors with
    `wrong-number-of-arguments`. This is by design (the task body and
    `workspace-new-default-path` task acknowledge it), but the breakage
    is total — `workspace-new` is non-functional on the branch until
    `workspace-new-default-path` lands. The orchestrator should sequence
    `workspace-new-default-path` immediately after this task in the same
    integration cycle, or any interactive testing of the branch will be
    blocked.
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    Integrate phase: confirm `workspace-new-default-path` and the
    test-fixture updates for `data-model-spec.el`, `home-spec.el`,
    `tabs-spec.el`, `persistence-spec.el`, `layouts-spec.el`,
    `buffer-membership-spec.el`, `anti-save-spec.el`,
    `save-restore-spec.el`, `buffer-reincarnation-spec.el`,
    `revert-spec.el`, and `workspaces-mode-spec.el` all merge in the
    same cycle as this task. The 76-failing-tests state is intentional
    but should not persist past cycle close.

- discovery_id: disc-add-home-slot-3
  class: vocabulary-mismatch
  description: |
    The task body specifies three accessors for the broken-state tag:
    `workspace--broken-p`, `workspace--mark-broken`, and
    `workspace--clear-broken`. The scaffolded
    `workspace-broken-disposition.el` enumerates two dispositions:
    `:refused` and `:allowed`. There is no `:cleared` or
    `:already-broken` disposition — the `clear-broken` operation has no
    counterpart in the command-vocabulary scaffold, which only catalogs
    *user-facing* commands' responses to a broken workspace.
    `workspace--clear-broken` is internal plumbing called by the
    persistence loader (re-evaluating the broken state on every load,
    per design.md §D5) and by `workspace-re-anchor` (after a successful
    re-anchor). This is not a conflict, just a note: the vocabulary
    register covers *command dispositions*, not internal data-layer
    helpers.
  affected_register_entry: register/vocabulary/workspace-broken-disposition
  recommendation: |
    No register change needed. The scaffolded dispatcher's closed-set
    enforcement (the `_` catch-all error arm) is still the right
    contract — it catches drift in user-facing commands, not internal
    helpers. Worth a one-line comment in the dispatcher noting that
    internal helpers like `workspace--clear-broken` are intentionally
    out-of-scope for this vocabulary.

