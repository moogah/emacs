---
name: add-home-slot-to-data-model
description: Add :home slot, accessor, and broken-state runtime tag to data-model.org
change: add-workspace-home-directory
status: ready
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
