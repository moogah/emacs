---
name: persistence-schema-v3
description: Bump schema constant to 3; reject v2 with notice; serialize/deserialize :home; tag missing-dir entries broken
change: add-workspace-home-directory
status: ready
relations:
  - blocked-by:add-home-slot-to-data-model
---

## Files to modify
- `config/workspaces/persistence.org` (modify)
- `config/workspaces/test/persistence-v3-spec.el` (new — Buttercup)
- `config/workspaces/test/broken-home-load-spec.el` (new — Buttercup)

## Implementation steps

1. In `persistence.org`, locate the schema version constant (search
   for `workspace--persistence-schema-version` or the literal `2`
   inside a `defconst`) and change its value from `2` to `3`.

2. Locate the reader's version-rejection gate. It currently rejects
   anything that is not `2`. Update it to reject anything that is not
   `3`. The `*Messages*` notice text should reference v2 explicitly
   (e.g., "workspaces persistence file at %s is schema v%d; v3
   required — file ignored. Delete the file to start fresh.").

3. Update the serializer to write `:home` for each workspace plist.
   If the in-memory plist has a `:broken` tag, the serializer SHALL
   NOT write it to disk (broken is runtime-only per design D5).
   Filter the plist before writing — easiest approach:

   ```elisp
   (defun workspace--persistence-serialize-workspace (ws)
     "Return WS reduced to its persistable form."
     (list :name (workspace--name ws)
           :home (workspace--home ws)
           :recent-layout-group (workspace--recent-group ws)
           :buffer-files (workspace--buffer-files ws)
           :layout-groups (workspace--layout-groups ws)))
   ```

   (Adjust per the actual serializer location; this is the shape.)

4. Update the deserializer to:
   - **Skip workspaces lacking `:home`**: emit a `*Messages*` notice
     naming the malformed entry; do NOT add to registry; continue with
     remaining entries.
   - **Tag broken-home workspaces**: for each entry with `:home` set
     where `(file-directory-p :home)` returns nil, emit a `*Messages*`
     notice naming the workspace name and missing path, and call
     `workspace--mark-broken` on the deserialized plist before
     inserting into the registry.

5. Create `test/persistence-v3-spec.el` with cases. Use the temp-dir
   helper pattern (inline `make-temp-file (dir) t` /
   `unwind-protect delete-directory` if no shared helper exists yet).
   Stub `workspace--persistence-file-path` (or its equivalent) via
   `cl-letf` to point at a tmp file.

   - Round-trip a single workspace with `:home` set: serialize to
     tmp file, deserialize, registry contains the workspace with
     `:home` intact.
   - v2 file rejection: write a file with `(:version 2 ...)` content;
     loader emits a notice and the registry stays empty. Assert via
     `with-current-buffer "*Messages*"` content check.
   - v1 file rejection: same as v2.
   - Workspace without `:home` in a v3 file: that entry is skipped
     (notice + continue); a sibling entry with `:home` IS loaded.
   - `:broken` round-trip: a workspace marked broken in memory should
     NOT have `:broken` in the written file. Read the file back as
     plain elisp and assert.

6. Create `test/broken-home-load-spec.el` exercising broken detection:
   - Set up a tmp registry file with one workspace whose `:home`
     points at a path that does not exist.
   - Load. Assert: registry contains the entry; the entry's
     `workspace--broken-p` is t; the missing path was named in the
     `*Messages*` buffer; no directory was created (assert
     `(file-exists-p missing-path)` is still nil after load).

7. Tangle and run tests:
   ```bash
   ./bin/tangle-org.sh config/workspaces/persistence.org
   ./bin/run-tests.sh -d config/workspaces -p persistence-v3-spec
   ./bin/run-tests.sh -d config/workspaces -p broken-home-load-spec
   ```

## Design rationale

Schema-v3 is a deliberate breaking change with no migration code — the
package is pre-alpha and the v1→v2 cutover already established the
rejection-with-notice pattern. v3 inherits the same machinery: a single
`(unless (= ver EXPECTED) (notice-and-return-nil))` gate.

`:home` is required at the data-model level, so a deserialized entry
without `:home` is data-corruption and gets skipped (not auto-defaulted
— that would mask user errors). Other entries in the same file are
still loaded; one bad entry shouldn't tank the whole session.

The `:broken` tag is runtime-only (design D5) because it represents an
observation made at load time. Persisting it would mean re-thinking
when to clear it. Keeping it ephemeral means the next load
re-evaluates afresh — desirable, because the user may restore the
directory between sessions.

The "skip lacking-`:home` / mark broken on missing dir" split is
deliberate:
- **Missing `:home` slot** is structural corruption (the writer should
  have produced one). Skip.
- **`:home` present but dir gone** is the expected user-action case
  (they `rm -rf`'d it or moved it). Mark broken; let the user re-anchor
  or purge.

## Design pattern

For temp-buffer test setup, the existing `persistence.org` likely uses
a `workspace--persistence-file-path` defcustom or function. Override it
in tests via `cl-letf` scoped to the `describe`/`it` block. Pattern:

```elisp
(it "round-trips :home"
  (workspace-test--with-tmp-home (tmp)
    (let ((registry-file (expand-file-name "registry.eld" tmp))
          (workspace--registry (copy-hash-table workspace--registry)))
      (cl-letf (((symbol-function 'workspace--persistence-file-path)
                 (lambda () registry-file)))
        ;; ... test body
        ))))
```

`*Messages*` content assertion pattern:

```elisp
(expect (with-current-buffer "*Messages*"
          (buffer-substring-no-properties (point-min) (point-max)))
        :to-match "v2.*required")
```

## Verification

- Tangle: `./bin/tangle-org.sh config/workspaces/persistence.org`
- New specs pass: `./bin/run-tests.sh -d config/workspaces -p persistence-v3-spec` and `-p broken-home-load-spec`
- Existing persistence tests: `./bin/run-tests.sh -d config/workspaces`
  (any reading-v2 tests need updating to write v3 fixtures; do that as
  part of this task)
- Manual: nuke `~/.emacs.d/state/workspaces-*.eld` and start Emacs;
  verify clean startup with no error, no auto-recreated entries.

## Context

design.md § Decisions / D5 — Persistence schema v3: slot addition, same file path
specs/workspaces/spec.md § MODIFIED "Per-machine persistence and restoration" (v2 rejection, `:home` skip, broken tagging)
specs/workspaces/spec.md § ADDED "Broken home directory tolerated on restore"

## Cycle 1 updates (cycle-20260525-200459)

### Cited register entries

- `register/shape/workspace-plist-v3`: speculated → **confirmed**. The
  data-model task introduced the `workspace--make(name home)`
  signature, `:home` slot, and `:broken` runtime tag. The serializer
  this task adds MUST filter out `:broken` before writing — the
  shape entry's contract names `:broken` as runtime-only,
  never-serialized. See
  `.orchestrator/cycles/cycle-20260525-200459/reconciliations/shape-workspace-plist-v3.md`.
- `register/invariant/home-required-no-floating-workspaces`:
  speculated → **confirmed**. The deserializer's "skip entries
  lacking `:home`" path you implement here is the persistence-side
  enforcement of this invariant (the constructor-side enforcement
  landed cycle 1). See
  `.orchestrator/cycles/cycle-20260525-200459/reconciliations/invariant-home-required-no-floating-workspaces.md`.
- `register/invariant/registry-name-equals-basename`: speculated →
  **confirmed**. When loading a workspace whose serialized `:name`
  disagrees with `basename(:home)`, the deserializer SHOULD prefer
  `basename(:home)` (or flag a corruption); current load path is
  silent on this case — worth a `## Observations` note if you encounter
  the edge.

### Already-shipped infrastructure (use, don't re-implement)

- `workspace--mark-broken` and `workspace--clear-broken` are defined
  in `config/workspaces/data-model.el` (cycle 1) and exported. Use
  `workspace--mark-broken` from the deserializer's broken-home path
  (step 4 above) rather than inlining `(plist-put ws :broken t)`.
  The architect's cycle-1 dead-branch finding
  (`arch-cycle-20260525-200459-2`) explicitly tracks whether this
  task's implementor uses the helper — if you bypass it, the finding
  promotes from informational to advisory in cycle 3's integrate.

### Meta-discovery: cascade risk

Cycle 1's data-model task changed a shared signature
(`workspace--make`) and cascaded into 76 failing specs across the
codebase. A mid-cycle `wire-home-into-callsites` fix closed it.
Your task is structurally similar: changing the persistence schema
version from 2 → 3 is a shared contract change. **Self-audit**
before merging: does any code path read the persistence file with
hardcoded `:version 2` expectations? If yes, fix in this same task
(or flag a `wire-v3-into-callers` follow-up at execute close so
the cycle doesn't ship a red baseline).

### Open ask carried from cycle 1

- `ask-cycle-20260525-200459-1`: design.md §D5/§D6 names the
  predicate `workspace--home-broken-p`, but the register +
  implementation use `workspace--broken-p`. If still open when you
  start this task, treat the register/implementation as authoritative.
  The user disposition lands in the cycle-2 plan handshake.

## Cycle 2 plan stanza (cycle-20260525-213500)

### Status

- `status: blocked` → `status: ready`. All blockers
  (`add-home-slot-to-data-model`) are closed at commit `7026d37`.

### Open ask resolution

- `ask-cycle-20260525-200459-1` (design.md predicate naming drift):
  **resolved** by the user with disposition `apply-now`. Design.md
  lines 78 and 197 were renamed in cycle-1's integrate tail at
  commit `36ae299` (`workspace--home-broken-p` →
  `workspace--broken-p`). The register and implementation were
  already authoritative; the design doc now matches. The
  "treat-register-as-authoritative" guidance above is therefore a
  no-op in cycle 2 — no naming discrepancy remains.

### Net-new cited register entry

In addition to the three cycle-1-confirmed entries already cited
above (`workspace-plist-v3`, `home-required-no-floating-workspaces`,
`registry-name-equals-basename`), cycle 2 adds:

- `register/invariant/broken-tag-runtime-only`: **speculated**. The
  contract that the persistence serializer filters `:broken` from
  each workspace plist before writing to disk, and that the
  deserializer re-derives the tag freshly via `(file-directory-p
  :home)`. Your task's step 3 ("`:broken` runtime-only — filter
  before writing") IS the implementation of this invariant.
  Scaffold at
  `openspec/changes/add-workspace-home-directory/scaffolding/invariants/broken-tag-runtime-only.el`
  has 4 `it` cases — the round-trip omission case, the load-side
  re-derivation case, the byte-equivalence round-trip case, and a
  structural lint asserting `:broken` does not appear in any
  serialize/save/write function body. Lift the assertion shapes
  into `persistence-v3-spec.el` (and `broken-home-load-spec.el`
  for the load-side case); the scaffold file is reference, not
  authority — push back in `## Discoveries` if a shape doesn't fit
  the actual serializer factoring.

### Cascade self-audit (per cycle-1 meta-discovery)

Cycle-1's `add-home-slot-to-data-model` cascaded into 76 failing
specs because it changed the shared `workspace--make` signature.
Schema-v3 is a structurally analogous contract change. Before
opening a PR / declaring the task done:

1. Grep the codebase for any code reading the persistence file
   that hardcodes `:version 2` or otherwise assumes v2 layout.
   Suggested commands:
   ```bash
   grep -rn "version" config/workspaces/ | grep -E ":version|persistence-schema|schema-version"
   grep -rn "workspace--persistence-schema-version" config/
   ```
2. Run the full workspaces suite (`./bin/run-tests.sh -d
   config/workspaces`); a v3-rejecting v2 fixture in an existing
   spec is expected — update it as part of THIS task (see step 7
   notes in your verification block).
3. If you find a non-test code path that needs adaptation but
   feels outside this task's scope, flag a follow-up named
   `wire-v3-into-callers` in your `## Observations` instead of
   shipping a red baseline.

### Already-shipped infrastructure (re-stated, no change from cycle 1)

`workspace--mark-broken` and `workspace--clear-broken` exist in
`config/workspaces/data-model.el`. Use `workspace--mark-broken` in
your deserializer's broken-home path (step 4 above) rather than
inlining `(plist-put ws :broken t)`. The architect's
`arch-cycle-20260525-200459-2` informational finding tracks whether
this happens — using the helper closes the finding; bypassing it
promotes the finding to advisory in cycle-2 integrate.

## Observations

(Reviewer does NOT see this section.)

- **Verification commands in this task's body use `-p` with Buttercup
  spec names** (`./bin/run-tests.sh -d config/workspaces -p persistence-v3-spec`).
  `bin/run-tests.sh -p` is ERT-only and the result is "Ran 0 tests"
  for Buttercup paths. The gate I actually ran is
  `./bin/run-tests.sh -d config/workspaces`, which loads all `*-spec.el`
  files in the tree and runs them under Buttercup. Worth noting in the
  task template — `-p` for Buttercup is silently a no-op rather than
  an error.
- **Constant name divergence between task body and codebase.** The task
  body searches for `workspace--persistence-schema-version`; the actual
  existing constant is `workspace--state-version`. I kept the existing
  name to avoid a churn cascade; the task body's reference is a stale
  alias for the same concept. Worth tightening in the task template if
  the project standardises one or the other.
- **Cycle-1 placeholder behaviour preserved unchanged.**
  `wire-home-into-callsites--synthesize-home` (in `tabs.org`) builds
  `~/emacs-workspaces/<name>/` paths that do NOT exist on disk in test
  environments. After v3 lands, every `workspace-new` followed by
  `workspace--restore` will tag the round-tripped workspace `:broken`.
  This is *correct behaviour by the design contract* (missing-dir →
  broken), but it means the existing `persistence-spec.el` round-trip
  tests now exercise a broken state by accident. No assertion in those
  tests reads `:broken`, so they continue to pass; the cycle-3
  `workspace-new-default-path` task should land a real on-disk default
  to remove this incidental coupling.
- **Adjacent stale doc fixed in scope.** `config/workspaces/docs/README.org`
  asserted `:version 2`; updated to `:version 3` since it described
  user-visible behaviour the v3 cutover changes. Pure prose; no tangle
  required.
- **Existing `persistence-spec.el` "Schema v2 version check" describe
  block renamed to "Schema version check"** plus a leading comment
  pointing at the v3 spec files for new behavioural coverage. The
  block's three `it` cases (constant equality, v1 reject, v99 reject)
  continue to hold under v3 unchanged. The `:to-match ":version"`
  assertion in the v1-reject test still passes because the new
  v3-rejection message contains the literal "schema :version" prefix.
- **Restore path's broken handling is deliberately limited to
  registration.** When a `:broken` workspace is later activated via
  the tab-switch hook (`workspace--persistence-after-tab-switch` →
  `workspace--activate-pending-workspace`), the existing call chain
  proceeds to `workspace--apply-saved-layout`, which is a no-op when
  no saved layout state exists. The data layer carries the
  `:broken` flag; the command-layer guards
  (register/vocabulary/workspace-broken-disposition) for
  `workspace-switch`/`workspace-restore` are NOT in this task's scope
  (they belong to a later task). For now, a broken workspace's tab
  CAN be selected and the user will see whatever `*scratch*` content
  the default applies, which is non-destructive. Worth a follow-up
  task `wire-broken-guards-into-commands` if the integrate phase
  agrees.
- **`workspace--deserialize-state` return value changed.** Previously
  it returned the raw `ws-list` (every entry, including unhealthy
  ones that would have been silently registered). It now returns
  only the workspaces actually inserted into the registry (skipped
  entries are excluded). The caller in `workspace--restore` ignores
  the return value, so this is internally consistent; if a future
  caller relies on "every plist as it appeared on disk," they will
  see a behaviour change. Tests pin only the registry side, not the
  return.

## Discoveries

(Reviewer does NOT see this section. Integrate phase reads for
register reconciliation.)

- **discovery_id**: persistence-schema-v3-d1
  **class**: vocabulary-mismatch
  **description**: The task body refers to the schema-version constant
  as `workspace--persistence-schema-version`, but the actual constant
  in `persistence.org` (introduced at the v1→v2 cutover) is
  `workspace--state-version`. I kept the existing name; the task
  template's reference is informational, not authoritative.
  **affected_register_entry**: register/shape/workspace-plist-v3 (no
  change recommended — the constant name is implementation detail not
  pinned in the entry's contract).
  **recommendation**: Update the task template / design.md cross-
  reference to use `workspace--state-version` when this change
  archives.

- **discovery_id**: persistence-schema-v3-d2
  **class**: invariant-gap
  **description**: The deserializer's broken-home detection sets the
  `:broken` tag at load time. Command-layer guards
  (`workspace-switch`/`workspace-restore` refusing to operate on broken
  workspaces per register/vocabulary/workspace-broken-disposition) are
  NOT wired up yet. A user who restores a session with a broken
  workspace can still `workspace-switch` to its tab, and
  `workspace--apply-saved-layout` will silently no-op. The data is
  marked correctly; the command-side enforcement is the gap.
  **affected_register_entry**: register/vocabulary/workspace-broken-disposition
  (status remains speculated for the command-side; the persistence-side
  enforcement is implemented by this task).
  **recommendation**: Open a follow-up task
  `wire-broken-guards-into-commands` so cycle 3 can pick it up. Not
  filed as a user-visible-bug task because the failure mode is silent
  (no-op), not destructive.

- **discovery_id**: persistence-schema-v3-d3
  **class**: scope-question
  **description**: Scaffold
  `openspec/changes/add-workspace-home-directory/scaffolding/invariants/broken-tag-runtime-only.el`
  included a "structural lint" `it` case asserting the persistence
  writer's defun body does not contain `:broken`. I did NOT lift that
  shape into the spec because the writer is now factored as
  `workspace--persistence-serialize-workspace`, which explicitly
  enumerates the persistable keys (`:name`, `:home`,
  `:recent-layout-group`, `:buffer-files`, `:layout-groups`) — the
  omission of `:broken` is structural-by-construction rather than
  defensive-by-filter. The byte-equivalence test in
  `persistence-v3-spec.el` ("serializing a workspace twice (once clean,
  once :broken) yields identical bytes") is the stronger pin: any
  drift would surface as a byte diff. The grep-the-defun-body lint is
  redundant given the construction shape, and would tie the spec to
  the current function name (rename-fragile).
  **affected_register_entry**: register/invariant/broken-tag-runtime-only
  (status: speculated → confirmed at integrate; the byte-equivalence
  pin is the load-bearing test).
  **recommendation**: Confirm the entry at integrate; revise the
  scaffold's lint test or drop it in favour of the byte-equivalence
  pin in the eventual `openspec/specs/workspaces/spec.md` reference.

- **discovery_id**: persistence-schema-v3-d4
  **class**: interface-drift
  **description**: `workspace--deserialize-state` previously returned
  the raw `ws-list` (every entry on disk). The v3 implementation
  returns only the workspaces actually inserted into the registry
  (skipped-for-missing-`:home` entries are excluded). The sole caller
  (`workspace--restore`) ignores the return value, so this is
  internally consistent, but the function's contract has narrowed.
  **affected_register_entry**: none directly — the entry covers the
  invariant ("skip entries lacking :home") but not the function's
  return-value contract.
  **recommendation**: If a register entry is created for the deserializer's
  signature in a future cycle, pin the return as "registered subset"
  rather than "all entries seen on disk".
