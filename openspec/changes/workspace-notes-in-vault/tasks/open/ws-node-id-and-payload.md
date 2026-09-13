---
name: ws-node-id-and-payload
description: Add the opaque :node-id slot to the workspace record, payload, and persistence; delete the sessions-dir helpers and payload key.
change: workspace-notes-in-vault
status: ready
task_class: contract
on_critical_path: true
cycle: cycle-1789334430
relations:
  - enables:ws-scaffold-and-anchor
  - enables:int-workspaces-org-graph
cites_register_entries:
  - register/shape/workspace-plist-v3
  - register/shape/workspace-integration-anchor-payload
  - register/invariant/registry-name-equals-basename
created_at: 2026-09-13T00:00:00Z
planned_at: 2026-09-13T21:20:30Z
---

## Files to modify
- config/workspaces/data-model.org (modify; tangle to data-model.el)
- config/workspaces/integrations.org (modify; tangle)
- config/workspaces/persistence.org (modify; tangle)
- config/workspaces/workspaces.org (modify; tangle — delete `workspace-sessions-dir`)
- config/workspaces/test/data-model-spec.el (modify)
- config/workspaces/test/integration-dispatch-spec.el (modify)
- config/workspaces/test/persistence*-spec.el (modify: round-trip + absent-key specs)
- openspec/specs/workspaces/spec.md, openspec/specs/workspace-integrations/spec.md (read only — the delta specs under this change's `specs/` are the contract)

## Implementation steps
1. In `data-model.org`, extend `workspace--make (name home)` to
   `(name home &optional node-id)`; `cl-check-type node-id (or null string)`;
   emit `:node-id node-id` as a sixth key. Add `workspace-node-id (ws)` and
   `workspace-set-node-id (ws node-id)` (public names, no `--`); the setter
   validates the type and calls the persistence flush (`workspace--persist`
   or whatever the synchronous-flush entry point is named in persistence.org)
   after mutating — it is an identity change, same class as `workspace-new`.
2. Delete `workspace--sessions-dir` (data-model.org ~:429) and
   `workspace-sessions-dir` (workspaces.org ~:425-441) and their prose
   sections. Grep the package for callers: `grep -n 'sessions-dir' config/workspaces/*.org`
   must be empty after this task except the integrations payload line you
   remove in step 3.
3. In `integrations.org`, `workspace--integration-payload` returns
   `(list :name name :home home :node-id (workspace-node-id ws) :context context)`.
   The constructor currently receives `(name home context)`; change it to take
   the workspace record (or look it up by name) so `:node-id` is live — later
   handlers in the same dispatch must see a node-id set by an earlier handler.
4. In `persistence.org`, add `:node-id` to the serialize whitelist
   (~:110-122) and let the deserializer (~:132-190) accept a missing or nil
   `:node-id` silently (no notice). Schema version stays 3. Add
   `workspace-set-node-id` to the list of synchronous flush triggers in the
   prose.
5. Tangle all four files with `./bin/tangle-org.sh`; run the workspaces suite.
6. Specs: `workspace--make` with and without node-id; payload has `:node-id`
   and no `:sessions-dir`; persistence round-trips `"ABC"`; a v3 plist without
   `:node-id` loads with nil and no `*Messages*` notice; the setter flushes
   (spy on the flush function).

## Design rationale
The workspace has no identity beyond `name == basename(home)`; the org ID
stamped into `home.org` was never read back. This change makes a vault
project note the canonical, machine-independent identity and stores its
ID on the record as an opaque token the package never interprets, so the
per-machine registry can map a synced note ID to a local home. The
registry stays keyed by name (rekeying would churn every completing-read,
tab name, and persistence lookup) and a nil node-id must remain valid so a
workspace works without any integration. `:sessions-dir` leaves the payload
because the package no longer has a session concept — sessions are
gptel's, associated through the graph by the integration layer.

## Design pattern
Follow the existing plist-record accessors in `data-model.org`
(`workspace--home` / `workspace--set-home`) and the whitelist serializer in
`persistence.org`. Public setters that mutate identity flush synchronously —
mirror how `workspace-new`'s home stamp triggers the flush.

## Verification
- `./bin/tangle-org.sh config/workspaces/data-model.org` (and the other three) validate.
- `./bin/run-tests.sh -d config/workspaces` — green.
- `grep -n 'sessions-dir' config/workspaces/data-model.el config/workspaces/workspaces.el config/workspaces/integrations.el` → no matches.
- `grep -n 'node-id' config/workspaces/integrations.el config/workspaces/persistence.el` → payload key and whitelist entry present.
- Whole-tree grep (lineage meta-discovery 1): `grep -rn 'sessions-dir' config/ init.org --include='*.org'` → only matches outside `config/workspaces/` that a later task owns (list them in Observations).

## Context
design.md § D2 'node-id is identity; the registry name stays the key'
specs/workspaces/spec.md § 'Workspace node identity', 'Per-machine persistence and restoration'
specs/workspace-integrations/spec.md § 'Anchor payload contract (push, not consult)'

## Cited register entries

Register entries are reference material to pressure-test, not authority to defer to. Speculated entries carry explicit licence to push back via Observations / Discoveries.

- `register/shape/workspace-plist-v3` — RE-STATED to speculated this cycle. Pressure-test: is `:node-id` really just an optional key with no v4 bump, and does a plist lacking it load with nil and *no* notice through every restore path (startup hydrate, re-anchor, broken-tag)? If the serializer whitelist or the deserializer forces a notice, push back.
- `register/shape/workspace-integration-anchor-payload` — RE-STATED. Pressure-test the ordering claim: a `:node-id` set by an earlier `:on-create` handler must be visible to later handlers in the same dispatch. If the payload is built once before the loop, the entry is wrong — say so in Discoveries rather than silently rebuilding it per handler.
- `register/invariant/registry-name-equals-basename` — Confirmed invariant; must still hold. `:node-id` does not rekey anything.

## Out-of-scope

See sibling tasks in this change; do not fold in cycle-2 work (ws-scaffold-and-anchor, ws-inversion-points).

## Plan-phase notes (architect forward-mode, cycle-1789334430)

- Task-ordering hazard: `config/workspaces/scaffold.org` stage 4 (create `sessions/`) still calls `workspace--sessions-dir` until cycle-2 task `ws-scaffold-and-anchor` deletes that stage. Do NOT reach into scaffold.org to delete the stage here. Inline the one-line path computation `(expand-file-name "sessions" home)` at that call site (the stage dies next cycle) so the helper can be deleted now and the scaffold suite stays green. Record it in Observations so the cycle-2 task knows the inlined line is theirs to remove.
- Whole-tree grep for `sessions-dir` will also hit `config/org-graph/discovery.org` (soft-dependency `declare-function workspace--sessions-dir`) — that line is owned by `og-vault-only-discovery`, which runs in this same batch; list it in Observations, do not edit it.

## Observations

- **Setter placed in `persistence.org`, not `data-model.org`** (departure
  from step 1). `data-model.org` is the package's declared "pure,
  side-effect-free data layer" and `workspace--flush-state` is defined in
  `persistence.org`, which `require`s data-model — so a flushing setter in
  data-model would need an `fboundp`-guarded upward call and would put a
  disk write in the pure layer. `workspace-node-id` (pure reader) lives in
  data-model next to the other accessors; `workspace-set-node-id`
  (validate → mutate → flush) lives in persistence.org's new "Identity
  setter" section, next to `workspace-save`. Symbol names and the
  `(ws node-id)` signature are exactly as prescribed, so sibling tasks
  are unaffected. See disc-ws-node-id-and-payload-1.
- **The setter mutates the live record in place** (`plist-put`; always
  in-place on a non-empty plist, whether `:node-id` is already present
  from `workspace--make` or appended to a pre-slot persisted plist). This
  departs from the copy-and-return convention of `workspace--set-home` /
  `--set-name`, deliberately: in-place mutation is what makes the
  ordering contract hold without a registry round-trip — the registry,
  the dispatcher mid-loop, and the transient all hold the same object.
  Coupling to note for future maintainers: if the setter ever becomes
  copy+puthash, `workspace--dispatch-create-integrations` must re-read
  the record by name per handler (and purge cannot — see next bullet).
  Pinned by the v3 spec "mutates in place a pre-slot record". See
  disc-ws-node-id-and-payload-4.
- **Step 3's "(or look it up by name)" alternative is not viable**:
  `workspace-purge` `remhash`es the entry *before* dispatching
  `:on-purge`, so a by-name lookup at payload-build time would yield no
  record and `:on-purge` handlers would never see `:node-id` — which
  design D8 needs (the org-graph handler retires the project note).
  Therefore `workspace--integration-payload`, `--dispatch-create-` and
  `--dispatch-purge-integrations` all take `(ws context)` (the record),
  and the payload is rebuilt from it per handler. This required one-line
  call-site adaptations in files outside the listed set: `tabs.org` (two
  `workspace-new` branches pass `(gethash name workspace--registry)`),
  `workspaces.org` (purge passes its already-captured `ws`), and
  `workspaces-transient.org` (menu invoke passes `ws`). Verified by the
  unit spec "purge dispatch carries the record's :node-id" and by
  `workspace-delete-purge-spec.el` (called-with `ws 'purge`). See
  disc-ws-node-id-and-payload-2.
- **`scaffold.org` stage 4 inlined** per the plan-phase note:
  `(make-directory (expand-file-name "sessions" home) t)`. That line is
  cycle-2 `ws-scaffold-and-anchor`'s to remove along with the stage;
  `scaffold-spec.el`'s two sessions/ assertions stay green meanwhile.
- **Deleted `config/workspaces/test/gptel-integration-spec.el`** — it
  tested only `workspace-sessions-dir`, which this task deletes; design
  D10 lists it for deletion. Not in "Files to modify" but unavoidable for
  a green suite.
- **`config/workspaces/docs/README.org` touched** because the task's
  whole-tree grep expects no `sessions-dir` inside `config/workspaces/`:
  replaced the "gptel sessions" section (which documented the deleted
  consult) with a "Workspace identity (`:node-id`)" section and updated
  the payload key list. The README's wider framing (header and Concepts
  still describe `home.org` / `sessions/` under `:home`) is left for the
  cycle-2 tasks that change that behaviour.
- **Whole-tree grep (`grep -rn 'sessions-dir' config/ init.org
  --include='*.org'`)** — every remaining match is outside
  `config/workspaces/` and owned by a sibling:
  - `config/gptel/sessions/workspace-integration.org:26,68,80,116,123,129`
    (reads `:sessions-dir` from the payload) — `gptel-single-root`
    deletes the module.
  - `config/gptel/sessions/filesystem.org:76,90,91` — `fboundp`-guarded
    consult of `workspace-sessions-dir`; safe at runtime (falls through
    to the global root); `gptel-single-root`.
  - `config/org-graph/discovery.org:56,130` — `declare-function` +
    unguarded call of `workspace--sessions-dir`; `og-vault-only-discovery`.
  - `config/org-graph/workspace-integration.org:16,99` — prose naming
    `:sessions-dir` in the payload; owned by whichever task retires /
    relocates that module (design D1/D8 → `config/integrations/`).
  - Substring false positives: `jf/gptel-sessions-directory` in
    `config/gptel/sessions/{commands,constants,filesystem}.org` — not
    the removed symbol; a future version of this verification should
    grep `sessions-dir\b` or exclude `sessions-directory`.
- **Merge-window runtime hazards (batch-internal, tests unaffected)**:
  (a) `org-graph--active-workspace-homes` calls `workspace--sessions-dir`
  unguarded whenever `(featurep 'workspaces)` and
  `org-graph-watch-workspace-homes` (default `t`) — a live session with
  both loaded will hit `void-function` until `og-vault-only-discovery`
  merges. Its spec stubs the helper via `cl-letf` on `symbol-function`,
  which does not require the symbol to be bound, so the suite stays
  green. (b) `config/gptel/sessions/workspace-integration.el` will read
  `:sessions-dir` as nil from the new payload and its `:on-create` will
  skip/fail until `gptel-single-root` deletes it. Both resolve within
  this batch's integrate.
- **No public name→record lookup exists.** The end-to-end ordering spec's
  handler A locates the live record with
  `(gethash (plist-get p :name) workspace--registry)` — a private
  variable — because that is the only way today to go from a payload to
  the record `workspace-set-node-id` requires. The cycle-2 integration
  (`int-workspaces-org-graph`) will face the same gap. See
  disc-ws-node-id-and-payload-3.
- **Deserializer does not type-check `:node-id`.** A hand-edited
  non-string `:node-id` loads verbatim (same verbatim-preservation policy
  as `:home`; the constructor and setter do validate) and would propagate
  into payloads. Latent; no consumer today; left as-is to match the
  loader's "never rewrite / never notice canonical-form drift" contract.
- **Strict test assertion to be aware of**: the "loads a v3 plist lacking
  :node-id … no *Messages* notice" specs assert an *empty* `*Messages*`
  delta across `workspace--restore`. That is the right strictness for the
  scenario, but any future unrelated `message` on the healthy restore
  path will trip it rather than a node-id-specific one.

## Discoveries

- discovery_id: disc-ws-node-id-and-payload-1
  class: responsibility-leakage
  description: |
    The entry lists both `workspace-node-id` and `workspace-set-node-id`
    as data-model.org consumers. data-model.org is the declared pure
    layer and the synchronous flush (`workspace--flush-state`) is defined
    in persistence.org, which requires data-model — the flushing setter
    cannot live in data-model without an fboundp-guarded upward call and
    a disk write in the pure layer. Implemented: reader in
    data-model.org; setter in persistence.org ("Identity setter" section,
    beside workspace-save). Names and signature unchanged.
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    Reconcile the consumers list: `workspace-set-node-id` → file
    config/workspaces/persistence.org (writes_keys [":node-id"]; flushes
    via workspace--flush-state); `workspace-node-id` stays in
    data-model.org. Note in data-model's header that the reader is its
    one public symbol.

- discovery_id: disc-ws-node-id-and-payload-2
  class: invariant-gap
  description: |
    The entry (and task step 3) allow the constructor to "re-read it
    from the registry" per handler as an alternative to receiving the
    record. That alternative is wrong for purge-time dispatch:
    `workspace-purge` removes the registry entry before dispatching
    `:on-purge`, so a by-name re-read yields nil and `:on-purge` handlers
    would never see `:node-id` (design D8's org-graph teardown needs it).
    Implemented: `workspace--integration-payload (ws context)`,
    `workspace--dispatch-create-integrations (ws context)`,
    `workspace--dispatch-purge-integrations (ws context)`; the payload
    is rebuilt from WS immediately before each handler; purge passes the
    record it captured before unregistering. The ordering contract
    ("a :node-id set by an earlier handler is visible to later ones") is
    satisfied because `workspace-set-node-id` mutates that same record
    in place — verified end-to-end (integration-dispatch-spec) and at
    unit level (integration-registry-spec).
  affected_register_entry: register/shape/workspace-integration-anchor-payload
  recommendation: |
    Pin the signature as record + context and drop the "or re-reads it
    from the registry" clause; add a note that purge-time payloads are
    built from the record handed over post-unregister, and that the
    per-handler rebuild depends on the setter's in-place mutation
    (cross-reference disc-ws-node-id-and-payload-4). Update
    constructed_by to the new arity; update prior_form accordingly.
    Status: speculated → reconciled-recommended with these edits.

- discovery_id: disc-ws-node-id-and-payload-3
  class: scope-question
  description: |
    `workspace-set-node-id` takes the live record, but an `:on-create`
    handler only has the payload (`:name`, `:home`, `:node-id`,
    `:context`). There is no public name→record lookup in the package;
    the only route is `(gethash name workspace--registry)`, a private
    variable, which the delta spec's "Registry is the published
    boundary" requirement discourages consumers from touching. The
    ordering spec in this task does exactly that to exercise the setter.
  affected_register_entry: register/shape/workspace-integration-anchor-payload
  recommendation: |
    Decide in cycle-2 (int-workspaces-org-graph / ws-inversion-points)
    whether (a) `workspace-set-node-id` should also accept a registry
    name string, or (b) a small public reader (e.g. `workspace-record
    NAME`) joins the published boundary. Either keeps the record-based
    signature this task pinned; (a) is the smaller surface.

- discovery_id: disc-ws-node-id-and-payload-4
  class: mutation
  description: |
    The public setter mutates the record destructively (`plist-put` on
    the live plist) and returns it, whereas every existing `workspace--
    set-*` helper is non-destructive (copy-sequence + caller puthash).
    The departure is deliberate: identity must be observable by every
    holder of the record within the same dispatch without a registry
    round-trip, and purge dispatch holds a record that is no longer in
    the registry. Pressure-test outcome for the rest of the entry: no
    v4 bump; a plist lacking `:node-id` loads as nil with no notice on
    the startup-hydrate path (the re-anchor and broken-tag paths go
    through copy-sequence, preserving the key or its absence); the
    serializer whitelists `:node-id`; the loader does not validate its
    type (verbatim, like `:home`).
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    Record in the `:node-id` optional_keys note that the public setter
    is IN-PLACE (unlike the `--set-*` copy helpers) and why; add the
    loader's verbatim/no-type-check behaviour to the deserializer
    producer note alongside the `:home` wording. Status: speculated →
    reconciled-recommended.
