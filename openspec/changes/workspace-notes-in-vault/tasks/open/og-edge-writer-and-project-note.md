---
name: og-edge-writer-and-project-note
description: Add org-graph/write-edge (append an EDGES item under the coordinator lock) and org-graph/create-project-note.
change: workspace-notes-in-vault
status: done
reviewed_at: 2026-09-14T01:00:00Z
findings_path: .orchestrator/cycles/cycle-1789334430/reviews/og-edge-writer-and-project-note.md
inline_fix_commit: f83b3226
merge_commit: 3b946013
merged_at: 2026-09-14T00:20:00Z
task_class: feature
on_critical_path: true
cycle: cycle-1789334430
relations:
  - enables:int-workspaces-org-graph
  - enables:int-gptel-org-graph
cites_register_entries:
  - register/boundary/edge-writer-api
  - register/boundary/project-note-creation
  - register/invariant/edge-drawer-immediately-after-property-drawer
  - register/invariant/write-edge-idempotent
  - register/invariant/coordinator-lock-contract
  - register/vocabulary/relation-types
  - register/invariant/edge-drawer-discriminator
created_at: 2026-09-13T00:00:00Z
planned_at: 2026-09-13T21:20:30Z
---

## Files to modify
- config/org-graph/authoring.org (modify; tangle)
- config/org-graph/extractor.org (read only — reuse the drawer-name resolver `org-graph-extractor--drawer-name` or equivalent ~:69-78)
- config/org-graph/coordinator.org (read only — `org-graph-coordinator/with-file-lock`)
- config/org-graph/test/authoring-spec.el (modify)

## Implementation steps
1. `org-graph/write-edge (file type target &optional node-id)`:
   - Under `(org-graph-coordinator/with-file-lock file ...)`: `insert-file-contents`
     into a temp buffer, `(delay-mode-hooks (org-mode))`.
   - Locate the node: nil node-id → `point-min`; otherwise search for the
     heading whose `:ID:` property equals node-id (`org-find-property "ID" node-id`
     / `org-id-find` restricted to this file) and signal `user-error` if absent.
   - Find the node's property drawer end (`org-get-property-block` at file
     level or heading level). If an `EDGES` drawer (name from the extractor's
     resolver, case-insensitive) directly follows it, append the item inside
     it; else insert `:EDGES:\n- <type> :: [[id:<target>]]\n:END:\n`
     immediately after the property drawer's `:END:` line. Skip when an item
     with the same normalised type and target already exists (idempotent).
   - `write-region` back; then `(vulpea-db-update-file file)` so the edge is
     indexed without waiting for autosync.
   - Type normalisation: lowercase, spaces/underscores → hyphens (same rule
     the extractor applies when reading).
2. `org-graph/create-project-note (title &optional body)`: call
   `vulpea-create` with a template that yields `#+title: TITLE`,
   `#+filetags: :project:`, a file-level property `status: active` (match
   how the `project` schema expects the field — check
   `org-graph-schemas-register` / `vulpea-schema` field lookup; it reads
   note meta, so emit it as vulpea meta or a property consistently with the
   schema predicate), an empty `#+BEGIN: org-graph-connected\n#+END:` block,
   then BODY. Return the note ID; index immediately (`vulpea-create` already
   calls `vulpea-db-update-file`).
3. Specs (temp vault): write-edge on a file with only a property drawer
   creates the drawer right after it and leaves the rest byte-identical;
   second identical call is a no-op; heading node-id targets the heading's
   drawer; the lock function is invoked (spy); `vulpea-db-update-file`
   called with the file. create-project-note produces the expected
   keywords/fields and an ID the `project` finder's predicate accepts.

## Design rationale
Association in this design is a typed `part-of` edge in the note's
`EDGES` drawer, written by the integration layer from gptel hooks and
workspace dispatch. org-graph owns the drawer format and the file lock, so
it must publish the writer; callers never touch drawer syntax. Placing the
drawer directly after the node's property drawer matters for sessions: a
branch's verbatim head copy carries the edge, and gptel's identity rewrite
never touches it. The project-note constructor exists so the integration
layer creates the workspace identity note through org-graph's API rather
than by writing org text itself.

## Design pattern
`org-graph-tools/write-node` (tools.org ~:217-243) shows lock usage and
file writing; `org-graph/find-or-create` (authoring.org) shows the
`vulpea-create` wrapper style. Drawer parsing rules are in extractor.org
§ Edge-drawer items — the writer must produce exactly what the reader
accepts.

## Verification
- `./bin/tangle-org.sh config/org-graph/authoring.org` validates.
- `./bin/run-tests.sh -d config/org-graph` — green.
- `grep -n 'defun org-graph/write-edge\|defun org-graph/create-project-note' config/org-graph/authoring.el` → both defined.
- Round-trip scenario (un-stubbed): `org-graph/write-edge` on a temp file, then run the real extractor over it and expect exactly one edge row with the normalised type.
- Read `runtime/straight/repos/vulpea/` for `vulpea-create` / `vulpea-db-update-file` signatures before writing the project-note template; quote them in your report.

## Context
design.md § D6 (Edge-writer API, Project-note API bullets), § D7
specs/org-graph/spec.md § 'Edge-writer API', 'Project note creation API'

## Cited register entries

Register entries are reference material to pressure-test, not authority to defer to. Speculated entries carry explicit licence to push back via Observations / Discoveries.

- `register/boundary/edge-writer-api` — NEW, speculated. Eight stages; pressure-test stage 3 (heading lookup restricted to this file) and stage 4 (drawer directly after the property drawer, case-insensitive name from `org-graph-extractor--edge-drawer-name`).
- `register/boundary/project-note-creation` — NEW, speculated. The `status: active` form must satisfy the `project` schema predicate in `schemas.org` — check how it reads the field and record which form you emitted.
- `register/invariant/edge-drawer-immediately-after-property-drawer` — NEW, speculated. For the file-level node the drawer lands BEFORE `#+title:`; the byte-identity spec is the enforcement.
- `register/invariant/write-edge-idempotent` — NEW, speculated. Same (node, normalised type, target) twice → one item, file byte-identical.
- `register/invariant/coordinator-lock-contract` — Confirmed; the writer must run inside `org-graph-coordinator/with-file-lock` — spy on it.
- `register/vocabulary/relation-types` — Reconciled; the normalisation rule is the contract. Route TYPE through `org-graph-extractor--normalize-rel` — a second inlined normaliser is a duplication finding.
- `register/invariant/edge-drawer-discriminator` — Confirmed; the writer must emit exactly what the extractor's drawer parser accepts. Add a round-trip spec: write an edge, run the extractor on the file, expect one edge row.

## Out-of-scope

See sibling tasks in this change; do not fold in cycle-2 work (og-connected-dblock, int-workspaces-org-graph, int-gptel-org-graph).

## Plan-phase notes (architect forward-mode, cycle-1789334430)

- `status: active` is a vulpea META item (`- status :: active` in the note body) because the `project` schema predicate in `schemas.org` reads `vulpea-note-meta-get`; it is not a property-drawer entry as D6's wording suggests. Emit it as META and say so in your report.
- `org-graph/write-edge`'s heading lookup is pinned by the register entry as an in-file `:ID:` scan (not `org-id-find`), so an unindexed file still works. If you disagree, push back in Discoveries.

## Observations

- **Deviation (security): BODY is passed to `vulpea-create` as a `:context` value, not as `:body` template text.** Probe (batch Emacs against the checked-out vulpea 2.4): `(vulpea--expand-template "50%(+ 1 2) and ${title}" "TT" "ID" nil)` → `"503 and TT"` — `%(elisp)` in a `:body` string is *evaluated* and `${var}` substituted. The task/register say "BODY via :body"; our BODY is caller data (the migration command carries a legacy `home.org` body here), so the template is the trusted skeleton `"#+BEGIN: org-graph-connected\n#+END:\n\n${body}\n"` and BODY rides in `:context (:body BODY)`, which vulpea substitutes *after* directive expansion and never re-scans (probe: `%(`, `${title}`, `%<` all land verbatim). One `vulpea-create` call, one write, one index — the pinned order is unchanged. Spec'd both at the delegation level and on disk with the real `vulpea-create`.
- **`status: active` emitted as vulpea META (`- status :: active`), as the plan-phase note directs — verified end-to-end, not by inspection.** The on-disk spec runs the real `vulpea-create`, re-parses the file with vulpea's real parser (`vulpea-db--parse-file`), and asserts `(vulpea-note-meta-get note "status" 'symbol)` → `active`, `(org-graph/note-of-type-p note 'project)` → t, and `(vulpea-schema-validate note 'org-graph-project)` → nil violations. A `:STATUS:` property would have failed all three.
- **Latent design gap found by probe (recorded as a Discovery, not fixed here): an EDGES drawer at the pinned position shadows a note's vulpea META.** `vulpea-db--extract-meta` takes the *first* descriptive plain-list before the first headline and walks into drawers; a drawer `- part-of :: [[id:P]]` directly after the property drawer therefore becomes the note's META (`(("part-of" "[[id:P]]"))`) and the real meta list below is ignored. Sessions carry no META so the D7 case is safe; any vault note with META that receives a written edge (cycle-2 `new-in-workspace` on a `reference` note with a required `source` field, for example) would lose its schema fields. Not user-visible yet — no caller of `org-graph/write-edge` exists until cycle 2 — so no task filed; the architect should route it (a natural home is the parser-hardening advice slot: advise `vulpea-db--extract-meta` to skip lists inside drawers).
- **Register stage order refined by the implementation.** Argument validation (empty TYPE / TARGET / unconfigured drawer name) runs *before* the lock is taken — nothing to release, and the spec asserts `org-graph-coordinator--acquire` is not called on those paths. The register lists normalise-type as stage 5 (after locate); in code it is stage 1. Dedupe parses only the drawer's own region (narrowed `org-element-parse-buffer`), not the whole file, so a long session is never fully parsed for a duplicate check. Return value pinned: the appended item text when written, nil on a duplicate (the migration command can report added vs present).
- **Heading lookup is the in-file scan the plan-phase note pins; no push-back.** `org-find-property "ID" NODE-ID` is bounded to the buffer and returns `point-min` when NODE-ID is the file-level ID — so passing the file's own ID resolves to the file-level node rather than erroring; spec'd. `org-get-property-block` handles the file-level drawer (org 9.8-pre, comments-before-drawer tolerated).
- **Small duplication, deliberately left.** `org-graph-authoring--rel-symbol` (symbol → `symbol-name` → `org-graph-extractor--normalize-rel`) is the same 3-line shim as `org-graph-tools--rel-symbol` in tools.el (authoring loads before tools, so it cannot reuse it). The normalisation chain itself is not duplicated. Recorded as a `duplication` Discovery with the obvious fix (let the extractor's normaliser accept a symbol).
- **Spec robustness against the sibling scope-gate change.** The extractor round-trip spec binds both `org-graph-roam-root` and `org-graph-vault-root` to the temp dir because og-vault-only-discovery moves the gate to the vault root; after merge the roam binding is a harmless lexical no-op. The placement spec accepts `-` or `_` in the filename because vulpea's own default template is `${timestamp}_${slug}.org`; the dash pin is discovery-spec's assertion, not this module's.
- **Weakly asserted spec, flagged.** "never runs org-mode hooks" passes because the writer itself binds `org-mode-hook` to nil around `(delay-mode-hooks (org-mode))`; it asserts the contract, not an independent mechanism. The "never visits the file" half (`get-file-buffer` nil) is independent.
- **Byte identity verified with non-ASCII content.** The fixture title carries `—` and `ü`; `insert-file-contents` + `write-region` round-trips the bytes unchanged (asserted on the literal bytes, not the decoded string).
- **Minor normalisation in `create-project-note`.** An empty-string BODY is treated as nil; trailing newlines on BODY are trimmed and exactly one is emitted, so the file always ends with a newline. The block skeleton without BODY also gets a final newline (vulpea itself would leave the file without one).
- **Vulpea signatures read from `runtime/straight/repos/vulpea/` as the task requires:** `(cl-defun vulpea-create (title &optional file-name &key id head meta body context properties tags parent (after 'last)))` → returns the `vulpea-note`; `(defun vulpea-db-update-file (path))` → "Returns number of notes updated (file-level + headings)". `vulpea--format-note-content` emits drawer, `#+title:`, `#+filetags:`, HEAD, blank line, META items; `vulpea--create-file` appends `"\n\n" body`, writes with `write-region … 'silent`, calls `org-id-add-location`, then `vulpea-db-update-file` (erroring on 0), then `vulpea-db-get-by-id`.


## Discoveries

- discovery_id: disc-og-edge-writer-and-project-note-1
  class: invariant-gap
  description: |
    The pinned placement (EDGES drawer on the line directly after the
    node's property drawer, ahead of #+title: and everything else)
    collides with vulpea's META rule. vulpea-db--extract-meta (vulpea
    2.4, vulpea-db-extract.el) defines a note's META as the FIRST
    descriptive plain-list found by org-element-map before the first
    headline, and org-element-map descends into drawers. Probe on the
    checked-out vulpea: a file with the drawer before the meta list
    yields META (("part-of" "[[id:p]]")) and the real "- category :: x"
    item is lost; with the drawer after the meta list META is
    (("category" "x")). Sessions have no META (safe), and a project
    note receives only incoming edges (safe), but any META-bearing
    vault note that becomes an edge SOURCE via org-graph/write-edge
    (cycle-2 new-in-workspace on a topic/debug/reference note) has its
    schema fields shadowed — a required `source` on a reference note
    would start failing validation. Implemented as pinned (the
    sessions design depends on it); nothing fixed here.
  affected_register_entry: register/invariant/edge-drawer-immediately-after-property-drawer
  recommendation: |
    Architect decision needed before cycle-2 callers land. Options:
    (a) extend the parser-hardening advice (og-parser-hardening owns
    advice on vulpea internals) to advise vulpea-db--extract-meta so
    plain-lists inside a `drawer` element are skipped — keeps the
    placement invariant and fixes it for every note kind; (b) upstream
    the same skip to vulpea; (c) narrow the invariant to "directly
    after the property drawer OR after the META list when one exists"
    — rejected by default since it makes placement note-kind-dependent
    and breaks the byte-identity spec shape. Add a spec either way:
    a META-bearing note with a written edge still reads its META.

- discovery_id: disc-og-edge-writer-and-project-note-2
  class: deviation
  description: |
    Stage 2 of the entry says the meta item and dblock — and BODY —
    go "via :body". vulpea-create expands %(elisp), %<format> and
    ${var} directives in :body (probe: "50%(+ 1 2)" -> "503";
    "${title}" substituted). BODY here is caller data (int-migration-
    command carries a legacy home.org body into it), so passing it as
    template text is an arbitrary-code-execution and text-corruption
    hazard. Implemented: the trusted skeleton
    "#+BEGIN: org-graph-connected\n#+END:\n\n${body}\n" is the :body
    template and BODY is supplied as :context (:body BODY), which
    vulpea substitutes after directive expansion and never re-scans
    (its own documented safety order). Same single vulpea-create call,
    same on-disk order (drawer, #+title:, #+filetags: :project:, blank,
    "- status :: active", blank, empty dblock, BODY). `status` is
    emitted as META per the plan-phase note and verified with the real
    parser + vulpea-schema-validate (zero violations).
  affected_register_entry: register/boundary/project-note-creation
  recommendation: |
    Reconcile stage 2: "meta via :meta (("status" . "active")), dblock
    via :body, BODY via :context (:body BODY) behind a ${body}
    placeholder — never as :body template text". Mark the `status`
    META form confirmed. Add note: BODY has trailing newlines trimmed
    and one appended; empty-string BODY == nil.

- discovery_id: disc-og-edge-writer-and-project-note-3
  class: interface-drift
  description: |
    The entry's eight stages survive with three refinements the code
    wanted. (1) Validation is stage 1, before the lock: empty TYPE,
    empty TARGET and an unconfigured drawer name are user-errors that
    never call org-graph-coordinator--acquire (spec'd); the entry
    places normalise-type at stage 5 and has no target check.
    (2) Locate-node: org-find-property "ID" NODE-ID (bounded to the
    buffer, confirmed — no org-id-find) returns point-min when NODE-ID
    is the file-level ID, so the file's own ID is a valid NODE-ID
    resolving to the file-level node rather than an error (spec'd).
    (3) Dedupe parses only the drawer's own region (narrowed
    org-element-parse-buffer over the org-element-at-point drawer) and
    reuses org-graph-extractor--item-relation plus the extractor's
    id-link walk at item depth, so equality is exactly the reader's
    (normalised tag, id: path) with descriptions ignored (spec'd).
    Return value pinned: appended item text on write, nil on duplicate.
    vulpea-db-update-file confirmed as the reindex entry point (name
    and signature read from the checked-out source).
  affected_register_entry: register/boundary/edge-writer-api
  recommendation: |
    Reconcile stages to: 1 validate (TYPE, TARGET, drawer name; before
    lock) -> 2 acquire-lock -> 3 visit-temp -> 4 locate-node (file-level
    ID accepted) -> 5 find-property-drawer -> 6 find-or-create-drawer +
    dedupe (drawer-region parse, reader's own item/link rules) -> 7
    write-back (skipped on duplicate) -> 8 reindex (skipped on
    duplicate). Record the return contract. Status: speculated ->
    reconciled-recommended.

- discovery_id: disc-og-edge-writer-and-project-note-4
  class: duplication
  description: |
    org-graph-authoring--rel-symbol (authoring.el) and
    org-graph-tools--rel-symbol (tools.el) are the same three-line
    shim: coerce a symbol via symbol-name, then call
    org-graph-extractor--normalize-rel. The normalisation chain is not
    duplicated (single translation site holds), only the symbol
    coercion; authoring loads before tools so it cannot reuse the tools
    helper.
  affected_register_entry: register/vocabulary/relation-types
  recommendation: |
    Let org-graph-extractor--normalize-rel accept a symbol (symbol-name
    it internally) and delete both shims; update the entry's
    canonical_mapping_function accordingly. Small follow-up, not
    urgent.
