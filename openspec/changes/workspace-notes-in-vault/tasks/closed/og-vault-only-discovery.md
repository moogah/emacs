---
name: og-vault-only-discovery
description: Index the vault root only; delete workspace-home discovery, its toggle, and org-graph's workspace-integration module and loader slot.
change: workspace-notes-in-vault
status: done
reviewed_at: 2026-09-14T00:30:00Z
findings_path: .orchestrator/cycles/cycle-1789334430/reviews/og-vault-only-discovery.md
merge_commit: b6678ebc
merged_at: 2026-09-13T23:35:00Z
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

## Orchestrator merge notes (cycle-1789334430)

- Merge b6678ebc resolved one conflict in `config/org-graph/test/discovery-spec.el`: both this task and og-parser-hardening edited the file's header commentary. Resolution kept this task's corrected sentence ("scoped stub for it.") followed by the parser-hardening paragraph; no code or spec bodies were involved.
- Inline fix (arch-cycle-1789334430-ot-vault-discovery-1): the `jf/enabled-modules` comment/description for `org-graph/org-graph` in `init.org` still claimed the workspaces-registry coupling; rewritten by the orchestrator in the follow-up commit and `init.el` re-tangled.

## Observations

- `init.org` entry text ("Layered knowledge graph over vulpea, plugged into
  workspaces") and its preceding comment ("MUST load after both gptel and
  workspaces/workspaces (RE-5): org-graph registers into ... the workspaces
  integration registry") are now stale but were NOT touched here: the task
  body offered either owner, and `int-scaffold-and-lint` step 5 already
  plans to edit that exact entry when it adds `integrations/integrations`
  after it. Leaving `init.org`/`init.el` untouched also keeps this merge
  clean against `gptel-single-root` / `ws-node-id-and-payload`, whose
  verification greps run over `init.org`. The whole-tree grep in this
  task's Verification does not match those lines (its patterns are the
  deleted symbol names), so it passes as written.
- `tools.org` was the neighbour the task flagged: `org-graph/agent-tools`'s
  docstring and two prose paragraphs named `workspace-integration` and
  `workspace-assistant`. Reworded to "a preset owner (the integration
  layer, never org-graph)"; tangled `tools.el` is now clean. Behaviour
  unchanged. `tools-spec.el`'s commentary and one `it` title were
  reworded to match (no assertion changed).
- `org-graph-roam-root`'s defcustom docstring and section prose said
  "Workspace homes are indexed for discovery/navigation but excluded from
  typed-edge extraction". Minimal reword to "Notes elsewhere under the
  vault root ..." so the binding `grep 'workspace' org-graph.el` passes.
  The defcustom itself is deleted by cycle-2 `og-extraction-root-and-agent-dir`,
  which will re-touch that section — no conflict expected, but flagging
  the overlap.
- Runbook (`docs/spike-eval.org`): the task named only the Discovery
  section, but the binding whole-tree grep also hit the Agent-surface
  section's "Fresh-workspace assistant LISTS the tools" check (named
  `--populate-assistant-tools`) and the Discovery section's Integrations
  "G" check (named `--menu`). Both were rewritten as annotated
  `workspace-notes-in-vault` items; the Coexistence and OQ2 prose that
  described "vault root and workspace homes" were also corrected. The
  new Discovery checks are "Only the vault root is a sync root", "~/work
  is NOT walked", and "Hidden .agents/ transcript is absent after a full
  scan" (with a `vulpea-db-query` one-liner to prove it).
- `.tasks/update-stale-workspace-assistant-tools-spec.md` is affected by
  this deletion: its premise was that `config/org-graph/workspace-integration.el`
  populates the `workspace-assistant` preset's `:tools`. That code no
  longer exists, so the "registers no palette/agent tools yet" assertion
  in `config/gptel/presets/test/workspace-assistant-spec.el` passes again
  (`./bin/run-tests.sh -d config/gptel/presets` → 86 specs, 0 failed on
  this branch). The `.tasks/` item should be closed or re-pointed at
  `int-workspaces-gptel`, which re-injects the tools from the integration
  layer and will re-break that stale assertion unless it is updated then.
  Not edited here (out of scope; `.tasks/` is repo-root backlog).
- `module-load-spec.el` previously asserted NO load order at all — only
  that each module's registrations fired (which cannot distinguish a
  mis-ordered loader that happens to work because `require` found an
  already-provided feature). Added a structural describe block that
  reads the tangled `org-graph.el`'s `jf/load-module` lines and asserts
  the exact ten-basename sequence AND contiguity (span of 9 lines) —
  the same audit the register's enforcement note prescribes as a manual
  grep. Also added a loaded-image lint (`mapatoms` over `org-graph-*`
  symbols containing "workspace": none fbound/bound/provided) as the
  interim enforcement of `packages-independent-no-cross-naming` on the
  org-graph side until `int-scaffold-and-lint` lands the tangled-file
  grep spec.
- Weakly-asserted boundary to be aware of: the new hidden-directory
  specs pin vulpea's SYNCHRONOUS enumerator (`vulpea-db-sync--list-org-files`
  → `directory-files-recursively` + `vulpea-db-sync--org-file-p`, the
  latter also being the filenotify watcher's filter). vulpea's ASYNC
  full-scan path (`vulpea-db-sync--scan-files-async`) implements the
  same rule separately via `fd --hidden --no-ignore --exclude ".*"` (or a
  `find` fallback). Both exclude any path segment beginning with `.`,
  but the async subprocess path is only covered by the runbook's manual
  "Hidden .agents/ transcript is absent after a full scan" check, not by
  a spec. Worth knowing when `gptel-hidden-agents` relies on it.
- The three `emacs-startup-hook` deferrals (`org-graph--register-extractor`
  → `org-graph--configure-sync-deferred` → `org-graph--seed-org-id-locations-deferred`)
  are untouched by the slot removal: wiring, runtime order, and
  resilience specs all pass unchanged. `org-graph/configure-sync` still
  sets `vulpea-db-sync-directories` from `org-graph/index-roots`; the
  cold-load guard now additionally pins that value to
  `(list (file-name-as-directory (expand-file-name org-graph-vault-root)))`.
- `delete-dups` dropped from `org-graph/index-roots` (a one-element list
  cannot contain duplicates). Spec count 236 → 229: −10
  (`workspace-integration-spec.el`), −1 (watch-toggle defcustom), −1
  (registry registration step), +5 (hidden-dir ×2, loader-structure ×3);
  the six index-roots specs were replaced one-for-one.

## Discoveries

- discovery_id: disc-og-vault-only-discovery-1
  class: spec-signal
  description: |
    register/invariant/bounded-discovery-roots (speculated, re-stated to
    exactly one root) is CONFIRMED by implementation. `org-graph/index-roots`
    is now a pure `(list (file-name-as-directory (expand-file-name
    org-graph-vault-root)))` with the `boundp` guard falling back to
    `~/org/`; the fallback is pinned by a spec that `makunbound`s the
    variable. A fake `workspaces` feature changes nothing (spec). The
    "hidden directories are excluded by vulpea's own rule" clause was
    pressure-tested against vulpea's REAL sync-time enumerator on a
    throwaway tree containing `sessions/S/branches/main/.agents/x/session.org`:
    `vulpea-db-sync--org-file-p` rejects it and
    `vulpea-db-sync--list-org-files` omits it while listing the visible
    sibling — so the `.agents/` rename (gptel-hidden-agents) can rely on
    it. Nuance for the entry text: vulpea implements the rule TWICE
    (sync: `not (string-match-p "/\\." path)`; async full scan: `fd
    --hidden --no-ignore --exclude ".*"` / `find` fallback). Both exclude
    any dot-segment; only the sync form is spec-covered.
  affected_register_entry: register/invariant/bounded-discovery-roots
  recommendation: |
    Reconcile speculated -> confirmed. Update enforcement_mechanism.location
    to: config/org-graph/test/discovery-spec.el ("org-graph/index-roots"
    and "hidden subdirectories are excluded by vulpea's own rule"
    describe blocks) + module-load-spec.el ("boot sync configuration"
    pins vulpea-db-sync-directories to the single expanded vault root).
    Amend the statement's hidden-dir clause to name both vulpea paths
    (`vulpea-db-sync--org-file-p` for sync + watcher; fd/find `--exclude
    ".*"` for the async scan) so a future vulpea bump that changes one
    is recognised as touching this invariant.

- discovery_id: disc-og-vault-only-discovery-2
  class: interface-drift
  description: |
    register/invariant/org-graph-loader-ordered-sequence: the ten-module
    order is implemented (schemas -> extractor -> coordinator -> query ->
    finders -> authoring -> edge-type -> tools -> discovery -> menu) and
    the three emacs-startup-hook deferrals are UNCHANGED by the slot
    removal (wiring, runtime order extractor -> configure-sync -> seed,
    and display-warning resilience all asserted and green). Drift found
    in the entry's enforcement text: it has claimed since
    cycle-1786636086 that module-load-spec.el "asserts the ten/eleven-
    module order", but the spec never asserted ORDER — only that each
    module's registrations fired, which a mis-ordered loader can also
    satisfy when `require` finds an already-provided feature. This task
    adds the missing structural assertion (reads the tangled loader's
    contiguous `jf/load-module` block and compares the basename sequence
    exactly, plus a contiguity check), making the enforcement text true
    for the first time.
  affected_register_entry: register/invariant/org-graph-loader-ordered-sequence
  recommendation: |
    Reconcile speculated -> confirmed. Update enforcement_mechanism to
    name the new describe block ("loader load-wires exactly the ten
    submodules, in canonical order, contiguously") as the ORDER guard,
    distinct from the registration assertions. When cycle-2
    og-connected-dblock inserts `dblock` after `query`, it must update
    `module-load-spec--canonical-order` (the spec fails loudly
    otherwise — intended).

- discovery_id: disc-og-vault-only-discovery-3
  class: invariant-gap
  description: |
    register/invariant/packages-independent-no-cross-naming (speculated):
    the org-graph side is now clean — `grep -rn 'workspace'
    config/org-graph/*.el` and `grep -rn 'jf/gptel-' config/org-graph/*.el`
    both return nothing, and a loaded-image lint (no `org-graph-*` symbol
    or feature containing "workspace" is fbound/bound/provided after a
    cold `(require 'org-graph)`) is in module-load-spec.el as interim
    enforcement. Gap in the entry's stated lint pattern: it prescribes
    `"workspace-\|jf/gptel-"` for config/org-graph/*.el, but the
    `org-graph.el` defgroup docstring said "plugged into workspaces"
    (bare plural, no hyphen) and would have passed that pattern while
    still naming the package. The task body's stricter bare `workspace`
    grep caught it. Prose in docstrings IS tangled, so "lint against
    tangled .el, not .org prose" does not exempt it.
  affected_register_entry: register/invariant/packages-independent-no-cross-naming
  recommendation: |
    When int-scaffold-and-lint writes config/integrations/test/boundary-lint-spec.el,
    use bare `workspace` (or `workspace[s-]`) for the org-graph and
    gptel-sessions greps rather than `workspace-`, and consider the
    symmetric `org-graph` / `gptel` bare forms for the other two
    packages. Also record that the loaded-image mapatoms lint in
    module-load-spec.el is a secondary enforcement site (it catches
    symbols defined by any code path, not just this package's files).
    Related backlog effect: .tasks/update-stale-workspace-assistant-tools-spec.md
    is now moot on this branch and should be re-pointed at
    int-workspaces-gptel.
