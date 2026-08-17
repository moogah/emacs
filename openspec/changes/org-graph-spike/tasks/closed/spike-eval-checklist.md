---
name: spike-eval-checklist
description: Author the spike evaluation runbook so the user can systematically validate the layered system over the eval window before deciding the long-term shape.
change: org-graph-spike
status: done
relations:
  - blocked-by:wire-into-init
cites_register_entries:
  - register/boundary/org-graph-agent-tools
  - register/invariant/org-graph-loader-ordered-sequence
---

## Files to modify
- `config/org-graph/docs/spike-eval.org` (new) — the evaluation runbook

## Implementation steps
1. Write a checklist the user works through during the ~1-week eval window,
   organized by the re-centered capabilities:
   - **Discovery (vulpea-only):** create a workspace; confirm its `:home`
     gets watched and indexed via the `:on-create` handler; confirm `id:`
     links resolve cross-workspace on a fresh session (the startup seed);
     confirm `~/work` is NOT being walked wholesale.
   - **Taxonomy (vulpea-schema):** tag notes as each type; run the per-type
     finders; run `org-graph/validate-all-of-type` and eyeball violations —
     is the schema field set right, too strict, too loose?
   - **Typed edges:** add `:IMPLEMENTS:`/`:CONTRADICTS:`/`:SUPERSEDES:`/
     `:RELATES_TO:` to 3-5 roam concept notes; verify outgoing/incoming/
     connected queries; confirm project notes do NOT get typed edges.
   - **Agent surface:** from a workspace assistant, exercise `org-graph-query`,
     `org-graph-typed-edges`, and a coordinator-mediated `org-graph-write-node`;
     confirm `:agent-draft:` stamping and no file corruption under rapid calls.
   - **Coexistence:** confirm org-roam UX is unchanged throughout.
2. Capture the open questions to answer empirically: is `RELATES_TO` too broad
   (OQ4); does `vulpea-journal` add value (OQ5); is the parser-epoch re-run
   actually repopulating `typed_edges`; do un-ID'd notes still slip through.
3. End with the decision prompt: keep both, retire org-roam, fold org-graph
   into workspaces, or abandon — and what evidence would point each way.

## Design rationale
This is a spike: the deliverable is a system the user evaluates day-to-day,
and findings inform a follow-up change that decides the long-term shape
(proposal.md). A written runbook makes the evaluation systematic instead of
ad-hoc and ensures the resolved decisions (RE-1..RE-6, RE-2a) actually get
exercised and stress-tested.

## Design pattern
Plain org checklist with `[ ]` items grouped by capability, plus a
"Findings" section to fill in. Reference the open questions in design.md so
nothing gets evaluated in a vacuum.

## Verification
- The runbook exists, is tangle-free prose (no code to validate), and covers
  every resolved decision and open question.
- A dry read-through confirms each item is concretely checkable by the user
  without further explanation.

## Context
proposal.md (spike framing, findings inform follow-up);
design.md § Open Questions; design.md § Re-evaluation.
</content>

## Cycle 1782564058 updates (cycle-1782564058)
> Still blocked on `wire-into-init`. Context update only — no prose invalidated.

- The **Agent surface** eval section is now concretely backed: the three tools
  exist (`org_graph_query`, `org_graph_typed_edges`, `org_graph_write_node`) with
  coordinator-mediated, `:agent-draft:`-stamping `write-node`. When you write the
  runbook, name the snake_case tool identifiers the assistant will expose, and
  point the "no file corruption under rapid calls" check at
  `org-graph-coordinator/with-file-lock` (the confirmed unconditional lock).

## Cycle 1782566912 updates (cycle-1782566912)
> Still blocked on `wire-into-init`. Context + a meta-discovery the runbook must
> bake in, plus two cites.

- **Discovery section — distinguish birth-indexing from ongoing watching.** A
  cycle-finding (review finding 1, spec-signal): the `:on-create` handler calls
  `vulpea-db-sync-update-directory`, which **one-shot INDEXES** the new workspace
  `:home` at birth — it does NOT install a live filenotify watcher. vulpea does not
  retroactively watch a directory added to `vulpea-db-sync-directories` after autosync
  has started; ongoing watching is picked up only on the next autosync restart, which
  the **`:menu` → `org-graph/configure-sync`** path performs. So the runbook's "confirm
  its `:home` gets watched and indexed" item must split into TWO checks: (a) is the new
  home **indexed at birth** (immediately queryable)? (b) does a note added to that home
  **later** get picked up WITHOUT a manual re-index — or does it require running the
  Integrations `:menu` "G" / `configure-sync`? This is exactly the kind of load-bearing
  ergonomics question the spike exists to answer (is birth-index + manual re-index
  acceptable, or do we want on-create to trigger an autosync restart?).
- **Agent surface section is now fully backed.** `workspace-integration` landed (merge
  `6c5fa7ce`): the `workspace-assistant` preset's `:tools` slot is populated with
  `org-graph/agent-tools`, so the per-workspace assistant really does expose the three
  tools `org_graph_query` / `org_graph_typed_edges` / `org_graph_write_node`. Name those
  snake_case identifiers in the runbook; point the "no file corruption under rapid calls"
  check at `org-graph-coordinator/with-file-lock`. Add a check that the assistant in a
  freshly-created workspace lists the org-graph tools (confirms the `:on-create` +
  preset-population path fired in a live boot).
- **Menu check:** confirm the workspaces Integrations transient shows the org-graph
  entry under key `"G"` and that invoking it re-indexes the current roots.
- Cites added: `register/boundary/org-graph-agent-tools` (the surface the runbook
  exercises) and `register/invariant/org-graph-loader-ordered-sequence` (the runbook is
  the day-to-day proof the consolidated module loads and registers in a real session).

## Cycle 1782570180 updates (cycle-1782570180)
> Still blocked on `wire-into-init`. Context only — no prose invalidated.

- **The cold-load gate now exists.** `module-load-smoke` landed this cycle (merge
  `d74a0d55`): `config/org-graph/test/module-load-spec.el` asserts every org-graph
  registration fires and org-roam is intact. Your runbook's "confirm the consolidated
  module loads and registers in a real session" item is now backed by a spec — but note
  that the cited invariant `register/invariant/org-graph-loader-ordered-sequence` is
  **DIVERGENT** until `wire-into-init` consolidates the loader and adds a real
  `(require 'org-graph)`-alone cold-load guard. By the time this runbook task runs
  (blocked-by `wire-into-init`), the loader will be consolidated and the invariant
  reconciled — so the runbook's live-session load check is the day-to-day proof the
  reconciled loader holds. No prose change needed; this is a dependency note.

## Cycle 1782573574 updates (cycle-1782573574)
> **Now the LAST open task on the critical chain.** `wire-into-init` landed this
> cycle (merge `50af89ae`): org-graph is wired into `jf/enabled-modules` (after
> gptel + workspaces), the loader is consolidated, and
> `register/invariant/org-graph-loader-ordered-sequence` moved **divergent →
> reconciled (re-stated)**. The module now loads in a real boot. Read before writing
> the runbook.

- **The "module loads/registers in a real session" check is now backed AND
  reconciled.** `(require 'org-graph)` loads all eight submodules by path in
  canonical order; the cold-load guard (`module-load-spec.el`) passes. Your runbook
  can assume org-graph loads on a normal boot.
- **NEW runbook item — verify the DEFERRED DB work actually fires (review Finding 1,
  spec-signal).** Two operations are deferred to `emacs-startup-hook` to keep module
  load DB-free: (a) the typed-edge extractor REGISTRATION (applies its `typed_edges`
  schema), and (b) the discovery `org-id-locations` SEED. These are **only
  wiring-tested** (the suite asserts they are on the hook + drives them with the DB
  stubbed); their actual firing at real launch is NOT covered by tests, and they do
  NOT re-fire on `jf/reload-module` after startup. So the runbook MUST add explicit
  live-session checks:
  - After a fresh real boot, confirm the typed-edge extractor is actually
    registered with vulpea (e.g. `vulpea-db-get-extractor 'org-graph-typed-edges`
    is non-nil) — i.e. `emacs-startup-hook` fired and registration landed against a
    real DB.
  - After a fresh real boot, confirm `id:` links resolve cross-workspace (the seed
    ran) WITHOUT a manual re-index.
  - Note the `jf/reload-module` caveat: re-loading org-graph mid-session does NOT
    re-run the deferred ops; a restart is needed (or call the deferred fns directly).
    The runbook should tell the evaluator this so a mid-session reload isn't
    mistaken for a registration failure.
- **`emacs-startup-hook` (not `after-init-hook`) is the post-init seam** under
  `emacs -q --load init.el`; mention this if the runbook references where deferred
  work runs.

## Observations

- **OQ3 has no implemented surface yet.** The task and design.md OQ3 ask whether
  `:agent-draft:` exclusion from the type finders should be a defcustom. As built,
  `config/org-graph/finders.el` ships `org-graph/find-agent-drafts` but the other
  finders carry no draft-exclusion toggle (no `org-graph-exclude-drafts-from`
  defcustom exists). The runbook frames OQ3 as still-open and notes this absence so
  the evaluator records the felt need rather than expecting an existing knob.
- **Typed-edge "roam-only" is enforced by a private predicate.** D2/RE-4 "project
  notes do NOT get typed edges" is implemented as `org-graph-extractor--roam-note-p`
  gating `org-graph-extractor--edges-from-note`. There is no public command to assert
  this, so the runbook check is "author an edge on a non-roam note, re-index, confirm
  the query returns nothing." Concretely checkable, but indirect.
- **No defcustom for the agent-draft tag string.** `org-graph-tools/write-node`
  hard-codes the `"agent-draft"` filetag (string literal), so the runbook names the
  literal tag rather than a configurable symbol.
- Everything else in the task body matched the source: the three snake_case gptel
  tool `:name`s, `org-graph/agent-tools`, `org-graph-coordinator/with-file-lock`
  (unconditional, 5.0s default timeout), the `:menu` "G" entry →
  `org-graph/configure-sync`, the two `emacs-startup-hook` deferred ops
  (`org-graph--register-extractor`, `org-graph--seed-org-id-locations-deferred`), and
  the `vulpea-db-get-extractor 'org-graph-typed-edges` registration probe.

## Discoveries
- discovery_id: disc-spike-eval-checklist-1
  class: spec-signal
  description: |
    OQ3 (`:agent-draft:` finder exclusion: defcustom vs hard-coded) is referenced by
    the task/design but has no corresponding code surface. The type finders in
    `config/org-graph/finders.el` do not exclude agent-draft notes and there is no
    `org-graph-exclude-drafts-from` defcustom; only `org-graph/find-agent-drafts`
    exists. This is consistent with OQ3 being an open question (decide after a week of
    use), but worth flagging so a future implementor doesn't assume the toggle exists.
  recommendation: |
    Leave as-is for the spike; resolve OQ3 in the follow-up change based on runbook
    findings. No register entry affected.
- discovery_id: disc-spike-eval-checklist-2
  class: interface-drift
  description: |
    Minor terminology drift between the original task body and as-built source. The
    task's Discovery bullet said the new workspace `:home` "gets watched and indexed"
    via `:on-create`; the source (`org-graph-workspace-integration--on-create` calling
    `vulpea-db-sync-update-directory`) only ONE-SHOT INDEXES at birth and explicitly
    does NOT install a live watcher. The later cycle stanzas (1782566912, 1782573574)
    already corrected this; the runbook reflects the as-built one-shot-index behavior
    and splits the check into birth-index vs ongoing-pickup accordingly.
  affected_register_entry: register/boundary/org-graph-agent-tools
  recommendation: |
    No change needed — register entries and cycle stanzas already describe the
    one-shot-index behavior. Recorded for provenance.
