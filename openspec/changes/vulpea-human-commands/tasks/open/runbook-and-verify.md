---
name: runbook-and-verify
description: Update spike-eval runbook for changed boot/root expectations; full test run and fresh-boot verification
change: vulpea-human-commands
status: done
merge_commit: f4cf2b86
relations:
  - blocked-by:menu-module
---

## Files to modify
- config/org-graph/docs/spike-eval.org (modify)
- (verification only) all config/org-graph tests

## Implementation steps
1. Update `spike-eval.org` where this change invalidates recorded
   expectations — edit the checklist text (leave existing `[x]` history
   intact; annotate rather than rewrite where an item was already checked
   under old behavior):
   - *Boot model* section: there are now THREE deferred
     `emacs-startup-hook` ops — extractor registration, sync
     configuration (`org-graph--configure-sync-deferred`), org-id seed —
     and the reload caveat applies to all three.
   - *Live-session firing* section: add a check — from a fresh boot,
     `(bound-and-true-p vulpea-db-autosync-mode)` → t and
     `vulpea-db-sync-directories` equals `(org-graph/index-roots)` with no
     manual step; `*Warnings*` additionally clean of "sync configuration
     skipped".
   - *Discovery* section: `(org-graph/index-roots)` now returns `~/org/`
     (the vault root) instead of `~/org/roam/`; the "ongoing pickup"
     sub-check (a) expectation CHANGES for boot-time roots: a note added
     under a boot-time root during the session IS picked up automatically
     (autosync live from startup). The unchanged limitation: roots added
     mid-session (new workspaces) still get only the `:on-create`
     one-shot.
   - *Coexistence (D8)* section: annotate that the vulpea-human-commands
     change supersedes the side-by-side framing — org-roam interop is an
     explicit non-goal now; keep the "org-roam UX unchanged" check (we
     still must not break it) but drop the expectation that the two
     indices track each other.
   - Add a short *Human commands & menu* capability section: `SPC v`
     opens the menu; find-or-create and insert-link behave per the
     org-graph-note-commands spec (create lands in `~/org/` as
     `<timestamp>-<slug>.org`, immediately findable and id-resolvable);
     edge queries at point render `*org-graph-edges*`.
2. Run the full suite: `./bin/run-tests.sh -d config/org-graph` — zero
   failures across both frameworks.
3. Fresh-boot manual verification (GUI `Emacs.app` or
   `./bin/emacs-isolated.sh`, NOT `jf/reload-module`) of the new/changed
   runbook items above; record results in the runbook checkboxes and
   Findings.
4. Sanity-check the untouched boundaries survived: typed-edge extraction
   still gated to `~/org/roam/` (an `EDGES` drawer on a note at `~/org/`
   top level yields no `typed_edges` rows — expected per design D1/OQ-A),
   agent `org_graph_write_node` still targets the roam root.

## Design rationale
The runbook is the spike's evidence base for the end-of-window decision
prompt; leaving stale expectations (roam-only root, no boot autosync,
side-by-side D8 framing) would corrupt the findings this change was born
from. The deferred ops are only wiring-tested by the automated suite —
their firing against a real DB on a real launch is exactly what the
runbook verifies by hand, so the new third op inherits that obligation.
The D8 annotation records the strategic shift (vulpea replaces org-roam;
coexistence during the eval is transitional, not a goal) without erasing
the still-binding "don't break org-roam while it's installed" check.

## Design pattern
Follow the runbook's own conventions (`spike-eval.org` § How to use this
runbook): each item names the exact command/eval and the observable
result meaning "pass"; surprises go under Findings keyed by RE-/OQ- ids.

## Verification
- `./bin/run-tests.sh -d config/org-graph` — all pass.
- Runbook diff reviewed: no previously-recorded `[x]` evidence deleted,
  only annotated/extended.
- All new runbook items exercised on a real fresh boot and checked off
  (or surprises recorded under Findings).

## Context
design.md § 'Migration Plan' and Risks (deferred-op wiring-only testing)
proposal.md § Impact (spike-eval supersession note)
config/org-graph/docs/spike-eval.org (structure and conventions)

## Observations

- Automated gate (step 2) is green: `./bin/run-tests.sh -d config/org-graph`
  ran 232 specs, 0 failed (buttercup; no ERT files in the directory).
- The full `./bin/run-tests.sh` run has 15 failures, ALL outside
  `config/org-graph` and none attributable to this task (working tree is
  docs-only). Left unfixed per scope: 9 ERT in `config/bash-parser`
  (`test-corpus-integration-002` + 8 `test-pattern-flow-*`), 3 buttercup
  gptel-scope bug-reproduction specs ("Bug 4: end-to-end add-to-scope",
  "multi-violation add-to-scope leaks", corpus fs-ops count), 1
  workspaces persistence spec ("skips a persisted entry whose :home is a
  relative path" — fails inside a `substring` traceback, likely a
  *Messages*-assertion perturbed by extra load output), and 1 gptel
  presets spec (see disc-runbook-and-verify-1).
- Steps 3 and 4 (fresh-boot GUI verification and real-vault sanity
  checks) are NOT performed here — they require a human at a real GUI
  Emacs against the real `~/org` vault. All new/changed runbook items
  are written as unchecked `[ ]` with exact evals. Outstanding for the
  user, by runbook section:
  - *Live-session firing*: "Sync configuration fired — autosync live
    from boot" (new); "org-id seed ran" and "Warnings are clean"
    (pre-existing, still unchecked; Warnings item now also covers
    "sync configuration skipped").
  - *Discovery*: "Roots are explicit and bounded" (expectation changed
    to `~/org/`); "Boot-time roots are watched live" (new).
  - *Typed edges*: "Vault-top-level notes get NO typed edges either"
    (new, step-4 boundary check).
  - *Agent surface*: `org_graph_write_node` item extended — confirm the
    created file lands under `~/org/roam/` (step-4 boundary check).
  - *Human commands & menu*: all six new items (`SPC v`, find-or-create,
    insert-link, edge queries at point, no-ID user-error, absent-evil).
- Deliberate small extensions beyond the five enumerated bullet areas,
  all under step 1's governing sentence ("update where this change
  invalidates recorded expectations"): Boot-model prose count fixed
  eight → eleven submodules (loader invariant); OQ1 extended to watch
  the new boot-time async full scan for startup stall (design § Risks
  explicitly leans on OQ1 for this); OQ2's watched-roots parenthetical
  updated `~/org/roam/` → `~/org/`; the two step-4 sanity checks were
  given runbook homes (above) so the manual hand-off has checkboxes.
- No previously recorded `[x]` evidence was deleted; the two `[x]` items
  whose expectations changed (reload caveat, ongoing pickup) carry
  /Annotation (vulpea-human-commands):/ sub-items instead.
- Latent runbook staleness NOT touched (out of this task's scope, from
  an earlier change): the *Typed edges* section intro and its "Author
  edges" item still describe PROPERTIES-drawer authoring
  (`:IMPLEMENTS:` keys), but extraction now reads only the `EDGES`
  drawer (`org-graph-edge-drawer`; `config/org-graph/extractor.el`
  states ordinary PROPERTIES entries are never edges). A human
  following that item verbatim will author edges that never extract.
  See disc-runbook-and-verify-2.

## Discoveries

- discovery_id: disc-runbook-and-verify-1
  class: interface-drift
  description: |
    The full-suite gate fails a stale gptel presets spec:
    config/gptel/presets/test/workspace-assistant-spec.el, "registers no
    palette/agent tools yet (out of scope for this change)". It asserts
    the workspace-assistant preset carries no :tools, but the org-graph
    workspace integration now intentionally populates the preset's
    :tools with org-graph/agent-tools (the confirmed three-tool agent
    boundary). The old spec encodes the pre-org-graph contract and
    contradicts the register entry; it fails whenever org-graph is
    loaded in the full run. (Its failure message also crashes with
    "Not enough arguments for format string", masking the diff.)
  affected_register_entry: register/boundary/org-graph-agent-tools
  recommendation: |
    Update or delete the stale spec so the full suite reflects the
    confirmed boundary: workspace-assistant's preset :tools IS populated
    with the three org-graph tools. External to this change's file set —
    candidate for a .tasks/ item owned by the gptel presets subsystem.
- discovery_id: disc-runbook-and-verify-2
  class: vocabulary-mismatch
  description: |
    The runbook's "Capability: Typed edges" section (intro + "Author
    edges on 3-5 concept notes" item) still teaches PROPERTIES-drawer
    edge authoring (:IMPLEMENTS:/:CONTRADICTS:/... keys), while the
    implementation and the register entry speak only the EDGES-drawer
    vocabulary ("- <rel> :: [[id:...]]" items; PROPERTIES entries are
    explicitly never edges per extractor.el). The register entry's
    confirmed claim (a vault-top-level note with an EDGES drawer yields
    zero typed_edges rows) is correct; the mismatch is that the
    evidence-base runbook would have the evaluator author edges in a
    surface that no longer extracts anywhere, corrupting the RE-4
    findings. Pre-dates vulpea-human-commands, so left unedited here.
  affected_register_entry: register/invariant/typed-edge-extraction-scope
  recommendation: |
    Rewrite the Typed edges section's authoring instructions to the
    EDGES-drawer format in a small doc follow-up (the change that moved
    authoring off PROPERTIES should own it, or a .tasks/ item). The new
    vault-top-level check added by this task already uses the correct
    EDGES-drawer wording.
