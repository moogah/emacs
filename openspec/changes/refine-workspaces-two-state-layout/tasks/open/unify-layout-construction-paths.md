---
name: unify-layout-construction-paths
description: Funnel workspace-save-layout and workspace--capture-home-layout through workspace--autosave-current-layout :saved-state so the three explicit-save variants share one canonical construction path. Removes a latent shape-fragmentation that will activate when :etc carries content (per design.md §D5 deferred feature). Closes end-of-cycle architect Finding 3 + on-touch Finding 5 for two-state-layout.
change: refine-workspaces-two-state-layout
status: ready
relations:
  - "discovered-from:two-state-layout"
discovered_by: architect
discovered_class: shape-fragmentation
---

## Files to modify

- `config/workspaces/layouts.org` (modify) — change `workspace-save-layout` and `workspace--capture-home-layout` to invoke `workspace--autosave-current-layout :saved-state` (or a structurally equivalent helper) instead of constructing a fresh layout via `workspace--layout-make` + upsert. The copy-and-mutate path preserves prior `:etc` (and any `:working-state`, though that's cleared elsewhere for the explicit-save variants).
- `config/workspaces/test/layouts-spec.el` (modify) — add a cross-producer shape-equivalence test: pre-populate `:etc` on a layout via direct registry edit, then exercise the three explicit-save variants; assert each preserves the `:etc` value.

## Implementation steps

1. **Inspect the current divergence**:
   - `workspace-save` → `workspace--autosave-current-layout :saved-state` → COPIES existing layout, preserves `:etc`.
   - `workspace-save-layout NAME` → `workspace--layout-make` + upsert → REPLACES the whole layout, wiping `:etc`.
   - `workspace--capture-home-layout` (called by `workspace-new`) → same fresh-`layout-make` pattern → wipes `:etc`.

2. **Choose one resolution**:
   - **Option A** (recommended): unify by routing the two divergent producers through the copy-and-mutate helper. Pros: single canonical construction path; eliminates latent fragmentation; structural fix for the shape-fragmentation finding. Cons: the home-stamp case is constructing a layout for a freshly-minted workspace — there's nothing to copy. May need a small variation in the helper to handle the "no existing layout" case while still respecting the :saved-state slot routing.
   - **Option B**: document the divergence in `register/shape/layout-v2-plist` with explicit `producer_canonical` annotations and accept that `:etc` may differ across construction paths.

3. **If Option A**: extend `workspace--autosave-current-layout` to handle the no-existing-layout case for `:saved-state`. The current `t` arm of its cond already constructs a fresh layout via `workspace--layout-make captured`; the helper is already most of what's needed. Audit calls and confirm the helper handles "workspace has no recent layout-group yet" (the workspace-new home-stamp case is the only producer of a brand-new layout-group).

4. **Add the shape-equivalence test** in `layouts-spec.el`:

   ```elisp
   (describe "layout-v2-plist producer shape equivalence"
     (before-each (layouts-spec--reset))
     (it "preserves :etc across explicit-save variants"
       ;; Pre-populate :etc on a workspace's recent layout, then exercise
       ;; each of the three explicit-save variants and assert :etc round-
       ;; trips. Today :etc is universally nil; this test catches the
       ;; latent divergence that activates when :etc carries content.
       (cl-letf (((symbol-function 'workspace--flush-state) (lambda () nil)))
         (workspace-new "alpha")
         ;; Inject a known :etc value via direct registry mutation.
         (let* ((ws (gethash "alpha" workspace--registry))
                (group (workspace--find-group ws "home"))
                (layout (workspace--group-recent-layout group)))
           (plist-put layout :etc '((sentinel . cross-producer-test))))
         ;; Variant 1: workspace-save
         (workspace-save)
         (let* ((ws (gethash "alpha" workspace--registry))
                (layout (workspace--group-recent-layout
                         (workspace--find-group ws "home"))))
           (expect (plist-get layout :etc) :to-equal '((sentinel . cross-producer-test))))
         ;; Variant 2: workspace-save-layout NAME — preserves :etc when funneled through the canonical path
         (workspace-save-layout "magit")
         (let* ((ws (gethash "alpha" workspace--registry))
                (layout (workspace--group-recent-layout
                         (workspace--find-group ws "magit"))))
           ;; Currently FAILS — workspace-save-layout wipes :etc.
           ;; After this task: passes.
           (expect (plist-get layout :etc) :to-equal '((sentinel . cross-producer-test)))))))
   ```

5. **Reconcile** — update `register/shape/layout-v2-plist`'s `producer_fragmentation_note` to describe the resolution (or remove it if Option A unifies the producers).

## Design rationale

End-of-cycle architect Finding 3 + on-touch Finding 5 surfaced the same latent shape fragmentation. Today `:etc` is universally nil (design.md §D7's `:buffer-files` consolidation deferred), so the divergence is invisible. Tomorrow when `:etc` carries content (per design.md §D5's deferred per-layout-group git-observation feature), the three variants diverge silently — `workspace-save-layout` wipes the `:etc` value that `workspace-save` would have preserved.

The overlay's `architect.severity-overrides.duplication: blocking` flagged this as the kind of "literate-org duplication that benefits from a single canonical home" the project treats with prejudice. The fragmentation isn't duplication strictly — it's three producers that produce different shapes for the same logical operation — but the resolution (one canonical home for layout construction) is the same.

## Verification

```bash
./bin/tangle-org.sh config/workspaces/layouts.org
./bin/run-tests.sh -d config/workspaces
```

- The new shape-equivalence test passes (Option A) or is removed/relaxed (Option B).
- `grep -n "workspace--layout-make" config/workspaces/*.el` shows the producer surface (Option A: only `workspace--autosave-current-layout` calls it; Option B: continues to be called from `workspace-save-layout` and `workspace--capture-home-layout` with a documented producer note in the register).

## Context

- Finding IDs: `arch-cycle-20260524-200631-eoc-3` (end-of-cycle, advisory) and `arch-cycle-20260524-200631-on-touch-two-state-layout-5` (on-touch, advisory; same shape).
- Findings files: `.orchestrator/cycles/cycle-20260524-200631/findings/end-of-cycle-audit.md`, `.orchestrator/cycles/cycle-20260524-200631/findings/on-touch-two-state-layout.md`.
- Related register entry: `register/shape/layout-v2-plist` (reconciled in cycle 1 with a producer_fragmentation_note; this task either eliminates or formalizes that note).
- design.md §D7 (`:buffer-files` consolidation deferred) and §D5 (per-layout-group git observation deferred) are the future scenarios where the divergence would activate.
