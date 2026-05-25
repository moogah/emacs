---
name: docs-and-sync
description: Finalize the v2 refinement cycle. Update config/workspaces/docs/README.org with the new commands, key bindings, schema, and the bookmark/two-state/anti-save/idle stories. Sync the delta spec to openspec/specs/workspaces/spec.md via /opsx-sync. Refresh the in-tree references to the old D4 and D7 gap callouts in the archived design (leave the archive intact, but cross-reference from the new spec). Ready the change for /opsx-archive.
change: refine-workspaces-two-state-layout
status: ready
relations:
  - "blocked-by:bookmark-reincarnation"
  - "blocked-by:two-state-layout"
  - "blocked-by:anti-save-predicates"
  - "blocked-by:idle-save-mode"
---

## Files to modify

- `config/workspaces/docs/README.org` (modify) — full pass for v2: new commands, new schema, idle-save opt-in instructions, the predicates defcustom, the workspace-revert affordance. Drop any "MVP-gap" callouts that v2 fixes.
- `openspec/specs/workspaces/spec.md` (modify) — sync the delta from this change's `specs/workspaces/spec.md`. The MODIFIED requirements replace the v1 wording; the ADDED requirements are appended. The Purpose section gets a one-paragraph update noting v2 closed D4 + D7 and added crash safety; the "follow-up refinements in flight" sentence is removed.
- `openspec/changes/archive/2026-05-24-add-workspaces-package/design.md` — **NOT modified**. The archive is historical. The v2 spec's Purpose section is where the resolution-of-D4/D7 narrative lives going forward.

## Implementation steps

1. **Sync the spec.** Run the `/opsx-sync` flow manually (the skill is agent-driven). Specifically, for each block in `openspec/changes/refine-workspaces-two-state-layout/specs/workspaces/spec.md`:

   - **MODIFIED Requirements**:
     - "Auto-save layout on context switch" — replace the entire requirement body in `openspec/specs/workspaces/spec.md` with the v2 body (the "MVP-gap" scenario is removed; the inverse scenarios are added).
     - "Per-machine persistence and restoration" — replace the body. The v2 wording covers schema-v2, restore precedence, new triggers, and the non-fatal version-mismatch behavior.
     - "Explicit save command" — replace the body. The `workspace-save` contract grows the `:working-state` clear.
   - **ADDED Requirements** (append):
     - "Buffer reincarnation across restart"
     - "Working-state revert"
     - "Anti-save predicates"
     - "Idle save mode"

2. **Update the main spec's Purpose section.** Currently it says "Two follow-up refinements are in flight: a `:version 2` schema bump… and a bookmark-based buffer reincarnation port." Replace with:

   > The on-disk persistence schema is `:version 2`. Each layout holds a `:saved-state` slot (written only by explicit `workspace-save`) and a `:working-state` slot (written by autosaves). Buffer reincarnation across restart uses bookmark records embedded in window-state leaves so point position, narrowing, and non-file buffers (magit, eshell, *Messages*) survive the round-trip. Background idle save is available as an opt-in `workspaces-mode`.

3. **Update README.org.** Sections to add/revise:
   - **Persistence model** (new or expanded section): the two-slot story, the explicit-vs-autosave distinction, why both exist.
   - **Buffer reincarnation** (new section): how the bookmark chain works, what fails gracefully, the named error buffer for total failures.
   - **Commands** (revise): add `workspace-revert` and the `C-x w r` binding. Drop any documentation of MVP gaps.
   - **Configuration** (new or revised): the defcustoms (`workspace-anti-save-predicates`, `workspaces-mode-idle-frequency`) with example overrides. The `workspaces-mode` opt-in story.
   - **Pre-alpha note**: a small explicit callout that state-file format may break between cycles and the migration story is "delete the state file." Document this so future contributors don't add v1-migration code.

4. **Verify the spec validates.**

   ```bash
   openspec validate workspaces --specs
   ```

   Should be clean.

5. **Cross-link the catalog.** In the main spec's Purpose section, add a parenthetical pointing readers at `openspec/changes/refine-workspaces-two-state-layout/notes/activities-patterns-catalog.md` for the prior-art reference. The catalog stays in the change directory after archive — it's the authoritative source for "why we made these choices."

6. **Final sanity sweep.**

   ```bash
   grep -rn "MVP gap\|D4\|D7" openspec/specs/workspaces/spec.md config/workspaces/
   ```

   Should return little or nothing. The v2 spec replaces those callouts; the README should not still mention them. The archived design.md is allowed to keep them — that's history.

## Design rationale

The W1 task exists to make sure the cycle closes cleanly:

- The main spec is the source of truth for future readers; if it still talks about "deferred follow-ups" after v2 lands, the spec is wrong about itself.
- The README is what new contributors read first; it must reflect the current package, not the MVP.
- The catalog stays in the (now-archived) change directory as prior-art reference. We don't move it to the main spec; it's too long and too implementation-flavored.
- The archived `add-workspaces-package/design.md` is **left untouched** because archives are historical records. Modifying an archive to reflect later state would erase the trail of how we got here.

This task does NOT run `openspec archive refine-workspaces-two-state-layout` — that's a separate `/opsx-archive` invocation the user runs after this task closes, mirroring the add-workspaces-package archive pattern.

## Verification

```bash
./bin/tangle-org.sh config/workspaces/docs/README.org   # if README is .org
openspec validate workspaces --specs
openspec validate refine-workspaces-two-state-layout
./bin/run-tests.sh -d config/workspaces
```

Specific assertions:

- `grep -n "version 2\|:saved-state\|:working-state" openspec/specs/workspaces/spec.md` shows the new wording.
- `grep -n "workspace-revert\|workspaces-mode\|anti-save-predicates" openspec/specs/workspaces/spec.md` shows all three new requirements landed.
- `grep -n "MVP gap\|deferred\|v2 schema lands" openspec/specs/workspaces/spec.md` returns nothing — those forward references are resolved.
- The change is in the `[x] specs` state when `openspec status --change refine-workspaces-two-state-layout` is run.

## Context

- `openspec/changes/refine-workspaces-two-state-layout/proposal.md` — what changed and why.
- `openspec/changes/refine-workspaces-two-state-layout/design.md` — the technical decisions.
- `openspec/changes/refine-workspaces-two-state-layout/specs/workspaces/spec.md` — the delta to apply.
- `openspec/specs/workspaces/spec.md` — the target.
- `openspec/changes/archive/2026-05-24-add-workspaces-package/` — historical; do not modify.
- **Depends on** all four implementation tasks. This is the only task that touches the main spec; it should be the last thing on the branch before the change is archived.

## Cycle 1 updates (2026-05-24)

Major updates from cycle 1 (merges `32bbd36` + `8a9cb29` + inline fixes
`bc64784`, `b70a5b4`, `46afc31`). The README + spec sync need to absorb:

### Keybinding inventory correction (spec-signal from reviewer)

- `design.md §D9` stated "C-x w r" was free. It was not — `workspace-
  remove-buffer` was bound there in v1. The cycle's `two-state-layout`
  task relocated `workspace-remove-buffer` to `C-x w b` to free `r`
  for `workspace-revert`. Updated bindings:
  - `C-x w S` → `workspace-save` (unchanged)
  - `C-x w o` → `workspace-restore` (unchanged)
  - `C-x w n` → `workspace-new` (unchanged)
  - `C-x w s` → `workspace-switch` (unchanged)
  - `C-x w r` → `workspace-revert` (NEW, cycle 1)
  - `C-x w b` → `workspace-remove-buffer` (RELOCATED from `C-x w r`)
- README's keybinding table must reflect both the new binding and the
  relocation.
- `design.md §D9` should be updated to acknowledge the relocation
  (or sync should mention "the design's §D9 keybinding inventory was
  factually incorrect about r being free; the cycle resolved this by
  rebinding workspace-remove-buffer to b").

### Schema v2 documentation

- Persistence file format is now `:version 2`. v1 files are rejected
  with an `*Messages*` notice; users delete the file by hand. Document
  the no-migration policy (design.md §D3 — "the workspaces package is
  pre-alpha; the user accepted that the state-file format may break
  between refinement cycles").
- Layout shape has changed from single `:frameset` to `:saved-state` +
  `:working-state` + `:etc`. Document the two-state semantics and the
  restore precedence (working wins; saved fallback).

### Buffer reincarnation (D4 gap closed)

- Document the four-step fallback chain (bookmark → filename → name
  → error-buffer) and the bug#56643 workaround. Reference the
  `bookmark-make-record` API as the primary restore primitive.
- Note that non-file buffers (`magit-status`, `eshell`, `*Messages*`)
  reincarnate via their major mode's registered bookmark handler.

### v2 spec sync (the `/opsx-sync` step)

The delta spec at `openspec/changes/refine-workspaces-two-state-layout/
specs/workspaces/spec.md` should sync to the main spec at `openspec/
specs/workspaces/spec.md`. Note: the delta's **MODIFIED Requirement:
Auto-save layout on context switch** now correctly reflects the
shipped clobber-impossible structure (the v1 MVP-gap scenario is
inverted). The delta's **ADDED Requirement: Buffer reincarnation
across restart** is fully realized in cycle 1.

### Followup tasks now in the open queue

This task's `relations.blocked-by` originally listed four tasks. Two
landed in cycle 1; two remain (`anti-save-predicates`, `idle-save-
mode`). Cycle 1 also added four new in-scope follow-up tasks:
`test-switch-layout-race-guard`, `add-reincarnation-step-predicate`,
`unify-layout-construction-paths`, `remove-dead-after-tab-switch-
placeholder`. None of these block docs-and-sync; this task closes the
change once `anti-save-predicates` and `idle-save-mode` complete (and
optionally after the four cycle-1-followups land, depending on user
preference about archiving with vs. without them).

> Cycle 1: keybinding inventory + schema v2 + buffer reincarnation
> all landed and need documentation. See pm-digest at `.orchestrator/
> cycles/cycle-20260524-200631/pm-digest.md` (to be written) for the
> cycle's full output. The spec-signal asks routed to the user during
> integrate may also influence the docs (record the user's decisions
> in this task's README updates).

## Cycle 2 updates (2026-05-25, cycle-20260525-082618)

Cycle 2 landed the remaining two feature tasks and four cycle-1
follow-ups. **All `blocked-by` dependencies are now resolved**: the
task is structurally unblocked and ready for cycle 3 (or sooner). The
frontmatter's `blocked-by` list is left as historical record.

### Additional surface to document

Beyond the cycle-1 stanza's coverage of keybindings, schema v2, and
buffer reincarnation, the README + spec sync now need to absorb:

**Anti-save predicates** (cycle-2 `anti-save-predicates`, merge `93e833e`):

- New defcustom `workspace-anti-save-predicates`. Default value
  contains `active-minibuffer-window` (built-in) and
  `workspace--backtrace-visible-p` (new helper). If any predicate
  returns non-nil, the autosave is silently skipped.
- Explicit `workspace-save` **structurally bypasses** the predicate
  consultation (enters the autosave-guard pipeline at stage 2). This
  is the load-bearing invariant: a user invoking `M-x workspace-save`
  cannot be trapped by a misconfigured predicate.
- Three autosave call sites carry the wrap: tab-switch advice
  (`workspace--persistence-before-tab-switch`), kill-emacs hook
  (`workspace--kill-emacs-flush`), and idle-tick callback
  (`workspaces-mode--idle-tick`). The fourth listed entry in the
  cycle-1 boundary register entry (`workspace--persistence-after-tab-
  switch`) was determined NOT to be an autosave path during execute;
  the register entry was corrected at integrate.

**Idle save mode** (cycle-2 `idle-save-mode`, merge `53a7557` +
inline-fix `7f14276`):

- New module `config/workspaces/workspaces-mode.org` introducing
  `workspaces-mode` global minor mode (OFF by default). Loaded by
  the package but not enabled at startup; users opt in via
  `(workspaces-mode 1)` in `init.org` or `M-x workspaces-mode`.
- New defcustom `workspaces-mode-idle-frequency` (default 60 seconds;
  set to nil to disable the timer while keeping the mode loaded).
- The idle-tick callback is guarded by `(workspace--current-name)`
  AND by the anti-save predicate consultation. Crash-safety net only;
  explicit save + tab-switch autosave remain the primary persistence
  triggers.
- README needs a new "Idle save (opt-in)" section. The cycle-2
  implementor already drafted one in `config/workspaces/docs/
  README.org`; this task should integrate that draft with the cycle-1
  schema-v2 narrative.

**Closed-set discipline (reincarnation chain)** (cycle-2
`add-reincarnation-step-predicate`, merge `abffa09`):

- New `workspace--valid-reincarnation-steps` defconst and
  `workspace--reincarnation-step-p` predicate (mirrors the cycle-1
  `workspace--state-slot-p` pair).
- The reincarnation chain `(bookmark filename name error-buffer)` is
  now closed-set-enforced at the code level. Documentation should
  note this symmetry — adding a fifth fallback step is now a
  register-level change AND a defconst update (both must move
  together).

**Layout-construction funnel** (cycle-2
`unify-layout-construction-paths`, merge `df89ce2`):

- `workspace-save-layout` and `workspace--capture-home-layout` now
  funnel through `workspace--autosave-current-layout :saved-state`.
  `workspace--layout-make` is called only from inside the canonical
  helper.
- README's persistence-model section should note that all three
  explicit-save variants share one construction path — relevant if
  the design.md §D5 deferred `:etc` git-observation feature ever
  lands.

**Dead placeholder removed** (cycle-2
`remove-dead-after-tab-switch-placeholder`, merge `71281ab`):

- The `workspace--after-tab-switch` placeholder defun is gone.
  The live `workspace--persistence-after-tab-switch` advice (the
  lazy-restore path) is now the sole `:after` on
  `tab-bar-(select|switch-to)-tab`. No documentation impact for
  users (the placeholder was internal).

### Cycle-2 register status

All eight register entries this change has touched are now in a
stable disposition (5 reconciled, 3 confirmed). Reconciliation notes
live at `.orchestrator/cycles/cycle-20260525-082618/reconciliations/
*.md`. The docs/spec do NOT need to reference register entry IDs by
name — the register is internal architectural plumbing — but the
behavioral content the entries pin (anti-save bypass, idle-tick guard,
closed-set predicate symmetry, canonical layout producer) should all
be discoverable in README + spec prose.

### Two `.tasks/` follow-ups (not blockers)

Both arose from cycle-2 review findings and are independent of this
task's outcome:

- `.tasks/extract-workspace-explicit-save-into-group.md` — extract a
  named helper that `workspace-save` and `workspace-save-layout`
  share (post-funnel duplication).
- `.tasks/pin-explicit-save-clears-working-state-on-resave.md` —
  strengthen the cycle-2 cross-producer test to pin
  `:working-state` clear on re-save.

Neither blocks the docs-and-sync or the change archive; both are
parked for a future maintainer.

### Sync step adjustments

- The delta spec at `openspec/changes/refine-workspaces-two-state-
  layout/specs/workspaces/spec.md` should already have the cycle-2
  additions documented (anti-save-predicates, idle-save-mode requirements). If
  not, generate them from the cycle-2 implementor reports under
  `.orchestrator/cycles/cycle-20260525-082618/implementor-reports/`.
- The "ADDED Requirements" list in step 1 should now include four
  items (Buffer reincarnation across restart, Working-state revert,
  Anti-save predicates, Idle save mode) — confirm all four are
  present in both the delta and after-sync.

### Verification additions

```bash
# Cycle-2 surface checks
grep -n "workspace-anti-save-predicates\|workspaces-mode-idle-frequency" config/workspaces/docs/README.org   # both defcustoms documented
grep -n "workspaces-mode" config/workspaces/docs/README.org                                                  # opt-in story explained
grep -n "workspace--reincarnation-step-p" openspec/specs/workspaces/spec.md                                  # closed-set symmetry mention (optional but recommended)
```

> Cycle 2: anti-save + idle-save + funnel + closed-set predicate +
> dead-placeholder removal all landed and need documentation. See
> pm-digest at `.orchestrator/cycles/cycle-20260525-082618/pm-digest.md`
> for the cycle's full output. No new user-decision asks from cycle 2;
> the cycle's findings either inline-fixed, externalised to `.tasks/`,
> or queued as task-body / register documentation updates (this
> stanza absorbs them).
