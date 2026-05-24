## Context

The workspaces v1 MVP (archived as `add-workspaces-package`, 2026-05-24)
ships a correct round-trip but four user-visible gaps remain. The
prior-art catalog at `notes/activities-patterns-catalog.md` enumerates
28 patterns in activities.el (commit `5025962`) and recommends six as
high-priority ports. This change implements four of those six —
patterns 1+2 (buffer reincarnation), 5 (anti-save), 6 (etc-slot),
7 (two-state), plus pattern 8 (idle save) — bundled in a single
schema-v2 cycle.

Read order for this document: see *Decisions* for the choices that
shape the implementation. Each decision cross-references the catalog
pattern it derives from. *Risks / Trade-offs* and *Open Questions*
capture what we deliberately deferred.

## Goals / Non-Goals

**Goals:**

- Close gaps D4 (buffer reincarnation), D7 (autosave-on-switch) from
  the v1 MVP design.
- Add crash safety via opt-in idle save (no equivalent in v1).
- Pay the schema-version-bump cost once: v2 covers two-state slots,
  buffer-leaf enrichment, and the `:etc` open-ended slot.
- Match Emacs conventions where activities.el matched them; diverge
  only where activities' choice was tied to its single-default-and-last
  model (which workspaces doesn't have).
- Test discipline matches v1: Buttercup specs co-located with the
  module, scenarios in `specs/workspaces/spec.md` map directly to
  `it` blocks.

**Non-Goals:**

- External bookmark integration (catalog pattern 10). Workspaces will
  not appear in `bookmark-jump` output, will not register themselves
  in `bookmark-default-file`. Deferred to a follow-up change if there
  is demand.
- `workspace-rename`, `workspace-discard`, completion annotations
  (catalog patterns 25, 26, 23). Quality-of-life backlog, not v2.
- Per-workspace activities-style hooks (catalog pattern 9). Catalog
  flagged as medium-priority; deferred so v2 stays focused.
- `workspace-buffer-local-variables` capture/restore (catalog pattern
  11). Reserved as a defvar slot in the `workspace-buffer` struct
  (`:local-variables` field, nil by default), but no capture path is
  wired in v2. Users wanting to capture mode-specific buffer-locals
  can let-bind the variable themselves.
- Default-on idle save. Opt-in via `(workspaces-mode 1)`.
- Replacing or rebinding `kill-buffer` (Story A from v1 design D3
  remains intact).

## Decisions

### D1. Pattern bundling: one schema-v2 change, four improvements, five commits.

**Why:** All four improvements (A bookmark, B two-state, C idle, D
predicates) touch the persistence format or its read/write surface.
Bumping the schema once amortizes the migration cost. Catalog Q2
explicitly recommended this bundling.

**Commit slicing** (per user direction):

| Order | Commit | Bundles |
|---|---|---|
| 1 | `bookmark-reincarnation` | Foundation F2 (leaf walker) + Improvement A (A1-A3) |
| 2 | `two-state-layout` | Foundation F1 (schema v2) + Improvement B (B1-B3) |
| 3 | `anti-save-predicates` | Improvement D (D1) |
| 4 | `idle-save-mode` | Improvement C (C1) |
| 5 | `docs-and-sync` | Improvement W1 (docs, spec sync) |

The first two can land in either order — the schema-v2 migration
code lives with whichever ships first; the second commit accepts the
migration code as already present. Order chosen above puts buffer
reincarnation first because it is the more user-visible win.

### D2. Buffer reincarnation: internal bookmark records, external integration deferred.

**Why:** Catalog Q1 split. We use `bookmark-make-record` (the standard
Emacs API for capturing a buffer's restorable state) but store the
resulting record *inside our persistence file*, not in
`bookmark-default-file`. This:
- Avoids name collisions with the user's existing bookmarks.
- Avoids cluttering `bookmark-jump`'s candidate list with workspace
  member buffers.
- Keeps workspace persistence self-contained.

External bookmark integration (catalog pattern 10) is a separate
follow-up. When it lands, it would store one bookmark per workspace
(not per member buffer) using `bookmark-store`.

**Reincarnation fallback chain** (Requirement: Buffer reincarnation
across restart, catalog pattern 17):

1. Bookmark record (handles point, narrowing, non-file buffers via
   each major-mode's registered bookmark handler — e.g. magit,
   eshell, *Messages*).
2. Filename (`find-file-noselect`) — fast path for plain file
   buffers where the bookmark restore failed.
3. Buffer name (`get-buffer`) — when neither bookmark nor filename
   yields a buffer but a live buffer of the same name happens to
   exist in the session.
4. Error buffer — visible, named, harmless. Window slot is never
   left empty.

**Gotcha 1 — `help-mode` natively-compiled subr (Emacs `bug#56643`)**:
some help-mode bookmarks contain compiled subrs that cannot be `read`
back. The bookmark restorer wraps each leaf's bookmark restore in
`condition-case-unless-debug` so one failure does not abort the
whole window-state-put. The leaf falls through to step 2 (filename)
or step 4 (error buffer). Catalog pattern 1 notes the same
workaround in `activities--bookmark-buffer`.

**Gotcha 2 — `bookmark--jump-via` async-display race** (catalog
pattern 14): `bookmark--jump-via` calls a buffer-display function
synchronously as part of its restore protocol. If our
`window-state-put` runs in the same tick, the display function and
the state-put can race over the selected window. Fix: defer the
`window-state-put` via `(run-at-time nil nil ...)`. The state form
is `copy-tree`'d before being passed to the timer so the in-memory
registry is not mutated by `--bufferize-window-state`'s side effects
(catalog pattern 14, "load-bearing gotcha #4").

**Gotcha 3 — race against concurrent restores** (catalog Q7): if
the user mashes `workspace-restore` while the deferred restore is
pending, two `window-state-put` calls can fire against the same
frame. Guard via a generation counter:

```elisp
(defvar workspace--restore-generation 0)
(defun workspace--apply-saved-layout (name)
  (let ((gen (cl-incf workspace--restore-generation)))
    (run-at-time nil nil
                 (lambda ()
                   (when (= gen workspace--restore-generation)
                     ...))
                 ...)))
```

Stale deferred restores no-op when a newer one has been queued.

### D3. Schema v2 shape; no v1 migration.

**Layout shape** (was `(:timestamp T :frameset W :git-state G)`):

```elisp
(:timestamp T
 :saved-state W-saved        ; window-state, written only by workspace-save
 :working-state W-working    ; window-state, written by autosaves
 :etc nil)                   ; alist, forward-compat
```

**No migration code.** The workspaces package is pre-alpha; the
user accepted that the state-file format may break between
refinement cycles. The v2 reader checks `:version` and emits a
non-fatal notice + skips load when it sees anything other than 2.
Users with v1 files on disk delete
`state/workspaces/<machine-role>/workspaces.eld` before running v2
code. This is far cheaper than carrying a migration codepath
through a still-evolving schema.

This is the only place the v1/v2 split is mentioned in the
implementation. There is no fixture file, no migration test, no
in-memory `:frameset → :saved-state` translation.

**Window-state leaf enrichment** (catalog pattern 1+2):

Each leaf in `:saved-state` / `:working-state` carries a
`workspace-buffer` struct in its `parameters` slot:

```elisp
(cl-defstruct workspace-buffer
  (bookmark nil)        ; bookmark-make-record result; may be nil
  (filename nil)        ; file path if file-backed; else nil
  (name nil)            ; buffer-name at save time
  (narrowed-p nil)
  (indirect-p nil)
  (local-variables nil) ; reserved (D7 non-goal); always nil in v2
  (etc nil))            ; alist forward-compat
```

The walker (Foundation F2) is a pcase-based recursive descent over
the window-state form. The one-window-frame edge case
(`(leaf . attrs)` directly at the top level, no enclosing tree)
needs to be detected and treated specially — `cl-position 'leaf` on
the form yields the leaf attributes' position; if non-nil, the form
is a one-window frame and the walker translates just the leaf
attrs. Catalog pattern 2 has the exact recipe.

### D4. Two-state semantics.

**Writes** (Requirement: Per-machine persistence and restoration):

| Trigger | Writes to | Notes |
|---|---|---|
| `workspace-save` | `:saved-state`; clears `:working-state` | Synchronous flush |
| `workspace-save-layout NAME` | `:saved-state` of the named layout-group | Same as above |
| `workspace-switch-layout` | `:working-state` of the outgoing layout-group | Debounced flush |
| `workspace-new` home stamp | `:saved-state` of new `home` group | Synchronous |
| Tab switch advice | `:working-state` of the outgoing workspace's recent layout | Debounced |
| `workspaces-mode` idle timer | `:working-state` of the current workspace's recent layout | Debounced |
| `kill-emacs-hook` | `:working-state` of the current workspace's recent layout | Synchronous |

**Restore precedence**: `workspace--apply-saved-layout` prefers
`:working-state`; falls back to `:saved-state`. The user's
"reset" affordance is `workspace-revert`, which clears
`:working-state` and re-applies `:saved-state`.

**Why clearing `:working-state` on `workspace-save`:** the explicit
save is the user's "I'm happy, make this the baseline" gesture.
Without the clear, a subsequent restart would still resurrect the
stale `:working-state` (since it's preferred over `:saved-state`),
making the explicit save feel ignored.

### D5. Anti-save predicates: list of nullaries, short-circuit on success.

**Why nullary:** matches `active-minibuffer-window`'s shape and
`run-hook-with-args-until-success`'s contract. Predicates pull from
dynamic context (current frame, selected window) rather than
receiving parameters.

**Naming:** `workspace-anti-save-predicates` (matches activities'
`activities-anti-save-predicates`). Per catalog Q5, suffix
`-predicates` for predicate lists; `-functions` for hooks called
with arguments. We use `-predicates` here.

**Defaults**:

- `active-minibuffer-window` — built-in.
- `workspace--backtrace-visible-p` — new helper. Walks the current
  frame's windows; returns non-nil if any displays `*Backtrace*`.
  Activities has the same predicate at
  `activities.el:1010-1016`.

**Catalog pattern N6 non-goal**: activities also defaults
`activities-anti-kill-predicates` to include
`activities-buffer-special-p`, which treats *Messages*, *Help*, and
similar as "specially-restored." We do not adopt this — our
buffer-reincarnation chain handles those buffers via the bookmark
mechanism. Special-buffer skipping was a workaround for activities'
older save-by-name approach; with bookmarks, it is not needed.

### D6. Idle save: opt-in, 60s default.

**Why opt-in:** v1 users explicitly chose explicit-only persistence
when they signed off on the MVP. Auto-enabling a background timer
would change that behavior silently. The user toggles via
`(workspaces-mode 1)` or `M-x workspaces-mode`.

**Why 60s default:** activities defaults to 5s
(`activities-mode-idle-frequency`), which is aggressive for our
case because:
- Our autosave already fires on every tab switch, covering most
  intra-session changes.
- Idle save is specifically the "crash safety" net, not the primary
  persistence trigger.
- 60s of work is a tolerable loss window; 5s of disk-write churn is
  not.

The interval is a defcustom (`workspaces-mode-idle-frequency`),
trivially overridable.

### D7. The legacy buffer-files slot stays as-is.

The v1 `:buffer-files` slot (per-workspace absolute file path list)
is unchanged by this change. With bookmark-based reincarnation
(D2), the `:buffer-files` slot is *theoretically redundant* — the
window-state leaves carry filenames in the `workspace-buffer`
struct. But removing it would:
- Break the `workspace-remove-buffer` command's persistence contract
  (it currently mutates `:buffer-files`).
- Require a separate migration path for users who lean on it.

Treat as a known redundancy. A future change can consolidate by
deriving `:buffer-files` from the window-state on demand.

### D8. Testing approach.

**Framework**: Buttercup (matches MVP).

**Test layout** (new specs under `config/workspaces/test/`):

```
data-model-spec.el              — extended for workspace-buffer struct + v2 layout
layouts-spec.el                 — extended for leaf-walker + bookmark capture
persistence-spec.el             — extended for v2 migration + restore-precedence
buffer-reincarnation-spec.el    — NEW: bookmark / filename / name fallback chain
revert-spec.el                  — NEW: workspace-revert semantics
anti-save-spec.el               — NEW: predicate guard, default predicates
workspaces-mode-spec.el         — NEW: idle timer registration + cleanup
```

**Scenario mapping**: every `#### Scenario:` block in the spec
delta maps to at least one `it` block. Use `cl-letf` to mock at the
boundary: `bookmark-make-record`, `bookmark-handle-bookmark`,
`window-state-get` / `window-state-put`, idle-timer primitives. Do
not mock our own code.

**No migration tests.** The reader has a `:version` check; that's
covered by a unit test on the reader, not a fixture round-trip.

**Race tests**: the generation-counter guard in `workspace--apply-saved-layout`
needs a test that fires two restores with the second following the
first's deferred timer. Use `run-at-time` with explicit delays in
the test harness rather than relying on idle.

### D9. Keybindings.

`C-x w r` for `workspace-revert`. No `C-x w` prefix collision —
the current bindings under `C-x w` are `S` (save), `o` (restore),
`n` (new), `s` (switch), `R` (rename — reserved, see Non-Goals),
plus the `M-x` namespace for layout commands. `r` is free and the
mnemonic is natural.

`workspaces-mode` has no default binding; toggled via `M-x`.

## Risks / Trade-offs

| Risk | Mitigation |
|---|---|
| Bookmark restore can be slow for large layouts (many windows, each loading a buffer via bookmark handler). | Restore is already lazy per workspace (first tab-switch into a restored workspace). Within a workspace, the deferred `run-at-time` lets the buffer-display redraw breath. If a single bookmark handler takes too long, it blocks the rest of the tree — acceptable for v2; revisit if measurements show a hot path. |
| `bookmark-make-record` is not implemented for every major mode. Modes without a bookmark handler get nil bookmarks; restore falls through to filename/name. | Documented behavior. Users wanting full reincarnation for a custom mode register a `bookmark-make-record-function` on that mode (an existing Emacs extension point). |
| The `help-mode` `bug#56643` workaround is per-leaf, so the help buffer comes back as an error buffer rather than the original help. | Acceptable: help buffers are transient by nature. A future change can add a `help-mode`-specific deserializer that re-runs the help command from saved args. |
| Pre-alpha schema breaks mean state-file content is non-portable across refinement cycles. | Explicit user agreement: this is acceptable cost during pre-alpha. The eld is small enough to recreate by hand. |
| Idle-save tests are timing-sensitive in CI. | Mock the idle-timer primitive (`run-with-idle-timer`) and assert registration / arguments rather than waiting for real idle. |
| Anti-save predicates can be misused (user adds a predicate that always returns non-nil, then loses work because nothing saves). | The explicit `workspace-save` ignores the predicate list, so the user always has a manual escape hatch. Document the "saves are silent when blocked" behavior in the defcustom docstring. |
| Bookmark records embedded in window-state can grow the persistence file noticeably (each record carries the mode's bookmark data). | Measurement-driven: if a single workspace's state file exceeds a few hundred KB, look at compression or pruning idle workspaces. Not a v2 concern. |
| `tab-bar-select-tab` advice was the source of an MVP-blocking bug (D8 of v1). Re-introducing it is the riskiest part of this change. | The bug was specifically that the advice wrote to the *only* state slot, clobbering explicit saves. With two slots, the advice can only touch `:working-state` — clobber is structurally impossible. Anti-save predicates add a second line of defense. |

## Open Questions

1. **`workspace-revert` and the `:buffer-files` slot.** Should
   revert also restore `:buffer-files` from the registry at save
   time? Current spec says no (revert only touches the layout's
   state slots). But if the user invokes
   `workspace-remove-buffer` in their drift and then reverts, the
   removed buffer is gone from `:buffer-files` and not restored.
   *Probably accept as a known limitation; revisit if user
   complaint.*

2. **Idle-save default-frequency.** 60s is a guess. Users with
   short attention spans may want 30s; users with long sessions and
   slow disks may want 300s. Defer; the defcustom is the answer for
   now.

3. **Should `workspaces-mode` auto-enable at startup?** Catalog
   leaves this open. Current decision (D6): off by default. Could
   add an autoenable defcustom (`workspaces-mode-autoenable t`) in
   a follow-up if the user finds themselves toggling it every
   session.

4. **`workspace-buffer-local-variables` defcustom.** Catalog Q3
   recommended exposing this as a defcustom. Current decision
   (Non-Goal): keep it as an unused slot for v2; do not surface as
   defcustom. *Revisit if a user requests it.*
