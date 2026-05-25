# Activities.el patterns catalog

Source: `runtime/straight/repos/activities.el/activities.el` (1091 lines), branch
`master`, last commit `5025962` ("Meta: Reduce required Emacs version to 28.2",
dated 2025-11-18). Companion files surveyed: `activities-tabs.el` (226 lines),
`activities-list.el` (103 lines). `persist.el` (224 lines, version 0.8) was also
read because activities depends on it for on-disk variable persistence.

Workspaces source surveyed (under `config/workspaces/`):

- `data-model.el` (134 lines) — workspace / layout-group / layout plists.
- `tabs.el` (120 lines) — tab-bar integration. Registry lives here as
  `workspace--registry` (eq-hash on workspace name).
- `layouts.el` (152 lines) — `workspace-save-layout`, `workspace-switch-layout`,
  `workspace-delete-layout`, plus the autosave-into-recent-layout helper that is
  *not* currently wired to anything except `workspace-save`.
- `persistence.el` (239 lines) — eld file at
  `state/workspaces/<machine-role>/workspaces.eld`, schema version 1, debounced
  idle writer (`workspace-save-state`), `kill-emacs-hook` flush, lazy tab
  materialization on first selection.
- `buffer-membership.el` (80 lines) — `bufferlo` for in-memory per-tab buffer
  scoping; explicit `workspace-remove-buffer`. `:buffer-files` only sync'd on
  the explicit `workspace-save` path.
- `workspaces.el` (38 lines) — top-level loader and `C-x w …` keybindings.

For each pattern below: cite line numbers in activities.el (or the companion
file named), state the problem it solves, note whether workspaces already
addresses it, estimate porting cost, and recommend priority. **No
implementation code is proposed inline** — this is a survey, not a plan.

Sections:

- High-priority ports
- Medium-priority ports
- Low-priority / nice-to-have
- Explicit non-goals
- Open questions

---

## High-priority ports

### 1. `activities-buffer` struct + bookmark-based buffer reincarnation

- **What it does**: Replaces a buffer leaf in a serialized window-state with a
  rich record (`activities-buffer`) carrying the buffer's bookmark record,
  filename, name, narrowing/indirect flags, and selected buffer-local
  variables. On restore, the buffer is reincarnated by jumping to the bookmark
  — which means major-mode-specific machinery (eg. `Info-mode`'s node,
  `magit-status-mode`'s repo, `eshell`'s directory, `dired`'s subdirs and
  marks, `gnus`'s group/article) restores correctly, not just "the file was
  re-found".
- **Source**:
  - Struct definition: `activities.el:766-772`.
  - Serializer: `activity--serialize` at `activities.el:774-797`. Note the
    `:bookmark` slot uses `bookmark-make-record` wrapped in
    `condition-case` (lines 778-785) — for buffers not visiting a
    file/directory it intentionally swallows the "Buffer not visiting a file
    or directory" error and leaves `:bookmark` nil. `:filename` and `:name`
    are recorded unconditionally so the fallback chain (pattern 17) can
    operate.
  - Deserializer: `activities--deserialize` at `activities.el:799-808`.
    Dispatch order: bookmark → filename → name → error.
  - Bookmark restore: `activities--bookmark-buffer` at `activities.el:810-853`.
- **Problem solved**: D4 buffer-reincarnation gap. Our current
  `:buffer-files` list only handles file-visiting buffers, and even those lose
  point/narrowing/major-mode state because we feed them through
  `find-file-noselect` and then let `window-state-put` resolve by buffer name.
- **Workspaces status**: **Gap** (already documented as D4 in design.md).
  `config/workspaces/persistence.el:144-146` pre-loads files via
  `find-file-noselect` and `config/workspaces/layouts.el:6-12` uses
  `window-state-get/put` directly — no leaf rewriting.
- **Port cost**: **Large.** This is the central architectural rework. It
  requires:
  1. Defining a `workspace-buffer` cl-struct (or similar) parallel to
     `activities-buffer`.
  2. Adding a window-state leaf walker (pattern 2 below).
  3. Threading serialize/deserialize through `workspace--capture-frameset`
     and `workspace--restore-frameset`.
  4. Bumping the persistence schema version from 1 to 2 with a migration
     path (old layouts without leaf-embedded buffer records continue to
     work via the filename-fallback branch).
  5. Deciding whether bookmark records are persisted *inside* the workspaces
     eld file (simple, fully self-contained) or via the Emacs bookmark
     system (interop with `bookmark-jump`, but coupling to a moving target).
     **Recommendation**: persist inline; we do not want bookmark file
     contention.
- **Recommendation**: **Port now**, as part of (or before) the two-state
  refinement work in this change. The two-state model is largely pointless
  without proper reincarnation, because what you "go back to" when you switch
  layouts is currently a window grid pointing at *scratch* for any non-file
  buffer.
- **Notes / gotchas**:
  - The `help-mode-compiled-subr` workaround is described in a NOTE comment at
    `activities.el:813-817`, citing Emacs `bug#56643`. Summary: certain modern
    `help-mode` bookmarks serialize native-compiled subr objects, which
    cannot be `read` back. The reference is to burly.el's earlier discovery
    of the same problem. Activities' workaround is the `with-temp-buffer` +
    `save-window-excursion` + `condition-case` machinery in
    `activities--bookmark-buffer` (lines 827-841): jump to the bookmark in
    isolation, detect failure by checking whether the buffer actually
    changed, and if it didn't, return a synthetic error buffer (pattern 17).
    **We will hit this** as soon as a user's saved layout includes a help
    buffer. The defensive shape is mandatory, not optional.
  - `bookmark-jump` works by side effect, not return value, which is why the
    "did the temp buffer change?" check is the failure detector. This is
    fragile but it is the established idiom — don't reinvent it.
  - Indirect-buffer and narrowing state are noted as TODO in
    `activities.el:788` and only the *flags* are recorded in `:etc`; the
    actual narrowing bounds and base-buffer reference are not yet captured
    by activities itself. So this is not "free" from porting — there is
    still gap-on-top-of-gap if we want narrowing to round-trip.
  - Per-buffer-local-variables capture (pattern 5) hangs off this struct.

### 2. Window-state leaf walker (`activities--mapcar-window-state-leafs` family)

- **What it does**: Traverses an Emacs `window-state-get` blob, identifying
  leaf nodes (the per-window records) and applying a transform. Handles two
  shapes that `window-state-get` produces:
  1. **Multi-window**: a tree of vert/horz splits whose terminal nodes are
     `(leaf . ATTRS)` cons cells.
  2. **One-window frame**: the *root* is itself `(leaf …)` — there's no
     enclosing split. Activities special-cases this via `cl-position 'leaf
     state` and `cl-subseq` to find the leaf segment.
- **Source**:
  - Read-only walker: `activities--mapcar-window-state-leafs` at
    `activities.el:527-541`. Used by `activities--buffers-and-files` (line
    543) for the completion-annotation count and dirty-detection.
  - Serializer walker: `activities--window-serialized` at
    `activities.el:684-711`. Two co-recursive `cl-labels`:
    `translate-state` and `translate-leaf`. Same one-window-frame branch at
    lines 706-711.
  - Deserializer walker: `activities--bufferize-window-state` at
    `activities.el:728-764`. Three co-recursive labels: `bufferize-state`,
    `bufferize-leaf`, `translate-leaf`. Same one-window-frame branch at
    lines 759-764.
- **Problem solved**: Lets us rewrite buffer-leaf attributes (substituting our
  `workspace-buffer` records) without reimplementing `window-state-get`'s
  recursion.
- **Workspaces status**: **Gap.** `config/workspaces/layouts.el:6-12` treats
  the window-state as opaque.
- **Port cost**: **Small once the buffer struct exists** (pattern 1). Roughly
  three `cl-labels` walkers, ~40 lines each.
- **Recommendation**: **Port now**, as the substrate for pattern 1. Cannot
  reasonably be deferred — patterns 1 and 2 ship together.
- **Notes / gotchas**:
  - The one-window-frame case is a *real* edge case. Our naive
    `window-state-get/put` masks it because Emacs handles round-tripping
    transparently. As soon as we rewrite leaves, we have to handle both
    shapes, and the test corpus must include single-window layouts.
  - The walker pattern at `activities.el:534-538` uses
    `proper-list-p` plus `cl-position 'leaf state` to handle "is this list a
    `(leaf . _)` masquerading as a tree node?". Reuse the same idiom; do
    not try to be cleverer about it.
  - `pcase` discriminator `((pred atom) state)` and `((pred list) (mapcar
    #'translate-state state))` is how the recursion descends through
    interior tree nodes (vert/horz/window-divider). Atoms and `(KEY .
    ATOM)` pairs pass through untouched — these are the geometry/size
    attributes that don't need rewriting.

### 3. Window-parameter translators (`activities-window-parameters-translators`)

- **What it does**: Some window parameters in `window-state` are not directly
  `read`-able after round-tripping because their values include live objects
  (buffers, frames). `window-preserved-size` is the canonical case: its value
  is `(BUFFER DIRECTION SIZE)` where BUFFER is a buffer object. Activities
  defines a per-parameter alist of `(serialize . FN)` / `(deserialize . FN)`
  callbacks. The serializer turns `BUFFER` into `(buffer-name BUFFER)`; the
  deserializer turns it back via `get-buffer`. If the buffer doesn't exist,
  the deserialize step is wrapped in `condition-case-unless-debug` and on
  error the parameter is silently dropped (`activities.el:752-756`).
- **Source**:
  - Variable: `activities-window-parameters-translators` at
    `activities.el:191-200`.
  - Apply at serialize: `activities.el:699-703`.
  - Apply at deserialize: `activities.el:750-757`.
- **Problem solved**: Avoids "value of `window-preserved-size` cannot be
  printed" failures, and avoids `void-variable` / dead-buffer errors on
  restore.
- **Workspaces status**: **Gap.** We don't customize
  `window-persistent-parameters` and we don't translate any parameters.
  `window-state-get … 'writable` does *some* of this for us (the
  `'writable' arg tells Emacs to be conservative), but `window-preserved-size`
  is precisely the case where `'writable` still leaves a buffer-object
  reference. Users who use `display-buffer-alist` with `(preserve-size .
  …)` will hit this.
- **Port cost**: **Small.** ~25 lines for the variable + a couple of
  `pcase-dolist`-driven `let*` bindings inside the leaf walker.
- **Recommendation**: **Port now**, alongside patterns 1 and 2. The walker is
  already there; the translator dispatch is a free add-on.
- **Notes / gotchas**:
  - The translator alist is *open-ended*. If a major mode wants to participate,
    it can `add-to-list` an entry. Worth exposing as a defcustom.
  - The error-handling branch (drop the parameter on deserialize error) is
    important — without it a stale buffer reference becomes a startup-time
    crash. Workspaces' current `:safe` arg to `window-state-put`
    (`config/workspaces/layouts.el:11-12`) is similar in spirit but only
    covers `buffer` resolution, not parameter values.

### 4. `activities-window-persistent-parameters` (extended persistence list)

- **What it does**: Extends Emacs's built-in `window-persistent-parameters`
  for the duration of capture and restore. Activities adds: `header-line-format`,
  `mode-line-format`, `tab-line-format`, `no-other-window`,
  `no-delete-other-windows`, `window-preserved-size`, `window-side`,
  `window-slot`. All marked `'writable` (the value Emacs uses to mean
  "serialize the parameter via `prin1`").
- **Source**:
  - Defcustom: `activities.el:244-257`.
  - Binding (capture): `activities.el:679-682` — `let` shadows
    `window-persistent-parameters` for the duration of `window-state-get`.
  - Binding (restore): `activities.el:720-724` — same trick around
    `window-state-put`.
- **Problem solved**: By default Emacs does *not* persist mode-line/header-line
  overrides, window-side/slot (side-window placement), or "preserved size"
  semantics. Users with side windows (eg. `display-buffer-in-side-window`),
  preserved-size pinned windows, or buffer-specific mode-line overrides lose
  those properties on restore.
- **Workspaces status**: **Gap.** Our `workspace--capture-frameset` /
  `workspace--restore-frameset` (`config/workspaces/layouts.el:6-12`) use the
  global default `window-persistent-parameters`, so side windows are silently
  dropped on save/restore.
- **Port cost**: **Trivial.** A defcustom plus two `let` bindings. The walker
  changes from pattern 2 don't even need to know about it.
- **Recommendation**: **Port now.** Could ship independently of patterns 1-3 —
  pure win, no schema change required.
- **Notes / gotchas**:
  - `'writable` is the magic symbol — `t` (just truthy) would persist the
    parameter as the raw value, which can be unreadable. Always use
    `'writable` for parameters whose values are printable.
  - Side-window users specifically will notice this — those are typically
    treemacs / vterm / compilation buffers, which are common in our usage.

### 5. Anti-save predicates (`activities-anti-save-predicates`)

- **What it does**: A hook-style list of nullary predicates called before
  saving an activity's state. If any returns non-nil, the save is skipped.
  The two defaults are `active-minibuffer-window` (don't save while the user
  is in the middle of a minibuffer interaction) and
  `activities--backtrace-visible-p` (don't save when a `*Backtrace*` window is
  showing — likely an in-progress debug session whose layout you don't want
  burnt in).
- **Source**:
  - Defcustom: `activities.el:302-310`.
  - Application: `activities.el:580` inside `activities-save`. Uses
    `run-hook-with-args-until-success` (succeeds = "skip the save").
  - `activities--backtrace-visible-p`: `activities.el:1010-1016`.
- **Problem solved**: The autosave-on-switch path can fire at "wrong" moments
  — partway through a minibuffer prompt, partway through a debug session.
  Saving captures unwanted state.
- **Workspaces status**: **Gap (but partial mitigation).** Our autosave is
  currently disabled (per design D7); the only path that captures layouts is
  the explicit `workspace-save` and `workspace-save-layout`. When we *re-enable*
  autosave (which is the whole point of the two-state refinement), we will need
  these predicates. If we ship the two-state model without them, the first
  user to be debugging when they switch tabs will get an unwanted layout
  capture.
- **Port cost**: **Small.** A defcustom plus a `run-hook-with-args-until-success`
  guard inside `workspace--autosave-current-layout` (currently at
  `config/workspaces/layouts.el:22-37`).
- **Recommendation**: **Port now**, as part of the two-state refinement.
  Concretely: this is a precondition for safely re-enabling autosave.
- **Notes / gotchas**:
  - `active-minibuffer-window` is a built-in Emacs function (not custom). It
    returns non-nil while a minibuffer is active.
  - Activities checks predicates inside `activities-with`, *after* selecting
    the activity's frame (line 578). So predicates run in the context of
    the activity being saved. For us that's the workspace tab — easy to
    replicate because our autosave already runs in the tab being switched
    *away from* (see `workspace--autosave-current-layout` in `layouts.el:22`).

### 6. The `etc` open-ended-alist slot on both structs

- **What it does**: Both `activities-activity` (line 73-75) and
  `activities-buffer` (line 766-772) carry an `:etc` slot holding an alist for
  arbitrary forward-compatible data. Currently used for:
  - On `activities-activity-state-etc`: a `time` key recording the timestamp
    of the state capture (used by sort-by-age and completion annotation).
  - On `activities-buffer-etc`: `indirectp` and `narrowedp` flags
    (`activities.el:789-790`).
- **Source**: Struct defs above; usage examples at
  `activities.el:668` (state etc), `activities.el:587-588`
  (preserve time across re-save when buffers didn't change),
  `activities.el:917-918` (sort by age).
- **Problem solved**: Lets the package evolve without schema migrations.
  Adding a new per-state or per-buffer property is just "set an alist key".
- **Workspaces status**: **Partial.** We have `:git-state nil` on the layout
  plist (`config/workspaces/data-model.el:9`) which is a placeholder slot,
  but it's a single named slot, not an alist. Adding a second forward-compat
  property means another schema migration.
- **Port cost**: **Small.** Replace `:git-state` with `:etc nil` in
  `workspace--layout-make` and accessors. Schema version bumps from 1 to 2.
  Reader must tolerate either shape during migration.
- **Recommendation**: **Port now**, as part of any schema-version-2 work
  (specifically alongside pattern 1, since that's already going to bump the
  version).
- **Notes / gotchas**:
  - Activities uses `map-elt` / `map-insert` for alist access — `map.el`
    works uniformly on alists, plists, hash tables. Lets us swap the
    underlying representation later.
  - The `time` key is the key for "stale state detection" — without a
    timestamp on each saved state, we can't tell whether a workspace has
    been modified since last persist. Pattern 11 (idle save) and the
    completion-annotation pattern both depend on it.

---

## Medium-priority ports

### 7. Two-state model (`default` + `last`) and the explicit "save as default" gesture

- **What it does**: Each activity has two state slots:
  - `default`: the "saved by user, this is the canonical layout" state.
  - `last`: the auto-captured working-copy state, updated on idle and on
    suspend.
  `activities-resume` (lines 406-417) picks `last` by default, but with a
  universal-prefix arg, picks `default` (the "revert" gesture). Save commands
  distinguish what they update:
  - `activities-define` (line 359): creates the activity, saves *both* slots
    to the current state (`activities-save activity :defaultp t :lastp t`).
  - `activities-new` (line 381): same but only `:defaultp t`.
  - `activities-save-all` (line 453): periodic autosave, sets `:lastp t`
    only.
  - `activities-suspend` (line 429): saves `:lastp t` then closes the
    frame/tab.
  - `activities-revert` (line 467): no save, just calls `activities-set` with
    `:state 'default`.
- **Source**:
  - Struct slot definition: `activities.el:73-75`.
  - Save dispatcher: `activities-save` at `activities.el:573-594`.
  - State-loader dispatcher: `activities-set` at `activities.el:596-607`.
- **Problem solved**: D7 single-state-per-group. The user can "park" a
  canonical layout and still pin-point-restore later, while *also* having
  unsaved working state survive a switch-and-switch-back.
- **Workspaces status**: **Gap.** Documented as D7 in design.md and is the
  motivating concern for the change this catalog lives under
  (`refine-workspaces-two-state-layout`). Our `workspace--group-add-layout`
  (`config/workspaces/data-model.el:47-53`) *says it appends* but actually
  replaces — there is exactly one layout slot.
- **Port cost**: **Medium.** Two layout slots per group, a "which state to
  load" parameter on switch, schema migration, UI surface for "save as
  default" vs "the autosave path".
- **Recommendation**: **Port now** — this is the change itself. Catalog
  entry is here for completeness, not as a port-or-defer decision.
- **Notes / gotchas**:
  - The `activities-save` body at lines 583-588 has a subtle behavior:
    when re-saving `:lastp t`, if the buffers/files haven't changed, the
    *time* of the previous `last` is preserved. This prevents the
    completion-annotation age from resetting on every idle save when
    nothing semantically changed. Worth porting; the heuristic is
    `activities--buffers-and-files-differ-p` (line 555) — sets-of-files,
    with file-truename normalization.
  - The "no last state, fall back to default" branch is in
    `activities-set` at lines 604-607 — emits a `message` so the user
    notices the fallback. Subtle UX touch.

### 8. Idle-based autosave (`activities-mode` + `activities-mode-idle-frequency`)

- **What it does**: A global minor mode `activities-mode` starts a
  periodic idle timer that calls `activities-save-all`, which iterates over
  *active* activities and saves their `:lastp t` state. Defaults to 5
  seconds.
- **Source**:
  - Customization: `activities-mode-idle-frequency` at
    `activities.el:499-501`.
  - Mode body: `activities.el:504-517`.
  - `run-with-idle-timer` arg `t` (third arg) means "repeating idle timer" —
    fires once per idle window, then again after each new idle window.
  - `activities-save-all`: `activities.el:453-465`. Note the
    `(activities-saving-p t)` and `(activities-always-persist nil)`
    let-binding (lines 461-463) — this gates the "don't raise the frame
    when saving" branch in `activities--switch` (line 648-652) and
    suppresses the per-activity disk write, with a single
    `activities--persist persistp` at the end.
- **Problem solved**: Without periodic capture, the `last` state in pattern 7
  goes stale immediately after the user starts editing.
- **Workspaces status**: **Gap.** Our `workspace-save-state` is a debounced
  disk-writer (`config/workspaces/persistence.el:88-98`) triggered by
  `:after` advice on `workspace--autosave-current-layout`
  (lines 100-105) — but autosave-current-layout is currently only called
  from `workspace-save` (explicit) and from `workspace-switch-layout`
  (`config/workspaces/layouts.el:83`). There is no periodic capture.
- **Port cost**: **Medium.** A `define-minor-mode workspaces-mode`, a timer,
  and a `workspace-save-all-active` that walks the registry for tabs whose
  workspaces are live. The hard part is composing with patterns 5 (anti-save
  predicates) and the existing debounce.
- **Recommendation**: **Port now**, as a follow-on to pattern 7. The
  two-state model is *visible to the user* only via the idle save — that's
  what keeps `last` fresh.
- **Notes / gotchas**:
  - `run-with-idle-timer` with `t` repeat-arg fires *once per idle window*,
    not periodically. The user has to actually be idle. This is desirable —
    if you're typing, you don't want to be saved mid-keystroke.
  - The `activities-saving-p` dynamic-var pattern (line 202) is a poor
    person's "don't recursively re-enter the save path" guard. Worth
    copying if we have any side-effecting advice that might trigger on
    `tab-bar-select-tab` during the periodic save.
  - The `activities-always-persist nil` let-binding inside
    `activities-save-all` is a perf optimization: don't write to disk
    after every individual activity, just at the end. Our debounce
    already gives us something similar by coalescing into the idle
    timer, but if we want the same shape (capture-many-then-persist-once)
    we should add an equivalent dynamic variable.

### 9. `activities-after-resume-functions` / `before-resume-functions` /
       `after-switch-functions` hook system

- **What it does**: User-extensible hooks fired around resume and switch.
  Each is a list of single-argument functions called with the activity.
  `activities-tabs.el` adds its own
  `activities-tabs-before-resume-functions` (line 46) so the tabs
  integration can layer on top without forking the core hooks.
- **Source**:
  - Defcustoms: `activities.el:259-272`.
  - Invocation: inside `activities-switch` at line 427
    (`activities-after-switch-functions`). The before/after-resume hooks
    are wired by `activities-tabs.el:74` and the body of the tabs mode
    integration.
- **Problem solved**: Lets users limit `consult-buffer` candidates to the
  active workspace, track time in workspace, run project-specific setup,
  etc., without modifying the package.
- **Workspaces status**: **Gap.** No hook surface at all. We have advice on
  `tab-bar-switch-to-tab` and `tab-bar-select-tab`
  (`config/workspaces/tabs.el:117-118` and
  `config/workspaces/persistence.el:171-174`) but those are internal.
- **Port cost**: **Small.** Four defcustoms and four `run-hook-with-args`
  calls.
- **Recommendation**: **Port now or in the very next change.** Two-state
  refinement is a good time because the "before-load `last` vs `default`"
  decision point is a natural hook site (think: hook that wants to flag
  "we just reverted to default — clear my time-tracking timer").
- **Notes / gotchas**:
  - Distinguishing "switch" from "resume" matters: switch = activity was
    already active (a frame/tab existed); resume = activity was inactive
    (frame/tab is being recreated). Different hooks for different
    semantics. Map this onto workspaces:
    - "switch" = tab-bar-select-tab on an existing workspace tab.
    - "resume" = `workspace-restore` materializing a registered-but-untabbed
      workspace, or first selection of a `:restore-pending` workspace.
  - Keep the names symmetric — `workspace-before-resume-functions`,
    `workspace-after-resume-functions`, `workspace-after-switch-functions`.

### 10. Bookmark integration (`activities-bookmark-store`, prefix, handler)

- **What it does**: When `activities-define` creates a new activity, and the
  defcustom `activities-bookmark-store` is `t` (default), a system bookmark
  is created with name "`Activity: <name>`" whose handler is
  `activities-bookmark-handler`. That handler calls `activities-resume` on
  the activity. Result: activities appear in `bookmark-jump`,
  `consult-bookmark`, `marginalia`-annotated bookmark lists, etc.
- **Source**:
  - Defcustoms: `activities-bookmark-store` at `activities.el:228-238`,
    `activities-bookmark-name-prefix` at `activities.el:240-242`.
  - Store call: `activities.el:375-376` inside `activities-define`.
  - Handler: `activities-bookmark-store` (defun, confusingly same name as
    the defcustom) at `activities.el:1065-1072`; `activities-bookmark-handler`
    at `activities.el:1074-1077`.
- **Problem solved**: Cross-package discoverability — workspaces show up
  wherever bookmarks show up.
- **Workspaces status**: **Gap.** No bookmark integration.
- **Port cost**: **Small.** ~20 lines total. The wire-up is the
  `bookmark-store` call inside `workspace-new` and a top-level
  `;;;###autoload` handler function.
- **Recommendation**: **Port later** (medium priority). Genuinely nice but
  not core. Defer until after the two-state refinement and reincarnation
  work.
- **Notes / gotchas**:
  - Activities notes at `activities.el:478` (TODO comment in
    `activities-discard`) that the corresponding bookmark is **not** cleaned
    up on discard. So bookmarks can become stale. We should do better when
    we port — delete the bookmark on `workspace-delete` (which we'd need to
    add — currently we have `workspace-delete-layout` but no
    `workspace-delete` for the workspace itself).
  - Bookmarks live in the user's global bookmark file. Multi-machine users
    sharing bookmarks (eg. via syncthing) might be surprised by activity
    bookmarks appearing on machines where the activity doesn't exist.

### 11. Per-buffer-local-variables capture (`activities-buffer-local-variables`)

- **What it does**: A list of buffer-local variable symbols. When a buffer
  is serialized via `activity--serialize`, any variables in this list that
  are buffer-local-bound in the buffer are captured into the
  `activities-buffer-local-variables` slot, and on restore (after the
  bookmark jump), those variables are re-applied via `setf` on
  `buffer-local-value`.
- **Source**:
  - Defvar (intentionally not a defcustom, see below): `activities.el:184-186`.
  - Capture: `activities.el:791-797`.
  - Restore: `activities.el:836-839` (inside `activities--bookmark-buffer`).
- **Problem solved**: Major modes whose visible state is partially encoded in
  buffer-local variables (eg. `org-mode` folding state via
  `org-cycle-overlay-list`, or any of the language-server-modes' lsp
  workspace mappings) round-trip correctly.
- **Workspaces status**: **Gap.** No buffer-local-var capture.
- **Port cost**: **Small** if pattern 1 is in place.
- **Recommendation**: **Port later** — ship it after the bookmark
  reincarnation lands and is stable. It's strictly additive.
- **Notes / gotchas**:
  - Doc string says "Intended to be bound around code calling
    `activities-' commands" (line 186). That phrasing implies a `let`-style
    binding scoped to specific use cases, not a one-size global. The
    variable is intentionally a defvar, not a defcustom, suggesting the
    author wanted users to opt in mode-by-mode rather than globally.
  - There's a separate helper `activities--buffer-local-variables` at
    `activities.el:1079-1085` that returns an alist — appears to be
    dead code (not called from anywhere). Don't port.

### 12. Default workspace name function (`activities-default-name-fn`)

- **What it does**: Defcustom whose default is
  `activities--project-name`, which returns the current
  `project-current`'s name. Used as the default value for the `read-string`
  prompt in `activities-define`.
- **Source**:
  - Defcustom: `activities.el:274-280`.
  - Project helper: `activities--project-name` at `activities.el:1057-1061`.
  - Use: `activities.el:367-368`.
- **Problem solved**: When you're in a project, "make this an activity" is
  one keystroke (accept default).
- **Workspaces status**: **Gap.** `workspace-new` (`config/workspaces/tabs.el:66`)
  takes a name via `interactive "s"` with no default.
- **Port cost**: **Trivial.** Defcustom plus changing the `interactive` form.
- **Recommendation**: **Port later** (nice-to-have). Defer until after the
  two-state refinement.
- **Notes / gotchas**:
  - `project.el` integration assumes the user uses `project.el`. For users
    using `projectile`, the default fn should detect and use
    `projectile-project-name`. Worth letting users supply their own fn.
  - The defcustom shape (`:type '(choice (const :tag "No default name" …)
    (const :tag "Current project's name" …) (function-item :tag "Other
    function"))`) is a nice pattern — copy it.

### 13. `etc` on activity-state for richer per-state metadata (timestamp)

- **What it does**: See pattern 6 above for the open-ended slot mechanism.
  This entry calls out *specifically* the `time` key inside
  `activities-activity-state-etc` (set in `activities.el:668` via
  `(map-insert nil 'time (current-time))`), which drives:
  - Age-based sort (`activities-sort-by-active-age`, line 920).
  - Completion-buffer annotation showing age in human-readable form
    (`activities-completing-read`, lines 934-999).
  - "Don't reset time when re-saving identical buffers" optimization
    (lines 584-588).
- **Source**: cited above.
- **Problem solved**: User-facing "when did I last touch this workspace?"
  signal in `completing-read`.
- **Workspaces status**: **Partial gap.** We have `:timestamp` on each
  layout (`config/workspaces/data-model.el:8`) but it's not surfaced
  anywhere in the UI. Our completion menus
  (`config/workspaces/persistence.el:223-228`,
  `config/workspaces/layouts.el:55-57`, etc.) are vanilla
  `completing-read` with no annotations.
- **Port cost**: **Small** if we already have `:etc` (pattern 6). The
  annotation function is a self-contained ~30 lines.
- **Recommendation**: **Port later.** This is polish, not correctness.
  Worth doing once the two-state model exists because then the "dirty"
  indicator becomes meaningful (last differs from default).
- **Notes / gotchas**:
  - The age coloring (`activities-annotation-colors` at
    `activities.el:348-354`) is over-the-top; copy the structure but
    consider a simpler one-color scheme.
  - `activities--age` (line 890) is duplicated from `magit--age`. We
    could just `(require 'magit)` if magit is already a dependency
    (it is, per `config/major-modes/magit.org`). Worth the dep
    reuse.

### 14. `run-at-time nil nil ...` immediate-timer trick in `activities--windows-set`

- **What it does**: `activities--windows-set` (lines 713-726) does *not*
  call `window-state-put` directly. Instead it schedules a zero-delay
  timer that calls `window-state-put` once control returns to the event
  loop. The HACK comment (lines 715-717) explains: when restoring an
  activity by jumping to a bookmark via `bookmark--jump-via`, the bookmark
  system insists on calling a buffer-display function (eg. `pop-to-buffer`)
  *after* the handler runs. If we try to set the window configuration
  inside the bookmark handler, the bookmark system's subsequent
  buffer-display call will *clobber* our just-set window state. Deferring
  the `window-state-put` until after the current command finishes
  side-steps this.
- **Source**: `activities.el:713-726`.
- **Problem solved**: Bookmark-system / window-restore ordering race.
- **Workspaces status**: **Will be a gap once we port pattern 1.** Currently
  we don't use the bookmark system, so we don't hit this. The instant we
  start calling `bookmark-jump` inside our deserialize path, we will.
- **Port cost**: **Small.** A one-line refactor of
  `workspace--restore-frameset`.
- **Recommendation**: **Port now**, *as part of* pattern 1. This is the
  kind of cross-cutting gotcha that's invisible until it bites; the
  comment in activities.el is the load-bearing piece of documentation.
- **Notes / gotchas**:
  - The timer captures `(selected-frame)` and the state at the *time of
    schedule*, not at the time of execution. So selecting a different
    tab between schedule and execution is OK — the closure remembers.
  - `copy-tree` on the state (line 725-726) is important: bookmark jump
    can mutate buffer references in the state tree. Without `copy-tree`,
    the *stored* state in `activities-activities` gets corrupted.
  - The "HACK" disclaimer + `bookmark--jump-via` referencing an internal
    function is a smell. If Emacs upstream changes the bookmark dispatch
    contract this breaks. Maintainers should pin against this risk in
    docs.

### 15. Frame-buffer-list-based buffer membership (alternative to `bufferlo`)

- **What it does**: When `activities-kill-buffers` is enabled (defcustom,
  default `nil`), `activities-close` kills the buffers that are *only* in
  the closing frame's `buffer-list` frame-parameter. The set-difference
  computation walks `(frame-list)`, excludes the current frame, accumulates
  the union of the other frames' `buffer-list` parameters, and subtracts
  from the current frame's `buffer-list`.
- **Source**:
  - Defcustom: `activities-kill-buffers` at `activities.el:329-333`.
  - Implementation: `activities--kill-buffers` at
    `activities.el:1018-1031`. Note the inner predicate gate using
    `activities-anti-kill-predicates` (pattern 16).
  - Tabs variant: `activities-tabs--kill-buffers` at
    `activities-tabs.el:152-169`. Same logic on tab-bar-tabs.
- **Problem solved**: Lets users opt into "closing this activity should
  reclaim its buffer memory". Off by default — activities defers to the
  user's preference.
- **Workspaces status**: **Different design.** We use `bufferlo` for the
  same problem space, which maintains per-tab buffer lists in a way that
  doesn't require bookkeeping on the buffer-list frame parameter.
  Activities' `--window-configuration-change-hook` advice
  (`activities-tabs.el:144-150`) is doing manually what bufferlo does
  natively.
- **Port cost**: Activity's design — **non-applicable**. Bufferlo gives us
  the in-memory model. *However*, we have no opt-in "kill workspace
  member buffers on workspace delete" — see pattern 19 for that follow-on.
- **Recommendation**: **Non-goal for the membership mechanism.** We're not
  porting activities' way of tracking. But the user-facing *option* is
  worth offering (pattern 19).
- **Notes / gotchas**:
  - Frame `buffer-list` parameter only knows about buffers that have been
    *displayed* in that frame. Buffers killed before being displayed are
    invisible to it. This is one reason bufferlo is better — it tracks
    membership independent of display.

### 16. Anti-kill predicates (`activities-anti-kill-predicates`)

- **What it does**: Companion to anti-save predicates (pattern 5) but for
  the kill-buffer-on-close pathway (pattern 15). Each is a single-argument
  predicate called with a buffer; if any returns non-nil, the buffer is
  preserved. Defaults: `activities-buffer-hidden-p` (name starts with
  space) and `activities-buffer-special-p` (name starts with `*`).
- **Source**:
  - Defcustom: `activities.el:287-300`. Note the TODO comment at lines
    288-291 suggesting `activities-buffer-special-p` may be removed
    because it prevents magit buffers from being killed.
  - Predicates: `activities-buffer-special-p` at `activities.el:1033-1036`,
    `activities-buffer-hidden-p` at `activities.el:1038-1041`.
- **Problem solved**: Even with opt-in kill, you don't want to nuke
  `*Messages*` etc.
- **Workspaces status**: **Non-applicable** for the same reason as
  pattern 15 — but the same shape of predicate list is needed if we
  implement pattern 19.
- **Port cost**: **Small** if pattern 19 is implemented.
- **Recommendation**: **Defer** until pattern 19 is on the table.

### 17. Fallback chain for buffer reincarnation
       (`activities--name-buffer`, `activities--filename-buffer`,
       `activities--error-buffer`)

- **What it does**: When the bookmark-jump path fails (bookmark file no
  longer exists, bookmark handler errors, etc.) or wasn't present
  (`bookmark` slot was `nil`), the deserializer falls back to:
  1. `filename` → `find-file-noselect` (line 864-867).
  2. `name` → `get-buffer` or a synthetic "*Activities (error): NAME*"
     buffer explaining the failure (line 869-878).
  3. Bookmark-failure case → `activities--error-buffer` (line 855-862)
     showing a multi-line message with the failed bookmark record
     prin1-ed for the user to inspect.
- **Source**:
  - Dispatcher: `activities--deserialize` at `activities.el:799-808`.
  - Bookmark fallback (case 3): `activities--bookmark-buffer` lines
    842-853 inside an `if`.
  - Filename fallback (case 1): `activities--filename-buffer` at
    `activities.el:864-867`.
  - Name fallback (case 2): `activities--name-buffer` at
    `activities.el:869-878`.
  - Generic error buffer factory: `activities--error-buffer` at
    `activities.el:855-862`.
- **Problem solved**: Restore *always* produces a live buffer. Never errors
  out mid-restore leaving the layout half-built. The user can see *what*
  failed in the synthetic buffer.
- **Workspaces status**: **Gap.** Our restore at
  `config/workspaces/persistence.el:144-151` does
  `(ignore-errors (find-file-noselect path))` which silently maps to
  `*scratch*` on failure; the user has no visibility.
- **Port cost**: **Small once pattern 1 is in place** — these are 3 small
  helper functions.
- **Recommendation**: **Port now**, as the inevitable error-handling
  surface around pattern 1. Without it, the bookmark approach is worse
  than what we have (it can error in more ways).
- **Notes / gotchas**:
  - The error buffer's text (lines 851-852, 875-877) is *informative*:
    it tells the user which bookmark failed, what the error was, and
    what to do about it. Imitate that tone.
  - The error buffer name format `*Activities (error): NAME*` makes
    these buffers easy to find and bulk-kill later.

### 18. Compatibility-aware persistence layer (`persist.el`)

- **What it does**: Activities uses `persist-defvar` for
  `activities-activities` (line 182). `persist.el` provides:
  - One file per variable (in `~/.emacs.d/persist/` by default), so
    different versions of the same variable don't fight.
  - Atomic-ish write via `write-region` (lines 159-161 of persist.el).
  - Hooks for load (`persist-load-hook`, lines 55-60 of persist.el).
  - Equal-check on save: if value equals default, the file is deleted
    (lines 143-146 of persist.el).
  - Auto-save on `kill-emacs-hook` (lines 194-195 of persist.el).
  - Hash-table-aware copy and equality (lines 197-221 of persist.el).
- **Source**: `persist.el` (224 lines, version 0.8).
- **Problem solved**: Standardized variable persistence that handles edge
  cases like "what if the user changed the schema in a let-binding before
  exit?" (the persist-default mechanism preserves the original default).
- **Workspaces status**: **We have our own.** Our
  `workspace--write-state` / `workspace--read-state`
  (`config/workspaces/persistence.el:50-70`) is a plain
  `with-temp-file` + `prin1` / `with-temp-buffer` + `read`. We *do* have
  some advantages over persist.el:
  - Per-machine-role directory (`state/workspaces/<machine-role>/`)
    matching our existing convention.
  - Explicit schema-versioning via `:version 1` plist envelope.
  - Debounced disk writes via idle-timer.
  - One file, not one-per-variable (simpler to back up / sync).
- **Port cost**: **Effectively zero, but in the *opposite* direction**: we
  should *not* migrate to `persist.el`; we should keep our setup.
- **Recommendation**: **Explicit non-goal.** See non-goals section. The
  only thing worth borrowing from persist.el is the
  `with-temp-buffer` + `let print-* …` boilerplate (which we already do
  correctly).
- **Notes / gotchas**:
  - `persist.el` does *not* offer locking against concurrent Emacs
    instances. Two Emacs processes sharing the same persist dir is
    a documented foot-gun. Ours has the same property, so neither
    side wins on this axis.
  - The "delete file if value equals default" branch (lines 143-146 of
    persist.el) would actively *destroy* our state on every save of
    an empty registry — we don't want this.

---

## Low-priority / nice-to-have

### 19. Opt-in "kill member buffers on workspace delete"

- **What it does**: User-facing analogue of `activities-kill-buffers`
  (pattern 15), without porting the underlying mechanism (which we don't
  need because bufferlo handles tracking).
- **Source**: `activities.el:329-333`, mechanism at lines 1018-1031.
- **Problem solved**: When a user permanently discards a workspace, they
  often want its buffers released. Without it, they have to walk
  `bufferlo-buffer-list` manually.
- **Workspaces status**: **Gap.** We don't even have `workspace-delete`
  yet (only `workspace-delete-layout`).
- **Port cost**: **Small.** Once `workspace-delete` exists, a defcustom and
  a `(when workspace-kill-buffers-on-delete (mapc #'kill-buffer …))`
  inside the delete path. Anti-kill predicates (pattern 16) gate it.
- **Recommendation**: **Port later**, alongside the eventual
  `workspace-delete` command.
- **Notes / gotchas**:
  - "Kill orphans only" — buffers that are members of *other* workspaces
    should not be killed. Bufferlo's `bufferlo-buffer-list` per-tab
    semantics let us compute this cleanly.

### 20. `activities-resume-into-frame`: current vs new frame

- **What it does**: When `activities-tabs-mode` is disabled, lets the user
  choose whether resuming an activity reuses the current frame or makes a
  new one. Defcustom with two values: `current` (default), `new`.
- **Source**:
  - Defcustom: `activities.el:312-318`.
  - Use: `activities.el:645-647` inside `activities--switch`.
- **Problem solved**: Workflow preference — some users like dedicated
  frames, others reuse.
- **Workspaces status**: **Non-applicable in current form.** We are tabs-
  only by design. The analogue for us would be "switch to existing tab vs
  always create a new tab" but tab-bar UX already covers this (the user
  can `M-x tab-bar-new-tab` then `workspace-restore`).
- **Port cost**: **Trivial** if we wanted to expose
  `workspace-restore-into-tab` semantics; meaningful only if multi-frame
  becomes a goal.
- **Recommendation**: **Defer indefinitely.** Note for posterity.
- **Notes / gotchas**:
  - If we ever add multi-frame support, the same axis applies but with
    the user's `display-buffer-alist` taking precedence.

### 21. `activities-set-frame-name` and frame-title management

- **What it does**: When the tabs mode is *not* active, switching activity
  updates the frame title via `set-frame-name` to
  `(concat activities-name-prefix activity-name)`. The default prefix is
  `"α: "` (line 224).
- **Source**:
  - Defcustom: `activities-set-frame-name` at `activities.el:282-285`.
  - Application: `activities.el:653-654` inside `activities--switch`.
- **Problem solved**: External tools (window managers, `wmctrl`, etc.)
  can identify which activity an Emacs frame is showing.
- **Workspaces status**: **Non-applicable.** We're tab-bar-driven, and
  tab-bar already displays the tab name. Activities-tabs.el handles this
  via `tab-bar-rename-tab` (line 177) which we also do.
- **Port cost**: N/A.
- **Recommendation**: **Non-goal.** Frame title may still be worth setting
  for users running multiple Emacs instances or for window-manager
  workflows, but that's a per-user concern, not workspaces' problem.

### 22. `activities-list` buffer-mode UI (vtable-based)

- **What it does**: Tabular view of all activities with columns: active
  status, name, last-saved time, default-saved time. Sortable. Keybindings
  for resume/kill/suspend/discard. Implemented with the built-in `vtable`
  package.
- **Source**: `activities-list.el` (103 lines, especially lines 47-87).
- **Problem solved**: Discoverability and bulk management.
- **Workspaces status**: **Gap.** We have nothing equivalent.
- **Port cost**: **Small** (~100 lines, vtable is built-in to Emacs ≥ 29).
- **Recommendation**: **Port later — defer.** Lower priority than
  reincarnation, two-state, hooks, completing-read annotations. A
  `consult-workspace` would arguably be more idiomatic for our stack
  given we already use consult elsewhere.
- **Notes / gotchas**:
  - `vtable` requires Emacs ≥ 29. We require ≥ 30 (per `init.el` checks
    elsewhere), so this is safe.
  - The `activities-list-command` macro pattern (lines 39-45) — wrap a
    command, run it, refresh the buffer — is a clean idiom. Copy if we
    build a workspace list UI.

### 23. Completion annotations on workspace-completing-read

- **What it does**: See pattern 13. Specifically the per-activity
  annotation showing: active indicator (`@`), buffer count, file count,
  age (color-graded), and dirty indicator (`*` if `last` differs from
  `default`).
- **Source**: `activities-completing-read` at `activities.el:934-999`.
  The `activity-annotation-function` closure is lines 948-989; the
  `activity-table` completion table (lines 990-995) wires it via
  `(metadata (annotation-function . #'activity-annotation-function)
              (display-sort-function . ,activities-sort-by))`.
- **Problem solved**: User instantly sees activity health (active? recent?
  dirty?) when picking from `completing-read`.
- **Workspaces status**: **Gap.** All our completing-read calls are bare.
- **Port cost**: **Small.** The biggest chunk is computing
  buffers-and-files-per-state via the leaf walker (pattern 2), which we'll
  have by then anyway.
- **Recommendation**: **Port later.** Pure polish; ship after the
  structural changes are stable.
- **Notes / gotchas**:
  - Color blending (`color-rgb-to-hex` + `color-name-to-rgb`) is fine
    on graphical Emacs but degrades gracefully on TTY (since
    `face-foreground 'default` returns sensible values either way).
  - `display-sort-function` in the completion metadata is how
    `activities-sort-by-active-age` (line 920) gets respected.

### 24. `activities-with` macro for safe-frame-restoration

- **What it does**: `(activities-with activity body…)` evaluates `body`
  with `activity` active (its frame/tab selected), then unwinds back to
  the original frame/window/tab-index after `body` completes — even on
  error.
- **Source**: `activities.el:156-177`.
- **Problem solved**: Lets `activities-save-all` snapshot every activity's
  state without leaving the user's selected tab/frame somewhere weird at
  the end.
- **Workspaces status**: **Gap, but only matters when we add multi-tab
  iteration** (which we'd need for pattern 8 / idle save).
- **Port cost**: **Small.** The structure copies cleanly to tab-bar:
  remember `(tab-bar--current-tab-index)`, run body, restore.
- **Recommendation**: **Port alongside pattern 8** (idle save). They're
  paired: idle save needs to iterate over all active workspaces, which
  needs `with-workspace`.
- **Notes / gotchas**:
  - Activities' macro saves and restores frame, window, and tab-index
    (lines 161-164, 171-176). We only need tab-index. Simpler.

### 25. `activities-discard` (delete a workspace + its state permanently)

- **What it does**: Interactive `yes-or-no-p` confirmation, closes the
  activity if active, then `map-delete`s it from `activities-activities`.
- **Source**: `activities.el:475-486`.
- **Problem solved**: Permanent deletion.
- **Workspaces status**: **Gap.** No `workspace-delete` command.
- **Port cost**: **Trivial.**
- **Recommendation**: **Port soon.** Modest but useful, and a precondition
  for pattern 19.
- **Notes / gotchas**:
  - Activities' TODO at line 478 ("Discard relevant bookmarks") is the
    pattern-10 cleanup gap. Don't replicate it — clean up at delete time.

### 26. `activities-rename`

- **What it does**: Rename an activity. The interesting bit is the
  `map-delete + setf + map-elt` choreography (lines 399-402) to update
  both the map key and the struct's `:name` slot atomically.
- **Source**: `activities.el:392-403`.
- **Workspaces status**: **Gap.** No `workspace-rename`. Because our tab
  name is the workspace name, rename has to coordinate tab rename + registry
  rename + persistence.
- **Port cost**: **Small-to-medium.** Coordination across registry, tab,
  and `:buffer-files` references is the work. Layout-group references
  inside the workspace are by name only, so they survive.
- **Recommendation**: **Port later.**
- **Notes / gotchas**:
  - If we port pattern 10 (bookmarks), rename has to update the bookmark
    too. Activities doesn't do this — known limitation.

### 27. Activity-buffer comparison for "is this dirty?"

- **What it does**: `activities--buffers-and-files-differ-p` (line 555)
  computes whether two states differ semantically — using *normalized*
  file paths (`file-truename`, with remote-file passthrough at lines
  566-568). Drives the dirty indicator in `completing-read` annotations.
- **Source**: `activities.el:555-571`.
- **Workspaces status**: **Gap.** We track buffer-files but never
  compute set-differences.
- **Port cost**: **Small** if pattern 2 is in place.
- **Recommendation**: **Port later**, with pattern 23 (annotations).
- **Notes / gotchas**:
  - Remote file paths (`file-remote-p`) are explicitly skipped past
    `file-truename` to avoid hanging on a dead TRAMP connection. This is
    important — `file-truename` of a remote path can block for the TRAMP
    timeout.

### 28. `activities-tabs--switch-buffer` (per-workspace buffer switcher)

- **What it does**: A buffer-switching command scoped to the current
  workspace's buffer list. Builds a completion table predicated on tab
  parameter `activities-buffer-list`.
- **Source**: `activities-tabs.el:96-128`.
- **Workspaces status**: **Effectively covered.** `bufferlo` ships
  `bufferlo-switch-buffer` and related (`bufferlo-buffer-list`, etc.)
  which provide the same UX with better semantics. We just need to bind
  it.
- **Port cost**: **Zero** for the underlying capability. **Trivial** if we
  want a `workspace-switch-buffer` alias for discoverability.
- **Recommendation**: **Defer; bind bufferlo-switch-buffer to our keymap.**

---

## Explicit non-goals

These are patterns we should consciously **not** port and the reasons.

### N1. The `activities-tabs-mode` advice-override strategy

- **Source**: `activities-tabs.el:64-92`. The mode body builds an
  `override-map` of `(symbol . function)` pairs and `advice-add … :override
  …`s every one of them. The `activities--set`, `activities-current`,
  `activities-close`, `activities-switch`, etc. functions all get
  replaced wholesale when tabs-mode is enabled. Disabling the mode
  `advice-remove`s them. The activities core is *frame-oriented* and the
  tabs file *patches it* to be tab-oriented.
- **Why not port**: Our package is tab-bar-native from the start. The
  `:override` advice pattern is fragile (order-sensitive, hard to debug,
  collides with other packages that advise the same functions). Our
  design intentionally avoids it — design D1 in design.md is "tab is
  the workspace boundary, not the frame". Adopting any of the
  override-advice machinery would muddy this.
- **Note**: This *is* an example of how *not* to layer integrations in
  Emacs — useful for spec authors but not for our code.

### N2. `persist.el` as the persistence layer

- **Source**: `persist.el` whole-package (224 lines).
- **Why not port**: See pattern 18. Our schema-versioned, debounced,
  per-machine-role eld file is purpose-built and gives us advantages
  (schema versioning, machine-role isolation, single-file backup).
  Migrating to `persist.el` would lose the schema version envelope and
  give us per-variable files we don't need.

### N3. `activities-name-prefix` (the "α: " prefix on tab/frame names)

- **Source**: `activities.el:224-226`, applied in `activities-name-for`
  at `activities.el:1005-1008`.
- **Why not port**: A bare workspace name on the tab is fine. The Greek
  prefix is activities' branding. We can let users add their own
  prefixing via the after-switch hook if they want it.

### N4. The dual frame/tab API surface

- **Source**: Most of `activities.el` (`activities--frame`,
  `activities-resume-into-frame`, etc.) carries the frame-oriented
  history of the package. The tabs file is a late addition.
- **Why not port**: We are tabs-only. Don't carry the dead frame
  weight.

### N5. The duplicated `activities--age` magit-clone

- **Source**: `activities.el:880-909`, an explicit duplication from
  `magit--age` per the docstring at line 888.
- **Why not port**: We already depend on magit. Reuse `magit--age`
  directly if we end up wanting age-string-rendering for the
  completion annotation (pattern 23).

### N6. `activities-buffer-special-p` as a default anti-kill predicate

- **Source**: `activities.el:1033-1036`, default value of
  `activities-anti-kill-predicates` at line 292.
- **Why not port (as default)**: Even activities flags this as
  questionable (TODO at lines 288-291: "Not sure if it's really a good
  idea (e.g. it would prevent Magit buffers from being killed, and for
  no good reason I can think of)"). If we port pattern 16 we should
  start with a lighter default — just `activities-buffer-hidden-p`
  equivalent — and let users add more.

### N7. The single-default-and-last constraint

- **Source**: The whole two-slot design of
  `(cl-defstruct activities-activity … default last etc)` at line 73.
- **Why not port (as constraint)**: We *are* porting the two-state
  model (pattern 7), but our `layout-group` should remain capable of
  carrying an N-history later. Use `default` + `last` as the *MVP
  realization* of a slot list that can grow. Don't hard-code two slots
  into the struct shape — keep the `:layouts` plist key as a list.

---

## Open questions

These surfaced during the survey and need a user decision before we can
prioritize.

### Q1. Bookmarks: internal-only or via the Emacs bookmark system?

Pattern 1 (buffer reincarnation) uses bookmark *records* — the data
structure produced by `bookmark-make-record`. Two valid implementations:

1. **Internal**: store bookmark records inside our eld file, dispatch to
   `bookmark-handle-bookmark` on restore. No interop with `bookmark-jump`.
   Simpler, fully self-contained.
2. **External**: also register bookmarks in the Emacs bookmark file
   (`bookmark-default-file`), one per workspace member buffer. Lets
   users `bookmark-jump` to individual workspace buffers, but adds
   coupling to the bookmark file and creates name-collision risk.

The activities approach is *implicitly* internal (uses
`bookmark-make-record` to *capture* but stores the record inside the
activity, not in the bookmark file) for buffer reincarnation, and
*explicitly external* (uses `bookmark-store` to *register*) for activity-
level resume (pattern 10).

**Recommendation**: mirror this split. Internal records for buffer-level
reincarnation; external bookmarks (opt-in via defcustom) for
workspace-level resume.

### Q2. Schema version 2 timing

Pattern 1 forces a schema bump. Pattern 6 (open-ended `:etc`) is a
natural co-traveller. Pattern 7 (two-state) doesn't strictly require a
schema change (since `:layouts` is already a list) but it makes sense
to bundle.

Should we land a single schema-version-2 with patterns 1 + 6 + 7
together, or land them separately?

**Recommendation**: bundle. The migration code is the same, and three
schema bumps in three commits creates more migration surface than one.

### Q3. Should we expose `workspace-buffer-local-variables` as a defcustom?

Pattern 11 in activities is a defvar specifically *not* a defcustom,
implying the author wanted it bound in let-style. We could choose to
expose it as a defcustom with a sensible empty default — users can opt
in mode-specific variables.

**Recommendation**: defcustom with empty default. Document the intended
binding strategy in the docstring.

### Q4. Idle save vs. switch-triggered autosave

Activities does idle save (pattern 8). Our current design has
switch-triggered autosave (`workspace--autosave-current-layout` runs in
the pre-switch path). With the two-state model both make sense, but
they're not equivalent:

- Idle save: captures state periodically regardless of switching.
  Survives "user got distracted and Emacs crashed without ever
  switching".
- Switch-triggered: captures *exactly* the layout the user was looking
  at when they left.

**Recommendation**: ship both. The switch-triggered one is cheap (it
runs once per switch, not periodically) and gives perfect "where I left
off" semantics; the idle save guards against catastrophic loss.

### Q5. Hook-system naming

Pattern 9 — do we want `workspace-before-resume-hook` (lambdas) or
`workspace-before-resume-functions` (list)? Activities chose the latter.
The two are interchangeable but consistent naming with the rest of our
codebase matters.

**Recommendation**: match Emacs convention — `*-functions` for hooks
called with arguments. Quick `grep` of our own `config/` shows we already
use this pattern.

### Q6. Bookmark cleanup on workspace delete

If we port pattern 10 (bookmark integration), do we *automatically*
clean up the bookmark on workspace delete, or follow activities'
example and leave the orphan?

**Recommendation**: clean up. Activities' TODO at line 478 is a clear
known-bug; don't replicate it.

### Q7. What's the safety check on the `run-at-time nil nil ...` trick?

Pattern 14: the deferred `window-state-put` runs after control returns
to the event loop. If a second `workspace-switch` fires *during* that
event-loop window (eg. user is mashing keys), we could race against
ourselves. Activities doesn't seem to guard against this explicitly.

**Recommendation**: investigate during implementation. The naive guard
is a generation counter (each switch increments a counter; the timer
closure checks the counter before applying state).

---

## Footnotes

- Line numbers cite the master-branch snapshot of activities.el at
  commit `5025962126d140a7e26d36c3a2750bf4ff0bfd45` (2025-11-18,
  "Meta: Reduce required Emacs version to 28.2"). `activities-tabs.el`
  line numbers are stable in the same commit.
- **Pinning**: after the planned `cutover-remove-legacy` commit deletes
  the `use-package activities` block from
  `config/core/window-management.org`, straight.el no longer tracks
  the repo, so the snapshot stays at this commit unless someone
  manually `straight-pull-package`s it or removes the directory.
  Line-number citations remain valid for as long as
  `runtime/straight/repos/activities.el/` exists on disk.
- `persist.el` cites are against the package version 0.8, the version
  shipped with `straight.el` in our `runtime/` at the time of survey.
