## Context

The repository today loads two overlapping window/workspace systems:

- `perspective.el` (via `config/core/window-management.org`) — provides per-perspective buffer scoping and `consult`-friendly filtered buffer lists, but does not model window layouts as named, persistable artifacts and has no tab-bar integration.
- `activities.el` + a custom `config/activities/` extension layer — provides tab-per-activity visibility, suspend/resume, per-machine persistence, and a workflow shell that integrates projectile worktrees, org-roam documents, and gptel sessions. Its persistent state, however, is limited to a single "default" and "last" window configuration per activity; users cannot save and navigate an arbitrary set of named layouts inside an activity.

Both packages thus contribute a strict subset of the workflow the user actually needs:

```
                     activities + ext     perspective       gap (needed)
tabs in tab-bar           ✓                  ─                ✓
named layouts/activity    ╳ (default+last)   ╳                ✓ (unbounded)
buffer scoping            ╳                  ✓                ✓
"home" template           ~ (extension)      ─                ✓ (configurable)
per-machine persist       ✓                  ╳                ✓
workflow shell            ✓ (rich)           ─                deferred
```

`activities-extensions` is an early prototype layered on top of `activities.el`. It demonstrates the workflow shell idea (worktrees, gptel sessions, org-roam document linkage) but is constrained by the limits of `activities.el` itself and is not worth evolving. This change introduces a purpose-built `workspaces` package that supersedes both, and stages a hard cutover from the legacy modules.

Conceptually the data model is three-level:

```
Workspace ("code")                            ← tab in tab-bar
│   buffer membership (per-workspace set)
│   home-builder fn (configurable defcustom)
│   recent layout-group pointer
│
├── layout-group "home"                       ← named user-facing entity
│   └── layout @ <timestamp>                  ← MVP: exactly one per group
│
├── layout-group "magit"
│   └── layout @ <timestamp>
│
└── layout-group "tests"
    └── layout @ <timestamp>
```

In MVP, every layout-group holds exactly one layout, and the user-facing word for the entity is simply **"layout"**. The internal `layout-group` shell exists so that the deferred history feature (multiple timestamped revisions per group, browse/restore prior revisions, optional per-group git observations) can be added later without a persistence-file migration.

## Goals / Non-Goals

**Goals:**

- Replace `perspective` and `activities` + `activities-extensions` with a single, purpose-built `workspaces` package.
- Per-workspace named layouts (window configurations) with no upper bound.
- Per-workspace buffer membership: `switch-to-buffer`, `consult-buffer`, etc. show only the current workspace's members; cross-workspace sharing is allowed.
- `workspace-remove-buffer` for "I'm done with this here"; `kill-buffer` semantics unchanged (Story A).
- Configurable home layout builder per the user's customization.
- Per-machine persistence file mirroring the existing `activities` convention.
- Forward-compatible persistence schema: layout-group shell present from day one, so timestamped revisions and git-observation fields can be added without migrating saved state.
- Test discipline: Buttercup specs co-located with the module, one spec per requirement-set in `specs/workspaces/spec.md`, with scenarios from the spec mapping directly to `it` blocks.

**Non-Goals:**

- Multiple layouts (history/revisions) per layout-group in MVP UI. Schema slot present; no commands to surface it.
- Per-layout-group git observation (real or shadow commits). Schema slot present; no implementation.
- Migration tooling from existing `activities` state into workspaces. Hard cutover; users recreate workspaces by hand.
- Workspace-scoped winner-mode history. Out of scope.
- Promoting/sharing a buffer between workspaces via UI. Membership is acquired implicitly by displaying the buffer in a workspace's window; the underlying buffer can already be shown in multiple workspaces, no separate "share" command.
- Replacing or wrapping `tab-bar-mode` itself. Workspaces use stock tab-bar features.

## Decisions

### D1. Build on `tab-bar-mode` directly; do not use `activities-tabs-mode`.

**Why:** `activities-tabs-mode` ties tab lifecycle to `activities.el`'s default+last model, which is exactly the constraint we are escaping. Talking to `tab-bar-mode` directly gives us full control over tab creation/deletion, tab parameters (for storing workspace name and pointers), and the order of operations during tab switching.

**Alternatives considered:**
- *Subclass `activities-tabs-mode`* — would inherit the activities lifecycle assumptions; defeats the point.
- *Build our own tab UI on top of `header-line-format` or `tab-line-mode`* — `tab-bar-mode` already does what we need, with native muscle memory for users.

### D2. Use `bufferlo` for buffer-list scoping; pair one tab with one workspace.

**Why:** Buffer-list scoping is the load-bearing capability we want from `perspective.el`, and rolling our own filtered `buffer-list` / `consult-buffer` integration is non-trivial. `bufferlo` is actively maintained, hooks into the standard buffer-displaying primitives and into `consult`/`ido`/`ivy`, and supports per-tab buffer scopes natively. Since each workspace corresponds to exactly one tab (D1), bufferlo's per-tab scope == per-workspace scope without an extra adapter.

**Alternatives considered:**
- *`perspective.el`* — currently loaded; mature buffer scoping, but does not model named layouts and provides no tab-bar integration. Keeping it would mean re-doing the layout/tab work we already need to do on top of activities-style state. Net win is small after our other work.
- *`tabspaces`* — opinionated 1-tab-= 1-project bundle. Forces project.el coupling we do not want; workspaces are not necessarily project-scoped.
- *`beframe`* — per-frame scope; wrong axis for a tab-based UI.
- *Custom filtering on `buffer-list`* — feasible but adds maintenance surface and re-implements what bufferlo already integrates with completion frameworks.

**Consequence:** Adds `bufferlo` to `straight.el` dependencies. Removes `perspective`.

### D3. `kill-buffer` is left untouched; `workspace-remove-buffer` is the additive command (Story A).

**Why:** `C-x k` is universal Emacs muscle memory and packages that shadow it (e.g. `persp-mode`'s `kill-buffer` shadow) routinely surprise users in scripts, advice chains, and ERT teardown. Adding a sibling command preserves the principle of least surprise and keeps third-party code that calls `kill-buffer` directly working unchanged.

**Alternatives considered:**
- *Shadow `kill-buffer` (Story B)* — would more closely match the user's stated intent ("close in workspace A shouldn't affect B"), but the cost of overriding a primitive is higher than the cost of training one new command name.

**Consequence:** `workspace-remove-buffer` will be the binding under the `C-x w` (or similar) prefix; `C-x k` remains canonical kill.

**Implementation revision (2026-05-24):** Earlier iterations also installed a `kill-buffer-hook` that mirrored kill events into the registry — when any file buffer was killed, the file path was removed from every workspace's `:buffer-files`. The hook has been **removed**. It conflated two distinct events:

1. The user explicitly removes a buffer from a workspace (use `workspace-remove-buffer`).
2. The user kills a buffer for transient reasons — declutter, free memory, error recovery — and reasonably expects the workspace's *saved* file list to be unchanged.

The eager removal in case (2) broke the close-tab-then-restore flow: killing a buffer would drop the file from `:buffer-files`, and a subsequent `workspace-restore` would have nothing to pre-load, falling back to `*scratch*`.

`:buffer-files` is now only mutated by:
- `workspace-save` (the explicit user-facing save; syncs from bufferlo's current per-tab list)
- `workspace-remove-buffer` (the explicit "I'm done here" command)

Live-buffer membership for completion-filtering (`consult-buffer`, etc.) is still handled by bufferlo's own per-tab tracking, which naturally drops killed buffers from its list — but only in memory, never in the persisted `:buffer-files` slot.

### D4. Persist via `window-state-get/put` for window configs; reference buffers by file path.

**Original choice:** `frameset.el` for window configs (reasoning preserved below).

**Revised choice (2026-05-24):** `window-state-get` / `window-state-put`, applied to the selected frame's root window. This is what `tab-bar.el` itself uses internally to manage per-tab window state (see `tab-bar--tab`, which stores `(ws . ,(window-state-get ...))` per tab), and what `activities.el` uses for the same purpose.

**Why the revision:** `frameset.el` is designed to round-trip *entire Emacs sessions* (its primary client is `desktop.el`). On restore it matches frames by `frameset--id` and creates/reuses/deletes whole frames. In a per-tab restore context, the saved frame id never matches the live frame id (it's per-session), so `frameset-restore` does nothing useful for the current tab's window configuration. `window-state-put`, by contrast, operates on the selected frame's root window without frame-level side effects — exactly the granularity we need.

The legacy `:frameset` slot name in the persistence schema is retained (D5) for backward-compatibility with the existing format; the value stored in it is now a `window-state` form, not a `frameset` struct. A future schema version (`:version 2`) can rename it to `:window-state`.

**Buffer reference contract unchanged:** buffer membership is still tracked by absolute file path. Non-file buffers (e.g. `*Messages*`, magit buffers) cannot be restored across restarts — they will not be members on first activation post-restart; the user can re-add them by displaying them.

**Buffer reincarnation gap:** the current implementation pre-loads file buffers by path via `find-file-noselect` before `window-state-put` runs, so the saved window-state's buffer references resolve. Point position, narrowing, and major-mode-specific state are *not* preserved — buffers come back at point 1. activities.el solves this by embedding `activities-buffer` structs (bookmark + filename + name + buffer-local state) directly into each window-state `leaf`'s `parameters` slot, allowing reincarnation with full state inside `window-state-put`. Recommended as a follow-up enhancement for beta.

**Alternatives considered:**
- *`frameset.el`* — see "Original choice" above; wrong granularity for per-tab restore.
- *`current-window-configuration` + `set-window-configuration`* — opaque (not serializable to disk).
- *Custom serializer* — yak-shaving; `window-state-get/put` handles the cases we care about and is exactly what tab-bar uses.

**Consequence:** function names `workspace--capture-frameset` / `workspace--restore-frameset` are retained as internal names (now misnomers) to minimize churn; the underlying primitive is `window-state-get/put`.

### D5. Persistence schema is forward-compatible by design.

The on-disk layout (illustrative; not the user-facing API):

```elisp
(:version 1
 :workspaces
 ((:name "code"
   :recent-layout-group "tests"
   :buffer-files ("~/p/foo.el" "~/p/bar.el")
   :layout-groups
   ((:name "home"
     :layouts
     ((:timestamp 20260524093000
       :frameset <frameset-blob>
       :git-state nil)))            ; nil slot reserved for future
    (:name "tests"
     :layouts
     ((:timestamp 20260524141200
       :frameset <frameset-blob>
       :git-state nil)))))
  ...))
```

Every layout-group is always a list of layouts (length 1 in MVP). Every layout always carries `:timestamp` and a `:git-state` slot (nil in MVP). The future history feature is additive: it adds entries to `:layouts` and may populate `:git-state`; no schema rewrite, no migration.

**Note on the `:frameset` slot name:** per D4, the value stored in `:frameset` is a `window-state` form (the output of `window-state-get`), not a `frameset` struct. The slot name is retained for back-compat at `:version 1`. A future `:version 2` (planned alongside the two-state-model work; see D7) will rename it to `:window-state` and add a `:working-state` / `:saved-state` distinction to the layout shape.

**Why this shape:** Plists at every level for cheap pattern-matching; deeply nested but flat enough to navigate by hand if persistence ever needs forensic editing. A version field is included so a future v2 has a clean break path if it ever becomes necessary.

### D6. Reserve `home` as a layout-group name; configurable builder.

**Why:** Per spec (Requirement: Per-workspace home layout), every workspace gets a `home` layout on creation, produced by a user-configurable `workspace-home-builder` function. Reserving the name in code (rather than as a convention) keeps "delete the home" out of the command surface entirely, and makes the builder vs. overwrite-on-save distinction unambiguous (re-saving `home` overwrites the layout; the builder runs exactly once per workspace at creation).

**Builder signature (informal):**
```
(workspace-home-builder WORKSPACE-NAME) → unspecified
```
Runs with the new workspace active so any buffers it opens become members.

**Default builder:** opens a single `*scratch*` window. Users override with `(setq workspace-home-builder #'my/workspace-home-builder)`.

### D7. Explicit save in MVP; auto-save deferred to two-state model. **Known gap before beta.**

**Original intent:** Routine work should not require explicit "save layout" commands — that was a friction point with `activities.el`. Snapshot the outgoing layout's window config into its slot whenever the user switches workspaces or layouts.

**MVP behavior (revised 2026-05-24):** Auto-save on **workspace** context switch has been **removed**. Auto-save on **layout** context switch (`workspace-switch-layout`) is retained. The user-facing save commands are:

- `workspace-save` (`C-x w S`): captures the current frame into the current workspace's recent layout-group; syncs `:buffer-files` from bufferlo; writes the registry to disk synchronously.
- `workspace-restore` (`C-x w o`): materializes a saved workspace as a tab — switching to its live tab if one exists, otherwise creating a tab and applying the saved window-state and pre-loading buffer files.

**Why the revision was forced:** the layout schema (D5) holds exactly one layout per layout-group in MVP. With tab-switch autosave in place, the autosave would faithfully capture the *current* frame state — including any post-kill or transient state — and overwrite the user's explicit `workspace-save` snapshot. The trigger condition was easy to hit: `tab-bar-close-tab` internally calls `tab-bar-select-tab`, which fired the autosave on the outgoing tab, capturing whatever was left after killing buffers, and obliterating the prior save.

**Known gap (must close before beta):** Without auto-save on workspace switch, the persisted state can lag behind the user's working state. To restore auto-save semantics without re-creating the clobber bug, the layout shape needs a *two-state* split (activities.el-style):

- `:saved-state` — written by explicit `workspace-save` only. Authoritative across restarts.
- `:working-state` — written by autosaves (workspace-switch, idle-timer, kill-emacs). Tracks the current frame's drift.
- `workspace-restore` prefers `:working-state` if present, otherwise `:saved-state`.
- A new `workspace-revert` command resets `:working-state` from `:saved-state`.

This is a schema change (`:version 2`) and is tracked as a follow-up. The current MVP behavior — explicit save only — is *intentionally minimal* to ship a correct round-trip and is **not the steady-state design**.

**Implementation hook (current):** `workspace-switch-layout` calls `workspace--autosave-current-layout` before restoring the target layout (intra-workspace, still safe because the source and destination are sibling slots in the same workspace). `workspace-save` does the same plus a synchronous flush. There is no advice on `tab-bar-select-tab`.

### D8. Side-by-side development; single hard-cutover commit.

**Why:** The user accepted plan (b) from the discovery conversation: ship the `workspaces` package while `activities` and `perspective` remain loaded. The new package is exercised on its own keybindings (`C-x w` prefix is reserved here); legacy commands (`C-x C-a ...`, `C-c M-p ...`) remain functional. When the new package is stable, a single cutover commit removes legacy modules, dependencies, and the `gptel/sessions/activities-integration.org` loader. This avoids a "limbo" period where the user has neither a working old system nor a working new one.

**Tab discriminator (revised 2026-05-24):** Originally, workspace-owned tabs were tagged with a custom `workspace-name` tab parameter. Tab-bar's internal `tab-bar--current-tab-make` / `tab-bar-select-tab` (Emacs 30, `tab-bar.el` lines ~1352–1424) rebuild the current-tab alist from scratch on every switch and copy only the `name`, `explicit-name`, and optionally `group` keys — **any custom tab parameter is dropped on every tab switch.** That made the discriminator nil immediately after any selection.

The discriminator is now the tab's `name` slot itself. A tab is owned by workspaces iff its `name` matches a key in `workspace--registry`. `workspace--tab-workspace-name` does the registry lookup; `workspace--current-name` returns it for the selected tab. No custom tab parameter is involved, so the tab-bar drop-on-switch behavior is harmless to us.

Coexistence with activities-tabs-mode still works for the same reason: activities-owned tabs have their own names (prefixed `α:`), don't appear in `workspace--registry`, and are no-ops for workspace commands.

**Branch-local note:** On the `add-workspaces-package` branch, `persp-mode`, `activities-mode`, and `activities-tabs-mode` are explicitly disabled (`config/core/window-management.org`) to remove interference during MVP testing. The packages remain installed and their commands remain available via `M-x`; only the auto-activated modes are off. This is a temporary measure for diagnosis — the hard cutover commit (Migration Plan, Phase 2) is the permanent removal.

### D10. Explicit save and restore commands.

User-facing commands for persistence-as-deliberate-action:

- `workspace-save` (`C-x w S`) — capture current frame into the recent layout-group, sync `:buffer-files` from bufferlo, write the registry to disk synchronously. Errors when not on a workspace tab.
- `workspace-restore` (`C-x w o`) — completing-read over every workspace name in the registry (including saved-but-not-materialized ones). If a live tab exists for the chosen name, switch to it; otherwise create a tab, pre-load its `:buffer-files`, and apply its saved window-state.

These complement (do not replace) `workspace-switch`, which remains scoped to live tabs only.

The internal `workspace-save-state` function is retained as a debounced-flush primitive (still useful for the `:after workspace--autosave-current-layout` advice that schedules persistence after intra-workspace layout switches), but is no longer bound by default. The previous `C-x w S` → `workspace-save-state` binding was a usability trap because `workspace-save-state` flushes the in-memory registry without capturing the live frame — users hit "save" and silently persisted whatever stale snapshot was last captured.

### D9. Testing approach: Buttercup, co-located, behavioral-spec mapping.

**Why:** Per `CLAUDE.md`, Buttercup is the preferred framework for new tests. The `workspaces` spec is written in WHEN/THEN scenarios that translate naturally to Buttercup `it` blocks.

**Test layout:**

```
config/workspaces/test/
├── helpers-spec.el                 — shared fixtures/matchers
├── data-model-spec.el              — unit tests on workspace/layout/layout-group structs and pure functions
├── tab-integration-spec.el         — integration: tab-bar interactions
├── buffer-membership-spec.el       — behavioral: membership add/remove, kill-buffer interaction
├── layouts-spec.el                 — behavioral: save/switch/delete layout, recent pointer, auto-save
├── home-spec.el                    — behavioral: home builder, reserved-name handling
└── persistence-spec.el             — behavioral: save/restore round-trip with frameset
```

**Mocking strategy:** mock at the boundary with `cl-letf`, scoped to the function-under-test. Mocked surface: `tab-bar-*` primitives, `bufferlo-*` primitives, persistence I/O (`write-region`, `with-temp-file`). Avoid mocking our own code.

**Spec-to-test mapping:** every `#### Scenario:` block in `specs/workspaces/spec.md` corresponds to at least one `it` block. Scenario names map to `it` descriptions verbatim where practical, so future spec changes are grep-able.

**Run commands:**
- `./bin/run-tests.sh -d config/workspaces` — all workspaces tests
- `./bin/run-tests.sh -d config/workspaces -s` — with snapshot for regression tracking

## Risks / Trade-offs

| Risk | Mitigation |
|---|---|
| `bufferlo`'s per-tab scope is tighter than per-workspace if a single workspace ever wants multiple tabs. | The 1-tab-per-workspace invariant is structural in this design (D1, D2). If a future iteration wants multi-tab workspaces, we revisit by either using bufferlo's tab-group support if it lands, or layering our own membership table above bufferlo. |
| `frameset.el` restoration is lossy for non-file buffers (magit-status, *Messages*, `*scratch*`). | Documented in spec and design. On restore, only file-buffer membership is reconstituted; non-file members are forgotten and re-acquired on next display. |
| `tab-bar-switch-to-tab` advice can collide with other packages (e.g. activities-tabs-mode during the side-by-side period). | The advice is scoped: it checks for the `:workspace-name` tab parameter and is a no-op when absent. Activities tabs do not carry this parameter, so the advice does not interfere with them. |
| Auto-save on every context switch could be slow if `frameset-save` runs over a complex frame. | Auto-save serializes only the *active* layout-group's *current* layout in memory; the disk write to the persistence file is debounced (or batched into the kill-emacs-hook path) if measurement shows a hot path. |
| Loading `bufferlo` after a session has already created buffers may leave pre-existing buffers without workspace membership. | The package's init runs early in the module load order, before user-facing module code. `*scratch*` and `*Messages*` are explicitly added to the initial (single) workspace's membership on first creation. |
| `kill-buffer` removing a buffer from all workspaces (Story A) means a stray `C-x k` in workspace A loses that buffer from workspace B too. | Documented as the trade-off of Story A. `workspace-remove-buffer` is the safer "I'm done here" command and will be the recommended binding. |
| Reserved name `home` collides if the user already has activities or perspectives named `home`. | No collision: workspaces are a new namespace; legacy names never enter the workspaces persistence file. |

## Migration Plan

Two phases — neither requires user data conversion.

**Phase 1: Side-by-side development.**

1. Land the `workspaces` package as a new module (`config/workspaces/`) registered in `jf/enabled-modules`. `activities`, `activities-extensions`, and `perspective` remain loaded and functional. The user exercises both systems in parallel.
2. The `workspaces` package binds to a new prefix (proposed: `C-x w`) so it does not collide with `C-x C-a` (activities) or `C-c M-p` (perspective).
3. Persistence files live under `state/workspaces/<machine-role>/` — entirely disjoint from `state/activities/<machine-role>/`.

**Phase 2: Hard cutover (a single commit).**

When the new package is judged stable by the user:

1. Remove `config/activities/` (entire subtree).
2. Remove `activities/activities` entry from `jf/enabled-modules` in `init.org`.
3. Remove `(use-package activities ...)` and `(use-package perspective ...)` blocks from `config/core/window-management.org` (`winner-mode` and `ace-window` remain).
4. Remove `config/gptel/sessions/activities-integration.org` and the loader reference in `config/gptel/gptel.org`.
5. Drop `activities` and `perspective` from straight.el (they will no longer be required).
6. Archive `openspec/specs/activities-extensions.md` (it becomes obsolete).
7. State files under `state/activities/<machine-role>/` are not deleted by code; the user removes them by hand if desired.

**Rollback (within Phase 1):** Disable the `workspaces` entry in `jf/enabled-modules`. `activities` and `perspective` continue to work unaffected.

**Rollback (post Phase 2):** Standard git revert of the cutover commit restores the legacy modules and their state-file readers. Persisted workspaces state remains on disk and is ignored when the workspaces module is absent.

## Open Questions

1. **Keybinding prefix.** `C-x w` is a candidate (currently bound to `widen` via `C-x n w`, so the prefix `C-x w` itself is free). Alternatives: `C-c w`, `s-w`. Decide before binding to avoid double-rebinding later.
2. **Persistence file format.** Plain `read`/`print`-roundtrippable elisp form (this design assumes) vs. `persist.el`'s `persist-defvar` pattern (used by activities). The former is easier to inspect by hand and avoids an extra dependency; the latter aligns with the existing convention. Probably elisp form, but flag for review.
3. **Startup restoration behavior.** Should workspaces be eagerly recreated as tabs at startup, or lazily on first reference? Eager matches the persistence requirement most literally; lazy is cheaper at startup. Probably eager, but worth measuring restore time once the package exists.
4. **Interaction with `desktop-mode`.** Most users in this codebase do not use `desktop-mode`, but if a user enables it the two will both try to manage window state at startup. Likely resolution: explicitly document "workspaces and desktop-mode are mutually exclusive; pick one."
5. **Buffer membership for indirect buffers (clones).** `make-indirect-buffer` creates a new buffer that shares text but has its own name. Should it inherit membership from the source? Probably yes (the natural workflow is to clone a buffer of the current workspace), but worth a one-line decision before implementation.
6. **Whether the optional `architecture.md` artifact should be restored to the schema.** The installed `spec-driven-tasks` schema produces only 4 artifacts; existing changes in this repo were created with a 5-artifact schema that included `architecture.md`. This design absorbs architectural content; if the team decides to bring `architecture.md` back as a discipline, this design can be retroactively split.
