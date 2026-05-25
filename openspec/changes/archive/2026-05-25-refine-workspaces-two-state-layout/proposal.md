## Why

The MVP `workspaces` package (archived change `add-workspaces-package`,
landed 2026-05-24) ships a correct, minimal round-trip: tabs, named
layouts, per-workspace buffer membership, explicit `workspace-save`
(`C-x w S`), explicit `workspace-restore` (`C-x w o`). It is, however,
intentionally minimal in four user-visible ways that the original
design called out as known gaps before beta:

1. **Cursor positions, narrowing, and major-mode state are not
   preserved on restart.** `window-state-put` resolves window-leaf
   buffers by name; we pre-load files via `find-file-noselect` so
   the name lookup succeeds, but the underlying buffers come back
   at point 1, fully widened, with no buffer-local state.
   Non-file buffers (`*Messages*`, `magit-status`, `eshell`,
   `vterm`, `*scratch*` with edits) cannot be restored by name at
   all and fall back to `*scratch*`. Documented as gap **D4** in
   the archived design.
2. **No autosave-on-tab-switch.** Switching workspaces does not
   capture the outgoing workspace's window arrangement, so any
   intra-session rearrangement is lost unless the user remembers
   to `workspace-save`. Documented as gap **D7**.
3. **No crash safety.** If Emacs dies between explicit saves, the
   user loses any work done since the last save. The MVP's
   `kill-emacs-hook` flushes only the in-memory registry; it does
   not capture the current window state.
4. **Autosave (when re-enabled in #2) needs a safety guard.** The
   first user to switch tabs while a `*Backtrace*` window is up or
   while in the middle of a minibuffer prompt would have the
   debug/transient state captured into the workspace. Activities.el
   guards against this with anti-save predicates.

The structured exploration of `activities.el` (the legacy package
the workspaces MVP replaced) catalogued six high-priority patterns
that solve these problems
(`openspec/changes/refine-workspaces-two-state-layout/notes/activities-patterns-catalog.md`).
This change ports those patterns as the workspaces v2 layer, bundled
in one persistence-format change so the migration cost is paid once.

## What Changes

### Improvement A — buffer reincarnation (closes D4)

- **Persistence schema v2** (bundled with B; see below). Each
  window-state leaf's `parameters` slot grows a `workspace-buffer`
  entry: a `cl-defstruct` carrying a bookmark record, filename,
  buffer name, narrowed/indirect flags, and an `:etc` alist.
- **Capture side**: `workspace--capture-frameset` walks the
  window-state tree on save and embeds a `workspace-buffer` in each
  leaf. The bookmark record is produced via `bookmark-make-record`
  (the standard Emacs bookmark API). Modeled on
  `activities--window-serialized` (`activities.el:684-711`).
- **Restore side**: `workspace--restore-frameset` walks the saved
  window-state and replaces each leaf's buffer reference with a
  reincarnated buffer. The reincarnation chain is:
  1. Bookmark record (preserves point, narrowing, major-mode-specific
     state — and works for non-file buffers like `magit-status`,
     `eshell`, `*Messages*` because each has its own bookmark handler).
  2. Filename fallback (`find-file-noselect`) when the bookmark
     handler fails or no bookmark exists.
  3. Name fallback (`get-buffer`) when even the filename is gone.
  4. An error buffer (named, harmless) when all three fail, so
     restore never silently drops a window.
- **Gotchas captured from the catalog**:
  - `bookmark--jump-via` calls a buffer-display function as part of
    its restore protocol. We must `run-at-time nil nil ...` the
    `window-state-put` call to avoid racing against it
    (`activities.el:718-723`).
  - `help-mode` bookmarks may contain natively-compiled subrs that
    fail to read back (Emacs `bug#56643`). The bookmark restorer
    must trap the read error per buffer and fall through to the
    filename path (`activities.el:810-853`).
- **Replaces** the current pre-load step in
  `workspace--apply-saved-layout` (`find-file-noselect` loop) — the
  bookmark deserializer subsumes it.

### Improvement B — two-state layout slots (closes D7)

- **Persistence schema becomes `:version 2`**:
  - Each layout's single `:frameset` slot is replaced by:
    - `:saved-state` — the window-state most recently captured by an
      explicit `workspace-save` (or `workspace-save-layout`,
      `workspace-switch-layout`, or the home-builder stamp).
    - `:working-state` — the window-state most recently captured by
      an autosave (tab switch, idle timer, kill-emacs).
  - The `:git-state` placeholder slot is replaced by `:etc`
    (open-ended alist for forward compatibility, matching
    `activities-activity-state-etc`).
  - **No v1 migration code.** The workspaces package is pre-alpha;
    users with v1 state files on disk are expected to delete
    `state/workspaces/<machine-role>/workspaces.eld` and recreate
    workspaces by hand. The reader rejects non-v2 files with a
    non-fatal `*Messages*` notice.
- **`workspace-save`** now:
  1. Captures the current frame into `:saved-state`.
  2. Clears `:working-state` (the explicit save becomes the new
     clean baseline).
  3. Syncs `:buffer-files` from bufferlo (existing behavior).
  4. Flushes the registry synchronously (existing behavior).
- **Autosave on tab switch is re-enabled** via advice on
  `tab-bar-select-tab` / `tab-bar-switch-to-tab`. Writes only to
  `:working-state`. Cannot overwrite `:saved-state` by construction.
- **Restore precedence**: `workspace--apply-saved-layout` prefers
  `:working-state` when present; falls back to `:saved-state`. This
  is what makes restart land the user back where they left off, not
  at their last explicit save.
- **NEW**: `workspace-revert` (`C-x w r`) — clears `:working-state`
  on the current workspace's recent layout and re-applies
  `:saved-state`. The user's "undo my drift" affordance.

### Improvement D — anti-save predicates

- **NEW**: `workspace-anti-save-predicates` defcustom. A hook-style
  list of nullary predicates. If any returns non-nil at autosave
  time, the autosave is skipped.
- **Default predicates**:
  - `active-minibuffer-window` (built-in Emacs function) — skip if
    a minibuffer is active.
  - `workspace--backtrace-visible-p` — skip if a `*Backtrace*`
    window is showing. Modeled on
    `activities--backtrace-visible-p` (`activities.el:1010-1016`).
- **Guard application**: `workspace--autosave-current-layout` calls
  `run-hook-with-args-until-success` on the predicate list and
  returns early on a hit.
- **Does not affect explicit `workspace-save`**; the user's
  deliberate gesture is never blocked.

### Improvement C — idle save (crash safety)

- **NEW**: `workspaces-mode` minor mode. When enabled, runs an idle
  timer that captures the current workspace's `:working-state` every
  `workspaces-mode-idle-frequency` seconds (default: 60s).
- **NEW**: `workspaces-mode-idle-frequency` defcustom (number,
  default 60).
- The idle save respects the anti-save predicates (D); it is
  exactly the same code path as the tab-switch autosave, just a
  different trigger.
- **`kill-emacs-hook`** also captures `:working-state` once before
  flushing (existing flush kept; `:working-state` capture is new).
- Modeled on `activities-mode` and `activities-mode--killing-emacs`
  (`activities.el:499-525`).

## Capabilities

### New Capabilities

_(none — refinement of an existing capability.)_

### Modified Capabilities

- `workspaces`: revises four existing requirements and adds two
  new ones. Specifically:
  - **Auto-save layout on context switch** (modified) — workspace
    switch DOES auto-save under the v2 schema, into
    `:working-state`. The MVP-gap scenario is replaced by the
    inverse.
  - **Per-machine persistence and restoration** (modified) — schema
    version 2; new slot names; restore precedence
    (`:working-state` over `:saved-state`); new triggers
    (tab-switch advice, idle timer); read-time migration of v1
    files.
  - **Explicit save command** (modified) — `workspace-save` now
    clears `:working-state` after writing `:saved-state`.
  - **kill-buffer remains globally destructive** (modified —
    minor) — the `:buffer-files` saved file list is unchanged by
    this change, but the requirement is restated alongside the
    schema-v2 description so future readers see the full picture.
  - **NEW: Working-state revert** — the `workspace-revert` command
    and its semantics.
  - **NEW: Anti-save predicates** — the hook list, the default
    predicates, and the guard placement.
  - **NEW: Idle save mode** — `workspaces-mode`, its idle timer,
    and its interaction with the anti-save predicates.

## Impact

- **Modified**: `config/workspaces/data-model.org` — layout shape;
  accessors and constructors. New `workspace-buffer` struct + its
  accessors.
- **Modified**: `config/workspaces/layouts.org` —
  `workspace--capture-frameset` and `workspace--restore-frameset`
  walk the window-state tree; bookmark-based reincarnation chain;
  `workspace--autosave-current-layout` writes `:working-state`;
  anti-save guard.
- **Modified**: `config/workspaces/persistence.org` — schema version
  constant bumps to 2; read-time migration; new restore-precedence
  helper; re-introduced (clobber-safe) advice on `tab-bar-select-tab`
  for autosave-on-switch; `workspace--kill-emacs-flush` captures
  `:working-state`; new `workspace-revert` command.
- **NEW file**: `config/workspaces/workspaces-mode.org` —
  `workspaces-mode` minor mode + idle timer.
- **Modified**: `config/workspaces/workspaces.org` — `defgroup`
  hosts the new defcustoms (`workspace-anti-save-predicates`,
  `workspaces-mode-idle-frequency`).
- **NEW commands + bindings**: `workspace-revert` on `C-x w r`;
  `workspaces-mode` available via `M-x` and as a global mode.
- **Modified**: `config/workspaces/test/` — new specs for the
  reincarnation chain, two-state selection, schema migration,
  anti-save predicates, idle save, and `workspace-revert`. Existing
  layout/persistence specs revised to assert against the new slots.
- **Modified**: `openspec/specs/workspaces/spec.md` — synced via
  `/opsx-sync` when this change is ready to archive. The four
  modified requirements and three new requirements all land in the
  same sync.
- **No new dependencies**. `bookmark.el` is built-in.
- **State-file compatibility**: none. The package is pre-alpha; v1
  files are not read. Users delete
  `state/workspaces/<machine-role>/workspaces.eld` before running
  the v2 code and recreate their workspaces by hand. The reader
  surfaces non-v2 files via an `*Messages*` notice and continues
  startup with no workspaces.

## Implementation slicing

This change is implemented as **five tasks**, each landing as one
commit on the branch:

| Task | Bundles | Commit message hint |
|---|---|---|
| `bookmark-reincarnation` | F2 (leaf walker) + A1-A3 | "Reincarnate workspace buffers via bookmarks on restore" |
| `two-state-layout` | F1 (schema v2) + B1-B3 | "Split layout into saved/working state slots with autosave-on-switch" |
| `anti-save-predicates` | D1 | "Guard workspace autosave against debug and minibuffer states" |
| `idle-save-mode` | C1 | "Add workspaces-mode for periodic background save" |
| `docs-and-sync` | W1 | "Sync workspaces v2 spec, update README and design narrative" |

`bookmark-reincarnation` and `two-state-layout` can land in either
order — they share the schema-v2 migration code, which goes with
whichever ships first.
