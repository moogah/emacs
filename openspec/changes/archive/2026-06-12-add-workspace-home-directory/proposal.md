## Why

Workspaces today are "floating": each one has a name, buffer membership, and
window-state, but no filesystem anchor. That leaves workspace identity
trapped inside Emacs (no cross-machine portability, no place to keep
workspace-specific notes or gptel session files alongside the project), and
it forces every related concern — descriptions, session files, project
metadata — into either the binary persistence file or out-of-band ad-hoc
locations. Anchoring every workspace to a directory turns the workspace
into a portable, version-controllable artifact and gives ambient features
(gptel sessions, dashboard, future project integration) an unambiguous
place to live.

## What Changes

- **BREAKING**: every workspace MUST have a `:home` filesystem path. The
  "floating workspace" concept is removed. No migration code is shipped;
  pre-alpha users delete their persistence file (`*Messages*` notice as
  with v1→v2).
- `workspace-new NAME` (no prefix arg) creates `~/emacs-workspaces/<name>/`
  with `git init`, a generated `home.org` skeleton, a `sessions/` directory,
  one initial gptel session file (`<ISO-date>-initial.org`), and an initial
  `git commit` covering all generated content.
- `workspace-new NAME` with prefix arg (`C-u`) prompts for an existing
  directory to anchor:
  - already a git repo with `home.org` → register only; no scaffolding.
  - already a git repo without `home.org` → scaffold `home.org`, `sessions/`,
    and initial session; do **not** auto-commit (we never touch repos we
    did not initialize).
  - not a git repo → scaffold everything including `git init` and the
    initial commit.
- Workspace name is coupled to `basename(:home)` (registry key, tab name).
  `#+TITLE:` in `home.org` overrides the *display* name (mode-line, prompts,
  tab label) without changing the registry key.
- `home.org` is the user-authored dashboard / metadata file. The package
  writes it exactly once (creation skeleton); thereafter it is user space
  and the package only reads from it (live re-read, no caching).
- Sessions are filesystem-authoritative: `(directory-files <home>/sessions/)`
  is the source of truth. `home.org` may contain a `* Sessions` heading but
  it is static and user-curated — the package never auto-renders or syncs.
- When a workspace is active, new gptel sessions are filed under
  `<home>/sessions/` instead of the global gptel sessions directory. An
  escape hatch (prefix arg) preserves global-save behavior. gptel sessions
  module gains a *soft* dependency on workspaces (`featurep` check).
- Default `workspace-home-builder` becomes "find-file `<home>/home.org`"
  (was "switch to `*scratch*`"). The reserved layout name `home` and the
  filesystem concept "home" intentionally share naming — the home layout
  opens content from the home directory.
- `workspace-delete` is unregister-only (filesystem untouched). A separate
  `workspace-purge` command (or prefix arg variant) deletes the home
  directory and removes the registry entry.
- Workspace rename is via `#+TITLE:` (display-only). Real directory rename
  is out-of-band: `mv` the dir, restart, workspace re-appears under the new
  basename.
- Missing home dir on load logs a `*Messages*` notice and leaves a
  broken-state registry entry the user can re-anchor or prune. No auto
  recreate (silent data loss masking is worse than a visible broken state).
- Persistence schema bumps to v3: workspace plists gain `:home`. The reader
  rejects v2 with a notice (same pattern as the v1→v2 cutover).

## Capabilities

### New Capabilities

*(none)*

### Modified Capabilities

- `workspaces`: every workspace gains a required `:home` filesystem anchor;
  workspace creation scaffolds a git-initialized directory with `home.org`,
  `sessions/`, and an initial gptel session; the home layout opens
  `home.org` by default; gptel session creation files under `<home>/sessions/`
  when a workspace is active; persistence schema bumps to v3; `workspace-delete`
  becomes unregister-only with a separate `workspace-purge` for destructive
  removal.

## Impact

- **Code**:
  - `config/workspaces/data-model.org` — workspace plist gains `:home`;
    helpers for reading/setting it.
  - `config/workspaces/tabs.org` — `workspace-new` rewritten to scaffold;
    default `workspace-home-builder` re-targets `home.org`.
  - `config/workspaces/persistence.org` — schema v3 (add `:home`); v2 reader
    rejection with `*Messages*` notice; broken-home detection on load.
  - `config/workspaces/workspaces.org` — top-level loader, `defcustom`s for
    `workspaces-default-parent-dir` (`~/emacs-workspaces/`), default home
    builder targeting `home.org`.
  - New module(s) for the scaffold pipeline (dir/git/home.org/sessions
    creation) and `home.org` parsing helpers (live read).
  - `workspace-delete` / new `workspace-purge` command.
  - `config/gptel/sessions/` — session-dir resolution gains a workspace
    consult (soft dep via `featurep 'workspaces`); prefix-arg escape hatch
    to force global save.

- **Specs**: `openspec/specs/workspaces/spec.md` modified — new requirements
  for home dir, scaffolding, anchoring an existing dir, home.org contract,
  filesystem-authoritative session listing, gptel session routing,
  delete/purge split, broken-home handling, schema v3.

- **Persistence**: schema v3; v2 files rejected with a `*Messages*` notice
  and ignored. No migration code.

- **External commands invoked**: `git init`, `git add`, `git commit`.
  Failures must surface but not corrupt the in-memory registry.

- **Dependencies**: workspaces now invokes git as a subprocess. gptel
  sessions module gains a soft `featurep 'workspaces` check.

- **Deferred (explicitly out of scope)**:
  - Aggregator vs. single-repo type detection.
  - Worktree integration / sibling-vs-aggregator conventions.
  - Discovery commands beyond the `C-u` anchor flow.
  - `default-directory` / shell cwd binding to `:home`.
  - `project.el` integration.
  - Workspace-aware completion ranking for files under `:home`.
