---
name: update-docs-readme
description: Update config/workspaces/docs/README.org with new commands, scaffolding behavior, broken-home UX, gptel routing
change: add-workspace-home-directory
status: ready
relations: []
---

## Cycle 4 updates (cycle-20260526-191802)

### Status

- `status: blocked` → `status: ready`. Both blockers
  (`workspace-new-anchor-existing` merge `9cbba3e`,
  `workspace-delete-and-purge` merge `81001ee`) landed cycle-4. The
  user-visible command surface is now functionally complete.

### What landed in cycle 4 that the README must document

- `workspace-new` prefix-arg branch (`C-u M-x workspace-new`) for
  anchoring an existing directory. Three sub-cases:
  - Git repo + `home.org` → register only; no scaffolding.
  - Git repo, no `home.org` → scaffold files; no git ops.
  - Non-repo → full scaffold (mkdir, git init, files, initial
    commit).
  Plus double-registration guard via `file-equal-p` on normalised
  paths. Bound to `C-u M-x workspace-new`; no separate command.
- `workspace-delete` is now unregister-only (no filesystem effect).
  Bound to `C-x w D` (rebound from `workspace-delete-layout`).
- `workspace-purge` (new, destructive) deletes the home directory
  recursively. Scope-safeguarded: refuses to operate outside
  `workspaces-default-parent-directory` unless prefix-arg. Bound to
  `C-x w P`.
- The `workspace-delete-layout` command (previously on `C-x w D`)
  is now `M-x`-only — not destroyed, just unbound at the prefix
  keymap. Document the migration.

### Cycle-4 register-diff hits

None directly on this task (no register cites in body). The README
is downstream of the register's user-facing contracts; the cycle-4
reconciliations are all `confirmed` (no contract changes).

### Open asks (not docs-blocking, but mention if relevant)

- `ask-cycle-20260526-171719-1` (writer-lint heuristic) is a
  developer-facing concern; not README material.
- `ask-cycle-20260526-171719-2` (*scratch* fallback) is also
  developer-facing; not README material.

### Verification keybindings table

Audit `config/workspaces/docs/README.org`'s keybindings table for
ALL the cycle-3 + cycle-4 changes:
- `C-x w R` → `workspace-re-anchor` (cycle-3; was
  `workspace-switch-to-recent-layout`).
- `C-x w T` → `workspace-switch-to-recent-layout` (cycle-3
  relocation).
- `C-x w D` → `workspace-delete` (cycle-4; was
  `workspace-delete-layout`).
- `C-x w P` → `workspace-purge` (cycle-4 new).

The cycle-3 reviewer (workspace-delete-and-purge F-rule-out)
noted that the README still showed pre-cycle-3 bindings. Cycle 4
inherits that staleness; this task is the right place to refresh
everything.



## Files to modify
- `config/workspaces/docs/README.org` (modify — substantial new content)

## Implementation steps

1. Read the current `config/workspaces/docs/README.org` to understand
   its structure and tone. Match the existing voice.

2. Add or update sections covering the new behavior. Suggested
   structure (adapt to existing layout):

   **a) Top-of-file mental model update.** The first paragraph likely
   describes a workspace as a named context with layouts + buffer
   membership. Update to reflect the new shape: a workspace is also
   anchored to a filesystem directory (`:home`) that holds `home.org`
   and `sessions/`.

   **b) New: "Home directory" section.** Explain:
   - Every workspace has a `:home` (absolute path).
   - Default is `~/emacs-workspaces/<name>/`; override
     `workspaces-default-parent-directory` to change.
   - The home dir contains `home.org` (dashboard / display-name
     source) and `sessions/` (gptel session files).
   - `home.org` is yours — the package writes it once at creation
     and never modifies it after that.
   - `#+TITLE:` in `home.org` overrides the display name without
     changing the registry name (which is the directory basename).

   **c) Update "Creating workspaces" section** to document the two
   `workspace-new` flows:
   - No prefix arg → scaffold a new dir at the default parent.
   - `C-u workspace-new` → anchor an existing directory; three
     behaviors depending on whether the dir is a git repo and
     whether it has `home.org`.
   - The collision error and how to resolve it.

   **d) Update or add "Deleting and purging workspaces" section.**
   Be explicit that:
   - `workspace-delete` (`C-x w D`) unregisters only — files on disk
     are preserved.
   - `workspace-purge` (`C-x w P`) deletes the home dir; refuses to
     touch dirs outside the default parent without a `C-u` opt-in;
     prompts via `yes-or-no-p`.
   - The "deleted then re-anchored" workflow works because delete
     leaves files intact.

   **e) New: "Broken workspaces" section.** Describe:
   - What broken means (`:home` missing on load).
   - The `*Messages*` notice at startup.
   - `workspace-re-anchor` (`C-x w R`) to fix.
   - `workspace-purge` to remove the registry entry.

   **f) Update "gptel sessions" mention** (or add one if absent):
   - When on a workspace tab, new sessions land under
     `<:home>/sessions/`.
   - `C-u` on the session-creation command forces global save.
   - The `sessions/` directory contents are the source of truth —
     if you want a listing in `home.org`, add it manually; the
     package does not auto-render.

   **g) Update keybindings cheatsheet** with the new bindings:
   - `C-x w D` — workspace-delete
   - `C-x w P` — workspace-purge
   - `C-x w R` — workspace-re-anchor

   **h) Persistence schema bump callout.** A one-paragraph note that
   the persistence file format jumped to v3; old (v2) files are
   rejected with a `*Messages*` notice; pre-alpha, no migration —
   delete the file to start fresh.

3. If `openspec/specs/workspaces/spec.md`'s prose Purpose section
   has been kept in sync with the README in past changes, also touch
   it here so the two stay aligned. (The spec delta from this change
   only adjusts requirements; the Purpose paragraph may need a
   one-line update to mention `:home`.)

4. Sanity check that no broken links remain (search for any references
   to removed commands like a previous destructive `workspace-delete`
   or floating-workspace semantics).

## Design rationale

Docs land last so the user-visible surface is stable before being
described. This avoids re-writing docs mid-implementation when a
function name or binding shifts during apply.

The README is for end-users of the package; the spec is for changes.
The README mentions the spec by reference only — implementation
details and behavioral nuance live in `home.org`/`docs/README.org`,
not in `openspec/specs/`.

## Verification

- Visual diff: `git diff config/workspaces/docs/README.org`
- Render check: open the file in Emacs and confirm org headings parse
  cleanly (`C-c C-e t a` for ASCII preview, or just navigate).
- Cross-reference: every new command mentioned in the README exists
  in the codebase (`grep workspace-purge config/workspaces/*.el` etc.).
- No mentions of removed concepts: search README for "floating" and
  remove any stale references.

## Context

design.md § Risks / R1 — README note about sessions / home.org drift
specs/workspaces/spec.md § ADDED (all six new requirements)
specs/workspaces/spec.md § MODIFIED "Per-workspace home layout", "Per-machine persistence and restoration"


## Cycle 2 updates (cycle-20260525-213500)

### Status

- One of 5 blockers (`gptel-sessions-workspace-consult`) closed at
  merge `8ce82df`. Still blocked by 4 cycle-3+ tasks.

### Cycle-2 register-diff hits relevant to this task

When you ship the README update, these cycle-2 outcomes will need
to be documented:

- **Scaffold pipeline shape**: `workspace-scaffold HOME NAME &key
  INIT-AND-COMMIT?`. Document the six stages and the
  `INIT-AND-COMMIT? nil` knob for the anchor-existing-repo branch.
  Source: `register/boundary/workspace-scaffold-pipeline` (cycle-2
  reconciled).

- **Sessions directory helper**: `workspace--sessions-dir HOME` is
  the canonical attachment point for the `<HOME>/sessions/` path.
  README should not document this directly (it's a helper, not a
  user-facing API) but the section on "Where do my gptel sessions
  go?" should note that the path composition is uniform across the
  scaffold and the gptel consult.

- **gptel routing**: gptel-side function is
  `jf/gptel--target-sessions-root` in
  `config/gptel/sessions/filesystem.el`. Global default is
  `jf/gptel-sessions-directory`. The user-facing escape hatch is
  the separate command `jf/gptel-persistent-session-global` (NOT a
  prefix arg on `jf/gptel-persistent-session` — the existing
  command already binds `current-prefix-arg` to preset selection).
  Document both commands. **Status note**: the UX divergence is
  the subject of user ask `ask-cycle-20260525-213500-2`; pick up
  the user's disposition before writing this section of the README.

- **Broken-home behaviour**: the persistence v3 loader now tags
  missing-home entries `:broken` and logs to `*Messages*`. README
  should describe the user-visible recovery flow (re-anchor, purge,
  ignore — broken workspaces appear in the registry but activation
  is refused; cycle-3 task `broken-home-tolerance` ships the
  user-facing `workspace-re-anchor`).

- **Runtime-only tags**: `:broken` and `:restore-pending` exist on
  registry plists but never on disk. README's "Persistence schema
  v3" section should name both as runtime-only tags managed by
  helpers.

### Cycle-2 inline-fix hits

The cross-contract collision finding (orchestrator fix at `63d60ec`)
means scaffold does NOT call into gptel-sessions. README should not
promise such integration; cycle 3+ may add it via a gptel-side hook.

## Cycle 3 updates (cycle-20260526-171719)

### Status

- Still `blocked`. Two of four cycle-2 blockers cleared this cycle
  (`workspace-new-default-path` and `broken-home-tolerance` both done).
  Two remain: `workspace-new-anchor-existing` and
  `workspace-delete-and-purge` — both now `ready` after this cycle,
  so this task unblocks two cycles from now at the earliest.

### What's landed since this task was written that should be reflected in the README

The README rewrite should now include:

- **`workspace-new`** (no prefix arg) — scaffolds a new directory under
  `workspaces-default-parent-directory` (defcustom), with full git
  init + initial commit. Collision is a hard `user-error`.
- **`workspace-default-home-builder`** opens `<home>/home.org` (not
  `*scratch*`). Custom builders can override via
  `workspace-home-builder`.
- **`workspace-re-anchor`** (`C-x w R`) — recovery command for
  broken-state workspaces. Lockstep updates `:home` and `:name`
  (registry key matches new basename). Refuses non-existent or
  collision targets.
- **Activation guards** — `workspace-switch` and `workspace-restore`
  refuse broken workspaces (`:home` no longer exists on disk), with
  error messages naming both remediation commands.
- **Absolute-path enforcement** — persistence deserializer skips
  entries with relative-path `:home` (with `*Messages*` notice).
- **Key-binding reshuffle** — `C-x w R` now means re-anchor;
  `workspace-switch-to-recent-layout` moved to `C-x w T`.
- **Closed vocabulary** — switch/restore are refused on broken;
  re-anchor/purge/delete are permitted on broken.

### Open asks to mention or defer

Two cycle-3 spec-signal asks (writer-lint heuristic; *scratch*
fallback reachability) are open. README should NOT preempt the
user's disposition — write the docs against the current behaviour;
update later if the asks resolve to changes that affect users.

### Architect-tier follow-ups not user-facing

The architect's invariant-completeness audit (externalised to
`.tasks/audit-load-bearing-invariant-enforcement-completeness.md`) is
not user-facing; no README change required.
