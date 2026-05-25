---
name: update-docs-readme
description: Update config/workspaces/docs/README.org with new commands, scaffolding behavior, broken-home UX, gptel routing
change: add-workspace-home-directory
status: blocked
relations:
  - blocked-by:workspace-new-default-path
  - blocked-by:workspace-new-anchor-existing
  - blocked-by:workspace-delete-and-purge
  - blocked-by:broken-home-tolerance
  - blocked-by:gptel-sessions-workspace-consult
---

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
