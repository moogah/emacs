## Why

Today a gptel session is recognized, activated, and identified by **reverse-engineering its filesystem path**. A global `find-file-hook` (`jf/gptel--auto-init-session-buffer`) fires on *every* file open, matches `session.org` against three hardcoded layout regexes (branch / nested-agent / flat-agent), walks up the tree with `../..` / `../../..`, and derives the session identity from a directory basename (`jf/gptel--session-id-from-directory`). Nothing about the session is stored *in* the session — its name, branch, and type are properties of where it happens to sit on disk.

This is fragile to any reorganization (the newly merged workspaces package is one of several), couples identity to directory layout, and pays a per-open cost on unrelated files. The session's configuration (preset, scope) already lives self-describingly in its `:PROPERTIES:` drawer; only activation and identity still depend on the path. This change finishes that move: a session is recognized because it *contains* a gptel drawer, and is identified by *what the drawer says*, not by where the file lives.

This folds **Thread A** (binding & activation) and **Thread F** (identity off the path) from `.tasks/explore-working-directory-scoping.org`, whose exploration log records the converged design and the decisions ratified during `/opsx-explore`.

## What Changes

- **Content-addressed activation via `magic-mode-alist`.** Register a signature predicate that matches a `:PROPERTIES:` drawer at `point-min` carrying any `:GPTEL_`-prefixed property key. Because `magic-mode-alist` takes precedence over `auto-mode-alist`, a drawer-bearing `.org` file opens directly in `gptel-chat-mode` (still derived from `org-mode`, so org features are retained). Auto-adoption is preserved for old sessions, which already carry `:GPTEL_PRESET:`. The create and agent paths write complete file content (drawer + body) before `find-file[-noselect]`, so the signature is always present at open — no empty-buffer race.

- **Drawer-resident identity.** New drawer keys `:GPTEL_SESSION_ID:` and `:GPTEL_BRANCH:` (neither written today) become the authoritative source of a session's identity. Writers (`create-session-core`, `persistent-agent--task`, branch creation) emit them into the drawer they already render. Resolution is **drawer-first, basename-fallback**: present → use the drawer value; absent → fall back to the directory basename, so existing sessions keep working (beta / no-migration grace).

- **Discovery reads drawers.** `jf/gptel--init-registry` and the filesystem discovery helpers learn identity by a cheap head-read of each `session.org` drawer (reusing the activation signature's parse) instead of from directory names. The registry key `"session-id/branch-name"` is sourced from the drawer. Filesystem layout (`branches/`, `agents/`, the `current` symlink, directory names) becomes pure storage convention with no identity meaning.

- **Binding moves into the mode hook.** The buffer-local binding (`jf/gptel--session-id`, `--session-dir`, `--branch-name`, `--branch-dir`), registry registration, and symlink update move out of the global `find-file-hook` into a guarded function run from `gptel-chat-mode-hook`. The guard is "drawer carries a `:GPTEL_` key," not "buffer-file-name matches a session-path layout" (chat-mode can be activated on non-session buffers). `branch-dir` derives trivially from the file's own location; session type is inferred from `:GPTEL_PARENT_SESSION_ID:` presence (agent) vs absence (branch).

- **BREAKING (internal): retire the find-file-hook archaeology.** Remove the `find-file-hook` registration, `jf/gptel--auto-init-session-buffer`, and the three layout regexes + `../..` walks. Activation is no longer triggered by opening an arbitrary file; it is triggered by content recognition through `set-auto-mode`. (No on-disk migration — see constraints.)

## Capabilities

### New Capabilities
*(none — this reworks existing session behavior rather than introducing a new capability)*

### Modified Capabilities
- `gptel/chat-mode`: "Mode definition and activation" gains a content-addressed activation path — chat-mode now registers a `magic-mode-alist` signature (previously it registered no automatic activation and left `auto-mode-alist` to the user).
- `gptel/sessions-persistence`: "Session Identification" and "Registry" change from path/basename-derived identity to drawer-resident identity with basename fallback; discovery reads drawers rather than directory names.
- `gptel/sessions-branching`: "Configuration inheritance via drawer," "Session ID consistency via directory path," and "Auto-initialization of new branches" change — identity moves from the directory path into the drawer; new-branch activation is content-addressed, not find-file-hook-driven.
- `gptel/persistent-agent`: agent "Initialization" no longer depends on the `find-file-hook` auto-init pipeline; `find-file-noselect` triggers content-addressed activation, and the agent's identity/parent link are read from its drawer.

## Impact

**Code (literate `.org` → tangled `.el`):**
- `config/gptel/sessions/commands.org` — remove `find-file-hook` registration and `jf/gptel--auto-init-session-buffer` + layout regexes; add the magic signature predicate and the mode-hook binder; emit identity keys in `create-session-core`.
- `config/gptel/chat/mode.org` — register `magic-mode-alist`; wire the binder onto `gptel-chat-mode-hook`.
- `config/gptel/sessions/filesystem.org` — drawer head-read helper; `session-id-from-directory` becomes the fallback path; discovery helpers read drawers.
- `config/gptel/sessions/registry.org` — `init-registry` keys on drawer identity.
- `config/gptel/sessions/branching.org` — branch creation writes identity keys; new-branch activation via signature.
- `config/gptel/tools/persistent-agent.org` — `--task` writes identity keys; init relies on content-addressed activation.

**Tests:** `config/gptel/sessions/test/`, `config/gptel/chat/` (mode/menu specs). New coverage for the signature predicate (including false-match guards), drawer-first/basename-fallback resolution, drawer-based discovery, and mode-hook binding.

**Constraints carried from exploration (must hold):**
- gptel must remain operable with the **workspaces package absent** (one-way dependency).
- Prefer **self-describing** sessions over path reverse-engineering.
- **Beta, no-migration default** — on-disk formats may break; basename-fallback is the grace path, not a migration.

**Deferred to design.md (open sub-decisions):** agent identity scheme (own `:GPTEL_SESSION_ID:` vs parent-id + agent-name); whether the `current` symlink remains needed or becomes vestigial; signature anchoring against false matches (org files that merely quote `:GPTEL_PRESET:`); and `session-dir` derivation for open buffers once layout is non-semantic.
