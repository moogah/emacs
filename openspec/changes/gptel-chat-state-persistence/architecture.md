## Components

The change modifies three existing modules, deletes one, and reuses two upstream helpers.

**Modified:**

- **`config/gptel/chat/menu.el`** (chat-mode module): gains a before-save hook (`gptel-chat--save-state`) and extends mode-activation to overlay non-preset drawer properties and read `GPTEL_PARENT_SESSION_ID` (`gptel-chat--apply-drawer-overrides`).
- **`config/gptel/sessions/commands.el`**: `jf/gptel--auto-init-session-buffer` no longer reads `metadata.yml`; `jf/gptel--create-session-core` pre-populates the initial `session.org` content with a `:PROPERTIES:` drawer carrying `GPTEL_PRESET` (and `GPTEL_PARENT_SESSION_ID` for agents). All `(require 'gptel-session-metadata)` calls and metadata read/write sites are removed.
- **`config/gptel/sessions/branching.el`**: drops the `metadata.yml` copy step during branch creation. `branch-metadata.yml` handling is untouched.
- **`config/gptel/sessions/activities-integration.el`**: drops `(require 'gptel-session-metadata)` and replaces the `(fboundp 'jf/gptel--read-session-metadata)` existence guard with a guard on a function that still exists (e.g., `jf/gptel--register-session`).
- **`config/gptel/sessions/filesystem.el`**: removes `jf/gptel--metadata-file-path`.
- **`config/gptel/sessions/constants.el`**: removes `jf/gptel-session--metadata-file`.

**Deleted:**

- **`config/gptel/sessions/metadata.org` / `metadata.el`**: the entire module.

**Reused upstream:**

- **`gptel-org-set-properties`** (from `gptel-org.el`): called directly by the chat-mode save hook. Writes the upstream-compatible keys as deltas. No `GPTEL_BOUNDS` because we skip the `gptel-org--save-state` wrapper.
- **`gptel-org--entry-properties`** (from `gptel-org.el`): called directly by the chat-mode restore path to read the upstream keys. `GPTEL_PARENT_SESSION_ID` is read separately via `org-entry-get` because upstream's reader does not know about it.

## Interfaces

### `gptel-chat--save-state` (new)

- **Registration**: added to `before-save-hook` buffer-locally by `gptel-chat--install-preset-hooks` (already invoked from `gptel-chat-mode-hook`). No global hook registration.
- **Behavior**: when the buffer is a live chat-mode buffer, call `gptel-org-set-properties (point-min) nil` (msg suppressed). Immediately after, when `jf/gptel--parent-session-id` is non-nil, write it via `org-entry-put (point-min) "GPTEL_PARENT_SESSION_ID" jf/gptel--parent-session-id`. Guarded by `(derived-mode-p 'gptel-chat-mode)` as defense-in-depth.
- **GPTEL_BOUNDS**: explicitly never written.

### `gptel-chat--apply-drawer-overrides` (new)

- **Input**: current buffer. Reads via `(gptel-org--entry-properties (point-min))` for the upstream keys, then `(org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID")` for the chat-mode extension.
- **Behavior**: for each non-nil upstream field (system, backend, model, temperature, tokens, num, tools), install as buffer-local. When `GPTEL_PARENT_SESSION_ID` is present and non-empty, install `jf/gptel--parent-session-id` as buffer-local. Absent properties are left alone.
- **Caller**: `gptel-chat--apply-declared-preset`, which runs from `gptel-chat-mode-hook`. No longer called from session auto-init (unnecessary — mode activation already covers it).

### `gptel-chat--apply-declared-preset` (extended)

- **Existing behavior** (unchanged): resolves a declared preset via drawer or file-local; when found, calls `gptel--apply-preset` with a buffer-local setter; emits a warning when the preset does not resolve.
- **New behavior**: after preset application (or when no preset is declared), call `gptel-chat--apply-drawer-overrides`. The no-preset branch still triggers the overlay so that a buffer with drawer properties but no `GPTEL_PRESET` still honors the overrides (including `GPTEL_PARENT_SESSION_ID`).

### `jf/gptel--auto-init-session-buffer` (simplified)

- **Removed behavior**: no longer reads `metadata.yml`. The block reading `jf/gptel--read-session-metadata` is deleted. The post-mode metadata-preset apply is deleted — the drawer (via mode activation) does the entire job.
- **Retained behavior**: path extraction, buffer-local session variables (`jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`), registry registration, mode ensure, current-symlink update.
- **Ordering note**: mode activation must precede the buffer-local session-var setq-local calls, because `kill-all-local-variables` fires on mode activation and would wipe prior session vars. This ordering is already correct today (mode activation comes first, then the session vars are installed); removing the metadata read does not perturb it.

### `jf/gptel--create-session-core` (modified)

- **Removed behavior**: no longer writes `metadata.yml`. The with-temp-file block that wrote `session_id`, `created`, `updated`, `preset` YAML is deleted.
- **Modified behavior**: the `initial-content` parameter, if nil, now defaults to a drawer-prefixed template rather than the bare `#+begin_user\n\n#+end_user\n`. A helper `jf/gptel--initial-session-content (preset-name &optional parent-session-id)` constructs the content string:
  ```
  :PROPERTIES:
  :GPTEL_PRESET: <name>
  [:GPTEL_PARENT_SESSION_ID: <id>]
  :END:
  #+begin_user

  #+end_user
  ```

### `jf/gptel--branch-session-core` (modified)

- **Removed behavior**: no longer copies `metadata.yml` from the parent branch. The new branch's preset is inherited via the `PROPERTIES` drawer copied as part of the truncated `session.org` content.
- **Retained behavior**: `branch-metadata.yml` writing, context truncation, `current` symlink update.

### Upstream boundary

The save path calls `gptel-org-set-properties` directly. The restore path calls `gptel-org--entry-properties` directly. Both are already exported enough in upstream `gptel-org.el` to be called from outside (no advice, no forking). `GPTEL_PARENT_SESSION_ID` is our extension — upstream ignores unknown drawer keys, preserving cross-compat.

## Boundaries

**In scope:**
- Configuration drawer at point-min for chat-mode buffers.
- Save hook that writes drawer deltas using upstream semantics + `GPTEL_PARENT_SESSION_ID`.
- Restore overlay that reads drawer deltas using upstream semantics + `GPTEL_PARENT_SESSION_ID`.
- Removal of `metadata.yml`, its writer module, and all call sites.
- Session-creation update to pre-populate the drawer.
- Branching update to stop copying `metadata.yml`.
- Activities-integration update to remove the metadata guard.
- Full test rewrite of metadata-referencing specs.

**Out of scope:**
- `GPTEL_BOUNDS` support (intentionally excluded — chat-mode uses blocks).
- Enabling `gptel-mode` (Decision 16 point 1 retained).
- Migration of pre-existing session files lacking a drawer (user confirmed no legacy constraint).
- Changes to `scope.yml` or `branch-metadata.yml`.
- Sessions registry, activities worktree-path storage (still an inline HTML comment in session.org).

## Testing Approach

### Test Framework

**Buttercup**. The tests being touched and extended are all Buttercup (`preset-wiring-spec.el`, `preset-application-spec.el`, `session-org-creation-spec.el`, `auto-init-chat-mode-spec.el`, `branching-integration-spec.el`, `activity-session-chat-spec.el`). Buttercup is the repo's preferred framework per `CLAUDE.md`. The `spy-on` facility is load-bearing because the save/restore path depends on upstream helpers we mock at the boundary, not globally.

### Test Organization

- New chat-mode save specs: `config/gptel/chat/test/menu/save-state-spec.el`.
- Extended restore specs: `config/gptel/chat/test/menu/preset-wiring-spec.el`.
- Rewritten session specs: `config/gptel/sessions/test/commands/{preset-application,session-org-creation,auto-init-chat-mode}-spec.el`.
- Rewritten branching spec: `config/gptel/sessions/test/branching/branching-integration-spec.el`.
- Rewritten activities spec: `config/gptel/sessions/test/activities/activity-session-chat-spec.el`.

### Naming Conventions

- Files: `<topic>-spec.el` (Buttercup).
- Describe blocks: `"gptel-chat save state"`, `"gptel-chat drawer overrides"`, `"Preset application during auto-init — drawer authoritative"`.
- Nested `it` blocks mirror spec scenarios word-for-word where practical, so linkage is visible at failure time.

### Running Tests

```bash
# Chat-mode tests (most change surface)
./bin/run-tests.sh -d config/gptel/chat

# Sessions tests (metadata removal surface)
./bin/run-tests.sh -d config/gptel/sessions

# Full gptel regression
./bin/run-tests.sh -d config/gptel
```

### Test Patterns

- **Boundary mocking via `spy-on`**: `gptel-org-set-properties`, `gptel-org--entry-properties`, `gptel--apply-preset`, `gptel-get-preset`, and `gptel-mode` are spied in tests that need to isolate chat-mode's own logic.
- **End-to-end (real upstream)**: at least one integration test per path exercises the real `gptel-org-set-properties` / `gptel-org--entry-properties` against a temp buffer so a future upstream-signature change fails a test we actually run, not just a mock.
- **Temp-file fixture for save round-trip**: for the save-then-reopen scenario, write to a real temp file, kill the buffer, reopen, and assert the overlay applied.
- **Session fixtures**: reuse the existing per-example cleanup pattern from `preset-application-spec.el`; do not introduce a second fixture style in the same file.
- **No `metadata.yml` in fixtures**: after this change, test fixtures that previously wrote `metadata.yml` write the drawer into `session.org` instead. A helper `jf-gptel-test--write-session-with-drawer (path preset &optional parent-id body)` replaces the old `metadata.yml` writer helpers.

### Scenario Mapping

Every `#### Scenario:` in `specs/gptel/chat-mode.md` and `specs/gptel/sessions-persistence.md` maps to at least one `it` block:

- "Drawer written on save when preset is applied" → `it "writes a drawer with GPTEL_PRESET on save when a preset is applied"` in `save-state-spec.el`.
- "Drawer is delta-from-preset" → `it "writes only GPTEL_PRESET when no user overrides exist"`.
- "Drawer captures user overrides as deltas" → `it "writes GPTEL_TOOLS when the user adds a tool via gptel-menu"`.
- "Drawer carries parent-session-id for agent sessions" → `it "writes GPTEL_PARENT_SESSION_ID when jf/gptel--parent-session-id is set"`.
- "Save path never writes GPTEL_BOUNDS" → `it "never writes GPTEL_BOUNDS in any save path"`.
- "Drawer overlay restores user tools after reopen" → `it "overlays GPTEL_TOOLS after preset apply on mode activation"`.
- "Drawer overlay restores parent-session-id" → `it "restores jf/gptel--parent-session-id from GPTEL_PARENT_SESSION_ID"`.
- "Fresh branch session.org has drawer" → `it "populates session.org with GPTEL_PRESET drawer at creation"` in `session-org-creation-spec.el`.
- "Fresh agent session.org carries parent session id" → `it "populates agent session.org with GPTEL_PARENT_SESSION_ID in the drawer"`.
- "Branch creation (does not copy metadata.yml)" → `it "does not create a metadata.yml in the new branch directory"` in `branching-integration-spec.el`.
- "Preset applied from drawer on open" (session) → `it "applies drawer preset on session auto-init without reading metadata.yml"` in `auto-init-chat-mode-spec.el`.

The existing `preset-application-spec.el` scenarios are fully rewritten: the assertions switch from "does the metadata preset win?" to "does the drawer preset drive activation, and do drawer deltas overlay correctly?"

## Dependencies

- **Upstream `gptel-org.el`**: `gptel-org-set-properties`, `gptel-org--entry-properties`. Both exist today in `runtime/straight/repos/gptel/gptel-org.el`. No new dependency.
- **Upstream `gptel.el`**: `gptel--apply-preset`, `gptel-get-preset`, `gptel-tool-name`, `gptel-get-tool`, `gptel-backend-name`, `gptel--preset-mismatch-value` (already transitively used).
- **`org-entry-put` / `org-entry-get`** (built-in): chat-mode already requires `org`.
- **No new external packages.**

## Constraints

- **GPTEL_BOUNDS must never leak in.** Save path calls `gptel-org-set-properties` directly (not `gptel-org--save-state`) because the latter also writes `GPTEL_BOUNDS`. Tested explicitly.
- **No `gptel-mode` activation.** All new code paths preserve Decision 16 point 1. Tests assert `gptel-mode` is never called.
- **No `metadata.yml` anywhere.** After this change, no code path writes or reads `metadata.yml`. A regression check (grep in CI / local) catches accidental resurrection.
- **Load-time safety.** `declare-function` forward declarations mirror existing `menu.el` patterns; symbol resolution happens at hook-fire time.
- **No legacy compatibility required.** User has confirmed no existing sessions require legacy support. Reopened files without a drawer silently get no preset — acceptable degraded path.
- **Performance.** Save hook runs on every `save-buffer`. `gptel-org-set-properties` is the upstream hot path and is fast. The added `org-entry-put` for `GPTEL_PARENT_SESSION_ID` is a single regex pass. No measurable overhead.
