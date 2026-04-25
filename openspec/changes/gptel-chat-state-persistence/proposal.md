## Why

`gptel-chat-mode` currently does not persist per-buffer configuration into the session file. Decisions 16 and 18 of the original `gptel-chat-mode` change explicitly disabled `gptel--save-state` / `gptel--restore-state`, on the rationale that "the block structure is self-describing" and `metadata.yml` was authoritative. The smoke test surfaces this as a real defect: freshly-created `session.org` files have no `:PROPERTIES:` drawer, and any `gptel-menu` change (e.g., adding a tool) is silently discarded on `save-buffer` and lost on reopen.

The original reasoning conflated two separate concerns. `GPTEL_BOUNDS` (response-span tracking) is genuinely incompatible with chat-mode's block-based format and should stay disabled. But `GPTEL_PRESET` / `GPTEL_TOOLS` / `GPTEL_MODEL` / etc. — the *configuration* drawer upstream writes — is orthogonal to response-span tracking and is the canonical mechanism upstream uses to make per-buffer edits durable. Chat-mode should stay aligned with upstream's property-drawer behavior as closely as possible, modulo the one legitimate exception (`GPTEL_BOUNDS`).

Once chat-mode writes the full configuration drawer, `metadata.yml` — originally introduced to carry the preset name for the old `gptel-mode`-driven session format — is left with no job it actually does. Every key (`session_id`, `created`, `updated`, `preset`, `type`, `parent_session_id`) is either derivable from the session path, redundant with filesystem mtime, or already covered by the drawer. It is a vestigial artifact of the pre-chat-mode design. No existing sessions rely on it meaningfully (the current design treats it as authoritative only because the drawer was missing), and the user has confirmed it has no legacy constraint.

## What Changes

- **BREAKING (Decision 16 reversal)**: `gptel-chat-mode` SHALL install a buffer-local `before-save-hook` that writes a configuration `:PROPERTIES:` drawer at point-min, mirroring upstream `gptel-org-set-properties` — emitting `GPTEL_PRESET`, `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_SYSTEM`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND` as deltas from the applied preset. The drawer additionally carries `GPTEL_PARENT_SESSION_ID` for agent sessions (the one field `metadata.yml` used to own that is not part of upstream's set).
- **BREAKING (Decision 18 clarification)**: The save path SHALL NOT write `GPTEL_BOUNDS`. The block-based format remains self-describing for message structure; the drawer persists only configuration. This exception is explicit in the spec.
- **BREAKING (restore path)**: `gptel-chat-mode` mode-activation SHALL read the full configuration drawer (not just `GPTEL_PRESET`), and overlay non-preset properties (`GPTEL_TOOLS`, `GPTEL_MODEL`, ...) on top of the applied preset. This matches upstream `gptel-org--restore-state`.
- **BREAKING (metadata.yml removal)**: `metadata.yml` is removed from the session layout entirely. All its keys either move to the `:PROPERTIES:` drawer (`preset`, `parent_session_id`), become derivable from the session path (`session_id`, `type`), or are dropped as redundant with filesystem mtime (`created`, `updated`).
- **BREAKING (module deletion)**: `config/gptel/sessions/metadata.org` / `metadata.el` and the functions `jf/gptel--read-session-metadata`, `jf/gptel--write-session-metadata`, `jf/gptel--metadata-file-path`, `jf/gptel--update-metadata-timestamp` are removed. Downstream callers (`commands.org`, `branching.org`, `activities-integration.org`) lose their `(require 'gptel-session-metadata)` and the associated read/write calls.
- **BREAKING (session creation)**: `jf/gptel--create-session-core` and `jf/gptel-persistent-session` SHALL populate `session.org`'s initial content with a `:PROPERTIES:` drawer containing `GPTEL_PRESET: <name>` (and `GPTEL_PARENT_SESSION_ID: <id>` for agent sessions). No `metadata.yml` is written.
- **BREAKING (auto-init)**: `jf/gptel--auto-init-session-buffer` no longer reads `metadata.yml`. Preset selection comes from the drawer (applied by `gptel-chat-mode`'s activation hook); `parent_session_id` is read from the drawer into `jf/gptel--parent-session-id`. Auto-init still performs path extraction, buffer-local session vars, registry registration, mode enable, and current-symlink update.
- **BREAKING (branching)**: `jf/gptel--branch-session` no longer copies `metadata.yml`. The new branch inherits the preset via the PROPERTIES drawer that is copied as part of the session.org content. `branch-metadata.yml` (which tracks parent_branch and branch_point_position) is a separate concern and remains unchanged.
- **BREAKING (test rewrites)**: Every session test that creates or reads `metadata.yml` must be updated — `session-org-creation-spec.el`, `preset-application-spec.el`, `auto-init-chat-mode-spec.el`, `branching-integration-spec.el`, `activity-session-chat-spec.el`. Scenarios that asserted "metadata.yml wins" are rewritten to the new contract: PROPERTIES drawer is authoritative, with the overlay model described above.
- `gptel-mode` is still NOT enabled by chat-mode (Decision 16 point 1 — chat-mode owns the major-mode role — remains intact).

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gptel/chat-mode`: drawer save/restore; `GPTEL_PARENT_SESSION_ID` added to the persisted set.
- `gptel/sessions-persistence`: `metadata.yml` removed from directory layout; session creation writes a pre-populated drawer into `session.org`; auto-init reads the drawer instead of `metadata.yml`.

## Impact

- **Code (modify)**:
  - `config/gptel/chat/menu.org` / `menu.el` — save hook, restore overlay, `GPTEL_PARENT_SESSION_ID` handling.
  - `config/gptel/sessions/commands.org` / `commands.el` — drop metadata reads/writes; session creation writes drawer into initial content; auto-init reads drawer-based parent-session-id.
  - `config/gptel/sessions/branching.org` / `branching.el` — drop `metadata.yml` copy step.
  - `config/gptel/sessions/activities-integration.org` / `activities-integration.el` — drop the `jf/gptel--read-session-metadata` existence guard; replace with a guard on a still-present function.
  - `config/gptel/sessions/constants.org` / `constants.el` — remove `jf/gptel-session--metadata-file` constant.
  - `config/gptel/sessions/filesystem.org` / `filesystem.el` — remove `jf/gptel--metadata-file-path` helper.
- **Code (delete)**:
  - `config/gptel/sessions/metadata.org` / `metadata.el` (entire module).
  - `config/gptel/sessions/test/commands/...` — any leftover metadata-only specs (actual functions TBD during implementation; most metadata-referencing tests are rewritten, not deleted).
- **Tests (rewrite)**:
  - `config/gptel/chat/test/menu/preset-wiring-spec.el` — add overlay scenarios.
  - New `config/gptel/chat/test/menu/save-state-spec.el` — save-path scenarios.
  - `config/gptel/sessions/test/commands/preset-application-spec.el` — rewrite to new contract; assertions on drawer, not metadata.yml.
  - `config/gptel/sessions/test/commands/session-org-creation-spec.el` — assert pre-populated drawer; drop metadata.yml assertions.
  - `config/gptel/sessions/test/commands/auto-init-chat-mode-spec.el` — drop metadata fixtures; fixtures author drawer instead.
  - `config/gptel/sessions/test/branching/branching-integration-spec.el` — drop metadata.yml copy assertion.
  - `config/gptel/sessions/test/activities/activity-session-chat-spec.el` — drop metadata.yml assertions; replace with drawer.
- **Existing `session.org` files**: The user has confirmed no existing sessions need legacy support. New sessions gain a pre-populated drawer; reopened old sessions without a drawer work as degraded no-op (no preset applied until user sets one via `gptel-menu` and saves) — this is acceptable under the user's constraint.
- **Upstream dependency**: Reuses `gptel-org-set-properties` and `gptel-org--entry-properties` from upstream `gptel-org.el`. `GPTEL_PARENT_SESSION_ID` is our own extension — upstream ignores unknown drawer keys, so cross-compat is preserved.
- **No changes to**: `gptel-mode` enablement (still disabled), block-based format, `GPTEL_BOUNDS` prohibition, `scope.yml`, `branch-metadata.yml`, registry, activities worktree-path storage (still an inline HTML comment in session.org).
