## MODIFIED Requirements

### Requirement: Mutable scope drawer in session.org

Each session SHALL receive a populated `:PROPERTIES:` drawer in its `session.org` at creation time. The drawer is the single source of truth for scope enforcement and for chat-mode configuration; the profile template, the registered preset, and the upstream `gptel-org-set-properties` writer are never consulted by validators or by the chat-mode save hook.

The scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`, `:GPTEL_SCOPE_CLOUD_AUTH:`, ...) are written by this module. The chat-mode configuration keys (`:GPTEL_PRESET:`, `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, `:GPTEL_NUM_MESSAGES_TO_SEND:`, optional `:GPTEL_PARENT_SESSION_ID:`) are also written by this module's renderer at session creation, drawn from the resolved preset spec — see Requirement: Integration with session creation. The drawer DOES NOT include `:GPTEL_SYSTEM:` (system prompt is intentionally excluded; it is read from the preset file at mode activation — see `gptel/chat-mode` Requirement: Configuration drawer save on buffer save).

**Implementation**: `jf/gptel-scope-profile--apply-to-drawer` and `jf/gptel-scope-profile--render-drawer-text` in `config/gptel/scope-profiles.org`. The renderer accepts the resolved preset spec (in addition to the existing scope plist) and emits the chat-mode snapshot keys alongside the scope keys.

#### Scenario: Drawer populated at session creation with full snapshot
- **WHEN** a session is created with preset `executor`
- **THEN** the new `session.org` is written with a `:PROPERTIES:` drawer at `point-min` containing `:GPTEL_PRESET: executor`, the preset's snapshot keys (`:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:` etc., when non-nil in the preset spec), and the preset's resolved scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`, `:GPTEL_SCOPE_CLOUD_AUTH:`, ...)
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** followed by the chat-mode initial content (`#+begin_user\n\n#+end_user\n`)
- **AND** no `scope.yml` is written

#### Scenario: Drawer is mutable
- **WHEN** scope expansion adds a path during a session
- **THEN** the chat buffer's drawer is updated in place via `org-entry-put` / multi-value helpers
- **AND** the buffer is saved
- **AND** the registered preset and the profile template remain unchanged

#### Scenario: Drawer is the enforcement source
- **WHEN** a tool invocation requires path validation
- **THEN** `scope-validation` reads the chat buffer's drawer (or the file's drawer if no buffer is open)
- **AND** does NOT read from `gptel--known-presets` or from profile files
- **AND** does NOT read any `scope.yml` (no such file exists)

### Requirement: Integration with session creation

The profile module SHALL expose a single creation entrypoint that, given a preset name (and the module resolves the preset spec internally), target session.org path, optional project root, optional worktree paths, and optional parent-session-id, returns the drawer text (as a string) ready to embed in a freshly created `session.org`.

The returned drawer text carries:

1. `:GPTEL_PRESET: <name>`
2. `:GPTEL_PARENT_SESSION_ID: <id>` when supplied (agent sessions)
3. The chat-mode snapshot keys derived from the resolved preset spec — `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, `:GPTEL_NUM_MESSAGES_TO_SEND:` — each emitted only when the preset declares a non-nil value for the corresponding key
4. The scope keys `:GPTEL_SCOPE_*:` resolved from the preset's scope profile, project root, and worktree paths
5. NO `:GPTEL_SYSTEM:` line — the system prompt is intentionally excluded

**Implementation**: `jf/gptel-scope-profile--create-for-session` and `jf/gptel-scope-profile--render-drawer-text` in `config/gptel/scope-profiles.org`. The renderer's signature gains an explicit preset-spec argument (or resolves the spec internally from the preset name) so it can emit the chat-mode snapshot keys.

#### Scenario: Returns drawer text for embedding in initial session.org
- **WHEN** session creation calls `--create-for-session` with a preset and target directory before opening the buffer
- **THEN** the function resolves the profile, resolves the preset spec, expands variables, and returns a string of the form `:PROPERTIES:\n:GPTEL_PRESET: ...\n:GPTEL_MODEL: ...\n:GPTEL_TOOLS: ...\n[other snapshot keys]\n:GPTEL_SCOPE_READ: ...\n[other scope keys]\n:END:\n` ready to prepend to the chat-mode initial content
- **AND** the returned text contains no `:GPTEL_SYSTEM:` line
- **AND** session creation writes the resulting `session.org` in one shot

#### Scenario: Applies snapshot to an existing session.org buffer
- **WHEN** `--create-for-session` is called against an already-open chat buffer (e.g. an agent or branched session being initialized in a buffer)
- **THEN** the function uses `org-entry-put` and multi-value helpers to write each snapshot key (`:GPTEL_MODEL:`, `:GPTEL_TOOLS:`, ...) and each `:GPTEL_SCOPE_*` key into the buffer's `:PROPERTIES:` drawer at `point-min`
- **AND** the function does NOT write `:GPTEL_SYSTEM:` even if the preset declares `:system`
- **AND** existing drawer keys not part of the snapshot or scope set (`:GPTEL_PARENT_SESSION_ID:`, custom keys) are preserved

#### Scenario: Sparse preset produces sparse snapshot
- **WHEN** the preset declares only `:model` and `:tools` (no `:temperature`, no `:max-tokens`, no `:num-messages-to-send`, no `:backend`)
- **AND** `--create-for-session` is invoked
- **THEN** the returned drawer text contains `:GPTEL_PRESET:`, `:GPTEL_MODEL:`, `:GPTEL_TOOLS:` from the preset (plus `:GPTEL_SCOPE_*:` if the preset has a scope profile)
- **AND** the returned text does NOT contain `:GPTEL_BACKEND:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, or `:GPTEL_NUM_MESSAGES_TO_SEND:` lines
