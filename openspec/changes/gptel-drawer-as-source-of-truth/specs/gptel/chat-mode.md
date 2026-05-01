## ADDED Requirements

### Requirement: Chat-menu defaults configuration scope to buffer-local

`gptel-chat-menu` SHALL set `gptel--set-buffer-locally` to `t` for the lifetime of the menu invocation. This causes the upstream tool / model / temperature / system / etc. infixes (which read this variable to decide whether `gptel--set-with-scope` should set globally, buffer-locally, or oneshot) to write buffer-locally by default when invoked from the chat menu.

The buffer-local default applies only to `gptel-chat-menu`. Upstream `gptel-menu` (invoked directly via `M-x gptel-menu` or its upstream binding) is unchanged and retains its global default.

The user MAY still override per-invocation via the menu's scope toggle (the upstream `gptel--infix-variable-scope`), cycling through global / buffer-local / oneshot.

#### Scenario: Tool toggle from chat menu writes buffer-locally
- **WHEN** the user invokes `gptel-chat-menu` in a chat-mode buffer and toggles a tool via "Select tools" without changing the scope toggle
- **THEN** `gptel-tools` is set buffer-locally in the chat-mode buffer
- **AND** the global `gptel-tools` value is unchanged
- **AND** `default-value 'gptel-tools` is unchanged

#### Scenario: Upstream gptel-menu retains global default
- **WHEN** the user invokes `M-x gptel-menu` (not the chat-menu binding) in any buffer and toggles a tool
- **THEN** the upstream global behaviour is unchanged

#### Scenario: Buffer-local tool change persists to drawer on save
- **WHEN** the user toggles a tool via `gptel-chat-menu` and then invokes `save-buffer`
- **THEN** the saved drawer contains a `:GPTEL_TOOLS:` line listing the new buffer-local tool list

## MODIFIED Requirements

### Requirement: Configuration drawer save on buffer save

`gptel-chat-mode` SHALL install a buffer-local `before-save-hook` that writes a configuration `:PROPERTIES:` drawer at point-min before the buffer is written to disk. The drawer SHALL contain a **full snapshot** of the upstream-compatible configuration keys read from the buffer's current state — `GPTEL_PRESET`, `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND` — written via `org-entry-put` (and `org-entry-put-multivalued-property` for list-valued keys) with no delta-from-preset deletion. Each key is written when its source buffer-local variable is non-nil; nil-valued keys are deleted via `org-entry-delete` so the drawer never carries stale values from a prior save.

Additionally, the drawer SHALL contain `GPTEL_PARENT_SESSION_ID` when the buffer-local variable `jf/gptel--parent-session-id` is a non-empty string.

The save hook SHALL NOT write `:GPTEL_SYSTEM:`. The system prompt is intentionally excluded from drawer persistence — long, multi-line, special-character-heavy strings are unwieldy as a single property value. The system prompt is read from the active preset's `:system` key at mode activation. If a user authored `:GPTEL_SYSTEM:` in the drawer manually, the read-time overlay still respects it (back-compat); the writer simply never emits or replaces it.

The save hook SHALL NOT write `:GPTEL_BOUNDS:`. `GPTEL_BOUNDS` is incompatible with chat-mode's block-based format and remains explicitly excluded.

The save hook SHALL NOT delegate to upstream `gptel-org-set-properties`. That function deletes properties whose buffer-local values match the active preset, defeating the WYSIWYG goal of this change. The chat-mode writer is independent and emits the full snapshot regardless of preset matching.

The save hook SHALL NOT enable `gptel-mode` (minor mode). Chat-mode owns the major-mode role exclusively.

#### Scenario: Drawer carries full preset snapshot on save
- **WHEN** a chat-mode buffer has the `coding` preset applied and `save-buffer` is invoked
- **AND** the buffer-local `gptel-tools`, `gptel-model`, `gptel-backend`, `gptel-temperature`, `gptel-max-tokens`, `gptel--num-messages-to-send` all match the preset's values exactly
- **THEN** the saved file contains a `:PROPERTIES:` / `:END:` drawer at point-min
- **AND** the drawer contains `:GPTEL_PRESET: coding`
- **AND** the drawer contains `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, and any other non-nil preset key
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the drawer does NOT contain `:GPTEL_BOUNDS:`

#### Scenario: Drawer carries user overrides on top of preset snapshot
- **WHEN** the `coding` preset is applied and the user adds one tool to `gptel-tools` via `gptel-chat-menu`
- **AND** `save-buffer` is invoked
- **THEN** the drawer contains `:GPTEL_PRESET: coding`
- **AND** the drawer contains a `:GPTEL_TOOLS:` line listing the preset's tools plus the one added tool

#### Scenario: Drawer carries parent-session-id for agent sessions
- **WHEN** the buffer is an agent session with `jf/gptel--parent-session-id` set to `"parent-abc-20260424000000"`
- **AND** `save-buffer` is invoked
- **THEN** the drawer contains `:GPTEL_PARENT_SESSION_ID: parent-abc-20260424000000`

#### Scenario: Drawer omits parent-session-id for non-agent buffers
- **WHEN** the buffer has no `jf/gptel--parent-session-id` (branch session or standalone chat buffer)
- **AND** `save-buffer` is invoked
- **THEN** the drawer does NOT contain a `:GPTEL_PARENT_SESSION_ID:` line

#### Scenario: GPTEL_SYSTEM never written by the save path
- **WHEN** any chat-mode buffer is saved, regardless of preset or buffer-local `gptel--system-message` content
- **THEN** the saved drawer contains no `:GPTEL_SYSTEM:` line

#### Scenario: Save path never writes GPTEL_BOUNDS
- **WHEN** a chat-mode buffer contains any number of assistant blocks and `save-buffer` is invoked
- **THEN** the saved file contains no `:GPTEL_BOUNDS:` line under any circumstance

#### Scenario: Save with no preset still writes a snapshot of buffer-local config
- **WHEN** a fresh chat-mode buffer with no preset has the user set `gptel-model` to `claude-opus-4-7` buffer-locally
- **AND** `save-buffer` is invoked
- **THEN** the drawer contains `:GPTEL_MODEL: claude-opus-4-7`
- **AND** the drawer does NOT contain `:GPTEL_PRESET:`
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`

### Requirement: Configuration drawer overlay on restore

On `gptel-chat-mode` activation, after a declared preset has been applied (Requirement: Preset system integration), the mode-activation hook SHALL read the configuration drawer via `gptel-org--entry-properties` and overlay every drawer-present value as buffer-local bindings. The overlaid keys are `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_SYSTEM`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, and `GPTEL_NUM_MESSAGES_TO_SEND`.

The drawer wins over the preset for every key it carries — this is the WYSIWYG contract: what is in the drawer is what the buffer uses. Even when a drawer key matches the preset's value, the overlay still installs the buffer-local binding (a harmless no-op semantically; load-bearing for `gptel-org-set-properties`-incompatible test scenarios).

The mode-activation hook SHALL additionally read `GPTEL_PARENT_SESSION_ID` via `org-entry-get` and, when present, set `jf/gptel--parent-session-id` buffer-locally.

The overlay SHALL be applied with a buffer-local setter so overlaid values do not leak into other buffers.

When the drawer carries no entry for a given key, the buffer-local value remains whatever the preset installed (or whatever the global default is in the no-preset case). `:GPTEL_SYSTEM:` is the practical case where this matters most: the drawer typically does not carry it (the writer never emits it), so the system prompt comes from the preset.

When the buffer has no drawer at all, neither the preset nor the overlay path fires.

#### Scenario: Drawer overlay restores user tools after reopen
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_TOOLS: tool-a tool-b tool-c`
- **AND** the `coding` preset resolves `:tools` to `(tool-a tool-b)`
- **THEN** after mode activation, buffer-local `gptel-tools` contains `(tool-a tool-b tool-c)`
- **AND** buffer-local `gptel--preset` is `coding`

#### Scenario: Drawer model wins over preset model
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_MODEL: claude-haiku-4-5`
- **AND** the `coding` preset resolves `:model` to `claude-sonnet-4-6`
- **THEN** after mode activation, buffer-local `gptel-model` is `claude-haiku-4-5`

#### Scenario: System prompt comes from preset when drawer omits it
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and no `:GPTEL_SYSTEM:` line
- **AND** the `coding` preset resolves `:system` to `"You are a coding assistant."`
- **THEN** after mode activation, buffer-local `gptel--system-message` is `"You are a coding assistant."`

#### Scenario: Drawer-authored system prompt still respected on restore
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_SYSTEM: Custom override prompt.`
- **THEN** after mode activation, buffer-local `gptel--system-message` is `"Custom override prompt."`
- **NOTE**: the next save will NOT re-write `:GPTEL_SYSTEM:` (writer never emits it), but the overlay does respect manually-authored entries on read.

#### Scenario: Drawer overlay restores parent-session-id for agent sessions
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PARENT_SESSION_ID: parent-abc-20260424000000`
- **THEN** after mode activation, `jf/gptel--parent-session-id` is `"parent-abc-20260424000000"` buffer-locally

#### Scenario: Drawer overlay does not leak globally
- **WHEN** a chat-mode buffer with a drawer overlay finishes activation
- **THEN** the overlaid keys (`gptel-tools`, `gptel-model`, `gptel-backend`, `jf/gptel--parent-session-id`, ...) are buffer-local
- **AND** their global (default) values are unchanged

#### Scenario: No drawer at all triggers no overlay
- **WHEN** a chat-mode buffer with no `:PROPERTIES:` drawer and no file-local `gptel--preset` is activated
- **THEN** no preset is applied and no overlay runs
- **AND** the buffer inherits global or dir-local configuration unchanged

### Requirement: Preset system integration

The system SHALL integrate with upstream gptel's preset mechanism. On mode activation, the mode SHALL detect a preset declaration in one of two places (in order of precedence):

1. A `:GPTEL_PRESET: <name>` entry in an Org `:PROPERTIES:` drawer at point-min
2. A file-local `gptel--preset: <name>` variable (via `-*- ... -*-` header or `Local Variables:` block)

When a preset is found, the system SHALL call `gptel--apply-preset` with a buffer-local setter function, installing the preset's `:backend`, `:model`, `:system`, `:tools`, and other keys as buffer-local values. Subsequent `gptel-request` calls in that buffer use the applied values.

After the preset is applied, the system SHALL overlay every drawer-present configuration property as buffer-local bindings (Requirement: Configuration drawer overlay on restore). The drawer wins over the preset for every key it carries. For keys the drawer does not carry, the preset's value remains in effect — this is how `:system` propagates from preset to buffer (the writer never emits `:GPTEL_SYSTEM:`, so the drawer typically lacks it).

When no preset is declared, the mode SHALL take no preset-related action; the buffer inherits whatever global or dir-local configuration is in effect. A drawer that contains only non-preset keys (e.g., a `:GPTEL_MODEL:` and nothing else) SHALL still trigger the overlay.

The system SHALL NOT enable `gptel-mode` (minor mode) as part of preset application. Preset application is buffer-local and does not alter the major mode.

#### Scenario: Preset applied from Org property drawer
- **WHEN** a buffer opens with `:PROPERTIES:\n:GPTEL_PRESET: coding\n:END:` at point-min
- **AND** the mode is activated
- **THEN** `gptel--apply-preset` is called with the symbol `coding`
- **AND** the preset's `:model`, `:backend`, `:tools`, `:system` etc. are set as buffer-local values

#### Scenario: Preset applied from file-local variable
- **WHEN** a buffer opens with `# -*- gptel--preset: coding -*-` on line 1
- **AND** the mode is activated
- **THEN** `gptel--apply-preset` is called with the symbol `coding`

#### Scenario: No preset declared
- **WHEN** a buffer is activated with no preset property or file-local
- **THEN** no `gptel--apply-preset` call is made
- **AND** `gptel--preset` remains at its inherited value (global or nil)

#### Scenario: Property drawer wins over file-local
- **WHEN** both a `GPTEL_PRESET` property drawer and a file-local `gptel--preset` are present and name different presets
- **THEN** the property drawer value is applied

#### Scenario: Drawer overlay wins over preset for shared keys
- **WHEN** a buffer opens with `:GPTEL_PRESET: coding` AND `:GPTEL_MODEL: claude-haiku-4-5` in the drawer
- **AND** the `coding` preset resolves `:model` to `claude-sonnet-4-6`
- **AND** the mode is activated
- **THEN** `gptel--apply-preset` is called with `coding` first
- **THEN** `gptel-model` is then overlaid to `claude-haiku-4-5` from the drawer

#### Scenario: Preset's :system survives because drawer typically omits :GPTEL_SYSTEM:
- **WHEN** a buffer opens with `:GPTEL_PRESET: coding` and no `:GPTEL_SYSTEM:` in the drawer
- **AND** the `coding` preset resolves `:system` to `"You are a coding assistant."`
- **AND** the mode is activated
- **THEN** buffer-local `gptel--system-message` is `"You are a coding assistant."`

### Requirement: Session file format and persistence

Session files (those under `branches/<branch>/session.org` or `agents/<agent>/session.org`) SHALL use the chat-mode block format defined by this specification. Saving a session buffer SHALL use plain `save-buffer`; the file's on-disk content is the chat-mode block structure plus a `:PROPERTIES:` configuration drawer at point-min written by the chat-mode save hook (Requirement: Configuration drawer save on buffer save).

The drawer carries a full snapshot of the buffer's upstream-compatible configuration (`GPTEL_PRESET`, `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND`), the chat-mode extension `GPTEL_PARENT_SESSION_ID` (when set), and the scope keys `GPTEL_SCOPE_*` (managed by the scope subsystem). It does NOT carry `:GPTEL_SYSTEM:` (excluded from the writer; system prompt lives in the preset file) or `:GPTEL_BOUNDS:`.

The sessions subsystem SHALL NOT write `gptel--bounds` Local Variables, SHALL NOT append `gptel-mode`-style Local Variables blocks, and SHALL NOT invoke `gptel--save-state` or `gptel-org-set-properties`. The chat-mode save hook is the only writer of configuration properties.

The sessions subsystem SHALL NOT maintain `metadata.yml`. `scope.yml` is no longer used for per-session storage (replaced by drawer scope keys). `branch-metadata.yml` remains a separate sidecar for non-main branches.

#### Scenario: Fresh session file is a chat-mode buffer with full preset snapshot drawer
- **WHEN** `jf/gptel-persistent-session` creates a new session with preset `coding`
- **THEN** `session.org` contains a `:PROPERTIES:` drawer at point-min carrying `:GPTEL_PRESET: coding`, `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, and any other non-nil keys from the preset
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the file ends with `#+begin_user\n\n#+end_user\n`
- **AND** no Local Variables block is present

#### Scenario: Agent session.org has parent-session-id and full preset snapshot in drawer
- **WHEN** an agent session is created with preset `executor` under parent `p-abc-20260424000000`
- **THEN** the agent's `session.org` drawer contains `:GPTEL_PRESET: executor`, `:GPTEL_PARENT_SESSION_ID: p-abc-20260424000000`, and the `executor` preset's snapshot keys (`:GPTEL_MODEL:`, `:GPTEL_TOOLS:`, etc.)
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`

#### Scenario: Saving a session buffer writes full-snapshot drawer, no gptel-mode artifacts
- **WHEN** the user edits a session buffer and invokes `save-buffer`
- **THEN** the file on disk contains a `:PROPERTIES:` / `:END:` drawer at point-min carrying the full configuration snapshot (system excluded)
- **AND** the drawer contains no `:GPTEL_SYSTEM:` line
- **AND** the drawer contains no `:GPTEL_BOUNDS:` line
- **AND** no `:: Local Variables:` block is appended
- **AND** no `metadata.yml` exists alongside the session file
