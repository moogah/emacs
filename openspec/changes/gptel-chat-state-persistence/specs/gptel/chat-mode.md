## ADDED Requirements

### Requirement: Configuration drawer save on buffer save

`gptel-chat-mode` SHALL install a buffer-local `before-save-hook` that writes a configuration `:PROPERTIES:` drawer at point-min before the buffer is written to disk. The drawer SHALL contain:

1. The keys upstream `gptel-mode` already persists via `gptel-org-set-properties`: `GPTEL_PRESET`, `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_SYSTEM`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND`.
2. `GPTEL_PARENT_SESSION_ID` — written only when the buffer-local variable `jf/gptel--parent-session-id` is non-nil (agent-session case). This key is chat-mode-specific; upstream ignores unknown drawer keys.

The save hook SHALL delegate the upstream-compatible subset to `gptel-org-set-properties` (declared in `gptel-org.el`) so that drawer shape, delta-from-preset logic, and value-encoding rules stay bit-for-bit compatible with an upstream `gptel-mode` buffer. It SHALL write `GPTEL_PARENT_SESSION_ID` via `org-entry-put` as an additional, targeted write at `point-min` after the upstream helper returns.

The save hook SHALL NOT write `GPTEL_BOUNDS`. `GPTEL_BOUNDS` is incompatible with chat-mode's block-based format (Requirement: gptel-request backend usage prohibits `:GPTEL_BOUNDS:` mutation) and remains explicitly excluded from the chat-mode save path. This is the single documented divergence from upstream `gptel--save-state`.

The save hook SHALL NOT enable `gptel-mode` (minor mode). Chat-mode owns the major-mode role exclusively; only the save-side effect is reused from upstream.

#### Scenario: Drawer written on save when preset is applied
- **WHEN** a chat-mode buffer has the `coding` preset applied and `save-buffer` is invoked
- **THEN** the saved file contains a `:PROPERTIES:` / `:END:` drawer at point-min
- **AND** the drawer contains `:GPTEL_PRESET: coding`
- **AND** the drawer does NOT contain a `:GPTEL_BOUNDS:` line
- **AND** `gptel-mode` is NOT enabled as a side effect

#### Scenario: Drawer is delta-from-preset
- **WHEN** the `coding` preset is applied and the user has not changed any buffer-local configuration
- **AND** `save-buffer` is invoked
- **THEN** the drawer contains `:GPTEL_PRESET: coding` only
- **AND** no `:GPTEL_TOOLS:`, `:GPTEL_MODEL:`, or other delta lines are present

#### Scenario: Drawer captures user overrides as deltas
- **WHEN** the `coding` preset is applied, and the user adds one tool to `gptel-tools` via `gptel-menu`
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

#### Scenario: Drawer omitted when no preset is applied and no config changed
- **WHEN** a fresh chat-mode buffer has no preset applied and buffer-local config matches defaults
- **AND** `save-buffer` is invoked
- **THEN** the saved file contains no `:PROPERTIES:` drawer (upstream `gptel-org-set-properties` is a no-op for this case; `GPTEL_PARENT_SESSION_ID` path also skipped when its source var is nil)

#### Scenario: Save path never writes GPTEL_BOUNDS
- **WHEN** a chat-mode buffer contains any number of assistant blocks and `save-buffer` is invoked
- **THEN** the saved file contains no `:GPTEL_BOUNDS:` line under any circumstance

### Requirement: Configuration drawer overlay on restore

On `gptel-chat-mode` activation, after a declared preset has been applied (Requirement: Preset system integration), the mode-activation hook SHALL read the full configuration drawer via `gptel-org--entry-properties` and overlay any non-preset values it finds as buffer-local bindings. The overlaid keys are `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_SYSTEM`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, and `GPTEL_NUM_MESSAGES_TO_SEND` — the same upstream-compatible set persisted by the save path.

The mode-activation hook SHALL additionally read `GPTEL_PARENT_SESSION_ID` via `org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID"` and, when present, set `jf/gptel--parent-session-id` buffer-locally. This restores the agent-session parent reference on reopen without `metadata.yml`.

The overlay SHALL be applied with a buffer-local setter matching the preset-application pattern so overlaid values do not leak into other buffers.

When the drawer contains no non-preset keys (delta-from-preset is empty), the overlay is a no-op. When the buffer has no drawer at all, neither the preset nor the overlay path fires.

#### Scenario: Drawer overlay restores user tools after reopen
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_TOOLS: tool-a tool-b tool-c`
- **AND** the `coding` preset resolves `:tools` to `(tool-a tool-b)`
- **THEN** after mode activation, buffer-local `gptel-tools` contains `(tool-a tool-b tool-c)`
- **AND** buffer-local `gptel--preset` is `coding`

#### Scenario: Drawer overlay restores parent-session-id for agent sessions
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PARENT_SESSION_ID: parent-abc-20260424000000`
- **THEN** after mode activation, `jf/gptel--parent-session-id` is `"parent-abc-20260424000000"` buffer-locally

#### Scenario: Drawer overlay does not leak globally
- **WHEN** a chat-mode buffer with a drawer overlay finishes activation
- **THEN** the overlaid keys (`gptel-tools`, `gptel-model`, `gptel-backend`, `jf/gptel--parent-session-id`, ...) are buffer-local
- **AND** their global (default) values are unchanged

#### Scenario: Drawer without deltas is a restore no-op
- **WHEN** a chat-mode buffer is opened whose drawer contains only `:GPTEL_PRESET: coding`
- **THEN** the preset is applied
- **AND** no additional overlay writes occur (no redundant calls to the buffer-local setter for absent keys)

#### Scenario: No drawer at all triggers no save/restore
- **WHEN** a chat-mode buffer with no `:PROPERTIES:` drawer and no file-local `gptel--preset` is activated
- **THEN** no preset is applied and no overlay runs
- **AND** the buffer inherits global or dir-local configuration unchanged

## MODIFIED Requirements

### Requirement: Preset system integration

The system SHALL integrate with upstream gptel's preset mechanism. On mode activation, the mode SHALL detect a preset declaration in one of two places (in order of precedence):

1. A `:GPTEL_PRESET: <name>` entry in an Org `:PROPERTIES:` drawer at point-min
2. A file-local `gptel--preset: <name>` variable (via `-*- ... -*-` header or `Local Variables:` block)

When a preset is found, the system SHALL call `gptel--apply-preset` with a buffer-local setter function, installing the preset's `:backend`, `:model`, `:system`, `:tools`, and other keys as buffer-local values. Subsequent `gptel-request` calls in that buffer use the applied values.

After the preset is applied, the system SHALL overlay any non-preset configuration properties found in the same `:PROPERTIES:` drawer (`GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_SYSTEM`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND`, `GPTEL_PARENT_SESSION_ID`) as buffer-local bindings — see Requirement: Configuration drawer overlay on restore.

When no preset is declared, the mode SHALL take no preset-related action; the buffer inherits whatever global or dir-local configuration is in effect. A drawer that contains only non-preset keys (unusual but legal) SHALL still trigger the overlay.

The system SHALL NOT enable `gptel-mode` (minor mode) as part of preset application. Preset application is buffer-local and does not alter the major mode.

#### Scenario: Preset applied from Org property drawer
- **WHEN** a buffer opens with `:PROPERTIES:\n:GPTEL_PRESET: coding\n:END:` at point-min
- **AND** the mode is activated
- **THEN** `gptel--apply-preset` is called with the symbol `coding`
- **AND** the preset's `:model`, `:backend`, `:tools` etc. are set as buffer-local values

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

#### Scenario: Drawer overrides overlay after preset
- **WHEN** a buffer opens with `:GPTEL_PRESET: coding` AND `:GPTEL_TOOLS: tool-a tool-b tool-c` in the drawer
- **AND** the `coding` preset resolves `:tools` to `(tool-a tool-b)`
- **AND** the mode is activated
- **THEN** `gptel--apply-preset` is called with `coding` first
- **THEN** `gptel-tools` is then overlaid to `(tool-a tool-b tool-c)` from the drawer

### Requirement: Session file format and persistence

Session files (those under `branches/<branch>/session.org` or `agents/<agent>/session.org`) SHALL use the chat-mode block format defined by this specification. Saving a session buffer SHALL use plain `save-buffer`; the file's on-disk content is the chat-mode block structure plus a `:PROPERTIES:` configuration drawer at point-min written by the chat-mode save hook (Requirement: Configuration drawer save on buffer save).

The sessions subsystem SHALL NOT write `gptel--bounds` Local Variables, SHALL NOT append `gptel-mode`-style Local Variables blocks, and SHALL NOT invoke `gptel--save-state` (which would also write `GPTEL_BOUNDS`). The chat-mode save hook is the only writer of configuration properties, and it writes only the keys enumerated in Requirement: Configuration drawer save on buffer save.

The sessions subsystem SHALL NOT maintain `metadata.yml`. `scope.yml` remains a separate sidecar; `branch-metadata.yml` remains a separate sidecar for non-main branches.

#### Scenario: Fresh session file is a chat-mode buffer with pre-populated drawer
- **WHEN** `jf/gptel-persistent-session` creates a new session with preset `coding`
- **THEN** `session.org` contains a `:PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n` drawer at point-min
- **AND** the file ends with `#+begin_user\n\n#+end_user\n`
- **AND** no Local Variables block is present
- **AND** no `metadata.yml` is written in the branch directory

#### Scenario: Agent session.org has parent-session-id in drawer
- **WHEN** an agent session is created with preset `executor` under parent `p-abc-20260424000000`
- **THEN** the agent's `session.org` drawer contains `:GPTEL_PRESET: executor` and `:GPTEL_PARENT_SESSION_ID: p-abc-20260424000000`
- **AND** no `metadata.yml` is written in the agent directory

#### Scenario: Saving a session buffer writes configuration drawer, no gptel-mode artifacts
- **WHEN** the user edits a session buffer and invokes `save-buffer`
- **THEN** the file on disk contains a `:PROPERTIES:` / `:END:` drawer at point-min
- **AND** the drawer contains no `:GPTEL_BOUNDS:` line
- **AND** no `:: Local Variables:` block is appended
- **AND** no `metadata.yml` exists alongside the session file
