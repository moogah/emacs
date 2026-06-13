## REMOVED Requirements

### Requirement: System prompt heading is authoritative

**Reason for removal**: The `* System Prompt` heading body inside `session.org` proved fragile in practice — system prompts are long and often authored in markdown with code blocks and XML-like tags, which interact badly with org-mode fontification, folding, and emphasis parsing. The replacement contract is the new `Requirement: System prompt sibling file is authoritative`, which keeps the prompt out of `session.org` entirely and stores it as a sibling file in the session directory in whatever format the preset authored it in.

The legacy `:GPTEL_SYSTEM:` drawer entry continues to be respected by the existing drawer overlay (back-compat for hand-authored entries); only the heading-body tier is removed.

## ADDED Requirements

### Requirement: System prompt sibling file is authoritative

The buffer-local `gptel--system-message` for a chat-mode session SHALL be sourced from a sibling file in the session's branch directory, referenced from the configuration drawer via a `:GPTEL_SYSTEM_PROMPT_FILE:` property. The system prompt is carried as a separate file — outside `session.org` entirely — so its content (typically markdown with code, XML-like tags, and other special characters) does not interact with org-mode parsing, fontification, or folding.

**Drawer link.** When session creation persists a system prompt, it SHALL emit `:GPTEL_SYSTEM_PROMPT_FILE:` into the configuration drawer carrying a path. The path is typically a basename (e.g. `system-prompt.md`) resolved relative to `(file-name-directory buffer-file-name)`. The property MAY carry an absolute path (or one with directory components) and the resolver SHALL honor it. The save path SHALL NOT delete this property.

**Restore.** On `gptel-chat-mode` activation, after preset application and the drawer overlay have run, the mode SHALL read `:GPTEL_SYSTEM_PROMPT_FILE:` from the configuration drawer, resolve it relative to the session.org file's directory, and — when the resolved file exists and is non-empty — install its contents as the buffer-local `gptel--system-message`. The body SHALL be read verbatim (no trimming, no transformation) so prompts authored with intentional leading/trailing whitespace survive the round trip.

**Pre-send refresh.** Before dispatching a chat request, the chat-mode request submission path SHALL re-read the sibling file (when `:GPTEL_SYSTEM_PROMPT_FILE:` is set and the file exists) and refresh `gptel--system-message` from its current on-disk contents. The buffer-local value functions as a per-request cache; the file is the source of truth. A user edit to the sibling file is picked up on the next request without explicitly reopening `session.org`.

**Restore precedence.** With the heading tier removed, the system-prompt restore precedence is:
1. `:GPTEL_SYSTEM_PROMPT_FILE:` sibling file contents (when the property is set and the file exists and is non-empty)
2. legacy `:GPTEL_SYSTEM:` drawer entry (when present and the property/file above is absent — back-compat)
3. preset `:system`

**Save.** The save path SHALL NOT write the sibling file. The sibling file is canonical and is mutated only by the user (typically through the "Edit system prompt" menu affordance, which opens the file in another window). On `before-save-hook`, the chat-mode save path SHALL continue to never emit a `:GPTEL_SYSTEM:` drawer line (the existing write-exclusion stands); the sibling file is independent of the save hook.

**Missing file / missing property.** When `:GPTEL_SYSTEM_PROMPT_FILE:` is unset, or set to a path that does not resolve to a readable file, the restore step SHALL be a no-op — whatever the preset / drawer overlay already installed remains in effect. The mode SHALL NOT signal an error or warning for a missing sibling file (the session may legitimately have no system prompt; or the property may reference a path the user has not yet created).

#### Scenario: Restore reads sibling file when property and file are present
- **WHEN** a chat-mode buffer opens whose drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** the file `system-prompt.md` in the same directory as `session.org` exists with content `"Custom prompt from the file."`
- **AND** `:GPTEL_PRESET: coding` resolves `:system` to `"Preset text."`
- **THEN** `gptel--system-message` is `"Custom prompt from the file."`

#### Scenario: Restore falls back to preset when sibling file is absent
- **WHEN** a chat-mode buffer opens whose drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** no file named `system-prompt.md` exists in the session's branch directory
- **AND** `:GPTEL_PRESET: coding` resolves `:system` to `"Preset text."`
- **THEN** `gptel--system-message` is `"Preset text."`
- **AND** no error or warning is signaled

#### Scenario: Restore falls back to preset when property is unset
- **WHEN** a chat-mode buffer opens whose drawer carries no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **AND** `:GPTEL_PRESET: coding` resolves `:system` to `"Preset text."`
- **THEN** `gptel--system-message` is `"Preset text."`

#### Scenario: Restore respects legacy drawer entry when no sibling file
- **WHEN** a chat-mode buffer opens whose drawer contains `:GPTEL_SYSTEM: Legacy drawer prompt.` and no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **THEN** `gptel--system-message` is `"Legacy drawer prompt."` (legacy back-compat tier)

#### Scenario: Sibling file wins over legacy drawer entry
- **WHEN** a chat-mode buffer opens whose drawer contains both `:GPTEL_SYSTEM: Legacy.` and `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** the file `system-prompt.md` exists with content `"File wins."`
- **THEN** `gptel--system-message` is `"File wins."`

#### Scenario: Empty sibling file is a no-op
- **WHEN** a chat-mode buffer opens whose drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** the file `system-prompt.md` exists but is empty (or whitespace-only)
- **AND** `:GPTEL_PRESET: coding` resolves `:system` to `"Preset text."`
- **THEN** `gptel--system-message` is `"Preset text."` (empty file falls through, does not wipe the prompt)

#### Scenario: Pre-send refresh picks up file edit
- **WHEN** a chat-mode buffer is loaded with `gptel--system-message` cached as `"Old prompt."`
- **AND** the sibling file referenced by `:GPTEL_SYSTEM_PROMPT_FILE:` is modified on disk to `"New prompt."`
- **AND** the user submits a chat request from the buffer (without reopening or reverting `session.org`)
- **THEN** the request is dispatched with `gptel--system-message` equal to `"New prompt."`

#### Scenario: Save does not write the sibling file
- **WHEN** a chat-mode buffer has `gptel--system-message` set to `"In-buffer override."` (e.g., set programmatically) and `save-buffer` is invoked
- **AND** the sibling file on disk contains `"On-disk prompt."`
- **THEN** the sibling file on disk is unchanged — still `"On-disk prompt."`
- **AND** the saved drawer contains no `:GPTEL_SYSTEM:` line

### Requirement: Edit system prompt menu affordance opens the sibling file

The chat-mode menu SHALL provide an "Edit system prompt" affordance that opens the session's sibling system-prompt file in another window via `find-file-other-window`. This affordance replaces the upstream `gptel-system-prompt` minibuffer-editing infix in the chat-mode menu — the file is the source of truth for the system prompt, so the menu routes the user to the file rather than offering an in-menu text field.

When the chat-mode buffer's drawer carries `:GPTEL_SYSTEM_PROMPT_FILE:`, the affordance SHALL resolve and open that file. When the property is unset (e.g., the session was created from a preset with no `:system`), the affordance SHALL prompt the user for a filename (defaulting to `system-prompt.md`), write the property into the drawer, create the file (empty), save the session buffer, and open the new file.

Upstream `gptel-system-prompt` (invoked outside the chat-mode menu) is unchanged.

#### Scenario: Edit opens the resolved sibling file
- **WHEN** a chat-mode buffer's drawer carries `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** the file `system-prompt.md` exists in the session's branch directory
- **AND** the user invokes the "Edit system prompt" menu entry
- **THEN** `system-prompt.md` is opened in another window via `find-file-other-window`

#### Scenario: Edit creates and opens a file when property is unset
- **WHEN** a chat-mode buffer's drawer carries no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **AND** the user invokes the "Edit system prompt" menu entry and confirms the default filename
- **THEN** `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md` is added to the configuration drawer
- **AND** a new empty `system-prompt.md` is created in the session's branch directory
- **AND** the new file is opened in another window

## MODIFIED Requirements

### Requirement: Configuration drawer save on buffer save

`gptel-chat-mode` SHALL install a buffer-local `before-save-hook` that writes a configuration `:PROPERTIES:` drawer at point-min before the buffer is written to disk. The drawer SHALL contain a **full snapshot** of the upstream-compatible configuration keys read from the buffer's current state — `GPTEL_PRESET`, `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND` — written via `org-entry-put` (and `org-entry-put-multivalued-property` for list-valued keys) with no delta-from-preset deletion. Each key is written when its source buffer-local variable is non-nil; nil-valued keys are deleted via `org-entry-delete` so the drawer never carries stale values from a prior save.

Additionally, the drawer SHALL contain `GPTEL_PARENT_SESSION_ID` when the buffer-local variable `jf/gptel--parent-session-id` is a non-empty string.

The save hook SHALL preserve `:GPTEL_SYSTEM_PROMPT_FILE:` when present — it is written by session creation (and by the "Edit system prompt" menu affordance) and read by the system-prompt restore step (Requirement: System prompt sibling file is authoritative). The save path does not generate or rewrite the property; it simply does not delete it.

The save hook SHALL NOT write a `:GPTEL_SYSTEM:` drawer line. Long, multi-line, special-character-heavy strings are unwieldy as a single property value, so the system prompt is never persisted as a drawer *property*. The system prompt lives in the sibling file referenced by `:GPTEL_SYSTEM_PROMPT_FILE:`, which is canonical and is not written by the save hook (Requirement: System prompt sibling file is authoritative). If a user authored `:GPTEL_SYSTEM:` in the drawer manually, the read-time overlay still respects it (back-compat); the writer simply never emits or replaces the drawer property.

The save hook SHALL NOT write `:GPTEL_BOUNDS:`. `GPTEL_BOUNDS` is incompatible with chat-mode's block-based format and remains explicitly excluded.

The save hook SHALL NOT delegate to upstream `gptel-org-set-properties`. That function deletes properties whose buffer-local values match the active preset, defeating the WYSIWYG goal of the drawer. The chat-mode writer is independent and emits the full snapshot regardless of preset matching.

The save hook SHALL NOT serialize the system prompt into a `* System Prompt` heading body. The heading-based authoritative-source contract is removed (see Requirement removal); the sibling file replaces it.

The save hook SHALL NOT enable `gptel-mode` (minor mode). Chat-mode owns the major-mode role exclusively.

#### Scenario: Drawer carries full preset snapshot on save
- **WHEN** a chat-mode buffer has the `coding` preset applied and `save-buffer` is invoked
- **AND** the buffer-local `gptel-tools`, `gptel-model`, `gptel-backend`, `gptel-temperature`, `gptel-max-tokens`, `gptel--num-messages-to-send` all match the preset's values exactly
- **THEN** the saved file contains a `:PROPERTIES:` / `:END:` drawer at point-min
- **AND** the drawer contains `:GPTEL_PRESET: coding`
- **AND** the drawer contains `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, and any other non-nil preset key
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the drawer does NOT contain `:GPTEL_BOUNDS:`

#### Scenario: Save preserves sibling-file drawer link
- **WHEN** a chat-mode buffer's drawer carries `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md` and `save-buffer` is invoked
- **THEN** the saved drawer still contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`

#### Scenario: GPTEL_SYSTEM never written by the save path
- **WHEN** any chat-mode buffer is saved
- **THEN** the saved drawer contains no `:GPTEL_SYSTEM:` line

#### Scenario: Save does not emit `* System Prompt` heading
- **WHEN** a chat-mode buffer with no `* System Prompt` heading is saved
- **THEN** the saved file still contains no `* System Prompt` heading
- **AND** the saved file still contains no `* Chat` heading

### Requirement: Configuration drawer overlay on restore

On `gptel-chat-mode` activation, after a declared preset has been applied (Requirement: Preset system integration), the mode-activation hook SHALL read the configuration drawer via `gptel-org--entry-properties` and overlay every drawer-present value as buffer-local bindings. The overlaid keys are `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_SYSTEM`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, and `GPTEL_NUM_MESSAGES_TO_SEND`.

The overlay is **not delta-based**: any drawer-present key wins over the preset value, even when the two are equal. The drawer is the source of truth; the preset is the fallback baseline applied first so absent drawer keys are filled in.

For `:system` specifically, the drawer overlay is the back-compat middle tier. After the overlay runs, a separate restore step reads `:GPTEL_SYSTEM_PROMPT_FILE:` and, when set, installs the sibling file contents as `gptel--system-message` (Requirement: System prompt sibling file is authoritative) — superseding both a legacy `:GPTEL_SYSTEM:` drawer entry and the preset's `:system`. The restore precedence for the system prompt is: sibling file > legacy `:GPTEL_SYSTEM:` drawer entry > preset `:system`. The overlay's role for `:system` is to honor a hand-authored `:GPTEL_SYSTEM:` drawer line for old sessions that have no sibling file.

For the chat-mode extension `GPTEL_PARENT_SESSION_ID`, when present the overlay SHALL bind `jf/gptel--parent-session-id` buffer-locally.

#### Scenario: Drawer-present model wins over preset model
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_MODEL: claude-opus-4-7`
- **AND** the `coding` preset specifies `:model 'claude-sonnet-4-6`
- **THEN** the buffer-local `gptel-model` is `'claude-opus-4-7`

#### Scenario: Sibling file wins over preset for system prompt
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** the sibling file contains `"File-based prompt."` and the preset's `:system` is `"Preset prompt."`
- **THEN** `gptel--system-message` is `"File-based prompt."`

#### Scenario: Legacy drawer GPTEL_SYSTEM respected when no sibling file
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_SYSTEM: Custom override prompt.`
- **AND** the buffer has no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **THEN** the buffer-local `gptel--system-message` is `"Custom override prompt."`
- **NOTE**: the next save will NOT re-write `:GPTEL_SYSTEM:` (writer never emits it); the legacy property sits in the file untouched.

### Requirement: Preset system integration

Chat-mode SHALL integrate with the gptel preset system so that opening a session file applies the preset declared in the configuration drawer.

When `gptel-chat-mode` activates, the activation hook SHALL look for a `:GPTEL_PRESET:` property in the configuration drawer (read via `gptel-org--entry-properties`). When present, it SHALL call `gptel--apply-preset` with that preset name, which installs the preset's tools, model, backend, temperature, max-tokens, num-messages-to-send, and `:system` as buffer-local bindings.

After preset application, the configuration drawer overlay runs (Requirement: Configuration drawer overlay on restore) and binds every drawer-present key buffer-locally, with each drawer value winning over the corresponding preset value when both are present. This makes the drawer the source of truth for the configuration the chat-mode buffer uses.

For `:system` the preset is the lowest tier only. After preset application and the drawer overlay, the system SHALL read `:GPTEL_SYSTEM_PROMPT_FILE:` (Requirement: System prompt sibling file is authoritative); when that property resolves to a readable file with non-empty contents, the file body supersedes the preset's `:system`. The preset's `:system` is used as the buffer's system prompt only for sessions whose sibling file is absent or empty and whose drawer carries no legacy `:GPTEL_SYSTEM:` entry.

When `:GPTEL_PRESET:` is absent, the activation hook SHALL NOT apply any preset; the drawer overlay still runs (every present key is honored), and the sibling-file restore still runs.

#### Scenario: Preset applied when GPTEL_PRESET present
- **WHEN** a chat-mode buffer opens whose drawer contains `:GPTEL_PRESET: coding`
- **THEN** `gptel--apply-preset` is called with `'coding`
- **AND** the preset's tools, model, backend, etc. are installed buffer-locally

#### Scenario: No preset application without GPTEL_PRESET
- **WHEN** a chat-mode buffer opens with no `:GPTEL_PRESET:` line in its drawer
- **THEN** `gptel--apply-preset` is NOT called
- **AND** no buffer-local preset variables are mutated

#### Scenario: Preset :system is the bottom of the three-tier precedence
- **WHEN** a chat-mode buffer opens with `:GPTEL_PRESET: coding`, no `:GPTEL_SYSTEM:` in the drawer, and no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **THEN** `gptel--system-message` is the preset's `:system` value

### Requirement: Chat-mode transient menu and keybindings

The chat-mode transient menu SHALL be a fork of upstream `gptel-menu` that:
- Defaults configuration scope to buffer-local for the lifetime of the menu invocation (existing requirement, unchanged here)
- Replaces the upstream `gptel-system-prompt` infix with an "Edit system prompt" affordance that opens the sibling file in another window (Requirement: Edit system prompt menu affordance opens the sibling file)
- Routes the "Send" command through the chat-mode request submission path (which performs the pre-send sibling-file refresh — Requirement: System prompt sibling file is authoritative)

Upstream `gptel-menu` (invoked outside chat-mode buffers) is unaffected.

#### Scenario: Chat menu replaces system-prompt infix with file-opener
- **WHEN** a user invokes the chat-mode menu in a chat-mode buffer
- **THEN** the menu offers an "Edit system prompt" entry (not the minibuffer-editing upstream `gptel-system-prompt`)
- **AND** invoking the entry opens the sibling system-prompt file in another window

#### Scenario: Send triggers pre-send refresh
- **WHEN** a user invokes "Send" from the chat-mode menu
- **AND** the sibling file referenced by `:GPTEL_SYSTEM_PROMPT_FILE:` differs from the buffer-cached `gptel--system-message`
- **THEN** the request is dispatched with the on-disk file contents (refreshed cache)
