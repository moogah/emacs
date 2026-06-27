## MODIFIED Requirements

### Requirement: session.org as authoritative session file

`session.org` SHALL be the single authoritative file for session content and per-buffer configuration. It is in `gptel-chat-mode` block format (`#+begin_user` / `#+begin_assistant` / nested `#+begin_tool`), with a `:PROPERTIES:` drawer at point-min that carries a **full snapshot** of the active configuration: `GPTEL_PRESET`, `GPTEL_PARENT_SESSION_ID` (when applicable), the upstream-compatible chat-mode keys (`GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND`), the scope keys (`GPTEL_SCOPE_*`), and — when the session carries a system prompt — `:GPTEL_SYSTEM_PROMPT_FILE:` pointing at a sibling file in the session's branch directory.

The drawer does NOT carry `:GPTEL_SYSTEM:` — the system prompt is not a drawer property value, and is also not embedded as document content in `session.org`. The system prompt is carried as a separate file in the session's branch directory, referenced from the drawer via `:GPTEL_SYSTEM_PROMPT_FILE:` (see `gptel/chat-mode` Requirement: System prompt sibling file is authoritative). Keeping the prompt out of `session.org` entirely means org-mode parsing, fontification, and folding never interact with the prompt's content (typically markdown with code blocks and XML-like tags).

The drawer does NOT carry `:GPTEL_BOUNDS:` — incompatible with the chat-mode block format.

Initial content for a fresh branch session SHALL be the file-level config drawer at `point-min`, followed (after a blank line) by the empty user block — no headings:
```
:PROPERTIES:
:GPTEL_PRESET: <name>
:GPTEL_MODEL: <preset-model>
:GPTEL_BACKEND: <preset-backend>
:GPTEL_TOOLS: <preset-tool-names>
[:GPTEL_TEMPERATURE: <preset-temperature>]
[:GPTEL_MAX_TOKENS: <preset-max-tokens>]
[:GPTEL_NUM_MESSAGES_TO_SEND: <preset-num-messages-to-send>]
[:GPTEL_SCOPE_READ: ...]
[:GPTEL_SCOPE_WRITE: ...]
[other :GPTEL_SCOPE_* keys]
[:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md]
:END:

#+begin_user

#+end_user
```

The file-level config drawer location is unchanged (`point-min`, no heading or blank line precedes `:PROPERTIES:`). The `* System Prompt` and `* Chat` headings introduced by the prior change are removed — neither appears in the canonical layout. Turn blocks (`#+begin_user` / `#+begin_assistant`) appear directly under the drawer, as in the pre-Addendum layout. The chat parser remains heading-indifferent (locates turn blocks by `#+begin_*` markers, not headings); `gptel-chat-new` scratch buffers (bare `#+begin_user` blocks, no drawer) remain valid input.

When the active preset declares a non-empty `:system`, session creation SHALL also write a sibling file (`system-prompt.<ext>`, extension mirroring the preset's source file) containing the preset's `:system` body verbatim, and SHALL emit `:GPTEL_SYSTEM_PROMPT_FILE: <basename>` into the drawer. When the preset declares no `:system`, neither the sibling file nor the drawer property is emitted; the chat-mode restore falls through to the preset's (empty) `:system`.

Initial content for a fresh agent session additionally carries `:GPTEL_PARENT_SESSION_ID: <parent-id>` after `:GPTEL_PRESET:`. Only the keys whose preset value is non-nil are emitted; presets that don't declare e.g. `:max-tokens` produce a drawer without the corresponding line.

#### Scenario: Fresh branch session.org carries drawer and bare user block
- **WHEN** creating a session with preset `coding`
- **THEN** `session.org` starts with a file-level `:PROPERTIES:` config drawer at `point-min` containing `:GPTEL_PRESET: coding`
- **AND** the drawer contains `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:` populated from the `coding` preset
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the drawer is followed by an empty `#+begin_user\n\n#+end_user\n` block (no heading between drawer and block)
- **AND** the file contains no `* System Prompt` heading
- **AND** the file contains no `* Chat` heading
- **AND** no `metadata.yml` is written in the branch directory

#### Scenario: Fresh branch session.org with preset that has a system prompt
- **WHEN** creating a session with the `coding` preset, whose `:system` text is `"You are a coding assistant."`
- **THEN** the drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** a file `system-prompt.md` exists in the session's branch directory with content `"You are a coding assistant."` (verbatim, including any trailing newline from the preset source)
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`

#### Scenario: Fresh branch session.org with preset that has no system prompt
- **WHEN** creating a session with a preset that declares no `:system` (or whose `:system` is empty/whitespace-only)
- **THEN** the drawer does NOT contain `:GPTEL_SYSTEM_PROMPT_FILE:`
- **AND** no `system-prompt.<ext>` file is created in the branch directory
- **AND** the document layout is otherwise identical — drawer + empty user block

#### Scenario: Sibling file extension mirrors the preset source file
- **WHEN** creating a session from a preset whose source file is `executor.md`
- **THEN** the sibling file is named `system-prompt.md`
- **AND** the drawer carries `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **NOTE**: when `.org`-format presets are introduced (out of scope for this change), the same emission produces `system-prompt.org`.

#### Scenario: Fresh agent session.org carries parent session id and full snapshot
- **WHEN** creating an agent session under parent `p-abc-20260424000000` with preset `executor`
- **THEN** the agent `session.org` drawer contains `:GPTEL_PRESET: executor` AND `:GPTEL_PARENT_SESSION_ID: p-abc-20260424000000`
- **AND** the drawer contains `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:` populated from the `executor` preset
- **AND** the drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md` (when the `executor` preset has a `:system`)
- **AND** a sibling `system-prompt.md` exists with the preset's `:system` body
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the file contains no `* System Prompt` heading and no `* Chat` heading

#### Scenario: Fresh agent session.org matches the canonical document layout
- **WHEN** `PersistentAgent` creates an agent session with preset `executor` and initial prompt `"DO THE THING"`
- **THEN** the agent `session.org` starts with a file-level `:PROPERTIES:` config drawer at `point-min` (no heading, no blank line precedes `:PROPERTIES:`)
- **AND** the drawer is followed by a populated `#+begin_user\nDO THE THING\n#+end_user\n` block (no `* Chat` heading)
- **AND** the file contains no `* System Prompt` heading
- **AND** the file contains no `* Chat` heading

**Implementation**: `config/gptel/tools/persistent-agent.org` — `jf/gptel-persistent-agent--task` (orchestrator) → `jf/gptel-persistent-agent--initial-body` (returns the drawer + bare user block, with no heading helper involved). The interactive and agent creation paths share the sibling-file emission code in `config/gptel/sessions/commands.org`.

### Requirement: Session creation

The system SHALL provide an interactive command for creating persistent sessions. `jf/gptel-persistent-session` SHALL create new sessions with `session.org` as the session file, populated with a pre-configured `:PROPERTIES:` drawer carrying:

- `:GPTEL_PRESET: <name>`
- `:GPTEL_PARENT_SESSION_ID: <id>` for agent sessions
- The full upstream-compatible chat-mode snapshot drawn from the resolved preset spec: `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, `:GPTEL_NUM_MESSAGES_TO_SEND:` (each emitted only when the preset declares a non-nil value for the corresponding key)
- The scope keys `:GPTEL_SCOPE_*:` resolved from the preset's scope profile
- `:GPTEL_SYSTEM_PROMPT_FILE: <basename>` when the resolved preset has a non-empty `:system` and a sibling file is written (Requirement: session.org as authoritative session file)

The file-level config drawer is followed (after a blank line) by the chat-mode empty user block (`#+begin_user\n\n#+end_user\n`). No `* System Prompt` heading is emitted, and no `* Chat` heading is emitted.

Session creation SHALL additionally write a sibling system-prompt file when the resolved preset declares a non-empty `:system`. The file is named `system-prompt.<ext>` where `<ext>` is the extension of the preset's source file (today: `.md` for all presets, since `jf/gptel-preset-register-all` scans only `*.md`; the design accommodates `.org` when that scanner is widened). The file is written into the same directory as `session.org` with the preset's `:system` body verbatim (no transformation, no escaping).

`:GPTEL_SYSTEM:` is NOT emitted as a drawer property at creation. The preset's `:system` text is written to the sibling file referenced by `:GPTEL_SYSTEM_PROMPT_FILE:` (or, when the preset declares no `:system`, no sibling file and no property are emitted; the chat-mode restore falls through to the preset's empty `:system`).

`metadata.yml` is NOT created.

**Implementation**: `config/gptel/sessions/commands.org` — `jf/gptel-persistent-session`, `jf/gptel--create-session-core`, `jf/gptel--initial-session-body`. The drawer-text rendering is delegated to `config/gptel/scope-profiles.org`'s `jf/gptel-scope-profile--render-drawer-text` (unchanged from prior). The sibling-file writer is a new helper in `config/gptel/sessions/commands.org`: it resolves the preset's source file path (basename + `jf/gptel-presets-directory`), derives the extension, writes `system-prompt.<ext>` with the preset's `:system` body, and returns the basename to be threaded into the drawer text.

#### Scenario: Create session with default preset writes drawer plus bare user block plus sibling file
- **WHEN** run `M-x jf/gptel-persistent-session`
- **THEN** prompted for name
- **AND** generates session-id with timestamp
- **AND** creates directory structure
- **AND** creates `branches/main/session.org` whose file-level config drawer at point-min contains `:GPTEL_PRESET: <default-preset>`, `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:` from that preset, plus `:GPTEL_SCOPE_*:` keys
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md` (when the preset has a non-empty `:system`)
- **AND** a sibling `system-prompt.md` exists in `branches/main/` with the preset's `:system` body verbatim
- **AND** the drawer is followed by the empty user block `#+begin_user\n\n#+end_user\n` with no intervening heading
- **AND** the file contains no `* System Prompt` heading and no `* Chat` heading
- **AND** does NOT create `metadata.yml`
- **AND** opens session.org in `gptel-chat-mode` with the preset applied via the drawer

#### Scenario: Create session with selected preset writes its snapshot and sibling file
- **WHEN** run `C-u M-x jf/gptel-persistent-session`
- **THEN** prompted for preset selection
- **AND** the selected preset's `:model`, `:backend`, `:tools`, etc. are written to the drawer
- **AND** scope from preset's profile is written to the drawer's `:GPTEL_SCOPE_*:` keys (no `scope.yml` is created)
- **AND** the sibling system-prompt file is written when the preset has a `:system`

#### Scenario: Create agent session writes full snapshot plus parent-session-id plus sibling file
- **WHEN** `PersistentAgent` creates an agent under parent session `p-abc-20260424000000` with preset `executor`
- **THEN** agent `session.org` drawer contains `:GPTEL_PRESET: executor`, `:GPTEL_PARENT_SESSION_ID: p-abc-20260424000000`, and the `executor` preset's snapshot keys
- **AND** the drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md` (assuming the `executor` preset has a `:system`)
- **AND** a sibling `system-prompt.md` exists in the agent's session directory
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** does NOT create `metadata.yml`

#### Scenario: Preset with no :system produces no sibling file and no drawer link
- **WHEN** creating a session with a preset whose `:system` is nil or empty
- **THEN** the drawer does NOT contain `:GPTEL_SYSTEM_PROMPT_FILE:`
- **AND** no `system-prompt.<ext>` file is created in the branch directory
- **AND** the chat-mode restore relies on the preset's (empty) `:system` — `gptel--system-message` remains at whatever the preset/global default installs

#### Scenario: Preset with sparse keys produces a sparse drawer
- **WHEN** the preset declares only `:model` and `:tools` (no `:temperature`, no `:max-tokens`, no `:num-messages-to-send`)
- **AND** a session is created with that preset
- **THEN** the drawer contains `:GPTEL_PRESET:`, `:GPTEL_MODEL:`, `:GPTEL_TOOLS:` only (plus `:GPTEL_SCOPE_*:` if scope-profile applies, plus `:GPTEL_SYSTEM_PROMPT_FILE:` if the preset has a `:system`)
- **AND** the drawer does NOT contain `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, or `:GPTEL_NUM_MESSAGES_TO_SEND:`

#### Scenario: Create session with projects
- **WHEN** user selects projects during creation
- **THEN** first project used as project-root for scope expansion
- **AND** `${project_root}` variables expanded to project path
