# Sessions Persistence

## Purpose

Provides file-based persistence for gptel conversations across Emacs sessions. Enables long-lived conversations with:
- Directory-based storage (introspectable on disk)
- Branch support (fork conversations)
- Auto-restoration (sessions initialize on file open)
- Scope-based access control (declarative permissions)
- Activities integration (project-isolated sessions)

## Key Concepts

### Directory Structure

```
~/.gptel/sessions/<session-id>/
├── branches/<branch-name>/
│   ├── session.org         # Conversation history (chat-mode block format) +
│   │                       # file-level :PROPERTIES: drawer (preset snapshot,
│   │                       # parent-session-id, scope keys)
│   ├── branch-metadata.yml # Branch lineage (non-main only)
│   ├── tools.org           # Tool log (optional)
│   ├── system-prompts.org  # Prompt log (optional)
│   └── agents/             # Sub-agents (optional)
└── current -> branches/<branch-name>  # Active branch symlink
```

`session.org` is the single authoritative file for session content AND session-level configuration. All preset, parent-session-id, and scope state lives in its file-level `:PROPERTIES:` drawer at `point-min`. The sessions subsystem does NOT write `metadata.yml`, `scope.yml`, `scope-plan.yml`, or any other session-level sidecar. (`branch-metadata.yml` is the lone exception — it carries branch lineage information and is documented in sessions-branching.md.)

### File Formats

**session.org**:
- Format: Org-mode using `gptel-chat-mode`'s symmetric special-block syntax
  (`#+begin_user` / `#+begin_assistant` with nested `#+begin_tool` blocks;
  delimiter lines at column 0)
- Content: file-level `:PROPERTIES:` drawer at `point-min` (preset, parent
  session id, and `:GPTEL_SCOPE_*` keys) followed by conversation history;
  self-describing — no sidecar bounds or Local Variables block
- Initial: `:PROPERTIES:` drawer (`:GPTEL_PRESET:`, scope keys, etc.)
  followed by `"#+begin_user\n\n#+end_user\n"`
- Persistence: plain `save-buffer`; no `gptel--save-state` round-trip
- Mutable scope configuration lives in the drawer and is updated in place
  by the expansion UI

**branch-metadata.yml**:
- Format: YAML
- Location: Non-main branches only
- Keys: `parent_branch`, `created`, `branch_point_position` (optional)
- Purpose: branch lineage (parent-child links). Session-level metadata
  (preset, parent-session-id) does NOT live here — see `session.org`'s
  `:PROPERTIES:` drawer below.

### Session Identification

**Format**: `<slug>-<timestamp>`
- Slug: lowercased, alphanumeric-and-dash only
- Timestamp: `%Y%m%d%H%M%S` (14 digits)
- Example: `react-refactoring-20260120153042`
- Immutable after creation

### Registry

**Data structure**: Hash table `jf/gptel--session-registry`
- **Key**: `"session-id/branch-name"` (string)
- **Value**: `(:session-id :session-dir :branch-name :branch-dir :buffer)` (plist)
- **Important**: Session-level configuration (preset, parent-session-id,
  scope) is NOT cached in the registry — it lives in `session.org`'s
  `:PROPERTIES:` drawer and is read on-demand at mode activation by
  `gptel-chat--apply-declared-preset`.
- **Source of truth**: Filesystem (`session.org`'s drawer), not registry

### Buffer-Local Variables

Session buffers have these buffer-local vars:
- `jf/gptel--session-id`
- `jf/gptel--session-dir`
- `jf/gptel--branch-name`
- `jf/gptel--branch-dir`
- `jf/gptel--parent-session-id` (for agents — read from the drawer's
  `:GPTEL_PARENT_SESSION_ID:` at mode activation)
- `jf/gptel-autosave-enabled`
- `gptel-activity-worktrees` (for activities)

## Requirements

### Requirement: Directory structure initialization

The system SHALL create this hierarchy for each session:

```
<session-dir>/
├── branches/<branch-name>/
│   ├── session.org
│   └── branch-metadata.yml (if not main)
└── current -> branches/<branch-name>
```

**Implementation**: `config/gptel/sessions/filesystem.org`

#### Scenario: New session creation
- **WHEN** running `M-x jf/gptel-persistent-session`
- **THEN** creates `branches/main/` directory
- **AND** `current` symlink points to `branches/main`
- **AND** no `branch-metadata.yml` in main branch
- **AND** no `scope.yml`, `metadata.yml`, or other sidecar is created (preset and scope live in `session.org`'s `:PROPERTIES:` drawer)

#### Scenario: Branch creation
- **WHEN** running `M-x jf/gptel-branch-session`
- **THEN** creates `branches/<timestamp>-<name>/` directory
- **AND** includes `branch-metadata.yml` with parent reference
- **AND** updates `current` symlink to new branch
- **AND** the branch's `session.org` carries its own `:PROPERTIES:` drawer (preset, scope keys, parent-session-id when applicable) inherited verbatim from the parent branch's `session.org` (the drawer travels with the org file when context is copied — see sessions-branching.md)

#### Scenario: Agent session creation
- **WHEN** PersistentAgent tool creates sub-agent
- **THEN** creates `branches/<branch-name>/agents/<preset>-<timestamp>-<desc>/`
- **AND** the agent directory contains `session.org` with a `:PROPERTIES:` drawer carrying `:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, the agent's scope keys, and the upstream-compatible chat-mode snapshot
- **AND** no `metadata.yml` sidecar is written (agent-type and parent-session-id live in the drawer, not in a sidecar)

### Requirement: Session file formats

Session-level metadata (preset name, parent-session-id, scope keys, chat-mode snapshot) lives in `session.org`'s file-level `:PROPERTIES:` drawer at `point-min`. The sessions subsystem does NOT emit `metadata.yml`, `scope.yml`, or any other session-level sidecar — see "Requirement: session.org as authoritative session file" below for the drawer-key contract. (`branch-metadata.yml` carries branch lineage only and is documented in sessions-branching.md.)

#### session.org format

SHALL be org-mode using `gptel-chat-mode`'s symmetric special-block syntax (`#+begin_user` / `#+begin_assistant` with nested `#+begin_tool`, delimiter lines at column 0). Initial content: `"#+begin_user\n\n#+end_user\n"`. The format is self-describing; no `gptel--bounds` property drawer or Local Variables block is written by the sessions subsystem. Persistence is plain `save-buffer`; `gptel--save-state` and `gptel--restore-state` are NOT invoked on session buffers.

Legacy `session.md` branches from before the chat-mode cutover remain on disk in their original format but are invisible to the sessions subsystem (see `jf/gptel--valid-branch-directory-p` — only branches containing `session.org` are enumerated).

##### Scenario: Initial session.org
- **WHEN** creating session
- **THEN** session.org created with content `"#+begin_user\n\n#+end_user\n"`
- **AND** file is valid chat-mode (parseable, empty conversation)
- **AND** no Local Variables block is present

##### Scenario: Conversation persistence
- **WHEN** user makes requests and saves (`C-x C-s`)
- **THEN** `save-buffer` writes the chat-mode block structure to session.org
- **AND** no `gptel--bounds` Local Variables block is appended
- **AND** no separate metadata sidecar is touched — `session.org` (drawer + conversation body) is the only file the save path writes for session-level state

##### Scenario: Legacy `session.md` branches are not enumerated
- **WHEN** the sessions subsystem scans for session files (e.g., in `jf/gptel--init-registry` or `jf/gptel--find-all-branches-with-agents`)
- **THEN** only branch directories containing `session.org` are surfaced
- **AND** branch directories containing only legacy `session.md` are filtered out inside the enumeration helper
- **AND** a session directory whose branches all use legacy `session.md` still passes `jf/gptel--valid-session-directory-p` but contributes zero entries to enumeration

### Requirement: Session discovery and registry

The system SHALL maintain global in-memory registry for active sessions.

**Registry structure**:
- Key: `"session-id/branch-name"`
- Value: `(:session-id :session-dir :branch-name :branch-dir :buffer)`
- Metadata: NOT cached (read from disk on-demand)

**Implementation**: `config/gptel/sessions/registry.org`

#### Scenario: Registry initialization
- **WHEN** gptel initializes
- **THEN** `jf/gptel--init-registry` scans all session directories
- **AND** creates entry for each session/branch pair
- **AND** registry count matches number of valid branches

#### Scenario: Registry lookup
- **WHEN** looking up session
- **THEN** `jf/gptel-session-find` returns plist for session-id/branch-name
- **AND** returns nil if not found

#### Scenario: Buffer association
- **WHEN** session opened
- **THEN** registry stores buffer reference
- **AND** `jf/gptel--update-session-buffer` updates registry

#### Scenario: Session-level configuration read on-demand
- **WHEN** code needs the active preset, parent-session-id, or scope for a session
- **THEN** the value is read from the session buffer's `:PROPERTIES:` drawer (or, when no buffer is open, from `session.org`'s drawer on disk)
- **AND** the registry holds NO cached copy — `gptel-chat--apply-declared-preset` re-reads the drawer at mode activation

### Requirement: Buffer-local session state

The system SHALL track session metadata in buffer-local variables for runtime access.

#### Scenario: Buffer-local vars set on session open
- **WHEN** user opens `branches/main/session.org`
- **THEN** auto-init sets:
  - `jf/gptel--session-id` (from directory name)
  - `jf/gptel--session-dir` (absolute path to session)
  - `jf/gptel--branch-name` (extracted from file path)
  - `jf/gptel--branch-dir` (absolute path to branch)
- **AND** `gptel-chat-mode-hook` runs `gptel-chat--apply-declared-preset`, which reads the file-level `:PROPERTIES:` drawer and sets `jf/gptel--parent-session-id` from `:GPTEL_PARENT_SESSION_ID:` when present (agent sessions)

#### Scenario: Agent session vars
- **WHEN** opening agent session
- **THEN** `jf/gptel--parent-session-id` is set from the drawer's `:GPTEL_PARENT_SESSION_ID:` value
- **AND** `jf/gptel--branch-name` set to "main" (agents don't branch)

### Requirement: Auto-initialization enables `gptel-chat-mode`

The auto-init hook (`jf/gptel--auto-init-session-buffer`) SHALL detect session files by matching the path pattern `*/branches/<branch-name>/session.org` (or the nested agent shape `*/<session-id>/branches/<branch>/agents/<agent>/session.org` and the flat legacy agent shape `*/<session-id>/agents/<agent>/session.org`). On match, it SHALL:

1. Extract `session-id` and `branch-name` from the path (branch-name defaults to `"main"` for the flat legacy agent shape that has no `branches/` segment).
2. Ensure the major mode is `gptel-chat-mode` (switching if necessary). The mode hook then runs `gptel-chat--apply-declared-preset`, which reads the file-level `:PROPERTIES:` drawer at `point-min` and applies its `:GPTEL_PRESET:` buffer-locally, sets `jf/gptel--parent-session-id` from `:GPTEL_PARENT_SESSION_ID:` (when present), and installs the scope keys.
3. Set the four buffer-local session-identification variables (`jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`). These run AFTER mode activation because mode-switch wipes buffer-locals via `kill-all-local-variables`.
4. Register the buffer in `jf/gptel--session-registry`.
5. Update the `current` symlink to point at this branch (suppressed for the flat legacy agent shape, which has no `branches/` directory).

**Ordering is load-bearing.** Mode activation runs before session-var setup so that `gptel-chat-mode-hook`'s drawer-driven preset application sees a clean buffer; the session-identification vars are then re-set on top of the mode-cleared buffer-local table.

The hook SHALL NOT enable `gptel-mode` (minor mode), SHALL NOT invoke `gptel--save-state`, and SHALL NOT invoke `gptel--restore-state`.

**Implementation**: `config/gptel/sessions/commands.org` — `jf/gptel--auto-init-session-buffer` via find-file-hook.

#### Scenario: Session file detection
- **WHEN** file matches `*/branches/<branch-name>/session.org` pattern
- **THEN** auto-init recognizes as branch session
- **AND** extracts session-id and branch-name from path
- **AND** enables `gptel-chat-mode` as the major mode

#### Scenario: Agent file detection (nested)
- **WHEN** file matches `*/<session-id>/branches/<branch>/agents/<agent>/session.org` pattern
- **THEN** auto-init recognizes as nested agent session
- **AND** extracts both session-id and branch-name from the path
- **AND** enables `gptel-chat-mode` as the major mode
- **AND** updates the `current` symlink (the `branches/` segment is present)

#### Scenario: Agent file detection (flat legacy)
- **WHEN** file matches `*/<session-id>/agents/<agent>/session.org` pattern (no `branches/` segment)
- **THEN** auto-init recognizes as flat legacy agent session
- **AND** extracts session-id from the path
- **AND** sets branch-name to `"main"` as the default
- **AND** enables `gptel-chat-mode` as the major mode
- **AND** suppresses the `jf/gptel--update-current-symlink` side-effect

#### Scenario: Parent session id is populated from the drawer
- **WHEN** mode-activation runs `gptel-chat--apply-declared-preset` on an agent or branch `session.org`
- **AND** the file-level `:PROPERTIES:` drawer contains a `:GPTEL_PARENT_SESSION_ID:` line
- **THEN** the buffer-local `jf/gptel--parent-session-id` is set to that value
- **WHEN** the drawer does NOT contain `:GPTEL_PARENT_SESSION_ID:`
- **THEN** `jf/gptel--parent-session-id` remains nil (its `defvar-local` default)

#### Scenario: New session (preset from drawer)
- **WHEN** a freshly created `session.org` is opened for the first time
- **THEN** `gptel-chat-mode-hook` reads the file-level drawer's `:GPTEL_PRESET:` value
- **AND** `gptel-chat--apply-declared-preset` applies that preset via `gptel--apply-preset` with a buffer-local setter
- **AND** `gptel-chat-mode` is active
- **AND** `gptel-mode` minor mode is NOT enabled

#### Scenario: Existing session (no Local Variables round-trip)
- **WHEN** a previously-saved `session.org` is reopened
- **THEN** mode-activation reads `:GPTEL_PRESET:` from the file-level drawer (the authoritative source)
- **AND** applies it buffer-locally
- **AND** does NOT call `gptel--restore-state` or parse any Local Variables block
- **AND** does NOT read any `metadata.yml` sidecar (none exists)

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

### Requirement: Branch management

The system SHALL support creating conversation branches within sessions.

**Implementation**: `config/gptel/sessions/filesystem.org`

#### Scenario: Current symlink tracking
- **WHEN** new session created
- **THEN** `current` symlink points to `branches/main`
- **AND** `jf/gptel--get-current-branch-name` returns "main"

#### Scenario: Switch to another branch
- **WHEN** user opens different branch
- **THEN** auto-init calls `jf/gptel--update-current-symlink`
- **AND** `current` now points to new branch

#### Scenario: List branches
- **WHEN** calling `jf/gptel--list-branches`
- **THEN** returns list of branch names
- **AND** excludes hidden directories

### Requirement: Scope profile integration

The system SHALL embed scope configuration from the preset's scope profile in the new `session.org`'s `:PROPERTIES:` drawer at session creation time.

**Implementation**: Calls `jf/gptel-scope-profile--create-for-session` during session creation. The function returns a drawer block (string) that is prepended to the chat-mode initial content before writing `session.org`.

#### Scenario: Drawer populated from preset
- **WHEN** session created with preset `executor` (has `:scope-profile "coding"`)
- **THEN** the new `session.org` is written with a drawer containing `:GPTEL_PRESET: executor` and the resolved scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`, `:GPTEL_SCOPE_CLOUD_AUTH:`, etc.) from the `coding` profile
- **AND** the drawer is followed by the chat-mode initial content (`#+begin_user\n\n#+end_user\n`)

#### Scenario: Drawer with variable expansion
- **WHEN** selected project is `/Users/user/projects/my-project`
- **AND** profile contains `${project_root}/src/**/*.ts`
- **THEN** the corresponding `:GPTEL_SCOPE_READ:` (or similar) line in the drawer reads `/Users/user/projects/my-project/src/**/*.ts`

### Requirement: Activities integration

The system SHALL support creating sessions tied to activities with project isolation.

#### Scenario: Activity session creation
- **WHEN** creating session via activities-extensions
- **THEN** session directory created in activity's session/ subdirectory
- **AND** worktree paths stored in Local Variables (`gptel-activity-worktrees`)
- **AND** auto-opens when activity resumed

#### Scenario: Worktree paths available
- **WHEN** session created with activities integration
- **AND** activity has worktree projects `/project-a`, `/project-b`
- **THEN** Local Variables includes `gptel-activity-worktrees` list
- **AND** tools can access via buffer-local variable

## Integration Points

### With Upstream gptel
- Uses `gptel--apply-preset` and `gptel-get-preset` for preset application
- Session buffers use `gptel-chat-mode` (downstream) exclusively; upstream
  `gptel-mode` is NOT enabled on session buffers
- `gptel--save-state` / `gptel--restore-state` are NOT invoked on session
  buffers — the chat-mode block format is self-describing
- Plain `save-buffer` persists conversation AND the drawer (via the
  chat-mode save path's drawer materialiser); no metadata sidecar is
  refreshed because none exists

### With Scope Subsystem
- Scope created via `jf/gptel-scope-profile--create-for-session`
- The session's `session.org` is written with a `:PROPERTIES:` drawer (preset, parent session id, `:GPTEL_SCOPE_*` keys) at creation time
- Scope system reads from the drawer (chat buffer, or `session.org` file when no buffer is open) for enforcement

### With Activities Integration
- Activities creates sessions in activity directories
- Provides worktree paths for scope isolation
- Manages Local Variables for worktree tracking

### With File Discovery
- Uses find-file-hook for auto-initialization
- Sessions openable via C-x C-f, dired, recentf, command line
- No special resume commands needed

## Summary

The Sessions Persistence System provides directory-based storage for gptel conversations. Key characteristics:
- **Filesystem as source of truth** (minimal caching)
- **Auto-initialization** (no special resume commands)
- **Branch support** (conversation forks)
- **Scope-based access control** (via `session.org`'s `:PROPERTIES:` drawer)
- **Activities integration** (project isolation)
- **Introspectable storage** (plain text files)
