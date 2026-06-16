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
```

**Implementation**: `config/gptel/sessions/filesystem.org`

#### Scenario: New session creation
- **WHEN** running `M-x jf/gptel-persistent-session`
- **THEN** creates `branches/main/` directory
- **AND** no `branch-metadata.yml` in main branch
- **AND** no `scope.yml`, `metadata.yml`, or other sidecar is created (preset and scope live in `session.org`'s `:PROPERTIES:` drawer)

#### Scenario: Branch creation
- **WHEN** running `M-x jf/gptel-branch-session`
- **THEN** creates `branches/<timestamp>-<name>/` directory
- **AND** includes `branch-metadata.yml` with parent reference
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

### Requirement: Drawer-resident session identity

The system SHALL store a session's identity in its `session.org` file-level `:PROPERTIES:` drawer at `point-min`, so that identity travels with the file and does not depend on the filesystem path.

The drawer SHALL carry:
- `:GPTEL_SESSION_ID:` — the session id (format `<slug>-<timestamp>`, see Session Identification).
- `:GPTEL_BRANCH:` — the branch name (e.g. `main`).

Session creation, agent creation, and branch creation SHALL emit these keys into the drawer they already render (alongside `:GPTEL_PRESET:`, the chat-mode snapshot keys, and the `:GPTEL_SCOPE_*:` keys).

Identity resolution SHALL be **drawer-first, basename-fallback**:
- When `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:` are present in the drawer, those values are authoritative.
- When absent (a pre-existing session created before this change), the system SHALL fall back to deriving the id from the directory basename (`jf/gptel--session-id-from-directory`) and the branch from the enclosing `branches/<branch>/` segment, preserving today's behavior. This fallback is a back-compat grace path, NOT an on-disk migration; no rewrite of old files is performed.

Session type SHALL be inferred from drawer content, not from the path layout: a drawer carrying `:GPTEL_PARENT_SESSION_ID:` denotes an agent session; its absence denotes a branch session.

#### Scenario: Fresh session carries identity keys in its drawer
- **WHEN** a session `react-refactoring-20260120153042` is created on branch `main`
- **THEN** the `session.org` drawer at `point-min` contains `:GPTEL_SESSION_ID: react-refactoring-20260120153042`
- **AND** the drawer contains `:GPTEL_BRANCH: main`

#### Scenario: Identity read from the drawer, not the path
- **WHEN** a session buffer is activated and its drawer carries `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:`
- **THEN** the buffer-local `jf/gptel--session-id` / `jf/gptel--branch-name` take the drawer values
- **AND** the directory basename is NOT consulted

#### Scenario: Identity stable across a directory move
- **WHEN** a session directory carrying drawer identity keys is moved or renamed on disk
- **AND** its `session.org` is reopened
- **THEN** the resolved session-id and branch-name are unchanged (they come from the drawer)

#### Scenario: Old session falls back to basename identity
- **WHEN** a `session.org` whose drawer omits `:GPTEL_SESSION_ID:` is opened
- **THEN** the session-id is derived from the directory basename and the branch from the `branches/<branch>/` segment
- **AND** no rewrite of the file is performed

### Requirement: Content-addressed activation and binding

The system SHALL activate and bind a session buffer through the major mode, not through a global file-open hook. When `gptel-chat-mode` becomes active (by any route — `magic-mode-alist` signature, mode cookie, `M-x`, or `find-file-noselect` of a signature-bearing file), `gptel-chat-mode-hook` SHALL run a guarded binder that establishes all session context.

The binder SHALL:
1. Guard on session content, not path: it SHALL proceed only when the buffer carries the session signature (a `point-min` drawer with a `:GPTEL_`-prefixed key). On a non-session chat buffer (e.g. a `gptel-chat-new` scratch buffer with no drawer) it SHALL be a no-op for session-identification.
2. Resolve identity (drawer-first, basename-fallback per "Drawer-resident session identity") and set the four buffer-local session-identification variables: `jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`. `jf/gptel--branch-dir` SHALL be the directory of the buffer's own file (`file-name-directory` of `buffer-file-name`) — derived, not reverse-engineered.
3. Register the buffer in `jf/gptel--session-registry` under the drawer-resolved key.
4. Enable autosave (`jf/gptel-autosave-enabled`).

The binder SHALL NOT enable `gptel-mode` (minor mode), SHALL NOT invoke `gptel--save-state`, and SHALL NOT invoke `gptel--restore-state`. Preset application, drawer overlay, and parent-session-id installation continue to be performed by `gptel-chat--apply-declared-preset` on the same hook (unchanged).

**Implementation**: `config/gptel/sessions/commands.org` — the binder runs from `gptel-chat-mode-hook` (registered in `config/gptel/chat/`); the `magic-mode-alist` signature is registered alongside the mode definition.

#### Scenario: Binding happens on mode activation
- **WHEN** a signature-bearing `session.org` is opened and `gptel-chat-mode` activates
- **THEN** the four buffer-local session variables are set from drawer-resolved identity
- **AND** the buffer is registered in `jf/gptel--session-registry`
- **AND** `gptel-mode` minor mode is NOT enabled

#### Scenario: Non-session chat buffer is not bound as a session
- **WHEN** `gptel-chat-mode` is activated in a buffer with no `point-min` `:GPTEL_` drawer (e.g. a scratch chat buffer)
- **THEN** the binder performs no session-identification and registers nothing
- **AND** the buffer remains a usable chat buffer

#### Scenario: branch-dir is the file's own directory
- **WHEN** the binder sets `jf/gptel--branch-dir`
- **THEN** the value is `(file-name-directory (buffer-file-name))`
- **AND** no `../..` path-walking is performed to compute it

### Requirement: Session discovery and registry

The system SHALL maintain a global in-memory registry for active sessions.

**Registry structure**:
- Key: `"session-id/branch-name"`
- Value: `(:session-id :session-dir :branch-name :branch-dir :buffer)`
- Metadata: NOT cached (read from disk on-demand)

Registry initialization (`jf/gptel--init-registry`) and the filesystem discovery helpers SHALL learn each session's identity by reading the `session.org` drawer (a cheap head-read reusing the session-signature parse), NOT by deriving it from directory names. The registry key is sourced from the drawer's `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:` values, with the basename/segment fallback for files that lack them. Consequently the filesystem layout (`branches/`, `agents/`, directory names) is pure storage convention and carries no identity meaning.

**Implementation**: `config/gptel/sessions/registry.org`, `config/gptel/sessions/filesystem.org`.

#### Scenario: Registry initialization reads drawers
- **WHEN** gptel initializes
- **THEN** `jf/gptel--init-registry` enumerates session files and reads each one's drawer head
- **AND** creates an entry per session/branch keyed by the drawer's `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:` (basename fallback when absent)
- **AND** registry count matches the number of valid branches

#### Scenario: Registry lookup
- **WHEN** looking up a session
- **THEN** `jf/gptel-session-find` returns the plist for `session-id/branch-name`
- **AND** returns nil if not found

#### Scenario: Buffer association
- **WHEN** a session is opened
- **THEN** the registry stores the buffer reference
- **AND** `jf/gptel--update-session-buffer` updates the registry

#### Scenario: Session-level configuration read on-demand
- **WHEN** code needs the active preset, parent-session-id, or scope for a session
- **THEN** the value is read from the session buffer's `:PROPERTIES:` drawer (or, when no buffer is open, from `session.org`'s drawer on disk)
- **AND** the registry holds NO cached copy — `gptel-chat--apply-declared-preset` re-reads the drawer at mode activation

### Requirement: Buffer-local session state

The system SHALL track session metadata in buffer-local variables for runtime access. The identity variables are populated from the drawer (drawer-first, basename-fallback), not from the file path.

#### Scenario: Buffer-local vars set on session open
- **WHEN** the user opens a session's `session.org`
- **THEN** the `gptel-chat-mode-hook` binder sets:
  - `jf/gptel--session-id` (from the drawer's `:GPTEL_SESSION_ID:`, basename fallback)
  - `jf/gptel--branch-name` (from the drawer's `:GPTEL_BRANCH:`, segment fallback)
  - `jf/gptel--branch-dir` (the file's own directory)
  - `jf/gptel--session-dir` (the session root for this branch)
- **AND** `gptel-chat-mode-hook` runs `gptel-chat--apply-declared-preset`, which reads the file-level `:PROPERTIES:` drawer and sets `jf/gptel--parent-session-id` from `:GPTEL_PARENT_SESSION_ID:` when present (agent sessions)

#### Scenario: Agent session vars
- **WHEN** opening an agent session
- **THEN** `jf/gptel--parent-session-id` is set from the drawer's `:GPTEL_PARENT_SESSION_ID:` value
- **AND** `jf/gptel--branch-name` is `main` (from the drawer's `:GPTEL_BRANCH:` or the default; agents don't branch)

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
- Uses content-addressed activation (`magic-mode-alist` signature) — no find-file-hook
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
