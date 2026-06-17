## ADDED Requirements

### Requirement: Agent working directory (parent-supplied)

The parent SHALL set the agent's working directory explicitly; the agent SHALL NOT inherit it by any runtime cascade. PersistentAgent SHALL accept a `work_root` parameter naming the absolute directory the agent operates in. When `work_root` is omitted, it SHALL default to the parent session's own work root (the value of the parent buffer's `GPTEL_WORK_ROOT` / `default-directory` at spawn time) — a single explicit default chosen at creation, written into the agent's drawer, not a live link to the parent. `jf/gptel-persistent-agent--task` SHALL write the resolved value into the agent's `session.org` drawer as `:GPTEL_WORK_ROOT:`, from which the standard activation binder sets the agent buffer's `default-directory` (see `gptel/sessions-persistence` Requirement: Default-directory resolution on session activation).

As a consistency guardrail against the silent-deny trap (a relative write resolving outside the agent's write scope), the tool SHALL warn when the resolved `work_root` falls outside the agent's read scope. The warning SHALL NOT abort agent creation.

**Implementation**: `config/gptel/tools/persistent-agent.org` (`jf/gptel-persistent-agent--task`, drawer write).

#### Scenario: Explicit work_root written to the agent drawer
- **WHEN** an agent is created with `work_root "/Users/x/proj/worktree-a"`
- **THEN** the agent's `session.org` drawer contains `:GPTEL_WORK_ROOT: /Users/x/proj/worktree-a`
- **AND** on activation the agent buffer's `default-directory` is `/Users/x/proj/worktree-a/`

#### Scenario: Omitted work_root defaults to the parent's work root
- **WHEN** an agent is created without a `work_root` argument
- **AND** the parent session's work root is `/Users/x/proj`
- **THEN** the agent's drawer records `:GPTEL_WORK_ROOT: /Users/x/proj`
- **AND** the value is frozen at creation time (not a live reference to the parent buffer)

#### Scenario: Guardrail warns when work_root escapes read scope
- **WHEN** the resolved `work_root` is not matched by any of the agent's `read_paths`
- **THEN** a warning is emitted noting the work root falls outside the agent's read scope
- **AND** the agent is still created

## MODIFIED Requirements

### Requirement: Tool invocation and validation

PersistentAgent SHALL only operate within persistent session buffers and SHALL accept exactly six parameters: `preset`, `description`, `prompt`, `work_root`, `read_paths`, and `write_paths`. The previously advertised `allowed_paths` and `denied_paths` parameters SHALL NOT exist on the tool surface; `read_paths` replaces the read-scope role formerly played by `allowed_paths`, and `write_paths` makes write scope parent-controlled (see *Requirement: Agent session creation* for how `/tmp` scratch is handled).

**Tool registration**:
- `:async t` (non-blocking parent)
- `:confirm t` (requires user confirmation)
- `:include t` (results appear in parent)
- `:category "gptel-persistent"`

**Categorized as "meta"**: bypasses scope validation for tool invocation itself (agent's tools still respect agent's scope).

#### Scenario: Parent session requirement
- **WHEN** PersistentAgent is invoked from a buffer where `jf/gptel--session-dir` is unbound
- **THEN** the tool raises a user-error with text `"PersistentAgent requires parent persistent session"`
- **AND** no agent directory, file, or buffer is created

#### Scenario: Tool argument schema
- **WHEN** the registered tool's `:args` are inspected
- **THEN** the argument list contains exactly `preset`, `description`, `prompt`, `work_root`, `read_paths`, and `write_paths`
- **AND** does NOT contain `allowed_paths` or `denied_paths`

#### Scenario: Explicit path configuration (zero inheritance)
- **WHEN** `read_paths` is omitted, `nil`, or an empty array
- **THEN** the agent's `session.org` `:PROPERTIES:` drawer omits `:GPTEL_SCOPE_READ:` (the validator loads `:read nil`, i.e. no read permissions)
- **AND** the parent's drawer scope keys are NOT copied into the agent's drawer
- **WHEN** `read_paths` is provided as a non-empty array of glob patterns
- **THEN** the agent's drawer declares `:GPTEL_SCOPE_READ:` containing exactly the supplied patterns
- **AND** the parent's drawer scope keys are NOT merged in

### Requirement: Agent session creation

The system SHALL create agent sessions as standard `gptel-chat-mode` sessions, sharing the same drawer-driven configuration and content-addressed activation pipeline as standalone interactive sessions. The agent's `session.org` SHALL be written with a `:PROPERTIES:` drawer at `point-min` declaring the agent's preset (`:GPTEL_PRESET:`), its parent link (`:GPTEL_PARENT_SESSION_ID:`), its working directory (`:GPTEL_WORK_ROOT:`), the agent's scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`), and `:GPTEL_SYSTEM_PROMPT_FILE:` (always present for agents). The agent's write scope SHALL be the parent-supplied `write_paths` with `/tmp/**` auto-appended as scratch space; `/tmp/**` is a guaranteed scratch grant, NOT the write default it was when `write_paths` did not exist. The drawer SHALL be followed directly by the initial `#+begin_user` / `#+end_user` block, with no `* System Prompt` heading and no `* Chat` heading. A sibling `system-prompt.<ext>` file SHALL always be written next to `session.org` holding the agent-harness preamble followed by the preset's `:system` body when it declares one (see *Requirement: Agent system-prompt preamble* and `gptel/chat-mode` Requirement: System prompt sibling file is authoritative). NO sidecar config files (`scope.yml`, `metadata.yml`, `tools.org`) are written — all configuration and metadata lives in the session-file drawer; the conversation including tool blocks lives directly under the drawer as turn blocks.

The session file SHALL be opened with `find-file-noselect` so that the file's drawer signature triggers content-addressed activation (`magic-mode-alist`) into `gptel-chat-mode`, and the `gptel-chat-mode-hook` binder applies the drawer-declared preset buffer-local, sets `default-directory` from `:GPTEL_WORK_ROOT:`, registers the buffer in `jf/gptel--session-registry`, and enables autosave.

The agent's directory SHALL live under the parent branch's `agents/` subdirectory and SHALL be named `<preset>-<timestamp>-<slug>`. Agents SHALL NOT have a `branches/` subdirectory or a `current` symlink — they remain single-timeline sessions.

#### Scenario: Agent directory created under parent branch
- **WHEN** an agent is created with preset `researcher` and description `analyze code` from a parent branch
- **THEN** a directory `<parent-branch-dir>/agents/researcher-<timestamp>-analyze-code/` is created
- **AND** the directory is under the parent's current branch, not the parent's session root
- **AND** the directory does NOT contain a `branches/` subdirectory or a `current` symlink

#### Scenario: session.org carries a self-describing :PROPERTIES: drawer
- **WHEN** an agent is created with preset `researcher` and parent session id `parent-20260425100000`
- **THEN** the agent's `session.org` begins with a `:PROPERTIES:` drawer at `point-min`
- **AND** the drawer contains `:GPTEL_PRESET: researcher`
- **AND** the drawer contains `:GPTEL_PARENT_SESSION_ID: parent-20260425100000`
- **AND** the drawer is followed directly by the initial `#+begin_user` / `#+end_user` block, with no `* System Prompt` heading and no `* Chat` heading

#### Scenario: Drawer scope and work-root keys written from parent-supplied paths
- **WHEN** an agent is created with `work_root "/path/to/project"`, `read_paths ["/path/to/project/**"]`, and `write_paths ["/path/to/project/**"]`
- **THEN** the session.org `:PROPERTIES:` drawer carries the agent's work root and full scope:
```org
:PROPERTIES:
:GPTEL_PRESET: researcher
:GPTEL_PARENT_SESSION_ID: <parent-id>
:GPTEL_WORK_ROOT: /path/to/project
:GPTEL_SCOPE_READ: /path/to/project/**
:GPTEL_SCOPE_WRITE: /path/to/project/**
:GPTEL_SCOPE_WRITE+: /tmp/**
:GPTEL_SCOPE_DENY: **/.git/**
:GPTEL_SCOPE_DENY+: **/runtime/**
:GPTEL_SCOPE_DENY+: **/.env
:GPTEL_SCOPE_DENY+: **/node_modules/**
:END:
```
- **AND** `/tmp/**` appears in the write scope as auto-appended scratch, not as the sole write target
- **AND** no `scope.yml` (or any other sidecar file) is written in the agent directory

#### Scenario: Agent buffer activates via content-addressed signature
- **WHEN** the agent's `session.org` is opened (whether at agent creation time or by a later `find-file`)
- **THEN** the file's drawer signature selects `gptel-chat-mode` via `magic-mode-alist` and the `gptel-chat-mode-hook` binder runs
- **AND** the buffer is in `gptel-chat-mode`
- **AND** the preset declared in the `:PROPERTIES:` drawer has been applied buffer-local
- **AND** the buffer's `default-directory` is the drawer's `:GPTEL_WORK_ROOT:`
- **AND** the buffer is registered in `jf/gptel--session-registry`
- **AND** `jf/gptel-autosave-enabled` is non-nil

#### Scenario: No sidecar config files written
- **WHEN** creating any agent
- **THEN** the agent directory contains `session.org` plus a sibling `system-prompt.<ext>` (always written for agents — the harness preamble, plus the preset's `:system` when it has one)
- **AND** no `scope.yml`, `metadata.yml`, or `tools.org` is written at any point in the lifecycle

### Requirement: Configuration isolation (zero inheritance)

The system SHALL enforce zero inheritance from the parent session. Agent configuration SHALL come from a single source: the agent's own `session.org` `:PROPERTIES:` drawer — the `:GPTEL_PRESET:` key (applied buffer-local at mode-activation time by `gptel-chat--apply-declared-preset`), the agent's working directory (`:GPTEL_WORK_ROOT:`), and the agent's drawer scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`). The agent SHALL NOT inherit `gptel-backend`, `gptel-model`, `gptel-temperature`, `gptel-tools`, `gptel-system`, `default-directory`, or any scope path from the parent buffer at runtime. The agent's work root and scope are values the parent *passes* at spawn time and that are then frozen in the agent's own drawer — a parent-supplied default is not runtime inheritance. Globals (the user's defaults outside any session) MAY fill gaps when the preset does not declare a key — globals are the user's standing intent, not parent-specific state.

#### Scenario: Backend, model, tools come only from the drawer-declared preset
- **WHEN** the parent session has a non-default `gptel-backend` and `gptel-model` set buffer-local
- **AND** the agent's preset declares only `:backend` and `:model`
- **THEN** the agent buffer's `gptel-backend` and `gptel-model` come from the preset
- **AND** the parent's buffer-local `gptel-backend` / `gptel-model` are not propagated into the agent buffer

#### Scenario: Agent reads its own drawer, never the parent's
- **WHEN** the agent invokes a scope-validated tool
- **THEN** the scope system reads from the agent buffer's `:PROPERTIES:` drawer (or, when called outside the buffer, from `<agent-dir>/session.org`'s drawer)
- **AND** does NOT read from the parent's drawer or any `scope.yml`
- **AND** validates the tool call against the agent's scope only

#### Scenario: Path and work-root configuration never runtime-inherited
- **WHEN** the parent's `session.org` drawer declares `:GPTEL_SCOPE_READ:` with patterns
- **AND** the agent is created without `read_paths`
- **THEN** the agent's drawer omits `:GPTEL_SCOPE_READ:` (empty read paths)
- **AND** the parent's drawer scope keys are NOT copied into the agent's drawer
- **AND** the agent has read access only to the patterns its own drawer declares
- **WHEN** the agent is created without `work_root`
- **THEN** the agent's drawer records its own `:GPTEL_WORK_ROOT:` resolved to the parent's work root at spawn time
- **AND** the agent buffer's `default-directory` is read from that drawer value, not from a live reference to the parent buffer
