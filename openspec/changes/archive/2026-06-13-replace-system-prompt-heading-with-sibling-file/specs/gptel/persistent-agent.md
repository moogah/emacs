## MODIFIED Requirements

### Requirement: Agent session creation

The system SHALL create agent sessions as standard `gptel-chat-mode` sessions, sharing the same drawer-driven configuration and auto-init pipeline as standalone interactive sessions. The agent's `session.org` SHALL be written with a `:PROPERTIES:` drawer at `point-min` declaring the agent's preset (`:GPTEL_PRESET:`), its parent link (`:GPTEL_PARENT_SESSION_ID:`), the agent's scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`), and — when the preset declares a non-empty `:system` — `:GPTEL_SYSTEM_PROMPT_FILE:`. The drawer SHALL be followed directly by the initial `#+begin_user` / `#+end_user` block, with no `* System Prompt` heading and no `* Chat` heading. When the preset declares a non-empty `:system`, a sibling `system-prompt.<ext>` file holding that text SHALL be written next to `session.org` (see `gptel/chat-mode` Requirement: System prompt sibling file is authoritative). NO sidecar config files (`scope.yml`, `metadata.yml`, `tools.org`) are written — all configuration and metadata lives in the session-file drawer; the conversation including tool blocks lives directly under the drawer as turn blocks.

The session file SHALL be opened with `find-file-noselect` so that the codebase's `find-file-hook`-driven auto-init pipeline activates `gptel-chat-mode`, applies the drawer-declared preset buffer-local, registers the buffer in `jf/gptel--session-registry`, and enables autosave.

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

#### Scenario: Agent session with a system prompt writes a sibling file
- **WHEN** an agent is created with a preset whose `:system` text is non-empty
- **THEN** the agent's `session.org` drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** a sibling `system-prompt.md` exists in the agent directory holding the preset's `:system` body verbatim
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`

#### Scenario: Agent session with no system prompt writes no sibling file
- **WHEN** an agent is created with a preset that declares no `:system` (or whose `:system` is empty)
- **THEN** the agent's `session.org` drawer does NOT contain `:GPTEL_SYSTEM_PROMPT_FILE:`
- **AND** no `system-prompt.<ext>` file is created in the agent directory

#### Scenario: Drawer scope keys written with explicit allowed paths
- **WHEN** an agent is created with `allowed_paths ["/path/to/project/**"]`
- **THEN** the session.org `:PROPERTIES:` drawer carries the agent's full scope:
```org
:PROPERTIES:
:GPTEL_PRESET: researcher
:GPTEL_PARENT_SESSION_ID: <parent-id>
:GPTEL_SCOPE_READ: /path/to/project/**
:GPTEL_SCOPE_WRITE: /tmp/**
:GPTEL_SCOPE_DENY: **/.git/**
:GPTEL_SCOPE_DENY+: **/runtime/**
:GPTEL_SCOPE_DENY+: **/.env
:GPTEL_SCOPE_DENY+: **/node_modules/**
:END:
```
- **AND** no `scope.yml` (or any other sidecar config file) is written in the agent directory

#### Scenario: Agent buffer auto-initializes via find-file-hook
- **WHEN** the agent's `session.org` is opened (whether at agent creation time or by a later `find-file`)
- **THEN** `jf/gptel--auto-init-session-buffer` recognizes the agent path layout and runs
- **AND** the buffer is in `gptel-chat-mode`
- **AND** the preset declared in the `:PROPERTIES:` drawer has been applied buffer-local
- **AND** the buffer is registered in `jf/gptel--session-registry`
- **AND** `jf/gptel-autosave-enabled` is non-nil

#### Scenario: No sidecar config files written
- **WHEN** creating any agent
- **THEN** the agent directory contains `session.org` (plus a sibling `system-prompt.<ext>` when the preset declares a non-empty `:system`)
- **AND** no `scope.yml`, `metadata.yml`, or `tools.org` is written at any point in the lifecycle
