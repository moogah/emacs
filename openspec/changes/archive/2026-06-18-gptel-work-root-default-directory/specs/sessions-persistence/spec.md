## ADDED Requirements

### Requirement: Work-root drawer key

The session-file `:PROPERTIES:` drawer SHALL carry a `GPTEL_WORK_ROOT` key recording the session's working directory — the project / work root, distinct from the bookkeeping `branch-dir`. `jf/gptel--create-session-core` SHALL write `GPTEL_WORK_ROOT` from the same `project-root` input it expands into the session's `GPTEL_SCOPE_*` keys, so the working directory and the scope boundary derive from a single value and cannot disagree. The value SHALL be stored as an absolute directory path, verbatim. The key SHALL be optional on read: sessions whose drawer predates it, or hand-authored sessions that omit it, SHALL remain valid.

**Implementation**: `config/gptel/sessions/commands.org` (`jf/gptel--create-session-core`).

#### Scenario: Created session records the work root from its project-root input
- **WHEN** a session is created with `project-root` `/Users/x/proj`
- **THEN** the session.org drawer contains `:GPTEL_WORK_ROOT: /Users/x/proj` (absolute)
- **AND** the drawer's `GPTEL_SCOPE_*` write patterns are expanded from the same `/Users/x/proj`

#### Scenario: Work root and scope derive from one input (agreement by construction)
- **WHEN** `jf/gptel--create-session-core` runs with a single `project-root` value
- **THEN** the persisted `GPTEL_WORK_ROOT` equals the root used to expand `${project_root}` in `GPTEL_SCOPE_*`
- **AND** the two are not computed independently

#### Scenario: Drawer without the key remains valid
- **WHEN** a `session.org` has no `:GPTEL_WORK_ROOT:` key (pre-existing or hand-authored)
- **THEN** the file is still recognized and activated as a session
- **AND** no error is raised for the missing key

### Requirement: Default-directory resolution on session activation

When `gptel-chat-mode` activates a session buffer, the `gptel-chat-mode-hook` binder SHALL set the buffer-local `default-directory` from the drawer's `GPTEL_WORK_ROOT`, normalized to an absolute directory (trailing separator via `file-name-as-directory`). When the drawer omits `GPTEL_WORK_ROOT`, the binder SHALL fall back to `jf/gptel--branch-dir` (the buffer file's own directory — the pre-existing behavior), so keyless sessions are unchanged. This establishes the directory against which the model's relative paths resolve, and it SHALL be in effect before any tool call (tool calls execute in this buffer's context).

**Implementation**: `config/gptel/sessions/commands.org` — the binder run from `gptel-chat-mode-hook`, alongside the buffer-local session-identification variables.

#### Scenario: default-directory set from the work-root key
- **WHEN** a `session.org` whose drawer declares `:GPTEL_WORK_ROOT: /Users/x/proj` is activated
- **THEN** the buffer-local `default-directory` is `/Users/x/proj/`

#### Scenario: Keyless session falls back to branch-dir
- **WHEN** an activated `session.org` drawer omits `:GPTEL_WORK_ROOT:`
- **THEN** the buffer-local `default-directory` equals `jf/gptel--branch-dir` (the file's own directory)
- **AND** behavior is identical to before this change

#### Scenario: Work-root value is normalized to a directory
- **WHEN** the drawer's `:GPTEL_WORK_ROOT:` value lacks a trailing separator
- **THEN** the binder sets `default-directory` with a trailing separator (via `file-name-as-directory` and `expand-file-name`)
