# PersistentAgent Tool

## Purpose

Enables autonomous sub-agents with isolated configuration, full conversation history, and parent-child relationships. Agents execute asynchronously by composing chat-mode's public programmatic-send API in their own buffers while providing non-intrusive progress feedback via overlays in the parent buffer. An agent session is, by construction, a standard `gptel-chat-mode` session — the only distinction is a `GPTEL_PARENT_SESSION_ID` link in its session-file `:PROPERTIES:` drawer.

## Key Concepts

### Configuration Isolation (Zero Inheritance)

**CRITICAL PRINCIPLE**: Agents have ZERO inheritance from parent sessions.

Agent configuration comes ONLY from the agent's `session.org` `:PROPERTIES:` drawer, populated at agent creation time and read at auto-init time:
1. **Drawer-declared preset** (`:GPTEL_PRESET:`) supplies backend, model, and tools. The agent's system message is the baseline agent-harness preamble followed by the preset's `:system` body (see *Agent System-Prompt Preamble* below), materialized in the sibling `system-prompt.<ext>` file.
2. **Drawer-declared scope keys** (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`) supply file-access permissions, populated from the `allowed_paths` parameter plus the codebase's fixed write/deny patterns.

Agents do NOT inherit:
- Parent's drawer scope keys
- Parent's backend/model/temperature
- Parent's tools or system message

**Path configuration**:
- `allowed_paths` provided → agent uses exactly those patterns
- `allowed_paths` nil/empty → agent's drawer omits `:GPTEL_SCOPE_READ:` (no read permissions)
- **Never** inherits parent's allowed paths

The `denied_paths` parameter advertised by earlier implementations no longer exists on the tool surface; deny patterns are fixed (default deny list) and not configurable per agent.

### Agent Session Structure

```
<parent-branch-dir>/agents/<preset>-<timestamp>-<slug>/
├── session.org          # Agent conversation + :PROPERTIES: drawer
│                        # (preset, parent id, scope read/write/deny keys,
│                        #  :GPTEL_SYSTEM_PROMPT_FILE: — always present for agents)
└── system-prompt.md     # Sibling system-prompt file (always written for
                         # agents: the agent-harness preamble, plus the
                         # preset's :system body when it declares one)
```

Agents do NOT have:
- `branches/` subdirectory (no branching support; single-timeline)
- `current` symlink (no branch tracking)
- Sidecar config files (`metadata.yml`, `scope.yml`, `tools.org`) — all agent
  configuration and metadata lives in the session-file `:PROPERTIES:`
  drawer; the conversation (including tool-call/result blocks) lives
  directly under the drawer as `#+begin_user` / `#+begin_assistant`
  turn blocks (no `* Chat` heading). The system prompt is the one
  exception to "session content lives in `session.org`": it is carried
  in a sibling `system-prompt.<ext>` file referenced by the drawer's
  `:GPTEL_SYSTEM_PROMPT_FILE:` key (see `gptel/chat-mode` Requirement:
  System prompt sibling file is authoritative).

The agent's `session.org` follows the canonical chat-mode session
layout (drawer + bare turn blocks, no headings; system prompt in a
sibling file):

```org
:PROPERTIES:
:GPTEL_PRESET: <preset-name>
:GPTEL_PARENT_SESSION_ID: <parent-session-id>
:GPTEL_SCOPE_READ: <pattern> <pattern> ...    # omitted if no read paths
:GPTEL_SCOPE_WRITE: /tmp/**
:GPTEL_SCOPE_DENY: **/.git/** **/runtime/** **/.env **/node_modules/**
:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md   # always present for agents
:END:

#+begin_user
<prompt>
#+end_user
```

with the agent's system prompt living in the sibling
`system-prompt.md` next to `session.org`. Unlike interactive sessions
— whose sibling file is the preset's `:system` verbatim, or absent
when the preset has none — an agent's sibling file is **always**
written: the baseline agent-harness preamble, followed by the preset's
`:system` body when it declares one. Every agent therefore has a
non-empty system prompt and a `:GPTEL_SYSTEM_PROMPT_FILE:` drawer key.

### Execution Lifecycle

1. **Validation**: Parent-session check + preset existence — raise user-error before any side effect on validation failure.
2. **Creation**: Build agent directory under `<parent-branch>/agents/`, write `session.org` carrying the canonical layout — `:PROPERTIES:` drawer (preset, parent session id, `:GPTEL_SCOPE_READ:` / `:GPTEL_SCOPE_WRITE:` / `:GPTEL_SCOPE_DENY:` keys, plus `:GPTEL_SYSTEM_PROMPT_FILE:`, always present for agents) followed directly by the initial `#+begin_user` block (no `* System Prompt` or `* Chat` headings). Always write the sibling `system-prompt.<ext>` file holding the agent-harness preamble followed by the preset's `:system` body when it declares one (see *Agent System-Prompt Preamble*). No sidecar config files (`metadata.yml`, `scope.yml`, `tools.org`) are written.
3. **Initialization**: Open the agent file with `find-file-noselect`; the codebase's `find-file-hook`-driven auto-init pipeline activates `gptel-chat-mode`, applies the drawer-declared preset buffer-local, registers the buffer in `jf/gptel--session-registry`, and enables autosave.
4. **Execution**: Compose chat-mode's public API (`gptel-chat-parse-buffer`, `gptel-chat-turns-to-messages`, `gptel-chat-open-assistant-block`, `gptel-chat-stream-callback`, `gptel-chat-fsm-handlers`) to drive the request. The agent supplies its own FSM-handler-chained overlay updates and a parent-feedback overlay as `gptel-request`'s `:context`.
5. **FSM States**: `WAIT` (overlay: "Waiting…"), `TOOL` (overlay: "Calling Tools… (+N)" with cumulative count).
6. **Completion**: On `DONE`, read the last `#+begin_assistant` block from the agent buffer, extract the trailing text segment (skipping tool-call segments), delete the parent overlay, invoke the caller-supplied `main-cb` with that text. On `ERRS`/`ABRT`, also delete the overlay and invoke `main-cb` with an error/abort string.
7. **Persistence**: Chat-mode's normal save path persists the buffer incrementally (no agent-owned auto-save hook). Saved `session.org` files re-open as standard interactive `gptel-chat-mode` sessions.

### Parent-Child Communication

**Overlay system**:
- Created at the parent's response-tracking marker (falling back to `:position` if no tracking marker exists yet) so progress feedback appears where the parent's response is being inserted.
- Shows task description, preset name, progress status.
- Updates during `WAIT` and `TOOL` states via FSM handlers chained alongside chat-mode's lifecycle handlers and gptel's upstream state-driving handlers.
- Displays cumulative tool count with formatted calls.
- Deleted on every terminal FSM state — `DONE`, `ERRS`, `ABRT` — without exception.

**Result delivery (final text only)**:
- The agent does NOT maintain a string accumulator parallel to the buffer.
- At terminal state, the result returned to the parent is derived from the agent buffer's last `#+begin_assistant` block — specifically its final text segment after any trailing tool-call segments.

## Requirements

### Requirement: Tool invocation and validation

PersistentAgent SHALL only operate within persistent session buffers and SHALL accept exactly four parameters: `preset`, `description`, `prompt`, and `allowed_paths`. The previously advertised `denied_paths` parameter SHALL NOT exist on the tool surface.

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
- **THEN** the argument list contains exactly `preset`, `description`, `prompt`, and `allowed_paths`
- **AND** does NOT contain `denied_paths`

#### Scenario: Explicit path configuration (zero inheritance)
- **WHEN** `allowed_paths` is omitted, `nil`, or an empty array
- **THEN** the agent's `session.org` `:PROPERTIES:` drawer omits `:GPTEL_SCOPE_READ:` (the validator loads `:read nil`, i.e. no read permissions)
- **AND** the parent's drawer scope keys are NOT copied into the agent's drawer
- **WHEN** `allowed_paths` is provided as a non-empty array of glob patterns
- **THEN** the agent's drawer declares `:GPTEL_SCOPE_READ:` containing exactly the supplied patterns
- **AND** the parent's drawer scope keys are NOT merged in

### Requirement: Agent session creation

The system SHALL create agent sessions as standard `gptel-chat-mode` sessions, sharing the same drawer-driven configuration and auto-init pipeline as standalone interactive sessions. The agent's `session.org` SHALL be written with a `:PROPERTIES:` drawer at `point-min` declaring the agent's preset (`:GPTEL_PRESET:`), its parent link (`:GPTEL_PARENT_SESSION_ID:`), the agent's scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`), and `:GPTEL_SYSTEM_PROMPT_FILE:` (always present for agents). The drawer SHALL be followed directly by the initial `#+begin_user` / `#+end_user` block, with no `* System Prompt` heading and no `* Chat` heading. A sibling `system-prompt.<ext>` file SHALL always be written next to `session.org` holding the agent-harness preamble followed by the preset's `:system` body when it declares one (see *Requirement: Agent system-prompt preamble* and `gptel/chat-mode` Requirement: System prompt sibling file is authoritative). NO sidecar config files (`scope.yml`, `metadata.yml`, `tools.org`) are written — all configuration and metadata lives in the session-file drawer; the conversation including tool blocks lives directly under the drawer as turn blocks.

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
- **AND** no `scope.yml` (or any other sidecar file) is written in the agent directory

#### Scenario: Agent buffer auto-initializes via find-file-hook
- **WHEN** the agent's `session.org` is opened (whether at agent creation time or by a later `find-file`)
- **THEN** `jf/gptel--auto-init-session-buffer` recognizes the agent path layout and runs
- **AND** the buffer is in `gptel-chat-mode`
- **AND** the preset declared in the `:PROPERTIES:` drawer has been applied buffer-local
- **AND** the buffer is registered in `jf/gptel--session-registry`
- **AND** `jf/gptel-autosave-enabled` is non-nil

#### Scenario: No sidecar config files written
- **WHEN** creating any agent
- **THEN** the agent directory contains `session.org` plus a sibling `system-prompt.<ext>` (always written for agents — the harness preamble, plus the preset's `:system` when it has one)
- **AND** no `scope.yml`, `metadata.yml`, or `tools.org` is written at any point in the lifecycle

### Requirement: Agent system-prompt preamble

Every persistent agent SHALL receive a fixed agent-harness preamble as the head of its system prompt, independent of which preset it runs. The preamble is injected at the persistent-agent layer (not in the shared sibling-file writer that also serves interactive, human-driven sessions), so only agents receive the harness framing; interactive sessions continue to use the preset's `:system` verbatim.

The agent's effective system prompt SHALL be composed preamble-first: the baseline preamble, followed by the preset's `:system` body when the preset declares a non-empty one. When the preset declares no `:system`, the sibling file SHALL hold the preamble alone. In all cases the sibling `system-prompt.<ext>` file SHALL be written and the drawer's `:GPTEL_SYSTEM_PROMPT_FILE:` key SHALL be present, so every agent has a non-empty system prompt.

The preamble SHALL establish the agent operating contract: (1) the agent does the task **itself** and SHALL NOT delegate it to another agent (it SHALL NOT call the `PersistentAgent` tool to perform its work), preventing self-delegation loops; (2) the agent runs headless and cannot ask the user follow-up questions, so it makes and states reasonable assumptions; (3) the agent stays within its granted task and scope, requesting scope expansion rather than abandoning the task when a needed operation is refused; (4) the agent terminates with a single, self-contained final message — the only text returned to the parent.

#### Scenario: Preamble composed ahead of a preset's :system
- **WHEN** an agent is created with a preset that declares a non-empty `:system`
- **THEN** the sibling `system-prompt.<ext>` file content is the agent-harness preamble, then a blank-line separator, then the preset's `:system` body verbatim
- **AND** the drawer carries `:GPTEL_SYSTEM_PROMPT_FILE:`

#### Scenario: Preamble written alone when the preset has no :system
- **WHEN** an agent is created with a preset that declares no `:system`
- **THEN** the sibling `system-prompt.<ext>` file content is exactly the agent-harness preamble
- **AND** the drawer still carries `:GPTEL_SYSTEM_PROMPT_FILE:` (every agent has a system prompt)

#### Scenario: Preamble forbids self-delegation
- **WHEN** the agent-harness preamble is materialized into an agent's system prompt
- **THEN** it instructs the agent to perform the task itself and to NOT call the `PersistentAgent` tool to do its work
- **AND** it instructs the agent to return a single final message as its only output to the parent

#### Scenario: Interactive sessions do not receive the preamble
- **WHEN** an interactive (non-agent) session is created from the same preset
- **THEN** its sibling `system-prompt.<ext>` (if any) holds the preset's `:system` verbatim, with no agent-harness preamble prepended

### Requirement: Configuration isolation (zero inheritance)

The system SHALL enforce zero inheritance from the parent session. Agent configuration SHALL come from a single source: the agent's own `session.org` `:PROPERTIES:` drawer — the `:GPTEL_PRESET:` key (applied buffer-local at auto-init time by `gptel-chat--apply-declared-preset`) plus the agent's drawer scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`). The agent SHALL NOT inherit `gptel-backend`, `gptel-model`, `gptel-temperature`, `gptel-tools`, `gptel-system`, or any scope path from the parent buffer at runtime. Globals (the user's defaults outside any session) MAY fill gaps when the preset does not declare a key — globals are the user's standing intent, not parent-specific state.

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

#### Scenario: Path configuration never inherited
- **WHEN** the parent's `session.org` drawer declares `:GPTEL_SCOPE_READ:` with patterns
- **AND** the agent is created without `allowed_paths`
- **THEN** the agent's drawer omits `:GPTEL_SCOPE_READ:` (empty read paths)
- **AND** the parent's drawer scope keys are NOT copied into the agent's drawer
- **AND** the agent has read access only to the patterns its own drawer declares

### Requirement: Execution lifecycle

The system SHALL drive the agent's request through gptel-chat-mode's public programmatic-send API (see `gptel/chat-mode` spec, "Public programmatic-send API" requirement), composing custom FSM handlers with chat-mode's lifecycle handlers and gptel's upstream state-driving handlers.

The agent SHALL NOT supply a custom `:callback` to `gptel-request` — the chat-mode public stream callback handles token insertion into the open `#+begin_assistant` block. The agent's overlay updates SHALL be driven by FSM handlers chained on `WAIT` and `TOOL` states, with the parent-feedback overlay passed through `gptel-request`'s `:context` keyword.

The execution flow:
1. Insert the supplied prompt into the agent buffer's empty `#+begin_user` block.
2. Resolve the user turn and convert turns to messages using the public chat-mode API.
3. Open a fresh `#+begin_assistant` block using the public chat-mode API.
4. Build an FSM whose handler alist composes (in order): the agent's overlay-update handler, chat-mode's lifecycle handler, and gptel's upstream state-driving handler — for `WAIT` and `TOOL`. For `DONE`, `ERRS`, and `ABRT`, the agent's completion handler chains alongside chat-mode's lifecycle handler and gptel's `gptel--handle-post`.
5. Issue `gptel-request` with the messages list, the public chat-mode stream callback, the FSM, and the overlay as `:context`.
6. The FSM completion handlers (`DONE`/`ERRS`/`ABRT`) return control to the parent (see "Parent-child communication" requirement).

#### Scenario: Prompt populates the user block before send
- **WHEN** an agent is launched with a non-empty `prompt` argument
- **THEN** the prompt text is inserted into the agent buffer's empty `#+begin_user` block (the one created by the session template)
- **AND** the buffer is parsed and converted to a messages list via the public chat-mode API
- **AND** a fresh `#+begin_assistant` block is opened via the public chat-mode API

#### Scenario: gptel-request invocation shape
- **WHEN** the agent issues its `gptel-request`
- **THEN** the request is called with a messages list (not `nil`, not a buffer-text string)
- **AND** the request supplies the public chat-mode stream callback as `:callback`
- **AND** the request supplies a `gptel-make-fsm` instance with composed handlers as `:fsm`
- **AND** the request supplies the parent overlay as `:context`

#### Scenario: WAIT state updates the parent overlay
- **WHEN** the agent's FSM enters the `WAIT` state
- **THEN** the agent's overlay-update handler updates the parent overlay's `after-string` to a "Waiting…" indicator
- **AND** chat-mode's `WAIT` lifecycle handler also fires, setting the agent buffer's lifecycle state
- **AND** gptel's upstream `gptel--handle-wait` fires, driving the network request

#### Scenario: TOOL state updates the parent overlay with cumulative count
- **WHEN** the agent's FSM enters the `TOOL` state with N pending tool calls
- **THEN** the agent's overlay-update handler updates the parent overlay's `after-string` to a "Calling Tools…" indicator with cumulative count `(+N)`
- **AND** the formatted tool calls are rendered in the overlay
- **AND** chat-mode's lifecycle handler and gptel's upstream tool-use handler also fire

### Requirement: Parent-child communication

The system SHALL signal completion to the parent by reading the agent buffer's last `#+begin_assistant` block and extracting only its trailing text segment (skipping any tool-call segments) — "final text only," not an accumulated stream.

The overlay in the parent buffer SHALL be created at the parent's response-tracking marker (or `:position` if no tracking marker exists yet) so that progress feedback appears where the parent's response is being inserted. The overlay SHALL be deleted on every terminal FSM state — `DONE`, `ERRS`, `ABRT` — without exception.

#### Scenario: DONE returns the final assistant text segment
- **WHEN** the agent's FSM transitions to `DONE`
- **THEN** the completion handler reads the last `#+begin_assistant` block from the agent buffer
- **AND** extracts the final text segment (the segment after any trailing tool-call segments)
- **AND** invokes the caller-supplied `main-cb` with that text
- **AND** deletes the parent overlay

#### Scenario: ERRS returns an error message
- **WHEN** the agent's FSM transitions to `ERRS`
- **THEN** the completion handler invokes `main-cb` with a string describing the error
- **AND** deletes the parent overlay

#### Scenario: ABRT returns an abort message
- **WHEN** the agent's FSM transitions to `ABRT` (user invoked `gptel-abort` or denied a tool confirmation)
- **THEN** the completion handler invokes `main-cb` with a string indicating the abort
- **AND** deletes the parent overlay

#### Scenario: No string accumulator
- **WHEN** the agent emits multiple streaming response chunks across multiple turns
- **THEN** the agent code does NOT accumulate those chunks into a parallel string buffer
- **AND** the result returned to the parent is derived from the agent buffer at terminal state, not from an in-flight accumulator

### Requirement: Persistence and resumption

The system SHALL rely on the chat-mode session-init pipeline for persistence: autosave is installed by auto-init when the session file is opened, and the buffer's contents (including streamed assistant text and tool blocks) are persisted incrementally through chat-mode's normal save path. The agent SHALL NOT install its own custom auto-save hook.

A saved agent `session.org` SHALL be reloadable as an interactive `gptel-chat-mode` session: opening the file via `find-file` SHALL auto-initialize it identically to a standalone session — same major mode, same drawer-declared preset application, same registry registration, same autosave behavior — with the only distinction being the presence of `GPTEL_PARENT_SESSION_ID` in the drawer.

#### Scenario: No agent-specific auto-save hook
- **WHEN** the persistent-agent module is loaded
- **THEN** the module does NOT add `jf/gptel--auto-save-session-buffer` (or any agent-specific save function) to `gptel-post-response-functions`
- **AND** persistence is driven by chat-mode's standard save path

#### Scenario: Saved agent session reloads as interactive
- **WHEN** an agent has completed and its `session.org` is later opened with `find-file`
- **THEN** the buffer is in `gptel-chat-mode`
- **AND** the drawer-declared preset has been applied buffer-local
- **AND** the user can invoke `gptel-chat-send` from the buffer to continue the conversation interactively
- **AND** behavior is indistinguishable from a standalone chat session except for the presence of `GPTEL_PARENT_SESSION_ID` in the drawer

#### Scenario: Tool blocks persist verbatim
- **WHEN** an agent run includes one or more tool calls
- **THEN** the on-disk `session.org` contains corresponding `#+begin_tool` / `#+end_tool` blocks inside the assistant block
- **AND** those blocks are parsed by `gptel-chat-parse-buffer` on reload as `tool-call` segments

### Requirement: Error handling

The system SHALL handle terminal FSM states (`ERRS`, `ABRT`) and creation-time validation failures with consistent overlay cleanup and a single return path to the parent callback. Validation failures (e.g., the parent-session check or an unknown preset name) SHALL signal a user-error before any directory or buffer is created — partial state SHALL NOT be left on disk.

#### Scenario: Network failure cleanup
- **WHEN** the agent's FSM transitions to `ERRS` due to a network error
- **THEN** the parent overlay is deleted
- **AND** `main-cb` is invoked exactly once with a descriptive error string

#### Scenario: User abort cleanup
- **WHEN** the user denies a tool confirmation or invokes `gptel-abort`
- **THEN** the FSM transitions to `ABRT`
- **AND** the parent overlay is deleted
- **AND** `main-cb` is invoked exactly once with an abort message

#### Scenario: Unknown preset rejected before any side effect
- **WHEN** the agent is invoked with a `preset` argument that does not exist in `gptel--known-presets`
- **THEN** a user-error is signaled
- **AND** no agent directory is created
- **AND** no buffer is created

#### Scenario: Overlay never leaks
- **WHEN** any terminal FSM state is reached (`DONE`, `ERRS`, `ABRT`)
- **THEN** the parent overlay has been deleted before `main-cb` is invoked
- **AND** no overlay with property `gptel-persistent-agent` remains in the parent buffer

## Integration Points

### With Sessions Subsystem
- Agent directory created by a private agent-specific helper that mirrors the session-directory layout under `<parent-branch>/agents/`.
- Agent `session.org` is registered via `jf/gptel--register-session` by the auto-init pipeline when the file is opened.
- All metadata (preset, parent session id) lives in the session file's `:PROPERTIES:` drawer — no sidecar `metadata.yml` is written.

### With Scope Subsystem
- Agent scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`) are written into the agent's `session.org` `:PROPERTIES:` drawer at creation time by a private fixed-shape wrapper inside the persistent-agent module — `allowed_paths` populates the `:READ:` key; write/deny are constants from the codebase's defaults. No `scope.yml` is written.
- The scope validator reads from the agent buffer's drawer (or, when called outside the buffer, from `<agent-dir>/session.org`'s drawer) at tool-validation time; it NEVER reads the parent's drawer.
- Agent tools are validated against the agent's drawer only.
- `meta` tool categorization bypasses parent scope checks for the PersistentAgent tool invocation itself.

### With gptel Package
- Drives requests through `gptel-request` using the chat-mode public programmatic-send API; supplies a custom FSM whose handler alist composes the agent's overlay/completion handlers with chat-mode's lifecycle handlers and gptel's upstream state-driving handlers.
- Reads the agent buffer's last `#+begin_assistant` block at terminal state via the public `gptel-chat-parse-buffer` to derive the parent-callback result.
- Uses `gptel-make-tool` to register the `PersistentAgent` tool.

## Critical Invariants

1. **Parent session required**: `jf/gptel--session-dir` must be non-nil in the calling buffer; otherwise `user-error` before any side effect.
2. **Zero inheritance**: Paths ONLY from `allowed_paths` parameter; backend/model/tools/system ONLY from the drawer-declared preset; never from parent.
3. **Drawer-driven init**: Agent configuration is fully recoverable from the on-disk `session.org` `:PROPERTIES:` drawer — opening the file via `find-file` produces the same buffer state as the original creation.
4. **No agent-owned auto-save**: Persistence flows through chat-mode's standard save path; the persistent-agent module installs no save hook of its own.
5. **No string accumulator**: The parent's callback receives text derived from the agent buffer at terminal state, not from an in-flight string accumulator.
6. **Overlay cleanup on all paths**: Parent overlay is deleted on every terminal FSM state (`DONE`, `ERRS`, `ABRT`) before the parent callback is invoked.
7. **Single-timeline agents**: No `branches/` subdirectory, no `current` symlink — agents are flat, single-timeline sessions.
8. **No `denied_paths`**: The tool surface accepts exactly four parameters; per-agent deny configuration is not supported.

## Summary

PersistentAgent provides autonomous sub-agents with:
- **Zero inheritance** (isolated configuration; drawer-declared preset + explicit `allowed_paths`)
- **Chat-mode native** (composes the public programmatic-send API; agent sessions are reloadable as standard interactive chat-mode sessions)
- **Async execution** (non-blocking parent)
- **Progress feedback** (overlays in parent, FSM-handler-driven updates)
- **Final-text result** (parent callback receives text extracted from the agent buffer at terminal state; no string accumulator)
- **Parent-child tracking** (drawer-declared `GPTEL_PARENT_SESSION_ID`)
