# gptel/persistent-agent (delta)

## ADDED Requirements

### Requirement: Agent identity in the drawer

An agent `session.org` SHALL carry drawer-resident identity exactly like a standalone session: `:GPTEL_SESSION_ID:` and `:GPTEL_BRANCH:` (in addition to the agent-distinguishing `:GPTEL_PARENT_SESSION_ID:`). These keys SHALL be emitted by agent creation (`jf/gptel-persistent-agent--task`) into the drawer it already writes, so that an agent's identity, like a branch's, is read from its drawer rather than reverse-engineered from the nested `agents/<agent>/` path layout.

Agent activation SHALL flow through the same content-addressed path as any session: the agent file is written complete (drawer + body) before it is opened, so `find-file-noselect` triggers the `magic-mode-alist` session signature, and the `gptel-chat-mode-hook` binder establishes the agent buffer's state from the drawer. The agent module SHALL NOT depend on the retired global `find-file-hook` auto-init pipeline.

#### Scenario: Agent drawer carries identity keys
- **WHEN** an agent is created under parent `p-abc-20260424000000`
- **THEN** the agent `session.org` drawer contains `:GPTEL_SESSION_ID:`, `:GPTEL_BRANCH:`, and `:GPTEL_PARENT_SESSION_ID: p-abc-20260424000000`

#### Scenario: Agent activates via content-addressed signature
- **WHEN** `jf/gptel-persistent-agent--task` opens the freshly written agent file with `find-file-noselect`
- **THEN** the file's drawer signature selects `gptel-chat-mode` via `magic-mode-alist`
- **AND** the `gptel-chat-mode-hook` binder sets the agent buffer's identity (from the drawer), registers it, and enables autosave
- **AND** no `find-file-hook` entry is involved

## MODIFIED Requirements

### Requirement: Persistence and resumption

The system SHALL rely on the chat-mode session-binding pipeline for persistence: autosave is enabled by the `gptel-chat-mode-hook` binder when the session file is activated, and the buffer's contents (including streamed assistant text and tool blocks) are persisted incrementally through chat-mode's normal save path. The agent SHALL NOT install its own custom auto-save hook.

A saved agent `session.org` SHALL be reloadable as an interactive `gptel-chat-mode` session: opening the file via `find-file` SHALL activate and bind it identically to a standalone session — same major mode (selected by the content-addressed signature), same drawer-declared preset application, same registry registration, same autosave behavior — with the only distinction being the presence of `GPTEL_PARENT_SESSION_ID` in the drawer.

#### Scenario: No agent-specific auto-save hook
- **WHEN** the persistent-agent module is loaded
- **THEN** the module does NOT add `jf/gptel--auto-save-session-buffer` (or any agent-specific save function) to `gptel-post-response-functions`
- **AND** persistence is driven by chat-mode's standard save path

#### Scenario: Saved agent session reloads as interactive
- **WHEN** an agent has completed and its `session.org` is later opened with `find-file`
- **THEN** the content-addressed signature selects `gptel-chat-mode`
- **AND** the drawer-declared preset has been applied buffer-local
- **AND** the user can invoke `gptel-chat-send` from the buffer to continue the conversation interactively
- **AND** behavior is indistinguishable from a standalone chat session except for the presence of `GPTEL_PARENT_SESSION_ID` in the drawer

#### Scenario: Tool blocks persist verbatim
- **WHEN** an agent run includes one or more tool calls
- **THEN** the on-disk `session.org` contains corresponding `#+begin_tool` / `#+end_tool` blocks inside the assistant block
- **AND** those blocks are parsed by `gptel-chat-parse-buffer` on reload as `tool-call` segments
