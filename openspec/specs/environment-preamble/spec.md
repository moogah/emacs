## ADDED Requirements

### Requirement: Environment block appended to the composed system message

Every send dispatched from a `gptel-chat-mode` buffer SHALL append a dynamic
environment block to the **tail** of the effective system message, after the
session's role content (the sibling system-prompt body, or — for agents — the
baked harness-preamble-plus-role body). The append SHALL occur at the existing
pre-send seam (the `:before` advice on `gptel-request`) so that both interactive
chat buffers and persistent-agent buffers are covered by one mechanism with no
agent-specific code. The append SHALL be unconditional: a buffer with no sibling
system-prompt file still receives the environment block, appended to whatever
system message is otherwise in effect.

#### Scenario: Chat send appends the environment block after the role content
- **WHEN** a send is dispatched from an interactive `gptel-chat-mode` buffer whose role content is `R`
- **THEN** the effective system message is `R` followed by the environment block
- **AND** the environment block is the last section of the system message

#### Scenario: Agent send is covered by the same seam
- **WHEN** a persistent-agent buffer (a `gptel-chat-mode` buffer) dispatches its task send
- **THEN** the environment block is appended to the tail of the agent's composed system message
- **AND** no agent-specific code path is required to produce it

#### Scenario: Buffer with no sibling system-prompt file still gets the block
- **WHEN** a `gptel-chat-mode` buffer has no `:GPTEL_SYSTEM_PROMPT_FILE:` drawer key
- **THEN** the environment block is still appended to the system message in effect for that buffer

### Requirement: Environment block is a dynamic fragment

The environment block SHALL be produced by a dynamic fragment (a
`prompt-fragments` dynamic fragment) — a function evaluated at compose time —
rather than by a hard-coded builder `defun`. Sourcing the block as a dynamic
fragment SHALL change only how the block's text is produced, not its observable
behavior: the block SHALL remain appended at the **tail** of the composed system
message, SHALL be rebuilt wholesale on every send (no accumulation across
sends), and SHALL be sourced from the buffer's own inputs (work root and
`GPTEL_SCOPE_*` drawer keys), exactly as specified by the existing
environment-block requirements.

#### Scenario: Block produced by a dynamic fragment at compose time
- **WHEN** a chat-mode system message is composed for a send
- **THEN** the environment block is produced by evaluating the environment dynamic fragment at that compose time

#### Scenario: Tail placement preserved
- **WHEN** the environment dynamic fragment is included in the composition
- **THEN** the environment block remains the last section of the composed system message

#### Scenario: Wholesale rebuild preserved
- **WHEN** two consecutive sends are dispatched from the same chat buffer
- **THEN** the second send's system message contains exactly one environment block
- **AND** a scope change between the sends is reflected in the second send's block

### Requirement: Environment block content is drawer-sourced and workspace-neutral

The environment block SHALL be built from inputs intrinsic to the buffer only:
the work root (the buffer's `default-directory`, established from
`GPTEL_WORK_ROOT`) and the file-access scope (the `GPTEL_SCOPE_*` drawer keys).
The builder SHALL NOT depend on the workspaces package, `home.org`, or any
external producer at runtime. The block SHALL contain a short prose framing of
what the information is, the work root, a summary of the read/write/deny scope,
and a statement that the information is current as of this turn.

#### Scenario: Block reports the work root
- **WHEN** the buffer's work root (`default-directory`) is `/Users/x/proj/`
- **THEN** the environment block states that the working directory is `/Users/x/proj/`

#### Scenario: Block reports the file-access scope
- **WHEN** the buffer's drawer carries `GPTEL_SCOPE_READ`/`GPTEL_SCOPE_WRITE`/`GPTEL_SCOPE_DENY` patterns
- **THEN** the environment block summarizes the readable, writable, and denied paths from those keys

#### Scenario: Block declares the information is live
- **WHEN** the environment block is produced
- **THEN** it includes a statement that the environment information is current/up to date as of the present turn

#### Scenario: Builder names no workspace symbol
- **WHEN** the workspaces package is absent (not loaded)
- **THEN** the environment block is still produced from the drawer and `default-directory`
- **AND** building it raises no error referencing a workspaces function or variable

### Requirement: Idempotent wholesale rebuild

The effective system message SHALL be recomposed from its sources (the role
content plus a freshly built environment block) on every send, and SHALL NOT be
produced by appending to its own previously stored value. Consequently repeated
sends SHALL NOT accumulate multiple environment blocks, and a change to the
buffer's scope between sends SHALL be reflected in the next send's block.

#### Scenario: Two consecutive sends do not duplicate the block
- **WHEN** two sends are dispatched from the same chat buffer with no intervening change
- **THEN** the system message on the second send contains exactly one environment block

#### Scenario: Scope grown mid-session is reflected on the next send
- **WHEN** the buffer's `GPTEL_SCOPE_*` drawer keys are extended between two sends (e.g. by a scope expansion)
- **THEN** the environment block on the second send reflects the extended scope

### Requirement: Graceful degradation without a scope drawer

A `gptel-chat-mode` buffer with no `GPTEL_SCOPE_*` drawer keys SHALL still receive an environment block reporting its `default-directory` and stating that no scope restrictions apply (a plain chat not bound to a scoped session). Producing the block SHALL NOT signal an error and SHALL NOT emit an empty block in this case.

#### Scenario: Drawerless chat buffer reports cwd and no restrictions
- **WHEN** a `gptel-chat-mode` buffer has no `GPTEL_SCOPE_*` keys
- **THEN** the environment block reports the buffer's `default-directory`
- **AND** states that no scope restrictions apply
- **AND** no error is signaled

### Requirement: Scoped to chat-mode buffers only

The environment-block append SHALL apply only to buffers derived from
`gptel-chat-mode`. A `gptel-request` dispatched from a buffer that is not a
`gptel-chat-mode` buffer SHALL receive no environment block and SHALL incur no
work beyond the mode predicate check.

#### Scenario: Non-chat-mode caller is untouched
- **WHEN** `gptel-request` is dispatched from a buffer that is not a `gptel-chat-mode` buffer
- **THEN** no environment block is added and that caller's system message is unchanged
