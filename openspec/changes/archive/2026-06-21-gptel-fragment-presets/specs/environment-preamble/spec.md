## ADDED Requirements

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
