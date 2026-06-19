## ADDED Requirements

### Requirement: Emacs prelude sourced from a static fragment

The static Emacs runtime-framing prelude that leads every chat-mode system
message SHALL be sourced from a static fragment file (a `prompt-fragments` static
fragment), rather than a hard-coded `defconst`. The prelude SHALL remain the
**leading** element of the composed chat-mode system message, present even when
the buffer carries no role content, and SHALL continue to frame the model's
runtime (operating inside GNU Emacs via the gptel chat interface, Org markup
preferred, editor/file tools available within session scope). Sourcing it from a
fragment file SHALL change only where the text comes from, not its position or
its presence.

#### Scenario: Prelude leads the composed chat-mode system message
- **WHEN** a chat-mode system message is composed
- **THEN** the Emacs prelude fragment is the leading element, ahead of the role and the environment block

#### Scenario: Prelude present with an empty role
- **WHEN** a chat-mode buffer has no role content
- **THEN** the composed system message still begins with the Emacs prelude fragment

#### Scenario: Prelude text comes from a fragment file
- **WHEN** the chat-mode system message is composed
- **THEN** the prelude text is drawn from the static prelude fragment file, not from a hard-coded `defconst`
