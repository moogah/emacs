## MODIFIED Requirements

### Requirement: Agent system-prompt preamble

Every persistent agent SHALL receive a fixed agent-harness preamble as the head of its system prompt, independent of which preset it runs. The preamble SHALL be sourced from a static fragment file (a `prompt-fragments` static fragment) rather than a hard-coded `defconst`, so it is editable and composable as a fragment. The preamble is injected at the persistent-agent layer (not in the shared sibling-file writer that also serves interactive, human-driven sessions), so only agents receive the harness framing; interactive sessions continue to use the preset's role content verbatim.

The agent's effective system prompt SHALL be composed preamble-first: the baseline preamble fragment, followed by the preset's role content when the preset declares non-empty role content. When the preset declares no role content, the sibling file SHALL hold the preamble alone. In all cases the sibling `system-prompt.<ext>` file SHALL be written and the drawer's `:GPTEL_SYSTEM_PROMPT_FILE:` key SHALL be present, so every agent has a non-empty system prompt.

The preamble SHALL establish the agent operating contract: (1) the agent does the task **itself** and SHALL NOT delegate it to another agent (it SHALL NOT call the `PersistentAgent` tool to perform its work), preventing self-delegation loops; (2) the agent runs headless and cannot ask the user follow-up questions, so it makes and states reasonable assumptions; (3) the agent stays within its granted task and scope, requesting scope expansion rather than abandoning the task when a needed operation is refused; (4) the agent terminates with a single, self-contained final message — the only text returned to the parent.

#### Scenario: Preamble composed ahead of a preset's role content
- **WHEN** an agent is created with a preset that declares non-empty role content
- **THEN** the sibling `system-prompt.<ext>` file content is the agent-harness preamble fragment, then a blank-line separator, then the preset's rendered role content
- **AND** the drawer carries `:GPTEL_SYSTEM_PROMPT_FILE:`

#### Scenario: Preamble written alone when the preset has no role content
- **WHEN** an agent is created with a preset that declares no role content
- **THEN** the sibling `system-prompt.<ext>` file content is exactly the agent-harness preamble fragment
- **AND** the drawer still carries `:GPTEL_SYSTEM_PROMPT_FILE:` (every agent has a system prompt)

#### Scenario: Preamble is sourced from a fragment file
- **WHEN** the agent-harness preamble is materialized into an agent's system prompt
- **THEN** its text is drawn from the static agent-preamble fragment file, not from a hard-coded `defconst`

#### Scenario: Preamble forbids self-delegation
- **WHEN** the agent-harness preamble is materialized into an agent's system prompt
- **THEN** it instructs the agent to perform the task itself and to NOT call the `PersistentAgent` tool to do its work
- **AND** it instructs the agent to return a single final message as its only output to the parent

#### Scenario: Interactive sessions do not receive the preamble
- **WHEN** an interactive (non-agent) session is created from the same preset
- **THEN** its sibling `system-prompt.<ext>` (if any) holds the preset's rendered role content, with no agent-harness preamble prepended
