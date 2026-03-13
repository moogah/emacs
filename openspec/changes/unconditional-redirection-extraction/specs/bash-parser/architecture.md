## MODIFIED Requirements

### Requirement: Data flow architecture
The semantic extraction pipeline SHALL use a two-layer architecture where grammar-level concerns are separated from domain-specific concerns. No plugin system is involved.

#### Scenario: Layer 0 runs unconditionally
- **WHEN** `jf/bash-extract-semantics` is called with any parsed command
- **THEN** Layer 0 (grammar extraction) SHALL execute first
- **AND** Layer 0 SHALL decompose compound structures and extract redirections without any predicate gating

#### Scenario: Layer 1 runs per simple command
- **WHEN** Layer 0 produces simple commands from compound decomposition
- **THEN** Layer 1 (command handlers) SHALL execute for each simple command
- **AND** handler lookup SHALL use each simple command's `:command-name`

#### Scenario: Merge produces final result
- **WHEN** Layer 0 and Layer 1 complete
- **THEN** the merge step SHALL combine grammar-level and handler-level operations by domain
- **AND** the result SHALL conform to the existing `jf/bash-extract-semantics` return structure (minus `:plugin-results`)

#### Scenario: Plugin system removed from data flow
- **WHEN** examining the data flow
- **THEN** there SHALL be no plugin registry (`jf/bash-semantic-plugins`)
- **AND** there SHALL be no plugin registration function (`jf/bash-register-plugin`)
- **AND** there SHALL be no plugin-result struct (`jf/bash-plugin-result`)
- **AND** all domain-specific extraction SHALL go through command handlers

### Requirement: Component boundaries
The semantic extraction system SHALL have clear boundaries between grammar-level and domain-level components.

#### Scenario: Grammar layer has no domain knowledge
- **WHEN** Layer 0 extracts redirections
- **THEN** it SHALL produce `:filesystem` operations because redirections are shell I/O
- **BUT** it SHALL NOT inspect command names to decide whether to extract
- **AND** it SHALL NOT use any command handler registry

#### Scenario: Command handlers have no compound knowledge
- **WHEN** Layer 1 handlers execute
- **THEN** they SHALL receive only simple commands (never compound structures)
- **AND** they SHALL NOT need to handle pipelines, chains, loops, or conditionals

#### Scenario: Command handlers provide all domain-specific extraction
- **WHEN** domain-specific operations are needed (filesystem from positional args, authentication, network)
- **THEN** command handlers SHALL be the sole mechanism for producing them
- **AND** there SHALL be no parallel plugin system for domain extraction
