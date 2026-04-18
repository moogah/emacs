## REMOVED Requirements

### Requirement: Plugin system infrastructure
**Reason**: The plugin system (registry, registration, predicates, plugin-result struct) is redundant with command handlers. Both systems solve the same problem — given a parsed command, produce domain-specific semantic operations. Command handlers already have per-command dispatch, domain declaration, and are called per-simple-command by the recursive engine. The plugin system adds a parallel predicate-gated dispatch mechanism that provides no additional capability.
**Migration**: All domain-specific extraction moves to command handlers. Cloud-auth detection migrates to command handlers for `aws`, `gcloud`, `az`. Remove `jf/bash-semantic-plugins`, `jf/bash-register-plugin`, `jf/bash-plugin-result` struct, and all plugin-related code.

### Requirement: Filesystem plugin implementation
**Reason**: The filesystem plugin conflated grammar-level concerns (redirections, compound walking) with domain-specific concerns (positional argument file operations). Grammar-level extraction moves to the unconditional Layer 0. Positional argument extraction is handled by per-command handlers.
**Migration**: Redirection extraction is handled by grammar extraction layer. Positional argument extraction is handled by command handlers (cat.el, rm.el, cp.el, etc.). The `jf/bash-plugin-filesystem` function, its predicate `jf/bash-plugin-filesystem--has-file-tokens-p`, and its registration via `jf/bash-register-filesystem-plugin` are removed.

### Requirement: Plugin predicate filtering
**Reason**: Predicate filtering was the dispatch mechanism for plugins. With the plugin system removed, there is no need for predicate-based applicability checks. Command handlers use command-name lookup for dispatch, which is simpler and already works correctly.
**Migration**: Domain-specific detection logic moves into command handler implementations. For example, cloud-auth pattern matching moves into the `aws`/`gcloud`/`az` command handlers.

### Requirement: Cloud-auth plugin
**Reason**: Cloud-auth detection migrates from a predicate-gated plugin to command handlers. Handlers for `aws`, `gcloud`, `az` emit `:authentication` and `:network` domain operations. This is simpler and gains per-simple-command dispatch for compounds (fixing the old plugin's compound dispatch bug).
**Migration**: Create command handlers for `aws`, `gcloud`, `az` that produce `:authentication` / `:network` domain ops via the standard handler registry.

## MODIFIED Requirements

### Requirement: Semantic orchestration
The system SHALL orchestrate semantic extraction through a two-layer architecture: grammar extraction (unconditional) and command handler dispatch (per simple command). No plugin system is involved.

#### Scenario: Run grammar extraction unconditionally
- **WHEN** extracting semantics from any parsed command
- **THEN** the orchestrator SHALL run grammar extraction (compound decomposition + redirection extraction) unconditionally before any handler dispatch

#### Scenario: Dispatch simple commands to handlers
- **WHEN** grammar extraction produces simple commands from compound decomposition
- **THEN** the orchestrator SHALL dispatch each simple command to the command handler layer
- **AND** each handler lookup SHALL use the simple command's `:command-name`

#### Scenario: Aggregate all results
- **WHEN** grammar extraction and command handlers complete
- **THEN** the orchestrator SHALL aggregate all operations by domain
- **AND** claimed token IDs SHALL be collected from all sources
