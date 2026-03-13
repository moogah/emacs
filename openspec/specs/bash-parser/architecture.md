# Bash Parser Architecture

## Components

### Core Parser (bash-parser-core)
- **Responsibility**: Parse bash commands into structured representation with token inventory
- **Key Functions**: `jf/bash-parse` — entry point producing enhanced parsed command plist
- **Output**: Parsed command structure with `:tokens` list, `:parse-complete` flag, `:command-name`, `:positional-args`, `:flags`, `:redirections`, etc.

### Token System (integrated into parser core)
- **Responsibility**: Define token types, track token inventory, provide token utilities
- **Token Types**: `:command-name`, `:positional-arg`, `:flag`, `:flag-arg`, `:command-substitution`, `:separator`, `:redirection`, `:pipe`, `:operator`

### Orchestrator (bash-parser-orchestrator)
- **Responsibility**: Coordinate the two-layer semantic extraction pipeline
- **Key Functions**:
  - `jf/bash-extract-semantics` — Main entry point, orchestrates both layers
  - `jf/bash--decompose-to-simple-commands` — Layer 0: pure grammar decomposition
  - `jf/bash--claim-tokens-for-results` — Post-hoc token claiming from operations
- **Architecture**: Two-layer pipeline (grammar decomposition → command handlers → merge)
- **State**: Stateless — all context passed via parameters

### File Operations Extraction (bash-parser-file-ops)
- **Responsibility**: Provide extraction primitives for redirections and path resolution
- **Key Functions**:
  - `jf/bash-extract-operations-from-redirections` — Extract file operations from shell redirections
  - `jf/bash--resolve-path-variables` — Resolve variable references in file paths
  - `jf/bash-extract-file-operations` — DEPRECATED legacy wrapper
- **Consumed by**: Orchestrator Layer 0 (redirection extraction per simple command)

### Command Handler Registry (bash-parser-semantics)
- **Responsibility**: Register per-command handlers, dispatch extraction, collect results by domain
- **Key Functions**:
  - `jf/bash-register-command-handler` — Register handler for command + domain
  - `jf/bash-lookup-command-handlers` — Look up handlers by command name
  - `jf/bash-extract-command-semantics` — Execute all handlers for a parsed command
- **Data Structure**: Hash table `{command-name => {domain => [handler-fn ...]}}`
- **Consumed by**: Orchestrator Layer 1 (per-simple-command handler dispatch)

### Command Handlers (bash-parser/commands/)
- **Responsibility**: Per-command semantic extraction, one file per command or command family
- **Organization**: Auto-discovered from `commands/` directory via `commands/index.el`
- **Examples**: `cat.el`, `rm.el`, `aws.el` (multi-domain), `gcloud.el`, `az.el`
- **Registration**: Each file calls `jf/bash-register-command-handler` on load

### Coverage System (bash-parser-coverage)
- **Responsibility**: Calculate semantic coverage from tokens and claimed IDs
- **Key Functions**:
  - `jf/bash-calculate-coverage` — Compute coverage metrics
  - `jf/bash-visualize-coverage` — Human-readable coverage display
- **Coverage Metrics**: Total tokens, claimed tokens, coverage ratio, unclaimed tokens, coverage by type

### Security Validation (bash-parser-security)
- **Responsibility**: Validate commands against sandbox rules, dangerous pattern detection
- **Key Functions**: `jf/bash-check-security` — Validate command against sandbox rules
- **Independent**: Does not participate in the extraction pipeline

## Interfaces

### Main API
```elisp
;; Unified semantic extraction (primary API)
(jf/bash-extract-semantics parsed-command &optional var-context)
  => (:domains ((:filesystem . ops) (:authentication . ops) (:network . ops) ...)
      :coverage (:total-tokens N :claimed-tokens M :coverage-ratio R ...)
      :parse-complete t/nil)

;; Deprecated (backward compatible wrapper)
(jf/bash-extract-file-operations parsed-command &optional var-context)
  => (list of file operation plists)
```

### Command Handler Interface
```elisp
;; Handler registration
(jf/bash-register-command-handler
 :command "cat" :domain :filesystem :handler #'handler-fn)

;; Handler function signature
(defun handler-fn (parsed-command)
  "Returns plist with :domain, :operations, :claimed-token-ids, :metadata"
  ...)

;; Handler lookup
(jf/bash-lookup-command-handlers "aws")
  => hash-table {domain => [handler-fn ...]}

;; Handler dispatch (called by orchestrator)
(jf/bash-extract-command-semantics parsed-command)
  => (:domains ((domain . ops) ...) :claimed-token-ids (...))
```

### Token Inventory Structure
```elisp
;; Enhanced parsed command
(:command-name "aws-vault"
 :positional-args (...)
 :flags (...)
 :tokens [(:id 0 :type :command-name :value "aws-vault" :start 0 :end 9)
          (:id 1 :type :positional-arg :value "exec" :start 10 :end 14)
          ...]
 :parse-complete t
 :parse-errors nil)
```

### Data Flow
```
Bash String
    |
jf/bash-parse (enhanced with tokens)
    |
Parsed Command (with :tokens, :parse-complete)
    |
jf/bash-extract-semantics
    |
+-- Layer 0: Grammar Decomposition (unconditional) ----------------+
|                                                                    |
|  Compound?  --yes--> Recursive decomposition                      |
|      |               (pipelines, chains, loops, conditionals,      |
|      no               subshells, substitutions, nested commands)   |
|      |                      |                                      |
|      v                      v                                      |
|  Simple command        Simple commands (with accumulated context)  |
|      |                      |                                      |
|      +----------------------+                                      |
|      v                                                             |
|  For each simple command:                                          |
|    1. Extract redirections --> :filesystem ops (:source :redirection)|
|    2. Dispatch to command handlers (Layer 1) --+                   |
|                                                 |                   |
+-- Layer 1: Command Handlers --------------------+-----------------+
|                                                 v                  |
|  jf/bash-extract-command-semantics(simple-cmd)                     |
|    --> handler results by domain                                   |
|    (:filesystem, :authentication, :network, etc.)                  |
|                                                                    |
+--------------------------------------------------------------------+
                         |
+-- Merge ---------------+------------------------------------------+
|                        v                                           |
|  Combine: Layer 0 redirection ops + Layer 1 handler ops            |
|  Group by domain, both layers contribute to same domain            |
|  Claim tokens post-hoc from all operations                         |
|  Calculate coverage                                                |
|  Produce final (:domains :coverage :parse-complete)                |
|                                                                    |
+--------------------------------------------------------------------+
```

## Boundaries

### In Scope
- Token inventory tracking in parser
- Parse completeness flag
- Two-layer orchestration (grammar decomposition + command handlers)
- Post-hoc token claiming from operations
- Coverage calculation and reporting
- Command handler auto-discovery and registration
- Cloud authentication detection via command handlers (aws, gcloud, az)
- Security validation (sandbox rules, dangerous patterns)

### Out of Scope (Future Extensions)
- Database connection extraction (psql, mysql, mongo)
- Container operation extraction (docker, podman)
- Language-specific code parsing (python -c, node -e) — not bash injection

### Internal vs External
- **Internal**: Decomposition engine, token claiming algorithm, merge logic
- **External (stable)**: `jf/bash-extract-semantics` return structure, `jf/bash-register-command-handler` API, handler function signature

### Integration Points
- **Parser Core**: Produces parsed commands with token inventory
- **gptel Scope System**: Consumer of `jf/bash-extract-semantics` return value
- **File Operations Extraction**: Provides redirection extraction primitives to Layer 0

## Testing Approach

### Test Framework
**Buttercup** — BDD framework with `describe`/`it`/`expect` syntax. Preferred for all new tests per project convention.

### Test Organization
**Location**: `config/bash-parser/test/`

- `unit/core/` — Grammar extraction, redirection extraction, semantic pipeline
- `unit/semantic/` — Handler registry, command semantics
- `unit/analysis/` — Coverage calculation
- `integration/` — Orchestrator end-to-end, handler merge, error handling
- `behavioral/` — User-facing scenarios from specs
- `construct/` — Bash construct-specific tests
- `corpus/` — Corpus-driven tests

### Naming Conventions
- **Files**: `*-spec.el` suffix (Buttercup)
- **Describes**: `(describe "Grammar extraction"` — component-focused
- **Its**: `(it "decomposes pipeline into simple commands"` — behavior-focused

### Running Tests
```bash
./bin/run-tests.sh -d config/bash-parser              # All
./bin/run-tests.sh -d config/bash-parser/test/unit     # Unit tests
./bin/run-tests.sh -d config/bash-parser/test/integration  # Integration
./bin/run-tests.sh -d config/bash-parser --report      # Concise report
```

## Dependencies

### Internal Dependencies
- `bash-parser-core` — Parser with token tracking
- `bash-parser-protocol` — Forward declarations
- `bash-parser-file-ops` — Redirection extraction, path resolution
- `bash-parser-semantics` — Command handler registry and dispatch
- `bash-parser-coverage` — Coverage calculation
- `cl-lib` — Common Lisp extensions

### External Dependencies
- **None** — Architecture is self-contained within bash-parser

## Requirements

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
- **AND** the result SHALL conform to the existing `jf/bash-extract-semantics` return structure

#### Scenario: No plugin system in data flow
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

## Constraints

### Performance
- Token overhead: Minimal (~10% memory per parsed command)
- Handler dispatch: O(1) hash lookup per command name
- Decomposition: Linear in compound structure depth (capped at max-depth)

### Compatibility
- `jf/bash-extract-semantics` return structure is the public API contract
- `jf/bash-extract-file-operations` preserved as deprecated wrapper
- Parser output structure extended but backward compatible

### Technical
- Emacs Lisp: No true multithreading — handlers execute sequentially per command
- Token claiming is post-hoc (from operations), not inline during extraction
