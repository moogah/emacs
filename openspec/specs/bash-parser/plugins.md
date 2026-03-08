# Bash Parser Plugin System

## Purpose

Provide extensible plugin architecture for semantic extraction from parsed bash commands, allowing multiple specialized analyzers to claim tokens and extract domain-specific operations.

## Responsibilities

- Maintain plugin registry with priority-based execution order
- Define standard plugin protocol for input/output structure and token claiming
- Orchestrate plugin execution with predicate filtering
- Aggregate plugin results into unified semantic analysis
- Isolate plugin errors to prevent cascading failures
- Support shared token claiming where multiple plugins can claim same tokens

## Key Invariants

- Plugins execute in priority order (higher priority first)
- Plugin registration order is used as tiebreaker for same priority
- Each plugin receives complete parsed command structure with token inventory
- Plugin errors are isolated - one plugin failure doesn't break entire extraction
- Token claiming is shared - multiple plugins can claim the same token
- Claimed tokens are deduplicated when calculating coverage

## Requirements

### Requirement: Plugin registration
The system SHALL provide a registry for semantic extraction plugins with priority-based execution order.

#### Scenario: Register plugin
- **WHEN** registering a plugin with name, priority, extractor function, and predicates
- **THEN** plugin is added to registry and available for orchestration

#### Scenario: Multiple plugins registered
- **WHEN** multiple plugins are registered with different priorities
- **THEN** plugins execute in priority order (higher priority first)

#### Scenario: Registration order fallback
- **WHEN** multiple plugins have same priority
- **THEN** plugins execute in registration order

### Requirement: Plugin protocol
The system SHALL define a standard protocol for plugin implementations including input/output structure and token claiming.

#### Scenario: Plugin receives parsed command
- **WHEN** orchestrator invokes plugin
- **THEN** plugin receives complete parsed command structure with token inventory

#### Scenario: Plugin returns result structure
- **WHEN** plugin completes extraction
- **THEN** plugin returns result with domain, operations, claimed-token-ids, and metadata

#### Scenario: Plugin claims tokens
- **WHEN** plugin understands specific tokens
- **THEN** plugin includes those token IDs in claimed-token-ids list

### Requirement: Plugin predicate filtering
The system SHALL evaluate plugin predicates before invocation to determine applicability.

#### Scenario: Predicate matches
- **WHEN** all plugin predicates return true for parsed command
- **THEN** plugin is invoked

#### Scenario: Predicate fails
- **WHEN** any plugin predicate returns false for parsed command
- **THEN** plugin is skipped

#### Scenario: No predicates
- **WHEN** plugin has empty predicate list
- **THEN** plugin always runs (universal applicability)

### Requirement: Semantic orchestration
The system SHALL orchestrate all applicable plugins and aggregate their results into unified semantic analysis.

#### Scenario: Run applicable plugins
- **WHEN** extracting semantics from parsed command
- **THEN** orchestrator runs all plugins whose predicates match

#### Scenario: Aggregate domain results
- **WHEN** plugins complete extraction
- **THEN** orchestrator aggregates operations by domain

#### Scenario: Collect claimed tokens
- **WHEN** plugins return results
- **THEN** orchestrator collects all claimed token IDs for coverage calculation

### Requirement: Plugin error isolation
The system SHALL isolate plugin errors to prevent single plugin failure from breaking entire extraction.

#### Scenario: Plugin throws error
- **WHEN** plugin raises an error during extraction
- **THEN** orchestrator logs error and continues with remaining plugins

#### Scenario: Partial results on error
- **WHEN** some plugins fail and others succeed
- **THEN** orchestrator returns results from successful plugins

### Requirement: Shared token claiming
The system SHALL allow multiple plugins to claim the same tokens (shared claiming strategy).

#### Scenario: Multiple plugins claim same token
- **WHEN** filesystem and network plugins both claim token ID 5
- **THEN** both claims are recorded and token is considered claimed by both

#### Scenario: Coverage calculation with shared claiming
- **WHEN** calculating coverage with shared claiming
- **THEN** token is counted as claimed if at least one plugin claimed it

### Requirement: Plugin result structure
The system SHALL define plugin result structure with domain, operations, claimed tokens, and metadata.

#### Scenario: Plugin result contains domain
- **WHEN** plugin returns result
- **THEN** result includes :domain keyword identifying semantic domain

#### Scenario: Plugin result contains operations
- **WHEN** plugin returns result
- **THEN** result includes :operations list with domain-specific operation plists

#### Scenario: Plugin result contains claimed tokens
- **WHEN** plugin returns result
- **THEN** result includes :claimed-token-ids list of understood token IDs

#### Scenario: Plugin result contains metadata
- **WHEN** plugin returns result
- **THEN** result includes :metadata plist with domain-specific metadata

### Requirement: Main extraction entry point
The system SHALL provide unified entry point for semantic extraction replacing old file-operations-only API.

#### Scenario: Extract semantics from parsed command
- **WHEN** calling jf/bash-extract-semantics with parsed command
- **THEN** system runs all applicable plugins and returns semantic analysis result

#### Scenario: Result includes parse completeness
- **WHEN** extraction completes
- **THEN** result includes :parse-complete flag from parsed command

#### Scenario: Result includes coverage
- **WHEN** extraction completes
- **THEN** result includes :coverage plist with coverage metrics

#### Scenario: Result includes domain operations
- **WHEN** extraction completes
- **THEN** result includes :domains alist mapping domains to operations

#### Scenario: Result includes plugin results
- **WHEN** extraction completes
- **THEN** result includes :plugin-results list for debugging

## Plugin Protocol Specification

```elisp
;; Registration
(jf/bash-register-plugin
  :name 'plugin-name              ; Symbol identifying plugin
  :priority 100                   ; Higher = runs first
  :extractor #'plugin-function    ; Function accepting parsed-command
  :predicates (list #'pred-fn))   ; Optional predicate functions

;; Plugin function signature
(defun plugin-function (parsed-command)
  "Extract domain-specific semantics from PARSED-COMMAND.
   Returns jf/bash-plugin-result struct or nil."
  ...)

;; Plugin result structure (cl-defstruct)
(make-jf/bash-plugin-result
  :domain :keyword                    ; e.g., :filesystem, :cloud-auth
  :operations (list ...)              ; Domain-specific operation plists
  :claimed-token-ids (list 0 1 5 7)   ; Token IDs understood by this plugin
  :metadata (list ...))               ; Domain-specific metadata
```

## Integration Points

- **Core Parser**: Consumes parsed commands with token inventory
- **Coverage System**: Uses claimed token IDs for coverage calculation
- **Individual Plugins**: Filesystem, cloud-auth, future plugins all conform to protocol
- **gptel Scope System**: Consumer of semantic extraction API

## Example Usage

```elisp
;; Register a plugin
(jf/bash-register-plugin
  :name 'filesystem
  :priority 100
  :extractor #'jf/bash-plugin-filesystem
  :predicates nil)  ; Always applicable

;; Extract semantics
(jf/bash-extract-semantics parsed-command)
;; => (:parse-complete t
;;     :parse-errors nil
;;     :coverage (:total-tokens 10 :claimed-tokens 8 :coverage-ratio 0.8 ...)
;;     :domains ((:filesystem . ((file "/workspace/foo.txt" operation :read ...)))
;;               (:cloud-auth . ((provider :aws scope "dev-account" ...))))
;;     :plugin-results (...))
```
