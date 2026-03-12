# Scope System - Delta Spec

## Purpose

This delta spec modifies the core scope system to accept and propagate file metadata through the validation pipeline, enabling git-based policies and richer expansion UI.

## MODIFIED Requirements

### Requirement: Path-based validation

The scope system SHALL validate path-based tools against read/write/deny path lists using glob pattern matching, with file metadata provided as context.

**Implementation**: `config/gptel/scope/scope-core.org` - path validator

**Breaking change**: Validation function signatures now require metadata parameter

#### Scenario: Deny patterns have highest priority
- **WHEN** path matches both allow and deny patterns
- **THEN** system denies access (deny takes precedence)

#### Scenario: Read operation matches read patterns
- **WHEN** read tool accesses path matching `paths.read` patterns
- **THEN** system allows the operation

#### Scenario: Write operation matches write patterns
- **WHEN** write tool accesses path matching `paths.write` patterns
- **THEN** system allows the operation

#### Scenario: Path fails to match allowed patterns
- **WHEN** path doesn't match any allowed patterns for operation type
- **THEN** system denies access and returns `allowed_patterns` in error

#### Scenario: Glob patterns support wildcards
- **WHEN** patterns use `**` (any including /), `*` (any except /), or `?` (single char)
- **THEN** system correctly matches paths using converted regex patterns

#### Scenario: Symlinks resolved before matching
- **WHEN** validating path that is or contains symlinks
- **THEN** system resolves symlinks to real paths
- **AND** checks BOTH real-path AND absolute-path for pattern matches
- **AND** allows if either matches (patterns may match either form)

#### Scenario: Validation receives metadata parameter
- **WHEN** path validation is performed
- **THEN** jf/gptel-scope--validate-path-tool is called with signature `(tool-name args category config metadata)`
- **AND** metadata contains file context (git status, existence, type)

#### Scenario: Metadata available during validation
- **WHEN** path validation logic executes
- **THEN** metadata plist is accessible
- **AND** future policies can use `:git-tracked`, `:git-repo`, `:exists`, `:type` fields

## ADDED Requirements

### Requirement: Validation function signatures accept metadata

Core validation functions SHALL accept metadata parameter to enable context-aware validation.

**Breaking change**: All validation function signatures change

#### Scenario: check-tool-permission accepts metadata
- **WHEN** jf/gptel-scope--check-tool-permission is called
- **THEN** signature is `(config tool-name args metadata)`
- **AND** metadata is propagated to specific validators

#### Scenario: validate-path-tool accepts metadata
- **WHEN** jf/gptel-scope--validate-path-tool is called
- **THEN** signature is `(tool-name args category config metadata)`
- **AND** metadata is accessible for validation logic

#### Scenario: validate-pattern-tool accepts metadata
- **WHEN** jf/gptel-scope--validate-pattern-tool is called
- **THEN** signature is `(tool-name args config metadata)`
- **AND** metadata parameter exists (may be nil for pattern tools)

#### Scenario: validate-bash-tool accepts metadata
- **WHEN** jf/gptel-scope--validate-bash-tool is called
- **THEN** signature is `(tool-name args config metadata)`
- **AND** metadata parameter exists (may be nil for bash tools)

### Requirement: Scoped tool macro gathers metadata

The gptel-make-scoped-tool macro SHALL gather file metadata before calling validation for path-based tools.

#### Scenario: Metadata gathered for path-based tools
- **WHEN** scoped tool with path validation is invoked
- **THEN** macro calls metadata gathering function
- **AND** passes metadata to check-tool-permission

#### Scenario: Metadata nil for non-path tools
- **WHEN** scoped tool with pattern or bash validation is invoked
- **THEN** metadata parameter is nil
- **AND** validation proceeds without file metadata

#### Scenario: Metadata gathering uses first argument
- **WHEN** metadata is gathered for path-based tool
- **THEN** first argument (filepath) is used
- **AND** metadata plist is constructed with file context
