## MODIFIED Requirements

### Requirement: Integration with preset registration

Scope keys SHALL be extracted from preset frontmatter during registration and stored in `jf/gptel-preset--scope-defaults`, keyed by preset name symbol, so that `gptel--known-presets` contains no scope data. The extracted keys are `:paths`, `:shell-commands`, `:bash-tools`, and `:scope-profile`. The extraction SHALL NOT include `:org-roam-patterns` (cycle-3 retired pattern-based validation) or any other key not consumed by the validation pipeline.

**Implementation**: `jf/gptel-preset--extract-scope` in `config/gptel/preset-registration.org`. Extracted keys: `:paths`, `:shell-commands`, `:bash-tools`, `:scope-profile`.

#### Scenario: Scope defaults stored by preset name

- **WHEN** registering preset `executor` whose frontmatter contains `scope_profile: coding`
- **THEN** `jf/gptel-preset--scope-defaults` gains entry `(executor . (:scope-profile "coding"))`
- **AND** the plist passed to `gptel-make-preset` contains no scope keys

#### Scenario: Scope defaults consulted during session creation

- **WHEN** a session is created for preset `executor`
- **THEN** `jf/gptel-scope-profile--resolve` looks up `executor` in `jf/gptel-preset--scope-defaults`
- **AND** feeds the result into the resolution priority above

#### Scenario: Removed keys are not extracted

- **WHEN** a preset frontmatter contains legacy scope keys (`:org-roam-patterns`, `:shell-commands`, `:bash-tools`)
- **THEN** those keys are NOT stored in `jf/gptel-preset--scope-defaults`
- **AND** a warning is logged identifying the preset and the ignored keys so the user can clean up the preset definition

### Requirement: Mutable scope.yml in session branches

Each session branch SHALL receive a `scope.yml` at creation time. This file is the single source of truth for enforcement; the profile template and the registered preset are never consulted by validators. The written `scope.yml` SHALL contain only sections the validators consume (`paths`, `cloud`, `security`) — no `org_roam_patterns` or other legacy sections.

**Implementation**: `jf/gptel-scope-profile--write-scope-yml` in `config/gptel/scope-profiles.org`.

#### Scenario: scope.yml created at session creation

- **WHEN** a session is created with preset `executor`
- **THEN** `scope.yml` is written into the branch directory
- **AND** its contents derive from the preset's resolved scope configuration

#### Scenario: scope.yml is mutable

- **WHEN** scope expansion adds a path during a session
- **THEN** the session's `scope.yml` is updated in place
- **AND** the registered preset and the profile template remain unchanged

#### Scenario: scope.yml is the enforcement source

- **WHEN** a tool invocation requires path validation
- **THEN** `scope-validation` reads the session's `scope.yml` (via `scope-yaml`)
- **AND** does NOT read from `gptel--known-presets` or from profile files

#### Scenario: Empty-scope fallback writes only validator-consumed sections

- **WHEN** a preset has no scope configuration and the empty-scope fallback writes `scope.yml`
- **THEN** the emitted YAML contains only `paths`, `cloud`, and `security` (with safe defaults)
- **AND** it does NOT contain an `org_roam_patterns` section
