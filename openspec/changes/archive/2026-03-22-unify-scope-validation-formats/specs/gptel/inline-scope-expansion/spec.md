## MODIFIED Requirements

### Requirement: Inline expansion preserves violation context

The inline expansion UI SHALL consume violation-info produced from the unified validator format. The UI reads `:tool`, `:resource`, `:reason`, `:validation-type`, and `:metadata` from violation-info — all populated by `build-violation-info` from the unified `:error` + `:message` validator output.

#### Scenario: Inline expansion preserves violation context
- **WHEN** validation fails and triggers inline expansion UI
- **THEN** violation info includes tool name, resource, reason (human-readable), and validation type
- **AND** transient menu displays full context to user
- **AND** context is sufficient for user to make informed decision

#### Scenario: Expansion UI receives human-readable reason from all validator types
- **WHEN** path, pattern, or bash validator triggers expansion UI
- **THEN** the `:reason` field in violation-info contains human-readable text (from validator's `:message` field)
- **AND** the UI never displays machine codes like "denied-pattern" or "command-not-allowed" as the reason
