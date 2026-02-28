## MODIFIED Requirements

### Requirement: Scope profile file format

Scope profiles SHALL include an optional `bash_tools` section defining command categorization and denial rules.

**Format:**
```yaml
paths:
  read: ["/**"]
  write: ["${project_root}/**"]
  deny: ["**/.git/**", "**/runtime/**"]
org_roam_patterns:
  subdirectory: ["gptel/**"]
shell_commands:
  allow: ["ls", "find", "grep"]
bash_tools:                        # ← EXTENDED
  categories:
    read_only:
      commands: ["ls", "cat", "grep", "find", "git log", "git show", "git diff"]
    safe_write:
      commands: ["mkdir", "touch", "echo", "git add", "git commit"]
    dangerous:
      commands: []
  deny:
    - "rm"
    - "sudo"
```

**Note:** This extends the existing scope profile schema documented in `openspec/changes/gptel-preset-upstream-alignment/specs/scope-profiles/spec.md`.

#### Scenario: Profile includes bash_tools section
- **WHEN** a scope profile defines bash command permissions
- **THEN** it includes a `bash_tools` top-level key with `categories` and `deny` subsections

#### Scenario: Bash tools categories structure
- **WHEN** bash_tools is present in a profile
- **THEN** categories subsection includes `read_only`, `safe_write`, and `dangerous` keys
- **AND** each category contains a `commands` array of command names

#### Scenario: Bash tools deny list defined
- **WHEN** bash_tools is present in a profile
- **THEN** deny is an array of command names that are always rejected

#### Scenario: Bash tools section is optional
- **WHEN** a scope profile omits bash_tools
- **THEN** sessions created from that profile have no bash command access (deny by default)

#### Scenario: Bash tools config written to session scope.yml
- **WHEN** creating a session from a profile with bash_tools
- **THEN** the system writes the bash_tools section to the session's scope.yml
- **AND** bash_tools undergoes same YAML parsing and normalization as other scope config

#### Scenario: Variable expansion in bash_tools
- **WHEN** bash_tools config is written to scope.yml
- **THEN** any path variables are resolved (future enhancement; currently not used in bash_tools)

### Requirement: Default bash-enabled profiles

The system SHOULD provide default scope profiles that demonstrate bash tools configuration.

#### Scenario: bash-enabled profile provided
- **WHEN** the configuration is installed
- **THEN** a `bash-enabled.yml` profile SHALL exist demonstrating comprehensive bash command categorization

#### Scenario: restricted profile has no bash
- **WHEN** using the `restricted.yml` profile
- **THEN** bash_tools section is omitted (no bash command access)

#### Scenario: coding profile has bash
- **WHEN** using the `coding.yml` profile
- **THEN** bash_tools section includes common development commands (ls, grep, find, git)

**Related specs:** See `bash-tools/spec.md` for bash tools behavior and validation requirements.
