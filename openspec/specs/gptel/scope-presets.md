# Gptel Scope Presets - Preset File Format Spec

## Purpose

This spec defines the format and structure of preset.md files, which combine scope configuration (YAML frontmatter) with agent instructions (markdown body). Presets serve as both permission policies and system prompts for gptel sessions.

## Requirements

### Requirement: Preset YAML frontmatter structure
Preset files SHALL use YAML frontmatter delimited by --- markers to define scope configuration alongside tool lists and model settings.

#### Scenario: Frontmatter delimited correctly
- **WHEN** a preset.md file contains scope configuration
- **THEN** the YAML content is enclosed between opening --- and closing --- delimiters

#### Scenario: Frontmatter contains paths section
- **WHEN** defining filesystem tool permissions
- **THEN** the frontmatter includes a paths: section with read:, write:, and deny: subsections

#### Scenario: Frontmatter contains org_roam_patterns section
- **WHEN** defining org-roam tool permissions
- **THEN** the frontmatter includes an org_roam_patterns: section with subdirectory:, tags:, and node_ids: subsections

#### Scenario: Frontmatter contains shell_commands section
- **WHEN** defining shell command permissions
- **THEN** the frontmatter includes a shell_commands: section with allow: and deny: subsections

#### Scenario: Frontmatter contains tools list
- **WHEN** defining which tools are available in a preset
- **THEN** the frontmatter includes a tools: array listing tool names

### Requirement: Path patterns use glob syntax
Path patterns in paths.read, paths.write, and paths.deny SHALL use glob syntax with ** for recursive matching and * for single-level matching.

#### Scenario: Double-star matches recursively
- **WHEN** a pattern uses /** or **/
- **THEN** it matches any path depth including subdirectories

#### Scenario: Single-star matches within level
- **WHEN** a pattern uses * without double-star
- **THEN** it matches any characters except directory separators

#### Scenario: Deny patterns block sensitive paths
- **WHEN** a preset defines standard deny patterns
- **THEN** it typically includes **/.git/**, **/runtime/**, **/.env, **/node_modules/**

#### Scenario: Read patterns permissive by default
- **WHEN** a preset allows broad read access
- **THEN** the paths.read section typically includes /** to allow reading any file

#### Scenario: Write patterns restrictive by default
- **WHEN** a preset follows secure defaults
- **THEN** the paths.write section is empty [] or limited to safe directories like /tmp/**

### Requirement: Tool declarations in preset
Presets SHALL declare which tools are available via the tools: array in the YAML frontmatter.

#### Scenario: Tools array lists tool names
- **WHEN** a preset defines available tools
- **THEN** the tools: section contains a YAML array of tool name strings

#### Scenario: Scope-aware tools listed explicitly
- **WHEN** enabling scope-controlled operations
- **THEN** the preset lists tools like read_file, write_file_in_scope, edit_file_in_scope

#### Scenario: Meta tools included for expansion
- **WHEN** a preset supports scope expansion workflow
- **THEN** the tools list includes request_scope_expansion and inspect_scope_plan

#### Scenario: Legacy vs scoped tool names
- **WHEN** a preset has been updated to scope-aware tools
- **THEN** it uses new names (read_file, write_file_in_scope) not legacy names (Read, Write, Edit)

### Requirement: Org-roam pattern format
Org-roam patterns SHALL use structured formats for subdirectory (glob patterns), tags (exact strings), and node_ids (IDs or wildcard).

#### Scenario: Subdirectory patterns use globs
- **WHEN** restricting org-roam node creation to subdirectories
- **THEN** org_roam_patterns.subdirectory contains glob patterns like "gptel/**"

#### Scenario: Tags are exact matches
- **WHEN** allowing specific org-roam tags
- **THEN** org_roam_patterns.tags contains exact tag strings like "gptel"

#### Scenario: Node IDs support wildcard
- **WHEN** allowing linking between any nodes
- **THEN** org_roam_patterns.node_ids contains "*" for unrestricted linking

#### Scenario: Multiple patterns allowed
- **WHEN** defining org-roam permissions
- **THEN** each subsection (subdirectory, tags, node_ids) is an array allowing multiple patterns

### Requirement: Shell command pattern format
Shell command patterns SHALL use command name matching for allowlist and substring matching for denylist.

#### Scenario: Allowlist uses command names
- **WHEN** defining allowed shell commands
- **THEN** shell_commands.allow contains base command names like "ls", "grep", "git"

#### Scenario: Denylist uses substring patterns
- **WHEN** defining dangerous command patterns
- **THEN** shell_commands.deny contains patterns like "rm -rf", "sudo" that match anywhere in the command

#### Scenario: Wildcard allows all commands
- **WHEN** granting unrestricted shell access
- **THEN** shell_commands.allow contains "*"

#### Scenario: Standard denials for safety
- **WHEN** a preset follows secure defaults
- **THEN** shell_commands.deny typically includes "rm -rf", "sudo", "chmod", "chown"

### Requirement: Preset file location convention
Preset files SHALL be located in config/gptel/presets/ directory and referenced by name without extension.

#### Scenario: Presets in standard directory
- **WHEN** defining a new preset
- **THEN** the file is created at config/gptel/presets/<name>.md

#### Scenario: Preset name used without extension
- **WHEN** users select a preset
- **THEN** they reference it by name (e.g., "claude-plan") not full path

#### Scenario: Preset copied to session directory
- **WHEN** a session is initialized with a preset
- **THEN** the preset.md file is copied to the session's branch directory

### Requirement: Preset metadata fields
Presets SHALL include metadata fields for description, backend, model, temperature, and other gptel configuration.

#### Scenario: Description field documents preset purpose
- **WHEN** a preset is defined
- **THEN** it includes a description: field explaining its intended use case

#### Scenario: Backend and model specified
- **WHEN** a preset configures LLM settings
- **THEN** it includes backend: and model: fields for provider and model selection

#### Scenario: Temperature controls randomness
- **WHEN** a preset defines generation behavior
- **THEN** it includes a temperature: field (typically 0.3-1.0)

#### Scenario: Tool confirmation policy
- **WHEN** a preset defines tool execution safety
- **THEN** it may include confirm-tool-calls: with values like "auto", "always", or "never"

### Requirement: Preset format evolution
The preset format SHALL support both legacy template-generated formats and modern hand-crafted formats, with modern presets as authoritative.

#### Scenario: claude-plan.md as reference implementation
- **WHEN** determining canonical preset format
- **THEN** config/gptel/presets/claude-plan.md is considered the reliable reference

#### Scenario: Template functions are legacy
- **WHEN** scope-commands.el contains template functions
- **THEN** these are legacy code; actual preset files are source of truth

#### Scenario: Empty read patterns valid
- **WHEN** a preset restricts all file reading
- **THEN** paths.read can be empty [] requiring explicit expansion for any read

#### Scenario: Empty write patterns secure default
- **WHEN** a preset follows deny-by-default principle
- **THEN** paths.write is empty [] requiring expansion for any write operation

### Requirement: Preset markdown body as instructions
Preset files SHALL include markdown content after the frontmatter that provides system prompt instructions for the agent using that preset.

#### Scenario: Frontmatter followed by markdown
- **WHEN** a preset.md file is structured
- **THEN** YAML frontmatter is followed by markdown documentation of the agent's role and guidelines

#### Scenario: System prompt guidance
- **WHEN** an LLM session loads a preset
- **THEN** the markdown content provides context about the agent's capabilities, responsibilities, and constraints

#### Scenario: Preset body references scope
- **WHEN** preset markdown explains tool usage
- **THEN** it may reference the scope system and expansion workflow to guide LLM behavior

### Requirement: Deny-by-default security model
Presets SHALL follow a deny-by-default security model where operations require explicit permission patterns.

#### Scenario: Empty patterns deny all
- **WHEN** a paths section has empty read: [] or write: [] arrays
- **THEN** no file operations of that type are allowed without expansion

#### Scenario: Deny patterns override allows
- **WHEN** both deny and allow patterns could match a path
- **THEN** deny patterns take precedence regardless of order

#### Scenario: Safe directories in write patterns
- **WHEN** a preset allows limited write access
- **THEN** write patterns typically restrict to safe locations like /tmp/**

#### Scenario: Sensitive paths always denied
- **WHEN** a preset defines deny patterns
- **THEN** it includes .git, runtime, .env, node_modules to protect system files
