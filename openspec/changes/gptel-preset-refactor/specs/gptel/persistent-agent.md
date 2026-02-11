# GPTEL PersistentAgent Tool (Delta Spec)

This delta spec modifies preset loading during agent initialization to use the new phase-separated preset configuration architecture.

## MODIFIED Requirements

### Requirement: Configuration isolation

The system SHALL enforce zero inheritance - agent configuration comes ONLY from the agent's preset.md file, never from parent session state.

Configuration loading SHALL:
1. Copy preset template to agent directory (file-first approach)
2. **Parse preset YAML frontmatter to intermediate plist (strings only)**
3. Set buffer-local session variables BEFORE applying preset
4. **Resolve backend/model/tools names to gptel objects**
5. **Apply resolved configuration to buffer-local variables (backend, model, tools, system message)**
6. Capture tools from agent buffer context AFTER preset application
7. Use captured tools for gptel-request (not parent's tools)

**Changes**: Steps 2, 4, and 5 now use the new preset configuration architecture (`jf/gptel-preset-parse`, `jf/gptel-preset-resolve`, `jf/gptel-preset-apply`) instead of monolithic `jf/gptel--load-preset-from-file` and `jf/gptel--apply-session-preset`.

#### Scenario: Preset loaded from file, not memory
- **WHEN** initializing an agent buffer
- **THEN** the system **parses agent's preset.md using `jf/gptel-preset-parse`**
- **AND** **resolves configuration using `jf/gptel-preset-resolve`**
- **AND** loads configuration from agent's preset.md YAML frontmatter
- **AND** does NOT use gptel-agent's in-memory agents alist

#### Scenario: Buffer-local session variables set before preset
- **WHEN** initializing an agent buffer
- **THEN** the system sets buffer-local variables in this order:
  1. jf/gptel--session-id (agent's session ID)
  2. jf/gptel--session-dir (agent's directory path)
  3. jf/gptel--branch-name ("main")
  4. jf/gptel--branch-dir (same as session-dir, agents don't branch)
  5. **Apply resolved preset configuration using `jf/gptel-preset-apply`**
- **AND** session variables persist outside preset's dynamic scope

#### Scenario: Tools captured from agent buffer context
- **WHEN** setting up gptel-request for the agent
- **THEN** the system captures gptel-tools INSIDE the agent buffer's with-current-buffer
- **AND** uses the captured tools list for gptel-request
- **AND** does NOT use parent buffer's tools (context matters for buffer-local vars)

#### Scenario: Backend, model, system message from preset YAML
- **WHEN** agent preset.md contains:
```yaml
backend: Claude
model: claude-3-7-sonnet-20250219
temperature: 0.3
```
- **THEN** **the system parses these as strings in intermediate plist**
- **AND** **resolves "Claude" to gptel-backend struct**
- **AND** **resolves model string to symbol**
- **AND** the agent buffer uses those exact settings
- **AND** does NOT inherit parent's backend/model/temperature

#### Scenario: Parent configuration never leaked
- **WHEN** parent session uses different backend, model, tools, or system message
- **THEN** the agent SHALL NOT inherit any of those settings
- **AND** agent configuration comes exclusively from agent's preset.md

#### Scenario: gptel-with-preset isolation
- **WHEN** executing the agent request
- **THEN** the system uses gptel-with-preset to create a clean dynamic scope
- **AND** passes :tools (captured from agent buffer), :use-tools t, :include-tool-results t
- **AND** parent's preset scope does NOT affect agent execution

#### Scenario: Agent preset parsing error is graceful
- **WHEN** agent's preset.md cannot be parsed (missing file, invalid YAML)
- **THEN** the system logs an error with file path and parse details
- **AND** agent creation fails with clear error message
- **AND** no partial agent state is created

#### Scenario: Agent preset resolution error is graceful
- **WHEN** agent's backend name cannot be resolved to struct
- **THEN** the system logs an error with backend name and registry state
- **AND** agent creation fails with clear error message
- **AND** does not silently fall back to parent's backend
