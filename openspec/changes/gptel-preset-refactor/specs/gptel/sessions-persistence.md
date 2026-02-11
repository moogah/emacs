# GPTEL Sessions Persistence (Delta Spec)

This delta spec modifies preset loading behavior during session initialization to use the new phase-separated preset configuration architecture.

## MODIFIED Requirements

### Requirement: Session lifecycle - Open

The system SHALL auto-initialize session buffers when opening session.md files via find-file-hook.

Auto-initialization SHALL:
1. Detect files matching pattern `*/branches/*/session.md`
2. Extract session-id and branch-name from path
3. Look up session in registry (or create entry if missing)
4. **Load preset configuration using parse-resolve-apply phases**
5. Set buffer-local variables: `jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`
6. Enable gptel-mode
7. Set `jf/gptel-autosave-enabled` to t

**Change**: Step 4 now uses the new preset configuration architecture (`jf/gptel-preset-parse`, `jf/gptel-preset-resolve`, `jf/gptel-preset-apply`) instead of monolithic `jf/gptel--load-preset-from-file`.

#### Scenario: Opening existing session via find-file
- **WHEN** user opens `~/.gptel/sessions/my-session-20260205/branches/main/session.md`
- **THEN** the auto-initialization hook detects the session file
- **AND** extracts session-id "my-session-20260205" and branch-name "main"
- **AND** **parses preset.md to intermediate plist (strings only)**
- **AND** **resolves backend/model/tools names to gptel objects**
- **AND** **applies resolved configuration to buffer-local variables**
- **AND** sets buffer-local session variables
- **AND** enables gptel-mode
- **AND** enables auto-save

#### Scenario: Opening session with Local Variables
- **WHEN** opening an existing session.md that contains Local Variables footer
- **THEN** the system loads the preset system message
- **AND** applies buffer-local settings from Local Variables
- **AND** does NOT duplicate system message in Local Variables

#### Scenario: Fast guard for non-session files
- **WHEN** opening a file that is not a session.md
- **THEN** the auto-initialization hook SHALL exit early (before expensive checks)
- **AND** NOT impact file-open performance

#### Scenario: Preset parsing error is graceful
- **WHEN** preset.md cannot be parsed (missing file, invalid YAML)
- **THEN** the system logs an error with file path and parse details
- **AND** continues with buffer initialization using default configuration
- **AND** does not crash or leave buffer in invalid state

#### Scenario: Preset resolution error is graceful
- **WHEN** backend name cannot be resolved to struct
- **THEN** the system logs an error with backend name and registry state
- **AND** continues with partial configuration (use default backend)
- **AND** user can still interact with the session buffer
