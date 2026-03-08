# Sessions Persistence (Delta Spec)

This delta spec documents changes to sessions-persistence for org-mode session support.

## MODIFIED Requirements

### Requirement: Session file formats

The system SHALL support both org-mode (`.org`) and markdown (`.md`) session file formats.

**session.org format** (primary for new sessions):
- Format: Org-mode markup
- Content: Conversation history in org format
- Initial: `"* Session\n\n"` (org-mode heading)
- Local Variables: Emacs Local Variables format (`# gptel-model: ...`)
- Conversion: LLM markdown responses auto-converted to org by gptel's `gptel-org-convert-response`

**session.md format** (legacy, backward compatible):
- Format: Markdown (gptel native format)
- Content: Conversation history
- Initial: `"###\n"` (markdown heading)
- Local Variables: HTML comments (`<!-- gptel-model: ... -->`) or Emacs Local Variables format
- Supported indefinitely for existing sessions

**metadata.yml** (unchanged):
- Format: YAML
- Keys: `session_id`, `created`, `updated`, `preset`, `type` (optional), `parent_session_id` (optional)
- Contains session-level metadata

**scope.yml** (unchanged):
- Format: YAML
- Content: Mutable scope configuration (paths, org-roam patterns, bash tools)
- Generated from preset's scope profile at creation

**branch-metadata.yml** (unchanged):
- Format: YAML
- Location: Non-main branches only
- Keys: `parent_branch`, `created`, `branch_point_position` (optional)

#### Scenario: Initial session.org file
- **WHEN** creating new org-mode session
- **THEN** session.org created with `"* Session\n\n"` content
- **AND** file opens in org-mode
- **AND** gptel's markdown-to-org converter is active

#### Scenario: Initial session.md file (legacy)
- **WHEN** creating legacy markdown session
- **THEN** session.md created with `"###\n"` content
- **AND** file is valid markdown
- **AND** maintains backward compatibility

#### Scenario: Org-mode conversation persistence
- **WHEN** user makes requests in org-mode session buffer
- **AND** saves (Ctrl+S)
- **THEN** gptel writes conversation in org-mode format
- **AND** LLM markdown responses already converted to org by gptel
- **AND** Emacs Local Variables block appended with session state

#### Scenario: Markdown conversation persistence (legacy)
- **WHEN** user makes requests in markdown session buffer
- **AND** saves (Ctrl+S)
- **THEN** gptel writes conversation in markdown format
- **AND** HTML comment or Emacs Local Variables block used for metadata

### Requirement: Directory structure initialization

The system SHALL create session hierarchy with format-appropriate session file.

```
<session-dir>/
├── branches/<branch-name>/
│   ├── session.org         # Org-mode format (new sessions)
│   │   OR
│   ├── session.md          # Markdown format (legacy sessions)
│   ├── metadata.yml
│   ├── scope.yml
│   └── branch-metadata.yml (if not main)
└── current -> branches/<branch-name>
```

**Implementation**: `config/gptel/sessions/filesystem.org`

#### Scenario: New org-mode session creation
- **WHEN** running `M-x jf/gptel-persistent-session`
- **THEN** creates `branches/main/` directory
- **AND** creates `session.org` with org-mode initial content
- **AND** `current` symlink points to `branches/main`
- **AND** no `branch-metadata.yml` in main branch

#### Scenario: New markdown session creation (if specified)
- **WHEN** creating session with explicit markdown format
- **THEN** creates `branches/main/` directory
- **AND** creates `session.md` with markdown initial content
- **AND** maintains legacy behavior

#### Scenario: Branch creation inherits format
- **WHEN** running `M-x jf/gptel-branch-session` from org-mode session
- **THEN** creates new branch with `session.org`
- **AND** uses org-mode format for new branch
- **AND** conversely, markdown branches created from markdown sessions

### Requirement: Auto-initialization on file open

The system SHALL detect and auto-initialize both `.org` and `.md` session files when opened.

**Implementation**: `config/gptel/sessions/commands.org` - `jf/gptel--auto-init-session-buffer` via find-file-hook

#### Scenario: Org-mode session file detection
- **WHEN** file matches `*/branches/<branch-name>/session\.org` pattern
- **THEN** auto-init recognizes as org-mode branch session
- **AND** extracts session-id and branch-name from path
- **AND** applies org-mode specific handling

#### Scenario: Markdown session file detection (legacy)
- **WHEN** file matches `*/branches/<branch-name>/session\.md` pattern
- **THEN** auto-init recognizes as markdown branch session
- **AND** extracts session-id and branch-name from path
- **AND** applies markdown-specific handling (HTML comments)

#### Scenario: Org-mode agent file detection
- **WHEN** file matches `*/agents/<agent-name>/session\.org` pattern
- **THEN** auto-init recognizes as org-mode agent session
- **AND** sets branch-name to "main"

#### Scenario: Markdown agent file detection (legacy)
- **WHEN** file matches `*/agents/<agent-name>/session\.md` pattern
- **THEN** auto-init recognizes as markdown agent session
- **AND** applies legacy handling

#### Scenario: Fast-path optimization for both formats
- **WHEN** find-file-hook checks for session files
- **THEN** checks if filename ends with `.org` OR `.md`
- **AND** proceeds only if either extension matches
- **AND** avoids processing non-session files

#### Scenario: Existing org-mode session (has Local Variables)
- **WHEN** opening `session.org` with Emacs Local Variables block
- **THEN** Emacs reads Local Variables automatically
- **AND** sets buffer-local session vars
- **AND** calls `jf/gptel--ensure-mode-once` (enables gptel-mode)
- **AND** gptel's org-mode support activates automatically

#### Scenario: New org-mode session (no Local Variables yet)
- **WHEN** opening `session.org` with no Local Variables block
- **THEN** reads preset from metadata.yml
- **AND** applies via `gptel--apply-preset` with buffer-local setter
- **AND** enables gptel-mode
- **AND** on first save, Emacs writes Local Variables block

### Requirement: Session creation

The system SHALL create new sessions in org-mode format by default, with backward compatibility for markdown.

**Implementation**: `config/gptel/sessions/commands.org` - `jf/gptel-persistent-session`

#### Scenario: Create org-mode session with default preset
- **WHEN** calling `jf/gptel-persistent-session` without arguments
- **THEN** prompts for session name
- **AND** creates session directory with `session.org`
- **AND** uses default preset (e.g., "executor")
- **AND** opens `session.org` in org-mode buffer
- **AND** initializes with org-mode heading

#### Scenario: Create org-mode session with specified preset
- **WHEN** calling with prefix arg to choose preset
- **THEN** prompts for preset selection
- **AND** creates session with chosen preset
- **AND** uses `session.org` for file
- **AND** applies preset's scope profile to `scope.yml`
