# Org-Mode Sessions

## Purpose

Provides org-mode as the primary format for gptel session files, leveraging gptel's built-in `gptel-org-convert-response` feature to automatically convert LLM markdown responses to org-mode markup. This enables rich org-mode features (folding, properties, agenda integration) while maintaining natural LLM interaction (LLMs output markdown, gptel converts transparently).

## Key Concepts

### File Format

Session files use `.org` extension and org-mode format:
- **Filename**: `session.org` (was `session.md`)
- **Major mode**: org-mode (detected by Emacs from file extension)
- **Content format**: Org-mode markup (headings use `*`, code blocks use `#+begin_src`)
- **LLM output**: Markdown (natural for LLMs), converted automatically to org-mode

### Automatic Conversion

gptel's built-in converter (`gptel-org-convert-response`, enabled by default):
- Converts markdown responses to org-mode format on-the-fly
- Handles streaming responses (incremental conversion as text arrives)
- Supports: code blocks (` ``` ` → `#+begin_src`), headings (`#` → `*`), emphasis (`**text**` → `*text*`), inline code (`` `code` `` → `=code=`)
- Detects mixed org/markdown markup and handles gracefully
- No external dependencies (built into gptel, no pandoc needed)

### Local Variables Format

Session metadata stored using Emacs Local Variables syntax (works in both org-mode and markdown-mode):
```elisp
# Local Variables:
# gptel-model: claude-sonnet-4-6
# gptel--backend-name: Claude
# gptel--bounds: ((response . ((100 500) (800 1200))))
# End:
```

This replaces markdown HTML comments (`<!-- gptel-model: ... -->`) used in markdown sessions.

### Initial Content

New org-mode sessions initialized with org-mode heading:
```org
* Session

```

This replaces markdown heading (`###\n`) used in markdown sessions.

## ADDED Requirements

### Requirement: Org-mode session file format

The system SHALL create new session files with `.org` extension and org-mode format.

Session creation SHALL:
1. Use filename `session.org` instead of `session.md`
2. Initialize content with org-mode heading (`* Session\n\n`)
3. Let Emacs auto-detect org-mode from file extension
4. Enable gptel's `gptel-org-convert-response` for automatic markdown-to-org conversion

#### Scenario: New org-mode session creation
- **WHEN** creating a new session via `jf/gptel-persistent-session`
- **THEN** creates `branches/main/session.org` file
- **AND** initializes with content `"* Session\n\n"`
- **AND** Emacs opens file in org-mode (based on `.org` extension)
- **AND** gptel's markdown-to-org converter is active (default enabled)

#### Scenario: Activities session uses org-mode format
- **WHEN** creating a session via activities integration
- **THEN** creates `session.org` in the session directory
- **AND** initializes with org-mode heading
- **AND** stores session-specific metadata in `metadata.yml` (unchanged)

#### Scenario: Agent sessions use org-mode format
- **WHEN** PersistentAgent tool creates a sub-agent session
- **THEN** creates `agents/<agent-id>/session.org`
- **AND** initializes with org-mode heading
- **AND** inherits format from parent session type

### Requirement: Automatic markdown-to-org conversion

The system SHALL rely on gptel's built-in `gptel-org-convert-response` feature to convert LLM markdown responses to org-mode format.

Conversion behavior:
- LLMs output natural markdown (` ``` `, `#` headings, `**bold**`, etc.)
- gptel detects buffer is org-mode
- gptel applies conversion automatically before inserting response
- No configuration changes needed (enabled by default)
- No custom conversion logic in session management code

#### Scenario: LLM markdown response converted to org-mode
- **WHEN** LLM responds with markdown code block:
  ````markdown
  ```elisp
  (defun hello () (message "Hello"))
  ```
  ````
- **AND** session buffer is org-mode
- **THEN** gptel converts to org-mode before insertion:
  ```org
  #+begin_src elisp
  (defun hello () (message "Hello"))
  #+end_src
  ```
- **AND** conversion happens automatically (no manual intervention)

#### Scenario: Mixed markdown/org markup handled gracefully
- **WHEN** LLM outputs mixed markdown and org syntax (sometimes happens mid-response)
- **THEN** gptel's converter detects org syntax and preserves it
- **AND** converts only the markdown portions
- **AND** avoids double-conversion errors

#### Scenario: Streaming responses converted incrementally
- **WHEN** LLM streams response with markdown formatting
- **AND** streaming is enabled (gptel default)
- **THEN** gptel applies conversion incrementally as text arrives
- **AND** user sees org-mode formatted text appearing in real-time

### Requirement: Emacs Local Variables format

The system SHALL use Emacs Local Variables syntax for session metadata in org-mode sessions.

Local Variables format:
- Placed at end of file
- Prefixed with `# ` (org-mode comment)
- Contains buffer-local variable assignments
- Read automatically by Emacs when file is opened
- Works in both org-mode and markdown-mode (forward/backward compatible)

#### Scenario: Write Local Variables in org-mode format
- **WHEN** saving session state to `session.org`
- **THEN** appends Local Variables block at end of file:
  ```elisp
  # Local Variables:
  # gptel-model: claude-sonnet-4-6
  # gptel--backend-name: Claude
  # gptel--bounds: ((response . ((100 500))))
  # End:
  ```
- **AND** uses `# ` comment prefix (org-mode syntax)

#### Scenario: Read Local Variables from org-mode session
- **WHEN** opening existing `session.org` file
- **THEN** Emacs reads Local Variables block automatically
- **AND** sets buffer-local variables (`gptel-model`, `gptel--backend-name`, etc.)
- **AND** restores session state without manual parsing

#### Scenario: Backward compatibility with HTML comments
- **WHEN** opening legacy markdown session with HTML comment Local Variables
- **THEN** system continues to read HTML comment format for `.md` files
- **AND** uses Emacs Local Variables format only for `.org` files
- **AND** supports both formats during transition period

### Requirement: Dual-format session detection

The system SHALL detect and initialize both `.org` and `.md` session files during the auto-initialization hook.

File detection SHALL:
1. Check for both `.org` and `.md` extensions in filename checks
2. Match both `session.org` and `session.md` patterns in path regex
3. Apply appropriate format handling based on detected extension
4. Maintain backward compatibility with existing markdown sessions

#### Scenario: Auto-initialize org-mode session
- **WHEN** opening file matching pattern `/branches/.*/session\.org$`
- **THEN** auto-init hook recognizes it as a session file
- **AND** loads session metadata from `metadata.yml`
- **AND** enables gptel-mode
- **AND** applies session preset
- **AND** registers session in session registry

#### Scenario: Auto-initialize markdown session (legacy)
- **WHEN** opening file matching pattern `/branches/.*/session\.md$`
- **THEN** auto-init hook recognizes it as a legacy session
- **AND** applies same initialization flow
- **AND** uses HTML comment format for Local Variables
- **AND** maintains full backward compatibility

#### Scenario: Fast-path detection for both formats
- **WHEN** find-file-hook runs for efficiency check
- **THEN** checks if filename ends with `.org` OR `.md`
- **AND** proceeds only if either extension matches
- **AND** avoids unnecessary processing for non-session files

### Requirement: Org-roam integration simplification

The system SHALL remove redundant markdown-to-org conversion from org-roam tool integration, since session content is already in org-mode format.

Integration changes:
- Remove `jf/markdown-to-org` function call from `create_roam_node` tool
- Pass session content directly to org-roam (already org-mode)
- Maintain org-roam node creation workflow unchanged
- Preserve pandoc fallback for edge cases (if needed)

#### Scenario: Create org-roam node from org-mode session
- **WHEN** `create_roam_node` tool is called with content from org-mode session
- **AND** content is already in org-mode format (converted by gptel)
- **THEN** writes content directly to org-roam node file
- **AND** does NOT call `jf/markdown-to-org` converter
- **AND** avoids redundant conversion overhead

#### Scenario: Org-roam node metadata unchanged
- **WHEN** creating org-roam node from session content
- **THEN** node structure remains unchanged:
  ```org
  :PROPERTIES:
  :ID: <uuid>
  :END:
  #+title: Node Title

  <content from session>
  ```
- **AND** org-roam indexing works identically

### Requirement: Backward compatibility

The system SHALL maintain support for existing markdown sessions indefinitely.

Compatibility SHALL:
- Detect and initialize `.md` sessions with full functionality
- Use HTML comment Local Variables format for markdown sessions
- Allow users to continue using markdown sessions if preferred
- Provide optional manual migration path (rename `.md` → `.org`)
- Create new sessions in org-mode format by default

#### Scenario: Open legacy markdown session
- **WHEN** opening existing `session.md` file
- **THEN** system detects markdown format
- **AND** applies markdown-specific handling (HTML comments)
- **AND** session functions identically to before
- **AND** user sees no breaking changes

#### Scenario: Create new session in org-mode format
- **WHEN** user creates a new session (any method)
- **AND** no format override specified
- **THEN** creates `session.org` file by default
- **AND** uses org-mode format and Local Variables syntax

#### Scenario: Manual migration is optional
- **WHEN** user has existing markdown sessions
- **THEN** no forced migration occurs
- **AND** both formats coexist peacefully
- **AND** user can manually rename `.md` → `.org` if desired
- **AND** system handles both formats correctly
