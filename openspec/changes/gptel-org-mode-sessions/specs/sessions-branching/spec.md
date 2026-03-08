# GPTEL Sessions Branching (Delta Spec)

This delta spec documents changes to sessions-branching for org-mode session support.

## MODIFIED Requirements

### Requirement: Context truncation

The system SHALL copy conversation history from the source branch to the new branch, truncating at the selected branch point, using the appropriate file format.

Context truncation SHALL:
1. Copy all buffer content from the beginning up to the branch point position
2. Preserve text properties and formatting
3. Exclude content at or after the branch point position
4. Write truncated content to the new branch's session file (`.org` or `.md` based on source format)
5. Use Emacs Local Variables format for `.org` files
6. Support HTML comment Local Variables for `.md` files (backward compatibility)

Context truncation SHALL operate at the buffer level, not the semantic message level.

#### Scenario: Copying org-mode content up to branch point
- **WHEN** creating a branch from org-mode session with branch point at position 5420
- **THEN** the new branch's session.org SHALL contain buffer content from position 1 to 5419
- **AND** preserve all text properties (gptel markers, tool outputs)
- **AND** exclude all content from position 5420 onward
- **AND** content is in org-mode format (already converted by gptel)

#### Scenario: Copying markdown content up to branch point (legacy)
- **WHEN** creating a branch from markdown session with branch point at position 5420
- **THEN** the new branch's session.md SHALL contain buffer content from position 1 to 5419
- **AND** preserve all text properties
- **AND** exclude all content from position 5420 onward
- **AND** content is in markdown format

#### Scenario: Format inheritance from parent
- **WHEN** creating a branch from a parent session
- **THEN** the new branch SHALL use the same file format as parent
- **AND** org-mode parent → org-mode branch (session.org)
- **AND** markdown parent → markdown branch (session.md)

### Requirement: Bounds filtering

The system SHALL filter gptel--bounds to match the truncated context and write metadata using format-appropriate Local Variables syntax.

Bounds filtering SHALL:
1. Preserve the gptel--bounds data structure (alist of type keys to region lists)
2. Include only bounds regions that START before the branch point position
3. Exclude regions that start at or after the branch point position
4. Validate bounds structure before and after filtering
5. Write filtered bounds to Local Variables block in appropriate format:
   - Emacs Local Variables format (`# gptel--bounds: ...`) for `.org` files
   - HTML comment format (`<!-- gptel--bounds: ... -->`) OR Emacs Local Variables for `.md` files

#### Scenario: Writing bounds to org-mode Local Variables
- **WHEN** creating branch from org-mode session
- **AND** filtered bounds are:
  ```elisp
  ((response . ((1000 2000) (3000 4000)))
   (tool . ((2500 2800))))
  ```
- **THEN** writes to session.org as:
  ```elisp
  # Local Variables:
  # gptel-model: claude-sonnet-4-6
  # gptel--backend-name: Claude
  # gptel--bounds: ((response . ((1000 2000) (3000 4000))) (tool . ((2500 2800))))
  # End:
  ```

#### Scenario: Writing bounds to markdown Local Variables (legacy)
- **WHEN** creating branch from markdown session
- **AND** filtered bounds exist
- **THEN** writes to session.md using HTML comments:
  ```html
  <!-- Local Variables: -->
  <!-- gptel-model: claude-sonnet-4-6 -->
  <!-- gptel--backend-name: Claude -->
  <!-- gptel--bounds: ((response . ((1000 2000) (3000 4000)))) -->
  <!-- End: -->
  ```
- **OR** uses Emacs Local Variables format if configured

#### Scenario: Reading Local Variables from both formats
- **WHEN** opening an existing branch session
- **THEN** Emacs reads Local Variables automatically (both formats)
- **AND** restores gptel--bounds from either syntax
- **AND** system doesn't need to parse manually

### Requirement: User-facing command

The system SHALL provide `jf/gptel-branch-session` command that creates new branches in the appropriate file format.

The command SHALL:
1. Verify the current buffer is a gptel session buffer (session-initialized)
2. Invoke branch point selection
3. Prompt for a user-provided branch name
4. Determine file format from parent session (`.org` or `.md`)
5. Orchestrate branch creation with format-appropriate handling
6. Open the new branch's session file in a buffer

The command SHALL be interactive and invocable via M-x or keybinding.

#### Scenario: Successful org-mode branch creation
- **WHEN** user invokes `M-x jf/gptel-branch-session` in org-mode session buffer
- **THEN** the system prompts for branch point selection
- **AND** prompts for a branch name
- **AND** creates new branch directory with session.org
- **AND** writes truncated content to session.org
- **AND** uses Emacs Local Variables format for metadata
- **AND** opens session.org in org-mode buffer

#### Scenario: Successful markdown branch creation (legacy)
- **WHEN** user invokes `M-x jf/gptel-branch-session` in markdown session buffer
- **THEN** creates new branch directory with session.md
- **AND** writes truncated content to session.md
- **AND** uses HTML comment or Emacs Local Variables format
- **AND** opens session.md in markdown-mode buffer

## ADDED Requirements

### Requirement: Local Variables format compatibility

The system SHALL support reading and writing Local Variables in both formats during the transition period.

Compatibility layer SHALL:
1. Read both HTML comment and Emacs Local Variables formats
2. Write Emacs Local Variables format for `.org` files
3. Write HTML comment format (or Emacs Local Variables) for `.md` files
4. Handle mixed formats gracefully (e.g., parent has HTML comments, child uses Emacs Local Variables)

#### Scenario: Migrating from HTML comments to Emacs Local Variables
- **WHEN** branching from markdown session with HTML comment Local Variables
- **AND** new branch is org-mode (manual format conversion)
- **THEN** reads metadata from HTML comments in parent
- **AND** writes metadata as Emacs Local Variables in child
- **AND** both formats represent identical session state

#### Scenario: Pure org-mode branch lineage
- **WHEN** all branches in a session are org-mode
- **THEN** all use Emacs Local Variables format consistently
- **AND** no HTML comment parsing needed
- **AND** simplified metadata handling

#### Scenario: Pure markdown branch lineage (legacy)
- **WHEN** all branches in a session are markdown
- **THEN** all use HTML comment format (or Emacs Local Variables)
- **AND** maintains full backward compatibility
- **AND** no org-mode specific code paths triggered
