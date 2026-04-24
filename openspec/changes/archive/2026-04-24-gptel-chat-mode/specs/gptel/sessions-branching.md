# Sessions Branching (delta for gptel-chat-mode)

This delta updates `sessions-branching` so that branch-point selection and context truncation operate on `gptel-chat-mode`'s turn block structure instead of `gptel-mode`'s text-property `gptel--bounds`. All unchanged requirements from the base spec continue to apply.

## MODIFIED Requirements

### Requirement: Branch point selection

The system SHALL provide interactive branch point selection based on **outer `#+begin_user` blocks** in a `gptel-chat-mode` session buffer.

Branch point selection SHALL:
1. Parse the source buffer via `gptel-chat--parse-buffer` to obtain the turn list (or equivalently: enumerate outer `#+begin_user` blocks with their buffer positions)
2. Present a numbered list of user turns for selection (showing the first line of each user block as the display label)
3. Allow the user to choose whether to include or exclude the selected user turn in the new branch
4. Return a buffer position marking the branch point:
   - **Include** → position immediately after the `#+end_user` line that closes the selected user turn (so the new branch ends with the selected user turn complete, awaiting an assistant response)
   - **Exclude** → position immediately before the `#+begin_user` line that opens the selected user turn (so the new branch ends before the selected user turn, awaiting a fresh user prompt)

Only outer `#+begin_user` blocks SHALL be valid branch points. Assistant blocks (`#+begin_assistant`), nested tool blocks (`#+begin_tool`), and non-block content (headings, prose, drawers) SHALL NOT be selectable.

This replaces the previous branch-point-selection requirement, which scanned for "user prompts" as regions *without* the `gptel` text property. That text-property convention belonged to `gptel-mode`; chat-mode has no such property, and the structural `#+begin_user` block is the authoritative marker.

#### Scenario: Interactive turn selection
- **WHEN** user invokes `jf/gptel-branch-session` in an active chat-mode session buffer
- **THEN** the system scans for all outer `#+begin_user` blocks via `gptel-chat--parse-buffer`
- **AND** presents a numbered selection interface showing each user block's first line
- **AND** allows the user to select a turn by number
- **AND** asks whether to include or exclude the selected turn

#### Scenario: Include selected turn in branch
- **WHEN** user selects a user turn and chooses INCLUDE
- **THEN** the branch point position is immediately after the `#+end_user` line of the selected turn
- **AND** the new branch contains the selected turn
- **AND** the next assistant response (if any) is truncated from the new branch

#### Scenario: Exclude selected turn from branch
- **WHEN** user selects a user turn and chooses EXCLUDE
- **THEN** the branch point position is immediately before the `#+begin_user` line of the selected turn
- **AND** the new branch does NOT contain the selected turn
- **AND** the user can author a different turn in the new branch

#### Scenario: Tool blocks and assistant blocks are not valid branch points
- **WHEN** scanning the buffer for branch points
- **THEN** `#+begin_assistant` and `#+begin_tool` blocks SHALL NOT appear in the selection list
- **AND** headings and prose outside turn blocks SHALL NOT appear

#### Scenario: No valid branch points
- **WHEN** a chat-mode session buffer contains no outer `#+begin_user` blocks (empty conversation or only a single assistant block)
- **THEN** the system SHALL report no available branch points
- **AND** NOT allow branch creation

### Requirement: Context truncation

The system SHALL copy conversation history from the source branch to the new branch, truncating the buffer at the selected branch point position (as defined in the modified branch-point-selection requirement).

Context truncation SHALL:
1. Copy buffer content from `point-min` up to the branch point position, verbatim
2. Write the truncated content to the new branch's `session.org`
3. Not attempt to filter, rewrite, or normalize the content — the chat-mode block structure in the source buffer is already the canonical form

Context truncation operates at the buffer-content level (like today), but because chat-mode has no `gptel--bounds` text properties, the bounds-filtering step is removed entirely. Block-delimiter integrity is guaranteed by construction: the branch point is always on a line boundary outside any open block.

#### Scenario: Copying content up to branch point
- **WHEN** creating a branch with a branch point at position 5420 (immediately after a `#+end_user`)
- **THEN** the new branch's `session.org` SHALL contain buffer content from position 1 to 5419
- **AND** the content is well-formed chat-mode (parseable by `gptel-chat--parse-buffer`)
- **AND** no truncated / half-open block exists at the end

#### Scenario: Empty branch from first-turn exclude
- **WHEN** the selected branch point is the first user turn
- **AND** the user chooses EXCLUDE
- **THEN** the new branch's `session.org` SHALL contain buffer content from position 1 up to the start of the first `#+begin_user`
- **AND** the result is a valid empty chat-mode session (typically just metadata/heading content with no turns, if any was present before the first user block)

#### Scenario: Branch preserves org commentary
- **WHEN** the source buffer contains org headings or prose between turns
- **AND** the branch point is after one of those commentary regions
- **THEN** the new branch's `session.org` SHALL include the commentary verbatim
- **AND** the chat-mode parser ignores it on message construction (per chat-mode's blocks-only model)

## REMOVED Requirements

### Requirement: gptel--bounds filtering

**Removed.** Chat-mode has no `gptel--bounds` text properties or property drawer — the block structure is the authoritative turn layout. The bounds-filtering step (preserving only bounds regions that start before the branch point, handling overlapping regions, validating bounds structure) has no analogue in chat-mode and is removed.

All scenarios previously under this requirement (filtering bounds before branch point, bounds region overlapping the branch point, validating bounds structure, empty bounds result) are removed along with it.

### Requirement: User prompt detection via text properties

**Removed.** The "user prompt detection" scenario that scanned for regions *without* the `gptel` text property belonged to `gptel-mode`. Chat-mode uses the structural `#+begin_user` block as the authoritative user-turn marker; detection is a byproduct of parsing and does not require a separate text-property scan. See MODIFIED Requirement: Branch point selection for the replacement mechanism.
