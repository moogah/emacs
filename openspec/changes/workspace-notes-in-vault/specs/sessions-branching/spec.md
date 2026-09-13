# sessions-branching capability — delta for workspace-notes-in-vault

A branch is its own note: it gets a fresh file-level org ID, does not
carry the parent's heading-level IDs, keeps the parent's keywords and
drawer otherwise verbatim, replicates agents under `.agents/`, and fires
a branch-created hook.

## ADDED Requirements

### Requirement: Branch note identity

On branch creation the system SHALL give the new branch's `session.org` a
**fresh** file-level org `:ID:`, replacing the value copied from the
parent, in the same identity-rewrite step that sets `:GPTEL_SESSION_ID:`
and `:GPTEL_BRANCH:`. It SHALL strip every heading-level `:ID:` property
from the copied history (removing a property drawer left empty by the
strip), so that a topic heading's identity belongs only to the branch in
which it was created. All other drawer properties, the `#+filetags:`
keyword, and any other drawer at file level SHALL be preserved verbatim.
The `#+title:` keyword SHALL be rewritten to `<session-id> (<branch>)`.
No two files under the sessions root SHALL share an org ID as a result of
branching.

#### Scenario: Branch has its own file-level ID
- **WHEN** the user branches session `S` whose `session.org` has
  `:ID: AAA`
- **THEN** the new branch's `session.org` has an `:ID:` that is not `AAA`
- **AND** the parent's `:ID:` is still `AAA`

#### Scenario: Copied topic headings lose their IDs
- **WHEN** the parent contains, before the branch point, a heading with
  `:ID: BBB` and `:GPTEL_TOPIC: retry` in its property drawer
- **THEN** the new branch's copy of that heading has `:GPTEL_TOPIC: retry`
  and no `:ID:`
- **AND** an `id:BBB` link still resolves to the parent's heading

#### Scenario: File-level drawers and keywords are inherited
- **WHEN** the parent's file level carries an `EDGES` drawer and
  `#+filetags: :gptel_session:`
- **THEN** the new branch's file level carries the same `EDGES` drawer
  content and the same filetags, and its title is `<session-id> (<branch>)`

### Requirement: Branch-created hook

The system SHALL run `jf/gptel-branch-created-hook` after the new
branch's files exist and its identity has been rewritten, with a plist carrying
`:branch-file`, `:parent-file`, `:session-id`, and `:branch`, under the
same error guard as the session-created hook.

#### Scenario: Hook fires after identity rewrite
- **WHEN** a function on `jf/gptel-branch-created-hook` reads the
  `:branch-file` it is given
- **THEN** the file already carries the fresh `:ID:` and the new
  `:GPTEL_BRANCH:` value

## MODIFIED Requirements

### Requirement: Branch creation model

The system SHALL support creating new branches from existing branches,
enabling divergent conversation paths while preserving shared history.

A branch operation SHALL:
1. Identify a **branch point** - a specific user prompt in the source branch
2. Copy context from the source branch up to (and optionally including) the
   branch point
3. Create a new branch directory with its own session.org, metadata, and
   agent state
4. Preserve lineage via branch-metadata.yml tracking parent branch and
   branch point
5. Give the branch its own note identity (Requirement: Branch note identity)
   and run the branch-created hook (Requirement: Branch-created hook)

Branches SHALL be first-class session objects with independent evolution.

#### Scenario: Branch as first-class session object
- **WHEN** a branch is created from a parent branch
- **THEN** the new branch SHALL have its own directory under `branches/`
- **AND** contain a complete session.org file (not a reference or delta)
  whose file-level `:PROPERTIES:` drawer carries the inherited preset,
  scope keys, (when applicable) parent-session-id, and a fresh `:ID:`
- **AND** contain its own branch-metadata.yml recording parent branch and
  branch point
- **AND** be registered independently in the session registry
- **AND** support further branching (branches can have child branches)

#### Scenario: Shared history preservation
- **WHEN** branching from a conversation at a specific user prompt
- **THEN** all conversation history before the branch point SHALL be copied
  to the new branch, minus heading-level `:ID:` properties
- **AND** both branches share identical conversation text up to the branch
  point
- **AND** subsequent edits to either branch SHALL NOT affect the other

### Requirement: Agent replication

The system SHALL replicate agent state from the parent branch to the new
branch.

Agent replication SHALL:
1. Identify all agent subdirectories in the parent branch's `.agents/`
   directory
2. Recursively copy each agent directory to the new branch's `.agents/`
   directory
3. Preserve agent state files (output files, context, metadata)

Because `.agents/` is hidden, copied agent transcripts that still carry
the parent's org IDs are not visible to an indexer that skips hidden
directories; the system SHALL NOT rewrite IDs inside copied agent
directories.

**Current implementation (MVP):** Copies ALL agent directories from parent
branch. **Future enhancement:** Copy only agents invoked before the branch
point position.

#### Scenario: Copying all agent directories
- **WHEN** creating a branch from a parent with agents
  `.agents/search-agent/` and `.agents/code-generator/`
- **THEN** both agent directories SHALL be recursively copied to the new
  branch's `.agents/`, preserving all files, structure, and permissions

#### Scenario: Empty agents directory
- **WHEN** the parent branch has no `.agents/` directory or it is empty
- **THEN** the new branch SHALL have an empty `.agents/` directory
- **AND** NOT fail branch creation

#### Scenario: Agent state isolation
- **WHEN** an agent writes output in a child branch
- **THEN** the agent state SHALL be isolated to that branch's `.agents/`
  directory and NOT affect the parent or sibling branches
