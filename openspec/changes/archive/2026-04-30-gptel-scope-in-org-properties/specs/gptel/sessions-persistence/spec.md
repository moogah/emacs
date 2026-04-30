# Sessions Persistence (Delta Spec)

This delta drops `scope.yml` from the session directory layout. Scope is now embedded in `session.org`'s file-level `:PROPERTIES:` drawer at session creation; see the `gptel/scope` and `gptel/scope-profiles` deltas for the storage details.

## MODIFIED Requirements

### Requirement: Directory structure initialization

The system SHALL create this hierarchy for each session:

```
<session-dir>/
├── branches/<branch-name>/
│   ├── session.org
│   ├── metadata.yml
│   └── branch-metadata.yml (if not main)
└── current -> branches/<branch-name>
```

**Implementation**: `config/gptel/sessions/filesystem.org`

#### Scenario: New session creation
- **WHEN** running `M-x jf/gptel-persistent-session`
- **THEN** creates `branches/main/` directory
- **AND** `current` symlink points to `branches/main`
- **AND** no `branch-metadata.yml` in main branch
- **AND** no `scope.yml` is created (scope lives in `session.org`'s drawer)

#### Scenario: Branch creation
- **WHEN** running `M-x jf/gptel-branch-session`
- **THEN** creates `branches/<timestamp>-<name>/` directory
- **AND** includes `branch-metadata.yml` with parent reference
- **AND** updates `current` symlink to new branch
- **AND** the branch's `session.org` carries its own `:PROPERTIES:` drawer with scope copied from the parent branch's drawer

#### Scenario: Agent session creation
- **WHEN** PersistentAgent tool creates sub-agent
- **THEN** creates `branches/<branch-name>/agents/<preset>-<timestamp>-<desc>/`
- **AND** agent directory has `session.org` and `metadata.yml`
- **AND** the agent's `session.org` includes a `:PROPERTIES:` drawer with the agent's scope keys
- **AND** metadata.yml includes `type: "agent"` and `parent_session_id`

### Requirement: Scope profile integration

The system SHALL embed scope configuration from the preset's scope profile in the new `session.org`'s `:PROPERTIES:` drawer at session creation time.

**Implementation**: Calls `jf/gptel-scope-profile--create-for-session` during session creation. The function returns a drawer block (string) that is prepended to the chat-mode initial content before writing `session.org`.

#### Scenario: Drawer populated from preset
- **WHEN** session created with preset `executor` (has `:scope-profile "coding"`)
- **THEN** the new `session.org` is written with a drawer containing `:GPTEL_PRESET: executor` and the resolved scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`, `:GPTEL_SCOPE_CLOUD_AUTH:`, etc.) from the `coding` profile
- **AND** the drawer is followed by the chat-mode initial content (`#+begin_user\n\n#+end_user\n`)

#### Scenario: Drawer with variable expansion
- **WHEN** selected project is `/Users/user/projects/my-project`
- **AND** profile contains `${project_root}/src/**/*.ts`
- **THEN** the corresponding `:GPTEL_SCOPE_READ:` (or similar) line in the drawer reads `/Users/user/projects/my-project/src/**/*.ts`

## REMOVED Requirements

### Requirement: scope.yml file format

**Reason**: `scope.yml` is no longer written by session creation, mutated by the expansion UI, or read by the validator. Scope is part of `session.org`'s drawer. Keeping the spec section would describe a file that is never produced.

**Migration**: All scope storage is via the drawer; see `gptel/scope` delta for the drawer encoding and `gptel/scope-profiles` delta for the writer.
