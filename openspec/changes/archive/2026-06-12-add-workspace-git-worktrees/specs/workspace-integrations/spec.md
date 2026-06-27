## MODIFIED Requirements

### Requirement: Integration registry and registration

The `workspace-integrations` capability SHALL provide a public registry into
which any subsystem registers a *workspace integration* under a unique
integration id. A registration SHALL declare a human-readable `:label`, and
SHALL declare at least one of: an `:on-create` handler (run automatically at
workspace creation), a `:menu` entry (a key plus an on-demand command), or an
`:on-purge` handler (run automatically before a workspace is purged).
Registering the same id again SHALL replace the prior registration, so a
module reload re-registers idempotently rather than accumulating duplicates.

Workspaces SHALL own and iterate the registry. The registry mechanism SHALL
NOT name, require, or import any specific consumer — consumers attach
themselves by registering.

#### Scenario: Registering an integration makes it available
- **WHEN** a subsystem registers integration id `gptel-session` with a
  `:label`, an `:on-create` handler, and a `:menu` entry
- **THEN** the registry reports `gptel-session` as a registered integration
- **AND** its `:label`, `:on-create`, and `:menu` are retrievable

#### Scenario: An on-purge-only registration is valid
- **WHEN** a subsystem registers an integration declaring only a `:label` and
  an `:on-purge` handler
- **THEN** registration succeeds and the entry is retrievable
- **AND** its `:on-purge` handler is retrievable

#### Scenario: Re-registering the same id replaces, not duplicates
- **WHEN** integration id `gptel-session` is registered twice with different
  handlers
- **THEN** the registry contains exactly one entry for `gptel-session`
- **AND** the entry reflects the most recent registration

#### Scenario: A registration must declare at least one surface
- **WHEN** a subsystem attempts to register an integration with none of
  `:on-create`, `:menu`, or `:on-purge`
- **THEN** registration signals a `user-error`
- **AND** no entry is added to the registry

## ADDED Requirements

### Requirement: Purge-time teardown dispatch

When a workspace is purged, workspaces SHALL run each registered integration's `:on-purge` handler exactly once, in registration order, passing the anchor
payload for that workspace, *before* the filesystem deletion (the directory is
about to be deleted) occurs. Integrations lacking an `:on-purge`
handler SHALL be skipped. Each `:on-purge` invocation SHALL be isolated by an
error guard following the established integration result protocol: a handler
that signals an error or reports `failed` SHALL be surfaced visibly via a
`*Messages*` notice naming the integration and its reason, SHALL NOT abort the
purge, and SHALL NOT prevent later handlers or the filesystem deletion from
proceeding. Teardown dispatch SHALL occur only on purge, never on
`workspace-delete` (which leaves `:home` and its contents on disk).

#### Scenario: on-purge runs before filesystem deletion
- **WHEN** a workspace `myproj` with a registered `:on-purge` handler is purged
- **THEN** the handler is called once with the anchor payload for `myproj`
- **AND** the handler runs before `myproj`'s home directory is deleted

#### Scenario: on-purge is skipped on delete
- **WHEN** workspace `myproj` is removed via `workspace-delete` (non-destructive)
- **THEN** no `:on-purge` handler is invoked
- **AND** the home directory is left on disk

#### Scenario: a throwing on-purge handler does not abort purge
- **WHEN** integration `A`'s `:on-purge` signals an error and integration
  `B`'s `:on-purge` is also registered
- **AND** a workspace is purged
- **THEN** `A` is recorded as `failed` and surfaced via a `*Messages*` notice
- **AND** `B`'s `:on-purge` still runs
- **AND** the workspace's home directory is still deleted
