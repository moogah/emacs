# workspace-integrations Specification

## Purpose

The `workspace-integrations` capability is the published extension point where
external subsystems hook into workspace creation and operation. Workspaces owns
and iterates a registry of integrations; it never names, requires, or imports
its consumers. Each integration declares an optional `:on-create` handler
(fired automatically and non-interactively at workspace birth) and an optional
`:menu` entry (invoked on demand against the current workspace). Workspaces
hands every handler a *pushed* anchor payload of workspace self-knowledge —
integrations never consult global state for "the current workspace." Outcomes
are reported through a domain-agnostic `ok` / `skipped` / `failed` protocol,
and integration results are strictly additive: a workspace is valid iff its own
scaffold succeeded, so a failing integration is surfaced visibly but never
rolls back, unregisters, or breaks the workspace. The registry is the single
published boundary between workspaces and its consumers (e.g. gptel-session),
preserving the `register/boundary/gptel-sessions-workspace-consult`
directionality contract.

## Requirements

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

---

### Requirement: Anchor payload contract (push, not consult)

Workspaces SHALL pass a single *anchor payload* to every integration handler
it invokes (whether an `:on-create` handler or a `:menu` command): a property
list of workspace self-knowledge containing at least `:name`, `:home`,
`:sessions-dir`, and `:context`. Integration handlers SHALL operate solely on
the payload they are given; they SHALL NOT determine the target workspace by
consulting global state such as the current tab.

Workspaces SHALL include only its own self-knowledge in the payload and SHALL
NOT include any integration-domain-specific data (e.g. git branch, gptel
preset). Domain-specific context is the integration's responsibility to source
itself. The payload SHALL be an extensible property list so that future
workspace facts can be added without breaking existing handlers.

#### Scenario: on-create handler receives the anchor payload
- **WHEN** a workspace `myproj` with `:home ~/emacs-workspaces/myproj/` is
  created and an `:on-create` handler is registered
- **THEN** the handler is called with a payload whose `:name` is `myproj`,
  `:home` is `~/emacs-workspaces/myproj/`, `:sessions-dir` is
  `~/emacs-workspaces/myproj/sessions/`, and `:context` reflects the creation

#### Scenario: menu command receives a payload built from the current workspace
- **WHEN** the current tab is healthy workspace `myproj`
- **AND** the user invokes a registered `:menu` command
- **THEN** the command is called with an anchor payload describing `myproj`

#### Scenario: payload carries no integration-domain data
- **WHEN** any integration handler is invoked
- **THEN** the payload contains only workspace self-knowledge keys
- **AND** it contains no git-, gptel-, or other consumer-domain-specific keys

---

### Requirement: Integration result protocol

Every integration handler SHALL report exactly one outcome: `ok` (it performed
its action), `skipped` (it deliberately took no action), or `failed` together
with a human-readable reason. Workspaces SHALL interpret these outcomes
generically: it SHALL NOT depend on knowing *why* a handler skipped or *what*
an `ok` handler did. Only a `failed` outcome SHALL produce a user-visible
notice (see Requirement: Integration failures are visible but never fatal).

#### Scenario: ok outcome is recorded quietly
- **WHEN** an `:on-create` handler creates its artifact and reports `ok`
- **THEN** no failure notice is shown for that integration

#### Scenario: skipped outcome is treated as normal
- **WHEN** an `:on-create` handler reports `skipped` (e.g. because the
  workspace was adopted, not created)
- **THEN** no failure notice is shown
- **AND** workspace creation proceeds unaffected

#### Scenario: failed outcome carries a reason
- **WHEN** an `:on-create` handler reports `failed` with reason `"git not on
  PATH"`
- **THEN** the recorded outcome for that integration is `failed`
- **AND** the reason `"git not on PATH"` is available for the notice

---

### Requirement: Creation-time dispatch

On every workspace creation, workspaces SHALL run each registered
integration's `:on-create` handler exactly once, in registration order,
passing the anchor payload carrying the `:context` of that creation. The
`:context` SHALL be one of `fresh` (the package created the directory and/or
initialized its repository), `anchored-scaffolded` (the package added the
workspace skeleton to a pre-existing repository it did not initialize), or
`anchored-existing` (the package adopted a directory that was already a
workspace, performing no scaffolding). Dispatch SHALL occur for all three
contexts.

`:on-create` handlers SHALL be non-interactive: they SHALL NOT prompt the
user. Each handler invocation SHALL be isolated by an error guard — a handler
that signals an error SHALL be recorded as `failed`, SHALL NOT abort the
creation, and SHALL NOT prevent later handlers from running.

#### Scenario: dispatch runs every on-create handler once
- **WHEN** two integrations with `:on-create` handlers are registered
- **AND** a workspace is created
- **THEN** each handler is called exactly once

#### Scenario: context is reported per creation kind
- **WHEN** a workspace is created via the default fresh path
- **THEN** the `:on-create` payload `:context` is `fresh`
- **WHEN** a workspace is created by anchoring a pre-existing repo that lacks
  `home.org`
- **THEN** the `:on-create` payload `:context` is `anchored-scaffolded`
- **WHEN** a workspace is created by adopting a directory that is already a
  workspace
- **THEN** the `:on-create` payload `:context` is `anchored-existing`

#### Scenario: a throwing handler does not abort creation
- **WHEN** integration `A`'s `:on-create` signals an error and integration
  `B`'s `:on-create` is also registered
- **AND** a workspace is created
- **THEN** the workspace is created successfully
- **AND** `A` is recorded as `failed`
- **AND** `B`'s handler still runs

---

### Requirement: Purge-time teardown dispatch

When a workspace is purged, workspaces SHALL run each registered integration's
`:on-purge` handler exactly once, in registration order, passing the anchor
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

---

### Requirement: Integration failures are visible but never fatal

A workspace's validity SHALL depend solely on its own scaffold and
registration succeeding. Integration outcomes SHALL be *additive*: they SHALL
NOT determine whether the workspace exists or is usable. An integration that
reports `failed` (or signals an error) SHALL be surfaced visibly via a
`*Messages*` notice naming the integration and its reason, but SHALL NOT roll
back, unregister, or mark the workspace broken.

#### Scenario: a failed integration leaves a valid workspace
- **WHEN** the `gptel-session` `:on-create` handler fails during creation of
  workspace `myproj`
- **THEN** `myproj` is registered, its tab is present and selected, and it is
  usable
- **AND** a `*Messages*` notice names the `gptel-session` integration and its
  failure reason
- **AND** `myproj` is NOT marked broken and is NOT unregistered

#### Scenario: integration failure does not roll back the workspace
- **WHEN** any `:on-create` handler fails during creation
- **THEN** no part of the workspace's own scaffold (directory, `home.org`,
  `sessions/`, registry entry, tab) is removed

---

### Requirement: On-demand integration invocation

Each integration that declares a `:menu` entry SHALL be invocable on demand
against the current workspace. The command SHALL receive an anchor payload
built from the current workspace and MAY prompt the user for
integration-domain-specific input (for example, a worktree name). On-demand
invocation SHALL be available only when the current tab is a healthy
(non-broken) workspace.

#### Scenario: invoking a menu integration acts on the current workspace
- **WHEN** the current tab is healthy workspace `myproj`
- **AND** the user invokes the `gptel-session` `:menu` command
- **THEN** a new gptel session is created under
  `~/emacs-workspaces/myproj/sessions/`

#### Scenario: a menu integration may prompt for domain input
- **WHEN** a `:menu` command for an integration that needs user input is
  invoked against a healthy workspace
- **THEN** the integration MAY prompt the user
- **AND** the prompted values are not sourced from or known to workspaces

#### Scenario: menu integrations are unavailable off-workspace
- **WHEN** the current tab is not a workspace, or is a broken workspace
- **THEN** no integration `:menu` command is offered

---

### Requirement: Integrations populate the workspaces transient

The registry-declared `:menu` entries SHALL appear as a dynamically-populated
*Integrations* group in the workspaces transient, built from the registry at
the moment the transient is displayed. The Integrations group SHALL appear
only when the current tab is a healthy workspace.

#### Scenario: registered menu integrations appear in the transient
- **WHEN** two integrations declaring `:menu` entries are registered
- **AND** the user opens the workspaces transient while in a healthy workspace
- **THEN** both appear as entries in the Integrations group

#### Scenario: Integrations group is absent off-workspace
- **WHEN** the user opens the workspaces transient while the current tab is
  not a healthy workspace
- **THEN** the transient shows no Integrations group

---

### Requirement: Registry is the published boundary (directionality preserved)

The integration registry SHALL be the single published boundary between
workspaces and its consumers. Code under `config/workspaces/` SHALL NOT
reference, require, or name any integration consumer symbol (for example, any
`gptel-sessions-*` symbol). The existing directionality lint enforcing
`register/boundary/gptel-sessions-workspace-consult` SHALL continue to pass.

#### Scenario: workspaces names no consumer symbol
- **WHEN** the directionality lint greps `config/workspaces/*.el` for
  consumer-domain symbols such as `gptel-sessions-`
- **THEN** it finds zero matches
