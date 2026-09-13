# workspace-integrations capability — delta for workspace-notes-in-vault

The payload gains `:node-id` and drops `:sessions-dir`; creation contexts
reduce to `fresh` and `anchored`; the directionality rule generalises to
"no package names another; only the integration layer subscribes".

## MODIFIED Requirements

### Requirement: Anchor payload contract (push, not consult)

Workspaces SHALL pass a single *anchor payload* to every integration handler
it invokes (whether an `:on-create` handler or a `:menu` command): a property
list of workspace self-knowledge containing at least `:name`, `:home`,
`:node-id` (a string or nil), and `:context`. Integration handlers SHALL
operate solely on the payload they are given; they SHALL NOT determine the
target workspace by consulting global state such as the current tab.

Workspaces SHALL include only its own self-knowledge in the payload and SHALL
NOT include any integration-domain-specific data (e.g. git branch, gptel
preset, note title). `:node-id` is self-knowledge: an opaque identity token
the package stores but does not interpret. The payload SHALL NOT carry a
`:sessions-dir` key — the package has no session concept. The payload SHALL
be an extensible property list so that future workspace facts can be added
without breaking existing handlers.

#### Scenario: on-create handler receives the anchor payload
- **WHEN** a workspace `myproj` with `:home ~/emacs-workspaces/myproj/` and
  `:node-id "ABC"` is created and an `:on-create` handler is registered
- **THEN** the handler is called with a payload whose `:name` is `myproj`,
  `:home` is `~/emacs-workspaces/myproj/`, `:node-id` is `"ABC"`, and
  `:context` reflects the creation
- **AND** the payload has no `:sessions-dir` key

#### Scenario: menu command receives a payload built from the current workspace
- **WHEN** the current tab is healthy workspace `myproj`
- **AND** the user invokes a registered `:menu` command
- **THEN** the command is called with an anchor payload describing `myproj`,
  including its current `:node-id`

#### Scenario: payload carries no integration-domain data
- **WHEN** any integration handler is invoked
- **THEN** the payload contains only workspace self-knowledge keys
- **AND** it contains no git-, gptel-, or graph-domain-specific keys

### Requirement: Creation-time dispatch

On every workspace creation, workspaces SHALL run each registered
integration's `:on-create` handler exactly once, in registration order,
passing the anchor payload carrying the `:context` of that creation. The
`:context` SHALL be one of `fresh` (the package initialized the directory's
repository, whether or not it created the directory) or `anchored` (the
package adopted an existing repository, performing no git operation).
Dispatch SHALL occur for both contexts.

`:on-create` handlers SHALL be non-interactive: they SHALL NOT prompt the
user. Each handler invocation SHALL be isolated by an error guard — a handler
that signals an error SHALL be recorded as `failed`, SHALL NOT abort the
creation, and SHALL NOT prevent later handlers from running. A handler MAY
set the workspace's `:node-id` through the public setter; handlers
registered later in the same dispatch SHALL observe the updated value in
their payload.

#### Scenario: dispatch runs every on-create handler once
- **WHEN** two integrations with `:on-create` handlers are registered
- **AND** a workspace is created
- **THEN** each handler is called exactly once

#### Scenario: context is reported per creation kind
- **WHEN** a workspace is created via the default fresh path or by anchoring
  a non-repository directory
- **THEN** the `:on-create` payload `:context` is `fresh`
- **WHEN** a workspace is created by anchoring an existing repository
- **THEN** the `:on-create` payload `:context` is `anchored`

#### Scenario: a node ID set by an earlier handler is visible to later ones
- **WHEN** integration `A` (registered first) sets `:node-id "ABC"` in its
  `:on-create` handler and integration `B` is registered second
- **THEN** `B`'s payload carries `:node-id "ABC"`

#### Scenario: a throwing handler does not abort creation
- **WHEN** integration `A`'s `:on-create` signals an error and integration
  `B`'s `:on-create` is also registered
- **AND** a workspace is created
- **THEN** the workspace is created successfully
- **AND** `A` is recorded as `failed`
- **AND** `B`'s handler still runs

### Requirement: Registry is the published boundary (directionality preserved)

The published boundary between workspaces and its consumers SHALL consist
only of the integration registry, the anchor payload, and the two inversion
variables (`workspace-display-name-function`, `workspace-home-builder`).
Code under `config/workspaces/` SHALL NOT reference, require, or name any
consumer symbol (any `jf/gptel-`, `gptel-`, or `org-graph` symbol). The rule
is symmetric: consumers SHALL NOT be named by workspaces, and workspaces
SHALL NOT be consulted by any package other than the integration layer
(`workspace-graph-integration`). The directionality lint SHALL cover all
three prefixes.

#### Scenario: workspaces names no consumer symbol
- **WHEN** the directionality lint greps `config/workspaces/*.el` for
  `jf/gptel-`, `gptel-`, and `org-graph`
- **THEN** it finds zero matches

#### Scenario: no package other than the integration layer consults workspaces
- **WHEN** the lint greps `config/gptel/sessions/*.el` and
  `config/org-graph/*.el` for `workspace-` symbols
- **THEN** it finds zero matches
