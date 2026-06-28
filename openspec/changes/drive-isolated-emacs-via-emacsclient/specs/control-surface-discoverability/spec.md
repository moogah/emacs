## ADDED Requirements

### Requirement: Curated Agent Control Registry

The system SHALL provide a curated set of intention-revealing control operations under
a single `jf/agent/*` namespace, loadable into a target, covering the common
drive/query operations. The baseline set SHALL be derived from established prior art
(the emcp tool set) and include, at minimum, Emacs introspection (apropos, describe,
find-definition), variable access, evaluation, key/text input, and frame-state
observation. Each operation SHALL be registered into a single tool-spec registry that
records, per operation, its name, its implementing function, a human-facing docstring,
and structured argument metadata. The registry SHALL be the authoritative contract for
the available operations. The curated set SHALL NOT restrict or replace raw evaluation
(see `emacs-control-tools` § Evaluate Arbitrary Elisp in a Target), which remains
available as an escape hatch for operations outside the curated set.

#### Scenario: Curated operation is invoked over the control channel

- **WHEN** a client invokes a curated `jf/agent/*` operation against a target with valid
  arguments
- **THEN** the operation is performed in the target and the documented return value is
  produced

#### Scenario: Curated surface does not preclude raw eval

- **WHEN** a client needs an operation not present in the curated set
- **THEN** it can still evaluate arbitrary Elisp against the same target, and the
  curated operations remain usable alongside it

### Requirement: Frontend-Agnostic Tool Metadata

The tool-spec registry SHALL carry, for each operation, metadata sufficient to drive
multiple delivery frontends without modification to the registry or the operation
functions — at minimum: a typed argument schema (each argument's name, type, and
description), a category, and a read-only/state-modifying indicator. The registry and
the operation functions SHALL NOT depend on any particular frontend (emacsclient, MCP,
or gptel). This guarantees that an additional consumer of the registry (for example, a
generator that registers each operation as a gptel tool) can be built against this
metadata alone, without changing the core.

#### Scenario: Registry metadata is sufficient for an additional frontend

- **WHEN** a new consumer iterates the registry to expose the operations through a
  different interface that requires typed argument schemas
- **THEN** each registry entry provides the operation name, implementing function, typed
  argument schema, category, and read-only indicator needed to construct that
  interface, without modifying the registry or the operation functions

#### Scenario: Core carries no frontend dependency

- **WHEN** the registry and operation functions are loaded into a target
- **THEN** they reference no emacsclient-, MCP-, or gptel-specific code, so the same
  core is usable across all frontends

### Requirement: Self-Describing API Manifest

The system SHALL provide a function `jf/agent-api` that returns, as JSON, a list of the
control operations in the registry — each entry containing at least the operation name,
its typed argument schema, and its documentation. The manifest SHALL be generated at
call time from the live registry so that it cannot drift from the implemented
operations. `jf/agent-api` SHALL itself be a registered operation, so it appears in its
own listing.

#### Scenario: Client discovers available operations

- **WHEN** a client calls `(jf/agent-api)` against a target with the support module
  loaded
- **THEN** it receives a JSON list of the available control operations with their
  argument lists and documentation

#### Scenario: Manifest reflects the live definitions

- **WHEN** the set of `jf/agent/*` functions in the loaded module differs from any prior
  static description
- **THEN** the result of `(jf/agent-api)` reflects exactly the functions actually
  defined in the target, with no separately-maintained list to fall out of sync

### Requirement: Per-Operation Detail Drill-Down

The system SHALL allow a client to retrieve the full documentation for a single control
operation on demand, so that the client can pull detail for one operation into context
without materializing the entire manifest. Standard Emacs introspection
(`documentation` / `describe-function`) SHALL be sufficient for this; a thin namespace
helper MAY wrap it.

#### Scenario: Retrieve documentation for one operation

- **WHEN** a client requests the documentation for a single named `jf/agent/*` operation
- **THEN** it receives that operation's full docstring without needing to fetch the
  whole manifest

### Requirement: Minimal Static Bootstrap Seed

The system SHALL provide a minimal static entry point that informs a client how to begin
driving a target — at least how to list the API via `jf/agent-api` — delivered through
the client-facing context channels (a Claude Code skill and/or a `CLAUDE.md` pointer,
alongside the client RECIPES doc). The static seed SHALL name the entry point only and
SHALL NOT statically enumerate every control operation; enumeration is the
responsibility of the runtime manifest so that the static surface cannot drift.

#### Scenario: Client learns the entry point from static context

- **WHEN** a client begins a task that requires driving an isolated target and consults
  its loaded context (skill / `CLAUDE.md` / RECIPES)
- **THEN** it learns how to enumerate the available operations at runtime (via
  `jf/agent-api`) without that context having to list each operation

#### Scenario: Static seed does not duplicate the operation list

- **WHEN** the curated `jf/agent/*` namespace changes
- **THEN** no change to the static bootstrap seed is required for the client to discover
  the updated operations, because the seed points at the runtime manifest rather than
  enumerating operations itself
