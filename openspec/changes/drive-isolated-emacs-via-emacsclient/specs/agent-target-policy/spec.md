## ADDED Requirements

### Requirement: Two Target Kinds

The system SHALL distinguish two kinds of target an agent can drive:

- the **live session** — the user's running Emacs, reached through the default
  server socket (`server`);
- **isolated targets** — separate Emacs processes started on demand, each reached
  through its own named server socket (see `isolated-emacs-targets`).

Every agent control call SHALL name its target explicitly. The system SHALL NOT
provide any agent-facing path that reaches the live session implicitly (for example
by invoking `emacsclient` without a socket name).

#### Scenario: Live target is named explicitly

- **WHEN** an agent issues a control call naming the `live` target
- **THEN** the call is delivered to the default `server` socket of the running
  Emacs session

#### Scenario: Missing target is refused

- **WHEN** an agent issues a control call without naming a target
- **THEN** the call is refused with an error and nothing is sent to any Emacs

#### Scenario: Live session not running

- **WHEN** an agent names the `live` target and no Emacs is serving the default
  socket
- **THEN** the call fails with an error stating that no live session is running,
  and no isolated target is used in its place

### Requirement: Target Selection Guidance

The system SHALL document a rule for choosing the target, based on what is at risk:

- operations on the user's data through the curated API (reading or writing
  org-graph / org-roam notes) and preparing demonstrations in the user's
  workspaces SHALL use the **live session**;
- exercising or demonstrating unmerged configuration code, exploratory Elisp, and
  developing data features SHALL use an **isolated target**, the latter with a
  scratch data profile.

The documentation SHALL state the principle behind the rule: isolation protects
against bad code, not against damaged data — an isolated target pointed at the real
vault bypasses the live session's write coordination.

#### Scenario: Agent consults guidance for a graph write

- **WHEN** an agent needs to create an org-graph note in the user's vault and
  consults the documented guidance
- **THEN** the guidance directs it to the live session's `call-write` tier rather
  than to an isolated target

#### Scenario: Agent consults guidance for unmerged code

- **WHEN** an agent needs to observe behavior introduced by an unmerged worktree
  change
- **THEN** the guidance directs it to an isolated target started from that worktree

### Requirement: Permission Tiers

The system SHALL expose three tiers of agent control, each as a distinct invocation
so that external permission rules can treat them differently:

- **`call`** — invoke a tool allowlisted by the `emacs-agent-read` preset;
- **`call-write`** — invoke a tool allowlisted by either the `emacs-agent-read` or
  the `emacs-agent-write` preset;
- **`eval`** — evaluate arbitrary Elisp.

Tier membership SHALL be enforced inside Emacs as well as by the wrapper, so a tool
outside a tier's allowlist is refused even when invoked directly.

On the **live session**, `call` and `call-write` SHALL be available, and `eval` SHALL
be used only with the user's explicit agreement (documented policy, enforced through
the client's permission rules). On **isolated targets**, all three tiers SHALL be
available.

#### Scenario: Read tier refuses a write tool

- **WHEN** an agent invokes a tool from the `emacs-agent-write` preset through the
  `call` tier
- **THEN** the call is refused with an error naming the required tier, and the tool
  function is not run

#### Scenario: Tool outside both presets is refused

- **WHEN** an agent invokes, through `call` or `call-write`, a gptel tool that is
  registered but not in either preset
- **THEN** the call is refused and the tool function is not run

#### Scenario: Tiers are distinguishable by permission rules

- **WHEN** the client's permission configuration allows `call` on the live session
  and asks before `eval` on the live session
- **THEN** a live `call` proceeds without prompting and a live `eval` prompts the
  user, because the tiers are distinct invocations

### Requirement: Wrapper as the Single Agent Entry Point

The system SHALL provide a `bin/emacs-agent` command that is the documented entry
point for all agent control: naming the target, selecting the tier, listing tools,
reading results, and starting, listing and stopping isolated targets. It SHALL always
pass an explicit socket name to `emacsclient`.

#### Scenario: Listing tools through the wrapper

- **WHEN** an agent runs the wrapper's tool-listing command against a target
- **THEN** it receives that target's manifest (see `control-surface-discoverability`)

#### Scenario: Wrapper never omits the socket

- **WHEN** the wrapper delivers any call to any target
- **THEN** the underlying `emacsclient` invocation carries an explicit socket name

### Requirement: Live-Session Safeguards

Agent calls against the live session SHALL NOT disrupt the user's working
environment:

- a call SHALL NOT change the selected frame, the selected window, the window
  configuration or the current tab;
- a call SHALL return promptly — the live session's main thread SHALL NOT be held
  waiting for an asynchronous result;
- every live call SHALL be recorded in an in-Emacs action log (time, tier, tool or
  form, a summary of arguments, outcome) that the user can inspect.

#### Scenario: Live call is logged

- **WHEN** an agent completes a `call-write` against the live session
- **THEN** an entry for that call, including its tier, tool name and outcome,
  appears in the action log

#### Scenario: Live call leaves the user's windows alone

- **WHEN** an agent invokes any preset tool against the live session
- **THEN** the selected frame, selected window, window configuration and current
  tab are the same after the call as before it

### Requirement: Asynchronous Tool Results

The system SHALL support tools whose results arrive asynchronously without blocking
the target: invoking an asynchronous tool SHALL return a request identifier
immediately, and a separate result query SHALL return either the stored result or a
pending status. The wrapper MAY poll so that the agent observes a single call. Until
this protocol is implemented, invoking an asynchronous tool SHALL fail with a clear
error rather than block.

#### Scenario: Asynchronous tool returns a request id

- **WHEN** an agent invokes an asynchronous tool through `call`
- **THEN** the call returns a request identifier immediately, and a later result
  query with that identifier returns the tool's result once it is available

#### Scenario: Asynchronous tool before the protocol exists

- **WHEN** an agent invokes an asynchronous tool and the request/poll protocol is
  not yet available
- **THEN** the call fails with an error stating that asynchronous tools are not
  supported, and the target is not blocked
