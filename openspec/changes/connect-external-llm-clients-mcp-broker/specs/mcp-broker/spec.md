## ADDED Requirements

### Requirement: MCP Endpoint Served From a Dedicated Controller

The broker SHALL expose a single Model Context Protocol endpoint that external
LLM clients connect to. The broker SHALL run as a dedicated, headless controller
process and MUST NOT be the user's live, interactive editing session. The endpoint
SHALL bind only to a loopback interface.

#### Scenario: External client connects and lists tools

- **WHEN** an external MCP client connects to the broker endpoint and requests the
  tool list
- **THEN** the broker completes the MCP handshake and returns its advertised tools
  (lifecycle, control, and observation tools)

#### Scenario: Broker is not the live editing session

- **WHEN** the broker is running and serving clients
- **THEN** it runs in a process separate from the user's interactive Emacs, and no
  client request is dispatched into the user's live session

#### Scenario: Endpoint is not exposed beyond loopback

- **WHEN** the broker starts its endpoint
- **THEN** the endpoint is reachable only on a loopback interface and is not bound
  to a publicly reachable address

### Requirement: Target Registry and Routing

The broker SHALL maintain a registry of active targets, each identified by a stable
identifier returned when the target is started. Every control and observation tool
call SHALL specify a target identifier, and the broker SHALL route the call to that
target. A call naming an unknown or already-disposed target SHALL return a
structured error rather than affecting any other target.

#### Scenario: Call routed to the named target

- **WHEN** a client issues a control tool call naming an existing target identifier
- **THEN** the broker dispatches the operation to that target and returns its result

#### Scenario: Call to an unknown target is rejected

- **WHEN** a client issues a tool call naming an identifier not present in the
  registry
- **THEN** the broker returns a structured error and performs no operation on any
  other target

### Requirement: Permissive Trust Without Per-Call Confirmation

The broker SHALL permit arbitrary evaluation and command execution against targets
without prompting for per-call confirmation, and MUST NOT require a human approval
step to service a control tool call. This is justified because every target is an
isolated, disposable instance and never the user's live session.

#### Scenario: Arbitrary evaluation proceeds without prompting

- **WHEN** a client calls the evaluation tool against a valid target
- **THEN** the broker executes it against that target and returns the result without
  presenting any confirmation gate

### Requirement: Non-Blocking Concurrent Execution

The broker SHALL remain responsive while a tool call is in flight. A single target
that is slow or unresponsive MUST NOT block the handling of requests for other
targets. Every call dispatched to a target SHALL be bounded by a timeout, and a
timed-out call SHALL return a structured error to the client.

#### Scenario: A hung target does not freeze the broker

- **WHEN** one target is unresponsive to an in-flight call
- **THEN** the broker continues to accept and service tool calls for other targets

#### Scenario: An unresponsive call times out

- **WHEN** a call dispatched to a target does not complete within the configured
  timeout
- **THEN** the broker returns a timeout error for that call and remains available
  for further requests
