## ADDED Requirements

### Requirement: Evaluate Elisp in a Target

The broker SHALL expose an `eval-elisp` tool that evaluates client-supplied Emacs
Lisp in a named target and returns the printed result. An evaluation that signals
an error SHALL be reported to the client as a structured error and MUST NOT crash
the broker or the target.

#### Scenario: Successful evaluation returns a result

- **WHEN** a client calls `eval-elisp` with a well-formed expression against a valid
  target
- **THEN** the broker evaluates it in that target and returns the printed value

#### Scenario: Erroring evaluation is reported, not fatal

- **WHEN** a client calls `eval-elisp` with an expression that signals an error
- **THEN** the broker returns the error message as a structured tool error, the
  target remains alive, and the broker remains available

### Requirement: Inject Key Sequences into a Target

The broker SHALL expose a `send-keys` tool that injects a key sequence (in `kbd`
notation) into a named target, invoking whatever commands those keys are bound to
in that target's current context.

#### Scenario: Key sequence triggers bound command

- **WHEN** a client calls `send-keys` with a key sequence against a valid target
- **THEN** the target processes the keys as input and the corresponding bound
  behavior occurs

### Requirement: Insert Literal Text into a Target

The broker SHALL expose a `type-text` tool that inserts literal text into the
target's current buffer, preserving whitespace exactly and without interpreting the
text as key bindings or commands.

#### Scenario: Literal text inserted verbatim

- **WHEN** a client calls `type-text` with a string containing spaces and newlines
  against a valid target
- **THEN** the exact characters are inserted into the target's current buffer with
  whitespace preserved

### Requirement: Run a Named Interactive Command in a Target

The broker SHALL expose a `run-command` tool that invokes a named interactive
command in the target.

#### Scenario: Named command is invoked

- **WHEN** a client calls `run-command` with the name of an interactive command
  against a valid target
- **THEN** the broker invokes that command interactively in the target and reports
  completion or a structured error

### Requirement: Control Tools Require a Valid Target

Every control tool SHALL require a target identifier and SHALL operate only on that
target. A control tool call naming an unknown or disposed target SHALL return a
structured error and perform no action.

#### Scenario: Control call without a live target errors

- **WHEN** any control tool is called naming a target that is not running
- **THEN** the broker returns a structured error and performs no operation
