## ADDED Requirements

### Requirement: Evaluate Arbitrary Elisp in a Target

The system SHALL allow a client to evaluate arbitrary Elisp in a running target and
receive the printed result, through the `eval` tier of `bin/emacs-agent` (which
delivers it via `emacsclient -s <name> --eval`). Evaluation SHALL execute in the
target's actually-loaded environment — for a worktree-configured target, the full
repository configuration — so that configuration-specific functions and variables
are in scope. Raw evaluation is the escape hatch beneath the curated tools: it is
freely available on isolated targets and, on the live session, used only with the
user's explicit agreement (see `agent-target-policy` § Permission Tiers).

#### Scenario: Eval runs in the loaded configuration

- **WHEN** a client evaluates a configuration-specific form (e.g. `jf/emacs-dir` or
  `(length jf/enabled-modules)`) against a worktree-configured target
- **THEN** the system evaluates it in the loaded configuration and returns the printed
  value

#### Scenario: Eval errors are reported to the client

- **WHEN** a client evaluates a form that signals an error
- **THEN** the error message is returned rather than a value, and the target remains
  usable for subsequent calls

### Requirement: Control Helpers Are Not Exposed on the Live Session

The literal-insertion, key-injection and command-invocation helpers SHALL be
available for driving isolated targets only. They SHALL NOT be added to the
`emacs-agent-read` or `emacs-agent-write` presets, so they cannot be invoked on the
live session through `call` / `call-write`.

#### Scenario: Helpers absent from the manifest

- **WHEN** an agent requests the manifest from the live session
- **THEN** the literal-insertion, key-injection and command-invocation helpers do
  not appear in it

### Requirement: Literal Text Insertion

The system SHALL provide a target-side helper, loadable into a target, that inserts a
given string into the current buffer literally (no key interpretation), invocable over
the control channel.

#### Scenario: Type literal text into the current buffer

- **WHEN** a client invokes the literal-insertion helper with a string against a target
- **THEN** the string is inserted into the target's current buffer exactly as given,
  and the change is observable via `jf/frame-state`

### Requirement: Key Sequence Injection

The system SHALL provide a target-side helper, loadable into a target, that executes a
key sequence expressed in `kbd` notation, so that keymap-driven behavior can be
exercised. The helper SHALL be documented as requiring a real frame for behavior that
depends on interactive context (see On-Demand Frame Attachment).

#### Scenario: Send a key sequence in kbd notation

- **WHEN** a client invokes the key-injection helper with a `kbd`-notation sequence
  against a target that has a suitable frame
- **THEN** the target executes the sequence as if typed, and resulting state is
  observable via `jf/frame-state`

### Requirement: Invoke Interactive Commands

The system SHALL provide a target-side helper, loadable into a target, that invokes a
named interactive command via `call-interactively`, invocable over the control channel.

#### Scenario: Run a named interactive command

- **WHEN** a client invokes the command helper with the name of an interactive command
  against a target
- **THEN** the target runs the command interactively, and resulting state is observable
  via `jf/frame-state`

### Requirement: On-Demand Frame Attachment

A target started headless SHALL carry no graphical frame by default. The system SHALL
document how to attach a real frame to a running target on demand — a graphical frame
via `emacsclient -s <name> -c` or a TTY frame via `emacsclient -s <name> -t` — so that
control operations depending on a real frame (keymap resolution, redisplay) can be
exercised without requiring a frame for eval-based interaction.

#### Scenario: Headless target needs no frame for eval and observation

- **WHEN** a client only evaluates forms and reads `jf/frame-state` against a headless
  target
- **THEN** the operations succeed without any graphical or TTY frame attached

#### Scenario: Attach a frame for frame-dependent behavior

- **WHEN** a client needs keymap- or redisplay-dependent behavior and attaches a frame
  with `emacsclient -s <name> -c` or `-t`
- **THEN** the target gains a real frame and frame-dependent control operations behave
  as in an interactive session
