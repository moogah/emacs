## ADDED Requirements

### Requirement: Start an Isolated Target on Demand

The system SHALL provide a documented way to start a new isolated Emacs *server*
target and obtain a stable identifier for it. The system SHALL support starting a
target in either of two modes: a worktree-configured target (the repository's full
configuration loaded with an isolated `runtime/` state directory) or a bare target
(no user configuration loaded). The target SHALL run a named Emacs server such that
it is addressable for subsequent control and observation calls via `emacsclient`.
The server-socket name SHALL serve as the target's identifier.

#### Scenario: Start a worktree-configured target

- **WHEN** a client starts a target in worktree-configured mode (e.g.
  `bin/emacs-isolated.sh --daemon=<name>`)
- **THEN** an isolated Emacs server starts, loads the repository configuration with
  an isolated runtime state directory, and is reachable via
  `emacsclient -s <name> --eval`

#### Scenario: Start a bare target

- **WHEN** a client starts a target in bare mode (e.g. `emacs -Q --daemon=<name>`)
- **THEN** an isolated Emacs server starts with no user configuration loaded and is
  reachable via `emacsclient -s <name> --eval`

#### Scenario: Server, not daemon, is the control requirement

- **WHEN** a long-lived Emacs has called `server-start` under a unique `server-name`
- **THEN** it is controllable via `emacsclient -s <name>` regardless of whether it
  was started with `--daemon`, because the named server socket is the control
  requirement

### Requirement: Targets Are Isolated From the Live Session and Each Other

Each target SHALL run as a separate operating-system process from the user's live
Emacs, with its own isolated runtime/state directory. Actions performed in one
target MUST NOT affect the user's live editing session and MUST NOT affect any other
target's session state.

#### Scenario: Target activity does not touch the live session

- **WHEN** a client performs evaluation or commands that modify buffers and state in
  a target
- **THEN** the user's live Emacs session and its state are unaffected

#### Scenario: Targets do not share session state

- **WHEN** two targets are running and one is modified
- **THEN** the other target's buffers and session state are unaffected

### Requirement: Crash Containment

A failure within a target SHALL be contained to that target, including a crash or
wedge of that target's process. Because each target is a separate operating-system
process driven by independent `emacsclient` invocations, a hung or crashed target
SHALL NOT block control of any other target or of the user's live session.

#### Scenario: A crashing target does not affect others

- **WHEN** a target's process crashes or becomes unresponsive
- **THEN** other targets remain usable, the user's live session is unaffected, and an
  `emacsclient` call against the dead target's socket fails locally without blocking
  other calls

### Requirement: Inspect Target Status

The system SHALL provide a documented way to enumerate running targets and determine
whether a given target is currently running. Enumeration SHALL be possible by
listing the server-socket directory, and liveness SHALL be determinable by a control
call against the target's socket.

#### Scenario: Enumerate running targets

- **WHEN** a client lists the server-socket directory
- **THEN** each live target appears as a socket file named by its identifier

#### Scenario: Detect a dead target

- **WHEN** a client issues a control call against an identifier whose target is no
  longer running
- **THEN** the call fails (no such socket / connection refused), indicating the
  target is not running

### Requirement: Dispose Targets and Clean Up

The system SHALL provide a documented way to stop and dispose a target, terminating
its process and releasing its socket. After disposal, the identifier SHALL no longer
route. Disposing a target MUST NOT affect the user's live Emacs session.

#### Scenario: Disposed target no longer routes

- **WHEN** a client disposes a target (e.g. `emacsclient -s <name> --eval
  '(kill-emacs)'`) and then issues a call naming that identifier
- **THEN** the target's process is terminated, its socket is removed, and the
  subsequent call fails because the target no longer exists

### Requirement: Documented Isolation Boundary

The isolation guarantee SHALL be documented as covering session, process, and
runtime-state separation only. A target SHALL be understood to share the host
filesystem; target isolation is NOT a filesystem sandbox. The control channel SHALL
be local-only (a Unix-domain server socket), and the system SHALL be documented as
for use only with trusted clients exercising the user's own code.

#### Scenario: Filesystem is shared, not sandboxed

- **WHEN** a target writes to a path on the host filesystem
- **THEN** the write affects the host filesystem (isolation does not contain
  filesystem effects), consistent with the documented boundary
