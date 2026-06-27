## ADDED Requirements

### Requirement: Start an Isolated Target on Demand

The broker SHALL expose a tool to start a new isolated Emacs target and return a
stable identifier for it. The broker SHALL support starting a target in either of
two modes: a worktree-configured target (the repository's full configuration with an
isolated `runtime/` state directory) or a bare target (no user configuration loaded).
A started target SHALL be addressable for subsequent control and observation calls.

#### Scenario: Start a worktree-configured target

- **WHEN** a client requests a new target in worktree-configured mode
- **THEN** the broker launches an isolated Emacs loading the repository configuration
  with an isolated runtime state directory, registers it, and returns its identifier

#### Scenario: Start a bare target

- **WHEN** a client requests a new target in bare mode
- **THEN** the broker launches an isolated Emacs with no user configuration loaded,
  registers it, and returns its identifier

### Requirement: Targets Are Isolated From the Live Session and Each Other

Each target SHALL run as a separate operating-system process from the broker and
from the user's live Emacs, with its own isolated runtime/state directory. Actions
performed in one target MUST NOT affect the user's live editing session and MUST NOT
affect any other target's session state.

#### Scenario: Target activity does not touch the live session

- **WHEN** a client performs evaluation or commands that modify buffers and state in
  a target
- **THEN** the user's live Emacs session and its state are unaffected

#### Scenario: Targets do not share session state

- **WHEN** two targets are running and one is modified
- **THEN** the other target's buffers and session state are unaffected

### Requirement: Crash Containment

A failure within a target (including a crash or wedge of that target's process) SHALL
be contained to that target. The broker process and all other targets SHALL survive,
and the broker SHALL continue serving requests.

#### Scenario: A crashing target does not take down the broker

- **WHEN** a target's process crashes or becomes unresponsive
- **THEN** the broker remains running, other targets remain usable, and a subsequent
  `inspect` reports the affected target as no longer running

### Requirement: Inspect Target Status

The broker SHALL expose a tool to report the status of targets, including each
target's identifier, configuration mode, and whether it is currently running.

#### Scenario: Inspect reports running and dead targets

- **WHEN** a client requests target status
- **THEN** the broker reports each known target's identifier, mode, and running state

### Requirement: Dispose Targets and Clean Up

The broker SHALL expose a tool to stop and dispose a target, terminating its process
and releasing its resources. After disposal, the target's identifier SHALL no longer
route. Disposing a target MUST NOT affect the user's live Emacs session. When the
broker itself shuts down, it SHALL dispose all targets it started, leaving no orphan
target processes.

#### Scenario: Disposed target no longer routes

- **WHEN** a client disposes a target and then issues a call naming that identifier
- **THEN** the target's process is terminated and the subsequent call returns a
  structured error for an unknown target

#### Scenario: Broker shutdown leaves no orphans

- **WHEN** the broker shuts down while targets are running
- **THEN** all targets it started are disposed and no orphan target processes remain

### Requirement: Documented Isolation Boundary

The isolation guarantee SHALL be documented as covering session, process, and
runtime-state separation only. A target SHALL be understood to share the host
filesystem; target isolation is NOT a filesystem sandbox. The broker therefore SHALL
only be used with trusted clients exercising the user's own code.

#### Scenario: Filesystem is shared, not sandboxed

- **WHEN** a target writes to a path on the host filesystem
- **THEN** the write affects the host filesystem (isolation does not contain
  filesystem effects), consistent with the documented boundary
