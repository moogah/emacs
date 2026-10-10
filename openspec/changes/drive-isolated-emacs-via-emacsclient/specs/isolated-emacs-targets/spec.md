## ADDED Requirements

### Requirement: Start an Isolated Target on Demand

The system SHALL provide a way to start a new isolated Emacs *server* target and
obtain a stable identifier for it, through `bin/emacs-agent target start <name>`
(see `agent-target-policy`). The system SHALL support starting a target in either of
two modes: a worktree-configured target (a given worktree's full configuration loaded
with that worktree's isolated `runtime/` state directory) or a bare target (no user
configuration loaded). The target SHALL run a named Emacs server such that it is
addressable for subsequent control and observation calls via `emacsclient`. The
server-socket name SHALL serve as the target's identifier. The name `server` (the
live session's socket) SHALL NOT be accepted as a target name.

#### Scenario: Start a worktree-configured target

- **WHEN** a client starts a target in worktree-configured mode (e.g.
  `bin/emacs-agent target start <name> --worktree <path>`, which runs that
  worktree's `bin/emacs-isolated.sh --daemon=<name>`)
- **THEN** an isolated Emacs server starts, loads the repository configuration with
  an isolated runtime state directory, and is reachable via
  `emacsclient -s <name> --eval`

#### Scenario: Start a bare target

- **WHEN** a client starts a target in bare mode (e.g.
  `bin/emacs-agent target start <name> --bare`, which runs `emacs -Q --daemon=<name>`)
- **THEN** an isolated Emacs server starts with no user configuration loaded and is
  reachable via `emacsclient -s <name> --eval`

#### Scenario: Live socket name is refused

- **WHEN** a client asks to start an isolated target named `server`
- **THEN** the request is refused and no Emacs process is started

#### Scenario: Server, not daemon, is the control requirement

- **WHEN** a long-lived Emacs has called `server-start` under a unique `server-name`
- **THEN** it is controllable via `emacsclient -s <name>` regardless of whether it
  was started with `--daemon`, because the named server socket is the control
  requirement

### Requirement: Targets Are Isolated From the Live Session and Each Other

Each target SHALL run as a separate operating-system process from the user's live
Emacs, with its own isolated runtime/state directory. Actions performed in one
target MUST NOT affect the user's live editing session's in-memory state and MUST
NOT affect any other target's session state. Effects on shared files are governed
by the target's data profile (see Data Profiles) and the Documented Isolation
Boundary.

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

- **WHEN** a client lists targets (e.g. `bin/emacs-agent target list`, which reads
  the server-socket directory)
- **THEN** each running isolated target appears by its identifier, and the live
  session's `server` socket is not listed as an isolated target

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

- **WHEN** a client disposes a target (e.g. `bin/emacs-agent target stop <name>`)
  and then issues a call naming that identifier
- **THEN** the target's process is terminated, its socket is removed, and the
  subsequent call fails because the target no longer exists

### Requirement: Documented Isolation Boundary

The isolation guarantee SHALL be documented as covering session, process, and
runtime-state separation only. A target SHALL be understood to share the host
filesystem; target isolation is NOT a filesystem sandbox. The documentation SHALL
state that a target using the `live` data profile reads and writes the user's real
notes without the live session's write coordination. The control channel SHALL
be local-only (a Unix-domain server socket), and the system SHALL be documented as
for use only with trusted clients exercising the user's own code.

#### Scenario: Filesystem is shared, not sandboxed

- **WHEN** a target writes to a path on the host filesystem
- **THEN** the write affects the host filesystem (isolation does not contain
  filesystem effects), consistent with the documented boundary

### Requirement: Data Profiles

A worktree-configured target SHALL be started with a data profile that determines
which notes it can read and write:

- **`live`** — the user's real note directories;
- **`scratch:<dir>`** — every note directory used by the configuration (the
  org-graph vault and roam roots, the org-roam directory, agenda and capture files,
  and gptel note directories) is located under `<dir>`; indexing of workspace home
  directories is disabled; workspace state is kept in a temporary directory;
- **`none`** — no note directory; org-graph synchronisation is disabled.

The profile SHALL take effect before configuration modules load, so that paths
fixed at module load time follow it. The default profile when none is given SHALL
be stated in the wrapper's documentation.

#### Scenario: Scratch profile confines note writes

- **WHEN** a target started with `scratch:/tmp/vault` creates a note through
  `org_graph_write_node`
- **THEN** the note file is created under `/tmp/vault`, and no file under the user's
  real note directories is created or modified

#### Scenario: Scratch profile indexes only the scratch vault

- **WHEN** a target started with `scratch:/tmp/vault` performs its initial note
  synchronisation
- **THEN** only notes under `/tmp/vault` are indexed, and no workspace home
  directory is added to the sync roots

#### Scenario: Module-load paths follow the profile

- **WHEN** a target is started with `scratch:/tmp/vault`
- **THEN** `org-roam-directory` and the org-graph roam root are under `/tmp/vault`
  once startup completes, and no directory is created under the user's real vault
  during startup

#### Scenario: None profile disables graph sync

- **WHEN** a target is started with the `none` profile
- **THEN** org-graph synchronisation is not started and no note directory is
  watched
