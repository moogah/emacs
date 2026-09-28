## MODIFIED Requirements

### Requirement: Anchor payload contract (push, not consult)

Workspaces SHALL pass a single *anchor payload* to every integration handler
it invokes (whether an `:on-create` handler, an `:on-purge` handler, or a
`:menu` command): a property list of workspace self-knowledge containing at
least `:name`, `:home`, `:sessions-dir`, and `:context`. Integration handlers
SHALL operate solely on the payload they are given; they SHALL NOT determine
the target workspace by consulting global state such as the current tab.

One constructor SHALL be the single source of the payload's shape, so that no
two push sites diverge. Every payload workspaces builds — at birth, from the
transient, and at purge — SHALL come from that constructor.

Workspaces itself SHALL include only its own self-knowledge in the payload and
SHALL NOT add any integration-domain-specific data (e.g. git branch, gptel
preset). Domain-specific context is the integration's responsibility to source
itself. The payload SHALL be an extensible property list so that future
workspace facts can be added without breaking existing handlers.

A consumer that invokes a registered `:menu` command directly — calling the
command with a payload it built through workspaces' constructor, rather than
going through registry dispatch or the workspaces transient — MAY append to
that payload optional keys the invoked command documents. Such keys reach
only that command: they are never present in a registry-dispatched payload,
and no other handler receives them. The only such key today is
`:source-repo`, an existing local directory naming the source repository the
caller has already decided; the git-worktrees add-worktree command reads it
(see the `workspace-git-worktrees` capability, Requirement: Worktree creation
off main). A direct invocation SHALL carry the context that names it
(`bookmark-jump` for the portable-bookmarks consumer) and SHALL be bound by
the same condition as on-demand invocation: a current workspace that is not
broken.

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
- **WHEN** any integration handler is invoked through registry dispatch or the
  workspaces transient
- **THEN** the payload contains only workspace self-knowledge keys
- **AND** it contains no git-, gptel-, or other consumer-domain-specific keys

#### Scenario: a consumer invoking a menu command directly may add a documented key
- **WHEN** the portable-bookmarks consumer answers "add worktree here" at a
  bookmark jump in workspace `myproj` and calls the git-worktrees `:menu`
  command directly
- **THEN** the command receives workspaces' anchor payload for `myproj` with
  `:context` `bookmark-jump`, plus `:source-repo` naming the canonical
  checkout the resolver chose
- **AND** no `:on-create` or `:on-purge` handler receives that payload
- **AND** a payload workspaces builds for registry dispatch never carries
  `:source-repo`

---

### Requirement: Creation-time dispatch

On every workspace creation, workspaces SHALL run each registered
integration's `:on-create` handler exactly once, in registration order,
passing the anchor payload carrying the `:context` of that creation. At
creation the `:context` SHALL be one of `fresh` (the package created the
directory and/or initialized its repository), `anchored-scaffolded` (the
package added the workspace skeleton to a pre-existing repository it did not
initialize), or `anchored-existing` (the package adopted a directory that was
already a workspace, performing no scaffolding). Dispatch SHALL occur for all
three contexts.

The `:context` vocabulary carried by the anchor payload SHALL be a closed set
of six symbols; a seventh is a design change, not a code-level addition.
Beyond the three birth contexts above:

- `menu-invoke` — not a birth context: built by the workspaces transient when
  the user invokes an integration's `:menu` command on the current workspace.
- `purge` — not a birth context: the teardown payload `workspace-purge` hands
  every `:on-purge` handler. Never built for a remote home (purging one only
  unregisters it).
- `bookmark-jump` — neither a birth context nor a transient invocation: built
  by the portable-bookmarks consumer, through workspaces' constructor, for a
  direct call of the git-worktrees `:menu` command when the user answers "add
  worktree here" at a bookmark jump. It is never dispatched through the
  registry, so no other handler receives it.

A handler that branches on `:context` SHALL tolerate every member of the set
(it MAY skip on values it does not act on); no handler is required to branch
at all.

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

#### Scenario: the transient's invocation carries menu-invoke
- **WHEN** the user invokes an integration's `:menu` command from the
  workspaces transient on healthy workspace `myproj`
- **THEN** the payload's `:context` is `menu-invoke`

#### Scenario: teardown carries purge
- **WHEN** workspace `myproj` is purged and an `:on-purge` handler is
  registered
- **THEN** the payload that handler receives has `:context` `purge`

#### Scenario: a consumer's direct menu call carries bookmark-jump
- **WHEN** the portable-bookmarks consumer calls the git-worktrees `:menu`
  command directly for workspace `myproj` at a bookmark jump
- **THEN** the payload's `:context` is `bookmark-jump`
- **AND** no `:on-create` handler runs

#### Scenario: a throwing handler does not abort creation
- **WHEN** integration `A`'s `:on-create` signals an error and integration
  `B`'s `:on-create` is also registered
- **AND** a workspace is created
- **THEN** the workspace is created successfully
- **AND** `A` is recorded as `failed`
- **AND** `B`'s handler still runs
