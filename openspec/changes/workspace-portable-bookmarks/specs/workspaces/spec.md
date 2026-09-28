## MODIFIED Requirements

### Requirement: Buffer reincarnation across restart

Each window-state leaf written by `workspace-save` (or by any autosave path) SHALL carry a `workspace-buffer` entry in its window `parameters` slot. The entry encodes enough information to reconstruct the buffer's visible state across a restart, even when the buffer is non-file (`*Messages*`, `magit-status`, `eshell`, narrowed indirect buffers, etc).

A `workspace-buffer` SHALL contain at least:
- A bookmark record produced by `bookmark-make-record` (the standard Emacs bookmark API).
- The buffer's filename (for file-backed buffers), or nil.
- The buffer's name string.
- Flags indicating whether the buffer is narrowed and/or an indirect buffer.

On restore, the package SHALL walk the saved window-state and replace each leaf's buffer reference with a freshly reincarnated buffer. The reincarnation SHALL attempt, in order:

1. **Bookmark restore** — `bookmark-jump` on the saved record, which dispatches
   through `bookmark-handle-bookmark` and so passes every seam a consumer has
   put on the host's handler dispatch. This restores point position, narrowing
   region, major-mode-specific state, and works for non-file buffers via their
   registered bookmark handlers. The jump SHALL run with the package's own
   variable `workspace--restoring-layout-leaf` bound non-nil, and with nothing
   else about the handler's environment changed: a handler that asks the user
   is asked as on any other visit. This step SHALL answer exactly one of three
   things: a live buffer (the step won), nothing (the step FAILED: any error
   other than `user-error`, or a jump that produced no buffer), or a DECLINE
   carrying the handler's message (the handler signalled `user-error`).
   `quit` propagates.
2. **Filename fallback** — `find-file-noselect` on the saved filename, if the
   bookmark restore FAILED.
3. **Name fallback** — `get-buffer` on the saved name, if neither of the above
   succeeded.
4. **Error buffer** — a visible, named buffer describing what happened, when
   the three steps above fail or when step 1 declined. The window slot is never
   left empty.

A DECLINE is a gate on the chain, not a fifth step: when the bookmark handler
declines, steps 2 and 3 SHALL be skipped and the leaf SHALL land on the error
buffer carrying the handler's reason, which says that the saved filename was
deliberately not opened. The saved filename is the path the buffer had when the
layout was captured; a handler that has just refused its bookmark's location
SHALL NOT be second-guessed by opening that path anyway. The gate applies to
every bookmark handler, stock or third-party, and to every consumer seam on the
handler dispatch. Step 1's three answers are a closed set: any other value
SHALL signal rather than reach the filename step.

The package SHALL collect the declines of one restore and report them ONCE,
after the leaf walk, in a single message naming each declined leaf and its
reason. It SHALL NOT prompt, and SHALL NOT message once per leaf.

The package SHALL trap read-time errors per leaf so that one unreadable bookmark (e.g. the `help-mode` natively-compiled-subr `bug#56643`) does not abort the entire restore. Each FAILED leaf falls through to the next step in the chain.

The package SHALL defer the `window-state-put` call via `run-at-time nil nil ...` so the restore does not race against `bookmark--jump-via`'s buffer-display call. The declines report SHALL be emitted before that deferred paint is queued.

#### Scenario: Point position is preserved across restart
- **WHEN** the user has `~/p/foo.el` open with point at line 234
- **AND** the user invokes `workspace-save` and quits Emacs
- **AND** Emacs is restarted and the user selects the workspace tab
- **THEN** `~/p/foo.el` is open in the restored window
- **AND** point is at line 234 (not line 1)

#### Scenario: Narrowed buffer comes back narrowed
- **WHEN** the user has `~/p/big.el` narrowed to a region in workspace `code`
- **AND** the user invokes `workspace-save` and restarts Emacs
- **AND** the user selects the `code` tab
- **THEN** `~/p/big.el` is displayed
- **AND** the narrowing is restored to the same region
- **AND** `widen` returns the buffer to its full extent (so the narrowing is the bookmark's, not a buffer truncation)

#### Scenario: Magit status buffer is reincarnated, not replaced
- **WHEN** the user has a `magit-status` window open on `~/repo/` in workspace `code`
- **AND** the user invokes `workspace-save` and restarts Emacs
- **AND** the user selects the `code` tab
- **THEN** the window shows a live `magit-status` buffer for `~/repo/` (via magit's registered bookmark handler)
- **AND** the window is NOT showing `*scratch*`

#### Scenario: One unreadable bookmark does not abort the restore
- **WHEN** a saved layout has three windows, one of which holds a `*Help*` buffer whose bookmark contains a natively-compiled subr (`bug#56643`)
- **AND** the user restores the layout
- **THEN** the other two windows restore successfully via the bookmark chain
- **AND** the third window holds either the file-based fallback (if the help buffer was visiting a file) or a clearly named error buffer
- **AND** no exception escapes to the user

#### Scenario: A bookmark handler that declines lands on a reason buffer
- **WHEN** a saved layout has a leaf whose bookmark handler signals
  `user-error` when jumped to during restore
- **THEN** that leaf's window holds the error buffer carrying the handler's
  message
- **AND** the saved filename is NOT opened in its place
- **AND** the saved buffer name is NOT looked up in its place
- **AND** the other leaves restore normally

#### Scenario: Declines are reported once per restore
- **WHEN** two leaves of one layout decline during a restore
- **THEN** exactly one message reports both leaves and their reasons
- **AND** no prompt is shown for either leaf

#### Scenario: A mere failure still falls through to the filename
- **WHEN** a leaf's bookmark restore signals an error that is not a
  `user-error`, and the saved filename exists
- **THEN** the leaf's window holds the file visited from the saved filename

#### Scenario: A stock handler's own question is asked once, as on any visit
- **WHEN** a leaf's file is large enough that the stock handler asks whether
  to open it
- **THEN** the question is asked exactly once during the restore, as on any
  other visit
- **AND** answering it lets the leaf restore normally

---

### Requirement: workspace-new default scaffolding

The package SHALL scaffold a fresh workspace directory at `(expand-file-name NAME workspaces-default-parent-directory)` when `workspace-new NAME` is invoked without a prefix argument. The default value of `workspaces-default-parent-directory` SHALL be `~/emacs-workspaces/`.

Before any filesystem work, the package SHALL signal `user-error` when:

- NAME is not a single directory name — it is empty, is `.` or `..`, contains
  a directory separator, or is an absolute or `~` path. The registry key must
  equal the home's basename, so such a NAME cannot name a workspace.
- A workspace is already registered as NAME, whether or not that entry is
  broken. Registering again would replace the existing entry and lose its
  layouts; the message names `workspace-re-anchor` and `workspace-purge` as
  the remedies for a broken one.
- The computed home is a remote (TRAMP) directory. Remote workspace homes are
  not supported; the check is by name alone and opens no connection.

The scaffold pipeline SHALL then, in order:

1. Compute the home path; signal `user-error` if a directory already
   exists at that path (the default path is reserved; collisions force the
   user to either pick a different name or use the prefix-arg anchor flow).
2. `make-directory HOME t`.
3. `git init` in `HOME` via subprocess.
4. Write `<HOME>/home.org` containing the skeleton template (at minimum:
   `#+TITLE: NAME` keyword, a `* Description` heading, and a `* Notes`
   heading; the package MAY include additional decorative headings but
   SHALL NOT include a `* Sessions` heading that the package will later
   want to overwrite).
5. `make-directory <HOME>/sessions/` (created empty; the package SHALL NOT
   write any session file here).
6. `git add .` followed by `git commit -m "Initial workspace"` via
   subprocess.
7. Insert the workspace into the registry with `:home` set to `HOME`,
   create and select its tab, and run `workspace-home-builder`.
8. Run integration creation-time dispatch with `:context` `fresh` (see the
   `workspace-integrations` capability). This step is additive: its outcome
   SHALL NOT affect whether the workspace was created, and any files an
   integration creates here are NOT part of the `Initial workspace` commit.

The pipeline SHALL stop and signal a user-visible error if any of steps
2–6 fail. On failure the package SHALL NOT register the workspace and
SHALL leave any partially-scaffolded directory in place for the user to
inspect (cleanup is not automatic — silent `rm -rf` on errors is too risky).
A failure in step 8 (integration dispatch) SHALL NOT signal a pipeline error
and SHALL NOT unregister the already-created workspace.

#### Scenario: Default-path workspace is fully scaffolded and committed
- **WHEN** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **AND** `~/emacs-workspaces/myproj/` does not exist
- **THEN** `~/emacs-workspaces/myproj/` exists and contains a `.git/`
  directory
- **AND** `~/emacs-workspaces/myproj/home.org` exists and contains
  `#+TITLE: myproj`
- **AND** `~/emacs-workspaces/myproj/sessions/` exists
- **AND** no file matching `*-initial.org` is created by the package itself
- **AND** the git log shows exactly one commit whose message is `Initial
  workspace`
- **AND** the tab `myproj` is selected
- **AND** the workspace's `:home` is `~/emacs-workspaces/myproj/`

#### Scenario: Creation dispatches integrations with fresh context
- **WHEN** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **THEN** after the workspace is registered, every registered integration's
  `:on-create` handler runs once with `:context` `fresh`

#### Scenario: Default-path collision is rejected
- **WHEN** `~/emacs-workspaces/myproj/` already exists (from any source —
  earlier session, manual mkdir, prior failed scaffold)
- **AND** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **THEN** the command signals a `user-error`
- **AND** the existing directory contents are unchanged
- **AND** no tab is created
- **AND** the user is directed (in the error message) to retry with a
  prefix arg to anchor the existing directory

#### Scenario: A name that is not a single directory name is refused
- **WHEN** the user invokes `workspace-new` with name `"a/b"`, `".."`, `""`
  or `"~/x"` and no prefix arg
- **THEN** the command signals a `user-error` naming the rule
- **AND** no directory is created and no tab is created

#### Scenario: A name already registered is refused
- **WHEN** a workspace named `myproj` is already registered (healthy or
  broken)
- **AND** the user invokes `workspace-new` with name `"myproj"`
- **THEN** the command signals a `user-error` naming the existing home
- **AND** the existing registry entry is unchanged
- **AND** no directory is created and no tab is created

#### Scenario: A remote default parent is refused
- **WHEN** `workspaces-default-parent-directory` is a remote (TRAMP) path
- **AND** the user invokes `workspace-new` with name `"myproj"` and no
  prefix arg
- **THEN** the command signals a `user-error` saying remote homes are not
  supported
- **AND** no connection is opened and no tab is created

#### Scenario: Mid-pipeline failure leaves no registry entry
- **WHEN** `git init` fails (e.g., `git` not on PATH) during scaffolding
  of `"myproj"`
- **THEN** the command signals a user-visible error
- **AND** the partially-created directory is NOT removed
- **AND** the registry contains no entry for `myproj`
- **AND** no tab is created
- **AND** no integration `:on-create` handler is run

---

### Requirement: Anchoring an existing directory via prefix arg

When `workspace-new` is invoked with a prefix argument, the package SHALL
prompt the user for an existing directory via `read-directory-name` and
SHALL anchor a workspace to that directory. The workspace's registry name is
the directory's basename. Before any tab, scaffold or registry work, the
package SHALL signal `user-error` when:

- the chosen directory is remote (TRAMP). Remote workspace homes are not
  supported; the check is by name alone and opens no connection;
- the chosen directory is already a registered workspace's `:home`, compared
  by file identity rather than spelling (so a trailing-slash, `~`, symlink or
  case variant of a registered home is still refused);
- a workspace is already registered under the directory's basename, whether
  or not that entry is broken (anchoring would replace its entry).

The behavior SHALL then depend on the chosen directory's state:

1. **Already a git repository AND contains `home.org`** → register only;
   no scaffolding occurs; no git operations occur.
2. **Already a git repository AND lacks `home.org`** → write
   `<HOME>/home.org` and create `<HOME>/sessions/` if absent. The package
   SHALL NOT create any session file and SHALL NOT run any git commands —
   the user owns this repository and decides when to stage/commit.
3. **Not a git repository** → run the full scaffold pipeline from
   `Requirement: workspace-new default scaffolding` starting at step 3
   (the directory already exists; skip `make-directory`).

After the workspace is registered, the package SHALL run integration
creation-time dispatch (see the `workspace-integrations` capability) with
`:context` `anchored-existing` for case 1, `anchored-scaffolded` for case 2,
and `fresh` for case 3. Integration dispatch is additive and its outcome SHALL
NOT affect whether the workspace was anchored.

The package SHALL NOT modify, rename, or delete any file in the target
directory that it did not itself create as part of scaffolding.

#### Scenario: Anchor an existing project repo with home.org
- **WHEN** `~/code/myproj/` is a git repo containing `home.org`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/code/myproj/`
- **THEN** no files in `~/code/myproj/` are created or modified
- **AND** no git command is invoked
- **AND** a workspace is registered with `:home ~/code/myproj/` and
  registry name `myproj`
- **AND** the tab `myproj` is selected
- **AND** integration dispatch runs with `:context` `anchored-existing`

#### Scenario: Anchor an existing repo without home.org — no auto-commit
- **WHEN** `~/code/myproj/` is a git repo with no `home.org`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/code/myproj/`
- **THEN** `~/code/myproj/home.org` is created with the skeleton template
- **AND** `~/code/myproj/sessions/` is created (empty)
- **AND** no `*-initial.org` file is created by the package
- **AND** NO git command is run (the new files appear as untracked /
  unstaged to the user's existing repo)
- **AND** a workspace is registered with `:home ~/code/myproj/`
- **AND** integration dispatch runs with `:context` `anchored-scaffolded`

#### Scenario: Anchor a non-repo directory triggers full scaffold including git init
- **WHEN** `~/work/notes/` exists but has no `.git/` and no `home.org`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/work/notes/`
- **THEN** `git init` runs in `~/work/notes/`
- **AND** `home.org` and `sessions/` are created
- **AND** an initial commit is made
- **AND** integration dispatch runs with `:context` `fresh`

#### Scenario: Anchoring rejects a directory already registered
- **WHEN** a workspace is already registered with `:home ~/code/myproj/`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/code/myproj/` again (in any spelling that names the same directory)
- **THEN** the command signals `user-error`
- **AND** the existing registry entry is unchanged
- **AND** no new tab is created
- **AND** no integration `:on-create` handler is run

#### Scenario: Anchoring rejects a basename already registered
- **WHEN** a workspace named `myproj` is registered with `:home ~/code/myproj/`
- **AND** the user invokes `workspace-new` with a prefix arg and chooses
  `~/other/myproj/`
- **THEN** the command signals `user-error` naming the existing workspace
- **AND** the existing registry entry is unchanged
- **AND** no new tab is created

#### Scenario: Anchoring rejects a remote directory
- **WHEN** the user invokes `workspace-new` with a prefix arg and chooses a
  remote (TRAMP) directory
- **THEN** the command signals `user-error` saying remote homes are not
  supported
- **AND** no connection is opened, nothing is scaffolded, and no tab is
  created

---

### Requirement: workspace-purge as the destructive deletion command

The package SHALL provide `workspace-purge NAME` as the destructive
counterpart to `workspace-delete`. `workspace-purge` SHALL:

1. Confirm with the user via `yes-or-no-p`, displaying the absolute path
   to be deleted.
2. On confirmation, perform `workspace-delete`'s unregister steps (the
   registry entry is removed and the state file flushed before anything
   touches the filesystem).
3. Re-check the purge scope (see below); on failure signal `user-error` and
   stop, leaving the entry unregistered.
4. Dispatch each registered integration's `:on-purge` handler with the anchor
   payload for the workspace (see the `workspace-integrations` capability,
   Requirement: Purge-time teardown dispatch). This step is additive and
   error-guarded: its outcome SHALL NOT affect whether the workspace is
   purged, and a failing handler SHALL be surfaced visibly but SHALL NOT
   abort the purge.
5. Recursively delete `:home` from the filesystem.

`:on-purge` dispatch (step 4) SHALL occur before the filesystem deletion
(step 5) so integrations can clean up resources anchored under `:home` (e.g.
git worktrees) while the directory still exists.

`workspace-purge` SHALL refuse to operate, before the confirmation prompt,
when `:home` does not lie STRICTLY below `workspaces-default-parent-directory`
unless the user passes a prefix argument acknowledging the unusual location.
Containment SHALL be judged by file identity, not spelling. The default parent
itself is NOT in scope: purging a workspace anchored at the parent would
delete every workspace below it, so it needs the prefix-argument opt-in like
any external home. A broken workspace whose `:home` no longer exists SHALL be
judged by its nearest existing ancestor, so a vanished home below the parent
is still in scope. This guards against accidentally purging an anchored
existing project (e.g., `~/code/myproj/`).

The scope check SHALL be repeated after the confirmation prompt, immediately
before teardown dispatch and the recursive delete: the filesystem can change
while the prompt is open. If the home has left the default parent in the
meantime and no prefix argument was given, the package SHALL signal
`user-error`, SHALL NOT dispatch `:on-purge`, and SHALL NOT delete anything;
the registry entry is already removed at that point and stays removed.

A remote (TRAMP) `:home` SHALL never be contacted. Purging such a workspace
SHALL only ask to unregister it (with or without a prefix argument) and, on
confirmation, SHALL remove the registry entry and nothing else: no scope
check, no `:on-purge` dispatch, no deletion on the remote host.

#### Scenario: Purge deletes the home directory after confirmation
- **WHEN** workspace `myproj` exists with `:home ~/emacs-workspaces/myproj/`
- **AND** the user invokes `workspace-purge` on `myproj`
- **AND** the user confirms `yes` at the prompt
- **THEN** registered `:on-purge` handlers run while the directory still exists
- **AND** `~/emacs-workspaces/myproj/` and all its contents are then removed
  from the filesystem
- **AND** `myproj` is no longer in the registry
- **AND** the `myproj` tab is closed

#### Scenario: Purge can be cancelled
- **WHEN** the user invokes `workspace-purge` on `myproj`
- **AND** the user answers `no` at the confirmation prompt
- **THEN** no `:on-purge` handler runs
- **AND** no filesystem deletion occurs
- **AND** `myproj` remains in the registry with its tab

#### Scenario: Purge refuses anchored external project without confirmation
- **WHEN** workspace `myproj` exists with `:home ~/code/myproj/`
  (anchored, outside the default parent directory)
- **AND** the user invokes `workspace-purge` on `myproj` without a prefix arg
- **THEN** the command signals a `user-error` explaining the safeguard
- **AND** no `yes-or-no-p` prompt is shown
- **AND** no `:on-purge` handler runs
- **AND** no filesystem changes occur
- **AND** the user is directed to use a prefix arg to override

#### Scenario: Purge refuses the default parent itself without a prefix arg
- **WHEN** a workspace is anchored at `workspaces-default-parent-directory`
  itself
- **AND** the user invokes `workspace-purge` on it without a prefix arg
- **THEN** the command signals a `user-error` explaining the safeguard
- **AND** no `yes-or-no-p` prompt is shown and nothing is deleted

#### Scenario: Scope is re-checked after the prompt
- **WHEN** workspace `myproj` has `:home ~/emacs-workspaces/myproj/`
- **AND** while the confirmation prompt is open, `~/emacs-workspaces/myproj`
  is replaced by a symbolic link to a directory outside the default parent
- **AND** the user confirms `yes`
- **THEN** the command signals a `user-error` saying the home left the
  default parent
- **AND** no `:on-purge` handler runs and nothing is deleted
- **AND** `myproj` is no longer in the registry

#### Scenario: A remote home is unregistered, never contacted
- **WHEN** workspace `remote` is registered with a remote (TRAMP) `:home`
  (loaded broken)
- **AND** the user invokes `workspace-purge` on `remote` and confirms
- **THEN** `remote` is removed from the registry
- **AND** no connection is opened, no `:on-purge` handler runs, and nothing
  is deleted

---

### Requirement: Broken home directory tolerated on restore

When the persistence file is loaded at startup, the package SHALL tolerate
workspaces whose `:home` no longer exists on disk. For each such broken
workspace the package SHALL:

1. Emit a non-fatal `*Messages*` notice naming the workspace and the
   missing path.
2. Leave the workspace in the registry in a *broken* state — present in
   `workspace--registry` but flagged such that activation is refused.
3. NOT recreate the directory.
4. NOT remove the workspace from the registry automatically.

A workspace whose `:home` is a remote (TRAMP) path SHALL likewise be loaded
into the registry in the broken state, with one warning naming the entry and
saying remote homes are not supported. The package SHALL make no filesystem
call on such a home at load time (which would open a connection at startup),
SHALL keep the entry so its layouts survive the next save, and SHALL never
restore, switch to, or otherwise contact it. One such entry SHALL NOT abort
the load of its siblings.

The package SHALL provide `workspace-re-anchor NAME PATH` (or an
equivalent re-anchoring flow accessible via interactive `workspace-purge`
or `workspace-restore`) that lets the user point a broken workspace at a
new home path. Until re-anchored or purged, attempting to switch to or
restore a broken workspace SHALL signal `user-error` with a message
explaining the broken state.

#### Scenario: Missing home directory is logged, not auto-recreated
- **WHEN** the persistence file contains workspace `myproj` with
  `:home ~/emacs-workspaces/myproj/`
- **AND** `~/emacs-workspaces/myproj/` has been deleted out-of-band
- **AND** Emacs starts up
- **THEN** a `*Messages*` notice names `myproj` and the missing path
- **AND** `myproj` appears in the registry as broken
- **AND** `~/emacs-workspaces/myproj/` is NOT recreated

#### Scenario: A remote home is loaded broken and never contacted
- **WHEN** the persistence file contains workspace `remote` with a remote
  (TRAMP) `:home`
- **AND** Emacs starts up
- **THEN** a warning names `remote` and says remote homes are not supported
- **AND** `remote` appears in the registry as broken
- **AND** no connection is opened to the remote host
- **AND** the other entries in the file are restored normally

#### Scenario: Switching to a broken workspace is refused
- **WHEN** workspace `myproj` is in the broken state
- **AND** the user invokes `workspace-switch` or `workspace-restore` and
  chooses `myproj`
- **THEN** a `user-error` is signalled naming the missing path
- **AND** the user is directed to re-anchor or purge
- **AND** no tab is created

#### Scenario: Purging a broken workspace removes only the registry entry
- **WHEN** workspace `myproj` is broken (`:home` does not exist)
- **AND** the user invokes `workspace-purge` on `myproj`
- **THEN** there is no filesystem deletion to perform (path is already
  absent)
- **AND** the registry entry is removed
- **AND** the persistence file is flushed
