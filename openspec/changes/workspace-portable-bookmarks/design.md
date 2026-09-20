## Context

Emacs bookmark records store absolute paths. In this configuration every
workspace holds its own git worktree of a repository, so a bookmark set in one
workspace resolves into that workspace's checkout forever — silently opening the
wrong branch's copy of a file, or nothing at all once the workspace is purged.

Current state, verified on this machine (2026-09-20):

- Dev sets `bookmark-default-file` to `state/bookmarks/bookmarks` under
  `jf/emacs-dir`; production (`~/.emacs.d`, tag `v0.2.0-rc2`) predates that and
  writes `runtime/bookmarks`. Each Emacs install keeps its own list.
- `bookmark-save-flag` is `1`: the whole file is rewritten on every change.
- The `emacs` repository has **seven** live checkouts: `~/emacs` (main worktree),
  `~/.emacs.d` (locked production worktree), three legacy `~/emacs-*` feature
  worktrees, and two workspace-owned worktrees under `~/emacs-workspaces/`.
- `~/src` holds 51 depth-1 repositories, 42 with a remote and 9 without.
- `magit-repository-directories` is unset, so the git-worktrees candidate seam
  has in practice been projectile-only.

Three assumptions were verified in `emacs -Q --batch` before this design:

1. Extra properties can be added to a bookmark record and stock Emacs writes
   them to the file.
2. They survive a save/reload round trip; no custom handler is needed to read
   the file.
3. A modified *copy* of a record can be handed to `bookmark-jump`: it opened a
   different file at the recorded position while the stored record's `filename`
   remained unchanged.

Constraints from the repository:

- Literate config: edit `.org`, tangle to `.el` with `./bin/tangle-org.sh`.
- One-way dependency rule: workspaces core names no consumer. This capability is
  a consumer — it may read the workspace registry and call git, and nothing in
  workspaces core may name it.
- The `workspace-git-worktrees` module hard-depends on magit for worktree
  operations. This capability must not inherit that dependency.

## Goals / Non-Goals

**Goals:**

- A bookmark resolves to the current workspace's checkout of the repository it
  belongs to.
- No silent wrong-file opens: every non-resolvable case is reported or offered as
  an explicit choice.
- Zero configuration; identical behaviour on any machine regardless of
  registration state.
- Purely additive: stock Emacs still reads the bookmark file, and pre-existing
  bookmarks keep working.

**Non-Goals:**

- Cross-machine sharing and the storage decision it requires (slice 1b).
- Entry-point bookmarks that restore a workspace at a named layout (1c).
- Portable layouts (2) and workspace templates (3).
- Records with nested records or window trees; path properties other than the
  record's single location (Tier 2/3).
- Any UI showing which bookmarks are portable.
- Replacing projectile's other uses in this config (only the repo-source
  ordering changes here).

## Decisions

### D1: Two advice seams, nothing else

`bookmark-make-record` gets `:filter-return` advice that stamps coordinates.
`bookmark-handle-bookmark` (`bookmark.el:1339`) gets advice that substitutes a
resolved copy.

Why `bookmark-handle-bookmark`: it is called by `bookmark--jump-via`, which is
the shared path behind `bookmark-jump`, `bookmark-jump-other-window` and
completion front-ends such as `consult-bookmark`. Advising `bookmark-jump`
instead would miss the other entry points.

*Alternatives considered.* A custom bookmark handler: rejected — it would
*replace* the dired/org/pdf handler and we would have to reimplement their
restoration behaviour. Advising `find-file`: rejected — far too broad a
surface for a bookmark-specific concern.

### D2: Resolve on a copy; never mutate the stored record

`bookmark-save-flag` is `1`, so an in-place rewrite would persist the resolved
path and destroy the absolute fallback that makes every degraded outcome safe.
The advice copies the record, substitutes the location, and passes the copy on.

Verified in batch: after jumping a rewritten copy, the stored record's
`filename` was unchanged.

### D3: Identity is the normalized remote URL

`git@github.com:moogah/emacs.git` and `https://github.com/moogah/emacs` must
yield one identity: strip scheme and user, drop a trailing `.git`, drop a
trailing slash, lowercase the host. The repository basename is for display only
— basenames collide, and main-checkout paths differ between machines.

`origin` is preferred when several remotes exist. A repository with **no**
remote produces no identity and its records stay unstamped (a path-based
identity would be meaningless elsewhere); this affects 9 of 51 repos in `~/src`,
so the unstamped path is a normal case, not an edge case.

*Alternative considered.* Identity by basename: rejected (collisions).
Identity by main-worktree path: rejected (machine-specific).

### D4: Tier 1 records only — one location per record

A record is a name plus an open bag of properties; `filename` is a convention,
not a schema. This slice stamps and resolves only records whose location is a
single file or directory.

The bookmark file on this machine already shows why the boundary matters: four
`tab-bookmark` records store `(filename . "3 buffers")` — a *label* — alongside
nested bookmark records, window trees, and `#N=`/`#N#` shared structure that is
in places genuinely circular. Rewriting those naively would corrupt them; a
recursive walk would need cycle handling. They are left unstamped and behave
exactly as today.

So the stamp step must answer "is this value even a path?" before treating it as
one, and must decline anything else.

### D5: Warm-path (git-derived) discovery only

Everything needed comes from git, starting from checkouts already in view:

| Question | Command |
|---|---|
| Which repository is this? | `git config --get remote.origin.url` |
| Where is its canonical checkout? | `git rev-parse --git-common-dir` |
| What other checkouts exist? | `git worktree list --porcelain` |
| Where is this checkout's root? | `git rev-parse --show-toplevel` |

Checkouts in view: the current workspace's worktrees, the worktrees of other
registered workspaces (from the registry), and the configuration repository the
running Emacs was loaded from.

No directory scanning, no cache, no machine-local configuration in this slice.
The scanning cold path (declared clone roots, replacing projectile for
*creating* worktrees) belongs to the git-worktrees candidate seam and is deferred.

Git is invoked as a subprocess; **magit is not used**, so bookmark resolution
does not inherit git-worktrees' magit dependency. Note the overlap with
`jf/workspace--worktree-children`, which does the same enumeration *via*
`magit-list-worktrees`; unifying them is left as a later refactor rather than
adding a magit dependency here.

### D6: Classification, and the canonical-checkout safety rule

Each discovered checkout is classified:

- **canonical** — the repository's main worktree. Cheap test: `.git` is a
  directory, not a file (on this machine `~/.emacs.d/.git` is a 54-byte file).
- **workspace-owned** — its root lies inside a registered workspace's `:home`.
- **other** — a linked worktree belonging to no workspace (the legacy
  `~/emacs-*` checkouts).

A workspace-owned worktree is never offered as the canonical checkout: doing so
would open one workspace's files while the user is in another, which is the
defect being fixed. `other` checkouts are likewise never canonical — they are
stale branches and would make the read-only view a lottery.

### D7: Outcomes are a closed set, evaluated in order

`resolved`, `not-on-branch`, `missing-repo`, `no-clone`, `non-portable` — as
specified. Mirrors the existing closed-set discipline in
`workspace--valid-reincarnation-steps`, where adding a step is a deliberate
change rather than an accident.

`not-on-branch` exists because Emacs would otherwise visit a nonexistent path as
a new empty buffer — a quiet wrong-file trap of its own. The `missing-repo`
read-only view opens with `view-mode` and a visible marker, so a quick lookup
cannot become an unnoticed edit of the canonical checkout.

### D8: Off-workspace jumps restore, they do not merely switch

Candidates come from the registry (`workspace--registered-names`), not from live
tabs, and the chosen workspace is materialized with `workspace-restore`, which
already creates-or-selects the tab, applies `:working-state` else `:saved-state`,
falls back to the home builder when a workspace has no saved layout, and refuses
broken workspaces with a remediation message.

This is why restore beats switch: the moment bookmarks are most useful is right
after a restart, when the target workspace has no live tab yet.

Candidate ordering puts workspaces that already hold a checkout of the
bookmark's repository first, since those resolve immediately while the others
lead straight into `missing-repo`.

### D9: Restore-then-jump replaces the selected window and is not remembered

The restored layout is painted, then the bookmark's file replaces the **selected
window**. The perturbation is deliberately **not** captured as the workspace's
`:working-state`.

Rationale: a bookmark jump is navigation, not a statement about how the
workspace should look. If it were captured, a workspace's remembered arrangement
would drift toward "whatever I last jumped to". `:saved-state` was never at risk
(autosaves never write it), but working-state drift is still noise the user did
not ask for.

Implementation note: the autosave path already consults
`workspace-anti-save-predicates`, so suppression has an established mechanism to
hook into rather than needing a new one.

*Alternatives considered.* Capture it like any other window change: rejected —
silent redefinition of "where I was". Open in a split: rejected — an extra
window on every jump, and a cramped layout on a small frame.

### D10: Module placement and load order

New module `config/workspaces/portable-bookmarks.org` →
`portable-bookmarks.el`, loaded from the package loader
`config/workspaces/workspaces.org` with a `jf/load-module` line — *not* from
`jf/enabled-modules` in `init.org`, which lists only the single entry
`("workspaces/workspaces" …)`.

Load order: after `tabs.el` (which owns `workspace--registry` and
`workspace--current-name`) and after `persistence.el` (which defines
`workspace-restore`), so it goes near the end of the loader, alongside
`workspaces-mode.el` and before `workspaces-transient.el`.

It names `bookmark`, the workspace registry, and `git`. Workspaces core does not
name it, preserving the one-way rule; the advice is installed at module load.
Symbol prefix: `workspace-bookmark-` for the few user-facing symbols and
`workspace-bookmark--` for internals, matching the `workspace--` style of the
core modules (`git-worktrees.el`'s `jf/workspace--` prefix is the older
pattern).

### D11: Testing approach

Framework: **Buttercup** (`*-spec.el`), per this repository's preference for new
suites. Location: `config/workspaces/test/`, flat — the existing suite holds 29
spec files with no subdirectories, so new files follow the
`portable-bookmarks-<topic>-spec.el` naming rather than introducing a nested
directory. Run with `./bin/run-tests.sh -d config/workspaces` or
`make test-buttercup-directory DIR=config/workspaces`.

Strategy is **both real git and mocks**, split by test level:

- **Behavioral** — build throwaway repositories and worktrees in a temp
  directory and run real `git`, matching this repository's rule of mocking only
  at the boundary with external dependencies. These cover stamping, identity
  normalization across SSH/HTTPS remotes, classification of main versus linked
  worktrees, and end-to-end resolution.
- **Unit** — mock the git-calling helpers per-function with `cl-letf` so
  outcomes that are awkward to stage for real (`no-clone`, a failing git
  invocation, malformed coordinates) can be forced deterministically. Mocks are
  scoped to the function under test, never global.

Fixtures needed: a temp-repo builder (init a repo, set a remote, add a worktree),
a fake workspace registry entry pointing at a temp home, and a temp
`bookmark-default-file` so no test touches the real bookmark list. Round-trip
tests write and re-read a bookmark file to prove stamped records survive.

Scenario mapping: each `#### Scenario:` in
`specs/portable-bookmarks/spec.md` maps to one `it` block; the spec's six
requirement groups map to six `describe` blocks.

## Risks / Trade-offs

- **Advice on core bookmark functions** → Both seams are narrow and additive: the
  stamp advice returns the record unchanged when it cannot stamp, and the jump
  advice passes the record through untouched when there are no coordinates. A
  failure in either degrades to today's behaviour instead of breaking jumps.

- **Per-jump git subprocess latency** → The warm set is small (seven checkouts
  for the largest repo here) and a jump is an interactive action. No cache in
  this slice; if latency is felt, caching is an additive follow-up.

- **`bookmark-handle-bookmark` is not a documented extension point** → Its
  contract could change across Emacs versions. Mitigated by keeping the advice
  minimal (substitute a record, call through) and by behavioral tests that
  exercise the real jump path rather than the advice in isolation.

- **Stamping adds properties other tools may not expect** → Verified that stock
  Emacs round-trips unknown properties; third-party bookmark UIs read the
  properties they know and ignore the rest.

- **Reading the workspace registry from a consumer module** → Permitted by the
  one-way rule (consumers may name workspaces; core may not name consumers), but
  it does couple this module to registry internals such as
  `workspace--registered-names`. Accepted for this slice; a published accessor
  would be the clean follow-up if 1c needs the same data.

- **Overlap with `jf/workspace--worktree-children`** → Two enumerations of the
  same fact, one magit-based and one git-based. Accepted deliberately to avoid a
  magit dependency; flagged for a later unification once the cold path lands.

- **Tests that shell out to git are slower than mocked ones** → Confined to the
  behavioral suite; unit tests stay mocked and fast.

## Migration Plan

No data migration. Existing bookmark files are read unchanged; records gain
properties the first time they are re-created, and unstamped records keep working
through `non-portable` indefinitely. Rollback is removing the module from
`jf/enabled-modules`: the advice disappears, stamped records keep their absolute
`filename`, and behaviour returns exactly to today's.

## Open Questions

- Should the two prompts be unified? "Off-workspace: pick a workspace" and
  "missing-repo: add worktree / view read-only / cancel" are arguably one
  question — *where should this open?* — and could share a single prompt whose
  candidates are workspaces holding the repository plus those actions. Deferred
  to implementation, where the shape of both prompts will be concrete.
- Should a bookmark ever be shared with teammates? A yes would settle the 1b
  storage decision by making tracked per-repo storage the point rather than an
  intrusion. Out of scope here; recorded so 1b starts from it.
