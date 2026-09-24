---
name: repo-identity
description: Derive a normalized repo identity from a checkout's remote URL, with no remote meaning no identity
change: workspace-portable-bookmarks
status: blocked
task_class: feature
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/boundary/git-derived-checkout-discovery
  - register/shape/portable-bookmark-coordinates
relations:
  - blocked-by:module-scaffold-and-fixtures
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify)
- `config/workspaces/portable-bookmarks.el` (tangled)
- `config/workspaces/test/portable-bookmarks-identity-spec.el` (new)

## Implementation steps

1. Add an internal helper that runs a git command in a directory and returns
   trimmed stdout, or nil on non-zero exit. Use `process-file` (not
   `shell-command-to-string`) with `default-directory` bound to the target
   directory, so no shell quoting is involved — directory names in this
   environment contain spaces (e.g. `~/src/Archived Repos/`).
   Never signal on git failure: return nil. Callers map nil to a degraded
   outcome.

2. Add `workspace-bookmark--repo-root (dir)` → `git rev-parse --show-toplevel`,
   returning an expanded directory name or nil when `dir` is in no repository.

3. Add `workspace-bookmark--remote-url (dir)`:
   - `git config --get remote.origin.url` first;
   - if that is nil, fall back to the first entry of `git remote` and read that
     remote's URL, so a repo whose only remote is named something else still
     works;
   - return nil when the repository has no remote at all.

4. Add `workspace-bookmark--normalize-identity (url)` returning a string such
   that the SSH and HTTPS forms of one repository normalize equal:
   - strip a leading `scheme://` and any `user@`;
   - convert the SSH `host:path` colon separator to `/`;
   - drop a trailing `.git` and any trailing slash;
   - downcase the host portion only (path case is significant on many forges);
   - return nil for input that does not yield a `host/path` shape.
   Examples that MUST agree: `git@github.com:moogah/emacs.git`,
   `https://github.com/moogah/emacs`, `ssh://git@github.com/moogah/emacs.git`
   → all `github.com/moogah/emacs`.

5. Add `workspace-bookmark--identity (dir)` composing the above: repo root →
   remote URL → normalized identity, nil at any failure.

6. Write `portable-bookmarks-identity-spec.el`:
   - **Behavioral** (real git, via the fixtures): a temp repo with an SSH remote
     and one with the equivalent HTTPS remote yield the same identity; a repo
     with no remote yields nil; a directory outside any repo yields nil; a repo
     whose only remote is named `upstream` still yields an identity.
   - **Unit** (mock the git-runner with `cl-letf` scoped to the function under
     test): normalization table-driven cases including a URL with a port, a
     trailing slash, an uppercase host, and a malformed string returning nil.

7. Tangle and validate.

## Design rationale

Identity must be the normalized remote URL because the alternatives fail across
machines: basenames collide (design.md D3), and main-checkout paths differ per
machine (`~/code/x` vs `~/src/x`). Normalizing SSH and HTTPS to one form matters
because the same repository is commonly cloned both ways on different machines —
if they disagreed, a bookmark would silently be unresolvable on the other
machine.

A repository with no remote deliberately yields **no** identity: there is nothing
portable to name it by, and a path-based identity would be meaningless elsewhere.
On this machine 9 of 51 repositories under `~/src` have no remote, so this is a
normal case that must degrade cleanly, not an edge case.

Git failures return nil rather than signalling, because these helpers run inside
bookmark creation and bookmark jumps: an error escaping into the bookmark
machinery would break a core Emacs interaction for a peripheral feature.

`process-file` avoids a shell entirely, which matters given directory names with
spaces in this environment.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — identity spec passes, existing
  specs unaffected.
- `grep -n "shell-command-to-string" config/workspaces/portable-bookmarks.el`
  returns nothing (no shell involvement).
- Manual check in the running config: evaluating the identity helper in a buffer
  visiting a file in this worktree returns `github.com/moogah/emacs`.

## Context
- specs/portable-bookmarks/spec.md § 'Portable coordinates stamped at bookmark creation'
- design.md § D3 'Identity is the normalized remote URL'
- design.md § D5 'Warm-path (git-derived) discovery only'

## Observations

- **The normalization rule set as written is wrong on ports, and the error is in
  the step ORDER, not in a missing case.** The task body (step 4) and
  `register/boundary/git-derived-checkout-discovery` both say "strip a leading
  `scheme://` and any `user@`" and *then* "convert the SSH `host:path` colon
  separator to `/`". Applied in that order,
  `ssh://git@example.com:2222/team/repo.git` becomes `example.com/2222/team/repo`
  and no longer matches `https://example.com/team/repo.git` — the exact
  disagreement the rule exists to prevent. The reason is that git's colon means
  two different things and only the scheme distinguishes them: with a scheme the
  URL is `scheme://[userinfo@]host[:PORT]/path`, and without one git reads the
  scp-like `[userinfo@]host:path`, which has **no port** (in
  `git@example.com:2222/team/repo.git` the `2222` is a real directory name).
  Stripping the scheme first destroys the information needed to decide.
  Implemented instead as: the scheme prefix selects the branch, the port is
  dropped when a scheme is present, and digits after the colon stay in the path
  when there is none. Both spellings are pinned by spec, including the
  asymmetry.

- **A cond fall-through between the two branches produced a WRONG identity, not
  an absent one — caught by the spec, not by reading.** The first implementation
  tried the scheme regexp and fell through to the scp regexp when it did not
  match. `https://github.com` (a host with no path) failed the scheme regexp,
  matched the scp one, and normalized to `https/github.com` — host `https`. That
  is the failure mode `register/invariant/no-silent-wrong-file-open` is about: a
  bookmark that resolves *somewhere* rather than degrading. Fixed by making the
  scheme prefix decide the branch exclusively; a URL with a scheme that does not
  parse is nil and is never retried as scp-like.

- **"nil for anything that does not yield a `host/path` shape" is not
  self-executing.** Taken loosely it accepts local and relative remotes:
  `sibling/repo` and `../sibling` both "look like" `host/path`. Those are real
  git remote values and they name a location on *this* machine, so they are no
  more portable than the absolute `filename` the record already carries. The
  implementation requires an explicit host — either a scheme or an scp colon
  before the first slash — which rejects absolute paths, relative paths and
  `file:///...` by construction. All are pinned as nil cases.

- **Userinfo stripping is a secrecy property, not only a normalization one.**
  `https://ghp_TOKEN@github.com/moogah/emacs.git` is a real remote shape, and the
  identity is *persisted to the user's bookmark file*. Keeping userinfo would
  write a credential into a file this capability has no business writing
  credentials into. Stripping covers the `user:password@` form too, and there is
  a spec asserting the token does not survive into the identity.

- **The "downcase the host only" rule is right and was kept**, on an asymmetry
  argument the entry does not state: keeping path case can at worst make two
  spellings of one repository look like two (an unstamped or non-portable
  bookmark — safe), while downcasing can make two *different* repositories on a
  case-sensitive host collide, which opens the wrong file. Recorded in the org
  prose so the next person does not "fix" it.

- `workspace-bookmark--git` coerces a non-directory DIR to its parent directory.
  Two reasons: wave 3 will hand it bookmark `filename` values, which are files,
  and `process-file` *signals* when `default-directory` does not exist — the
  guard is what makes the never-signal contract true for a path that was deleted
  after the bookmark was made. stderr is discarded (`'(t nil)`) rather than
  captured, because git writes diagnostics there on exactly the failures being
  ignored and folding them into stdout would turn a clean nil into a bogus
  string.

- **Latent, for `stamp-seam` (wave 3) to decide, not this task:** `process-file`
  is TRAMP-aware, so a bookmark on a remote file will run `git` *on the remote
  host* during bookmark creation and during every jump. That is a
  responsiveness question about when to invoke the helpers, not about the
  helpers themselves, so no `file-remote-p` short-circuit was added here. The
  register chose `process-file` in contrast to `shell-command-to-string` (no
  shell, no quoting), not in contrast to `call-process`, so this is an open
  question rather than a settled one.

- `workspace-bookmark--identity` asks for the remote at the repository ROOT
  rather than at the caller's DIR. The root is already known by then and is
  guaranteed to be an existing directory, which removes one way the second git
  call can fail differently from the first. A unit spec pins it, because it is
  invisible from the outside.

- `(require 'subr-x)` was added to the module's Requires block for
  `string-empty-p`.

- The origin-preference behavioral spec leans on `git remote` listing remotes
  alphabetically (it adds `alt` alongside `origin`, so `alt` sorts first). If a
  future git changed that ordering the spec would stop proving what it claims —
  it would still pass, just vacuously. The unit spec for the same behaviour
  (asserting only ONE git call is made when `origin` exists) does not have that
  weakness and is the real guard.

## Discoveries

- discovery_id: disc-repo-identity-1
  class: invariant-gap
  description: |
    register/boundary/git-derived-checkout-discovery states the
    normalization rules as an ordered recipe — "strip scheme:// and any
    user@; convert the SSH host:path colon to /" — and separately states
    the invariant that the SSH, HTTPS and ssh:// spellings MUST agree.
    The recipe violates the invariant. In git, a colon after the host means
    a PORT when a scheme is present and is the PATH SEPARATOR when one is
    not; stripping the scheme first makes the two indistinguishable, so
    `ssh://git@example.com:2222/team/repo.git` normalizes to
    `example.com/2222/team/repo` and disagrees with
    `https://example.com/team/repo.git`. This is precisely the port edge
    case the entry's status_note flagged as speculated.
    Implemented: the scheme prefix selects the parse; with a scheme the
    port is dropped (transport is not identity); without one, digits after
    the colon remain part of the path, which is git's own reading.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Reconcile the normalization rules to be branch-first rather than
    strip-first:
      1. If the URL begins SCHEME://, parse scheme://[userinfo@]host[:PORT]/path
         and DISCARD userinfo and PORT.
      2. Otherwise, if a colon precedes the first slash, parse the scp-like
         [userinfo@]host:path — there is NO port in this form.
      3. Otherwise nil.
    Then: downcase host only, strip a trailing slash then a trailing
    ".git", reject an empty host or empty path.
    Add the port pair to the entry's MUST-agree examples:
      ssh://git@example.com:2222/team/repo.git
      https://example.com/team/repo.git       -> example.com/team/repo
    and the contrasting scp case as an explicit non-example:
      git@example.com:2222/team/repo.git      -> example.com/2222/team/repo

- discovery_id: disc-repo-identity-2
  class: invariant-gap
  description: |
    Implementing the two branches as a cond that falls through from the
    scheme parse to the scp parse produces a WRONG identity rather than
    nil for a scheme URL with no path: `https://github.com` fails the
    scheme parse, matches the scp parse, and yields host `https`, path
    `github.com` -> `https/github.com`. Caught by the nil-cases spec, not
    by inspection. A wrong identity is materially worse than an absent one
    here: an absent identity leaves the record unstamped (stock behaviour),
    while a wrong one is the silent-wrong-file class that
    register/invariant/no-silent-wrong-file-open exists to forbid — and it
    would be PERSISTED, per register/shape/portable-bookmark-coordinates.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    State branch selection as EXCLUSIVE in the entry: the presence of
    SCHEME:// decides which parse applies, and a URL that has a scheme but
    does not parse is nil — never retried as scp-like syntax. Worth
    carrying as a named trap in the entry text, because the fall-through
    cond is the obvious implementation and it is silently wrong.

- discovery_id: disc-repo-identity-3
  class: spec-signal
  description: |
    The entry's terminating rule — "nil for anything that does not yield a
    host/path shape" — is under-specified against real git remote values.
    `sibling/repo` and `../sibling` are valid local remotes and both
    superficially present as host/path; `/srv/git/repo.git` and
    `file:///srv/git/repo.git` are valid local remotes with no host at all.
    Accepting any of them would mint a machine-local identity, which is the
    same defect the entry already rules out under NO REMOTE MEANS NO
    IDENTITY — a path-based identity is meaningless on another machine.
    Implemented as a positive requirement instead of a negative one: an
    identity requires an EXPLICIT host, evidenced by a scheme or by a colon
    before the first slash. All of the above are pinned nil by spec.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Restate the rule positively: "an identity requires an explicit host
    (a scheme, or an scp-like colon before the first slash); local
    absolute paths, relative paths and file:// URLs yield nil." Consider
    folding it under the existing NO REMOTE MEANS NO IDENTITY paragraph,
    since it is the same principle: a non-portable remote is, for this
    capability's purposes, no remote.

- discovery_id: disc-repo-identity-4
  class: invariant-gap
  description: |
    Neither cited entry says the identity must not carry credentials, but
    register/shape/portable-bookmark-coordinates makes the identity a
    PERSISTED field, written to bookmark-default-file by
    bookmark-write-file. Credential-embedded remotes
    (`https://ghp_TOKEN@github.com/...`, `https://user:pass@host/...`) are
    an ordinary shape in the wild, so the "strip any user@" rule is doing
    double duty: without it this capability would write a secret into the
    user's bookmark file as a side effect of setting a bookmark. Stripping
    covers the user:password@ form as well as the bare token@ form, and is
    pinned by spec.
  affected_register_entry: register/shape/portable-bookmark-coordinates
  recommendation: |
    Add to the identity field's description that the value NEVER contains
    userinfo, and say why — it is persisted, so userinfo would be a
    credential written to disk. Stating it on the shape entry (rather than
    only as a normalization step on the boundary entry) makes it a property
    of the stored value that a reviewer can check on the shape side.

- discovery_id: disc-repo-identity-5
  class: interface-drift
  description: |
    register/boundary/git-derived-checkout-discovery's four-questions table
    answers "which repository is this?" with exactly
    `git config --get remote.origin.url`. The task body (step 3) requires
    more: when there is no `origin`, fall back to the first remote `git
    remote` lists, so a checkout whose only remote is named `upstream`
    still has an identity. The implementation and its specs follow the task
    body; the entry as written would have a fork-with-no-origin degrade to
    non-portable. design.md D3 is consistent with the task body ("origin is
    preferred when several remotes exist"), so the entry's table is the
    outlier, not the design.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Amend the first row of the four-questions table to two steps:
      which repository is this?  git config --get remote.origin.url
                                 else `git remote` -> first entry ->
                                      git config --get remote.<first>.url
    and note that no remote at all is the nil case (already stated under
    NO REMOTE MEANS NO IDENTITY).
