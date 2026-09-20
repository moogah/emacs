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
