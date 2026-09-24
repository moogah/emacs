---
name: wave3-review-fixes
description: Close wave-3 review/architect findings — fail-closed same-file identity, symlinked workspace children, and a hardened git runner
change: workspace-portable-bookmarks
status: ready
task_class: bug
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/checkout-classification
  - register/boundary/git-derived-checkout-discovery
  - register/shape/portable-bookmark-coordinates
relations:
  - blocks:resolver-outcomes
  - blocks:worktree-candidates-git-first
discovered_from: [checkout-discovery, stamp-seam]
discovered_by: [reviewer, architect]
discovered_class: invariant-gap
reconciled_into: null
---

## Why

The canonical-checkout safety rule (never offer a checkout that belongs to a
workspace) can currently fail OPEN, and the shared git runner can be steered
to the wrong repository by the process environment or by whitespace in a path.
Both are silent wrong-file risks: `register/invariant/no-silent-wrong-file-open`.

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify) + tangled `.el`
- `config/workspaces/test/portable-bookmarks-discovery-spec.el` (modify)
- `config/workspaces/test/portable-bookmarks-identity-spec.el` and/or
  `portable-bookmarks-stamp-spec.el` (modify) — runner and memo specs

## Implementation steps

1. **One same-file predicate, by identity, not by attributes.** `file-equal-p`
   compares the two paths' FULL attribute lists, timestamps included, so a
   directory modified between the two reads compares unequal. Used in the
   ownership test, that makes a workspace-owned checkout classify `canonical`
   (demonstrated: 4,817 of 20,000 `--classify` calls under a concurrent writer).
   Add one private predicate that compares device and inode numbers of the two
   paths' truenames (nil when either does not exist), and route EVERY same-file
   comparison in the module through it. No module code may call `file-equal-p`
   afterwards.

2. **Symlinked workspace children are owned.** A child of a workspace `:home`
   that is a symlink to a checkout elsewhere (`ws/A/x -> ~/src/main`) makes that
   checkout workspace A's. Today the path is resolved before the containment
   walk, so the target classifies `canonical` and is offered to other
   workspaces. A checkout is `workspace-owned` if it is inside a home OR it is
   the same file (step 1) as a child of a home. Ownership is still tested before
   structure.

3. **Harden the shared runner `workspace-bookmark--git`:**
   - Remove the repository-locating variables from the subprocess environment:
     at least `GIT_DIR`, `GIT_WORK_TREE`, `GIT_COMMON_DIR`, `GIT_INDEX_FILE`,
     `GIT_OBJECT_DIRECTORY`, `GIT_ALTERNATE_OBJECT_DIRECTORIES`,
     `GIT_NAMESPACE`, `GIT_CEILING_DIRECTORIES`, `GIT_DISCOVERY_ACROSS_FILESYSTEM`.
     `default-directory` must be the only thing that decides which repository
     git answers about.
   - Strip only git's trailing line terminator, not surrounding whitespace, so
     a path that begins or ends with a space survives intact.
   - A DIR argument that does not exist returns nil. It must not fall back to
     querying the parent directory; only an EXISTING file resolves to its
     directory.

4. **Absolute common dir everywhere.** Every `rev-parse --git-common-dir` call
   passes `--path-format=absolute`. The identity memo's key is currently
   computed from a relative answer resolved against a symlinked path, so it
   becomes nil and never invalidates (demonstrated: stale identity stamped
   after `git remote set-url`).

5. **Specs**, each demonstrated to FAIL without its fix:
   - ownership holds under a concurrent writer in the home (a bounded loop is
     fine; assert zero `canonical` results);
   - a symlinked workspace child makes its target `workspace-owned`, and
     `--canonical-for-identity` never returns it;
   - with `GIT_DIR` / `GIT_WORK_TREE` set in `process-environment`, identity,
     stamping and `--workspace-checkout` still answer about the right repo;
   - a checkout whose directory name ends in a space gets its own identity,
     not the enclosing repository's;
   - a missing DIR returns nil;
   - stamping through a symlink to a repo subdirectory picks up a changed
     remote (memo invalidates).

6. Tangle and validate.

## Out of scope
- Jump-path cost / grouping discovery by common dir (reviewer advisory A2) —
  tracked separately.
- The "immediate children of :home" model gaps (reviewer spec-signal) and the
  `bookmark-relocate` stale-stamp rule — pending user decisions.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` all green.
- `grep -n "file-equal-p" config/workspaces/portable-bookmarks.el` — no calls
  (docstring mentions explaining why it is avoided are fine).
- `grep -n "git-common-dir" config/workspaces/portable-bookmarks.el` — every
  call site also passes `--path-format=absolute`.

## Context
- `.orchestrator/cycles/cycle-1789935943/reviews/checkout-discovery.md` (B1, B2, A1)
- `.orchestrator/cycles/cycle-1789935943/reviews/stamp-seam.md` (A1, A2)
- `.orchestrator/cycles/cycle-1789935943/findings/on-touch-checkout-discovery.md` (F2, F3)
