---
name: resolver-outcomes
description: Implement the closed set of resolution outcomes as a pure decision function over coordinates plus workspace context
change: workspace-portable-bookmarks
status: ready
task_class: feature
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/bookmark-resolution-outcome
  - register/invariant/no-silent-wrong-file-open
relations:
  - blocked-by:checkout-discovery
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify)
- `config/workspaces/portable-bookmarks.el` (tangled)
- `config/workspaces/test/portable-bookmarks-resolver-spec.el` (new)

## Implementation steps

1. Declare the closed set as a defconst, mirroring the existing
   `workspace--valid-reincarnation-steps` pattern in `layouts.el`, plus a
   predicate:
   ```elisp
   (defconst workspace-bookmark--valid-outcomes
     '(resolved not-on-branch missing-repo no-clone non-portable))
   ```
   Adding a sixth outcome must then be a deliberate two-place change (the
   defconst and the resolver), not an accident.

2. Add `workspace-bookmark--resolve (record workspace-name)` returning a plist
   `(:outcome SYMBOL :path PATH-OR-NIL :identity ID :relpath REL)`. Evaluation
   order, first match wins:
   - no coordinates on the record → `non-portable`, `:path` = the record's stored
     absolute filename.
   - WORKSPACE-NAME holds a checkout of the identity
     (`--workspace-checkout`), and the relpath exists inside it → `resolved`
     with that absolute path.
   - WORKSPACE-NAME holds a checkout but the relpath does not exist there →
     `not-on-branch`, `:path` nil.
   - no checkout in that workspace, but a canonical checkout is in view
     (`--canonical-for-identity`) → `missing-repo`, `:path` = the canonical
     checkout's copy of the relpath (the *candidate* for a read-only view; the
     caller decides whether to use it).
   - nothing in view → `no-clone`, `:path` nil.

3. The resolver must be a decision function only: it performs git queries through
   the discovery helpers but MUST NOT open buffers, prompt, or message. That
   keeps it unit-testable and keeps the interactive policy in the seam and prompt
   tasks.

4. Any error raised by a discovery helper must be caught and mapped to
   `no-clone` (when an identity is known) or `non-portable` (when it is not),
   never propagated. A jump must not break because git misbehaved.

5. Write `portable-bookmarks-resolver-spec.el`, one `describe` per outcome:
   - **Behavioral** (real git + fixtures): build a repo, add two worktrees, put
     one inside a registered fake workspace's `:home`; assert `resolved` for a
     file present there; delete that file and assert `not-on-branch` with
     `:path` nil; register a second workspace with no checkout and assert
     `missing-repo` with a `:path` under the canonical checkout.
   - **Unit** (mock discovery helpers with `cl-letf`, scoped to the function
     under test): `no-clone` when nothing is in view; `non-portable` for a record
     with no coordinates; a discovery helper that signals produces `no-clone`
     rather than propagating; a malformed relpath produces a degraded outcome.
   - **Unit**: assert every returned `:outcome` satisfies the closed-set
     predicate — the guard against a stray symbol.

6. Tangle and validate.

## Design rationale

The outcome set exists so that no code path can silently open a file the user did
not ask for. `not-on-branch` is the subtle one: Emacs would otherwise visit a
nonexistent path as a fresh empty buffer, which is its own quiet wrong-file trap
— you would start typing into a file that does not exist on this branch.

`missing-repo` returns a candidate path but does not act on it: the read-only
view is a choice the user makes, never a silent fallback to the main checkout.
Editing `main` unnoticed is exactly what worktrees exist to prevent, so the
canonical checkout is never opened without an explicit decision.

Keeping the resolver free of I/O and prompts is what makes the awkward cases
testable. Forcing `no-clone` or a failing git invocation against real repositories
is fiddly; against a mocked helper it is one `cl-letf`. That is why the test
strategy is split: real git for the behavioural paths, mocks for the edges.

Errors map to degraded outcomes because this code runs inside a core Emacs
interaction. A peripheral feature must not be able to break bookmark jumps.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — resolver spec passes, every
  outcome in the closed set covered by at least one test.
- `grep -n "find-file\|switch-to-buffer\|completing-read\|message"
  config/workspaces/portable-bookmarks.el` shows no such call inside the
  resolver function body (policy belongs to the seam/prompt tasks).

## Context
- specs/portable-bookmarks/spec.md § 'Closed set of resolution outcomes'
- design.md § D7 'Outcomes are a closed set, evaluated in order'
- design.md § D11 'Testing approach'

## Amendments (2026-09-26, after waves 3–4; these SUPERSEDE the steps above where they conflict)

A. **Six outcomes, not five (user decision, ask 13).** Add `ambiguous` to the
   closed set: `(resolved not-on-branch missing-repo no-clone non-portable
   ambiguous)`. It means that discovery found MORE THAN ONE candidate and will
   not choose. `:path` is nil, and a new `:candidates` key lists them. The
   meaning matches `not-on-branch`: it MUST be reported and NOTHING is
   opened. The plist becomes `(:outcome :path :identity :relpath :candidates)`,
   with `:candidates` nil except for `ambiguous`.
B. **Use the PLURAL discovery functions, so ambiguity is never lost.** They are
   `workspace-bookmark--workspace-checkouts (identity workspace-name)` and
   `workspace-bookmark--canonical-checkouts-for-identity (identity)`. Order, first match wins:
   1. no coordinates, fewer than all three stamped properties, or a format
      version this module does not know → `non-portable`, with `:path` = the
      stored absolute filename. (The stamp is ALL THREE OR NONE: repo, relpath,
      format. Format version 1 is the only one known.)
   2. the workspace holds exactly ONE checkout of the identity: relpath exists → `resolved`;
      relpath missing → `not-on-branch`.
   3. the workspace holds TWO OR MORE → `ambiguous` (`:candidates` = those checkouts).
   4. the workspace holds none, and exactly ONE canonical checkout is in view →
      `missing-repo`, with `:path` = the candidate under it.
   5. the workspace holds none, and TWO OR MORE canonical checkouts → `ambiguous`.
   6. nothing in view → `no-clone`.
C. **Relpath forms.** The repository root is stored as `"./"`
   (`workspace-bookmark--root-relpath`). Directory relpaths come from
   `git rev-parse --show-prefix`, with a trailing slash. A relpath may begin or
   end with whitespace and is stored verbatim. Re-anchor each form correctly,
   and never let a relpath escape its checkout (for example `..`, or an absolute
   path in a hand-edited record) → degrade to `non-portable`.
D. **Stale stamps are a documented limitation, NOT checked here (user decision,
   ask 5).** `bookmark-relocate` changes `filename` without re-stamping. Do not
   add a consistency check. Say so in the org prose, and point to
   `.tasks/portable-bookmarks-stale-stamp-detection.md`.
E. **The ONLY outcome that uses the stored absolute filename is `non-portable`**
   (`register/invariant/no-silent-wrong-file-open`). `missing-repo`'s `:path` is a
   candidate for the caller to offer, never something the resolver acts on.
F. **Placement.** New `* Resolution` section in portable-bookmarks.org,
   between `* Checkout discovery` and `* Advice installation`. Seam 2 stays
   pass-through; wiring it is `jump-seam`, deferred to cycle 2.
