---
name: module-scaffold-and-fixtures
description: Create the portable-bookmarks module skeleton, load it from the package loader, and add Buttercup test fixtures
change: workspace-portable-bookmarks
status: ready
task_class: module-load
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/boundary/portable-bookmarks-consumer
relations: []
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (new)
- `config/workspaces/portable-bookmarks.el` (new, tangled — commit alongside the `.org`)
- `config/workspaces/workspaces.org` (modify — add the loader line)
- `config/workspaces/workspaces.el` (tangled)
- `config/workspaces/test/portable-bookmarks-fixtures.el` (new)

## Implementation steps

1. Create `config/workspaces/portable-bookmarks.org` with the standard literate
   headers:
   ```org
   #+title: Workspaces — Portable Bookmarks
   #+property: header-args:emacs-lisp :tangle portable-bookmarks.el
   #+auto_tangle: y
   ```
   Add an Overview section stating the module's boundary: it is a *consumer*
   module — it may name `bookmark`, the workspace registry and `git`, and
   workspaces core must never name it. It must NOT require `magit`.

2. Header block (`:comments no`):
   ```elisp
   ;;; portable-bookmarks.el --- Workspace-relative bookmark resolution -*- lexical-binding: t; -*-
   ```
   Then a requires block: `(require 'cl-lib)` and `(require 'bookmark)`.

3. Add a customization group and two defcustoms/defvars for the property names
   used to stamp records, so the stamped keys are named in exactly one place:
   - the identity property (suggested symbol: `workspace-bookmark-repo`)
   - the repo-relative path property (suggested: `workspace-bookmark-relpath`)
   Use the `workspace-bookmark-` prefix for public symbols and
   `workspace-bookmark--` for internals.

4. Add a stub advice-installation section with the two seams named but not yet
   implemented (later tasks fill them in), plus matching removal so the module
   can be reloaded during development without stacking advice:
   - `bookmark-make-record` — `:filter-return`
   - `bookmark-handle-bookmark` — around/filter-args
   Keep the stubs pass-through: return the record unchanged.

5. End with `(provide 'workspace-portable-bookmarks)` and the closing
   `;;; portable-bookmarks.el ends here` comment, matching `tabs.el`.

6. Register the module in the package loader `config/workspaces/workspaces.org`
   (NOT in `init.org` — `init.org` lists only `("workspaces/workspaces" …)`).
   Add, after `workspaces-mode.el` and before `workspaces-transient.el`:
   ```elisp
   (jf/load-module (expand-file-name "config/workspaces/portable-bookmarks.el" jf/emacs-dir))
   ```
   Load order matters: the module reads `workspace--registry` and
   `workspace--current-name` (from `tabs.el`) and calls `workspace-restore`
   (from `persistence.el`), so it must load after both.

7. Create `config/workspaces/test/portable-bookmarks-fixtures.el` providing
   Buttercup helpers the later tasks all use. Tests in this repo are flat in
   `config/workspaces/test/` (29 existing `*-spec.el` files, no subdirectories).
   Fixtures needed:
   - **temp repo builder** — `make-temp-file` a directory, run `git init`, set
     `user.email`/`user.name` locally (batch git has no global identity), commit
     one file, and optionally `git remote add origin <url>` with a caller-supplied
     URL so SSH/HTTPS normalization can be exercised.
   - **temp worktree builder** — given a temp repo, `git worktree add` a child at
     a given path on a new branch.
   - **fake workspace registry entry** — put a plist with a `:home` pointing at a
     temp directory into `workspace--registry`, and a teardown that removes it.
     Check `data-model.el` for the accessor used to build the plist rather than
     writing the plist shape by hand.
   - **temp bookmark file** — bind `bookmark-default-file` to a temp path and
     `bookmark-alist` to nil so no test can touch the real bookmark list.
   - a teardown helper that deletes every temp directory it created.
   End with `(provide 'portable-bookmarks-fixtures)`.

8. Add a smoke spec (`config/workspaces/test/portable-bookmarks-spec.el`) that
   loads the module and asserts the feature is provided and that the fixtures
   can build a repo with a worktree. This proves the harness before any
   behaviour exists.

9. Tangle and validate both org files:
   `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` and
   `./bin/tangle-org.sh config/workspaces/workspaces.org`.

## Design rationale

The module is a consumer of workspaces, not part of its core: the package's
one-way dependency rule says core names no consumer, while consumers may name
core. Placing it in `config/workspaces/` alongside `git-worktrees.el` follows the
precedent already set for consumer modules living inside the package directory.

It must not require `magit`. The existing `jf/workspace--worktree-children`
enumerates worktrees via `magit-list-worktrees`, and reusing it would make
bookmark resolution inherit git-worktrees' hard magit dependency. This module
calls `git` as a subprocess instead; the duplication is deliberate and flagged in
design.md for later unification.

Stamped property names live in one place because they are written into the user's
bookmark file: renaming them later would orphan existing stamps, so the
indirection is cheap insurance.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0
  (it runs `check-parens`).
- `./bin/tangle-org.sh config/workspaces/workspaces.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — the smoke spec passes and the
  existing 29 spec files still pass.
- `grep -n "portable-bookmarks" config/workspaces/workspaces.el` shows the
  loader line.
- `grep -n "magit" config/workspaces/portable-bookmarks.el` returns nothing.

## Context
- proposal.md § 'What Changes' (new module bullet)
- design.md § D10 'Module placement and load order'
- design.md § D11 'Testing approach'

## Observations

- **The loader line sits after `(require 'workspaces-mode)`, not literally
  between the two `jf/load-module` calls.** The register entry's pin 2 says
  "inserted between the existing `workspaces-mode.el` (workspaces.org:135) and
  `workspaces-transient.el` (workspaces.org:140) loader lines". There is a bare
  `(require 'workspaces-mode)` between those two lines in the source. The new
  line went *after* that require, which satisfies the intent (after tabs and
  persistence, before the transient) and keeps the existing load/require pair
  adjacent. Verified in the tangled output at `workspaces.el:62`.

- **The stubs carry no `condition-case`.** `register/boundary/bookmark-advice-seams`
  validator 3 wants "both advice bodies contain a condition-case whose handler
  returns the original argument". At scaffold stage there is no fallible work to
  guard: a `condition-case` wrapped around a bare variable reference reads as
  protection that is not there, and for seam 2 wrapping `(apply orig-fn …)` would
  be actively wrong — it would swallow errors from the real jump rather than from
  resolution. The degradation contract is stated in the org prose at both seams;
  the guards belong with the bodies that `stamp-seam` / `jump-seam` add. That
  validator should be read as applying to those tasks, not to this one.

- **The module's `defgroup` parents onto `workspaces`, which does not exist when
  the module is loaded standalone.** Specs `load` the `.el` directly without
  loading `workspaces.el`, so the parent group is absent. Emacs tolerates a
  dangling `:group` parent (it only affects the customize tree), and under the
  real test harness `init.el` loads the parent first, so the tree is correct in
  practice. Noting it because a future "customize surface" task may want to
  assert the grouping and will need the parent loaded.

- **Idempotent install relies on `advice-add`'s own de-duplication.** `add-function`
  removes an existing advice naming the same function symbol before re-adding, so
  double-install cannot stack. That is load-bearing for the reload-during-development
  requirement, so the smoke spec asserts it (install twice, remove once, expect
  gone) rather than leaving it as folklore.

- **Fixtures disable commit signing locally.** Batch `git` inherits the developer's
  global config; a global `commit.gpgsign true` would make every fixture repo
  either prompt or fail. `user.email` / `user.name` are set locally for the same
  class of reason (batch git has no identity). This is not hypothetical hardening
  — it is the difference between the suite running on a signing developer's
  machine and not.

- **Temp directories are `file-truename`'d at creation.** On macOS `make-temp-file`
  returns a `/var/folders/...` path whose real name is `/private/var/folders/...`;
  `git worktree list --porcelain` reports the real name. Without truenaming, the
  later `worktree-candidates-git-first` and `checkout-discovery` tasks would get
  paths that look right but fail `equal`. Fixing it in the fixture builder is
  cheaper than every consumer remembering to normalise.

- **The smoke spec's seam assertions are structural on purpose, and weakly
  asserted as behaviour.** It checks that advice *exists* on the two seams and
  that a record survives the stamp stub `eq`-unchanged. It does not drive
  `bookmark-jump`. `register/boundary/bookmark-advice-seams` validator 4 requires
  a behavioral spec that drives `bookmark-jump-other-window` and asserts
  resolution happened — that belongs to `jump-seam`, once there is resolution to
  observe. Reviewer should not read the current specs as discharging that
  requirement.

- **Test counts.** `./bin/run-tests.sh -d config/workspaces`: 353 → **365 specs,
  0 failed, exit 0** (12 new). Full suite: 3089 → 3101 ran; the failure set is
  byte-identical to `.orchestrator/baseline-failures-cycle-1789935943.txt`
  (9 ERT + 5 Buttercup, same names). No regression.

## Discoveries

- discovery_id: disc-module-scaffold-and-fixtures-1
  class: vocabulary-mismatch
  description: |
    The cited entry flags as SPECULATED that `(provide 'workspace-portable-bookmarks)`
    from a file named `portable-bookmarks.el` is "a deliberate mismatch worth
    confirming rather than assuming". It is not a mismatch at all — it is the
    package's uniform convention. Every module in `config/workspaces/` provides a
    feature whose name differs from its basename:

      data-model.el       -> workspace-data-model
      integrations.el     -> workspace-integrations
      tabs.el             -> workspace-tabs
      persistence.el      -> workspace-persistence
      layouts.el          -> workspace-layouts
      buffer-membership.el-> workspace-buffer-membership
      home-org.el         -> workspace-home-org
      scaffold.el         -> workspace-scaffold
      git-worktrees.el    -> workspace-git-worktrees

    The basename stays short because the directory already says "workspaces";
    the feature carries the `workspace-` prefix to keep the global feature
    namespace collision-free. `workspace-portable-bookmarks` from
    `portable-bookmarks.el` is the tenth module to follow a nine-for-nine
    pattern, not a one-off. Implemented as specified; the rationale is now recorded in the
    module's own "Module Initialization" section so the next reader does not
    re-open it.

    The three apparent exceptions — `workspaces.el` -> `workspaces`,
    `workspaces-mode.el` -> `workspaces-mode`, `workspaces-transient.el` ->
    `workspaces-transient` — are the package root, the minor mode and the
    front-door transient, whose basenames are already prefixed. Nine of the
    twelve modules rename; the three that do not had nothing to rename.
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    Reconcile: drop the feature-symbol speculation from `status_note` and record
    it as a confirmed package convention instead. Suggested replacement text for
    that clause: "Feature symbol `workspace-portable-bookmarks` from
    `portable-bookmarks.el` — CONFIRMED cycle-1789935943 as the package-wide
    convention (9 of the 12 modules in config/workspaces/ provide a
    `workspace-` prefixed feature whose name differs from the file basename;
    the other 3 have basenames that are already prefixed)." Nothing else in
    the entry changes; producer_side, directionality and the four pins all held
    exactly as written.

- discovery_id: disc-module-scaffold-and-fixtures-2
  class: invariant-gap
  description: |
    `register/boundary/bookmark-advice-seams` leaves seam 2's advice COMBINATOR
    open ("the scaffold task names 'around/filter-args' as alternatives and the
    choice is not yet settled"). The choice is in fact forced, and not by
    preference — by `register/invariant/no-silent-wrong-file-open`, which states
    "Cancelling MUST open nothing AND signal nothing".

    `:filter-args` can only rewrite the argument list; it cannot decline to call
    through. Under `:filter-args`, a user who cancels at the missing-repo prompt
    would still reach `bookmark-handle-bookmark` with *some* argument, and every
    available argument is a wrong answer: the original record routes to the
    stored absolute path (prohibition 4), and there is no "open nothing" record
    to pass. Signalling to abort violates the "signal nothing" half of the same
    sentence. `:around` is the only combinator that can express a silent no-op.

    It is also the only one that can express the `not-on-branch` prohibition
    (prohibition 1: no `find-file` / `find-file-noselect` on a non-existent
    path), which likewise requires declining to call through rather than
    substituting a different argument.

    Scaffolded with `:around` (pass-through: `(apply orig-fn
    bookmark-name-or-record args)`), so `jump-seam` inherits a seam that can
    already express the full outcome set.
  affected_register_entry: register/boundary/bookmark-advice-seams
  recommendation: |
    Reconcile: settle the combinator as `:around` and record WHY, so it is not
    re-litigated as a style preference. Suggested addition under `purpose`,
    SEAM 2: "COMBINATOR: `:around`, forced by
    register/invariant/no-silent-wrong-file-open — `:filter-args` can substitute
    the record but cannot decline to call through, and both the cancel path
    ('open nothing AND signal nothing') and the not-on-branch path ('MUST NOT be
    visited') require declining." Then drop the corresponding speculation clause
    from `status_note`; the seam LOCATIONS were verified at plan time and held.

- discovery_id: disc-module-scaffold-and-fixtures-3
  class: responsibility-leakage
  description: |
    The registry fixture cannot reach `workspace--registry` through any published
    surface. The test-support file has to either load `tabs.el` (which drags in
    `tab-bar`, `workspace-home-org`, `workspace-scaffold` and
    `workspace-integrations`) or re-`defvar` the symbol defensively. It does the
    latter — `tabs.el`'s own `defvar` is a no-op once the symbol is bound, so both
    files observe the same hash table — but that is a fixture knowing a core
    module's storage decision, one layer worse than the production coupling the
    design already accepted.

    The design's own note ("a published accessor would be the clean follow-up if
    1c needs the same data") anticipated the production side of this. This
    discovery is the evidence that the TEST side wants it too, and wants it
    first: fixtures are where the coupling is cheapest to break and most likely
    to rot silently.

    Not acted on — out of scope for a scaffold task, and inventing an accessor
    here would put a new core symbol in front of the waves that are supposed to
    define what they need from it.
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    No change to the entry for this cycle — the `directionality.permitted` clause
    already names `workspace--registry` explicitly and the coupling is recorded as
    accepted. Note for the follow-up: when the published accessor lands, its first
    two consumers should be `workspace-bookmark--*` resolution AND
    `portable-bookmarks-fixtures.el`; retiring the defensive `defvar` in the
    fixture is the cheap test that the accessor is actually sufficient.
