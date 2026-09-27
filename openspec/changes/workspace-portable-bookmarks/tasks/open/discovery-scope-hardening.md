---
name: discovery-scope-hardening
description: Error barrier around git-worktrees' call into the public discovery scope; keep the add-worktree re-resolve out of the discovery memo; refresh two stale docstrings
change: workspace-portable-bookmarks
status: needs_review
task_class: refactor
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/boundary/workspace-git-worktrees-consumer
  - register/boundary/git-derived-checkout-discovery
relations:
  - discovered-from:arch-on-touch-1790453714-47
  - discovered-from:arch-on-touch-1790453714-48
  - discovered-from:arch-on-touch-1790453714-50
  - discovered-from:disc-docs-readme-1
  - blocked-by:workspaces-file-equal-p-timestamp-race
discovered_from: [arch-on-touch-1790453714-47, arch-on-touch-1790453714-48, arch-on-touch-1790453714-50]
discovered_by: architect
discovered_class: invariant-gap
reconciled_into: null
created_at: 2026-09-27T21:00:00+00:00
---

> Follow-up created by the orchestrator during cycle-1790453714 execute from the on-touch audit of
> `discovery-memo-dead-helpers-and-picker-scope` (merge 4dec801c), `findings/on-touch-dead-helpers.md`. Advisory; runs after
> `workspaces-file-equal-p-timestamp-race` merges (both edit portable-bookmarks.org and git-worktrees.org).

## Steps
1. **(47)** `git-worktrees.org`: the call to `workspace-bookmark-call-with-discovery-memo` (git-worktrees.el:93-103 at 4dec801c) has
   no error barrier, so a present-but-signalling scope aborts the "w" command instead of warning and falling back to the typed-path
   prompt that git-worktrees.org:133 promises for every portable-bookmarks call. Give it the same barrier as the other soft calls.
   Spec: scope stubbed to signal → the command warns and reaches the prompt.
2. **(48)** `portable-bookmarks.org`: state the precondition in `workspace-bookmark-call-with-discovery-memo`'s docstring (answers are
   equal only while nothing mutates the repositories inside the scope), and make `--add-worktree-and-replan` re-resolve OUTSIDE any
   enclosing discovery memo after the worktree is added. Spec: add-worktree inside an enclosing scope still finds the new worktree.
3. **(50)** Docstrings: git-worktrees.el:62-63 ("the only portable-bookmarks symbol") and the context list in
   `workspace--integration-payload`'s docstring (integrations.org) — name `bookmark-jump`.
4. **(disc-docs-readme-1)** `portable-bookmarks.org` says the add-worktree option is withheld when magit is absent, but
   `jf/workspace--add-worktree` is defined unconditionally (only the integration's registration waits for magit), so the `fboundp`
   guard always passes and, without magit, choosing the option declines with a void-function error (nothing opens). Make the guard
   match the prose (withhold when the command cannot run, e.g. magit's worktree function not `fboundp`) and pin it with a spec; or, if
   that widens the consumer boundary, correct the prose instead and say why in `## Observations`.
5. Tangle each `.org`; commit `.org` + `.el`.

## Verification
- `./bin/run-tests.sh -d config/workspaces </dev/null` green; full suite NO-REGRESSION via compare-baseline.py.
- Each new spec fails with its production change reverted.

## Observations

- **Step 1 (47), barrier shape.** `jf/workspace--worktree-in-one-discovery-call` now hands the scope a wrapped build that
  records how far it got (`before` / `running` / `returned`). Scope signals before the build starts (broken, or a changed
  calling convention → `wrong-number-of-arguments`): one warning through `jf/workspace--worktree-warn-discovery`, and the
  build runs unscoped, so both sources still contribute. Scope signals after the build returned: one warning, and the build's
  value is used. This goes one step past the architect's sketch, which re-signalled in that case. Build itself signals: the
  error propagates unchanged and unwarned. The build runs at most once. The warning counts against the collection's
  one-warning cap.
- **Step 2 (48).** `--add-worktree-and-replan` binds `workspace-bookmark--discovery-memo` to nil around the re-resolve
  (and the plan it feeds), so it starts a fresh memo whatever encloses it. I added a file-level `(defvar
  workspace-bookmark--discovery-memo)` before the function because its real `defvar` comes ~700 lines later in the file,
  and the `let` must be dynamic even if the file is ever byte-compiled. The docstring of
  `workspace-bookmark-call-with-discovery-memo` and the "One scope for several calls" prose now state the precondition:
  every discovery inside the scope joins the memo, and FUNCTION must not create, move or remove a checkout, change a
  repository's configuration, or wait for user input. **Budget: no number moved.** In production the replan is only
  reached from the chooser, which runs outside every scope, so the enclosing memo was already nil there. The cost specs
  and the git-worktrees picker budget specs are unchanged and green.
- **Step 3 (50).** The git-worktrees docstring now says "one of the two public portable-bookmarks symbols this module
  calls". The `workspace--integration-payload` docstring lists the contexts actually passed: the three birth contexts,
  `menu-invoke`, `purge` (workspaces.org:348 passes it to the purge dispatch) and `bookmark-jump`. The task named only
  `bookmark-jump`, but `purge` was missing too, so I listed it for the same reason. Also fixed in passing, in the same
  family: portable-bookmarks.org § Boundary still said git-worktrees calls
  `workspace-bookmark-canonical-checkouts-in-view` "and nothing else". It now names the scope too and says each call is
  error-barriered.
- **Step 4 (disc-docs-readme-1): guard changed, prose kept.** The new predicate
  `workspace-bookmark--add-worktree-runnable-p` is true when `jf/workspace--add-worktree` is `fboundp` AND registered as
  some integration's `:menu` command in core's `workspace--integrations` (read under `boundp`). That registration is
  exactly what git-worktrees defers until magit loads, and it is also what makes the workspace menu offer "w", so both
  places now agree. This does not widen the git-worktrees consumer boundary: portable-bookmarks still names no magit
  symbol (the structural spec's `magit-` check still holds) and no second git-worktrees symbol. It adds one read of
  core's registry, next to the core internals the module already reads (`workspace--registry`,
  `workspace--restoring-layout-leaf`). The Boundary section now says so. The user-facing reason text is unchanged ("the
  git worktree integration is not loaded"), and it is now accurate. `--add-worktree-and-replan`'s own `fboundp` check is
  left as it was (it is only reached when the option was offered).
- **Existing specs relied on harness state.** The missing-repo specs that expect `?a` passed only because the test
  harness loads magit, so `git-worktree` is registered in the global `workspace--integrations`. integration-registry-spec
  and integration-dispatch-spec `setq` that global to nil and never restore it, so under the new guard the result would
  have depended on file order. `portable-bookmarks-missing-repo-spec--hermetic` now `let`-binds `workspace--integrations`
  to a registry built with the real `workspace-register-integration`, exactly as git-worktrees registers it. That is a
  per-spec dynamic binding, not a registry swap. `workspace--registry` isolation still goes through the fixtures macro.
  Latent issue, out of scope: those two core specs leave the global integration registry empty for every spec file that
  runs after them.
- **Revert proof (every new spec fails with its production change reverted).** I copied `config/workspaces` into three
  `mktemp -d` trees, reverted one change in each tree's `.el`, and ran the spec file from that tree (specs load `../*.el`
  relative to themselves), stdin </dev/null:
  - step1 tree (old unbarriered `--in-one-discovery-call`): all 4 new git-worktrees specs FAIL (`raised` =
    `(error "Simulated scope failure")` / `wrong-number-of-arguments`; the prompt spec; the teardown spec).
  - step2 tree (fresh-memo `let` removed): "re-resolves after the add in a fresh discovery memo…" FAILS (`(length (car
    listings))` 2, expected 3).
  - step4 tree (guard back to `fboundp` only): "omits add worktree when the command is defined but not registered…"
    FAILS (offered `(?a ?v ?c)`).
  - With the changes in place, all pass. Workspaces suite: 755 specs, 0 failed (749 before, plus 6 new).
- The step-2 spec observes the listing the re-resolve sees (a call-through wrapper around `--resolve` records
  `--checkouts-of repo`), not an outcome. As the architect's probe F found, the `resolved` arm happens to survive a
  stale listing, so an outcome-only spec would pass on the reverted code. The spec also checks that the plan is
  `call-through` into the new worktree.

## Discoveries

- discovery_id: disc-discovery-scope-hardening-1
  class: interface-drift
  description: |
    The register's pin 3 of workspace-git-worktrees-consumer says "signalling: one warning per collection and
    that source contributes nothing" and names one public portable-bookmarks symbol. The code now has two public
    symbols, and every call to them, the scope included, is fboundp-guarded AND error-barriered. A scope that fails
    before the build starts does not make a source "contribute nothing": the build runs unscoped and both sources
    contribute. Only the memo sharing is lost.
  affected_register_entry: register/boundary/workspace-git-worktrees-consumer
  recommendation: |
    Pin 3 / directionality: "MAY name exactly two portable-bookmarks symbols, the public
    workspace-bookmark-canonical-checkouts-in-view and the scope workspace-bookmark-call-with-discovery-memo; every
    call is fboundp-guarded and error-barriered. A signalling source contributes nothing (one warning per
    collection); a scope signalling before the build starts costs one warning and the sharing (the build runs
    unscoped); the build's own errors propagate; the build runs at most once."

- discovery_id: disc-discovery-scope-hardening-2
  class: invariant-gap
  description: |
    The public scope's equal-answers promise holds only while nothing mutates the repositories inside it, and a
    nested discovery joins the enclosing memo, so inside the scope EVERY discovery (resolver, workspace
    checkouts) shares it, not only the entry point. That precondition is now in the docstring and .org.
    --add-worktree-and-replan re-resolves under a fresh memo, so it no longer depends on its caller.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    cross_module_surface, on the scope: "FUNCTION must not create, move or remove a checkout, change a
    repository's configuration, or wait for user input; every discovery made while it runs shares the memo. The
    one mutate-then-ask site (--add-worktree-and-replan) binds a fresh memo itself." No cost_contract change.

- discovery_id: disc-discovery-scope-hardening-3
  class: responsibility-leakage
  description: |
    Whether the add-worktree command can run was decided by fboundp alone, but git-worktrees defines the command
    unconditionally and only its registration waits for magit. portable-bookmarks now asks core's integration
    registry whether jf/workspace--add-worktree is some integration's :menu command, which is the same
    condition the workspace menu uses. This is a new read of a core internal (workspace--integrations, boundp-guarded)
    by portable-bookmarks. It adds no magit or git-worktrees symbol.
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    Add workspace--integrations (read-only, boundp-guarded, one question: is the add-worktree command registered)
    to the core internals portable-bookmarks may name. In workspace-git-worktrees-consumer pin 1, note that the
    registration is also the availability signal portable-bookmarks reads, so registration gating and the bookmark
    offer cannot drift apart.

- discovery_id: disc-discovery-scope-hardening-4
  class: vocabulary-mismatch
  description: |
    workspace--integration-payload is called with CONTEXT `purge` (workspaces.org:348, purge dispatch) and
    `bookmark-jump` (portable-bookmarks) in addition to the birth contexts and `menu-invoke`. The shape entry's
    :context note lists only fresh / anchored-scaffolded / anchored-existing / menu-invoke.
  affected_register_entry: register/shape/workspace-integration-anchor-payload
  recommendation: |
    :context note: "a member of register/vocabulary/workspace-creation-context: birth contexts, menu-invoke,
    purge, bookmark-jump". Confirm that `purge` and `bookmark-jump` are members of that vocabulary entry.
    consumed_by could also gain portable-bookmarks (it builds the payload for the git-worktree :menu command).
