---
name: consolidate-portable-bookmarks-test-fixtures
description: One fixtures-owned registry scope and one commit-a-file helper for the portable-bookmarks and git-worktrees specs; fold the five per-file git wrappers into one
change: workspace-portable-bookmarks
status: needs_review
task_class: test
on_critical_path: false
cycle: cycle-1790453714
merge_commit: ae45fdd5
cites_register_entries:
  - register/boundary/portable-bookmarks-consumer
  - register/invariant/spec-registry-isolation-via-fixtures
relations:
  - discovered-from:arch-cycle-1789935943-eoc-04
  - relates-to:.tasks/test-suite-shared-global-state-isolation
  - relates-to:.tasks/investigate-cross-suite-pollution-scope-tests
discovered_from: arch-cycle-1789935943-eoc-04
discovered_by: architect
discovered_class: duplication
created_at: 2026-09-26T14:00:00-04:00
---

## Summary

Three tasks each wrote the same test plumbing: swap `workspace--registry` for a fresh table
around the fixtures (discovery-spec.el:60-73, resolver-spec.el:63-82, git-worktrees-spec.el:285-313)
and "write a file, git add, git commit" (discovery-spec.el:380-385, resolver-spec.el:161-172,
stamp-spec.el:41-59). The shared `portable-bookmarks-fixtures.el` exists to receive them. Test
hygiene is in scope for this change by user decision (scope amendment 2026-09-20).

## Cited register entries

- `register/boundary/portable-bookmarks-consumer` (confirmed this cycle): its `test_corpus`
  names the spec files; keep the list accurate after the move.

## Implementation steps

1. Add to `config/workspaces/test/portable-bookmarks-fixtures.el`:
   `workspace-bookmark-fixtures-commit-file (repo relpath &optional contents)` and a macro
   `workspace-bookmark-fixtures-with-empty-registry` (save the registry, install a fresh
   table, run fixtures setup; restore on exit, including non-local exit).
2. Route discovery-, resolver-, stamp- and git-worktrees-spec through them; delete the local
   copies. The resolver helper's re-setting of user.email / user.name / gpgsign duplicates
   `fixtures-make-repo` (fixtures.el:151-153); drop it.
3. Fold the five per-file git wrappers (fixtures.el:115-128, birth-offer-spec.el:55,
   integration-dispatch-spec.el:58, scaffold-spec.el:45, workspace-new-anchor-spec.el:52) into
   one `config/workspaces/test/` helper, if the four pre-existing `call-process` forms can
   share it without changing their assertions; otherwise leave them and say why in
   `## Observations`.
4. All specs green, in isolation AND in the full run (the shared-registry hash is what the
   `.tasks/test-suite-shared-global-state-isolation` item is about; do not regress it).

## Verification
- `./bin/run-tests.sh -d config/workspaces` all green; full suite = baseline set.
- `grep -n 'setq[^)]*workspace--registry\|saved-registry\|(workspace--registry ' config/workspaces/test/*-spec.el` returns zero hits (validator 1 of `register/invariant/spec-registry-isolation-via-fixtures`; the earlier `grep -c` form could never pass — 20 core specs `clrhash` the table and dozens read it; finding arch-cycle-1790453714-plan-02).

## Out-of-scope
Promoting the two `.tasks/` test-hygiene items (a PM/user decision, ask in the cycle-1 digest).
Any production code.

## Context
- `.orchestrator/cycles/cycle-1789935943/findings/end-of-cycle-1789935943.md` § Finding 4
- meta-discovery M7; process note on the `rm -rf $TMPDIR/wsbm-*` incident

## Cycle 2 plan (cycle-1790453714)

- Cited register entry added: `register/invariant/spec-registry-isolation-via-fixtures` (NEW, speculated, load_bearing) — this task owns its enforcement; its `known_violations` lists the three swap sites above. Pressure-test the macro shape it speculates (save / fresh `equal` table / fixtures setup / restore under `unwind-protect`, including non-local exit).
- Wave 1: this task merges FIRST; `jump-seam` writes its new spec against the macro and helper names above, so keep those names.
- Line cites corrected at plan (finding arch-cycle-1790453714-plan-03); verification grep replaced (finding plan-02).
- Never bulk-delete shared temp prefixes (no `rm -rf $TMPDIR/wsbm-*`).

## Observations

- **Macro shape departs from the speculated one (evidence-driven).** The register speculated a
  body-wrapping macro: save / fresh table / fixtures setup / `unwind-protect` restore around a BODY.
  Every one of the 24 swap sites (20 `describe`s in discovery-spec, 3 in resolver-spec, 1 in
  git-worktrees-spec) is a buttercup `before-each` / `after-each` PAIR, and buttercup runs the two
  hooks as separate closures from `buttercup--run-spec` (buttercup.el:1765-1781), so no single dynamic
  extent can span the hook, the `it` body and the teardown. Restructuring ~60 `it`s to wrap their own
  bodies would have changed the tests' shape, not consolidated them. Shape implemented:
  `workspace-bookmark-fixtures-with-empty-registry` is a `describe`-level macro whose BODY is the
  per-spec fixture building (what used to follow `--setup` in each `before-each`); it expands to a
  `before-each` calling `workspace-bookmark-fixtures-setup-with-empty-registry (lambda () BODY)` and an
  `after-each` calling `workspace-bookmark-fixtures-teardown`. Primitives are public:
  `-isolate-registry` (save + fresh `equal` table, re-entrant), `-restore-registry`,
  `-live-registry` (accessor: the set-aside table while isolated, else the current one — so a spec
  can assert nothing leaked into the developer's registry without keeping a copy of its own),
  `-setup-with-empty-registry` (isolate, setup, BODY; unwinds to teardown + restore if any of them
  signals), and `-teardown` now restores the registry inside an `unwind-protect` cleanup so a failed
  `delete-directory` cannot leave the empty table behind. Restore-on-non-local-exit therefore holds at
  two layers: the primitive's own unwind (pinned by two specs), and buttercup's harness, which calls
  every hook and the spec body through `buttercup--funcall` (debugger-captured signal) so a failing
  hook or body still reaches `after-each`. The `equal` test speculation holds (tabs.el:17 uses `equal`;
  workspace names are strings). Pinned by 5 new specs under "portable-bookmarks fixtures: registry
  isolation" in portable-bookmarks-spec.el, including a `macroexpand-1` shape pin for `jump-seam`.
- **Step 3 (fold the five per-file git wrappers) NOT done; reasons.** (a) The four core wrappers'
  callers make no assertion on wrapper behaviour (bo/idsp/wna use theirs only in an identical
  `--init-repo`; `scaffold-spec--git`'s two callers, :94 `log --oneline` and :116 `status --porcelain`,
  both succeed), so assertions would not change — the condition in the task is technically met.
  (b) But the only in-tree home for a shared helper is `portable-bookmarks-fixtures.el`, and loading it
  from birth-offer / integration-dispatch / scaffold / workspace-new-anchor would make four CORE specs
  depend on a DROPPABLE consumer module's test file (`register/boundary/portable-bookmarks-consumer`
  pin 4): drop the module and four unrelated core suites stop loading. That inverts the direction the
  boundary exists to protect. (c) `scaffold-spec--git` also has a different contract (returns stdout,
  ignores exit status) — a deliberate choice of that spec, not an accident. (d) The genuine duplication
  in those files is the identical `--init-repo` triple (bo/idsp/wna), which belongs in a CORE-side
  helper file under `config/workspaces/test/` (loaded by path, as fixtures.el is) — a new file and a
  five-file core refactor outside this change's blast radius. Recommend a `.tasks/` item (or fold into
  `.tasks/test-suite-shared-global-state-isolation`). The five wrappers stand as accepted.
- **One helper beyond the two named symbols:** `workspace-bookmark-fixtures-write-file (root relpath
  &optional contents)`. stamp-spec's `--write` serves eight UNTRACKED-file cases that `commit-file`
  cannot express; without a fixtures-level write the stamp copy of the write step would have stayed.
  `commit-file` is layered on it. Kept stamp-spec's `--track` (staged-not-committed is a stamp-seam
  state, not a tracked file) and inlined its single bulk commit (300 staged files, one commit) rather
  than 300 `commit-file` commits. Deleted: discovery `--commit-file`, resolver `--commit-file` (its
  three `git config` re-sets dropped as prescribed — `make-repo` sets them and linked worktrees read the
  shared config), stamp `--write` and `--commit`, and all three `--saved-registry` / `--setup` /
  `--teardown` triples.
- **`commit-file` semantics unified upward:** every caller now gets `git add -f --` (discovery's
  `.DS_Store` cases need it; resolver's callers are indifferent), message `track RELPATH`, default
  contents `"x\n"` (discovery wrote `"tracked"`; nothing read it). No spec asserts on message,
  contents or ignore status other than the `.DS_Store` ones, which are exercised.
- **Latent, pre-existing, out of scope:** `portable-bookmarks-discovery-spec--clone-into` clones do
  not inherit `make-repo`'s LOCAL `user.email` / `user.name` / `commit.gpgsign=false`, so the
  `commit-file` call on the clone (discovery-spec "My package in the monorepo" case) depends on the
  runner's GLOBAL git identity and signing setting — precisely what `make-repo`'s docstring says the
  fixtures must not do. The old discovery helper had the same gap. Cheapest fix: `--clone-into` sets
  the three local config keys after cloning (or `commit-file` passes them as `-c` flags).
- **Register `test_corpus` needs no edit:** the boundary entry lists portable-bookmarks-spec.el,
  portable-bookmarks-fixtures.el and git-worktrees-spec.el; all three still exist and now carry the
  isolation specs (first) and the one macro (second, third). The new symbols live where the invariant
  entry says they do.
- **Counts:** `./bin/run-tests.sh -d config/workspaces` 627 -> 636 specs (+9 new fixture specs);
  first run had two failures in the NEW specs only (this buttercup's `:to-throw` takes the expression,
  not a lambda), fixed before the runs reported below.

## Discoveries

- discovery_id: disc-consolidate-portable-bookmarks-test-fixtures-1
  class: invariant-gap
  description: |
    Validator 1 of the cited invariant, and the task's amended verification grep
    (`grep -n 'setq[^)]*workspace--registry\|saved-registry\|(workspace--registry ' config/workspaces/test/*-spec.el`),
    span EVERY `*-spec.el` under config/workspaces/test/, but the entry's own `known_violations`
    assigns transient-menu-spec.el's six `(workspace--registry registry)` `let`-bindings (:51, :60,
    :69, :80, :135, :150) to the repo-wide owner `.tasks/test-suite-shared-global-state-isolation`,
    "NOT this cycle's fixtures task". After this task the grep returns exactly those six hits and
    nothing else; it cannot reach zero without touching a file the entry places out of scope.
    The three swap sites in the entry's known_violations are gone (verified: the same grep over
    portable-bookmarks-*-spec.el, portable-bookmarks-spec.el and git-worktrees-spec.el is empty,
    and validator 2 — every remaining `workspace--registry` mention in those files is a
    gethash / maphash / remhash read — holds). The fixtures' own set-aside slot is named
    `workspace-bookmark-fixtures--live-registry` precisely so the isolation specs that inspect it
    do not read as a per-file saved copy under this grep.
  affected_register_entry: register/invariant/spec-registry-isolation-via-fixtures
  recommendation: |
    Scope validator 1 (and the task's Verification grep) to the entry's stated scope:
    `config/workspaces/test/portable-bookmarks-*-spec.el config/workspaces/test/git-worktrees-spec.el`
    plus each new spec the cited_by_tasks add — or move transient-menu-spec.el's bindings onto the
    macro under the .tasks item and keep the wide grep. Either way, record the six hits as the
    known remainder until then.

- discovery_id: disc-consolidate-portable-bookmarks-test-fixtures-2
  class: deviation
  description: |
    The speculated macro shape (BODY wrapped in save / fresh table / setup / `unwind-protect`
    restore) cannot be used from buttercup's `before-each` / `after-each` split, which is the shape
    of all 24 call sites. Implemented as a `describe`-level macro whose BODY is the per-spec
    setup, expanding to the hook pair; restore-on-every-exit is supplied by the primitive
    `workspace-bookmark-fixtures-setup-with-empty-registry` (unwinds to teardown + restore when
    fixtures-setup or BODY signals), by `workspace-bookmark-fixtures-teardown` (restores the
    registry in an `unwind-protect` cleanup), and by buttercup running `after-each` after a failing
    hook or spec body. The fresh table IS `equal` (speculation confirmed). The two named symbols
    keep their exact names and `commit-file` its exact signature; one supporting helper was added
    (`workspace-bookmark-fixtures-write-file`). Details and evidence in ## Observations.
  affected_register_entry: register/invariant/spec-registry-isolation-via-fixtures
  recommendation: |
    Reconcile `statement`: "the ONE registry-scope construct is the describe-level macro
    `workspace-bookmark-fixtures-with-empty-registry (&rest setup-body)`, expanding to a
    before-each / after-each pair over the fixtures' `-setup-with-empty-registry` and `-teardown`;
    specs use it IN PLACE OF a before-each/after-each pair, never inside an `it`." Add the
    supporting symbols (`-isolate-registry`, `-restore-registry`, `-live-registry`,
    `-setup-with-empty-registry`, `-write-file`) to the entry so jump-seam / missing-repo-choice / off-workspace-restore build
    against the real surface. Note for `jump-seam` (parallel, wave 1): the macro is used at
    describe level with the fixture-building forms as its body — see the `macroexpand-1` pin in
    portable-bookmarks-spec.el.

- discovery_id: disc-consolidate-portable-bookmarks-test-fixtures-3
  class: spec-signal
  description: |
    Validator 3 (`grep -n '"commit"' config/workspaces/test/portable-bookmarks-*-spec.el` -> zero)
    is unsatisfiable as written: five inline scenario commits legitimately remain — stamp-spec's
    merge-conflict setup (`commit --quiet -am` x2), its single 300-file bulk commit, and
    resolver-spec's two symlink commits (`ln -s` then commit) — none of which is a commit-a-file
    helper or expressible through `commit-file`. Zero commit HELPERS remain
    (`grep -n 'defun .*commit' config/workspaces/test/portable-bookmarks-*-spec.el` is empty).
  affected_register_entry: register/invariant/spec-registry-isolation-via-fixtures
  recommendation: |
    Rephrase validator 3 to "no spec defines its own commit helper":
    `grep -n 'defun .*commit\|defun .*--write' config/workspaces/test/portable-bookmarks-*-spec.el`
    -> zero hits; inline scenario-specific `git commit` calls are permitted.

- discovery_id: disc-consolidate-portable-bookmarks-test-fixtures-4
  class: duplication
  description: |
    The five per-file git wrappers (fixtures.el `process-file`; birth-offer, integration-dispatch,
    scaffold, workspace-new-anchor `call-process`) were NOT folded. The four core wrappers could
    share one helper without changing any assertion, but the only existing home is the
    portable-bookmarks fixtures file, and loading it from four CORE specs would couple them to a
    DROPPABLE consumer module (boundary pin 4). The real duplicate is the identical `--init-repo`
    in three of them, which wants a core-side test helper file. Reasons in ## Observations.
  affected_register_entry: register/invariant/spec-registry-isolation-via-fixtures
  recommendation: |
    Per the entry's own clause ("if not, the task says why in ## Observations and this entry
    records them as accepted"): record the five wrappers as accepted; file a `.tasks/` item for a
    core-side `config/workspaces/test/` git helper (wrapper + init-repo) that fixtures.el could
    later delegate to, keeping the core->consumer direction intact.
