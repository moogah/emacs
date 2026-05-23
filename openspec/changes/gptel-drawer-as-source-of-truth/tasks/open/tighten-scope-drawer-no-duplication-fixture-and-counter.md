---
name: tighten-scope-drawer-no-duplication-fixture-and-counter
description: |
  The regression spec at no-duplicate-drawer-spec.el counts every
  `:PROPERTIES:` / `:END:` line buffer-wide and asserts exactly one,
  but the cycle-7 canonical session.org now carries TWO drawers (the
  file-level config drawer plus the * System Prompt heading's own
  :PROPERTIES:/:VISIBILITY: folded/:END: drawer). The test still
  passes only because the fixture `jf/gptel-test--with-scope-drawer`
  emits the pre-Addendum shape (no headings). The
  `register/invariant/scope-drawer-no-duplication` entry's prose is
  correctly qualified ("for the file-level drawer at point-min") but
  the enforcement test does not match the prose. Tighten the counter
  to a point-min-bounded scan and add a post-cycle-7-shape fixture
  variant.
change: gptel-drawer-as-source-of-truth
status: needs-review
relations:
  - discovered-from:arch-cycle-1779477564-9
---

## Files to modify

- `config/gptel/scope/test/drawer/no-duplicate-drawer-spec.el` (modify) — replace buffer-wide counters with point-min-bounded ones
- `config/gptel/scope/test/helpers-spec.el` (modify) — extend `jf/gptel-test--with-scope-drawer` with a `:include-headings` option OR add a sibling macro `jf/gptel-test--with-session-document` that emits the full canonical shape

## Why

End-of-cycle Architect finding `arch-cycle-1779477564-9` (advisory, class `invariant-gap`). The entry's prose says the invariant guards the file-level drawer at point-min, but the spec walks the whole buffer and counts every `^[ \t]*:PROPERTIES:[ \t]*$` and `^[ \t]*:END:[ \t]*$` line. With the cycle-7 canonical layout (`config/gptel/sessions/commands.org`'s `jf/gptel--session-headings-block`), a fresh session.org has two `:PROPERTIES:` blocks: the file-level config drawer and the `* System Prompt` heading drawer. The test passes only because no test fixture in the scope suite uses the new shape — the pre-Addendum fixture macro `jf/gptel-test--with-scope-drawer` emits a single drawer plus a bare `#+begin_user`/`#+end_user` block.

The gap is silent: a future regression in `jf/gptel-scope--write-pattern-to-drawer` that broke the file-level drawer's singleton property on a post-cycle-7 session.org buffer would still be caught (the file-level drawer's count would change as a side effect of the buffer-wide count), but the test would also START failing if the fixture were updated to include `* System Prompt` — and a future maintainer who updates the fixture would mistake the failure for a regression in the scope writer rather than a test-design defect.

## Implementation steps

1. In `config/gptel/scope/test/drawer/no-duplicate-drawer-spec.el`:
   - Replace `no-duplicate-drawer-spec--count-properties-headers` and `--count-end-lines` with point-min-bounded versions: walk from `(point-min)` until the first `^:END:[ \t]*$` (the file-level drawer's terminator), counting `:PROPERTIES:` / `:END:` lines inside that span. Assert exactly one of each.
   - Alternatively keep the buffer-wide count but assert "exactly one `:PROPERTIES:` and one `:END:` line appear BEFORE the first `^\* ` heading" (which is the same invariant stated differently).
2. In `config/gptel/scope/test/helpers-spec.el`:
   - Extend `jf/gptel-test--with-scope-drawer` with an optional `:include-headings` keyword that, when truthy, additionally inserts `* System Prompt` + heading-drawer + body + `* Chat` after the file-level drawer (matching `jf/gptel--session-headings-block`'s output).
   - OR add a sibling macro `jf/gptel-test--with-session-document` that emits the full canonical shape directly.
3. Add at least one `it` body to `no-duplicate-drawer-spec.el` that uses the new fixture (post-cycle-7 shape) and runs the same multi-write scope sequence; the tightened counter must still report exactly one file-level `:PROPERTIES:`/`:END:` pair.
4. Run the scope suite end-to-end.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/drawer
./bin/run-tests.sh -d config/gptel/scope
grep -n 'count-properties-headers\|count-end-lines\|with-scope-drawer\|with-session-document' config/gptel/scope/test/drawer/no-duplicate-drawer-spec.el config/gptel/scope/test/helpers-spec.el
```

Expect: counter restricted to the file-level drawer span; at least one post-cycle-7-shape fixture in use; scope suite green.

## Context

Provenance: end-of-cycle Architect finding `arch-cycle-1779477564-9`, severity advisory. Finding file: `.orchestrator/cycles/cycle-1779477564/findings/arch-cycle-1779477564-9.md`.

Cited register entry: `interfaces.org#register-invariant-scope-drawer-no-duplication`. Related to the canonical layout reconciled at `interfaces.org#register-shape-session-document-layout` — this task closes the test-side gap that the new canonical layout silently opened.

## Observations

- Picked the **sibling-macro** option over `:include-headings` keyword:
  20+ existing call sites of `jf/gptel-test--with-scope-drawer` (across
  `expansion/`, `integration/`, `tool-wrapper/`, `validation/`,
  `drawer/`) all pass a bare `(alist body...)` positional shape; keeping
  the existing macro's signature byte-equivalent eliminated any
  cascade-risk and let the new fixture document its differing
  invariants (buffer-wide :PROPERTIES: count = 2) on its own self-tests.
- Counter tightening uses a `point-min..first :END:` line-position
  region rather than a `point-min..first ^* heading` region. Either
  matches the invariant's prose; the `:END:` form is structurally tied
  to what the invariant *names* (the drawer's terminator) rather than
  to what happens to come after (the headings). It also degrades
  gracefully on bare-drawer fixtures (`with-scope-drawer`) where no
  heading exists.
- Sanity check that the tightening actually narrowed scope: a self-test
  on the new fixture explicitly pins `buffer-wide :PROPERTIES: count =
  2` and `buffer-wide :END: count = 2`, while the new drawer-writer
  `it` body using that same fixture asserts the file-level counter
  reports 1/1. The buffer-wide vs file-level-bounded counts diverging
  on the same input is the load-bearing evidence that the counter is
  now narrower than the buffer.
- Scope suite set-based comparison: failures byte-equivalent to
  baseline (same 10 buttercup failures by name, both before and
  after). +14 passing specs (7 mine: 6 helper self-tests + 1 drawer
  it-body; the other +7 come from pre-existing dirty files in the
  worktree that are NOT my edits — see Discoveries).

## Discoveries

- **Meta-discovery (cycle-8 integrate sweep candidate): buffer-wide
  counter survived cycle-7 only because no fixture migrated to the new
  shape.** The cycle-7 reconciliation of
  `register/shape/session-document-layout` introduced a second
  `:PROPERTIES:` drawer (the `* System Prompt` heading's own) into the
  canonical document, but `register/invariant/scope-drawer-no-
  duplication`'s enforcement spec was not updated, and no fixture in
  the scope test suite started using the new shape. The spec passed
  trivially against unchanged inputs. Class of gap to sweep for at
  cycle-8 integrate: every `confirmed, load_bearing: true` register
  entry whose `enforcement_mechanism.location` is a test file should
  be checked to see whether the test's *fixture inputs* exercise the
  document shape the invariant prose claims to guard, not just the
  shape that happened to exist at the spec's authoring time. A
  shape-change in one register entry can silently invalidate the
  enforcement spec of another if their fixtures are not coupled.
- **Tightening guidance recorded as docstring + self-test.** The new
  fixture's `it "carries two :PROPERTIES: drawers buffer-wide"`
  self-test isn't just a sanity check on the fixture — it is the
  load-bearing precondition for why the sibling counters are
  point-min-bounded rather than buffer-wide. If a future maintainer
  ever changes the canonical layout to a single drawer again, that
  self-test breaks first and signals the counter scoping must be
  revisited. Recording this coupling in the docstring of
  `jf/gptel-test--with-session-document` and as an executable
  assertion lowers the chance of the same enforcement-prose-drifting-
  away-from-shape gap recurring.
- **Unrelated dirty state in the worktree at task start.** Started
  the task with the working tree containing seven modified files
  unrelated to my task (`commands.el/.org`, `persistent-agent.el/.org`,
  `session-org-creation-spec.el`, two `test-report.txt` files). None
  were touched by my edits; I only staged my own files. Worth flagging
  to the orchestrator that worktree bootstrap may be leaving non-empty
  diffs — could pollute later tasks if they don't filter their `git
  add`.
- **`git stash` is not safe in this worktree workflow.** I attempted
  to baseline the scope suite by stashing my work-in-progress, running
  the tests, and popping. The stash silently swallowed my unstaged
  helpers-spec.el and no-duplicate-drawer-spec.el changes, and `git
  stash pop` did not restore them (no merge-conflict, no error — they
  simply weren't in the popped state). I had to redo both edits.
  Likely a stash-list collision (multiple stale stashes from other
  branches present), but worth flagging: cycle-8 integrator should
  avoid stash-based baselines in this workflow, and prefer
  committing-then-resetting (or a separate worktree) for before/after
  comparisons.
