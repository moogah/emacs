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
status: ready
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
