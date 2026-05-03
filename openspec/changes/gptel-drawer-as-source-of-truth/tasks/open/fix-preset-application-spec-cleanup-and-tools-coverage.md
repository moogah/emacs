---
name: fix-preset-application-spec-cleanup-and-tools-coverage
description: The three new describe blocks in `preset-application-spec.el` push onto a global registry-keys list inside their `it` bodies but their `after-each` hooks never drain it, leaking entries into `jf/gptel--session-registry` for the test process lifetime. Separately, the fresh-session scenario omits the `:GPTEL_TOOLS:` registration and assertion that the brief explicitly required — the most error-prone snapshot key (list, not scalar) goes uncovered.
change: gptel-drawer-as-source-of-truth
status: needs-review
relations:
  - discovered-from:update-preset-application-spec-for-snapshot
---

## Files to modify

- `config/gptel/sessions/test/commands/preset-application-spec.el` — add registry drain to the three new describe blocks (or one outer-scope drain); add `:tools` registration + assertion in the fresh-session scenario

## Why

**Finding 1 (advisory).** The cleanup function `jf-gptel-preset-app-test--register-cleanup` pushes keys onto the global `jf-gptel-preset-app-test--registry-keys` defvar. The drain (`remhash` each key from `jf/gptel--session-registry`, then reset the list) lives in the `after-each` at line 88–91 of the original `"Drawer-driven auto-init …"` top-level describe. The three new describe blocks (around lines 343, 390, 437) each call the registration helper but their own `after-each` hooks do not include the drain. In separate-process CI runs this is invisible, but a single-process run of the sessions suite leaves stale registry entries that can produce false hits in registry-lookup assertions downstream.

**Finding 2 (advisory).** The fresh-session scenario (lines 339–361) registers the preset with only `:model` and `:temperature` and asserts only `:GPTEL_MODEL:` and `:GPTEL_TEMPERATURE:`. The brief required `:tools` in both the registration and the assertion; the register entry `drawer-text-block` lists `:GPTEL_TOOLS:` as one of the six snapshot keys. Tools is the most likely key to mis-handle (list vs. scalar serialization) and it is currently unexercised at the integration level.

## Implementation steps

1. In `config/gptel/sessions/test/commands/preset-application-spec.el`, add the registry drain to the three new describe blocks. Two viable shapes:
   - **Per-describe:** add `(after-each (dolist (key jf-gptel-preset-app-test--registry-keys) (remhash key jf/gptel--session-registry)) (setq jf-gptel-preset-app-test--registry-keys nil))` to each.
   - **Outer drain:** wrap the three describes in a single outer `describe "gptel-drawer-as-source-of-truth: full snapshot end-to-end"` with one shared `after-each` doing the drain.
2. In the fresh-session scenario (around lines 339 / 349), add `:tools '(toolA toolB)` to the `gptel-make-preset` call and `(expect content :to-match ":GPTEL_TOOLS: ")` to the assertion block. A presence-match is sufficient — exact list formatting is implementation detail and the scope-profile-applicator dedupe task (separate) will cover it more rigorously.
3. Re-run `./bin/run-tests.sh -d config/gptel/sessions` and confirm green.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/sessions
grep -nE "remhash|jf-gptel-preset-app-test--registry-keys" config/gptel/sessions/test/commands/preset-application-spec.el
grep -n "GPTEL_TOOLS" config/gptel/sessions/test/commands/preset-application-spec.el
```

Expect: drain calls present in every after-each that touches the helper; tools assertion present.

## Context

Full reviewer findings: `.orchestrator/cycles/cycle-1777625426/reviews/update-preset-application-spec-for-snapshot.md` (Findings 1, 2).

Cited register entries: `interfaces.org#register-shape-drawer-text-block`, `interfaces.org#register-invariant-drawer-system-key-write-exclusion`.

## Observations

- Chose the **outer-drain** option from the task brief: a single
  `after-each` on the outer `describe "gptel-drawer-as-source-of-truth:
  full snapshot end-to-end"` covers all three inner describes with one
  copy of the drain logic. Less code churn (3 lines added vs. 9 if
  duplicated per-describe) and DRY w.r.t. the identical drain that
  already lives on the legacy outer `describe "Drawer-driven auto-init
  …"` block.
- Added `:tools '(toolA toolB)` to the fresh-session `gptel-make-preset`
  call and `(expect content :to-match ":GPTEL_TOOLS: ")` to the
  assertion block — presence-match only, per the task brief
  ("exact list formatting is implementation detail and the
  scope-profile-applicator dedupe task — separate — will cover it more
  rigorously"). Confirms the `:GPTEL_TOOLS:` snapshot key flows from
  preset registration through `jf/gptel--render-drawer-text` /
  `jf/gptel--initial-session-content` and lands in the rendered
  `session.org` drawer.
- New buttercup spec count for the file remains 3 (no new `it`
  added — the existing fresh-session `it` got an extra
  `expect`). `Ran 82 specs, 0 failed` from the buttercup phase of the
  sessions suite.

## Discoveries

- **Pre-existing ERT failure surfaced by run-tests:** `./bin/run-tests.sh
  -d config/gptel/sessions` reports `exit code 1` because of one ERT
  failure in `config/gptel/sessions/filesystem-test.el` —
  `test-directory-creation-org-session-structure`. The mock
  `cl-letf`'d for `jf/gptel-scope-profile--create-for-session` declares
  5 args (`_preset _dir &optional _root _paths _parent`) but the real
  call site now passes 6, yielding `wrong-number-of-arguments
  ... 6`. **This is on HEAD and is unrelated to the present task** —
  verified by stashing this task's diff and re-running ERT: same
  failure, same backtrace. The failure is owned by whichever change
  added the 6th argument to `jf/gptel-scope-profile--create-for-session`
  (likely `wire-snapshot-into-session-creation` or
  `update-preset-application-spec-for-snapshot` in cycle-5). It needs a
  separate `.tasks/` follow-up: update the ERT mock's lambda arity to
  match the production signature, OR (preferably) port the test to
  buttercup with a spy that does not pin arity. Filing as an
  **interface-drift** discovery: the mock's contract no longer matches
  the implementation it stands in for.
- **Outer-describe drain placement is non-trivially correct here:** the
  drain works at the outer level only because the helper
  `jf-gptel-preset-app-test--register-cleanup` pushes onto a
  module-global `defvar` (not a `let`-bound local), so a single
  `after-each` at any enclosing describe sees every push made by
  inner-`it` bodies regardless of nesting depth. If a future task
  refactors `jf-gptel-preset-app-test--registry-keys` to be
  describe-local (e.g. via `before-each`/`let`-binding), the outer
  drain pattern silently breaks — would need to revisit. Worth a
  comment in-place; left a brief one ("Shared registry drain ...") in
  the source.

