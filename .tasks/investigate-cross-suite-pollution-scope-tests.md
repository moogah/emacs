---
name: investigate-cross-suite-pollution-scope-tests
description: 19 scope-suite tests pass when run in isolation (`./bin/run-tests.sh -d config/gptel/scope`) but fail when run as part of the full suite (`./bin/run-tests.sh --report`). The polluter is somewhere in the load order before `config/gptel/scope/test/integration/` — likely `config/gptel/chat/test/`.
source: piped-hugging-flamingo (post-WS-A reassessment)
status: ready
relations: []
---

## Symptom

Two run modes report dramatically different failure counts for the same code:

```
./bin/run-tests.sh -d config/gptel/scope --report
  Buttercup: 497 ran, 494 passed   (3 failures, all known WS-B/Bug-4 territory)

./bin/run-tests.sh --report   (full suite)
  Buttercup: 1857 ran, 1835 passed (22 failures, including the 3 above)
```

The 19 extra failures are all in `config/gptel/scope/test/integration/`:
- `Parallel tool callback: …` (5 specs across 3 describes)
- `run_bash_command integration: "which brew" (no-op) full tool invocation through macro …` (3 specs)
- `run_bash_command integration: denied command …` (2 specs)
- `run_bash_command integration: file operation out of scope …` (3 specs)
- `run_bash_command integration: gptel callback contract …` (3 specs)

All 19 pass cleanly in scope-only and integration-directory-only runs.

## Hypothesis

Buttercup loads every `*-spec.el` into a single Emacs process and runs `buttercup-run` once. Test order is alphabetical by file path. Some test file under `config/gptel/chat/test/` (which runs before `config/gptel/scope/test/`) is leaving global state behind that the parallel + run_bash_command specs are sensitive to.

Candidates (each registers tools or mocks scope plumbing):

- `config/gptel/chat/test/stream/multi-round-tool-use-spec.el`
- `config/gptel/chat/test/stream/tool-confirm-spec.el`
- `config/gptel/chat/test/stream/tool-call-spec.el`

All three call `gptel-make-tool`, which mutates `gptel--known-tools` permanently. If a chat-side test (re-)defines a tool by a name that collides with a scope-side test's fixture, or leaves a spy active that persists past `(spy-on)`'s buttercup-managed teardown, the failures above would be the expected shape.

`config/gptel/scope/test/integration/parallel-tool-callback-spec.el:121-127` already shows that the cross-test reset pattern for scope expansion is to reset `jf/gptel-scope--expansion-active` / `jf/gptel-scope--expansion-queue` in `before-each`. That reset is present on the outer describes but the innermost `single tool call works (baseline)` describe (line 129) doesn't repeat it — it inherits via buttercup's before-each chain. Verify whether buttercup actually fires inherited before-each in this codebase's buttercup version.

## Verification steps

1. Bisect by directory to identify the polluting test directory:
   ```bash
   # Reproduce in full
   ./bin/run-tests.sh --report   # expect ~22 buttercup failures

   # Add directories one at a time
   ./bin/run-tests.sh -d config/gptel/scope --report          # 3 fails (baseline)
   ./bin/run-tests.sh -d config/gptel --report                 # 23 fails (chat+sessions+skills+tools layer)
   # …narrow further if the gptel run reproduces it
   ```

2. Once narrowed, inspect that directory's tests for `gptel-make-tool`, `gptel--known-tools`, or global state mutations missing teardown.

3. Either:
   - Add proper teardown in the polluter (preferred — `after-each` that restores state)
   - Add defensive reset in `before-each` of the affected scope tests (workaround)

## Why this matters

19 specs reporting noise in CI/full-suite runs hides real regressions. A test author who lands a real failure can't tell from the report whether their change introduced new failures or just re-shuffled the pollution. Cleaning this up makes the full-suite report trustworthy.

## Out of scope for this task

The 3 scope-only failures (`Bug 4`, `Bug: multi-violation × 2`) are tracked separately in `.tasks/fix-bash-violation-resource-routes-as-command.md` and are part of the broader `scope-rearch-followups` cleanup. Don't conflate them with the pollution diagnosis.

## Context

- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream A post-migration verification
- Pollution emerged after WS-A removed the YAML-fixture errors that previously masked the cleanly-failing-on-real-assertion shape of these tests; in earlier runs the same specs reported "108 failures, 6 distinct names" because the YAML errors swamped the meaningful signal
