---
name: fix-corpus-integration-002-cat-pattern-source
description: A family of ~9 ERT failures across `test-corpus-integration-002` and `test-pattern-flow-*` all share the same root cause — the bash-parser semantic pipeline does not propagate patterns from a producer command (`find`, `ls`, `grep -l` inside `$(...)`) to a consumer command (`cat`, `head`, `rm`, etc.) that operates on the substitution's output.
source: piped-hugging-flamingo (WS-C #3, WS-D)
status: ready
relations: []
---

## Bug in one sentence

When a command receives its arguments from `$(...)` command substitution that produces patterns (e.g. `cat $(find . -name '*.txt')`), the consuming command's read operation against the substituted pattern is not being extracted, leaving a coverage gap for the pipeline reader.

## Evidence

```bash
./bin/run-tests.sh -f ert -p "test-corpus-integration-002"
```

Failing assertion in `jf/test-run-corpus-case`:

```
Test integration-002 (Pipeline with pattern substitution and redirect):
  expected 4 operations, got 3
Expected: (
  (:file "." :operation :read-directory :command "find" :from-substitution t)
  (:file "*.txt" :operation :match-pattern :command "find" :pattern t :from-substitution t)
  (:file "*.txt" :operation :read :command "cat" :pattern t :from-substitution t
         :pattern-source (:substitution-content "find . -name '*.txt'"
                          :pattern "*.txt" :search-scope "."
                          :command "find" :from-substitution t))
  (:file "errors.log" :operation :write :source :redirection))
Actual: (
  (:file "." :operation :read-directory ... :command "find" :from-substitution t)
  (:file "*.txt" :operation :match-pattern ... :pattern t ... :command "find" :from-substitution t)
  (:file "errors.log" :operation :write ... :source :redirection :command "grep" ...))
```

The 3rd expected op (`cat` reading `*.txt`) is not produced.

## Why this matters

In the post-cycle-3 scope-validation pipeline, `cat <pattern>` operations get validated against the read paths. If pattern-substitution-fed reads aren't extracted, those reads aren't visible to validation, so a malicious or unintended command-substitution could read out-of-scope files without triggering scope expansion.

## Files likely involved

- `config/bash-parser/bash-parser-semantics.el` (or sibling) — semantic extraction for piped/substituted commands. Look for where the substitution producer's pattern is meant to be propagated to the consumer.
- `config/bash-parser/test/corpus/runners/test-corpus-file-operations.el:807` — the corpus test definition; the `:expect-ops` plist documents the intended contract.
- `config/bash-parser/test/integration/test-pattern-flow.el` — 8 ERT tests that document the full pattern-flow contract by command-class:
  - `test-pattern-flow-cat-find` (line 16) — `cat $(find ...)`
  - `test-pattern-flow-head-find` (line 59) — `head -n 10 $(find ...)` (file's own docstring already notes "KNOWN LIMITATION" for the `head` custom handler)
  - `test-pattern-flow-rm-ls` (line 87) — `rm $(ls *.tmp)`
  - `test-pattern-flow-cat-ls` (line 115)
  - `test-pattern-flow-cat-grep` (line 142)
  - `test-pattern-flow-nested` (line 174)
  - `test-pattern-flow-multiple-patterns` (line 204)
  - `test-pattern-flow-with-redirection` (line 228)

  All 8 specs fail today with the same shape: the producer's pattern is extracted, but the consumer's read against the substituted pattern is not. They are RED-PHASE tests for a not-yet-implemented feature; the file documents the intended contract.

## Implementation hints

1. Determine whether this is an expected-failure (parser limitation we accept) — if so, tag it via `:expected-result :failed` and document; or
2. Implement pattern-source propagation: when a command-substitution produces file patterns (find/ls with patterns), the consumer of that substitution should generate a `:read` op against each pattern, with `:pattern-source` metadata pointing back to the producer.

The presence of `:pattern-source` shape in the test fixture suggests a partial implementation exists somewhere — grep for `:pattern-source` to locate the contract.

## Verification

```bash
./bin/run-tests.sh -f ert -p "test-corpus-integration-002"
./bin/run-tests.sh -f ert -p "test-pattern-flow"
```

Pass criterion: all 9 tests report `0 unexpected`. Two paths to that state:
1. **Implement the feature** — propagate pattern-source metadata through `$(...)` to consuming commands. All 9 specs go green naturally.
2. **Tag as expected-failures** — add `:expected-result :failed` to each spec (matching the existing pattern for `test-extraction-xargs-*` and `test-extraction-heredoc-dynamic-redirect`). Accepts the parser limitation explicitly; suite reports them under "expected failures."

Path 2 is the lower-cost interim option if the feature is not scheduled; path 1 is the long-term correct fix.

## Context

- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream C #3
- Related: bash-parser ERT expected-failure audit (WS-D) — this test may rightfully belong in the expected-failure set if the parser limitation is by design
