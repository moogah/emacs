---
name: gptel-preexisting-async-scope-test-failures
description: "Investigate and fix 21 pre-existing buttercup failures in config/gptel (async-callback/queue and a few scope assertions), surfaced as the cycle-1 baseline of the content-addressed-session-activation change."
source: gptel-content-addressed-session-activation
status: open
discovered_from: cycle-1781448273-baseline
discovered_by: orchestrator
discovered_class: pre-existing-failure
relations:
  - "discovered-from:drawer-signature-and-head-read"
---

## Summary

`./bin/run-tests.sh -d config/gptel` reports **21 buttercup failures** (ERT side
clean, 23/23). They were present at the cycle-1 baseline of the
`gptel-content-addressed-session-activation` change — they **predate** it and sit
entirely **outside** that change's surface (sessions identity/signature, chat-mode
activation, commands writers). The change introduced **zero** new failures
(verified by normalized failset diff across all three merges). Parked here so the
next maintainer of the scope/chat-async subsystems can pick it up without holding
the active change open.

## Failure clusters (normalized, ~15 distinct assertions across 21 raw failures)

Dominant pattern — **async callbacks not firing in headless/batch test mode**:
- `parallel--callback-count` expected 1/2, got 0 (parallel-tool-callback)
- `bash-integ--callback-result` nil / `:success` not t (bash integration)
- `(gptel-tool-async tool)` expected t, got nil
- `bug--callback-invoked`, `callback-arg-count`, `prompt-call-count`, `load-count`
  all 0/nil (queue- and callback-driven specs)
- "CORRECT: FSM counter reaches ntools via queue for session to advance" FAILED

Scope assertions (smaller cluster):
- `jf/gptel-scope--validate-bash-tool` / `jf/gptel-scope-prompt-expansion`
  `:to-have-been-called` (spy not invoked)
- `(cl-some ... (jf/gptel-scope--glob-match-p "/brew" p))` nil
- `(length transient-scopes)` expected 1, got 0
- `write-paths` expected to contain `"/dev/null"`, got nil

## Hypothesis

Most of these look like **test-harness timing / async-callback delivery in batch
mode**, not deterministic logic regressions (the counters land at 0/nil, i.e. the
callback never ran, rather than a wrong value). A subset (glob `/brew`,
`write-paths /dev/null`, transient-scopes) may be genuine scope logic or
fixture-environment issues. Triage needed to split "flaky-in-batch" from
"actually broken".

## Suggested first steps

1. Reproduce per-directory: `./bin/run-tests.sh -d config/gptel/scope` and
   `-d config/gptel/chat` to isolate the clusters.
2. For the async/callback cluster, check whether the specs rely on real async
   delivery (timers, process sentinels) that batch Emacs doesn't pump, and whether
   they should `accept-process-output` / advance a queue deterministically.
3. For the scope assertions, check fixture/environment assumptions (`/brew`,
   `/dev/null`) and spy setup.
4. Promote to a full openspec change if the fix needs design discussion; otherwise
   fix in place and close here.

## Provenance

Surfaced by the orchestrator while establishing the cycle-1 baseline for
`gptel-content-addressed-session-activation`. Baseline snapshot and failset:
`.orchestrator/baseline-1781448273.txt` / `.failset.txt` (note: `.orchestrator/`
is gitignored/ephemeral — re-run the suite to regenerate).
