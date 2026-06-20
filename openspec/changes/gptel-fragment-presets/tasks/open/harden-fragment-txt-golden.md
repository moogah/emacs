---
name: harden-fragment-txt-golden
description: Make the static fragment .txt golden tests actually catch committed drift — currently the load-time mirror rewrites the artifact before the assertion reads it, so the goldens are tautological in a writable checkout.
change: gptel-fragment-presets
status: ready
task_class: test
on_critical_path: false
cites_register_entries:
  - register/invariant/static-prerender-dynamic-compose
relations:
  - "discovered-from:migrate-prelude-preamble"
discovered_from: migrate-prelude-preamble
discovered_by: reviewer
discovered_class: invariant-gap
---

## Why this exists

The author-blind review of `migrate-prelude-preamble` (cycle-1781941375) found the
committed `.txt` golden tests for the static prelude/preamble fragments are
**tautological**: the load-time render-once mirror rewrites
`config/gptel/presets/sources/{emacs-prelude,agent-preamble}.txt` from its `.org`
source *before* the spec reads the file, so the assertion can never observe drift
between the committed artifact and the rendered output in a writable checkout. The
`static-prerender-dynamic-compose` invariant relies on the committed `.txt` being a
diffable, in-sync mirror — a golden that can't detect divergence doesn't enforce
that.

Advisory (non-blocking): the migrate task merged clean and the failing-set stayed
at baseline. This is a test-quality follow-up, not a correctness regression.

## Files to modify

- `config/gptel/presets/sources/*-spec.el` (or wherever the prelude/preamble `.txt`
  goldens live — the spec added/changed by migrate-prelude-preamble) — restructure
  so the assertion compares the **on-disk committed** `.txt` against a freshly
  rendered value WITHOUT the load-time mirror having rewritten it first.

## Implementation steps

1. Identify where the load-time mirror writes the `.txt` (the source `.el`'s
   render-once-and-mirror path) and ensure the golden spec reads the committed file
   BEFORE any mirror write runs (e.g. read raw file bytes at spec time, or stub the
   mirror writer with `cl-letf` so it cannot rewrite during the test), then assert
   the committed bytes equal `jf/gptel-fragment-render`'s output.
2. Add a negative case: a deliberately stale `.txt` fixture must make the spec FAIL
   (proving the golden now catches drift).
3. Tangle touched `.org`; run the presets suite.

## Verification

- `./bin/run-tests.sh -d config/gptel/presets`
- Temporarily edit a committed `.txt` by hand and confirm the golden spec FAILS
  (then revert) — proves the golden is no longer tautological.

## Context pointers

- Reviewer finding: `.orchestrator/cycles/cycle-1781941375/reviews/migrate-prelude-preamble.md` (finding 2, advisory).
- Invariant: `register/invariant/static-prerender-dynamic-compose` (load-bearing) — committed `.txt` must be an in-sync mirror.

## Observations

## Discoveries
