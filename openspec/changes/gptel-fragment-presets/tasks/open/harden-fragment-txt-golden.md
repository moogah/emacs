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

## Cycle 5 plan note (cycle-1781944619 — final cycle)

The prelude/preamble `.txt` goldens live in (verified at plan): the agent-preamble
golden in `config/gptel/tools/test/persistent-agent/creation-spec.el`, the prelude
golden in `config/gptel/chat/test/menu/environment-preamble-spec.el`, and the
fragment-render comparison in `config/gptel/presets/test/composer-spec.el`. The
load-time mirror writers are in `config/gptel/presets/sources/emacs-prelude.el`
(`write-region` of `jf/gptel-fragment-emacs-prelude--text` → `emacs-prelude.txt`)
and `config/gptel/presets/sources/agent-preamble.el`. Stub/avoid that mirror writer
(via `cl-letf`) — or read the committed bytes before any load runs — so the golden
observes committed-vs-rendered drift.

**Batch & merge order this cycle:** [extract-shared-loader-helper-failpolicy →
**this task** → delete-old-presets]. This task OWNS the three golden spec files
above; it must NOT touch `registration.{org,el}`/`load-sources-spec.el`/
`registration-spec.el` (owned by extract-shared-loader-helper-failpolicy) nor delete
`presets/*.md` (owned by delete-old-presets). `delete-old-presets` merges last and
refreshes the snapshot AFTER this task's restructured golden lands.

This task's `register/invariant/static-prerender-dynamic-compose` citation: the
invariant's wording was reconciled this plan (load-time render-once + committed
mirror is the normative mechanism — see interfaces.org status_note); the golden must
enforce exactly that committed-mirror-is-in-sync property.

## Files to modify

- The three golden spec files named above — restructure so the assertion compares
  the **on-disk committed** `.txt` against a freshly rendered value WITHOUT the
  load-time mirror having rewritten it first (stub the mirror writer with `cl-letf`,
  or snapshot the committed bytes before load).

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
