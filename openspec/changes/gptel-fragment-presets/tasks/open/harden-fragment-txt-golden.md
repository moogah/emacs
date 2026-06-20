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

- **De-tautologizing mechanism chosen: read committed bytes via `git show
  HEAD:<path>`, not a `cl-letf` stub of the mirror writer.** The task offered
  two routes (stub the mirror writer, or snapshot committed bytes before load).
  The stub route is unsound here: the mirror runs at top-of-file *load* time
  (the spec's `(load ".../{emacs-prelude,agent-preamble}.el")` / `require`),
  which completes before any `it` body or `before-each` runs. A `cl-letf` scoped
  to an `it` cannot un-rewrite a working-tree file the mirror already healed at
  load. Reading the bytes committed at HEAD sidesteps this entirely: the
  working-tree mirror writer never touches the git object, so the committed
  artifact is observable in its true (possibly-stale) state. This also matches
  the invariant's wording precisely — "committed `.txt` is an in-sync diffable
  mirror" is a property of the *committed* artifact, which is exactly what `git
  show HEAD:` reads. Verified the golden FAILS when the committed `.txt`
  diverges from the rendered fragment (proof in the commit body / report).

- **`composer-spec.el` was NOT modified — it has no committed-`.txt`-vs-rendered
  assertion to de-tautologize.** The Cycle 5 plan note named it as the location
  of "the fragment-render comparison," but on inspection
  `config/gptel/presets/test/composer-spec.el` only exercises the composition
  algebra (`jf/gptel-fragment-compose`, `jf/gptel-fragment--default-composition`)
  with synthetic fragment refs (`"PRELUDE"`, `"PREAMBLE"`, `"ROLE"`, `"ENV"`).
  It never reads a `.txt` artifact nor compares committed bytes to rendered
  output (`grep -n 'insert-file\|\.txt\|git show\|HEAD:' composer-spec.el`
  returns only unrelated comment hits). Modifying it would have invented scope.
  The two actual tautological goldens live only in `creation-spec.el` (agent
  preamble) and `environment-preamble-spec.el` (chat prelude); both are now
  restructured. See discovery `composer-spec-no-txt-golden`.

- **Each golden now has a paired negative case** asserting the in-sync
  comparison FAILS against an injected stale committed value (a `cl-letf` stub
  of the local `jf/{preamble,prelude}-golden--committed-bytes` helper returning
  rendered-text + `"STALE COMMITTED LINE"`). This is scoped to the `it` body
  and commits no broken artifact. It pins that the positive golden has teeth: a
  divergence is observable, so the positive assertion is not vacuously true.

- **A residual (intentional) sanity assertion remains** in each positive golden:
  `(expect <seam-var> :to-equal rendered)`. That sub-assertion *is* close to
  tautological (the seam is `setq`'d from the same render call in the source
  `.el`), but it is a cheap guard that the seam wiring still holds and is not
  the load-bearing committed-mirror check — the `(expect committed :to-equal
  rendered)` line is. Left in deliberately as a seam-wiring regression guard.

## Discoveries

- discovery_id: composer-spec-no-txt-golden
  class: spec-signal
  description: >-
    The Cycle 5 plan note (and the task's "Files to modify") name
    `config/gptel/presets/test/composer-spec.el` as the home of "the
    fragment-render comparison" golden. It is not. composer-spec.el tests only
    the composition algebra with synthetic fragment refs; it never reads a
    committed `.txt` nor compares committed bytes to rendered output. The two
    tautological committed-`.txt` goldens are exclusively in `creation-spec.el`
    (agent preamble) and `environment-preamble-spec.el` (chat prelude). I left
    composer-spec.el untouched to avoid inventing scope.
  affected_register_entry: register/invariant/static-prerender-dynamic-compose
  recommendation: >-
    Correct the task/plan's file inventory: drop composer-spec.el from the
    de-tautologize write-set. If a future task wants composer coverage of the
    *real* committed fragments (vs synthetic refs), that is a distinct,
    additive piece of work, not a de-tautologization.

- discovery_id: stub-route-unsound-for-load-time-mirror
  class: deviation
  description: >-
    The task offered "cl-letf-stub the mirror writer (`write-region` or the
    mirror fn)" as one of two de-tautologizing routes. That route is unsound for
    this mirror: it fires at top-of-file *load* (the spec's `(load ...)` /
    `require` of the source `.el`), which fully completes before any `it` or
    `before-each`. A `cl-letf` scoped to a test body cannot prevent or undo a
    rewrite the mirror already performed at load. I took the second offered
    route (read committed bytes), implemented as `git show HEAD:<path>` so the
    read is provably immune to the working-tree mirror.
  affected_register_entry: register/invariant/static-prerender-dynamic-compose
  recommendation: >-
    Prefer the committed-bytes (git-object) read over mirror-writer stubbing for
    any future load-time-mirror golden; stubbing only works for per-send /
    per-test writers, not load-time ones.

- discovery_id: static-prerender-invariant-reaffirmed
  class: invariant-gap
  description: >-
    Pressure-tested `register/invariant/static-prerender-dynamic-compose`
    (confirmed, load_bearing) against the restructured goldens. The reconciled
    wording — "rendered ahead of send, once, into committed text" (load-time
    render-once + committed mirror) — is exactly what the new goldens now
    enforce: committed `.txt` bytes (at HEAD) must equal the freshly rendered
    fragment, with a negative case proving divergence is observable. No gap
    remains in the invariant's wording; the prior gap was purely that the
    *golden* could not detect committed drift, now closed. Reaffirmed, no
    change requested.
  affected_register_entry: register/invariant/static-prerender-dynamic-compose
  recommendation: >-
    No edit to the register entry. The enforcing golden now matches the
    reconciled normative mechanism; treat the committed-mirror-in-sync golden as
    the canonical enforcement point for this invariant.
