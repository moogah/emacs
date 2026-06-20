---
name: harden-sources-loader-against-partial-load-failure
description: A fragment source (or preset) whose .el throws during load silently leaves its composer seam at the default (#'ignore / empty) with only a warn-level log — the exact dark-seam failure the loader was built to prevent. Cross-cutting; the gap is inherited from the jf/gptel-preset-register-all pattern the sources loader mirrors.
source: gptel-fragment-presets (cycle-1781941375, wire-fragment-sources-load)
status: ready
task_class: bug
cites_register_entries:
  - register/boundary/sources-directory-load
  - register/boundary/preset-org-to-registration
relations:
  - "discovered-from:wire-fragment-sources-load"
discovered_from: wire-fragment-sources-load
discovered_by: reviewer
discovered_class: invariant-gap
---

## Why this exists

The author-blind review of `wire-fragment-sources-load` (cycle-1781941375) found
that the new `presets/sources/*.el` directory loader (and the
`jf/gptel-preset-register-all` sibling it mirrors) swallows a load-time error from
any single source with only a warn-level log. When a source's `.el` throws before
its seam `setq` runs (e.g. `jf/gptel-fragment-environment-fn`), the composer seam
stays at its default (`#'ignore` for env; empty text for prelude/preamble) — which
is the **exact dark-seam outcome** the loader exists to prevent, now triggered by a
runtime load error instead of a missing load step. Spec coverage is happy-path
only.

This is **cross-cutting**, not in-change scope: the gap is inherited from the
pre-existing `register-all` loader pattern and affects both presets and fragment
sources. It does not contribute to the `gptel-fragment-presets` proposal's stated
outcome (the env/prelude/preamble seams ARE wired in the happy path, which the
proposal required). Externalised for the next maintainer of the loader pattern.

## Files to modify

- `config/gptel/presets/registration.org/el` — the `jf/gptel-fragment-sources-directory`
  loader and the `jf/gptel-preset-register-all` discovery: decide the failure policy
  (fail-loud at init? collect-and-surface? per-source `condition-case` that re-signals
  after logging which seam is now dark?).
- `config/gptel/presets/test/load-sources-spec.el` (+ register-all spec) — add a
  spec exercising a source that throws during load and asserting the chosen policy
  (e.g. error surfaced, or seam-still-dark detected and reported, not silently
  warned).

## Implementation steps

1. Decide policy with the maintainer: at gptel init, is a source load error fatal,
   or surfaced as a prominent (not warn-level) diagnostic naming the dark seam? Lean
   to: log error + leave a detectable "seam unpopulated" signal that an init
   self-check can assert, rather than hard-failing all of gptel init on one bad
   source.
2. Implement the policy symmetrically in the sources loader and `register-all`
   (single shared helper if it reduces duplication — note the literate-org
   duplication-is-blocking overlay rule).
3. Add failing-then-passing specs for the throw-during-load case.
4. Tangle; run `./bin/run-tests.sh -d config/gptel/presets`.

## Verification

- `./bin/run-tests.sh -d config/gptel/presets`
- New spec: a source that signals on load is detected (not silently warn-logged
  with the seam left dark).

## Context pointers

- Reviewer finding: `.orchestrator/cycles/cycle-1781941375/reviews/wire-fragment-sources-load.md` (advisory).
- Loader: `config/gptel/presets/registration.el` (`jf/gptel-fragment-sources-directory` / `jf/gptel-preset-register-all`).
- Register: `register/boundary/sources-directory-load` (stage-2 `can_fail_with: [load-error]`; stage-3 post-condition "seam NOT #'ignore after init").
