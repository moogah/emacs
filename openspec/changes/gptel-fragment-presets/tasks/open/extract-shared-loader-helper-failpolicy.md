---
name: extract-shared-loader-helper-failpolicy
description: Extract one shared directory-loader helper for the presets/sources/*.el (fragment sources) and <name>/preset.el (register-all) loaders, and give it a symmetric, non-silent load-error fail-policy so a source/preset whose .el throws on load no longer leaves its composer seam dark (#'ignore) with only a warn-log.
change: gptel-fragment-presets
status: ready
task_class: refactor
on_critical_path: false
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
that the new `presets/sources/*.el` directory loader (`jf/gptel-fragment-sources-directory`)
and the `jf/gptel-preset-register-all` sibling it was modelled on **share ~all
their skeleton** AND share the same defect: each swallows a load-time error from a
single source/preset with only a warn-level log. When a source's `.el` throws
before its seam `setq` runs (e.g. `jf/gptel-fragment-environment-fn`), the composer
seam stays at its default (`#'ignore` for env; empty text for prelude/preamble) —
the **exact dark-seam outcome** the loader exists to prevent, now reachable via a
runtime load error instead of a missing load step.

Two architect end-of-cycle findings name this:
- `arch-cycle-1781941375-eoc-1` (class **duplication**, overlay-promoted to
  **blocking**): the two loaders share almost their entire skeleton.
- `arch-cycle-1781941375-eoc-4` (class **invariant-gap**): a source `.el` that
  throws on load leaves the seam dark with only a warn-log.

Handshake meta-discovery #1 (loader-robustness) recommends closing **both** in one
task: extract a single shared loader helper and set a symmetric load-error
failure policy. The user dispositioned this **in-change, promoted** (from
`.tasks/harden-sources-loader-against-partial-load-failure.md`) for the change's
final cleanup cycle.

## Fail-policy (recommended; pressure-test it)

At gptel init, a single bad source/preset SHOULD NOT hard-fail all of gptel init.
The recommended policy (from the originating externalised task) is **log at error
level + leave a detectable "seam unpopulated" signal** so an init self-check can
assert which seam is dark, rather than the current silent warn-and-continue. Lean
to: per-source `condition-case` that (a) logs an **error** (not warn) naming the
file and the seam it failed to populate, and (b) records the failure so a
post-init check can surface "N source(s) failed to load; seam X is dark". This is
**reference material to pressure-test, not authority to defer to** — if fail-loud
(re-signal after logging) or collect-and-surface proves cleaner, take it and record
the deviation in `## Discoveries`.

## Files to modify

- `config/gptel/presets/registration.org` / `.el` — extract the shared
  directory-glob-and-load helper used by `jf/gptel-fragment-sources-directory` and
  `jf/gptel-preset-register-all`; route both through it with the chosen fail-policy.
  (Duplication is **blocking** under this project's overlay — the extraction is the
  point, not optional.)
- `config/gptel/presets/test/load-sources-spec.el` — add a spec exercising a source
  whose `.el` throws during load; assert the chosen policy (error surfaced /
  seam-dark detected and reported — NOT silently warn-logged).
- `config/gptel/presets/test/registration-spec.el` — symmetric spec for
  `register-all`: a preset `.el` that throws on load is detected, not silently
  swallowed.

> Scoping for parallel safety this cycle: this task OWNS `registration.{org,el}`,
> `load-sources-spec.el`, and `registration-spec.el`. It MUST NOT touch the
> prelude/preamble `.txt` golden specs (owned by `harden-fragment-txt-golden`:
> `creation-spec.el`, `environment-preamble-spec.el`, `composer-spec.el`) and MUST
> NOT delete any `presets/*.md` (owned by `delete-old-presets`).

## Implementation steps

1. Write the failing specs first (overlay: failing-spec-first is the executable
   contract here): a source `.el` and a preset `.el` that each `(error "boom")` on
   load; assert the loader surfaces it per the chosen policy and reports the dark
   seam — not a bare warn.
2. Extract the shared glob-and-load helper; route both `*-directory` loaders
   through it. Keep each loader's seam-specific glue (which seam, which default)
   thin; the helper owns directory globbing + per-entry `condition-case` + the
   error/diagnostic policy.
3. Implement the post-init detectability signal (e.g. a list/var of failed sources,
   or an init self-check that asserts seams are populated) per the policy.
4. Tangle (`./bin/tangle-org.sh config/gptel/presets/registration.org`); run
   `./bin/run-tests.sh -d config/gptel/presets`.

## Verification

- `./bin/run-tests.sh -d config/gptel/presets`
- New specs: a source/preset that signals on load is **detected** (error-logged +
  dark-seam reported), not silently warn-logged with the seam left `#'ignore`.
- `grep -n "condition-case\|jf/gptel--log" config/gptel/presets/registration.el`
  shows the policy lives in ONE shared helper, not duplicated per loader.

## Context pointers

- Reviewer finding: `.orchestrator/cycles/cycle-1781941375/reviews/wire-fragment-sources-load.md` (advisory).
- Architect: `arch-cycle-1781941375-eoc-1` (duplication→blocking), `arch-cycle-1781941375-eoc-4` (invariant-gap).
- Meta-discovery #1 (loader-robustness) in `.orchestrator/handshake-cycle-1781941375.json`.
- Loaders: `config/gptel/presets/registration.el` (`jf/gptel-fragment-sources-directory` / `jf/gptel-preset-register-all`).
- Register: `register/boundary/sources-directory-load` (stage-2 `can_fail_with: [load-error]`; stage-3 post-condition "seam NOT #'ignore after init").
- Promoted from `.tasks/harden-sources-loader-against-partial-load-failure.md` (cycle-1781944619 plan; user disposition: promote + extract shared helper).

## Observations

## Discoveries
