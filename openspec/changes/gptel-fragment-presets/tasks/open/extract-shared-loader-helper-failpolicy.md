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

- **Discovery / load split for parallel-safe extraction.** The two loaders
  differ structurally only in *discovery* (flat `sources/*.el` sorted vs
  per-subdir `<name>/preset.el`). I extracted each loader's discovery into a
  thin `jf/gptel-...--discover-*-els` function and pushed everything they share
  (dir-exists check, per-entry `condition-case`, error log, failure record,
  success count, info log) into ONE `jf/gptel--load-directory (directory
  discover-fn seam label)`. This is the minimal cut that removes the duplicated
  skeleton without coupling the two distinct discovery shapes.

- **Behavior change in `register-all` discovery, surfaced by a pre-existing
  spec.** The old `register-all` used `file-readable-p` on `preset.el` inside
  the load loop; the discovery split now filters with `file-exists-p` at
  discovery time and `jf/gptel--load-directory` re-checks `file-readable-p`
  before loading. Net behavior is identical (the "ignores subdirectories
  without a preset.el" spec stays green), but the readability gate now lives in
  the shared helper, not the discovery function — so an unreadable-but-existing
  `preset.el` is silently skipped (no failure recorded) exactly as before. That
  silent-skip-on-unreadable is unchanged pre-existing behavior, NOT part of the
  fail-policy (only *load-time signals* are recorded). Noted in case a future
  task wants unreadable entries to also count as dark seams.

- **Failure registry is shared mutable state across module boundaries.**
  `jf/gptel--loader-failures` is a single module-level list both loaders push
  to. The overlay treats shared mutable state with prejudice; I kept it because
  (a) it is append-only within a load pass, (b) `jf/gptel-loader-clear-failures`
  gives callers (and gptel init) an explicit reset seam, and (c) the post-init
  self-check needs ONE place to read "which seams are dark" across both loaders.
  The tests reset it in `before-each`/`after-each`. If a future re-init path
  re-runs the loaders without clearing first, stale failures would linger —
  gptel.org's init sequence should call `jf/gptel-loader-clear-failures` before
  the load steps if it does not already (out of my write-set: gptel.org is owned
  elsewhere this cycle — see Discovery D3).

- **Test assertion strength.** The ERROR-vs-warn assertion stubs `jf/gptel--log`
  via `cl-letf` and asserts an error-level record naming the file exists AND no
  warn-level record naming the file exists — this is a strong assertion that
  directly encodes the fail-policy ("error, not warn"). The seam-name assertion
  (`:seam` = the composer seam symbol) is the load-bearing part: it is what lets
  a self-check say *which* seam is dark, not merely *that* something failed.

- **`jf/gptel-fragment-environment-fn` as the source seam symbol is a
  simplification.** The sources directory can hold multiple sources populating
  *different* seams (env today; prelude/preamble next). I record a single seam
  symbol per loader (the env seam for the sources loader), so a failing
  prelude/preamble source would be recorded against the env seam symbol, which
  is imprecise. Per-source seam attribution would require each source to declare
  its seam — out of scope here (it would touch `presets/sources/*`, owned by
  another task). Recorded as Discovery D2.

## Discoveries

- discovery_id: D1-fail-policy-decision
  class: deviation
  description: |
    Implemented the RECOMMENDED policy (log-at-error + record-failure +
    detectable self-check), NOT fail-loud re-signal. Rationale: both cited
    boundaries' contract is "a single bad source/preset SHOULD NOT hard-fail all
    of gptel init", and the loaders run unattended at startup — re-signalling
    would abort the whole gptel subsystem on one bad entry, which is strictly
    worse than the dark-seam it would replace. The non-silence requirement is met
    by (a) ERROR-level log naming file AND seam, and (b) a recorded failure that
    `jf/gptel-loader-seam-dark-p` exposes for a post-init self-check. This keeps
    the failure loud and detectable without taking down seams that loaded fine.
  affected_register_entry: register/boundary/sources-directory-load
  recommendation: |
    Update the entry's status_note: the stage-2 `can_fail_with: [load-error]`
    POLICY (previously "the open refinement") is now CLOSED — load errors are
    error-logged + recorded against the seam, not warn-and-forgotten. The
    stage-3 post-condition ("seam NOT #'ignore after init") now has a companion
    runtime self-check (`jf/gptel-loader-seam-dark-p SEAM`) that reports the
    inverse when a load error darkened the seam.

- discovery_id: D2-coarse-seam-attribution
  class: invariant-gap
  description: |
    The sources loader records ALL its load failures against a single seam
    symbol (`jf/gptel-fragment-environment-fn`), but the sources/ directory will
    soon hold sources populating other seams (prelude/preamble per
    migrate-prelude-preamble). A failing prelude source would be (mis)attributed
    to the env seam in `jf/gptel--loader-failures`. The self-check still
    correctly reports "a sources load failed", but `seam-dark-p` would point at
    the wrong seam.
  affected_register_entry: register/boundary/sources-directory-load
  recommendation: |
    When the prelude/preamble sources land, give each source a way to declare
    the seam it populates (e.g. a per-source manifest or a convention) so
    `jf/gptel--load-directory` can record the precise seam. Out of scope here
    (touches presets/sources/*, owned by another task this cycle).

- discovery_id: D3-init-clear-failures
  class: scope-question
  description: |
    `jf/gptel--loader-failures` is module-level shared state. For a post-init
    self-check to be meaningful, gptel.org's init sequence should call
    `jf/gptel-loader-clear-failures` before the `register-all` / `load-sources`
    steps (so a re-init does not show stale failures), and a self-check could
    assert no dark seams after the load steps. Both are in gptel.org, which is
    OUTSIDE this task's write-set this cycle.
  affected_register_entry: register/boundary/sources-directory-load
  recommendation: |
    A follow-up (or the integrate phase) should wire
    `jf/gptel-loader-clear-failures` before the load steps in gptel.org and
    optionally add an init self-check that logs any dark seams after both
    loaders run. The detectability primitives (`jf/gptel-loader-failures`,
    `jf/gptel-loader-seam-dark-p`) are now in place to support it.

- discovery_id: D4-duplication-resolved
  class: duplication
  description: |
    arch-cycle-1781941375-eoc-1 (duplication, overlay-promoted blocking) is
    closed: the two loaders previously shared their entire skeleton; they now
    route through ONE `jf/gptel--load-directory` helper. `grep -n
    "condition-case" config/gptel/presets/registration.el` shows a single
    occurrence (in the helper). arch-cycle-1781941375-eoc-4 (invariant-gap:
    silent warn leaves seam dark) is closed by the error-log + record policy.
  affected_register_entry: register/boundary/preset-org-to-registration
  recommendation: |
    Confirm the entry's status_note line "arch-cycle-1781941375-eoc-1 flags the
    two loaders share ~all skeleton ... a shared-helper extraction is the
    next-plan meta-discovery" can be marked resolved: the shared helper exists
    and both loaders route through it symmetrically.
