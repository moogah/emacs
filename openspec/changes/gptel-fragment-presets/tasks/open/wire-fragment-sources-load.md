---
name: wire-fragment-sources-load
description: Load the fragment source modules (presets/sources/*.el) in gptel.org's load order so dynamic/static fragment seams are populated in production, not just under tests.
change: gptel-fragment-presets
status: ready
task_class: infrastructure
on_critical_path: true
cites_register_entries:
  - register/boundary/sources-directory-load
  - register/boundary/preset-org-to-registration
  - register/invariant/static-prerender-dynamic-compose
relations:
  - "discovered-from:migrate-environment"
  - "enables:delete-old-presets"
discovered_from: migrate-environment
discovered_by: reviewer+implementor
discovered_class: scope-question
---

## Cited register entries

- **register/boundary/sources-directory-load** — *This entry is SPECULATED* (seeded
  this cycle by forward-mode from the source-load-wiring meta-discovery). It is the
  whole point of this task: pressure-test whether a single **directory loader** for
  `presets/sources/*.el` (mirroring `<name>/preset.el` discovery) is the right
  mechanism, vs. an explicit per-source load list. The entry's stage-1 contract is
  "every `*.el` under `sources/` is loaded exactly once at init"; stage-3
  post-condition is "`jf/gptel-fragment-environment-fn` is NOT `#'ignore` after
  init." If a directory loader proves wrong (e.g. ordering among sources matters,
  or a source must NOT auto-load), push back via `## Discoveries`.
- **register/boundary/preset-org-to-registration** (confirmed) — the loader you add
  must respect the SAME load-order risk this boundary names: sources load AFTER
  `presets/fragments.el` (composer/renderer + seam defvars) and BEFORE the
  chat/agent/env consumers. Mirror its `<name>/preset.el` discovery convention.
- **register/invariant/static-prerender-dynamic-compose** (confirmed, load-bearing)
  — loading the env SOURCE must not change WHEN rendering happens: static refs stay
  pre-rendered, the dynamic env ref still evaluates at compose-time tail. You are
  fixing *where the producer is registered*, not the render timing.

## Why this exists

`migrate-environment` (cycle-1781900938) moved the environment block into
`config/gptel/presets/sources/environment.el` as a dynamic fragment wired through
the composer seam `jf/gptel-fragment-environment-fn` (default `#'ignore`).
**But `presets/sources/` is not on `load-path` and `gptel.org` does not load it**,
and `jf/gptel-preset-register-all` only descends `<name>/preset.el` subdirs — so
nothing loads `environment.el` at runtime. `menu.el`'s `(require
'jf-gptel-fragment-environment nil t)` is a soft require that silently no-ops.
Net effect as merged: the env tail ships **dark** in real sessions until the
source is loaded; the seam stays `#'ignore`.

Independently surfaced by BOTH the migrate-environment implementor (scope-question
discovery) and the author-blind reviewer (advisory design-drift finding). The
env-source loading was explicitly scoped OUT of migrate-environment's write-set
(it owns `presets/sources/environment.*`, `chat/menu.*`, the env spec — not
`gptel.org`).

This is **in-change scope**: the proposal requires the environment block to keep
its existing observable behavior after migration; a dark seam fails that.

## Files to modify

- `config/gptel/gptel.org/el` (modify) — load the fragment SOURCE modules in the
  correct order: AFTER `config/gptel/presets/fragments.el` (the renderer/composer)
  and BEFORE the chat/agent/env consumers, alongside / near the
  `jf/gptel-preset-register-all` call (gptel.org:274–278).
- Possibly `config/gptel/presets/registration.org/el` OR a small dedicated loader
  — decide whether `register-all` (or a sibling `load-sources-all`) should also
  descend `presets/sources/*.el`, vs. an explicit per-source load list in
  `gptel.org`. A `sources/` directory loader mirrors the `<name>/preset.el`
  discovery and future-proofs for `emacs-prelude.el` / `agent-preamble.el`.

## Forward dependency (do not lose)

`migrate-prelude-preamble` (next cycle) creates `presets/sources/emacs-prelude.el`
and `presets/sources/agent-preamble.el` and wires the chat-prelude /
agent-preamble seam text. Those static sources need the SAME load wiring. Prefer a
**directory loader for `presets/sources/`** so both this task and
migrate-prelude-preamble are covered by one mechanism, rather than an explicit
file list that must be extended per source.

## Implementation steps

1. Decide the load mechanism (directory loader for `presets/sources/` vs explicit
   list). Lean to a directory loader (`jf/gptel-fragment--load-sources-all` or
   extend register-all's discovery) for symmetry with `<name>/preset.el`.
2. Insert the source-load step into `gptel.org` load order AFTER `presets/fragments.el`
   and before chat/agent/env consumers. Respect the gptel load-order invariant
   (skills → gptel-agent → tool defs → sessions … ).
3. After loading, the env seam (`jf/gptel-fragment-environment-fn`) must be the env
   producer (not `#'ignore`) in a real session. Add a spec/integration check that
   the seam is populated once gptel is loaded (NOT via absolute-path load — that's
   what the unit spec already does).
4. Replace `menu.el`'s soft `(require … nil t)` reliance with a hard guarantee that
   the source is loaded by gptel init, OR keep the soft require but ensure the
   gptel.org load makes it resolvable.
5. Tangle touched `.org`; run the gptel suite.

## Verification

- `grep -n "sources/environment\|load-sources\|fragment-environment" config/gptel/gptel.el`
  (env source is loaded at init).
- In a loaded session: `jf/gptel-fragment-environment-fn` is NOT `#'ignore`.
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` and `-d config/gptel/presets`.
- Full suite green (set vs baseline).

## Cycle 4 scoping (cycle plan — parallel-safe with migrate-prelude-preamble)

This task runs **in the same batch** as `migrate-prelude-preamble`, which owns
`config/gptel/chat/menu.org`. To keep the two write-sets **disjoint**, this task
**MUST NOT edit `menu.org`**. Do step 4 the soft-require way: the directory loader
in `gptel.org` makes the source loadable at init, so `menu.el`'s existing
`(require 'jf-gptel-fragment-environment nil t)` resolves naturally — leave it as is.
Do not "harden" it into a hard require here (that would collide with
migrate-prelude-preamble's menu.org edits). If you believe a menu.org change is
unavoidable, STOP and raise it in `## Discoveries` rather than editing menu.org.

**Merge order:** this task merges BEFORE `migrate-prelude-preamble` so that
task's new `emacs-prelude.el` / `agent-preamble.el` sources are picked up by the
loader you add (not shipped dark). The directory loader you build must cover those
two future sources without further `gptel.org` edits.

## Out-of-scope

- Editing `config/gptel/chat/menu.org` (owned by migrate-prelude-preamble this cycle).
- Authoring the prelude/preamble sources (migrate-prelude-preamble).
- Deleting legacy `.md` presets / dropping `yaml` (delete-old-presets, next cycle).

## Context pointers

- Source: `config/gptel/presets/sources/environment.org/el` (the dynamic fragment).
- Composer seam: `config/gptel/presets/fragments.el` (`jf/gptel-fragment-environment-fn`,
  `--default-composition`).
- Loader: `config/gptel/presets/registration.el` (`jf/gptel-preset-register-all`,
  the `<name>/preset.el` discovery to mirror).
- gptel load order: `config/gptel/gptel.org` (lines ~274–278).
- Reviewer finding: `.orchestrator/cycles/cycle-1781900938/reviews/migrate-environment.md`.

## Observations

- **Directory-loader mechanism confirmed as correct.** The flat `sources/*.el`
  loader (`jf/gptel-fragment--load-sources-all`) cleanly mirrors
  `jf/gptel-preset-register-all`'s discovery, but applied to a flat directory
  rather than `<name>/preset.el` subdirs (sources are not one-per-subdir and are
  not self-registering into a side table). No inter-source ordering dependency
  exists today: each source only depends on `jf-gptel-fragments` (loaded by
  `gptel.org` before this call). The loader sorts filenames (`string<`) purely
  for determinism — sort order is incidental, not load-bearing. This future-proofs
  `migrate-prelude-preamble`'s `emacs-prelude.el` / `agent-preamble.el` with zero
  further `gptel.org` edits, exactly as the cycle plan requires.

- **`presets/sources/` does NOT need to be on `load-path`.** The loader `load`s
  each source by absolute path. `environment.el`'s `(require 'jf-gptel-fragments)`
  resolves because `gptel.org` loads `presets/fragments.el` (which `provide`s
  `jf-gptel-fragments`) two steps earlier; and `environment.el`'s
  `(provide 'jf-gptel-fragment-environment)` then satisfies `menu.el`'s soft
  `(require … nil t)`. No `add-to-list 'load-path` was added or needed.

- **`menu.org` left untouched (Cycle 4 scoping honored).** The soft require in
  `menu.el` resolves naturally once the directory loader runs at init; no
  hardening to a hard require (that would collide with `migrate-prelude-preamble`).

- **Load-order placement.** The source-load step is inserted in `gptel.org`
  immediately after `jf/gptel-preset-register-all` (gptel.el line ~167), which is
  after `presets/fragments.el` (line ~147) and `presets/registration.el` (line
  ~152) and before the chat/agent/env consumers. Verified by inspecting the
  tangled `gptel.el` ordering.

- **Idempotency note.** Re-running the loader re-`load`s each source, which
  re-runs its `(setq jf/gptel-fragment-environment-fn …)` — the same assignment,
  so re-load is safe (covered by a spec). This matches the register-layer
  idempotency story for `register-all`.

- **Test approach.** The new `load-sources-spec.el` exercises the DISCOVERY +
  WIRING mechanism against the REAL `presets/sources/` directory (via
  `jf/gptel-fragment-sources-directory`), deliberately NOT loading
  `environment.el` by absolute path — that is what the env source's own unit spec
  already covers. It asserts the stage-3 post-condition (seam ≠ `#'ignore` after
  load) and the feature-provide that resolves the menu soft require, plus the
  missing-dir no-op and idempotency edges.

- **Adjacent (out of scope, not touched):** the full suite still carries the
  known baseline failures (5 buttercup + 9 ERT: 8 `test-pattern-flow-*` +
  `test-corpus-integration-002`). This task added none; the failing set is
  byte-identical to baseline.

## Discoveries

- discovery_id: disc-wire-fragment-sources-load-1
  class: spec-signal
  description: |
    register/boundary/sources-directory-load (status: SPECULATED) was
    pressure-tested by implementing the directory-loader mechanism it
    speculates. The implementation realizes the entry as specified:
      - stage-1 (discover-sources): `jf/gptel-fragment--load-sources-all`
        enumerates the flat `sources/*.el` via `jf/gptel-fragment-sources-directory`
        (derived from `jf/gptel-presets-directory`), the preferred directory-loader
        alternative — NOT an explicit per-source list. Contract met: every `*.el`
        under `sources/` is `load`ed exactly once at init.
      - stage-2 (load-in-order): the source-load step is sequenced in `gptel.org`
        AFTER `presets/fragments.el` (seam defvars) and BEFORE chat/agent/env
        consumers, near the `register-all` call (gptel.el ~167), respecting the
        gptel load-order invariant.
      - stage-3 (seam-populated): post-condition holds — after the loader runs,
        `jf/gptel-fragment-environment-fn` is NOT `#'ignore` (it is the env
        producer); verified by a behavioral spec against the real source dir.
    The speculation's two open questions both resolved in favor of the directory
    loader: (a) no inter-source ordering dependency exists (sort order incidental),
    and (b) no source must be prevented from auto-loading. The loader's producer
    sits in registration.org (`jf/gptel-fragment--load-sources-all` +
    `jf/gptel-fragment-sources-directory`), and the call-site producer sits in
    gptel.org — matching the entry's two `producers` rows.
  affected_register_entry: register/boundary/sources-directory-load
  recommendation: |
    RECONCILE speculated → confirmed. Update status_note to record:
    (1) chosen mechanism = directory loader `jf/gptel-fragment--load-sources-all`
    over a flat `sources/` dir keyed off `jf/gptel-fragment-sources-directory`
    (new defvar in registration.org), NOT an explicit per-source list;
    (2) discovery is flat `sources/*.el` (sorted, `string<` deterministic),
    deliberately distinct from register-all's `<name>/preset.el` subdir descent
    because sources are flat + not self-registering;
    (3) no `load-path` addition needed — sources `load`ed by absolute path; the
    renderer feature is already `provide`d by the earlier `fragments.el` load and
    each source `provide`s its own feature (resolving menu.el's soft require);
    (4) call-site in gptel.org is at the line after `register-all` (gptel.el ~167).
    Concrete producer function names that replace the `<source-load step>` /
    `<jf/gptel-fragment--load-sources-all OR register-all extension>` placeholders:
    `jf/gptel-fragment--load-sources-all` (registration.org, stages 1+2) and the
    bare `(jf/gptel-fragment--load-sources-all)` call (gptel.org, stage 2).

- discovery_id: disc-wire-fragment-sources-load-2
  class: invariant-gap
  description: |
    register/invariant/static-prerender-dynamic-compose is upheld and unchanged:
    loading the env source only sets the composer SEAM (`jf/gptel-fragment-environment-fn`)
    to the dynamic producer; it does not move any render. Static preset role text
    remains pre-rendered (at preset .el load time, into a defconst); the dynamic
    env fragment still evaluates at compose-time tail via the seam. This task
    fixes WHERE the producer is registered, not WHEN rendering happens. No change
    to the invariant entry is warranted.
  affected_register_entry: register/invariant/static-prerender-dynamic-compose
  recommendation: |
    No change. Note in reconciliation that wire-fragment-sources-load verified
    the invariant is render-timing-neutral (seam wiring only).
