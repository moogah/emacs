---
name: canonicalize-workspace-home-path-form
description: Reconcile :home representation between workspace-new default-path (no trailing slash) and anchor-existing (file-name-as-directory with trailing slash); either canonicalize at workspace--make or pin the canonical form in register/shape/workspace-plist-v3
change: add-workspace-home-directory
status: ready
relations:
  - discovered-from:workspace-new-anchor-existing
---

<!-- Provenance fields (orchestrator schema):
     discovered_from: workspace-new-anchor-existing
     discovered_by: reviewer (workspace-new-anchor-existing, Finding 1,
       severity: advisory, direction: design-drift)
     discovered_class: shape-fragmentation
     review_file: .orchestrator/cycles/cycle-20260526-191802/reviews/workspace-new-anchor-existing.md
     reconciled_into: register/shape/workspace-plist-v3 (pending decision
       between option A constructor-side canonicalisation and option B
       shape-spec disclaimer; integrate-phase architect will route) -->

## Why this task exists

Cycle-4's `workspace-new-anchor-existing` reviewer (advisory Finding 1)
identified that the two `workspace-new` branches store `:home` in
**different forms**:

- **default-path** (cycle-3 `workspace-new-default-path`):
  ```elisp
  (home (expand-file-name name workspaces-default-parent-directory))
  ;; → no trailing slash
  ```

- **anchor-existing** (cycle-4):
  ```elisp
  (home (file-name-as-directory (read-directory-name ... t)))
  ;; → trailing slash present
  ```

`register/shape/workspace-plist-v3` (load_bearing: true, status:
reconciled) pins `:home` as "absolute filesystem path" but is **silent
on canonicalisation** of trailing slash. Two workspaces created via
the two `workspace-new` branches now carry structurally different
`:home` strings even though they denote the same directory.

Downstream consumers tolerate both forms today:
- `expand-file-name "home.org" home` works either way.
- `workspace--sessions-dir` via `expand-file-name "sessions" home`
  works either way.
- `workspace--registered-for-home-p` correctly normalises via
  `file-equal-p` on `(file-name-as-directory (expand-file-name HOME))`.

But any future `equal` / `string=` comparison on `:home` values
(e.g., "is this buffer's path under any workspace's :home?") would
silently miss-match across the two branches. The cycle-3 BLOCKING
finding on broken-home-tolerance was exactly this representational
asymmetry class (between `:home` and `:name` slot mutation).

## Files to modify

Decision-dependent. Two options:

### Option A: canonicalise at the constructor (preferred default)

- `config/workspaces/data-model.org` (and `.el`) — modify
  `workspace--make` to canonicalise its `home` argument:
  ```elisp
  (defun workspace--make (name home)
    (let ((canonical-home
           (file-name-as-directory (expand-file-name home))))
      (list :name name
            :home canonical-home
            ...)))
  ```
- `config/workspaces/persistence.org` — confirm the deserialiser's
  `:home` reconstruction also canonicalises (or remove redundant
  canonicalisation if the constructor handles it).
- All existing test fixtures that build plists by hand may need to
  pre-canonicalise or call `workspace--make`.
- `interfaces.org` — update `register/shape/workspace-plist-v3` to
  pin the canonical form in the `:home` description.

### Option B: pin the canonical form in the shape spec only

- `interfaces.org` — update `register/shape/workspace-plist-v3` to
  pin the trailing-slash + `expand-file-name`-normalised form as
  the canonical `:home`.
- Update both `workspace-new` branches to canonicalise BEFORE calling
  `workspace--make` (i.e., the canonicalisation lives at the call
  sites, not in `workspace--make`).

Option A is the safer default: it makes `workspace--make` the single
source of canonicalisation, so future callers cannot diverge.

## Implementation steps

1. **Decision**: Option A (constructor-side) is recommended. The
   architect can override at integrate.

2. **Option A path**:
   a. In `config/workspaces/data-model.org`, change `workspace--make`
      to wrap its `home` argument with `(file-name-as-directory
      (expand-file-name home))` before assignment.
   b. Tangle: `./bin/tangle-org.sh config/workspaces/data-model.org`.
   c. Walk all callers of `workspace--make`:
      - `config/workspaces/scaffold.org` (scaffold-then-make)
      - `config/workspaces/tabs.org` (default-path + anchor-existing)
      - `config/workspaces/persistence.org` (deserialiser)
      Confirm none rely on the prior non-canonicalised behaviour.
   d. Walk all `puthash` sites that bypass `workspace--make`. (Per
      `ask-cycle-20260526-171719-2`, the *scratch* fallback test
      uses direct `puthash`; whatever that disposition resolves to
      will determine if a new structural lint is needed.)
   e. Update `register/shape/workspace-plist-v3` in `interfaces.org`:
      add a sentence to the `:home` description pinning the canonical
      form, and update the `validator` block (or scaffold) to assert it.
   f. Spec: add a unit test in
      `config/workspaces/test/workspace-data-model-spec.el` (or
      equivalent) asserting `workspace--make` normalises trailing-
      slash and `~` expansion identically.

3. **Tests**:
   - All existing workspaces specs continue to pass (224 baseline).
   - New unit test asserts canonical form.
   - Run: `./bin/run-tests.sh -d config/workspaces`.

## Design rationale

The cycle-3 reviewer flagged structural-asymmetry of `:name`/`:home`
mutation as a BLOCKING finding; that surfaced the missing
`workspace--set-name` setter. This task captures the same class of
asymmetry one tier up — representation rather than mutation.

Single-source canonicalisation at the constructor (Option A) is
preferred because:
- It cannot be bypassed by a new caller that forgets to canonicalise.
- It makes the shape spec describe a strict invariant rather than a
  convention.
- It allows downstream `equal`-comparison on `:home` to work without
  per-call-site normalisation.

The alternative (Option B) is acceptable if there's an
implementation-side reason to keep `workspace--make` argument-passive
(e.g., it would change the function's contract surface). The
architect should confirm at integrate.

## Verification

- `./bin/run-tests.sh -d config/workspaces` → 225+ specs (one new), 0 failed.
- `grep -rn 'expand-file-name name workspaces-default-parent-directory' config/workspaces/`
  → should not introduce a NEW callsite that bypasses the constructor.
- Manual: `M-x workspace-new alpha`; `C-u M-x workspace-new` → /tmp/test-anchor;
  inspect `workspace--registry` for both — `:home` values should be
  canonical (same form).

## Context

- Reviewer Finding: `.orchestrator/cycles/cycle-20260526-191802/reviews/workspace-new-anchor-existing.md` § Finding 1
- Cited register entry: `register/shape/workspace-plist-v3` (interfaces.org:1035, load_bearing: true)
- Cycle-3 precedent: cycle-3 reviewer BLOCKING on broken-home-tolerance
  identified the `:name`/`:home` mutation-side asymmetry; inline-fixed
  at commit `c6c1b22` with `workspace--set-name` introduction. This
  task captures the same class one representational tier up.

## Integrate-phase extended frame (cycle-20260526-191802)

Architect end-of-cycle audit `arch-cycle-20260526-191802-02`
(informational, shape-fragmentation) extends this task's frame:

### Three producers, not two

The reviewer F1 narrative framed the asymmetry as 1:1 (default-path
vs. anchor-existing). The architect's call-site enumeration found
**three** producers in tree:

1. `workspace--new-default-path`: stores `:home` **WITHOUT**
   trailing slash. `(expand-file-name name parent-dir)`.

2. `workspace--new-anchor-existing` (cycle-4): stores **WITH**
   trailing slash. `(file-name-as-directory (read-directory-name ...))`.

3. `workspace-re-anchor` (cycle-3): the rename branch passes
   `new-home` to `workspace--set-home`; `new-home` is read via
   `read-directory-name` and is **WITH** trailing slash.

So default-path is the *odd one out* (1:2), not 1:1. After any
re-anchor on a default-path workspace, the workspace acquires the
trailing-slash form. The persistence round-trip preserves whatever
form was stored at save time. Treat this in the implementation.

### Latent class: other string-typed slots

The shape entry `register/shape/workspace-plist-v3` is silent on
canonical form for **every** string-typed slot, not just `:home`:

- `:name`: silent on case-sensitivity and whitespace canonicalisation
  (currently de-facto preserved exactly as user-supplied; basename
  derivation pins this de facto, but it is not pinned in the spec).
- `:home`: this task's scope.
- `:recent-layout-group`: silent on canonicalisation; non-path.
- `:buffer-files`: list of strings — each string is a filename, and
  there is no per-element canonicalisation pin (absolute? resolved
  symlinks? double-slash?). **Latent** — no current consumer
  `equal`-compares cross-workspace `:buffer-files`, but a future
  cross-workspace buffer-membership query would surface this.

### Implementation guidance update

If you choose **Option A** (constructor-side canonicalisation at
`workspace--make`), the architect recommends a stronger version
of step (e) "update `register/shape/workspace-plist-v3`":

- Add a `canonical_form` sub-field to **each** string-typed key
  description in the shape spec, not just `:home`. For `:home`:
  `"absolute path; trailing slash present; expand-file-name
  applied; no double-slashes; symlinks NOT resolved (preserve
  user-supplied resolution choice)"`. For `:buffer-files`: pin to
  `"each element absolute, expand-file-name applied"` (current
  de facto). For `:name`: `"exact basename(:home); case-
  sensitive"` (current de facto).
- The `validator` block should assert these structurally — adding
  the `canonical_form` field is half the work; the corresponding
  `workspace--make` argument-normalisation is the other half.

If you choose **Option B** (shape-spec pin only, call-site
canonicalisation), the same multi-slot frame applies: pin each
slot's form in the shape entry's description, and add a comment
to each producer call site documenting the normalisation it
performs to satisfy the shape.

### PM-trends frame

Two cycles in a row have surfaced a shape-tier asymmetry on
`register/shape/workspace-plist-v3` (cycle-3: `:name`/`:home`
lockstep mutation; cycle-4: `:home` representational form). If
cycle-5 or later surfaces a third instance, the PM will propose
a between-cycle architect audit of the entire shape entry. This
task is the first opportunity to **resolve the second instance
and pre-empt a third** by addressing the latent class while the
constructor-side change is open.


## Observations

**Approach chosen: Option A (constructor-side canonicalisation).**

Implementation widened the attachment point from `workspace--make`
alone to **both** `workspace--make` and `workspace--set-home`. The
architect-enumerated third producer (`workspace-re-anchor`) reaches
`:home` via `workspace--set-home`, not via `workspace--make`; without
canonicalising the setter the re-anchor path would smuggle the
non-canonical form back into the registry. The shape contract becomes
"both attachment points pin the canonical form" rather than "the
constructor pins it."

**Three producers confirmed in code** (matching architect's
`arch-cycle-20260526-191802-02` enumeration):

1. `workspace--new-default-path` (config/workspaces/tabs.org:268) —
   reaches `:home` via `workspace--make`. Pre-canonicalisation it
   passed the unslashed `(expand-file-name name parent-dir)` form;
   post-canonicalisation the constructor normalises silently.
2. `workspace--new-anchor-existing` (config/workspaces/tabs.org:229)
   — reaches `:home` via `workspace--make`. Already passed
   `(file-name-as-directory (read-directory-name ...))` (already
   canonical); the constructor's canonicalisation is a no-op on its
   input.
3. `workspace-re-anchor` (config/workspaces/workspaces.org:151) —
   reaches `:home` via `workspace--set-home`. Already passed
   `(file-name-as-directory (read-directory-name ...))` in its
   interactive form (already canonical); the setter's
   canonicalisation is a no-op on its input.

**Architect's extended scope — implemented fully on `:home`,
documented for the other string-typed slots.**

- `:home` — structurally pinned at the constructor and setter;
  validator block extended with a `home-not-canonical` check.
- `:name` — `canonical_form` sub-field added to the shape entry
  pinning the de-facto `basename(:home)` contract. No constructor-
  side change: the canonical call site already synthesises
  `NAME = basename(HOME)`, and `register/invariant/registry-name-
  equals-basename` already pins it elsewhere.
- `:buffer-files` — `canonical_form` sub-field added pinning the
  de-facto "each absolute, `expand-file-name` applied" form as
  documentation only. Not enforced structurally because no consumer
  cross-workspace-equality-compares `:buffer-files` today;
  surfaced as a latent-class follow-up in `## Discoveries` for the
  on-touch architect.
- `:recent-layout-group` — `canonical_form` sub-field added
  documenting "non-path; no canonicalisation."

**Existing test fixtures touched.** Five existing test assertions
were updated to reflect the new canonical form (no fixture rewrite
required; the assertions were directly testing the old non-canonical
output of `workspace--make`):

- `config/workspaces/test/data-model-home-spec.el` — 4 expectations
  on `(workspace--home ws)` updated from `"/tmp/foo"` etc. to
  `"/tmp/foo/"` etc. (consequence of constructor canonicalisation).
- `config/workspaces/test/workspace-new-default-spec.el` — 1
  expectation updated to compare against
  `(file-name-as-directory home)` instead of `home` (default-path's
  pre-canonicalisation form was the odd-one-out per the architect
  enumeration).

No fixture bypassed `workspace--make` in a way that would have hidden
the canonicalisation; every existing test goes through the
constructor or `workspace--set-home`, so the behavior change is
uniformly visible. No tests use `(list :name ... :home ...)`
hand-construction that would have needed pre-canonicalisation.

**Persistence loader intentionally NOT canonicalised.** The
deserialiser in `persistence.org` preserves `:home` verbatim from
disk — this is the behavior pinned by `broken-home-load-spec.el`
line 102 ("`:home` is preserved verbatim (the user may want to
inspect or restore the path)"). Files written post-cycle-5 will
already be canonical on read; corrupted or hand-edited files surface
the offending path in `*Messages*` rather than being silently
normalised. The shape validator's new `home-not-canonical` check
would surface such drift to any consumer that calls the validator.

## Discoveries

- discovery_id: disc-canonicalize-workspace-home-path-form-1
  class: shape-fragmentation
  description: |
    Architect's extended frame called out three producers, not two
    (cycle-3 `workspace-re-anchor` is the third, reaching `:home`
    via `workspace--set-home`, not via `workspace--make`). Confirmed
    in code. Single-attachment-point canonicalisation at the
    constructor alone would have left the re-anchor path producing
    canonical `:home` only by accident (because `workspace-re-anchor`'s
    interactive form already wraps its `read-directory-name` result
    in `file-name-as-directory`). Implementation chose to canonicalise
    both `workspace--make` and `workspace--set-home` so the shape
    invariant is structurally enforced at every entry point a `:home`
    value can enter the registry through.
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    The shape entry's `producers` list now enumerates three
    attachment points (`workspace--make`, `workspace--set-home`,
    `workspace--persistence-deserialize-workspace`), where previously
    only two were listed (`workspace--make` and the deserialiser).
    The integrate-phase architect should confirm this expansion of
    the producer enumeration is correct.

- discovery_id: disc-canonicalize-workspace-home-path-form-2
  class: invariant-gap
  description: |
    The `:buffer-files` slot has a de-facto canonical form (each
    element absolute, `expand-file-name` applied) but no structural
    enforcement attachment point. `workspace--add-buffer-file`
    forwards the caller's path unchanged. The architect's extended
    frame called this out as a latent-class pin: no consumer
    cross-workspace-equality-compares `:buffer-files` today, but a
    future buffer-membership-across-workspaces query would surface
    drift. Implementation pinned the canonical form in the shape
    entry as documentation-only; structural enforcement deferred.
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    Defer structural enforcement to the cycle where a consumer
    actually emerges that cross-workspace-equality-compares
    `:buffer-files`. At that point, add `(expand-file-name path)`
    inside `workspace--add-buffer-file` and tighten the test
    assertions in `buffer-membership-spec.el`. Keep the
    `canonical_form` field in the shape entry as documentation
    pin in the meantime; the validator's symmetric check
    (`buffer-files-not-canonical`) is the natural sibling but is
    not yet useful (no producer would fail it today).

- discovery_id: disc-canonicalize-workspace-home-path-form-3
  class: spec-signal
  description: |
    The persistence deserialiser's verbatim `:home` preservation is
    pinned by `broken-home-load-spec.el` line 102 as a contract,
    not an accident. After cycle-5 canonicalisation, files written
    by the constructor will always be canonical, so the contract's
    practical scope narrows to (a) corrupted files and (b) hand-
    edited files. The shape validator's new `home-not-canonical`
    check is the natural detector for files in either category.
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    The integrate-phase architect should consider whether the
    deserialiser's verbatim-preservation contract is still load-
    bearing post-canonicalisation, or whether it could be tightened
    to "preserve verbatim AND emit a notice if non-canonical" so the
    drift signal surfaces in `*Messages*` automatically rather than
    requiring an external validator call.


