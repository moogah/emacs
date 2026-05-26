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


