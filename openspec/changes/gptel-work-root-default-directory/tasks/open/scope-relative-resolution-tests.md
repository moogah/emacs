---
name: scope-relative-resolution-tests
description: Verify relative paths resolve against default-directory (work root); no validator change
change: gptel-work-root-default-directory
status: ready
relations:
  - blocked-by:binder-default-directory
---

## Files to modify
- `config/gptel/scope/test/` (new spec, or extend an existing path-resolution spec) —
  e.g. `config/gptel/scope/test/validation/path-resolution-spec.el`

## Implementation steps
1. This task adds VERIFICATION ONLY — no production code change. Grounding confirmed
   the tools already resolve relative paths against the ambient `default-directory`:
   - filesystem tools use bare `(expand-file-name filepath)`
     (`scope-filesystem-tools.org:81,124,172`);
   - bash resolves against `default-directory` (`scope-validation.org:299` passes
     `directory default-directory`; `:420` does `(expand-file-name path directory)`).
   The scope spec is therefore "affirm, not change."
2. Add Buttercup specs that bind `default-directory` to a work root and assert that a
   RELATIVE path argument resolves under it before validation:
   - With `default-directory = /Users/x/proj/` and `paths.read` containing
     `/Users/x/proj/**`, a relative `config/x.el` validates as the absolute
     `/Users/x/proj/config/x.el` and is ALLOWED.
   - With the SAME relative path but `default-directory = /Users/x/other/`, it resolves
     to `/Users/x/other/config/x.el` (resolution follows the work root, not the
     bookkeeping dir).
3. Assert the tool execution path also uses that `default-directory` (the bash
   validator reads `directory default-directory`; confirm a relative-path command's
   extracted op is judged against the work-root-resolved absolute).
4. Do NOT duplicate the absolute-path / fail-closed coverage — that is already green in
   the validation suite (195/195) and verified.

## Design rationale
Fixing `default-directory` (the binder task) is what makes "the path the validator
judges" equal "the path the model meant." This task locks that behavior with tests so
a future regression in tool path-resolution is caught. The validator's allow/deny
logic, deny-precedence, and fail-closed behavior are unchanged — only WHICH directory
relative paths resolve against is being pinned (design D4).

## Design pattern
Follow the existing `config/gptel/scope/test/validation/path-resolution-spec.el` and
`path-validation-spec.el` structure (Buttercup `describe`/`it`/`expect`). Use a
`let`/`cl-letf`-bound `default-directory` per the project's behavioral-test convention
(mocks scoped to the function under test).

## Verification
- Run: `./bin/run-tests.sh -d config/gptel/scope` — all green, including the new specs.
- The new specs fail if `default-directory` is ignored (e.g. if a tool ever hardcodes a
  resolution root).

## Context
design.md § Decisions 'D4 — Tools inherit the work root through the existing buffer context'
specs/scope/spec.md § 'Requirement: Working-directory path resolution and cwd–scope agreement'

## Observations

- VERIFICATION ONLY — no production code touched. The grounding held exactly
  as stated: both seams resolve relative paths against the ambient
  `default-directory`, and the new specs are the read-side end-to-end proof.

- New spec: `config/gptel/scope/test/validation/relative-resolution-spec.el`
  (6 specs, all green). Hand-written `.el` to match every sibling under
  `config/gptel/scope/test/validation/` (all hand-written `.el`, no `.org`
  tangling for test specs). Requiring `jf-gptel-scope-shell-tools`
  transitively requires `jf-gptel-scope-validation`, where both validator
  entry points live.

- The two seams are covered by entry points that READ `default-directory`
  implicitly, which is what distinguishes this file from the sibling
  `path-resolution-spec.el` (that one passes an EXPLICIT `directory` arg):
  - Filesystem read-side: `jf/gptel-scope--validate-filesystem-tool`
    (scope-validation.org:244) takes NO directory argument; its scope check
    does bare `(expand-file-name filepath)` against ambient `default-directory`.
    The tool BODIES likewise use bare `(expand-file-name filepath)`
    (scope-filesystem-tools.org:81,124,172), so validation and execution
    resolve identically — the cwd<->scope agreement is by construction.
  - Bash execution-side: `jf/gptel-scope--validate-bash-tool`
    (scope-validation.org:299) reads `directory default-directory` itself and
    threads it to `jf/gptel-scope--validate-file-operation`, which resolves
    relative op paths with `(expand-file-name path directory)`
    (scope-validation.org:420-421).

- Test bite is structural, not incidental: each "different work root" case
  asserts the validator's denial `:resource` equals the work-root-relative
  ABSOLUTE (e.g. `/Users/x/other/config/x.el`). If any tool hardcoded a
  resolution root or ignored `default-directory`, the resolved resource would
  not track the bound work root and these assertions would fail. The dual
  ("grant the other root, same relative arg, now allowed") confirms the verdict
  flips with the binding, ruling out a fixed `/Users/x/proj` baked anywhere.

- Did NOT duplicate absolute-path / fail-closed coverage (already green in
  `path-validation-spec.el` / `path-resolution-spec.el`).

- Suite summary: `./bin/run-tests.sh -d config/gptel/scope/test/validation`
  → 201 specs, 0 failed (includes the 6 new specs).
  `./bin/run-tests.sh -d config/gptel/scope` → 523 specs, 3 failed. The 3
  failures are pre-existing and unrelated to this task: all in
  `config/gptel/scope/test/integration/bash-multi-violation-expansion-spec.el`
  ("multi-violation add-to-scope leaks subsequent denials" / drawer
  add-to-scope cluster), a file this task does not touch. The new specs are
  all green.

## Discoveries

- discovery_id: disc-scope-relative-resolution-tests-1
  class: spec-signal
  description: |
    The cwd<->scope-agreement invariant is now read-side proven at TWO entry
    points that both consult the ambient `default-directory` implicitly
    (filesystem `validate-filesystem-tool` via bare `expand-file-name`; bash
    `validate-bash-tool` via `(directory default-directory)`), confirming the
    enforcement mechanism end-to-end: a relative path resolves under the bound
    work root and lands in scope, and the SAME relative path under a different
    work root resolves differently. The work-root-activation-seam claim that
    "one buffer-local default-directory set reaches every tool for both
    validation and execution" is consistent with the code: the filesystem
    tool's scope check and its tool body both use the identical bare
    `(expand-file-name filepath)` form, so validation and execution cannot
    disagree on the resolved path.
  affected_register_entry: register/invariant/cwd-scope-agreement
  recommendation: |
    No change needed — affirmed. Keep these specs as the regression canary;
    they fail if any tool ever hardcodes a resolution root or stops reading
    `default-directory`.

- discovery_id: disc-scope-relative-resolution-tests-2
  class: interface-drift
  description: |
    Asymmetry worth noting (not a defect for this task): the bash path threads
    an EXPLICIT `directory` argument from `validate-bash-tool`
    (scope-validation.org:299 → :323 → :407) down to
    `validate-file-operation`, which resolves relative op paths against that
    arg. The filesystem path has NO such argument — its
    `validate-filesystem-tool` relies entirely on bare `(expand-file-name
    filepath)` reading `default-directory` from dynamic scope. Both currently
    resolve against the same `default-directory` because `validate-bash-tool`
    seeds `directory` FROM `default-directory`, so the two seams agree today.
    But the two code paths express "the work root" differently (explicit
    parameter vs. ambient dynamic var). A future refactor that changed how
    `validate-bash-tool` sources `directory` (e.g. from a plist/session field
    instead of `default-directory`) could silently diverge the bash seam from
    the filesystem seam without tripping these specs in isolation — the bash
    specs would still pass while the filesystem specs measured a different
    root.
  affected_register_entry: register/boundary/work-root-activation-seam
  recommendation: |
    Out of scope for this verification-only task; do not change. If the
    work-root sourcing in `validate-bash-tool` is ever refactored, add a
    cross-seam spec asserting that for one bound `default-directory` the
    filesystem and bash validators resolve the SAME relative path to the SAME
    absolute. For now the seam is single-sourced (`directory default-directory`)
    and the agreement holds by construction.
