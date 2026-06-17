---
name: session-write-work-root
description: create-session-core writes GPTEL_WORK_ROOT from the project-root input
change: gptel-work-root-default-directory
status: ready
relations: []
---

## Files to modify
- `config/gptel/sessions/commands.org` (modify) — `jf/gptel--create-session-core`

## Implementation steps
1. In `jf/gptel--create-session-core` (≈`commands.org:544`), the function already
   receives `project-root` and threads it into
   `jf/gptel-scope-profile--create-for-session` to expand `${project_root}` into the
   `GPTEL_SCOPE_*` drawer keys. Add a second, verbatim persistence of that same
   value as the `GPTEL_WORK_ROOT` drawer key.
2. After the `drawer-text` is built and the identity keys are spliced (the existing
   `jf/gptel--append-drawer-property` calls for `GPTEL_SESSION_ID` / `GPTEL_BRANCH`),
   add an append for `GPTEL_WORK_ROOT` whose value is `project-root` normalized to an
   absolute path: `(when project-root (expand-file-name project-root))`. Skip the key
   when `project-root` is nil (keyless session — the binder will fall back to
   `branch-dir`).
3. Use the same splice helper so the `:PROPERTIES:` / `:END:` adjacency invariant is
   preserved: `(jf/gptel--append-drawer-property drawer-text "GPTEL_WORK_ROOT" <abs>)`.
4. Do NOT introduce a second input parameter — `GPTEL_WORK_ROOT` is a *derived output*
   of the existing `project-root` input, not a new concept.
5. Tangle + validate: `./bin/tangle-org.sh config/gptel/sessions/commands.org`.

## Design rationale
The cwd bug exists because `default-directory` falls through to the bookkeeping
`branch-dir`. The fix begins by recording the project the model means. By writing
`GPTEL_WORK_ROOT` from the *same* `project-root` value that expands `GPTEL_SCOPE_*`,
the working directory and the scope boundary derive from one input and cannot
disagree — the cwd↔scope agreement invariant is structural, not enforced (design D1).
Stored absolute/verbatim to match how `${project_root}` already expands (design D3).

## Design pattern
Follow the existing identity-key emission in the same function: the
`(drawer-text (jf/gptel--append-drawer-property drawer-text "GPTEL_SESSION_ID" session-id))`
and `"GPTEL_BRANCH"` splices are the exact pattern to copy. The append helper keeps the
key inside the drawer block.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/commands.org` passes (check-parens).
- New/extended Buttercup spec under `config/gptel/sessions/test/commands/`:
  - WHEN a session is created with `project-root /Users/x/proj` THEN the written
    `session.org` drawer contains `:GPTEL_WORK_ROOT: /Users/x/proj` (absolute).
  - AND the `GPTEL_WORK_ROOT` value equals the root used to expand the
    `GPTEL_SCOPE_*` write patterns (agreement-by-construction).
  - WHEN `project-root` is nil THEN no `:GPTEL_WORK_ROOT:` line is emitted.
- Run: `./bin/run-tests.sh -d config/gptel/sessions`.

## Context
design.md § Decisions 'D1 — Work root is a second persisted output of the project-root input'
specs/sessions-persistence/spec.md § 'Requirement: Work-root drawer key'

## Observations

- Implemented exactly per the prescribed approach: a derived-output
  `GPTEL_WORK_ROOT` append spliced after the existing `GPTEL_BRANCH`
  append in `jf/gptel--create-session-core`, gated on `project-root`
  being non-nil, value `(expand-file-name project-root)`. No new
  parameter introduced.
- The docstring's `Creates:` section was extended to document the new
  conditional key so the literate source stays accurate (the docstring
  already enumerated the other drawer keys; omitting this one would have
  left a documentation gap). This is a doc-only edit, no behaviour
  change.
- The branch writer (`jf/gptel--create-branch-session`, case 3 in
  identity-keys-emission-spec.el) verbatim-COPIES the parent drawer and
  then REPLACES identity keys. It does NOT touch `GPTEL_WORK_ROOT`, so a
  branch inherits the parent's work root by copy. This is consistent
  with the agreement invariant (a branch shares the parent's scope keys
  too, by copy) but is OUT OF THIS TASK'S SCOPE — flagged as a discovery
  below for the orchestrator to confirm whether a branch-side spec is
  wanted. No code touched there.
- Agreement-by-construction is asserted structurally: the spec mocks
  `jf/gptel-scope-profile--create-for-session`, captures the
  `project-root` it receives, and proves the written `GPTEL_WORK_ROOT`
  equals `(expand-file-name <that same arg>)`. Using a relative-form
  input (`"proj"` with a fixed `default-directory`) exercises the
  normalisation, so the test is not a tautology over an already-absolute
  literal — both writers are shown to agree on the SAME absolute string.
- Latent/adjacent: `expand-file-name` resolves against `default-directory`
  at session-creation time for a relative `project-root`. In production
  the interactive caller passes a projectile project root (already
  absolute), so this is benign, but a relative `project-root` from a
  programmatic caller would bind the work root to whatever
  `default-directory` happened to be. Not changed — the task explicitly
  specifies `(expand-file-name project-root)`.

## Discoveries

- discovery_id: disc-session-write-work-root-1
  class: spec-signal
  description: |
    The agreement-by-construction invariant is now confirmable for the
    create-session-core path, but the BRANCH writer
    (jf/gptel--create-branch-session) propagates GPTEL_WORK_ROOT only by
    verbatim drawer copy — it never re-derives it and has no test
    asserting the branch's GPTEL_WORK_ROOT still equals the branch's
    GPTEL_SCOPE_* root. The invariant statement is scoped to
    "any session created by jf/gptel--create-session-core", so branches
    are arguably out of scope, but branches are sessions a user works in.
  affected_register_entry: register/invariant/cwd-scope-agreement
  recommendation: |
    Confirm the invariant for create-session-core (this task is its
    enforcement gate). Separately decide whether the invariant's scope
    should be widened to branch creation, and if so, file a follow-up
    task for a branch-side agreement spec. Do NOT widen silently here.

- discovery_id: disc-session-write-work-root-2
  class: deviation
  description: |
    The cited register entry says enforcement is "test (no runtime guard
    — the equality is structural)". Implementation confirms this is
    correct: because both outputs read the single `project-root` lexical,
    there is no code path where they can diverge for a fixed input. The
    only residual coupling is `expand-file-name`'s dependence on
    `default-directory` for a RELATIVE project-root — but that affects
    BOTH writers identically (the scope renderer also expands against the
    same root), so agreement holds regardless. No push-back; the
    SPECULATED entry is accurate as written.
  affected_register_entry: register/invariant/cwd-scope-agreement
  recommendation: |
    Promote register/invariant/cwd-scope-agreement from SPECULATED to
    CONFIRMED for the create-session-core path. The structural-equality
    claim is sound; the agreement-by-construction spec is the gate.
