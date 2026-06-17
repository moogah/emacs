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
