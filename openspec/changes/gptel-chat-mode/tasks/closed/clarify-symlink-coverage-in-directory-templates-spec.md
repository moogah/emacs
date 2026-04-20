---
name: clarify-symlink-coverage-in-directory-templates-spec
description: Either add a current-symlink assertion or document its scope omission in commentary
change: gptel-chat-mode
status: done
relations:
  - discovered-from:sessions-filesystem
---

## Files to modify
- `config/gptel/sessions/test/filesystem/directory-templates-spec.el`
  (modify — file-level commentary around lines 10-21, optionally add a
  spec for symlink resolution)

## Implementation steps
Pick option (a) or (b):

**Option (a) — add the assertion (preferred):**
1. After a branch directory is created, call
   `jf/gptel--update-current-symlink` on the session root.
2. Add a `(it ...)` block under "Sessions root / branches tree
   integrity" that verifies `(file-symlink-p ...)` and that following
   `current/session.org` resolves to `branches/<branch>/session.org`.

**Option (b) — document the scope gap:**
1. Edit the file-level commentary to explicitly state that `current`
   symlink behaviour is unchanged per Decision 19 and out of scope for
   this spec; pointer to wherever symlink behaviour is exercised.

## Design rationale
The new test's commentary lists what it covers but omits the `current`
symlink. The actual code does not exercise
`jf/gptel--update-current-symlink`, so a regression in which `current`
points at `branches/<name>/session.org` missing (or accidentally points
at a `session.md`) would not surface here. The task body's exclusion
("`current` symlink behaviour is unchanged") is correct, but the test
file itself doesn't carry that disclaimer, so a future reader inferring
coverage from the spec would be misled.

Option (a) is preferred because the symlink is an integral part of
"on-disk structure" that the task body asks the test to verify.

## Verification
- The commentary or new spec accurately describes what the test does.
- If option (a): `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem`
  passes with the new assertion.

## Context
- Review of sessions-filesystem (orchestrator session 2026-04-20)
  Finding #6
- directory-templates-spec.el commentary lines 10-21
