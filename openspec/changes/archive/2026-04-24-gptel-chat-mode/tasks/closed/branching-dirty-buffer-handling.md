---
name: branching-dirty-buffer-handling
description: Handle unsaved changes in source session buffer before deriving branch-point positions against on-disk file
change: gptel-chat-mode
status: done
relations:
  - discovered-from:sessions-branching
---

## Files to modify
- `config/gptel/sessions/branching.org` (modify
  `jf/gptel-branch-session` or `jf/gptel--copy-truncated-context`)
- `config/gptel/sessions/branching.el` (tangled)
- `config/gptel/sessions/test/branching/branching-integration-spec.el`
  (add regression coverage)

## Implementation steps
1. Decide the policy between three options (pick whichever matches
   the rest of the sessions subsystem's convention — likely option
   A, mirroring how other sessions commands handle dirty buffers):
   - **A. Save before branching.** Call `(save-buffer)` (or
     `(basic-save-buffer)`) on the parent session buffer before
     selection / copy. Simple, preserves all user edits in the
     parent; matches the common Emacs pattern of "write through
     before destructive operation." Downside: user loses opt-out.
   - **B. Error on dirty buffer.** `(when (buffer-modified-p)
     (user-error "Save the session buffer before branching"))`.
     Maximally conservative; forces the user to make the save
     decision explicitly.
   - **C. Copy from the live buffer instead of disk.** Replace
     `insert-file-contents` with `insert-buffer-substring` against
     the parent buffer's `(current-buffer)`. Positions and content
     are derived from the same source. Downside: the *parent
     session.org file* on disk diverges from the branch's apparent
     provenance until the user saves the parent.
2. Apply the chosen policy in `jf/gptel-branch-session` (before the
   `-select-branch-point` call, so positions and the copied content
   come from the same source of truth).
3. Add a `branching-integration-spec.el` spec:
   - Create a session, modify the buffer without saving, invoke
     the branch-creation flow, and assert that the new branch's
     `session.org` reflects (option A or C) the in-memory content,
     or (option B) the branch flow aborts with a `user-error`.

## Design rationale
The current implementation derives branch-point positions from the
live source buffer via `gptel-chat--parse-buffer`, then passes those
positions to `jf/gptel--copy-truncated-context`, which reads bytes
from the **on-disk** `session.org` via `insert-file-contents`. If
the user has unsaved edits in the parent session, positions computed
against the live buffer will not match the disk file's byte layout,
and the new branch will contain a silently truncated (or mis-aligned)
snapshot.

This is pre-existing: the `gptel-mode`-era bounds implementation had
the same issue. The rewrite was a natural opportunity to fix it but
did not — so this task picks up the open thread. Not a Decision 18
violation; it's a sequence-of-operations gap.

Severity: moderate. Silent byte misalignment at branch time is a
data-integrity concern, but user-recoverable (the live parent buffer
still holds the authoritative content). Real-world users are likely
in the habit of saving before branching anyway, which is why the
issue has not surfaced in smoke tests.

## Design pattern
Keep the position-source and the content-source identical. Either
snapshot the buffer (option A or C) before computing positions, or
refuse to proceed on a dirty buffer (option B). Never compute
positions from one source and copy bytes from another.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/branching.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/branching` passes,
  including the new dirty-buffer regression spec.
- Manual smoke: type a partial new user prompt into an existing
  session buffer **without saving**, invoke `jf/gptel-branch-session`,
  confirm the new branch's `session.org` matches the chosen policy
  (option A: includes the unsaved text; option B: aborts with
  user-error; option C: includes the unsaved text without touching
  the parent file on disk).

## Context
- Review of sessions-branching (2026-04-21, orch session
  `orch-review-1776796835`) Finding #1.
- `config/gptel/sessions/branching.el:209-243` —
  `jf/gptel-branch-session` orchestration; position arithmetic
  diverges from `insert-file-contents`-sourced bytes.
- design.md §Decision 18 (session file format is `.org`) —
  unaffected; this is about source-of-truth for the copy operation,
  not the file format.

## Review

Reviewed inline 2026-04-23 (orch-review-1777032xxx).

- Fix is minimal and correct: `(when (buffer-modified-p) (save-buffer))`
  placed before `-select-branch-point`, ensuring the position-source
  (live buffer) and content-source (on-disk file) are byte-identical.
- Option A choice aligns with the sessions subsystem's persistence
  convention (other flows treat `save-buffer` as the canonical flush
  point rather than prompting).
- Regression test (`branching-integration-spec.el` §dirty-buffer
  handling) exercises the real `jf/gptel-branch-session` path with a
  dirty buffer: pre-asserts the dirty state (so the test actually
  distinguishes the regression from the clean-buffer path), then
  post-asserts (a) save fired and (b) the new branch's session.org
  includes the previously-unsaved turn.
- `cl-letf` mocks for `derived-mode-p`, `completing-read`, `y-or-n-p`,
  and `find-file` are scoped to the spec's function-under-test call.

Findings: none. Follow-ups: none.
