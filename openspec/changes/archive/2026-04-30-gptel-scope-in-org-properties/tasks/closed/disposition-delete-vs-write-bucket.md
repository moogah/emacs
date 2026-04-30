---
name: disposition-delete-vs-write-bucket
description: "User-decision task (finding-10C). Decision recorded cycle-2 plan (2026-04-29): Path 2 — keep current WRITE collapse. Recovery-asymmetry tradeoff documented; users wanting deletion-locked-out add the file pattern to GPTEL_SCOPE_DENY explicitly."
change: gptel-scope-in-org-properties
status: done
relations:
  - discovered-from:implement-drawer-writer
---

## Decision (recorded cycle-2 plan, 2026-04-29)

**Path 2 chosen**: keep current WRITE collapse.

Granting `:GPTEL_SCOPE_WRITE:` on `/workspace/**` continues to imply
both overwrite and delete on matching paths. The recovery-asymmetry
between overwrite (VCS-recoverable) and delete (often not) is
accepted as a known limitation.

The deliverable for this task is the decision itself, recorded to:

- `interfaces.org` :: `register/vocabulary/operation-to-drawer-key`
  — `:delete` `decision_note` documents the accepted tradeoff and
  the explicit-deny escape hatch.

No drawer-key-set or scope-config-plist changes (no new bucket).
No follow-up implementation task.

---

## Original task (preserved for context)

## Cites register entries

- `register/vocabulary/operation-to-drawer-key` — current entry has `:delete → GPTEL_SCOPE_WRITE` with an `open_question` annotation.
- `register/vocabulary/drawer-key-set` — would gain an 8th member if Path 1 lands.
- `register/shape/scope-config-plist` — the `:paths` sub-plist would gain a `:delete` field if Path 1 lands.

## Background

Architect audit finding `arch-cycle-1777460733-10` (sub-case C): `rm
/workspace/file.txt` (operation `:delete`) and `echo X >
/workspace/file.txt` (operation `:write`) both end up in
`GPTEL_SCOPE_WRITE`. After the user grants WRITE on `/workspace/**`,
the agent can both *delete* and *overwrite* anything matching that
pattern.

Threat models for delete and write are not equivalent: a write modifies
content (recoverable from VCS / backups in many cases); a delete
removes the file from the working directory (recoverable only if the
file was tracked, and only by an explicit recovery step). A user
granting write may not have intended to grant delete.

## Two resolution paths

1. **Add `GPTEL_SCOPE_DELETE` as a 6th list-shaped drawer key.** The
   validator treats `:delete` as requiring `paths.delete` (no fallback
   to `paths.write`). Users granting deletion do so explicitly,
   separately from creation/overwrite. The expansion UI offers
   "add to delete scope" as a distinct action when `:operation` is
   `:delete`.

2. **Keep current WRITE collapse.** Accept the tradeoff; document
   that WRITE includes delete in the canonical mapping function's
   docstring and the register entry. Users who want narrower grants
   must use `--add-bash-to-scope` with explicit per-command syntax.

## Decision required

Routes to the user via the integrate→plan handshake.

## Implementation work (after decision)

- **Path 1**:
  - Extend `register/vocabulary/drawer-key-set` (8th member;
    `GPTEL_SCOPE_DELETE`).
  - Extend `register/shape/scope-config-plist` `:paths` to include
    `:delete`.
  - Update `--map-operation-to-drawer-key`'s `:delete` arm to return
    `"GPTEL_SCOPE_DELETE"`.
  - Update `--load-from-buffer` and `--load-from-file` to read the
    new key.
  - Update `--render-drawer-text` and `--apply-to-drawer` to emit it.
  - Update the validator's path-operation check to consult `paths.
    delete` for `:delete` operations (no WRITE fallback).
  - Add a buttercup spec for the new key's round-trip and a
    spec asserting that a WRITE grant does not authorise a DELETE.
- **Path 2**: no code change; update register entry's
  `open_question` to `accepted-tradeoff` with explicit rationale in
  the canonical mapping function's docstring.

## Verification

After implementation, run `./bin/run-tests.sh -d config/gptel/scope`.
