---
name: disposition-delete-vs-write-bucket
description: "User-decision task (finding-10C). Decide whether :delete should remain collapsed into WRITE or get its own GPTEL_SCOPE_DELETE drawer key."
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:rewire-expansion-writer
  - discovered-from:implement-drawer-writer
---

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
