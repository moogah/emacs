---
name: drawer-identity-resolver
description: Add drawer-first / basename-fallback resolution of session-id, branch-name, and session-type; demote jf/gptel--session-id-from-directory to the fallback path.
change: gptel-content-addressed-session-activation
status: done
merge_commit: 1ec479f
relations:
  - "blocked-by:drawer-signature-and-head-read"
---

## Files to modify

- `config/gptel/sessions/filesystem.org` (modify) — add `jf/gptel--resolve-session-id`, `jf/gptel--resolve-branch-name`, and `jf/gptel--session-type` (or equivalent). Keep `jf/gptel--session-id-from-directory` as the fallback.
- `config/gptel/sessions/test/filesystem/identity-resolution-spec.el` (new) — Buttercup specs for drawer-first, basename fallback, and type inference.

## Implementation steps

1. Write the spec first. Cover:
   - drawer carries `:GPTEL_SESSION_ID:` → resolver returns it; the directory basename is NOT consulted (use a fixture whose basename differs from the drawer id to prove independence);
   - drawer omits `:GPTEL_SESSION_ID:` → resolver falls back to `jf/gptel--session-id-from-directory` (basename);
   - branch: drawer `:GPTEL_BRANCH:` wins, else the `branches/<branch>/` path segment;
   - type: drawer carrying `:GPTEL_PARENT_SESSION_ID:` → `agent`; absent → `branch`.
2. Implement the resolvers. Each takes the buffer (or a parsed drawer alist from `jf/gptel--read-session-drawer-head` / the current buffer's drawer) plus the file/dir path for fallback:
   - `jf/gptel--resolve-session-id`: drawer `GPTEL_SESSION_ID` else `(jf/gptel--session-id-from-directory <session-dir>)`.
   - `jf/gptel--resolve-branch-name`: drawer `GPTEL_BRANCH` else the enclosing `branches/<branch>/` segment (extract with a single targeted regex, NOT the old 3-layout matcher).
   - `jf/gptel--session-type`: `agent` when `GPTEL_PARENT_SESSION_ID` present, else `branch`.
3. Leave `jf/gptel--session-id-from-directory` (`filesystem.org:124`) intact but referenced only from the fallback branch; add a docstring note that it is the legacy/back-compat path.
4. Tangle and run the spec.

## Design rationale

Identity becomes drawer-resident and move-safe for new sessions while old sessions keep working with zero rewrite — the basename/segment fallback is the no-migration grace path the beta constraint requires. Inferring type from `:GPTEL_PARENT_SESSION_ID:` removes the last need for path-layout discrimination (the old branch/nested-agent/flat-agent regex fork). (design.md §Decisions D2, D3; constraint "Beta, no-migration default".)

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/filesystem.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem` — green, including the "basename differs from drawer id" independence case.
- Done = resolvers return drawer values when present and fall back correctly when absent; type inferred from parent-id.

## Context

design.md § Decisions "D2. Identity" and "D3. Agent identity"; specs `sessions-persistence` Requirement "Drawer-resident session identity" and "Buffer-local session state".

## Cycle 1 updates (cycle-1781448273)

> Cycle 1 confirmed the producer + recognition substrate this task consumes.

- **Input shape is now concrete.** `jf/gptel--read-session-drawer-head` (merged) returns an
  **alist keyed by the bare key string** with string values, e.g.
  `(("GPTEL_SESSION_ID" . "<id>") ("GPTEL_BRANCH" . "main"))`, nil when no point-min drawer.
  Resolvers should consume it via `assoc`/`(cdr (assoc "GPTEL_SESSION_ID" alist))`. The buffer
  predicate path can reuse the engine `jf/gptel--scan-session-drawer-keys` (current buffer).
- `register/vocabulary/identity-drawer-keys` → **confirmed**; the writers now emit all three keys.
- `register/boundary/drawer-first-identity-resolution` remains **speculated** — THIS task builds
  its resolvers (`--resolve-session-id` / `--resolve-branch-name` / `--session-type`); it carries
  the disposition for that entry next cycle.
- **Legacy-fallback (meta M2):** under beta/no-migration, the fallback branch (basename / path
  segment) must handle drawers that carry *no* identity key at all — see
  `register/invariant/branch-drawer-shares-id-not-branch` (reconciled: set = replace-or-insert).
- `jf/gptel--session-type` is NOT yet defined in code (the canonical_mapping_function in the
  register entry is the spec); implement it here as part of the resolvers.
