---
name: tighten-sessions-persistence-spec-on-iteration
description: Spec language "old files are simply not found" is imprecise at session-iteration boundary
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:sessions-filesystem
---

## Files to modify
- `openspec/specs/gptel/sessions-persistence.md` (modify — language
  around legacy file invisibility)
- Possibly `config/gptel/sessions/filesystem.org` (modify
  `jf/gptel--valid-session-directory-p` if the spec sharpens the
  contract)

## Implementation steps
1. In `sessions-persistence.md`, replace the imprecise claim that "old
   files are simply not found by helpers looking for session.org" with
   precise language. The honest description is:
   - Branch directories without `session.org` are rejected by
     `jf/gptel--valid-branch-directory-p` and therefore invisible to
     `jf/gptel--find-all-branches-with-agents`.
   - Session directories whose branches all use legacy `session.md`
     still pass `jf/gptel--valid-session-directory-p` (which only checks
     for `branches/` existence). Iteration over branches inside such a
     session yields zero valid branches.
2. Decide whether to tighten `jf/gptel--valid-session-directory-p` to
   require at least one branch with `session.org`, or to leave the
   semantics permissive. Recommended: keep permissive, document that
   consumers of branch enumeration must handle empty branch lists.
3. If the spec is sharpened, update the docstring of
   `jf/gptel--valid-session-directory-p` accordingly.

## Design rationale
Decision 19's "clean break — old files remain on disk and are simply
not found" holds at the `valid-branch-directory-p` boundary but is loose
at the `valid-session-directory-p` boundary. A pre-rename session
directory still passes session-level validity and gets included in
iteration, but its branches all fail validity. Whether this matters
depends on how downstream consumers handle "valid session, zero valid
branches" — UI, registry, listing, etc. The spec should be precise
about which boundary holds the invisibility guarantee.

## Verification
- `sessions-persistence.md` spec language is precise about which helper
  enforces the invisibility.
- If `valid-session-directory-p` is tightened, its tests verify the new
  semantics.

## Context
- Review of sessions-filesystem (orchestrator session 2026-04-20)
  Finding #8
- filesystem.el:227-231 (`jf/gptel--valid-session-directory-p`) and
  filesystem.el:168-198 (`jf/gptel--find-all-branches-with-agents`)
- design.md Decision 19 (clean break, no migration)
