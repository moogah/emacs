---
name: disposition-match-pattern-handling
description: "User-decision task (finding-10B). Decide what add-to-scope does on :match-pattern violations — refuse to add globs, redirect to search-path, or keep current behaviour."
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:rewire-expansion-writer
  - discovered-from:implement-drawer-writer
---

## Cites register entries

- `register/vocabulary/operation-to-drawer-key` — current entry has `:match-pattern → GPTEL_SCOPE_READ` with an `open_question` annotation.
- `register/shape/violation-info` — `:operation :match-pattern` arrives on the violation plist; the `:resource` is a literal pattern (e.g. `*.txt`), not a path.

## Background

Architect audit finding `arch-cycle-1777460733-10` (sub-case B):
`find /home -name "*.txt"` emits two operations from the bash-parser:
`:read-directory :file "/home"` and `:match-pattern :file "*.txt"`. When
the second is denied and the user presses `a`, the writer routes the
*pattern* into `GPTEL_SCOPE_READ`. Subsequent reads of any file matching
`*.txt` *anywhere* on the filesystem now succeed without re-prompting.

That outcome is plausibly not what the user expects from "add to scope"
on a `find` command — they typically want the *search directory*
(`/home`) added, not the pattern.

## Three resolution paths

1. **Refuse to add globs to scope.** The expansion UI detects
   `:operation :match-pattern` and removes "add to scope" from the
   menu (or only offers it when the resource has no glob characters).
   Replace with a hint: "this is a pattern from `find -name`; expand
   the search directory instead". Users still need allow-once and
   deny actions.

2. **Smart redirect: offer to add the sibling `:read-directory`
   resource.** When a `:match-pattern` violation arrives alongside a
   `:read-directory` for the same `find` invocation, the menu shows
   "add `/home` (search directory)" instead of "add `*.txt` (pattern)".
   Requires plumbing the sibling violation into the expansion UI.

3. **Keep current READ collapse.** Document the tradeoff (pattern
   grants apply across the filesystem); accept that users who want
   narrower grants must use --add-bash-to-scope manually.

## Decision required

Routes to the user via the integrate→plan handshake.

## Implementation work (after decision)

- **Path 1**: filter the menu in `scope-expansion.org` when
  `:operation` is `:match-pattern`; remove the `:match-pattern` arm
  from `--map-operation-to-drawer-key` (with a comment that it's
  action-layer-only); update register entry's member.
- **Path 2**: extend the violation-info shape to carry a `:siblings`
  field listing related operations from the same command; the menu
  reads the siblings and surfaces them as alternative actions.
  Larger change; touches `register/shape/violation-info` and the
  bash-parser pipeline.
- **Path 3**: no code change; document tradeoff in register entry.

## Verification

After implementation, run `./bin/run-tests.sh -d config/gptel/scope`.
A new buttercup spec asserting the menu's behaviour on
`:match-pattern` violations.
