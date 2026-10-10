---
name: ws-persistence-v4
description: "Persistence schema v4: write v4, read v3 and v4; round-trip recipes and metadata"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:ws-layout-metadata"
  - "blocked-by:ws-recipe-layouts"
  - "enables:ws-programmatic-add-layout"
---

## Files to modify
- config/workspaces/persistence.org (modify)
- config/workspaces/test/persistence-v4-spec.el (new)

## Implementation steps
1. Specs first (mirror `persistence-v3-spec.el`):
   - a v3 file loads without loss; the next flush writes `:version 4`;
   - a v4 file containing a recipe `:saved-state` and `:etc` metadata round-trips;
   - versions other than 3 and 4 produce the existing `*Messages*` notice and load
     nothing;
   - the read-back verification (`read` round-trip) accepts recipe forms.
2. Bump the writer's version constant to 4; widen the reader's accepted set to
   (3 4); make sure the readable-by-construction scrubber passes recipes through
   unchanged (they contain only strings, symbols and integers).
3. Update `persistence-v3-spec.el` expectations that assert the written version.
4. Tangle; run tests.

## Design rationale
v3 is a strict subset of v4 (no recipes, no metadata), so accepting both avoids
forcing the user to delete real workspace state, while bumping the written version
makes older code reject a v4 file with its existing notice instead of trying to
`window-state-put` a recipe.

## Verification
- ./bin/tangle-org.sh config/workspaces/persistence.org
- ./bin/run-tests.sh -d config/workspaces

## Context
- design.md § Decisions 'D16. ...' (W6 Persistence)
- specs/workspaces/spec.md § 'Per-machine persistence and restoration' (MODIFIED)
