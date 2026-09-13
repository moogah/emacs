---
name: og-menu-extra-groups
description: Let the graph menu accept contributed groups via org-graph-menu-extra-groups; add the session finder and a Maintain entry to update the connected block.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:og-connected-dblock
  - enables:int-workspace-commands
---

## Files to modify
- config/org-graph/menu.org (modify; tangle)
- config/org-graph/finders.org (modify; tangle — `org-graph/find-session`)
- config/org-graph/test/menu-spec.el, finders-spec.el (modify)

## Implementation steps
1. `finders.org`: add `org-graph/find-session` using the existing
   `org-graph-finders--type-filter 'session`. The `session` schema itself is
   registered by the integration layer (its predicate is gptel's
   `gptel_session` filetag); with no schema registered the filter matches
   nothing — acceptable (design OQ-C).
2. `menu.org`: define `(defvar org-graph-menu-extra-groups nil ...)` — an
   alist of `(GROUP-LABEL . ((KEY DESCRIPTION COMMAND) ...))`. Build the
   transient prefix so that contributed groups are appended after Maintain
   when the alist is non-nil. transient prefixes are static by default:
   either rebuild the prefix with `transient-define-prefix` inside a setup
   function invoked when the variable changes (provide
   `org-graph-menu-rebuild`), or use `:setup-children` on a placeholder
   group to compute suffixes at invoke time. Prefer `:setup-children` so a
   late contribution needs no rebuild call.
3. Add to Find: `session` finder; to Maintain: "update connected block at
   point" → `org-dblock-update` (or a thin wrapper that finds the nearest
   `org-graph-connected` block and updates it).
4. Specs: with the alist nil the prefix has exactly Find/Author/Edges/
   Maintain; with one contributed group it appears with its entries bound
   to the given commands; Find lists the session finder; the Maintain entry
   dispatches to the dblock update.

## Design rationale
`find-in-workspace` and `new-in-workspace` belong to the integration
layer, but discoverability wants them in the same `SPC v` menu. org-graph
must not name the integration layer, so it publishes a contribution
variable and renders whatever is registered — the same publish/subscribe
posture the workspaces integration registry uses.

## Design pattern
`transient` `:setup-children` for dynamic groups (see transient manual
§ Dynamic groups); existing menu structure in menu.org for the static
groups; finder wrappers in finders.org.

## Verification
- `./bin/tangle-org.sh config/org-graph/menu.org` and `finders.org` validate.
- `./bin/run-tests.sh -d config/org-graph` — green.
- `grep -n 'org-graph-menu-extra-groups' config/org-graph/menu.el` → defined and consumed.

## Context
design.md § D8 (menu contribution), § Open Questions OQ-C
specs/org-graph-menu/spec.md § 'Graph Menu Prefix'
