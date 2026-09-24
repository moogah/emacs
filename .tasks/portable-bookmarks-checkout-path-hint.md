---
name: portable-bookmarks-checkout-path-hint
description: "Disambiguate sibling worktrees of one repository inside one workspace home by recording a checkout-path hint in the stamp, readable by the resolver and the layout importer."
source: workspace-portable-bookmarks cycle-1789935943 (ask-cycle-1789935943-7b)
status: backlog
task_class: feature
discovered_from: checkout-discovery
discovered_by: [reviewer, user]
discovered_class: spec-signal
reconciled_into: null
relations:
  - discovered-from:checkout-discovery
  - related-to:discovery-refinements
---

## Problem

A workspace home can hold two worktrees of the same repository. Discovery then
has more than one candidate for a stamped identity. From cycle-1789935943,
`discovery-refinements` reports the ambiguity instead of silently picking the
alphabetically-first, and the resolver maps it onto an existing outcome.

## User direction (2026-09-24)

"Each worktree in a workspace directory needs to be at a distinct path, so we
should be able to solve this eventually without too much hacking. By default the
worktree path will most often be the name of the repo, but optional customizing
and having some signal in the stamp which can be read by the importer sounds
like a good first step." The user wants this considered SOONER rather than
later, but NOT added to cycle-1789935943's scope.

## Notes for whoever picks this up

- The stamp has a format version (`workspace-bookmark-format`, currently 1).
  An additional, ignorable hint property can be added without orphaning v1
  stamps: records without it fall back to ambiguity handling. Decide whether
  that is a v1-compatible optional field or a v2 bump.
- The natural hint is the checkout directory's path relative to its workspace
  home (usually just the repo name), captured at stamp time.
- It interacts with slice 2 (layout export/import), where the importer is the
  main reader.
