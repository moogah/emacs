---
name: state-readmes-git-controlled-claims
description: Remove the false "git-controlled" claims from the bookmark comment in defaults.org and from state/projectile and state/activities READMEs
status: ready
task_class: doc
source: openspec/changes/workspace-portable-bookmarks (cycle-1790453714, docs-readme review finding 5)
discovered_from: docs-readme
discovered_by: reviewer
discovered_class: interface-drift
reconciled_into: null
relations:
  - discovered-from:docs-readme
---

`.gitignore` ignores `state/**` except README files, so nothing under `state/` is git-controlled. docs-readme corrected
`state/bookmarks/README.md`; the same claim survives in the comment above `bookmark-default-file` (`config/core/defaults.org:146`, tangle
after editing), `state/projectile/README.md` and `state/activities/README.md`. Verify each against `.gitignore` and the setting it
documents, and correct.
