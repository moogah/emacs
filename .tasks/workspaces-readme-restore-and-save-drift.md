---
name: workspaces-readme-restore-and-save-drift
description: config/workspaces/docs/README.org's Restore and Save-triggers sections describe startup tab recreation and debounced saves that the code no longer does
status: ready
task_class: doc
source: openspec/changes/workspace-portable-bookmarks (cycle-1790453714, docs-readme review finding 4)
discovered_from: docs-readme
discovered_by: reviewer
discovered_class: interface-drift
reconciled_into: null
relations:
  - discovered-from:docs-readme
---

Pre-existing drift found while reviewing the new *Portable bookmarks* section, which (correctly) says there is "no tab yet" after a
restart. The older *Persistence > Restore* and *Save triggers* sections still say tabs are recreated at startup and that saves are
debounced. Verify both against `persistence.org` / `workspaces-mode.org` and rewrite those sections to match the code.
