---
name: workspaces-registration-hardening-leftovers
description: Remaining small registration/remote-home hardening items in core workspaces after the portable-bookmarks change
status: ready
task_class: bug
source: openspec/changes/workspace-portable-bookmarks (cycle-1790453714, on-touch of registration-and-purge-guards)
discovered_from: registration-and-purge-guards
discovered_by: architect
discovered_class: invariant-gap
reconciled_into: null
relations:
  - discovered-from:arch-on-touch-1790453714-70
  - discovered-from:arch-on-touch-1790453714-71
  - discovered-from:arch-on-touch-1790453714-72
---

Evidence: `.orchestrator/cycles/cycle-1790453714/findings/on-touch-guards.md` (70-72). Remote workspace homes are unsupported (user
decision 2026-09-27) and loaded broken; the user never uses them, so these are low-impact.

1. **(70)** The tab-bar title reads `home.org` of a broken REMOTE workspace whenever a tab carries its name — a TRAMP contact on every
   redisplay. Skip the title lookup for a remote `:home`.
2. **(71)** `workspace-re-anchor`'s rename branch has its own inline name-collision check instead of `workspace--refuse-taken-name`
   (duplication; behaviour identical). Route it through the helper.
3. **(72)** Duplicate names or homes in a hand-edited state file load silently (the later entry wins); a case-variant name duplicate is
   still possible on case-insensitive filesystems; the purge refusal says "outside" for the default parent itself and for remote homes.
   Warn on duplicates at load; word the refusal per case.
