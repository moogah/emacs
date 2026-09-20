---
name: repair-unclosed-yaml-blocks-in-interfaces-register
description: Two register entries have unclosed `#+begin_src yaml` blocks, so their YAML is swallowed into the following entry and machine reads of interfaces.org are silently wrong
status: open
source: openspec/changes/workspace-portable-bookmarks (cycle-1789935943 plan, forward-mode)
relations:
  discovered-from: forward-mode-register-population
provenance:
  discovered_from: cycle-1789935943-plan-forward-mode
  discovered_by: architect
  discovered_class: interface-drift
---

## Problem
`interfaces.org` has 98 `#+begin_src yaml` openers but only 96 `#+end_src`
closers. Two entries never close their block:

- line ~4678 — `** chat-mode-session-display`
  (`register/boundary/chat-mode-session-display`)
- line ~8134 — `** system-prompt-heading-authoritative`
  (`register/invariant/system-prompt-heading-authoritative`)

Line numbers are as of the cycle-1789935943 plan commit; locate by heading,
not by number.

The imbalance is **pre-existing** — `git show HEAD:interfaces.org` at
215ddf8d already has 89 openers / 87 closers. The nine entries added by
cycle-1789935943 preserved the delta exactly, so this change neither caused
nor worsened it.

## Why it matters
The register is the artifact the Architect role audits against, and it is
increasingly read by tooling rather than by eye. An unclosed block means:

1. Everything from the unclosed opener to the *next* entry's opener parses as
   one YAML document. The affected entry's fields and its successor's heading
   text merge; a `yaml.safe_load` of the region either fails or — worse —
   succeeds with wrong contents.
2. Org itself renders the entry and its successor as one src block, so the
   damage is visible on export but easy to miss in a folded outline
   (`#+startup: overview`).
3. Both affected entries are `load_bearing: true`. A structural audit that
   silently reads the wrong body for a load-bearing entry is exactly the
   `interface-drift` class the register exists to close — here turned against
   the register itself.

Note both affected entries are already `superseded`
(`system-prompt-heading-authoritative`) or non-load-bearing-adjacent, so the
live risk today is low; the cost is that any future machine read of the file
is unreliable until fixed.

## Fix
1. Insert the missing `   #+end_src` at the end of each entry's YAML body
   (3-space indentation, matching every other entry in the file).
2. Re-run the balance check and confirm 98/98.
3. Add the check to whatever validates the register, so it cannot regress
   silently. There is no such validator today — adding one is the durable
   half of this task and the reason it is not a one-line fix.

## Verification
```bash
# must print equal counts
grep -c '#+begin_src yaml' interfaces.org
grep -c '#+end_src' interfaces.org
```
Plus a scan that every `#+begin_src yaml` is closed before the next `** `
heading (counting totals alone would pass on two compensating errors).

## Context
- Discovered during cycle-1789935943 plan forward-mode, while validating that
  the nine newly inserted entries parsed. The check that found it is worth
  keeping; see step 3.
- Both affected entries belong to the gptel subsystem
  (`config/gptel/chat/`, system-prompt handling), which
  `workspace-portable-bookmarks` does not touch — hence externalised rather
  than filed in-change.
