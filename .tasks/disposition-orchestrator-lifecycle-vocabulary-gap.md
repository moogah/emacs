---
name: disposition-orchestrator-lifecycle-vocabulary-gap
description: |
  Decide how the orchestrator's interfaces-register lifecycle should
  represent supersession. Cycle-2 of replace-system-prompt-heading-with-sibling-file
  shipped `status: superseded` and four supporting fields outside the
  documented vocabulary at `interfaces.org:27-28`. Three coherent
  options: extend the enum / re-encode as reconciled+notes / introduce
  a transient marker. The decision shapes the orchestrator's register
  schema going forward; not specific to any single change.
status: ready
source: openspec/changes/replace-system-prompt-heading-with-sibling-file
relations:
  - discovered-from:mark-superseded-interfaces-register-entries
  - discovered-from:ask-cycle-1779568860-1
  - discovered-from:arch-cycle-1779568860-1
  - relates-to:cycle-1779565028/meta-discoveries/invariant-supersession-pattern
---

> Surfaced during cycle-1779568860 integrate as `ask-cycle-1779568860-1`
> (architect finding `arch-cycle-1779568860-1`, severity spec-signal).
> Originally predicted by cycle-1779565028's `invariant-supersession-pattern`
> meta-discovery. Externalised because the decision affects the
> orchestrator's register schema across all future changes — not
> specific to the change that surfaced it. The change's two
> superseded entries (`register/shape/session-document-layout`,
> `register/invariant/system-prompt-heading-authoritative`) are
> shipped as-is with `status: superseded`; this task is to formalise
> the schema decision before the next supersession cycle.

## The gap in one paragraph

`interfaces.org:27-28` documents the register-entry lifecycle as:

```
status: speculated | confirmed | divergent | reconciled
```

The orchestrator's register-entry templates at
`~/.claude/skills/opsx-orchestrate/templates/register-entry-{shape,
vocabulary,boundary,invariant}.md` do not document any
supersession block. Cycle-2's T5 introduced four fields outside the
vocabulary:

- `status: superseded` (on 2 entries: shape/session-document-layout
  and invariant/system-prompt-heading-authoritative)
- `superseded_by: <entry-id>` (2 entries)
- `superseded_in_cycle: cycle-1779568860` (2 entries)
- `supersession_note: |` (2 entries)
- `supersedes: [<entry-id>]` (2 new replacement entries)

Cycle-1779565028's reconciliation note for
`invariant-system-prompt-heading-authoritative.md` already predicted
this gap as a meta-discovery (`invariant-supersession-pattern`).
Cycle-2 confirmed the prediction empirically with two simultaneous
supersession transitions in a single task.

## Three options (lifted from the reviewer of T5)

### Option A: Extend the enum

Add `superseded` as a fifth first-class transition. Update
`interfaces.org:27-28` and the template files. Define semantics:
"entry's prior contract was deliberately removed by a superseding
entry; entry retained for historical record; new contract at
`superseded_by:`". Require `superseded_by` + `superseded_in_cycle` +
`supersession_note` as a field block whenever `status: superseded`.

- Pro: First-class queryability for "find superseded entries"; clear
  semantic distinction from `reconciled` (entry text updated; contract
  survives) and `divergent` (impl contradicted spec).
- Con: Schema change to a widely-cited file. Existing register-entry
  templates need a new block.

### Option B: Re-encode as `reconciled` + supersession note

Keep the four-status enum unchanged. Revert cycle-2's two
`status: superseded` entries to `status: reconciled` with the
supersession metadata folded into the `status_note:` field as
free-form prose (a `Supersession block:` subsection within the
status_note).

- Pro: No schema change; current four-status enum stands as-is. Lowest
  cost to land.
- Con: Loses queryability. "Find superseded entries" becomes "grep for
  Supersession-block in reconciled entries". Supersession is no longer
  a first-class concept in the register's lifecycle. Future
  supersessions need maintainer discipline to keep the prose format
  consistent — likely to drift.

### Option C: Transient `superseded` marker between `reconciled` and deletion

Extend the enum (option A) but treat `superseded` as a short-lived
state that exists only until the entry is fully deleted (or migrated
to an archive section like `* Archived` at the bottom of
`interfaces.org`). Combines option A's clarity with built-in cleanup
pressure.

- Pro: Avoids accumulating superseded entries indefinitely. Each
  cycle's supersession creates a small follow-up task: "archive
  entries that have been superseded for N cycles".
- Con: Two-step lifecycle (supersede then archive) is more complex
  than option A's one-step. Risk of forgetting to archive — needs
  PM-cycle policing.

## Implementation steps (after disposition)

The disposition is the user's. After they pick A / B / C:

1. If A: edit `interfaces.org:27-28` to add `superseded` to the
   vocabulary; edit the four orchestrator register-entry templates to
   document the supersession block; touch the cycle-2 entries (no
   change needed — they already conform).
2. If B: edit cycle-2's two `status: superseded` entries to flip back
   to `status: reconciled` and fold the supersession metadata into
   `status_note:` as a documented sub-block. Edit
   `interfaces.org:27-28` to add a documentation note about the
   `Supersession block:` prose convention. Update orchestrator
   templates to document the convention.
3. If C: same as A, plus add an `* Archived register entries` section
   at the bottom of `interfaces.org`, plus a follow-up policy task
   for cycle-3+ defining the supersession→archive transition.

## Verification (after implementation)

```bash
grep -n "status: superseded\|superseded_by:\|status: reconciled" \
  /Users/jefffarr/emacs/interfaces.org | head -30
grep -n "speculated\|confirmed\|divergent\|reconciled\|superseded" \
  /Users/jefffarr/emacs/interfaces.org | head -10
```

Expect: the chosen scheme is consistently applied to the two
cycle-2 entries; `interfaces.org:27-28` documents the chosen
lifecycle; orchestrator templates match.

## Context

- This-cycle architect finding:
  `.orchestrator/cycles/cycle-1779568860/findings/arch-cycle-1779568860-1.md`
- Prior-cycle meta-discovery: cycle-1779565028's
  `invariant-system-prompt-heading-authoritative.md` reconciliation
  note's `## Meta-discovery` section.
- Orchestrator register-entry templates:
  `~/.claude/skills/opsx-orchestrate/templates/register-entry-{shape,
  vocabulary,boundary,invariant}.md`
- Current lifecycle vocabulary location: `interfaces.org:27-28`.
