---
name: menu-rewrite-tweak-response-scope
description: Drop gptel-chat-menu's Rewrite and Tweak-Response groups — dead code in chat-mode buffers because their :if predicates depend on gptel-mode's response-overlay and text-property infrastructure that chat-mode does not produce
change: gptel-chat-mode
status: done
relations:
  - discovered-from:menu-send-coupled-options-scope
  - blocked-by:menu-send-coupled-options-scope
---

## Files to modify
- `openspec/changes/gptel-chat-mode/design.md` (extend Decision 15's
  exclusion enumeration to cover Rewrite / Tweak-Response, with the
  same Decision 18 cross-reference)
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  (extend the "chat-mode menu omits Send-coupled groups" scenario —
  or add a sibling scenario — to include Rewrite / Tweak-Response)
- `config/gptel/chat/menu.org` (remove the two groups from
  `gptel-chat-menu`)
- `config/gptel/chat/menu.el` (tangled)
- `config/gptel/chat/test/menu/menu-send-rebind-spec.el` (add a
  negative assertion that Rewrite / Tweak-Response commands are
  absent from the flattened layout)

## Implementation steps

1. **Identify the two groups in `menu.org`** (current
   `config/gptel/chat/menu.el:499-518`):

   - **Rewrite group** — guarded by `(or (use-region-p) (and
     gptel--rewrite-overlays (gptel--rewrite-sanitize-overlays)))`.
     Contains the `r` binding to `gptel-rewrite`. The
     `gptel--rewrite-overlays` variable is populated by
     gptel-mode's response-insertion path; chat-mode never populates
     it. The region-active branch *could* fire in chat-mode, but
     `gptel-rewrite` itself reads from gptel-mode's response machinery
     downstream, so the command is broken even if its `:if` predicate
     matches.
   - **Tweak Response group** — guarded by `gptel--in-response-p`,
     which checks for the `gptel` text-property at point. Chat-mode
     stores response text inside `#+begin_assistant` blocks without
     the `gptel` text-property, so this predicate never matches.
     All five suffixes (`SPC` mark, `M-RET` regenerate, `P`/`N`
     variant, `E` ediff) are response-history operations on a
     response-under-point that chat-mode does not produce.

2. **Remove both groups** from the `gptel-chat-menu` prefix
   definition. The existing Logging group and `[(gptel-chat--suffix-
   send)]` row stay.

3. **Update `design.md §Decision 15`.** After the Send-coupled
   exclusion paragraph (added by `menu-send-coupled-options-scope`),
   append a paragraph:

   > The mirror also excludes Rewrite and Tweak-Response. Their
   > `:if` predicates (`gptel--rewrite-overlays`,
   > `gptel--in-response-p`) test for gptel-mode's response-insertion
   > artifacts — rewrite overlays and the `gptel` text-property on
   > response spans. Chat-mode stores response text inside
   > `#+begin_assistant` blocks without those artifacts (Decision
   > 18), so the predicates never match and the groups would only
   > be dead rows in the layout. `gptel-rewrite` itself depends on
   > the same response-overlay infrastructure downstream, so even
   > the region-active branch of the Rewrite guard cannot route to
   > a working command in a chat-mode buffer.

4. **Update `specs/gptel-chat-mode/spec.md`.** Extend the "chat-mode
   menu omits Send-coupled groups" scenario (or add a sibling named
   "chat-mode menu omits response-state groups") to list the
   exclusions:

   > **THEN** the prefix layout does not contain Rewrite or
   > Tweak-Response groups (their underlying predicates depend on
   > gptel-mode response-insertion artifacts chat-mode does not
   > produce)

5. **Add a test** in `menu-send-rebind-spec.el`:

   - Flatten the `gptel-chat-menu` layout and assert the symbols
     `gptel-rewrite`, `gptel--mark-response`, `gptel--regenerate`,
     `gptel--previous-variant`, and `gptel--ediff` are absent. One
     spec with five `expect` calls is fine — they test the same
     contract.

## Design rationale

**Different root cause from Send-coupled groups, same diff
location.** The `menu-send-coupled-options-scope` task drops the
three groups that actively route through upstream's `gptel--suffix-
send` (Prompt-from, Response-to, Dry-Run). Those are user-visible
bugs: the user clicks a toggle, nothing happens, or clicks Inspect
Query and gets a wrong preview. Rewrite and Tweak-Response are a
different failure mode — their `:if` guards are always false in
chat-mode, so the groups are silently hidden and the user never
sees them. Hygiene rather than bugfix.

The fix is the same shape (remove the vector blocks from the prefix
definition) and lives in the same spec section, but the rationale
and urgency differ enough to warrant a separate task. Grouping with
`menu-send-coupled-options-scope` would muddle the "Send-coupled"
framing in the task name and in Decision 15's wording — the reader
learning about the chat-mode mirror benefits from a clean split
between "dispatches through upstream Send" and "reads gptel-mode
response-insertion artifacts."

**Why drop rather than port.** A chat-mode-native Rewrite is
conceivable (rewrite a user-block region in place) but would
require a parallel command path equivalent in size to
`gptel-chat-send` itself, and there is no evidence a chat-mode user
wants this workflow. Tweak-Response (mark / regenerate / variant
navigation) depends on response-history tracking that chat-mode
has not committed to and may never want — responses live in
`#+begin_assistant` blocks that the user can freely edit. Dropping
the groups is the minimum-commitment position consistent with
Decision 18.

## Design pattern

A mirror layout with `:if`-guarded groups can silently hide broken
groups when the guard depends on state the host mode doesn't
produce. Hidden groups are not load-bearing — they cost nothing to
the user today — but they are a trap for the next person reading
`gptel-chat-menu` expecting it to reflect the actual feature set,
and they represent an implicit promise to keep the upstream infix
references alive. Prefer explicit exclusion (remove the group) over
implicit exclusion (rely on the guard never firing).

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` passes,
  including the new negative assertion for Rewrite / Tweak-Response
  symbols.
- Manual check: in a chat-mode buffer that contains both a
  `#+begin_user` and `#+begin_assistant` block, move point into the
  assistant block and invoke `M-x gptel-chat-menu`. Confirm neither
  Rewrite nor Tweak-Response groups appear. (Previously they were
  hidden by `:if` guards; post-edit they are removed entirely.)
- `grep -n 'Rewrite\|Tweak Response\|gptel--in-response-p\|gptel-rewrite' config/gptel/chat/menu.el`
  returns only the `declare-function` lines (if any remain) and the
  `gptel--in-response-p` `:if` references are gone.

## Context

- Follow-up to `menu-send-coupled-options-scope` — surfaced during
  the 2026-04-23 audit of the chat-mode menu mirror's scope.
- `config/gptel/chat/menu.el:499-518` — Rewrite and Tweak-Response
  groups in `gptel-chat-menu`.
- `runtime/straight/repos/gptel/gptel-transient.el` — upstream
  `gptel-menu`, authoritative reference for the two groups'
  original guards and suffixes.
- `design.md §Decision 15` — to be extended after
  `menu-send-coupled-options-scope` applies its update.
- `design.md §Decision 18` — block-based `session.org` format;
  root cause of the response-state-coupled predicates never firing
  in chat-mode.

## Review

Reviewed 2026-04-23 (orch-review session, batched with
`menu-send-coupled-options-scope`). Reviewer-agent delegation;
consolidated findings live on the sibling task's Review section.
No findings specific to this task.

### Verification re-run

- `./bin/tangle-org.sh config/gptel/chat/menu.org` — passes.
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` — 53/53 pass.
- `grep -n 'Rewrite\|Tweak Response\|gptel--in-response-p\|gptel-rewrite' config/gptel/chat/menu.el` —
  no matches (the dead forward-declaration was also removed).
- Broader `./bin/run-tests.sh -d config/gptel/chat` — 320/320 pass.

### Findings looked for and ruled out

- Five-symbol absence list (`gptel-rewrite`, `gptel--mark-response`,
  `gptel--regenerate`, `gptel--previous-variant`, `gptel--ediff`)
  matches exactly the command references in the two removed vector
  blocks. Note: upstream reuses `gptel--previous-variant` for both
  `P` and `N` directions, so a symbol-level absence check covers
  both keys.
- Unused `gptel--rewrite-overlays` forward declaration removed —
  verified.
- Decision 15 paragraph layering — composes cleanly with Task A's
  Send-coupled paragraph; no conflict in the diff.
- Spec scenario `AND` clause — extends Task A's "chat-mode menu omits
  Send-coupled groups" scenario as planned, not a duplicate scenario.
- Layout asymmetry after removal (outer `[...]` wrapper around only
  Logging) — transient accepts it and upstream uses the same shape.

### Blocked-by repointing

None. No open tasks depend on this one.
