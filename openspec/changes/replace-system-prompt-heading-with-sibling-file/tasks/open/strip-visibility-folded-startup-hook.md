---
name: strip-visibility-folded-startup-hook
description: Remove the chat-mode hook that activates `org-set-visibility-according-to-property` for `* System Prompt` startup-folding. With no `* System Prompt` heading in the canonical layout (no `:VISIBILITY:` property to honor), the hook has nothing to fold and can be removed cleanly.
change: replace-system-prompt-heading-with-sibling-file
status: ready
relations: []
---

## Files to modify

- `config/gptel/chat/mode.org` (modify, if the hook lives there) — strip the `org-set-visibility-according-to-property` activation; investigate first to confirm location

## Why

The prior change introduced startup-folding wiring so that the `* System Prompt` subtree opened collapsed via its `:VISIBILITY: folded` property (Addendum Finding C). With the heading gone (tasks `revert-initial-session-body-and-delete-headings-block` for the creation side and `delete-heading-reader-from-chat-menu` / `delete-heading-writer-from-chat-menu` for the chat-mode side), the wiring has nothing to act on.

Leaving the hook in place is harmless functionally but is dead code that obscures intent and slightly slows mode activation.

The file-level `:PROPERTIES:` drawer auto-fold (a separate task in the prior change, `fold-config-drawer-on-open`) is **kept**. It is unrelated to `* System Prompt` and still solves the original visual-clutter problem for the configuration drawer.

## Implementation steps

1. Locate the startup-folding wiring. Search:
   ```
   grep -rn 'org-set-visibility-according-to-property\|VISIBILITY' \
     config/gptel/chat/ config/gptel/sessions/
   ```
2. If the activation is in `config/gptel/chat/mode.org` (likely location), strip the hook entry. If it lives elsewhere (e.g., inline in `chat-mode-hook` setup in `menu.org`), update this task's files-to-modify list and document the location.
3. **Do NOT strip the file-level drawer auto-fold** (the `org-fold-hide-drawer-toggle` / `org-cycle-hide-drawers` call on the `:PROPERTIES:` drawer at point-min). That is a separate concern owned by the prior `fold-config-drawer-on-open` task and remains valid.
4. Re-tangle the touched `.org` file.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/mode.org   # or wherever the hook lives
./bin/run-tests.sh -d config/gptel/chat
grep -rn 'org-set-visibility-according-to-property' config/gptel/chat/
```

Expect: no matches for `org-set-visibility-according-to-property` in chat-mode tangled output. Chat-mode test suite passes; in particular `drawer-fold-spec.el` (or its equivalent for the file-level drawer fold) still passes — the file-level drawer fold is a separate hook.

## Context

architecture.md §Components — `chat/mode.org` may carry the activation; investigation step in the implementation confirms.

design.md §Goals — the heading layout is removed wholesale; this task cleans up one of the activation hooks introduced specifically to support it.

## Cycle 1779565028 updates (cycle-1779565028)

- **Heading shape is fully gone after this cycle**: cycle-1779565028
  closed the five foundational tasks (heading reader/writer deleted
  from `chat/menu.org`, headings-block helper deleted from
  `sessions/commands.org`, agent path updated). The
  `org-set-visibility-according-to-property` activation now has *no*
  `:VISIBILITY: folded` heading to fold in any production-emitted
  session.org — the cleanup is safe and the dead-code claim in this
  task's "Why" section is now empirically true.
- **No impact-set hits in code yet**: the cycle did not touch the
  hook's likely location (chat/mode.org or wherever it's wired). The
  investigation grep in step 1 is still the right place to start.
- **Test surface to verify**: the chat-mode suite (483-497 specs in
  cycle-1779565028) does not currently have any spec that depends on
  the visibility hook firing. Verification command in step 4 is
  sufficient; no spec deletions expected.

## Cycle 1779568860 updates (cycle-1779568860)

- **No new blockers.** Cycle-2 did not touch this task's likely
  files (`chat/mode.org` or wherever the visibility hook lives).
  The cycle-1 status here is unchanged — investigation grep in step
  1 is still the right entry point.
- **Chat-mode suite has grown to 504 specs** (was 497 at
  cycle-1779565028 close): 503 from cycle-2 T2's
  `pre-send-refresh-spec.el` (6 new + the original 497) plus 1
  post-review regression guard. None of these new specs depend on
  the visibility hook either; verification expectations from the
  prior cycle update still hold.
- **Register supersession landed** for the heading-shape contracts
  (`register/shape/session-document-layout`,
  `register/invariant/system-prompt-heading-authoritative` →
  `status: superseded`). The hook this task removes was a remnant
  of those contracts; the "Why" section's dead-code claim is now
  doubly reinforced (heading-shape removed in cycle-1; register
  supersession completed in cycle-2). No code-side impact on this
  task's implementation steps.

