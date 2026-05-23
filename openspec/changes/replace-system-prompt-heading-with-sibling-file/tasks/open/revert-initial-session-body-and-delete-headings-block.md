---
name: revert-initial-session-body-and-delete-headings-block
description: Delete `jf/gptel--session-headings-block` and revert `jf/gptel--initial-session-body` to its pre-Addendum signature (no `system-prompt` parameter). Strip the heading composition from `jf/gptel--create-session-core` so a freshly created `session.org` is the file-level config drawer followed by a bare `#+begin_user` block — no `* System Prompt` heading, no `* Chat` heading. Update `jf/gptel-persistent-agent--initial-body` to stop delegating to the deleted helper.
change: replace-system-prompt-heading-with-sibling-file
status: ready
relations: []
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — delete `jf/gptel--session-headings-block`; revert `jf/gptel--initial-session-body` signature; update `jf/gptel--create-session-core` to compose drawer + bare user block (no headings)
- `config/gptel/tools/persistent-agent.org` (modify) — `jf/gptel-persistent-agent--initial-body` no longer calls `jf/gptel--session-headings-block`; emits drawer + populated user block directly
- `config/gptel/sessions/test/commands/session-org-creation-spec.el` (modify) — assert the new (no-heading) layout for the interactive creation path
- `config/gptel/sessions/test/commands/preset-application-spec.el` (modify) — drop heading assertions; preserve drawer-shape assertions
- `config/gptel/sessions/test/commands/activity-session-chat-spec.el` (modify) — drop the "no markdown heading" workaround introduced for the heading layout; restore the simpler markup check

## Why

design.md §Goals — the canonical layout reverts to "drawer + bare turn block." With `* System Prompt` and `* Chat` headings gone, the heading-composition helper `jf/gptel--session-headings-block` is dead, and `jf/gptel--initial-session-body` no longer needs the `system-prompt` argument introduced in the prior change.

This task delivers the no-heading shape for the creation path. The sibling-file emission (which writes `system-prompt.<ext>` and threads `:GPTEL_SYSTEM_PROMPT_FILE:` into the drawer) lands in `wire-sibling-file-emission-into-session-creation` after the helper from `add-sibling-file-writer-helper` exists. This task establishes the clean baseline.

`persistent-agent.org` is touched here because the agent creation path is a second producer of `register/shape/session-document-layout` (per the prior change's reconciliation). With the heading helper deleted, the agent body emitter must compose its own bare drawer + populated user block.

## Implementation steps

1. **`config/gptel/sessions/commands.org`**:
   - Delete `jf/gptel--session-headings-block` and the commentary block that introduces it (search for "session-headings-block").
   - Revert `jf/gptel--initial-session-body` to its pre-Addendum signature `()` (no parameter); the body returns the empty user-block string `"#+begin_user\n\n#+end_user\n"`.
   - In `jf/gptel--create-session-core`, stop threading the preset's `:system` into `jf/gptel--initial-session-body`. The composition becomes: `(concat <drawer-text> "\n" (jf/gptel--initial-session-body))` (preserve a single blank line between drawer and user block).
2. **`config/gptel/tools/persistent-agent.org`**:
   - In `jf/gptel-persistent-agent--initial-body` (or whatever the agent-side composer is named), drop the call to `jf/gptel--session-headings-block`. The agent body becomes drawer + populated user block (e.g., `(concat <drawer> "\n#+begin_user\n" prompt "\n#+end_user\n")`).
3. **Tests**:
   - `session-org-creation-spec.el`: replace assertions that expected a `* System Prompt` heading (with `:VISIBILITY: folded` properties drawer and seeded body) and a `* Chat` heading with assertions that no such headings appear. Assert the drawer is followed directly by `#+begin_user\n\n#+end_user\n`.
   - `preset-application-spec.el`: drop heading assertions. The drawer-shape (full snapshot, no `:GPTEL_SYSTEM:`) is unchanged.
   - `activity-session-chat-spec.el`: the "no markdown `# ` heading" assertion was previously scoped to renderer-owned regions to avoid mis-flagging the verbatim preset `:system` body under the `* System Prompt` heading. With no heading and no preset body in the buffer, restore the simpler whole-file markup check.
4. Re-tangle `commands.org` and `persistent-agent.org`. Run the sessions test suite and the chat-mode test suite.

## Verification

```bash
./bin/tangle-org.sh config/gptel/sessions/commands.org
./bin/tangle-org.sh config/gptel/tools/persistent-agent.org
./bin/run-tests.sh -d config/gptel/sessions
./bin/run-tests.sh -d config/gptel/chat
grep -n 'session-headings-block\|System Prompt\|\\* Chat' \
  config/gptel/sessions/commands.el \
  config/gptel/tools/persistent-agent.el
```

Expect: no matches in `commands.el` or `persistent-agent.el` for `session-headings-block`, `* System Prompt`, or `* Chat`. Test suites pass.

## Context

architecture.md §Components — `sessions/commands.org` and `tools/persistent-agent.org` are the two producers of the session document layout; both need the heading composition removed.

design.md §Decision 7 — wholesale deletion of the heading shape, no intermediate state.

The pre-Addendum signature of `jf/gptel--initial-session-body` was a zero-argument function returning the bare user-block string. The git history before the prior change's `emit-system-prompt-and-chat-headings-at-creation` task is the reference for the simplest restored shape.
