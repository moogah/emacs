---
name: revert-initial-session-body-and-delete-headings-block
description: Delete `jf/gptel--session-headings-block` and revert `jf/gptel--initial-session-body` to its pre-Addendum signature (no `system-prompt` parameter). Strip the heading composition from `jf/gptel--create-session-core` so a freshly created `session.org` is the file-level config drawer followed by a bare `#+begin_user` block — no `* System Prompt` heading, no `* Chat` heading. Update `jf/gptel-persistent-agent--initial-body` to stop delegating to the deleted helper.
change: replace-system-prompt-heading-with-sibling-file
status: needs-review
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

## Observations

- The task brief named two test files (`session-org-creation-spec.el`, `preset-application-spec.el`, `activity-session-chat-spec.el`). Implementation surfaced a fourth: `config/gptel/tools/test/persistent-agent/creation-spec.el` had four heading-shape `it` blocks under `describe "PersistentAgent session.org matches the canonical document layout"`. Updated that file too — collapsed the four heading-shape scenarios down to two no-heading scenarios (config drawer + populated user block; drawer-first ordering with exactly one user turn). Drift surfaced because the persistent-agent path produces session.org files that were validated against `register/shape/session-document-layout`.
- The agent path no longer threads `system-prompt` through `jf/gptel-persistent-agent--initial-body` at all (signature `(prompt)` instead of `(system-prompt prompt)`). The system-prompt body will be picked up by the sibling-file writer call wired in by `wire-sibling-file-emission-into-session-creation`. Until that task lands, fresh agent sessions have no system prompt in either the document or the sibling file — graceful-degrade to preset-only is the documented behavior.
- The drawer + body concatenation is verbatim (no `"\n"` separator between drawer-text and the body string). Tests asserting a blank line between `:END:` and `#+begin_user` were wrong — the original behavior was also no-blank-line (the old heading block started with `* System Prompt\n`, not `\n* System Prompt\n`). Restored the no-blank-line assertion.
- `(require 'gptel-session-commands)` stays in `persistent-agent.org` (changed only the trailing comment). The agent body composer no longer calls `jf/gptel--session-headings-block`, but the require is the conventional shape — and a later task will wire `jf/gptel--write-system-prompt-sibling-file` from the same module into the agent path.

## Discoveries

- discovery_id: disc-revert-initial-session-body-1
  class: shape-fragmentation
  description: |
    `register/shape/session-document-layout` enumerates four
    structural invariants (config drawer at point-min, singleton
    `* System Prompt`, singleton `* Chat`, turn blocks under `* Chat`).
    After this task, three of those four invariants no longer hold:
    there is no `* System Prompt` heading and no `* Chat` heading,
    so "turn blocks under `* Chat`" is vacuously true. Only "config
    drawer at point-min" survives. The shape needs to be re-stated
    in terms of the new layout.
  affected_register_entry: register/shape/session-document-layout
  recommendation: |
    Reconcile at integrate. The new shape contract is:
    "config drawer at point-min; followed directly by zero or more
    turn blocks (`#+begin_user`/`#+begin_assistant`); no headings."
    Mark the heading-related invariants as superseded by
    replace-system-prompt-heading-with-sibling-file. Add a pointer
    to the new `:GPTEL_SYSTEM_PROMPT_FILE:` drawer key and the
    sibling `system-prompt.<ext>` shape.

- discovery_id: disc-revert-initial-session-body-2
  class: dead-branch
  description: |
    `jf/gptel-persistent-agent--task` previously threaded
    `system-prompt` (derived from preset-spec :system) into the
    body composer. After this task the local let-binding is gone
    and the composer takes only `prompt`. The preset-spec
    extraction at the call site is unaffected (preset-spec is
    still needed for `jf/gptel-scope-profile--render-drawer-text`
    and for the future sibling-file writer call). The branch is
    not dead — it's been narrowed to its remaining consumers.
  affected_register_entry: register/shape/session-document-layout
  recommendation: |
    No standalone action. The narrowed plumbing will be re-widened
    in `wire-sibling-file-emission-into-session-creation` (which
    will pass preset-name + preset-spec to the sibling-file writer
    for both the interactive and agent paths).
