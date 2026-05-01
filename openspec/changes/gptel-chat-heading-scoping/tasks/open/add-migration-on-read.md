---
name: add-migration-on-read
description: Apply heading escape to existing chat blocks on mode activation
change: gptel-chat-heading-scoping
status: ready
relations:
  - blocked-by:add-content-indentation-defcustom
  - blocked-by:add-point-in-block-body-predicate
---

## Files to modify

- `config/gptel/chat/mode.org` (and tangled `mode.el`)
- New test file: `config/gptel/chat/test/mode/migration-spec.el`
- Test fixture: a small `.org` file under `config/gptel/chat/test/mode/fixtures/` containing unescaped `* Heading` inside a `#+begin_assistant` body, mirroring the original repro session.

## Implementation steps

1. In `mode.org`, define a function `gptel-chat--migrate-headings` (or similar). Behavior:
   - Walk each outer chat block in the buffer using `gptel-chat-parse-buffer` (or a dedicated lighter pass).
   - For each chat block body, scan for lines matching `^\*+ ` and insert the configured indent prefix at the line's start.
   - Track a boolean: did any rewrite happen? If yes, leave the buffer marked modified. If no, restore the buffer's previous modified state (so a clean read stays clean).
2. Bind `inhibit-modification-hooks` while migrating to prevent the after-change hook (Decision 3) from double-processing.
3. Call `gptel-chat--migrate-headings` once on `gptel-chat-mode` activation, *after* buffer-local setup completes. Use `:after` advice or place the call in the mode body, ordered to run after `org-adapt-indentation` is set.
4. Migration MUST NOT modify content outside chat-block bodies. Free-form headings between blocks remain untouched.
5. Write Buttercup specs in `migration-spec.el`:
   - Open a buffer with the fixture file → migration applies, buffer is marked modified, content is normalized.
   - Open a buffer whose chat blocks have no column-0 `*` lines → no migration, buffer stays clean.
   - Open a buffer with `* Heading` outside any chat block AND no column-0 `*` lines inside chat blocks → no migration, the outer heading is preserved as a real heading.
   - Open a buffer with `* Heading` outside chat blocks AND `* In-block` inside an assistant block → only the in-block line is escaped; the outer heading is untouched.
   - Open a buffer with already-escaped headings inside a chat block → no further migration (idempotent).

## Design rationale

Read-time migration ensures `org-element-parse-buffer` sees a correct AST from the moment the buffer opens, with no destructive on-disk rewrite. Reversible via `M-x revert-buffer`. See design.md Decision 5.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/mode` passes.
- Manual smoke test: open `~/.gptel/sessions/heading-test-20260430145834/branches/main/session.org` in chat-mode and verify the `* Test Heading` line inside the user block becomes ` * Test Heading`, the buffer is modified, and `org-element-parse-buffer` returns a correctly-shaped AST with the user special-block intact.

## Context

- `openspec/changes/gptel-chat-heading-scoping/design.md` Decision 5.
- Original repro: `~/.gptel/sessions/heading-test-20260430145834/branches/main/session.org`.
- Predicate: `gptel-chat--point-in-block-body-p` (added by task `add-point-in-block-body-predicate`).
