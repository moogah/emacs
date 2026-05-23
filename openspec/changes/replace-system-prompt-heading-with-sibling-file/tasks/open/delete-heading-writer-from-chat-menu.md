---
name: delete-heading-writer-from-chat-menu
description: Delete the chat-mode save-side heading writer (`gptel-chat--write-system-prompt-heading`, `--config-drawer-end`, `--turn-block-marker-re`) and unwire it from `gptel-chat--save-state`. The `* System Prompt` heading is gone from the canonical layout; the save path no longer needs to materialize or rewrite a heading body, and the sibling file is canonical and never written by the save hook.
change: replace-system-prompt-heading-with-sibling-file
status: needs-review
relations: []
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — delete the writer, the two helper symbols it uses, and the call site in `gptel-chat--save-state`
- `config/gptel/chat/test/menu/save-state-spec.el` (modify) — delete heading-materialise and heading-rewrite `it` blocks; keep the "`:GPTEL_SYSTEM:` never written" scenario (it remains valid)

## Why

design.md §Goals — with `* System Prompt` and `* Chat` headings gone from the canonical layout, the save path no longer needs to:
- serialize `gptel--system-message` into a heading body,
- materialize headings for pre-Addendum sessions,
- bound rewrites away from turn blocks when `* Chat` is missing.

All of that complexity (introduced by `harden-system-prompt-save-against-missing-chat-heading` in the prior change) was necessary precisely because heading-body editing and turn-block editing shared the same document. With the prompt out of `session.org` entirely (sibling file, written only at creation and only edited by the user), the save path collapses back to "write the drawer; do nothing else."

The drawer write-exclusion for `:GPTEL_SYSTEM:` (`register/invariant/drawer-system-key-write-exclusion`, load-bearing) continues to hold; nothing here changes that contract.

## Implementation steps

1. Delete the following from `config/gptel/chat/menu.org` (and their introducing commentary blocks — search for "System Prompt heading save"):
   - `gptel-chat--config-drawer-end`
   - `gptel-chat--turn-block-marker-re`
   - `gptel-chat--write-system-prompt-heading`
   - The `declare-function jf/gptel--session-headings-block` form near the writer (the sessions-side helper is also deleted in `revert-initial-session-body-and-delete-headings-block`).
2. Remove the call to `(gptel-chat--write-system-prompt-heading)` from `gptel-chat--save-state`. Update the function's docstring: drop the paragraph that says "system prompt is serialised into the `* System Prompt` heading body" — the system prompt is no longer written by the save path at all.
3. In `config/gptel/chat/test/menu/save-state-spec.el`, delete `it` blocks that assert heading materialisation, heading body rewrite, or off-nominal recovery for missing `* Chat`. Keep:
   - "GPTEL_SYSTEM never written by the save path" (unchanged)
   - "Drawer carries full preset snapshot on save" (unchanged)
   - Any drawer-write scenario that does not involve a heading
4. Re-tangle `chat/menu.org`. Run the chat-mode save-state test suite.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/run-tests.sh -d config/gptel/chat/test/menu
grep -n 'write-system-prompt-heading\|turn-block-marker-re\|config-drawer-end' config/gptel/chat/menu.el config/gptel/chat/menu.org
```

Expect: no matches in `menu.el` or `menu.org` for the deleted symbol names. The save-state spec passes with the reduced (heading-free) scenario set.

## Context

architecture.md §Components and §Interfaces — this task is the save-side counterpart to `delete-heading-reader-from-chat-menu`. After both land, `chat/menu.org` no longer has any heading-aware code on either the restore or save path.

design.md §Decision 3 — the sibling file is canonical; the save path never writes it. This task encodes that decision on the save side by removing the heading-write surface entirely; the sibling-file path is independent of the save hook and lives in `chat/menu.org` only via the create/refresh helpers added in `add-sibling-file-restore-to-chat-mode` and `add-pre-send-refresh`.

## Observations

- The `* System Prompt heading save` section in `chat/menu.org` was the host for `gptel-chat--save-state` itself (not just the heading writer). Renamed the section to `* Save state hook entry` and shrunk the babel block to just the save-state hook. Its docstring now documents that the system prompt is owned by the sibling `system-prompt.<ext>` file and that the save hook never touches the prompt in any form.
- Two commentary lines in the `* System Prompt heading restore` section of `chat/menu.org` still reference `gptel-chat--write-system-prompt-heading`. They live inside the reader-section commentary (not the deleted save-side commentary) and are owned by the subsequent `delete-heading-reader-from-chat-menu` task. Left untouched here so the deletion scopes don't overlap.
- `gptel-chat--system-prompt-heading-body-region` is consumed by both the reader (`gptel-chat--system-prompt-heading-body`) and the deleted writer. With the writer gone, the helper is read-only until the reader task removes it. Renamed the helper's describe block from "shared reader/writer scan helper" to "helper, reader-only" and dropped the third `it` that round-tripped through `gptel-chat--write-system-prompt-heading`.
- The off-nominal / materialise / heading-rewrite / idempotence `it` blocks in `save-state-spec.el` were deleted wholesale — all of them depended on the writer's behaviour. The "GPTEL_SYSTEM never written by the save path" contract is preserved by the existing `it "never writes GPTEL_SYSTEM even when gptel--system-message is set buffer-locally"` in the `describe "gptel-chat--save-state (integration)"` block (line 413 of the pre-edit file). 117 specs pass after the deletion (down from 130-ish before; the 12-13 deleted scenarios were the heading-write coverage).

## Discoveries

- discovery_id: disc-delete-heading-writer-from-chat-menu-1
  class: dead-branch
  description: |
    Once the heading writer is gone, `gptel-chat--save-state` is a
    pure drawer + parent-id writer with no document-mutation step.
    The whole "save path materialises the canonical layout" branch
    (and its recovery sub-branches for missing `* Chat`) becomes dead
    code — the only writer to the document is the user.
  affected_register_entry: register/invariant/system-prompt-heading-authoritative
  recommendation: |
    Mark the invariant entry superseded at integrate. The new
    contract is "the sibling file is the system-prompt source of
    truth; the document does not represent the system prompt at
    all." No save-path behavior pins a heading-authoritative
    invariant anymore.

- discovery_id: disc-delete-heading-writer-from-chat-menu-2
  class: responsibility-leakage
  description: |
    `gptel-chat--system-prompt-heading-body-region` lived under the
    save-side test block (`describe "system prompt heading save"`)
    even though it was a shared reader/writer helper. With the
    writer deleted, it's read-only — but the test colocation
    suggested it was owned by the save side. The next task
    (`delete-heading-reader-from-chat-menu`) will delete both the
    helper and its consumer, but the misplaced ownership in the test
    layout is worth noting for the cleanup.
  affected_register_entry: register/shape/session-document-layout
  recommendation: |
    No action needed in this cycle. Surface to the reader-deletion
    task as a heads-up: when it removes
    `gptel-chat--system-prompt-heading-body-region` from menu.org,
    also remove the `describe "gptel-chat--system-prompt-heading-body-region
    (helper, reader-only)"` block from save-state-spec.el — that
    block survives this task only because the helper still has a
    (reader-only) caller.
