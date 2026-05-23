---
name: replace-system-prompt-infix-with-file-opener
description: Replace the upstream `gptel-system-prompt` infix in the chat-mode transient menu with a new "Edit system prompt" suffix that opens the sibling system-prompt file in another window via `find-file-other-window`. When the drawer carries no `:GPTEL_SYSTEM_PROMPT_FILE:` property yet, the suffix prompts for a filename (default `system-prompt.md`), writes the property, creates an empty file, and opens it. Upstream `gptel-system-prompt` (invoked outside the chat-mode menu) is unchanged.
change: replace-system-prompt-heading-with-sibling-file
status: done
relations:
  - blocked-by:add-sibling-file-restore-to-chat-mode
merge_commit: 6cd453b
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — add `gptel-chat--edit-system-prompt-file` (transient suffix + create-if-missing helper); replace the upstream `gptel-system-prompt` infix entry in the chat-mode menu definition
- `config/gptel/chat/test/menu/system-prompt-file-spec.el` (modify, file added in `add-sibling-file-restore-to-chat-mode`) — add menu-affordance `describe` block

## Why

design.md §Decision 5 — one affordance, one mental model. The sibling file is the source of truth for the system prompt; the menu takes the user to the file rather than offering an in-menu text field that would bypass the file (and lose its content on the next pre-send refresh).

Keeping the upstream infix alongside the file-opener would create a confusing double-write surface: edits via the upstream infix would mutate buffer-local `gptel--system-message` only to be overwritten on the next send. Replacing the infix in the chat-mode menu only — not globally — preserves the upstream behavior for non-chat-mode contexts.

architecture.md §Components — this is the third consumer of `gptel-chat--system-prompt-file-path` (alongside the activation-time installer and the pre-send refresh). The shared resolver keeps the file-resolution logic single-sourced.

## Implementation steps

1. Add `gptel-chat--edit-system-prompt-file` to `config/gptel/chat/menu.org`:
   - Interactive command (`(interactive)`)
   - Step 1: call `gptel-chat--system-prompt-file-path` to resolve.
   - Step 2a: if the resolver returns a non-nil path AND the file exists, `find-file-other-window` it.
   - Step 2b: if the resolver returns nil (no `:GPTEL_SYSTEM_PROMPT_FILE:`) OR the file does not exist (property points at a missing file — rare but handle it), prompt the user via `read-string` for a filename (default `system-prompt.md`). Then:
     - Write `:GPTEL_SYSTEM_PROMPT_FILE: <chosen-name>` into the drawer via `org-entry-put` (point-min context, matching the existing drawer-write style in `gptel-chat--write-config-drawer`).
     - Save the session buffer so the new drawer entry is persisted (use `save-buffer`; the existing `save-state` hook will rewrite the drawer with the new key alongside the snapshot).
     - Create the (empty) file via `(with-temp-file <resolved-path>)` or `write-region "" nil <path>` — confirm idiomatic style.
     - `find-file-other-window` the new file.
   - Add a docstring referencing the spec requirement and architecture.md.
2. In the chat-mode transient menu definition (the `gptel-chat-menu` fork), locate the entry that wires the upstream `gptel-system-prompt` infix. Replace it with a suffix entry calling `gptel-chat--edit-system-prompt-file`. Pick a label that reads cleanly in the transient grid (e.g., `"Edit system prompt"` or `"System prompt file"`; defer to whatever style the rest of the chat-mode menu uses). Preserve the existing key binding.
3. **Do not modify upstream `gptel-menu`** or `gptel-system-prompt`. Only the chat-mode fork's entry changes.
4. Tests in `system-prompt-file-spec.el`:
   - `describe "gptel-chat--edit-system-prompt-file"`:
     - `it "opens existing sibling file in other window"` — spy on `find-file-other-window`; set up the drawer with `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md` and a sibling file present; invoke the command; assert the spy was called with the resolved path
     - `it "creates and opens a new file when property is unset"` — set up a buffer with no property, mock `read-string` to return `"system-prompt.md"`; invoke; assert the drawer gained the property, the file exists in the buffer's directory, and `find-file-other-window` was called with that path
     - `it "creates and opens a new file when property is set but file is absent"` — drawer carries the property pointing at a missing file; invoke; assert the file is created and opened (no re-prompt)
     - `it "prompts only when no property is set"` — set up with the property present and file present; invoke; assert `read-string` was NOT called
5. Re-tangle `chat/menu.org`. Run the chat-mode test suite.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/run-tests.sh -d config/gptel/chat/test/menu
grep -n 'edit-system-prompt-file\|gptel-system-prompt' config/gptel/chat/menu.el
```

Expect: the new command tangles and loads; the chat-mode menu references `gptel-chat--edit-system-prompt-file` instead of the upstream `gptel-system-prompt` infix; upstream symbol is still referenced by upstream code paths (not removed); test cases pass.

## Context

architecture.md §Interfaces — `gptel-chat--edit-system-prompt-file` is the new transient command; it consumes `gptel-chat--system-prompt-file-path` from `add-sibling-file-restore-to-chat-mode`.

design.md §Decision 5 — rationale for replacing the upstream infix in the chat-mode menu only, not globally.

design.md §Open Question 2 — transient suffix vs. bound key; default to transient suffix in this task. A bound key can be added later if there's demand.

## Cycle 1779565028 updates (cycle-1779565028)

- **Unblocked**: `add-sibling-file-restore-to-chat-mode` landed
  (merge `301f99c`). `gptel-chat--system-prompt-file-path` is
  available; reuse it for step 2a of the implementation rather than
  reimplementing the drawer lookup.
- **Existing menu transient** lives in `config/gptel/chat/menu.org`
  under the `* =gptel-chat-menu= transient` section, with
  subsections for `Send suffix`, `Scope default: buffer-local for
  chat-menu lifetime`, and `Prefix definition`. The upstream
  `gptel-system-prompt` infix entry should be locatable in the
  prefix definition block.
- **Test fixture pattern**: the new
  `system-prompt-file-spec.el` already establishes a
  `find-file-noselect` against a temp session.org pattern. Reuse
  the helper functions (`jf-sysprompt-file-test--write-session`,
  `jf-sysprompt-file-test--write-sibling`) — extend rather than
  re-declare.
- **macOS quirk**: as in the restore task, use `file-truename`
  when comparing the resolved path to an expected path in tests;
  `find-file-noselect` canonicalises `/var/folders/...` to
  `/private/var/folders/...`.

## Cycle 1779568860 updates (cycle-1779568860)

- **No new blockers.** All three cited entry sources are still in
  place: the resolver `gptel-chat--system-prompt-file-path`, the
  activation installer `gptel-chat--apply-system-prompt-file`, and
  (new this cycle) the pre-send refresh
  `gptel-chat--refresh-system-prompt-from-file` (merge `b40c94b`).
- **`chat/menu.org` is now larger.** Cycle-2 T2 added a new section
  `* System Prompt sibling file > ** Pre-send refresh` immediately
  after the activation installer, plus an `(advice-add 'gptel-request
  :before #'gptel-chat--refresh-system-prompt-from-file)` form at
  module load. This sits ABOVE the `* =gptel-chat-menu= transient`
  section that this task targets, so no conflict — but the file
  has grown, so locate the upstream `gptel-system-prompt` infix
  inside the `Prefix definition` block by scrolling past the new
  pre-send-refresh material.
- **Pre-send refresh is the third consumer** of
  `gptel-chat--system-prompt-file-path` (now: resolver + activation
  installer + pre-send refresh + this task's file-opener will make
  four). Task body's "third consumer" prose at architecture.md
  §Components is now mildly stale — the new affordance is the
  fourth, not the third. Cosmetic; no implementation impact.
- **Register supersession landed**: `register/shape/session-document-layout`
  and `register/invariant/system-prompt-heading-authoritative` are
  now `status: superseded` with the new contracts at
  `register/shape/session-sibling-system-prompt-file` and
  `register/invariant/system-prompt-file-authoritative`. This task's
  new `gptel-chat--edit-system-prompt-file` will be the fourth pinned
  consumer of the new invariant; no test-side change needed beyond
  the four new `it`s the body already prescribes.
- **macOS-quirk + test-fixture-pattern reminders from cycle-1 still
  apply unchanged**.

