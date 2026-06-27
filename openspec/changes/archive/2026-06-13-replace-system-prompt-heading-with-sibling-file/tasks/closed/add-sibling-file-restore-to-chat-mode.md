---
name: add-sibling-file-restore-to-chat-mode
description: Add `gptel-chat--system-prompt-file-path` (drawer resolver) and `gptel-chat--apply-system-prompt-file` (installer) in `config/gptel/chat/menu.org`. Wire as the last step in `gptel-chat--apply-declared-preset` so the sibling file becomes the top tier in the system-prompt restore precedence (sibling file then legacy drawer then preset).
change: replace-system-prompt-heading-with-sibling-file
status: done
relations:
  - blocked-by:delete-heading-reader-from-chat-menu
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — add the two new functions; wire `apply-system-prompt-file` into `apply-declared-preset`
- `config/gptel/chat/test/menu/preset-wiring-spec.el` (modify) — add precedence scenarios (sibling file wins; sibling file empty falls through; sibling file missing falls through)
- `config/gptel/chat/test/menu/system-prompt-file-spec.el` (add) — focused unit tests for the resolver and installer

## Why

design.md §Decision 1, Decision 4 — the sibling file is the top tier of the restore precedence and the source of truth for `gptel--system-message` in chat-mode buffers. This task wires that contract into the restore path. The pre-send refresh (next task `add-pre-send-refresh`) ensures the file is also re-read on every chat request — but the activation-time install handled here is what gives the buffer a usable system prompt before the first request.

architecture.md §Components — `gptel-chat--system-prompt-file-path` is shared between this task, the pre-send refresh, and the menu affordance. Adding it as a standalone resolver first lets the other tasks consume it without duplication.

The heading reader was removed in the prior task `delete-heading-reader-from-chat-menu`, leaving a clean integration point at the end of `gptel-chat--apply-declared-preset` for this task to take.

## Implementation steps

1. Add `gptel-chat--system-prompt-file-path` to `config/gptel/chat/menu.org`:
   - Signature: `()` — operates on the current buffer
   - Behavior: read the value of the `GPTEL_SYSTEM_PROMPT_FILE` property from the configuration drawer at `point-min` via `gptel-org--entry-properties` (or `org-entry-get` against `(point-min)` — confirm which is already in use by `apply-drawer-overrides`). When unset, return `nil`. When set, resolve relative to `(file-name-directory (or buffer-file-name (buffer-file-name (buffer-base-buffer))))`. Return the absolute path string, even when the file does not exist (callers handle existence).
2. Add `gptel-chat--apply-system-prompt-file` to the same file:
   - Signature: `()` — operates on the current buffer
   - Behavior: call the resolver. When nil, return without action. When non-nil, `file-readable-p` check; if false, return without action (no error, no warning — file may legitimately not exist yet). Otherwise read the file via `insert-file-contents` into a temp buffer and `buffer-string`. When the result is non-empty (after a `string-trim` check — match the "blank file is no-op" contract in the spec), `setq-local gptel--system-message <body>`. Read verbatim — do NOT trim the installed value.
3. Wire `(gptel-chat--apply-system-prompt-file)` as the final call in `gptel-chat--apply-declared-preset`, after the drawer overlay. Update the function's docstring to document the precedence: sibling file (this step) then legacy `:GPTEL_SYSTEM:` drawer overlay (one tier down) then preset (bottom).
4. `preset-wiring-spec.el` scenarios to add (corresponding to delta spec scenarios in `openspec/changes/.../specs/gptel/chat-mode.md`):
   - "Restore reads sibling file when property and file are present"
   - "Restore falls back to preset when sibling file is absent"
   - "Restore falls back to preset when property is unset"
   - "Sibling file wins over legacy drawer entry"
   - "Empty sibling file is a no-op"
   - Keep the existing "Legacy drawer GPTEL_SYSTEM respected when no sibling file" scenario (it remains valid as the middle tier)
5. New `system-prompt-file-spec.el`:
   - `describe "gptel-chat--system-prompt-file-path"`:
     - `it "returns nil when GPTEL_SYSTEM_PROMPT_FILE is unset"`
     - `it "resolves a basename relative to session.org directory"`
     - `it "honors an absolute path verbatim"`
   - `describe "gptel-chat--apply-system-prompt-file"`:
     - `it "installs file body as buffer-local gptel--system-message"`
     - `it "no-ops when resolver returns nil"`
     - `it "no-ops when file is unreadable"`
     - `it "no-ops when file is empty / whitespace-only and preserves prior value"`
     - `it "preserves verbatim whitespace and special characters from the file"`
6. Re-tangle `chat/menu.org`. Run the chat-mode test suite.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/run-tests.sh -d config/gptel/chat/test/menu
grep -n 'system-prompt-file-path\|apply-system-prompt-file' config/gptel/chat/menu.el
```

Expect: both functions tangle and load. Restore precedence scenarios pass with the new top tier installed correctly. Existing legacy-drawer-overlay scenarios continue to pass (back-compat tier intact).

## Context

architecture.md §Interfaces (new internal symbols, `chat/menu.org`) — this task adds the resolver and installer. The pre-send refresh (`add-pre-send-refresh`) and the menu affordance (`replace-system-prompt-infix-with-file-opener`) both consume the resolver.

design.md §Decision 4 — the installer is activation-time only; the per-request refresh is the next task.

design.md §Decision 6 — the legacy `:GPTEL_SYSTEM:` overlay stays in place as the back-compat middle tier; the installer added here runs after the overlay so its result supersedes the legacy value when both are present.

## Observations

- Resolver uses `org-entry-get` (not `gptel-org--entry-properties`) because `:GPTEL_SYSTEM_PROMPT_FILE:` is a chat-mode extension key — same pattern as `:GPTEL_PARENT_SESSION_ID:` handling in `gptel-chat--apply-drawer-overrides`. Keeps the upstream-tuple consumer focused on upstream-compatible keys.
- For indirect buffers, the resolver falls back to `(buffer-file-name (buffer-base-buffer))` so chat sessions opened in a clone buffer still resolve their sibling file correctly. Documented in the docstring.
- macOS quirk surfaced in testing: `find-file-noselect` canonicalises `/var/folders/...` symlinks to `/private/var/folders/...`. The basename-resolution test compares via `file-truename` on both sides to stay portable. The production resolver itself doesn't `file-truename` — that would force a stat on every property read; `file-readable-p` in the installer is sufficient to handle absent files.
- 497 chat specs pass (up from 483 — 14 new specs across the focused `system-prompt-file-spec.el` (9) and the precedence describe in `preset-wiring-spec.el` (6 minus the 1 fallback spec I added in the reader-deletion task = +5)).

## Discoveries

- discovery_id: disc-add-sibling-file-restore-1
  class: interface-drift
  description: |
    The transient two-tier docstring left by
    `delete-heading-reader-from-chat-menu` is now restored to the
    final three-tier shape: sibling file > legacy drawer > preset.
    Resolves the cycle-coupling concern raised in that task's
    `disc-delete-heading-reader-2`.
  affected_register_entry: register/invariant/system-prompt-heading-authoritative
  recommendation: |
    At integrate, mark the heading-authoritative invariant
    superseded and introduce a new invariant
    `register/invariant/system-prompt-file-authoritative` describing
    the sibling-file precedence top-tier. The new invariant is
    pinned by the new specs in `system-prompt-file-spec.el` and
    the precedence-end-to-end describe in `preset-wiring-spec.el`.

- discovery_id: disc-add-sibling-file-restore-2
  class: shape-fragmentation
  description: |
    `:GPTEL_SYSTEM_PROMPT_FILE:` is a new chat-mode-specific drawer
    key that joins `:GPTEL_PARENT_SESSION_ID:` as a non-upstream
    extension. The drawer-shape register entries don't yet
    enumerate the chat-mode extension keys explicitly.
  affected_register_entry: register/shape/session-document-layout
  recommendation: |
    During integrate, add `:GPTEL_SYSTEM_PROMPT_FILE:` to the
    enumeration of drawer keys in the session-document-layout
    register entry (alongside `:GPTEL_PARENT_SESSION_ID:`) so the
    extension shape is visible to future maintainers without having
    to read the source.
