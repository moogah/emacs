---
name: add-pre-send-refresh
description: Add `gptel-chat--refresh-system-prompt-from-file` and wire it to run before every chat request in chat-mode buffers, so that mid-session edits to the sibling system-prompt file are picked up on the next send without explicitly reopening or reverting `session.org`. Implementation prefers an upstream pre-send hook if one exists; otherwise narrow `:before` advice on `gptel-request` filtered to `derived-mode-p 'gptel-chat-mode`.
change: replace-system-prompt-heading-with-sibling-file
status: blocked
relations:
  - blocked-by:add-sibling-file-restore-to-chat-mode
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — add the refresh function and the hook/advice wiring
- `config/gptel/chat/test/menu/pre-send-refresh-spec.el` (add) — behavioral test: modify file on disk, simulate send, assert new content went out

## Why

design.md §Decision 4 — the sibling file is canonical; the buffer-local `gptel--system-message` is a per-request cache. The activation-time install (from `add-sibling-file-restore-to-chat-mode`) populates the cache when the buffer opens; the pre-send refresh keeps the cache aligned with the file on disk through the session's lifetime.

Without this refresh, a user editing the sibling file mid-session would need to `revert-buffer` `session.org` (or close + reopen) to pick up the change. That's clunky enough to undermine the "file is the source of truth" mental model the menu affordance reinforces.

architecture.md §Components — the refresh function consumes `gptel-chat--system-prompt-file-path` (added in the prior task). The wiring is the only chat-mode-specific advice on a non-chat-mode upstream function in this change; the predicate filter (`derived-mode-p 'gptel-chat-mode`) keeps the cost at zero for non-chat-mode `gptel-request` calls.

## Implementation steps

1. **Investigate upstream first.** Search for any pre-send hook in upstream `gptel`:
   ```
   grep -rn 'pre-send\|before-request\|request-hook' runtime/straight/repos/gptel/
   ```
   If an upstream hook exists (e.g., `gptel-pre-send-hook`, `gptel-request-hook`), use it. Document the chosen integration point in the function's docstring.
2. Add `gptel-chat--refresh-system-prompt-from-file` to `config/gptel/chat/menu.org`:
   - Signature: `()` — operates on the current buffer (the buffer the request originates from)
   - Behavior: when `(derived-mode-p 'gptel-chat-mode)` is non-nil, call `gptel-chat--system-prompt-file-path`. When the path is non-nil and `file-readable-p`, re-read the file via `insert-file-contents` and update buffer-local `gptel--system-message`. When the path is set but the file is unreadable, log a warning via `jf/gptel--log 'warn "system-prompt sibling file unreadable: %s" path` and leave the cached value in place. No-op when the path is nil or the predicate fails.
3. **Wiring** — two options, picked at implementation time based on step 1:
   - **A (preferred)**: if an upstream pre-send hook exists, add `gptel-chat--refresh-system-prompt-from-file` to it from `gptel-chat--install-preset-hooks` (where the buffer-local `before-save-hook` is also installed). The function's chat-mode predicate guard handles non-chat-mode invocations.
   - **B (fallback)**: `:before` advice on `gptel-request` (the single most general request entry point), with the function's chat-mode predicate as the only filter. Register the advice once at module load via a top-level `advice-add` (NOT per-buffer; the predicate handles filtering). Document the advice with a clear "removed via `M-x gptel-chat-unload-advice` (if needed)" comment.
4. New `pre-send-refresh-spec.el`:
   - `describe "gptel-chat--refresh-system-prompt-from-file"`:
     - `it "re-reads sibling file before dispatch"` — set up a chat-mode buffer with a sibling file containing `"Old prompt."`, install the cached value, modify the file on disk to `"New prompt."`, invoke the refresh, assert `gptel--system-message` is now `"New prompt."`
     - `it "no-ops when sibling file path is nil"`
     - `it "logs warning and preserves cache when file becomes unreadable"`
   - `describe "pre-send wiring"`:
     - `it "refresh fires when gptel-request is invoked from chat-mode buffer"` — spy on the refresh, invoke `gptel-request` (with whatever minimal arg stub keeps it from actually dispatching), assert called
     - `it "refresh does not fire for non-chat-mode buffers"` — same setup but in a plain buffer, assert not called
5. Re-tangle `chat/menu.org`. Run the chat-mode test suite.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/run-tests.sh -d config/gptel/chat/test/menu
grep -n 'refresh-system-prompt-from-file\|pre-send\|gptel-request' config/gptel/chat/menu.el
```

Expect: refresh function tangles and loads; wiring is in place via the chosen integration point; behavioral test confirms the cache refresh works end-to-end.

## Context

architecture.md §Components — pre-send refresh is one of the new internal symbols; integration choice (hook vs. advice) is recorded in this task.

design.md §Decision 4 — rationale for per-request refresh over file-notify watcher.

design.md §Open Question 1 — investigate upstream hook availability is explicitly the first implementation step; record the finding in the task's discoveries when closing.

design.md §Risks — synchronous I/O per request is acceptable (file is small, OS cache is warm). If a future profile flags this, the refresh can be gated on `file-attribute-modification-time` change; not done in this task.
