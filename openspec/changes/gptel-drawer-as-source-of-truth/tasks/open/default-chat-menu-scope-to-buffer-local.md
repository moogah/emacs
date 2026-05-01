---
name: default-chat-menu-scope-to-buffer-local
description: Bind `gptel--set-buffer-locally` to `t` for the lifetime of `gptel-chat-menu` so tool / model / temperature toggles from the chat menu apply to the current buffer (and serialize to the drawer on save) instead of mutating the global default.
change: gptel-drawer-as-source-of-truth
status: ready
relations: []
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — wrap `gptel-chat-menu` invocation
- `config/gptel/chat/test/menu/menu-buffer-local-spec.el` (new Buttercup spec) — assert tool toggle from chat menu sets `gptel-tools` buffer-locally and not globally

## Implementation steps

1. Open `config/gptel/chat/menu.org` and locate the `gptel-chat-menu` `transient-define-prefix` (around `menu.el:571`).
2. Pick the wrapping strategy. Two viable approaches:
   - **(a) Setup/teardown via transient hooks**: add a `:setup-children` or use `:init-value` plus `transient-current-prefix` to bind `gptel--set-buffer-locally` on entry and restore on exit. This is the cleanest if transient supports it without a separate `unwind-protect`.
   - **(b) Wrap the body in a `let`-bound helper**: define a small wrapper command that does `(let ((gptel--set-buffer-locally t)) (transient-setup 'gptel-chat-menu))` and bind THAT command to the keymap instead of the prefix directly.
   
   Approach (b) is simpler and reliable. Pick (b) unless transient's setup machinery has a clean idiom that avoids the wrapper.
3. If using (b): rename the existing `transient-define-prefix gptel-chat-menu` to `gptel-chat--menu-prefix` (or keep the name and add a wrapper around the autoload). Define `gptel-chat-menu` as an interactive command that `let`-binds `gptel--set-buffer-locally` to `t` and calls `(call-interactively #'gptel-chat--menu-prefix)`. Update the chat-mode keymap binding to point at `gptel-chat-menu`.
4. Add a comment block explaining the design (Decision 5): "Chat-menu defaults configuration scope to buffer-local. The user retains the upstream `gptel--infix-variable-scope` toggle to switch to global / oneshot per-invocation."
5. Re-tangle: `./bin/tangle-org.sh config/gptel/chat/menu.org`.
6. Create `config/gptel/chat/test/menu/menu-buffer-local-spec.el` (Buttercup) with scenarios:
   - **"chat-menu binds gptel--set-buffer-locally to t for its lifetime"**: invoke `gptel-chat-menu` (via the wrapper command), confirm `gptel--set-buffer-locally` is `t` inside the menu (use a spy on a tool-toggle path or check the variable from inside a setup hook); confirm it is restored to its prior value (likely nil) after exit.
   - **"upstream gptel-menu retains nil default"**: directly call upstream `gptel-menu` (not the chat wrapper); confirm `gptel--set-buffer-locally` remains at its default.
   - **"tool toggle from chat-menu sets gptel-tools buffer-locally"**: simulate `gptel--set-with-scope 'gptel-tools NEW-LIST gptel--set-buffer-locally` inside a chat-mode buffer with the binding active; assert `(local-variable-p 'gptel-tools)` is `t` and `(default-value 'gptel-tools)` is unchanged.
   - **"buffer-local tool change persists to drawer on save"** (integration): set up a chat-mode buffer with a registered preset, simulate a tool toggle that goes buffer-local, run `save-buffer`, assert the saved drawer text contains a `:GPTEL_TOOLS:` line listing the new tool set. (This requires Task 3 to be merged for the writer to emit the line.)

## Design rationale

Decision 5 in `design.md` picks the wrapper approach over patching every infix individually. Wrapping is local, reversible, and respects upstream's machinery — the user can still flip the scope toggle to global or oneshot per-invocation, but the default is sane for chat-mode usage.

`setq-local`-ing `gptel--set-buffer-locally` in the chat-mode init was rejected because it would leak the default to upstream `M-x gptel-menu` invocations in the same buffer. Users explicitly invoking the upstream menu may want global-default behavior; we preserve their choice.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat` passes including the new `menu-buffer-local-spec.el`.
- Manual: open a session in `./bin/emacs-isolated.sh`, invoke chat-menu → Select tools → toggle a tool → confirm. Run `(default-value 'gptel-tools)` in `*scratch*` — unchanged. Run `gptel-tools` in the chat buffer — reflects the toggle. `C-x C-s` and inspect the drawer — `:GPTEL_TOOLS:` reflects the new list.

## Context

- design.md § Decision 5 — "Default `gptel--set-buffer-locally` to `t` in `gptel-chat-menu`"
- specs/gptel/chat-mode.md — Requirement: Chat-menu defaults configuration scope to buffer-local
