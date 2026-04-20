---
name: display-layer
description: Overlay-based role distinction with toggle command
change: gptel-chat-mode
status: blocked
relations:
  - blocked-by:mode-definition
---

## Files to modify
- `config/gptel/chat/display.org` (new)
- `config/gptel/chat/display.el` (tangled)
- `config/gptel/chat/test/display/display-layer-spec.el` (new)

## Implementation steps
1. Define two faces, `gptel-chat-user-face` and `gptel-chat-assistant-face`,
   with subtle defaults (e.g. slight background tint, or distinct
   foreground) — users can customize via `M-x customize-face`.
2. Implement `gptel-chat--refresh-overlays`:
   - Call `gptel-chat--parse-buffer` for the turn list.
   - For each user turn, add an overlay spanning the block *body* (between
     `#+begin_user\n` and `#+end_user`), with the `face` property set to
     `gptel-chat-user-face`. Do NOT cover the delimiter lines.
   - Same for assistant turns with `gptel-chat-assistant-face`.
   - Give overlays a `gptel-chat-display` property so `remove-overlays` can
     target them cleanly.
3. Install the refresher on mode activation:
   - Add to `gptel-chat-mode-hook`: one-shot initial pass.
   - Add to `after-change-functions` (buffer-local) with a debounced
     callback via `run-with-idle-timer`. Debounce delay = a `defcustom`
     `gptel-chat-display-refresh-delay` (default 0.1s; see Open Question
     3).
4. Implement `gptel-chat-toggle-display-layer` (interactive):
   - Toggle a buffer-local boolean `gptel-chat--display-enabled`.
   - If disabled: `(remove-overlays (point-min) (point-max)
     'gptel-chat-display t)` and clear `after-change-functions` entry.
   - If enabled: reinstall the hook and run a full refresh.
   - Default state: enabled on fresh buffers (spec scenario "Display layer
     is active by default").
5. Tests:
   - Fresh buffer with two turns → overlays exist on both block bodies
     with correct faces.
   - `buffer-substring-no-properties` of the full buffer matches the
     on-disk file (overlays must not alter text).
   - Toggle off → no overlays with the `gptel-chat-display` property.
   - Toggle on → overlays reappear.

## Design rationale
Decision 5: overlays are the least invasive mechanism for role distinction
and cleanly separate from buffer content. Alternatives rejected:
- **Font-lock keywords**: awkward for multi-line block content (needs a
  state machine in `font-lock-extend-after-change-region-function`), and
  re-fontification during streaming is wasteful.
- **Line-prefix overlays** (e.g. `▸` per user line): discussed in
  exploration; adds visual noise and cursor-column confusion. Deferred.
- **`display` property text overlays**: reserved for the deferred
  delimiter-hiding refinement ("store-symmetric, display-asymmetric") —
  explicitly out of scope for v1.

Debounced after-change refresh handles streaming without churning on every
chunk. Display is cleanly toggleable — toggle just enables/disables the
refresher and removes existing overlays.

## Design pattern
Standard Emacs overlay lifecycle:
1. Initial pass in mode hook.
2. Incremental refresh via `after-change-functions` with a short idle
   debounce (100–200ms per Decision 5 Mitigation / Open Question 3).
3. Teardown via `remove-overlays` with a marker property for targeting.

Display is strictly presentational — no other module reads its overlays
or depends on its state. Removing it leaves the buffer semantically
identical.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/display.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/display` passes.
- Scenarios (spec §"Display-layer role distinction"):
  - Display layer is active by default
  - `buffer-substring-no-properties` matches on-disk content
  - Toggle off removes visual distinction; delimiters/content unchanged

## Context
- design.md §Decision 5 (display layer — overlays, refreshed on change)
- design.md §Open Questions #3 (debounce timing as `defcustom`)
- design.md §Open Questions #5 (per-buffer toggle state, no persistence)
- specs/gptel-chat-mode/spec.md §"Display-layer role distinction"
- architecture.md §`gptel-chat-display`
