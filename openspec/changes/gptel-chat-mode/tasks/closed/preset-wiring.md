---
name: preset-wiring
description: Preset detection via property drawer/file-local and buffer-local gptel--apply-preset
change: gptel-chat-mode
status: needs-review
relations:
  - blocked-by:mode-definition
---

## Files to modify
- `config/gptel/chat/menu.org` (new — preset-parsing section)
- `config/gptel/chat/menu.el` (tangled)
- `config/gptel/chat/test/menu/preset-wiring-spec.el` (new)

## Implementation steps
1. Implement `gptel-chat--apply-declared-preset` called from
   `gptel-chat-mode-hook`. Resolution order (highest precedence first):
   a. An Org `:PROPERTIES:` drawer at `point-min` with a `:GPTEL_PRESET:
      name` line. Use the same parser upstream uses (see
      `gptel-org.el:564-572`) — do not reinvent property-drawer parsing.
   b. A file-local `gptel--preset: name` variable via the `-*- ... -*-`
      header or a `Local Variables:` block. `gptel--preset` is already
      declared `safe-local-variable` upstream, so standard
      `hack-local-variables` handles it.
2. If a preset name is found, call `gptel--apply-preset` with a buffer-
   local setter:
   ```elisp
   (gptel--apply-preset (intern preset-name)
     (lambda (sym val) (set (make-local-variable sym) val)))
   ```
   This installs `:backend`, `:model`, `:system`, `:tools`, `:temperature`,
   etc. as buffer-local values. Subsequent `gptel-request` calls in the
   buffer read those values.
3. If NO preset is declared, do nothing — the buffer inherits global
   defaults (same behaviour as any `gptel-request` caller with no preset).
4. **Do NOT enable `gptel-mode`.** Chat-mode owns the major-mode role
   exclusively (Decision 16). This function only borrows upstream's
   preset-parsing behaviour.
5. Tests (spec §"Preset system integration"):
   - Preset applied from property drawer (`:GPTEL_PRESET: coding`) →
     `gptel--apply-preset` called with symbol `coding` and a buffer-local
     setter (spy).
   - Preset applied from file-local variable (`# -*- gptel--preset: coding
     -*-`).
   - No preset declared → no `gptel--apply-preset` call, no buffer-local
     mutation.
   - Property drawer wins over file-local when both are present with
     different names.
   - `gptel-mode` is NOT enabled as a side effect (spy on `gptel-mode`).

## Design rationale
Decision 15 collapses per-buffer configuration into the upstream preset
mechanism rather than inventing parallel `#+gptel-*:` keyword conventions.
This is the pattern `config/gptel/tools/persistent-agent.org:646-650`
already uses — one public API for all configuration, one setter pattern
for buffer-locality.

Decision 16 makes explicit that sessions (and chat-mode generally) never
enable `gptel-mode` minor mode. Running `gptel-mode` on top of chat-mode
would produce a mixed-format file that neither parser can read cleanly —
upstream's text-property `gptel--bounds` would coexist with chat-mode's
`#+begin_*` blocks with no way to reconcile which is authoritative.

Property drawer wins over file-local because the drawer is authored as
part of the buffer content, while file-locals may be inherited from a
parent directory's `.dir-locals.el` — explicit drawer intent overrides
inherited default.

## Design pattern
`persistent-agent.org:646-650` is the in-repo canonical example of
preset-parsing + buffer-local apply. Match that pattern. Do not invoke
`gptel--apply-preset` with the global setter (the default) — that would
leak preset-configured values into other buffers.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/menu.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` passes preset-wiring
  suite.
- Scenarios covered:
  - Preset applied from Org property drawer
  - Preset applied from file-local variable
  - No preset declared → no-op
  - Property drawer wins over file-local
  - `gptel-mode` is NOT enabled

## Context
- design.md §Decision 15 (preset system + gptel-menu integration)
- design.md §Decision 16 (sessions use chat-mode, never gptel-mode)
- `config/gptel/tools/persistent-agent.org:646-650` — canonical pattern
- `gptel-org.el:564-572` — upstream property-drawer parsing
- specs/gptel-chat-mode/spec.md §"Preset system integration"
- architecture.md §`gptel-chat-menu`
