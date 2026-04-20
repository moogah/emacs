---
name: menu-integration
description: gptel-chat-menu transient with rebound Send suffix
change: gptel-chat-mode
status: blocked
relations:
  - blocked-by:send-command
  - blocked-by:preset-wiring
---

## Files to modify
- `config/gptel/chat/menu.org` (modify — add menu section)
- `config/gptel/chat/menu.el` (tangled)
- `config/gptel/chat/mode.org` (modify — bind the menu in the keymap)
- `config/gptel/chat/test/menu/menu-send-rebind-spec.el` (new)

## Implementation steps
1. Define `gptel-chat-menu` as a `transient-define-prefix` that mirrors
   `gptel-menu`'s configuration layout but with the Send suffix replaced
   by a suffix that invokes `gptel-chat-send`.
2. Decide the transient mechanism (Open Question 2 — pick during
   implementation):
   - **Option A**: `transient-replace-suffix` on a copy of `gptel-menu`'s
     layout — smaller diff, but couples to upstream's layout spelling.
   - **Option B**: fresh `transient-define-prefix` that references the
     same *infix symbols* upstream uses for preset/model/backend/tools/
     context, with our own Send suffix.
   - Pick whichever produces the smallest ongoing maintenance against
     upstream `gptel-transient.el` drift.
3. Bind `gptel-chat-menu` on `gptel-chat-mode-map` at the key chat-mode
   users should associate with the menu. Suggested: `C-c C-,` (mirror
   common gptel muscle memory) — final key decided during implementation,
   but it should NOT be the exact same key as `gptel-menu`'s global
   binding (we want `M-x gptel-menu` to remain available unchanged).
4. `M-x gptel-menu` invoked directly SHALL continue to work with its
   upstream layout in a chat-mode buffer. Configuration suffixes (preset
   pick, model, backend, tools, system message, context, temperature)
   all mutate buffer-local variables and require no modification. Only
   the Send suffix from our *rebound* prefix differs.
5. Tests:
   - `gptel-chat-menu` transient is defined and callable.
   - The rebound Send suffix invokes `gptel-chat-send` (spy), not
     `gptel--suffix-send`.
   - `M-x gptel-menu` invoked in a chat-mode buffer retains upstream
     layout; its Send suffix (`gptel--suffix-send`) is unchanged.
   - Configuration suffixes work in a chat-mode buffer: picking a preset
     calls `gptel--apply-preset`, changing model updates
     `gptel-model` buffer-locally.

## Design rationale
Decision 15: the upstream Send suffix (`gptel--suffix-send`) assumes
gptel-mode's prompt/response-prefix conventions and `gptel` text-property
markers. Invoking it in a chat-mode buffer would insert response text
outside our block structure. Replacing exactly one suffix is the
**narrowest** possible fork — transient's public API supports it, and
the maintenance cost is a few lines of code.

Alternatives rejected:
- **Do nothing; expect users to avoid the menu's Send button.** Rejected
  — the Send button is too obvious a trap; a user will press it and get
  wrong behaviour.
- **Advise `gptel--suffix-send` to delegate when `(derived-mode-p
  'gptel-chat-mode)`.** Rejected — advice on upstream internals is
  fragile, and would affect `M-x gptel-menu` globally in a way that
  surprises users who expect upstream behaviour.
- **Duplicate the whole `gptel-menu` layout verbatim.** Rejected —
  maintenance burden; any new infix upstream adds is one we'd need to
  mirror.

## Design pattern
The user gets the full upstream menu with exactly one behaviourally
different button. Configuration infixes are reused by reference — we
don't copy their implementations, just reference the same symbols.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/menu.org` succeeds.
- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds (keymap
  binding).
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` passes send-rebind
  suite.
- Scenarios (spec §"gptel-menu integration with rebound Send"):
  - Menu configuration works in chat-mode buffer (via `M-x gptel-menu`)
  - `gptel-chat-menu` Send invokes `gptel-chat-send` (not upstream)

## Context
- design.md §Decision 15 (gptel-menu integration — Send rebound,
  configuration free)
- design.md §Open Questions #2 — transient mechanism choice deferred to
  implementation
- specs/gptel-chat-mode/spec.md §"gptel-menu integration with rebound Send"
- architecture.md §`gptel-chat-menu`
