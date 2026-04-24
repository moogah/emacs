---
name: menu-integration
description: gptel-chat-menu transient with rebound Send suffix
change: gptel-chat-mode
status: done
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

## Review

Reviewed by agent 2026-04-23.

**Verdict:** Implementation is solid. Option B (fresh
`transient-define-prefix` with shared infix symbols) was correctly
chosen and executed — upstream `gptel-menu` is not mutated, shared
infixes (preset, provider, system prompt, tools, context) are
referenced by symbol. Keybinding `C-c C-,` lands on
`gptel-chat-mode-map` without clashing with existing chat-mode
bindings.

### Findings

1. **Send-coupled menu groups silently drop transient-args**
   (`config/gptel/chat/menu.el:417-486`). The fresh prefix mirrors
   every group of upstream's layout — including "Prompt from"
   (Minibuffer / Kill-ring / Respond in place), "Response to"
   (Echo area / Other buffer / gptel session / Kill-ring), and
   "Dry Run" (Inspect query Lisp / JSON). `gptel-chat--suffix-send`
   ignores `transient-args`, so these toggles are visible but
   dead. The Dry-Run inspector is worse: it calls
   `(gptel--suffix-send (cons "I" (transient-args ...)))` directly,
   leaking upstream's gptel-mode prompt-extraction semantics into
   a chat-mode buffer. This is a real spec-signal: Decision 15
   commits to "configuration is free, Send is rebound" but doesn't
   address Send-coupled options that are neither configuration nor
   Send. Severity: follow-up-task. **Opened as
   `menu-send-coupled-options-scope` (grouped with Finding #2).**

2. **Missing behavioral test for Scenario 1**
   (`config/gptel/chat/test/menu/menu-send-rebind-spec.el:89-218`).
   All 15 specs verify transient-layout structure (shared infix
   symbols, keybinding presence, upstream unmutated-ness); none
   invoke a configuration suffix from a chat-mode buffer and
   assert a buffer-local variable changed. Spec Scenario 1
   ("configuration actions … mutate buffer-local variables")
   is covered only transitively. Severity: follow-up-task.
   **Grouped into `menu-send-coupled-options-scope` (touches the
   same test file and spec section as #1).**

3. **`gptel-chat--suffix-send` interactive form computes args it
   discards** (`config/gptel/chat/menu.org:350-361`). The
   `(interactive (list (transient-args ...)))` form was building
   a value that the formal parameter `_args` immediately
   underscored-out. Cosmetic mismatch with the docstring's claim
   that "this suffix takes no transient arguments". Severity:
   inline-fix. **Fixed inline in commit `42ed46f`** — simplified
   to `(interactive)` with `()` formals. 45/45 menu specs still
   pass; no regression from the change (9 ERT + 3 Buttercup
   failures are all pre-existing unrelated bash-parser/scope
   tests, matching baseline).

### Dependents

- `verify-change`'s `blocked-by: menu-integration` was repointed to
  `blocked-by: menu-send-coupled-options-scope`. Finding #1 is
  user-visible (menu shows controls that silently do nothing) and
  should resolve before the change-wide verification sweep runs.
