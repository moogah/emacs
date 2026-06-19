---
name: pre-send-compose-and-wire
description: Widen the pre-send function to compose gptel--system-message = role-base + env block at the tail, wholesale, on every send from a gptel-chat-mode buffer (chat and agents), behind the existing mode guard. Behavioral specs for tail placement, two-send idempotency, scope-grown reflection, and non-chat-mode no-op.
change: gptel-dynamic-environment-preamble
status: done
relations:
  - blocked-by:env-block-builder
  - blocked-by:stable-role-base
---

> Third and final task. Wires the builder (`env-block-builder`) and the stable
> role base (`stable-role-base`) into the existing pre-send seam. Implements
> design D1 (one widened composer, not a second advice) and D4 (tail append).

## Goal

On every `gptel-request` dispatched from a `gptel-chat-mode` buffer, the
effective `gptel--system-message` is recomposed wholesale as
`role + "\n\n" + environment-block`, with the environment block as the tail
section. Recomposition reads only stable sources (sibling file / role base +
the freshly built block) so blocks never accumulate and mid-session scope
changes appear on the next send.

## Files to modify

- `config/gptel/chat/menu.org` — widen `gptel-chat--refresh-system-prompt-from-file`
  (the `:before` advice on `gptel-request`, ~`menu.org:399-447`) into the
  composer, or rename to reflect its broader job. Keep the existing
  `(derived-mode-p 'gptel-chat-mode)` guard and the `advice-add` wiring.
- `config/gptel/chat/test/menu/environment-preamble-spec.el` — add the
  `describe "pre-send composition"` block.

## Implementation steps

1. Inside the existing `(when (derived-mode-p 'gptel-chat-mode) ...)` guard:
   a. Resolve ROLE content (current logic, defensive):
      - if `:GPTEL_SYSTEM_PROMPT_FILE:` set and the file is readable and non-blank
        → role = file body (re-read from file each send);
      - else → role = `gptel-chat--system-prompt-base` (from `stable-role-base`),
        or `""` when nil.
      Preserve today's defensive behavior: an unreadable sibling logs a warning
      and falls back to the base rather than dropping the prompt.
   b. Build ENV = `(gptel-chat--build-environment-block)` (UNCONDITIONAL — every
      chat-mode buffer gets it, even with no sibling file).
   c. Compose and set WHOLESALE:
      `(set (make-local-variable 'gptel--system-message)
            (if (string-empty-p role) env (concat role "\n\n" env)))`
      Never read the prior `gptel--system-message` as an input (idempotency, D2).
2. Confirm `advice-add 'gptel-request :before #'<fn>` remains installed and
   idempotent under re-tangle (the existing comment notes this).
3. `./bin/tangle-org.sh config/gptel/chat/menu.org`.

## Tests (Buttercup, behavioral)

`describe "pre-send composition"` (real `gptel-chat-mode` buffer; mirror setup in
`pre-send-refresh-spec.el`):
- the environment block is the TAIL of `gptel--system-message` after invoking the
  composer (role precedes it).
- TWO consecutive composer invocations → `gptel--system-message` contains exactly
  ONE environment block (no accumulation — the D2 idempotency guard).
- extending the buffer's `GPTEL_SCOPE_*` drawer keys between two invocations is
  reflected in the second composition's block.
- no sibling file present → block still appended (role = base or empty).
- a NON-chat-mode buffer: invoking `gptel-request` (or the advice) adds no block
  and leaves its system message unchanged.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/run-tests.sh -d config/gptel/chat            # full chat suite (regression)
./bin/run-tests.sh -d config/gptel/tools           # agents share the seam — smoke
grep -n "build-environment-block\|system-prompt-base" config/gptel/chat/menu.org
```

Manual smoke (optional): open a session.org, send a turn, confirm the model's
system message tail carries the work root + scope; expand scope mid-session and
confirm the next send reflects it.

## Spec scenarios covered

`specs/gptel/environment-preamble.md` → Requirements "Environment block appended
to the composed system message" (all scenarios), "Idempotent wholesale rebuild"
(both scenarios), and "Scoped to chat-mode buffers only".
