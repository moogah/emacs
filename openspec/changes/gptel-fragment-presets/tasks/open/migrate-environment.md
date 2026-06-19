---
name: migrate-environment
description: Move the dynamic environment block into a dynamic fragment evaluated at the existing pre-send seam, and delete the builder defun, preserving tail placement and wholesale rebuild.
change: gptel-fragment-presets
status: blocked
relations:
  - "blocked-by:composer"
---

## Files to modify

- `config/gptel/presets/sources/environment.org` (create) — dynamic fragment:
  declares kind `dynamic` and the compute function.
- `config/gptel/presets/sources/environment.el` (tangled).
- `config/gptel/chat/menu.org/el` (modify) — env produced via the dynamic
  fragment at the pre-send seam; remove `gptel-chat--build-environment-block`
  defun (or relocate its body into the fragment function).
- `config/gptel/chat/test/menu/environment-preamble-spec.el` (modify) — assert
  fragment-sourced production; preserve tail/idempotency/drawer-sourcing checks.

## Implementation steps

1. Author `environment.org` as a **dynamic** fragment whose function reproduces
   the env block: work root (`default-directory` / `GPTEL_WORK_ROOT`) + scope
   summary from `GPTEL_SCOPE_*` drawer keys + "current as of this turn" framing +
   drawerless-buffer fallback. Decide wrap-and-relocate vs rewrite of the current
   `gptel-chat--build-environment-block` body (design §Open Questions).
2. Register the env fragment as the **tail** dynamic fragment of the chat/agent
   default compositions (composer task).
3. At the pre-send seam (the `:before` advice on `gptel-request` filtered to
   `gptel-chat-mode`), produce the env block by evaluating the dynamic fragment;
   keep the wholesale-rebuild invariant (compose from sources, never append to the
   prior `gptel--system-message`).
4. Remove the `gptel-chat--build-environment-block` defun once the fragment
   function is the sole producer.
5. Update `environment-preamble-spec.el`: env produced by the dynamic fragment at
   compose time; remains last section; two consecutive sends → exactly one block;
   scope grown mid-session reflected next send; non-chat buffers untouched.
6. Tangle and run the chat suite.

## Design rationale

The env block is the existing dynamic-content proof; making it a dynamic fragment
generalizes it under the fragment model without changing observable behavior
(design §Decision 4; `specs/environment-preamble/spec.md`). Tail placement keeps
the cacheable static prefix stable.

## Verification

- `grep -n "build-environment-block" config/gptel/chat/menu.el` (expect gone, or
  only the fragment function remains).
- `test -f config/gptel/presets/sources/environment.el`
- `./bin/run-tests.sh -d config/gptel/chat/test/menu`

## Context pointers

- Spec: `specs/environment-preamble/spec.md` (Environment block is a dynamic
  fragment) + existing `openspec/specs/environment-preamble/spec.md` (tail,
  idempotent rebuild, drawer-sourced, chat-only).
- Current code: `config/gptel/chat/menu.el` (`gptel-chat--build-environment-block`,
  pre-send composer advice).
