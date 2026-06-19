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

## Cycle 1 updates (cycle-1781883616)

> fragment-core landed and its interface is now **confirmed** in the register.
> Build against these concrete facts (no longer speculation):

- **Fragment value is a plist** `(:kind SYMBOL :sections ((name . body) ...))`
  — `:kind` ∈ `{static, dynamic}` (default `static`); a section is a cons
  `(name . body)` where `name` is the verbatim trimmed heading string and
  `body` is a whitespace-trimmed string. (`register/shape/fragment` reconciled;
  `register/shape/section` confirmed.)
- **Renderer:** `jf/gptel-fragment-render (fragment backend)` — `claude` only;
  each section → `"<tag>\nbody\n</tag>"`, body **verbatim**, sections joined
  with a **blank line** (`"\n\n"`). Unimplemented backend logs-then-signals
  `jf/gptel-fragment-unimplemented-backend` (`define-error` ⊂ `error`) — catch
  with `condition-case` on `error` if needed.
- **Parser:** `jf/gptel-fragment--parse-source` (Org string **or** file path →
  the fragment plist); batch-loadable (no interactive Org deps).
- **Kind is declared in source** via a `#+fragment_kind: static|dynamic` Org
  keyword (case-insensitive); unrecognized → `static`.
- **Section-name → tag:** `jf/gptel-fragment--section-name-to-tag` downcases,
  trims, and collapses whitespace runs to a single `_` (`Output Format` →
  `output_format`). Multi-word headings are safe.
- **Reviewer note:** a fragment with no sections renders to `""`. Decide
  explicitly whether the composer skips/rejects empty fragments rather than
  silently emitting an empty block.

## Cycle 2 updates (cycle-1781885402)

> composer + registration landed; the compose API and the static/dynamic-timing
> invariant are now **confirmed**. Build against these concrete facts:

- **Compose API:** `jf/gptel-fragment-compose (composition backend &optional context)`
  renders an ordered list of *fragment references* and joins with `"\n\n"`.
  `register/boundary/composer-compose` and `register/invariant/static-prerender-dynamic-compose`
  (load-bearing) both **confirmed**.
- **Environment is THE canonical dynamic fragment:** wire it as a `:kind dynamic :fn FN`
  reference (`register/shape/fragment-reference`), placed at the **tail**. The composer
  already exposes the seam var **`jf/gptel-fragment-environment-fn`** (defaults to
  `#'ignore`) — populate it with the environment-producing function (wrap/relocate the
  existing `gptel-chat--build-environment-block` body). `--default-composition` puts the
  dynamic env ref last for chat and agent.
- Dynamic fns own their own output formatting; the composer's `backend` arg is currently
  **inert** (static refs are pre-rendered) — do not rely on it for env formatting, and
  flag it as a candidate drop.
