---
name: migrate-environment
description: Move the dynamic environment block into a dynamic fragment evaluated at the existing pre-send seam, and delete the builder defun, preserving tail placement and wholesale rebuild.
change: gptel-fragment-presets
status: done
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

## Observations

- **Wrap-and-relocate chosen over rewrite** (design.md §Open Questions, resolved).
  The former `gptel-chat--build-environment-block` body and its sole helper
  `gptel-chat--render-scope-line` moved verbatim into
  `config/gptel/presets/sources/environment.org` as
  `jf/gptel-fragment-environment` / `jf/gptel-fragment-environment--render-scope-line`.
  The only adaptation is the new `(&optional _context)` parameter the composer
  passes to every dynamic reference's `:fn`; it is ignored because the block's
  inputs are INTRINSIC to the buffer (default-directory + drawer keys),
  preserving `register/boundary/environment-block-input-neutrality`. Behaviour is
  byte-for-byte preserved (same header prose, same verbatim glob rendering, same
  degraded fallback).

- **Single env producer.** The env block now has exactly one producer
  (`jf/gptel-fragment-environment`). Both the composer's tail dynamic reference
  (via `jf/gptel-fragment-environment-fn`, set by the source module) and the
  chat-mode pre-send composer funcall the SAME seam variable. The pre-send
  composer in `menu.org` was changed from `(gptel-chat--build-environment-block)`
  to `(funcall jf/gptel-fragment-environment-fn 'chat)` with an `ignore`/`""`
  guard for the not-yet-loaded case.

- **Seam timing.** `jf/gptel-fragment--default-composition` reads
  `jf/gptel-fragment-environment-fn` at composition-BUILD time (not at the moment
  the reference is realized). Loading `environment.el` sets the seam, so any
  composition built after the source module loads carries the live env fn. The
  chat-mode pre-send composer reads the seam var directly at send time, so it
  picks up the fn as soon as the source is loaded regardless of build order.

- **Load wiring is NOT yet in `gptel.org`.** `environment.el` lives under
  `presets/sources/`, which is neither on `load-path` nor loaded by `gptel.org`
  (the presets sub-module load wiring belongs to the registration/composer
  integration task, outside this task's write-set). The env spec therefore loads
  `environment.el` by absolute path before requiring its feature, so the spec
  exercises the real fn + seam regardless of init-time order. menu.org's
  `(require 'jf-gptel-fragment-environment nil t)` is a SOFT require (fails
  silently until the source is on load-path) — production correctness depends on
  a follow-up that loads the env source in `gptel.org`. See Discoveries.

- **Optional load-bearing strengthening added.** Per the cited advisory on
  `register/invariant/static-prerender-dynamic-compose`, the spec now asserts
  that two composes differing ONLY in live env input share a byte-identical
  static prefix up to the `# Environment` marker and differ only in the tail
  ("static prefix is byte-identical across composes; only the dynamic tail
  differs").

- Full `config/gptel/chat/test/menu` suite: 165 specs, 0 failed.

## Discoveries

- class: interface-drift
  affected_register_entry: register/boundary/composer-compose
  severity: low
  summary: |
    Confirmed the `backend` arg of `jf/gptel-fragment-compose` is inert at
    compose time, as flagged in the entry's status_note. This task's dynamic env
    fragment owns its own output formatting entirely (it returns a finished
    markdown block, never routing through `jf/gptel-fragment-render`), and static
    refs are pre-rendered — so no compose-time consumer reads `backend`. The
    arg is a candidate to DROP from the compose signature (and from
    `--default-composition`'s contract). Push-back: keeping it "for signature
    parity with the renderer" is weak justification given it is `(ignore backend)`
    in the body; dropping it removes a misleading parameter. Non-blocking for this
    task (the env fragment does not depend on it either way).

- class: invariant-gap
  affected_register_entry: register/invariant/static-prerender-dynamic-compose
  severity: low
  summary: |
    The load-bearing invariant's enforcement_mechanism names
    `config/gptel/presets/test/` for the "two composes differ only in the tail"
    property. This task added that assertion in the CHAT spec
    (`config/gptel/chat/test/menu/environment-preamble-spec.el`) instead, because
    that is where the real per-send composer (`gptel-chat--refresh-system-prompt-
    from-file`) runs the env tail end-to-end with live drawer input — the
    composer-level golden specs under presets/test/ exercise the composer in
    isolation, not the chat pre-send seam. Both locations are legitimate; flagging
    so the Architect can reconcile the enforcement_mechanism location (the
    byte-identical-prefix spec now exists, just not under presets/test/).

- class: scope-question
  affected_register_entry: register/boundary/composer-compose
  severity: medium
  summary: |
    `environment.el` is not loaded by `gptel.org` and `presets/sources/` is not on
    `load-path`. The env tail is therefore "off" in production until a follow-up
    loads the env source in the gptel load order (after `presets/fragments.el`,
    which defines the seam var). This task's write-set excludes `gptel.org`, so the
    spec loads the source by path; production wiring is owned by the
    registration/composer integration task. Filing so the orchestrator routes a
    "load env source in gptel.org" step to that task (or a new one) before archive
    — otherwise the migrated env block silently stops appearing in real sessions.

- class: deviation
  affected_register_entry: register/invariant/static-prerender-dynamic-compose
  severity: low
  summary: |
    Pressure-tested the load-bearing invariant: the env fragment is dynamic,
    evaluated at compose time, placed at the tail (both via the composer's default
    composition and via the chat pre-send seam). No static fragment is re-rendered
    per send; no dynamic fragment is placed ahead of the static prefix. The new
    byte-identical-prefix spec demonstrates the cacheable-prefix property holds.
    No deviation from the invariant — recorded here as the required pressure-test
    note for the on-touch Architect review.
