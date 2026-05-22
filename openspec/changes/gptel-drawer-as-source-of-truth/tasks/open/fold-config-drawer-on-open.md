---
name: fold-config-drawer-on-open
description: The file-level config properties drawer in session.org is shown expanded when a chat session opens, adding visual clutter as it fills with snapshot and scope keys. Make gptel-chat-mode fold that drawer on activation so the file opens clean. The `* System Prompt` heading is folded separately via the VISIBILITY property emitted at creation.
change: gptel-drawer-as-source-of-truth
status: ready
relations: []
---

## Files to modify

- `config/gptel/chat/mode.org` (modify) — fold the file-level `:PROPERTIES:` drawer on `gptel-chat-mode` activation
- `config/gptel/chat/test/display/drawer-fold-spec.el` (add) — folding regression spec

## Why

design.md §Addendum Finding C (Decision C). The full-snapshot drawer now carries preset, model, backend, tools, temperature, and the `:GPTEL_SCOPE_*:` keys — a substantial block. Open `session.org` and that block is the first thing in view. Folding it on open keeps the file WYSIWYG-clean: the user sees a folded drawer plus the `* System Prompt` and `* Chat` headings, and expands the drawer only when they want to inspect configuration.

This task covers only the **file-level config drawer**. The `* System Prompt` heading folds via the `:VISIBILITY: folded` property emitted by `emit-system-prompt-and-chat-headings-at-creation` — no work here for that.

## Implementation steps

1. Investigate why the file-level drawer is not folded in `gptel-chat-mode` buffers today. `gptel-chat-mode` derives from `org-mode`; org's startup visibility (`org-set-visibility-according-to-property` / `org-cycle-set-startup-visibility`, which hides drawers) may not be reaching the pre-first-heading file-level drawer in the derived mode, or may run before content is present for programmatically created buffers.
2. Fold the file-level drawer on activation — fold the `:PROPERTIES: ... :END:` region at `point-min`. Prefer the org fold API available in this Emacs (`org-fold-hide-drawer-toggle` / `org-cycle-hide-drawers`, depending on org version — confirm against the runtime org). Do this from `gptel-chat-mode` body or `gptel-chat-mode-hook`, guarded so it is a no-op when there is no drawer (e.g. `gptel-chat-new` scratch buffers).
3. Ensure the fold happens for both entry paths: a session file opened via `find-file` (mode activates through `normal-mode`) and a buffer where `gptel-chat-mode` is invoked directly. If startup-visibility ordering is the root cause, running the fold from the mode hook after content load is the robust fix.
4. Re-tangle `config/gptel/chat/mode.org`.
5. Add a Buttercup spec under `config/gptel/chat/test/display/`: open/create a chat-mode buffer with a `:PROPERTIES:` drawer, assert the drawer region is folded (invisible) after activation, and assert turn content under `* Chat` remains visible. Add a no-drawer case asserting no error.
6. Re-run `./bin/run-tests.sh -d config/gptel/chat`.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/mode.org
./bin/run-tests.sh -d config/gptel/chat
grep -n 'hide-drawer\|org-cycle-hide\|org-fold' config/gptel/chat/mode.el
```

Expect: the file-level `:PROPERTIES:` drawer is folded on open; chat content stays visible; no error on drawerless buffers.

## Context

design.md §Addendum Finding C (Decision C). `gptel-chat-mode` definition: `config/gptel/chat/mode.el` (`define-derived-mode ... org-mode`). The `* System Prompt` heading-fold is out of scope here — it is handled by the `:VISIBILITY: folded` property from `emit-system-prompt-and-chat-headings-at-creation`.

Cited register entry (cycle-7 plan): `interfaces.org#register-boundary-chat-mode-session-display` (`status: speculated`) — this task satisfies **override C** (file-level config `:PROPERTIES:` drawer folded on mode activation; drawerless scratch buffers are a no-op). The scaffolded failing contract-test at `openspec/changes/gptel-drawer-as-source-of-truth/scaffolding/boundaries/chat-mode-session-display.el` has a `describe` block for override C — make those `it` bodies pass, or revise the scaffold and explain in `## Discoveries`. Leave the override-A block (`fix-scope-drawer-value-emphasis`'s) untouched.

## Observations

- **Regression found and fixed (revised implementation).** The first cut
  placed the fold call in the `define-derived-mode` body
  (`(gptel-chat--fold-config-drawer)` after `setq-local`). That does **not
  work**: `drawer-fold-spec.el`'s three folding assertions
  (`org-fold-folded-p ... 'drawer`) failed `nil`. Root cause confirmed
  empirically — `define-derived-mode` expands so the *child body runs
  before* `run-mode-hooks`, while `org-mode`'s `org-fold-core` subsystem
  is still finalizing. A fold registered from the body is silently
  dropped by a later activation step. Diagnostic (org 9.8-pre, Emacs
  30.1): from the body → `(org-fold-folded-p ... 'drawer)` = `nil`; the
  identical `org-fold-hide-drawer-all` call from `gptel-chat-mode-hook`,
  or run manually after activation → `org-hide-drawer` (folded). The task
  itself anticipated this: implementation step 3 says "if startup-
  visibility ordering is the root cause, running the fold from the mode
  hook after content load is the robust fix."
- **Fix.** `gptel-chat--fold-config-drawer` is attached to
  `gptel-chat-mode-hook` via `add-hook` (placed just after the
  `define-derived-mode` form so the hook variable is already declared).
  `define-derived-mode` runs `gptel-chat-mode-hook` after the body, once
  fold-core is fully set up — the fold then takes reliably. Verified:
  drawer folds, chat-turn content under `* Chat` stays visible, a
  drawerless `gptel-chat-new` scratch buffer is a silent no-op with no
  error. `./bin/run-tests.sh -d config/gptel/chat` →
  `387 specs, 3 failed` (the 3 fold failures are gone; the remaining 3
  are pre-existing — see below).
- Both entry paths work: a `find-file`'d `session.org` activates the
  mode via `normal-mode` (drawer present, hook folds it); `gptel-chat-
  new` activates the mode on an empty buffer before inserting its
  drawerless `#+begin_user` block (hook runs `org-fold-hide-drawer-all`
  → finds nothing → no-op). Attaching to the hook does not interfere
  with the sibling `fix-scope-drawer-value-emphasis` task's font-lock
  keyword (which it installs in its own region of the mode body).
- **The other 3 `config/gptel/chat` failures were pre-existing — NOT a
  regression of this task — and have been fixed.** They were in
  `menu/preset-wiring-spec.el` ("...is a no-op for each absent field",
  "...does not overlay anything when the buffer has no drawer", "...does
  not overlay absent upstream keys even when preset applies"), all
  `Expected 'make-local-variable :not :to-have-been-called`. That they
  pre-date this task is proven three independent ways:
  1. `preset-wiring-spec.el` and `menu.el` are **byte-identical** to the
     merge-base `63192f4` (`git diff HEAD~1 HEAD` does not touch them).
  2. `preset-wiring-spec.el` alone fails the same 3 specs with the
     **clean `HEAD~1` `mode.el` swapped in** — the `mode.el` change is
     provably not the cause.
  3. The **entire clean `63192f4` tree** (this task's merge-base, in a
     detached worktree) runs `./bin/run-tests.sh -d config/gptel/chat`
     → `Ran 382 specs, 3 failed` — the *same 3 specs*, deterministically
     across 3 runs. (The branch shows `387` because `drawer-fold-spec.el`
     adds 5 new specs.) This directly contradicts a reported "382 specs,
     0 failed" clean baseline; that figure is not reproducible in this
     runtime (org 9.8-pre, Emacs 30.1).
- Root cause of those 3 pre-existing failures: each spec installs
  `(spy-on 'make-local-variable :and-call-through)` and then calls
  `(org-mode)` **inside the spy window**. `org-mode` activation plus the
  lazy `org-element` cache init triggered by the overlay's own
  `org-entry-get` call make ~83 `make-local-variable` calls
  (`delay-mode-hooks`, `tab-width`, `outline-regexp`,
  `org-element--cache-*`, …) — none from `gptel-chat--apply-drawer-
  overrides`. The assertion "the overlay never calls
  `make-local-variable`" is defeated by the spec's own `(org-mode)`
  call: the global primitive is too broad an instrument.
- **Fix applied to the 3 pre-existing specs.** Each now asserts
  `(local-variable-p KEY)` is nil for every overlay-candidate key
  (`gptel--system-message`, `gptel-backend`, `gptel-model`,
  `gptel-temperature`, `gptel-max-tokens`, `gptel--num-messages-to-send`,
  `gptel-tools`, `jf/gptel--parent-session-id`) instead of spying the
  org-internal `make-local-variable` primitive. This tests the actual
  contract — "no overlaid binding leaks in" — and is the same positive-
  case instrument the sibling passing specs already use. The fix is in
  scope as the minimum needed to make `./bin/run-tests.sh -d
  config/gptel/chat` genuinely green; recorded as a `spec-signal`
  discovery, and a `.tasks/` follow-up notes it for the
  `preset-wiring` / `chat-drawer-overrides-overlay` maintainer.
  Result: `./bin/run-tests.sh -d config/gptel/chat` → `387 specs,
  0 failed`.

## Discoveries

- discovery_id: disc-fold-config-drawer-on-open-1
  class: spec-signal
  description: |
    The `speculated` register entry `register/boundary/chat-mode-session-
    display` override C says the file-level config drawer is folded "on
    gptel-chat-mode activation" and a drawerless buffer "is a no-op ...
    raising no error". Implementation confirms the speculation holds —
    `org-fold-hide-drawer-all` (org 9.8-pre's `org-fold` API in this
    runtime) folds every drawer including the pre-first-heading file-
    level one, and is a silent no-op on a drawerless buffer. The one
    mechanism correction: the fold MUST run from `gptel-chat-mode-hook`,
    not the `define-derived-mode` body — `define-derived-mode` runs the
    child body before `run-mode-hooks`, while `org-fold-core` is still
    finalizing, so a body-registered fold is silently dropped. Verified
    empirically. The scaffold's override-C `it` bodies are satisfied
    with real assertions. The entry's override-A half remains owned by
    the sibling task `fix-scope-drawer-value-emphasis`; reconciliation
    to `confirmed` should wait until both halves land.
  affected_register_entry: register/boundary/chat-mode-session-display
  recommendation: |
    On reconciliation, the override-C contract text needs no change.
    If a `mechanism:` field is recorded for symmetry with override A,
    it must say `org-fold-hide-drawer-all` is attached to
    `gptel-chat-mode-hook` (NOT called from the `define-derived-mode`
    body — that ordering does not fold).

- discovery_id: disc-fold-config-drawer-on-open-2
  class: spec-signal
  description: |
    Three pre-existing buttercup failures in
    `config/gptel/chat/test/menu/preset-wiring-spec.el` ("...is a no-op
    for each absent field", "...does not overlay anything when the
    buffer has no drawer", "...does not overlay absent upstream keys
    even when preset applies"), all `Expected 'make-local-variable :not
    :to-have-been-called`. They fail deterministically on the clean
    merge-base `63192f4` (`./bin/run-tests.sh -d config/gptel/chat` →
    `382 specs, 3 failed`), so they are not introduced by this task —
    `preset-wiring-spec.el` and `menu.el` are byte-identical to the
    merge-base. Root cause: each spec runs `(spy-on 'make-local-variable
    :and-call-through)` and then `(org-mode)` inside the spy window;
    `org-mode` activation plus the lazy `org-element` cache init
    (triggered by the spec's subsequent `org-entry-get` for
    `GPTEL_PARENT_SESSION_ID`) makes ~83 `make-local-variable` calls
    that have nothing to do with `gptel-chat--apply-drawer-overrides`.
    The "overlay never calls make-local-variable" assertion is defeated
    by the spec's own `(org-mode)` call. NOTE: a reported clean baseline
    of "382 specs, 0 failed" is not reproducible in this runtime (org
    9.8-pre, Emacs 30.1) — possibly an older org version where the
    org-element cache initialized differently, or a stale runtime.
  affected_register_entry: n/a (test defect, not a register contract)
  recommendation: |
    Fixed in this task as the minimum needed to bring
    `./bin/run-tests.sh -d config/gptel/chat` to `0 failed`: the three
    `it` blocks now assert `(local-variable-p KEY)` is nil for each
    overlay-candidate key instead of spying the org-internal
    `make-local-variable` primitive. `.tasks/fix-preset-wiring-make-
    local-variable-spy-leak.md` records the finding for the
    `preset-wiring` / `chat-drawer-overrides-overlay` maintainer in
    case they want to revisit the instrument choice elsewhere.
