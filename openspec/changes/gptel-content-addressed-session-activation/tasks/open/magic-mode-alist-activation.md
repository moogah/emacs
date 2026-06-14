---
name: magic-mode-alist-activation
description: Register a magic-mode-alist entry that selects gptel-chat-mode when the session signature predicate matches, replacing path-based activation.
change: gptel-content-addressed-session-activation
status: ready
relations:
  - "blocked-by:drawer-signature-and-head-read"
---

## Files to modify

- `config/gptel/chat/mode.org` (modify) — register `(jf/gptel--session-signature-p . gptel-chat-mode)` on `magic-mode-alist` at load time, near the `define-derived-mode` form.
- `config/gptel/chat/test/mode-activation-spec.el` (new) — Buttercup specs for content-addressed activation and non-activation.

## Implementation steps

1. Write the spec first. Cover:
   - opening (via `find-file-noselect`) a fixture `.org` whose drawer carries a `:GPTEL_` key → buffer major mode is `gptel-chat-mode`;
   - opening an ordinary `.org` file with no `:GPTEL_` drawer → stays `org-mode`;
   - an `.org` file that only quotes `:GPTEL_PRESET:` in prose → stays `org-mode` (precedence + false-match guard end-to-end).
2. Add the registration in `mode.org`:
   ```elisp
   (add-to-list 'magic-mode-alist
                (cons #'jf/gptel--session-signature-p #'gptel-chat-mode))
   ```
   Ensure `jf/gptel--session-signature-p` is loaded/available when this runs (require or load-order note — the predicate lives in the sessions filesystem module; confirm gptel load order or autoload the predicate).
3. Do NOT add any `auto-mode-alist` entry. Document in the surrounding prose that activation is content-addressed and that `set-auto-mode` consults `magic-mode-alist` before `auto-mode-alist`, so a drawer-bearing `.org` overrides the default `.org → org-mode` mapping; `gptel-chat-mode` derives from `org-mode` so org features survive.
4. Tangle `mode.org`; run the new spec.

## Design rationale

magic-mode-alist dispatches by content at mode-selection time (through `set-auto-mode`), not by path on every file open. The drawer signature is itself an explicit authored marker, so this recognizes an explicit declaration rather than guessing from layout. Precedence over `auto-mode-alist` is what lets a `.org` session open in chat-mode. (design.md §Decision D1; specs `chat-mode` Requirement "Mode definition and activation".)

## Verification

- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat` — green, including the false-match-on-open case.
- Done = a signature-bearing file opens in `gptel-chat-mode`; ordinary org files do not.

## Context

design.md § Decision "D1. Activation"; specs `chat-mode` Requirement "Mode definition and activation".

## Cycle 1 updates (cycle-1781448273)

- The predicate you register is `jf/gptel--session-signature-p`, implemented as
  `(and (jf/gptel--scan-session-drawer-keys) t)` (merged in `filesystem.org`). It is **case-sensitive**
  now (`case-fold-search` bound nil) — a lowercase `:properties:`/`:gptel_*:` drawer does NOT match.
- `register/boundary/session-content-signature` → **reconciled** (concrete shape + engine);
  `register/invariant/signature-anchored-to-point-min-drawer` → **confirmed** and strengthened
  (bounded-scan + case-sensitivity specs landed). Your end-to-end activation/false-match specs can
  rely on the predicate being load-bearing-correct; focus on the `magic-mode-alist` wiring +
  precedence (over `auto-mode-alist`) and load-order availability of the predicate symbol.
