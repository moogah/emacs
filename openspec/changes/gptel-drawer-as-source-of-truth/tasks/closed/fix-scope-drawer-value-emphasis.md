---
name: fix-scope-drawer-value-emphasis
description: GPTEL_SCOPE_* path values in the session.org drawer render italicized — a value such as `/Users/jeff/emacs/` is a valid org `/emphasis/` span, so the boundary slashes are dimmed/hidden. Install a buffer-local font-lock keyword in gptel-chat-mode that re-stamps `org-property-value` (OVERRIDE) over property-drawer value spans so drawer values render as plain data with visible slashes.
change: gptel-drawer-as-source-of-truth
status: done
relations: []
---

## Files to modify

- `config/gptel/chat/mode.org` (modify) — add the font-lock matcher + keyword to `gptel-chat-mode`
- `config/gptel/chat/test/display/drawer-fontification-spec.el` (add) — fontification regression spec (Buttercup)

## Why

design.md §Addendum Finding A. A drawer line like `:GPTEL_SCOPE_READ: /Users/jeff/emacs/` is a syntactically valid org `/emphasis/` span: the leading `/` opens it and the trailing `/` closes it. Org applies emphasis font-lock buffer-wide — there is no context guard excluding `:PROPERTIES:` drawers — so the path renders italic and, with `org-hide-emphasis-markers` on, the boundary `/` characters are hidden. The user's report: italic is tolerable, the missing `/` is not. Both go away if drawer values are never emphasized.

Confirmed by a fontification probe (`font-lock-ensure` on a temp org buffer): a value of `/Users/jefffarr/emacs/` gets face `(italic org-property-value)`; a value of `/Users/jefffarr/emacs` (no trailing slash) gets plain `org-property-value`. Trailing-slash directory values are the trigger, but the fix is general — drawer values are data, not prose, and should never carry emphasis.

## Implementation steps

1. Add a matcher function to `mode.org`, e.g. `gptel-chat--drawer-value-matcher`, that advances point to the next property-drawer **value** span and sets match group 1 to it. It must (a) match `^[ \t]*:[A-Za-z][A-Za-z0-9_-]*:\(?: \(.*\)\)?$`, and (b) confirm the line sits inside a `:PROPERTIES: ... :END:` drawer by scanning backward for the nearest `:PROPERTIES:`/`:END:` and requiring `PROPERTIES`. (Verified-working shape — see Context.)
2. Register it buffer-locally in `gptel-chat-mode` via `font-lock-add-keywords` with `'append` and an **OVERRIDE** facespec: `(gptel-chat--drawer-value-matcher (1 'org-property-value t))`. Append ordering + the `t` override flag ensure our face wins over org's emphasis keyword (which runs in `org-font-lock-extra-keywords`).
3. Re-tangle: `./bin/tangle-org.sh config/gptel/chat/mode.org`.
4. Add a Buttercup spec under `config/gptel/chat/test/display/`: enable `gptel-chat-mode` on a buffer with a `:PROPERTIES:` drawer containing `:GPTEL_SCOPE_READ: /Users/jeff/emacs/`, call `font-lock-ensure`, and assert the `/` characters in the drawer value carry `org-property-value` (no `italic`). Add a counter-assertion that a `/italic/` span inside a `#+begin_user` block **keeps** its `italic` face — the fix must not flatten emphasis in chat prose.
5. Re-run `./bin/run-tests.sh -d config/gptel/chat` for regressions.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/mode.org
./bin/run-tests.sh -d config/gptel/chat
grep -n 'drawer-value-matcher\|font-lock-add-keywords\|org-property-value' config/gptel/chat/mode.el
```

Expect: the matcher + keyword are present in `mode.el`; the new spec passes; chat-prose emphasis is unaffected.

## Context

design.md §Addendum Finding A (Decision A).

Verified matcher/keyword shape (probe run during scoping): the function matcher below, registered with `'append` + override flag `t`, re-stamps drawer values to `org-property-value` while leaving `/italic/` in `#+begin_user` content as `(italic)`:

```elisp
(defun gptel-chat--drawer-value-matcher (limit)
  (let (found)
    (while (and (not found)
                (re-search-forward
                 "^[ \t]*:[A-Za-z][A-Za-z0-9_-]*:\\(?: \\(.*\\)\\)?$"
                 limit t))
      (when (and (match-beginning 1)
                 (save-excursion
                   (save-match-data
                     (forward-line 0)
                     (and (re-search-backward
                           "^[ \t]*:\\(PROPERTIES\\|END\\):[ \t]*$" nil t)
                          (string-equal (match-string 1) "PROPERTIES")))))
        (setq found t)))
    found))
```

Rejected alternatives (do not implement): `setq-local org-fontify-emphasized-text nil` (kills emphasis in chat content too); stripping trailing slashes in the renderer (only fixes our-rendered values, not hand-edited ones, and risks scope-matching semantics).

Cited register entry (cycle-7 plan): `interfaces.org#register-boundary-chat-mode-session-display` (`status: speculated`) — this task satisfies **override A** (drawer values render as plain `org-property-value`, never inherit org `/emphasis/`; chat-turn prose emphasis preserved). The scaffolded failing contract-test at `openspec/changes/gptel-drawer-as-source-of-truth/scaffolding/boundaries/chat-mode-session-display.el` has a `describe` block for override A — make those `it` bodies pass, or revise the scaffold and explain in `## Discoveries`. Leave the override-C block (`fold-config-drawer-on-open`'s) untouched.

## Observations

- The verified matcher/keyword shape in the task body's Context section is correct: a `-Q` probe confirmed drawer values get `org-property-value` (no italic) and `/italic/` prose stays `(italic)`. Implemented the function and keyword verbatim.
- The scaffold's original "re-stamps ... via a buffer-local font-lock keyword carrying the OVERRIDE flag" `it` body, and my first draft of the equivalent spec, asserted the override by *contrast*: "plain `org-mode` fontifies the trailing-slash value italic; chat-mode does not." That contrast assertion is environment-fragile. Under `emacs -Q`, plain `org-mode` does fontify the value italic; under the test harness's full init (which loads this repo's org configuration) it does **not** — `org-fontify-emphasized-text` / emphasis-regexp settings differ, so `value-italic-in-org` came back nil and the assertion failed. Rewrote both the new spec and the scaffold's override-A "re-stamps" `it` to assert deterministically against the installed keyword structure instead: the matcher is `gptel-chat--drawer-value-matcher`, the facespec is `(1 'org-property-value t)` with the OVERRIDE flag set, and `font-lock-keywords` is buffer-local. The face-level behavior (drawer value not italic; chat prose still italic) is still asserted in the other two `it` bodies, which exercise chat-mode directly and are config-independent.
- The scaffolding file lives under `openspec/changes/.../scaffolding/` and is **not** discovered by `./bin/run-tests.sh` (the harness only walks `config/`). Per the task's instruction to "make those `it` bodies pass", I made the scaffold's override-A block genuinely passing real assertions (it needs the chat module on `load-path`, added via `locate-dominating-file`). The override-C `describe` block is byte-identical to `HEAD` (verified via `git show HEAD:... | awk` diff) — its two `it` stubs still raise `error`, owned by sibling task `fold-config-drawer-on-open`. The new co-located regression spec `config/gptel/chat/test/display/drawer-fontification-spec.el` is the harness-discovered contract test.
- Pre-existing failure load in `config/gptel/chat`: 3 buttercup specs under `preset wiring drawer overrides overlay` fail with `Expected 'make-local-variable :not :to-have-been-called`. Confirmed pre-existing by stashing this task's changes and re-running `config/gptel/chat/test/menu` on a clean tree — the same 3 failed. Not introduced by this task; not in scope.

## Discoveries

- discovery_id: disc-fix-scope-drawer-value-emphasis-1
  class: deviation
  description: |
    The register entry register/boundary/chat-mode-session-display
    (override A, status: speculated) is satisfied as speculated — no
    push-back on the contract. One implementation-level note for
    reconciliation: the entry's `mechanism` says the keyword "re-stamps
    org-property-value over property-drawer value spans with the
    OVERRIDE flag." This is exactly what shipped; `font-lock-add-keywords`
    is called with a nil first argument (buffer-local install) inside
    the `define-derived-mode` body, `'append` ordering, facespec
    `(1 'org-property-value t)`. The face-contrast assertion style the
    scaffold originally used (plain org-mode vs chat-mode) is not a
    reliable contract test because plain-org emphasis fontification of
    trailing-slash drawer values is config-dependent; the contract is
    better verified by (a) the chat-mode face outcome and (b) the
    installed keyword structure. The reconciled entry could note that
    the override is observable as a buffer-local `font-lock-keywords`
    entry, not only as a face delta against vanilla org-mode.
  affected_register_entry: register/boundary/chat-mode-session-display
  recommendation: |
    Flip override-A's portion of the entry to `confirmed` on
    reconciliation — implementation matches the speculated mechanism
    exactly. No shape or vocabulary change needed. Optionally add to
    the entry's contract that the override manifests as a buffer-local
    `font-lock-keywords` entry whose matcher is
    `gptel-chat--drawer-value-matcher` and whose facespec carries the
    OVERRIDE flag, since that is the deterministic observable.
