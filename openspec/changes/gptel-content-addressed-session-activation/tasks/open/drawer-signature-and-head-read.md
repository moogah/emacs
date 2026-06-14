---
name: drawer-signature-and-head-read
description: Add the gptel session-signature predicate (point-min drawer with a :GPTEL_ key) and a file head-read helper, the shared foundation for content-addressed activation and drawer-based discovery.
change: gptel-content-addressed-session-activation
status: ready
cites_register_entries:
  - register/boundary/session-content-signature
  - register/invariant/signature-anchored-to-point-min-drawer
  - register/vocabulary/identity-drawer-keys
relations: []
---

## Files to modify

- `config/gptel/sessions/filesystem.org` (modify) — add `jf/gptel--session-signature-p` (operates on the current buffer) and `jf/gptel--read-session-drawer-head` (reads the head of a file on disk into a temp buffer and returns parsed drawer properties or a signature verdict).
- `config/gptel/sessions/test/filesystem/session-signature-spec.el` (new) — Buttercup specs for the predicate (positive, false-match, non-org, old-session) and the head-read helper.

## Implementation steps

1. Write the Buttercup spec first (`session-signature-spec.el`). Cover:
   - a buffer whose first non-blank content is a `:PROPERTIES:` drawer containing `:GPTEL_PRESET:` → predicate returns non-nil;
   - a buffer with `:GPTEL_SESSION_ID:` only → non-nil (any `:GPTEL_`-prefixed key qualifies);
   - an ordinary org buffer that merely mentions `:GPTEL_PRESET:` inside a paragraph or `#+begin_src` block (not a point-min drawer) → nil;
   - a non-org/plain-text buffer → nil, no error;
   - the head-read helper returns the drawer's `GPTEL_SESSION_ID` / `GPTEL_BRANCH` / `GPTEL_PARENT_SESSION_ID` from a fixture file.
2. Implement `jf/gptel--session-signature-p`. Reuse the native, org-independent parsing shape from `gptel-chat--declared-preset` (`config/gptel/chat/menu.org:152-173`):
   - `save-excursion` + `save-restriction` + `widen`; `goto-char (point-min)`; skip leading blank lines;
   - require `(looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$")`;
   - find the drawer's `:END:` (`re-search-forward "^[ \t]*:END:[ \t]*$"`), bound the search to it;
   - return non-nil iff a line matching `"^[ \t]*:GPTEL_[A-Z0-9_]+:"` exists before `:END:`.
   - The function must scan only the buffer head and never signal on a non-org buffer.
3. Implement `jf/gptel--read-session-drawer-head`: `with-temp-buffer` + `insert-file-contents` (bounded read of the file head — see Open Questions in design.md; start by reading the whole file but structure so a byte cap can be added), then run the same drawer scan and return an alist/plist of the `:GPTEL_*:` keys found (at least `GPTEL_SESSION_ID`, `GPTEL_BRANCH`, `GPTEL_PARENT_SESSION_ID`, `GPTEL_PRESET`).
4. Tangle: `./bin/tangle-org.sh config/gptel/sessions/filesystem.org`.
5. Run the new spec.

## Design rationale

This is the shared recognition primitive for both D1 (activation) and D7 (discovery). Keying on "a real `point-min` drawer carrying any `:GPTEL_`-prefixed property" recognizes old sessions (which have `:GPTEL_PRESET:`) and new ones (which add `:GPTEL_SESSION_ID:`) alike, and is unambiguous because nothing but gptel authors `:GPTEL_*:` properties. Anchoring to a real drawer (not a bare substring) is the false-match guard. Reusing the proven native parser avoids any dependency on org-mode being loaded at `magic-mode-alist` time. (design.md §Decisions D1, D7; risk "Signature false-match".)

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/filesystem.org` succeeds (paren check).
- `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem` — all green, including the false-match scenario.
- Done = predicate + head-read helper exist, are exported, and pass the four signature scenarios.

## Context

design.md § Decisions "D1. Activation" and "D7. Discovery"; specs `chat-mode` Requirement "Mode definition and activation" (signature scenarios).
