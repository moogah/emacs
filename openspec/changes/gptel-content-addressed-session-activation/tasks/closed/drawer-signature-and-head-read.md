---
name: drawer-signature-and-head-read
description: Add the gptel session-signature predicate (point-min drawer with a :GPTEL_ key) and a file head-read helper, the shared foundation for content-addressed activation and drawer-based discovery.
change: gptel-content-addressed-session-activation
status: done
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

## Observations

- Factored the shared drawer scan into a private engine
  `jf/gptel--scan-session-drawer-keys`; both public functions delegate
  to it so the recognition logic (anchoring, blank-line skip, :END:
  bounding, GPTEL_ key collection) lives in exactly one place. This is
  a structural addition beyond the two functions named in the task, but
  it keeps the predicate and the head-read provably identical — the
  invariant requires they recognize the same thing.
- The head-read returns the FULL set of `:GPTEL_*:` keys found, not just
  the four identity keys. The vocabulary entry calls the set OPEN and
  the head-read is documented as returning "whichever identity keys are
  found", so returning everything (e.g. a legacy `:GPTEL_MODEL:`) is
  consistent and strictly more useful to downstream resolvers. Tests
  assert presence of the four named keys, not absence of extras.
- The value capture regexp tolerates an empty value
  (`:GPTEL_FOO:` with nothing after the colon) — such a key still
  qualifies for the signature (key presence is the contract) and is
  returned with an empty-string value. No downstream consumer in this
  task, so no stronger assertion was warranted.
- Pre-existing `directory-templates-spec.el` (10 specs) continues to
  pass alongside the new 16 — no regression in the filesystem module.

## Discoveries

- discovery_id: disc-drawer-signature-and-head-read-1
  class: interface-drift
  description: |
    The speculated boundary entry describes the head-read as returning
    "an alist of the :GPTEL_* keys found" without fixing the alist key
    type. Implementation keys the alist by the BARE key STRING
    (e.g. "GPTEL_SESSION_ID"), as the task body recommended, and the
    value is the raw drawer string (trimmed). Recommend the register
    entry record this concrete shape so the later resolver task consumes
    it without guessing (string keys via `assoc`, string values).
  affected_register_entry: register/boundary/session-content-signature
  recommendation: |
    Promote the return shape from speculated to confirmed and state it
    explicitly: `(("GPTEL_SESSION_ID" . "<id>") ("GPTEL_BRANCH" . "main")
    ...)`, an alist keyed by the bare key string with string values,
    nil when no point-min drawer is present or the file is unreadable.
- discovery_id: disc-drawer-signature-and-head-read-2
  class: deviation
  description: |
    A private shared engine `jf/gptel--scan-session-drawer-keys` was
    introduced (not named in the speculation) so the buffer predicate
    and the on-disk head-read run byte-for-byte the same recognition
    logic. The speculated entries describe two functions; the engine is
    an internal third symbol they both delegate to. It is the mechanism
    that GUARANTEES the invariant (predicate == head-read recognition),
    so the register may want to name it as the canonical scanner.
  affected_register_entry: register/invariant/signature-anchored-to-point-min-drawer
  recommendation: |
    Note that the anchoring/scan logic is centralized in
    `jf/gptel--scan-session-drawer-keys`; the predicate is
    `(and (jf/gptel--scan-session-drawer-keys) t)` and the head-read is
    the same scan over a temp buffer. Future changes to the anchoring
    rule should touch only the engine.
- discovery_id: disc-drawer-signature-and-head-read-3
  class: scope-question
  description: |
    The byte-cap for the head read is structured but NOT yet applied:
    `insert-file-contents` currently reads the whole file, with a
    comment marking exactly where BEG/END args go. This matches the task
    instruction ("STRUCTURE it so a byte cap can be added later"). The
    session drawer is always at the file head, so reading the whole file
    is correct today and only a performance concern for pathologically
    large session.org files at discovery time.
  affected_register_entry: register/boundary/session-content-signature
  recommendation: |
    Leave uncapped for this task; a downstream discovery/perf task can
    set the `insert-file-contents` END bound once a drawer-size budget
    is chosen. No correctness risk in the interim.
