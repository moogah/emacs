---
name: add-chat-mode-public-api-tests
description: Add Buttercup specs asserting the chat-mode public API contract for parser, send, and stream
change: persistent-agent-rebuild
status: ready
relations: []
---

## Files to modify

- `config/gptel/chat/test/parser/public-api-spec.el` (new)
- `config/gptel/chat/test/send/public-api-spec.el` (new)
- `config/gptel/chat/test/stream/public-api-spec.el` (new)

These are Buttercup `*-spec.el` files (not `.org`).

## Implementation steps

1. **Set up `parser/public-api-spec.el`**. Require dependencies and the shared helper:
   ```elisp
   ;;; public-api-spec.el --- Public-contract tests for the parser API -*- lexical-binding: t; -*-
   (require 'buttercup)
   (require 'gptel-chat-parser)
   (require 'gptel-chat-test-helpers)
   ```
   Then write `describe` blocks. Required `it` cases:

   - `(it "is callable from a non-chat-mode buffer")` — Set up a temp buffer with chat-mode block content via `gptel-chat-test--with-buffer`. Do NOT activate `gptel-chat-mode`. Call `(gptel-chat-parse-buffer)`. Expect a non-nil list of turn plists with the documented shape (`:role`, `:content`/`:segments`, `:start`, `:end`).
   - `(it "produces gptel-request prompt-shaped output")` — Same buffer setup, then `(gptel-chat-turns-to-messages (gptel-chat-parse-buffer))`. Expect a cons-list whose entries are `(prompt . STR)`, `(response . STR)`, or `(tool . PLIST)` in document order.
   - `(it "exposes no double-dash aliases after load")` — After loading `gptel-chat-parser`, `(fboundp 'gptel-chat--parse-buffer)` and `(fboundp 'gptel-chat--turns-to-messages)` are both `nil`. (This catches accidental `defalias` shims.)
     > Comment in the `it` block: `;; Scenario: specs/chat-mode/spec.md (delta) § "Public programmatic-send API" → "No double-dash internal aliases remain"`

2. **Set up `send/public-api-spec.el`**:
   ```elisp
   (require 'buttercup)
   (require 'gptel-chat-send)
   (require 'gptel-chat-parser)
   (require 'gptel-chat-test-helpers)
   ```
   Required `it` cases:

   - `(it "opens an assistant block and returns an advance marker")` — Build a temp chat-mode buffer with one user turn. Parse it. Take the user turn. Call `(gptel-chat-open-assistant-block user-turn)`. Expect: (a) the buffer now contains a `#+begin_assistant` line right after the user turn's `#+end_user`; (b) the returned value is a marker; (c) `(marker-insertion-type m)` is `t`; (d) the marker is positioned at the line immediately after `#+begin_assistant`.
     > Scenario: specs/chat-mode/spec.md (delta) § "Public open-assistant-block returns insertion marker"
   - `(it "fsm-handlers exposes the documented per-state chain")` — Inspect `gptel-chat-fsm-handlers` as a value. Expect `(boundp 'gptel-chat-fsm-handlers)` and that the alist contains entries keyed `WAIT`, `TYPE`, `TOOL`, `DONE`, `ERRS`, `ABRT`. For each non-`TYPE` entry, the chain ends with one of the gptel upstream handlers (`gptel--handle-wait`, `gptel--handle-tool-use`, `gptel--handle-post`).
     > Scenario: specs/chat-mode/spec.md (delta) § "Public fsm-handlers usable with gptel-make-fsm"
   - `(it "exposes no double-dash aliases after load")` — Same shape as parser test, for `gptel-chat--open-assistant-block` and `gptel-chat--fsm-handlers`.

3. **Set up `stream/public-api-spec.el`**:
   ```elisp
   (require 'buttercup)
   (require 'gptel-chat-stream)
   (require 'gptel-chat-send)
   (require 'gptel-chat-parser)
   (require 'gptel-chat-test-helpers)
   ```
   Required `it` cases:

   - `(it "stream callback inserts at the supplied marker")` — Set up a chat-mode buffer with a user turn, open the assistant block via `gptel-chat-open-assistant-block`, get the insertion marker. Build the callback via `(gptel-chat-stream-callback insertion)`. Call it with a stringy chunk: `(funcall cb "hello" '(:position ... :buffer ... :tracking-marker ...))` (use the same INFO plist shape that gptel-request would pass). Assert the buffer now contains `"hello"` between the `#+begin_assistant` line and the next line. Calling `cb` with `(symbol t)` (stream-end signal) should append `#+end_assistant` after the inserted text.
     > Scenario: specs/chat-mode/spec.md (delta) § "Public stream-callback usable with gptel-request"
   - `(it "exposes no double-dash alias after load")` — `(fboundp 'gptel-chat--stream-callback)` is `nil`.

4. **Buffer fixture pattern**. Each test that needs buffer content uses `gptel-chat-test--with-buffer` from `test-helpers.el`. Example:
   ```elisp
   (it "is callable from a non-chat-mode buffer"
     (gptel-chat-test--with-buffer
         "#+begin_user\nHello\n#+end_user\n#+begin_assistant\nHi\n#+end_assistant\n"
       (let ((turns (gptel-chat-parse-buffer)))
         (expect (length turns) :to-equal 2)
         (expect (plist-get (car turns) :role) :to-equal 'user))))
   ```

5. **Tangle nothing**. These are direct `.el` test files, not org-tangled. Validate parens by loading the file in batch mode:
   ```
   emacs --batch -l <file>.el
   ```

## Design rationale

These tests assert *only* on the public contract documented in the spec delta — input shapes, output shapes, observable side effects. They do NOT assert on internal helpers (`gptel-chat--containing-turn` etc. are private; tests don't go near them). The point is to lock down the cross-module API surface the persistent-agent (and any future caller) will depend on.

The "exposes no double-dash aliases" test is a guard: if a future maintainer accidentally adds a `defalias` shim "for backwards compatibility," this test fails. The change explicitly rejects such shims.

Putting these tests in their own `public-api-spec.el` files (alongside the existing per-feature specs like `buffer-format-spec.el`) keeps the public-API contract assertions discoverable. A future reader looking for "what does the public API guarantee?" finds it in one place per module, not scattered.

## Design pattern

Existing chat-mode tests (`config/gptel/chat/test/parser/buffer-format-spec.el` etc.) demonstrate the Buttercup style used in this codebase: top-level `describe`, nested `describe` for sub-areas, `it` for individual scenarios, `expect` with matchers. Match that style.

Existing pattern for buffer-content fixtures: `gptel-chat-test--with-buffer "..."` (defined in `test-helpers.el`). Use it.

## Verification

- `./bin/run-tests.sh -d config/gptel/chat/test/parser` — all parser tests pass, including the new `public-api-spec.el`.
- `./bin/run-tests.sh -d config/gptel/chat/test/send` — all send tests pass.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` — all stream tests pass.
- The new spec files load without errors: `emacs --batch -l <file>.el` exits 0.
- Each `it` block has a leading comment naming the spec scenario it maps to.

**Done means**: three new spec files, every `it` mapped to a spec scenario via comment, all chat-mode tests green.

## Context

architecture.md § "Testing Approach" → "Test Organization" and "Scenario Mapping"
specs/chat-mode/spec.md (delta) § "Public programmatic-send API" — every scenario in this requirement is covered.
design.md § "Layer 1: Chat-mode public-API rename" → "Public-contract test"
