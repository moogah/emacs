---
name: add-agent-send-completion-tests
description: Buttercup specs for FSM composition, gptel-request invocation shape, and final-text extraction
change: persistent-agent-rebuild
status: needs-review
relations:
  - blocked-by:add-persistent-agent-test-fixtures
  - blocked-by:rebuild-persistent-agent-module
---

## Files to modify

- `config/gptel/tools/test/persistent-agent/send-and-completion-spec.el` (new)

## Implementation steps

1. **File header**:
   ```elisp
   ;;; send-and-completion-spec.el --- Persistent-agent send + completion tests -*- lexical-binding: t; -*-

   (require 'buttercup)
   (require 'jf-persistent-agent-test-helpers)
   (require 'gptel-persistent-agent)
   (require 'gptel-chat-send) ; for gptel-chat-fsm-handlers
   ```

2. **`describe "gptel-request invocation shape"`** containing:

   - `it "issues a gptel-request with messages, stream-callback, and composed FSM"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Execution lifecycle" → "gptel-request invocation shape"
     - Setup: `with-mock-parent-session` + `with-mock-preset 'test-preset` + `with-mock-gptel-request 'captured`.
     - Call the task with `prompt "do the thing"`.
     - Inspect `captured` (the captured gptel-request invocation).
     - Expect the first positional arg (`prompt` to `gptel-request`) is a list (not nil, not a string) — the messages list.
     - Expect that list contains `(prompt . "do the thing")` as one entry (round-trip from drawer + chat-mode parser).
     - Expect `:stream` keyword is `t`.
     - Expect `:callback` is a function (the chat-mode stream callback closure).
     - Expect `:context` is an overlay (the parent overlay).
     - Expect `:fsm` is a `gptel-fsm` struct (verify with `gptel-fsm-p`).

3. **`describe "FSM handler composition"`** containing:

   - `it "the FSM handler alist composes WAIT/TOOL with overlay handlers ahead of chat lifecycle and upstream"`
     > Scenarios: specs/persistent-agent/spec.md (delta) § "Execution lifecycle" → "WAIT state updates the parent overlay" + "TOOL state updates the parent overlay with cumulative count"
     - Same setup. Capture the FSM via `with-mock-gptel-request`.
     - Call `(gptel-fsm-handlers fsm)` to get the handler alist (or look up via the struct accessor — verify the right way with `grep gptel-fsm`).
     - For the `WAIT` entry: expect the handler chain (after the state symbol) is `(jf/gptel-persistent-agent--indicate-wait gptel-chat--on-wait gptel--handle-wait)` in that order. (`gptel-chat--on-wait` is *not* renamed in this change — only the five public-API symbols are. The chat-mode lifecycle handlers stay private. Verify by grep.)
     - For the `TOOL` entry: chain is `(jf/gptel-persistent-agent--indicate-tool-call gptel-chat--on-tool gptel--handle-tool-use)`.
     - For the `DONE` entry: chain is `(<closure> gptel-chat--on-done gptel--handle-post)` — the `<closure>` is the agent's done-handler. Assert it's a `byte-code-function-p` or `functionp` (not a symbol).
     - For the `ERRS` entry: chain is `(<closure> gptel-chat--on-errs gptel--handle-post)`.
     - For the `ABRT` entry: chain is `(<closure> gptel-chat--on-abrt gptel--handle-post)`.

4. **`describe "Final-text extraction"`** containing:

   - `it "DONE returns the last assistant text segment"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Parent-child communication" → "DONE returns the final assistant text segment"
     - Construct an agent buffer manually (no need to go through `--task`). Insert chat-mode content with two assistant turns, the second containing a tool block followed by trailing text:
       ```
       #+begin_user
       Q
       #+end_user

       #+begin_assistant
       intermediate
       #+end_assistant

       #+begin_user
       Q2
       #+end_user

       #+begin_assistant
       Sure.

       #+begin_tool (some_tool :arg "x")
       (:name "some_tool" :args (:arg "x"))

       result
       #+end_tool

       Final answer.
       #+end_assistant
       ```
     - Activate `gptel-chat-mode` in the buffer (so the parser dispatch is identical to a real session).
     - Call `(jf/gptel-persistent-agent--extract-final-text agent-buffer)`.
     - Expect return value `"Final answer."` (whitespace handling per parser — assert the relevant trimmed shape).

   - `it "DONE returns an empty string when no text segment exists"`
     > Decision 2 edge case (design.md § "Decisions" 2)
     - Construct an agent buffer with a single user turn and a single assistant block whose only content is a tool block (no trailing text):
       ```
       #+begin_user
       Q
       #+end_user

       #+begin_assistant
       #+begin_tool (some_tool)
       (:name "some_tool" :args nil)

       result
       #+end_tool
       #+end_assistant
       ```
     - Call `(jf/gptel-persistent-agent--extract-final-text agent-buffer)`.
     - Expect return value `""`.

   - `it "DONE handler invokes main-cb exactly once with the final text"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Parent-child communication" → "DONE returns the final assistant text segment" (call-count piece)
     - Build a fake FSM by hand:
       ```elisp
       (let* ((calls nil)
              (main-cb (lambda (text) (push text calls)))
              (agent-buf (<buffer with content from above>))
              (overlay (with-temp-buffer (make-overlay 1 1)))
              (handler (jf/gptel-persistent-agent--make-on-done main-cb agent-buf))
              (fsm (gptel-make-fsm :info (list :context overlay))))
         (funcall handler fsm)
         (expect (length calls) :to-equal 1)
         (expect (car calls) :to-equal "Final answer.")
         (expect (overlayp overlay) :to-be-truthy)        ; struct still exists
         (expect (overlay-buffer overlay) :to-be nil))    ; but deleted
       ```

5. **`describe "Stream callback wiring"`** containing:

   - `it "uses chat-mode's public stream callback (not a custom one)"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Execution lifecycle" → "gptel-request invocation shape" (stream-callback piece)
     - Same setup as the gptel-request invocation test.
     - Inspect the captured `:callback` value.
     - Expect it is a closure (returned by `gptel-chat-stream-callback`). Since the closure isn't directly comparable to `gptel-chat-stream-callback` itself (which is a function that returns closures), assert structurally: `(functionp callback)` and the closure's behavior matches when invoked with a stringy chunk.

## Design rationale

The send/completion tests are the heart of "did the rebuild actually use chat-mode's public API correctly?" Each `it` pins one piece of the contract:

- Invocation shape — caller-visible: messages list, stream-callback, composed FSM, overlay context.
- Handler ordering — agent first, then chat-mode lifecycle, then upstream. Out-of-order composition would silently delay overlay updates or bypass network firing.
- Final-text extraction — the user-facing payload returned to the parent.
- Empty-text fallback — Decision 2; needs an explicit edge-case test so the contract doesn't silently drift toward "throw an error" or "return last segment of any type."

The hand-built FSM in the DONE-handler test (no real `gptel-request`) is the cleanest way to assert on the handler closure's behavior without simulating a full state machine. Calling the handler directly with a synthesized FSM info plist is the same pattern chat-mode's own tests use.

## Design pattern

For FSM struct introspection: confirm the slot accessor names with `(describe-symbol 'gptel-fsm)` or by grepping `runtime/straight/repos/gptel/gptel-request.el`. The struct is `cl-defstruct gptel-fsm (state info handlers)`.

For "build a buffer with chat-mode content for the parser to walk": use `gptel-chat-test--with-buffer` (already imported via `gptel-chat-test-helpers` if you require it; otherwise inline `with-temp-buffer` + `insert`).

The captured-args inspection pattern: `(let ((captured nil)) (with-mock-gptel-request 'captured (...)) (let ((args (car captured))) ...))`. The mock pushes onto the var, so `(car captured)` is the most recent invocation.

## Verification

- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` includes the new file and all `it` blocks pass.
- Edge-case empty-text test passes (returns `""`, not nil, not error).
- Handler-composition assertion catches order regressions.

**Done means**: ~9 `it` blocks across the three `describe` groups, all green, FSM composition + invocation shape + final-text extraction (incl. empty case) all locked down.

## Context

specs/persistent-agent/spec.md (delta) § "Execution lifecycle", "Parent-child communication"
design.md § "Decisions" 2 (empty-text), § "Layer 2" (handler builder, extractor)
design.md § "Test Strategy" → "Mock pattern for `gptel-request`" — exact mock recipe
architecture.md § "Interfaces" → "FSM handler composition (request lifecycle)" — exact handler ordering
