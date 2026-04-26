## Overview

This document settles the four architecture-level open questions, fixes implementation details that flow from those decisions, and sequences the work for task generation.

The implementation is in two layers, executed in order:

1. **Chat-mode public-API rename** (mechanical, regression-tested by the existing chat-mode suite plus a new public-contract spec).
2. **Persistent-agent rebuild** on top of the new public API.

Layer 1 must complete before layer 2 — the agent depends on the new symbol names.

## Decisions

### Decision 1 — Scope-yml writing stays inside the agent module

Hand-format the agent's `scope.yml` inside `jf/gptel-persistent-agent--write-scope-file`. Do not promote a public scope-module writer in this change.

**Rationale**: The agent writes a single, fixed-shape `scope.yml` (one variable section: `paths.read`; everything else is constant). A generic public writer would either be over-engineered for one caller or so narrow it's not reusable. YAGNI applies. When a second caller emerges, promote then.

**Implication**: The agent's writer is the moral equivalent of the existing hand-formatted YAML — same output shape — but the function lives behind a name and has tests instead of being inline. The function calls `with-temp-file` against an `(expand-file-name jf/gptel-session--scope-file session-dir)` target and emits the YAML in the same shape currently produced.

**Fallback if it gets ugly**: if the deny-list grows variable, refactor inside the agent module first; only consider a public scope-module helper after that.

### Decision 2 — Empty final-text returns the empty string

`jf/gptel-persistent-agent--extract-final-text` returns the content of the last `text` segment in the last assistant turn. If `:segments` is `nil`, contains no text segments, or every text segment is empty, return `""`.

**Rationale**: User-stated preference for simplicity ("we may revisit this in the future"). An empty string is the least-surprise result — the parent's tool-result handling already deals with strings of any length. Sentinels and errors add complexity for an edge case that may never occur in practice.

**Implication**: The DONE handler always calls `main-cb` exactly once with a string; no error path for "agent finished with no text." The ERRS/ABRT handlers cover the actual error/abort cases.

**Test coverage**: an `it` block under `send-and-completion-spec.el` builds an assistant turn with only tool segments, runs the extractor, asserts `""`. Documented in `architecture.md` "Scenario Mapping" as one of the edge-case variants.

### Decision 3 — `find-file-noselect` for agent buffer creation

The agent module opens its session file with `find-file-noselect` (not `find-file`). The parent buffer retains focus throughout the agent run. The agent buffer is reachable through the buffer list, the registry, and the session-current symlink, but does not steal the user's window.

**Rationale**: The agent runs autonomously while the parent's tool call is in flight. Switching focus to the agent buffer would jar the user out of context. The overlay in the parent buffer is the intended progress UI.

**Implication**: All references to "open the file" in this change resolve to `find-file-noselect`. `find-file-hook` still fires (it runs in both selected and unselected variants), so auto-init still triggers.

### Decision 4 — Prompt is embedded in `initial-content`

The agent module constructs the `initial-content` string for `jf/gptel--create-session-core` directly, using the same `:PROPERTIES:` drawer layout that `jf/gptel--initial-session-content` produces, but with the supplied prompt embedded inside the `#+begin_user` block:

```elisp
(format ":PROPERTIES:\n:GPTEL_PRESET: %s\n:GPTEL_PARENT_SESSION_ID: %s\n:END:\n#+begin_user\n%s\n#+end_user\n"
        preset-name parent-session-id prompt)
```

This file is written before `find-file-noselect` runs. By the time auto-init fires, the file is already complete and parseable.

**Rationale**: Eliminates the need for any post-creation buffer mutation in the agent code. The send flow (parse → messages → open-assistant-block) reads the user block straight off disk.

**Drift risk**: if `jf/gptel--initial-session-content` ever changes its drawer or block shape, the agent's parallel format string must also update. Mitigation: a `creation-spec.el` test asserts on the *full* file content after creation (including drawer keys, block delimiters, and embedded prompt). Format drift in either function will fail the test.

**Cross-reference comment**: the agent's format-string call site links to `jf/gptel--initial-session-content` so a future maintainer sees both places.

## Implementation Approach

### Layer 1: Chat-mode public-API rename

**Files**: `config/gptel/chat/{parser,send,stream}.{org,el}` and any in-tree callers (verify with `grep -rn 'gptel-chat--\(parse-buffer\|turns-to-messages\|open-assistant-block\|stream-callback\|fsm-handlers\)\b' config/`).

**Steps**:

1. Rename in source: each `gptel-chat--<name>` → `gptel-chat-<name>`. Five renames total (four functions, one variable).
2. Promote each renamed symbol's docstring to public-API style: lead with the contract (signature, return value, semantics) before any implementation note. Move implementation notes to `;;;` comments inside the function body if needed.
3. Update in-tree callers. Initial scan locations to check (non-exhaustive — confirm with grep):
   - `config/gptel/chat/send.el` itself (chat-send composition)
   - `config/gptel/chat/menu.el` (preset application path may reference)
   - `config/gptel/chat/test/**` (tests reference internals directly)
   - The persistent-agent module's old code is *replaced* in layer 2, so layer-1 callers don't touch it.
4. No `defalias` shims. The change is intentional and verifiable: `(fboundp 'gptel-chat--parse-buffer)` returns nil after load.

**Public-contract test (`config/gptel/chat/test/{parser,send,stream}/public-api-spec.el`)**:

These specs assert *only* on the public contract, not implementation:

- Calling `gptel-chat-parse-buffer` from a non-chat-mode helper buffer returns the same turn list it would produce inside chat-mode.
- The cons-list returned by `gptel-chat-turns-to-messages` matches the documented shape.
- `gptel-chat-open-assistant-block` mutates the buffer and returns a usable advance marker.
- The callback returned by `gptel-chat-stream-callback` accepts the documented `(response info)` calling convention and inserts at the supplied marker.
- `gptel-chat-fsm-handlers` is a list whose entries match `(STATE FN ...)` shape, with the documented per-state handler chain.
- Each of the old `--`-prefixed names is `(not (fboundp ...))` after load.

**Risk**: byte-compilation order. If a downstream `.el` file references `gptel-chat-parse-buffer` while its `.org` source still has `gptel-chat--parse-buffer` (untangled), tangling order matters. Mitigation: tangle all renamed `.org` files in a single pass via `./bin/tangle-org.sh` per file, then run `./bin/run-tests.sh -d config/gptel/chat` to confirm everything resolves.

### Layer 2: Persistent-agent rebuild

**File**: `config/gptel/tools/persistent-agent.{org,el}` — full rewrite.

**Function inventory** (final names; replaces the current ~170-line implementation):

| Function | Role |
|---|---|
| `jf/gptel-persistent-agent--task` | gptel tool entry point; orchestrates the flow |
| `jf/gptel-persistent-agent--write-scope-file` | writes the agent's `scope.yml` from `allowed-paths` |
| `jf/gptel-persistent-agent--initial-content` | constructs the drawer + populated user block string |
| `jf/gptel-persistent-agent--build-fsm-handlers` | composes the agent + chat + upstream handler alist |
| `jf/gptel-persistent-agent--on-done` | DONE handler: `--extract-final-text`, delete overlay, `main-cb` |
| `jf/gptel-persistent-agent--on-errs` | ERRS handler: format error, delete overlay, `main-cb` |
| `jf/gptel-persistent-agent--on-abrt` | ABRT handler: format abort, delete overlay, `main-cb` |
| `jf/gptel-persistent-agent--extract-final-text` | reads agent buffer, returns last assistant text segment (Decision 2) |

The old `--create-overlay`, `--indicate-wait`, `--indicate-tool-call`, `--fsm-handlers` (defvar), and `jf/gptel--auto-save-session-buffer` are removed.

**Tool registration** uses the new argument schema (no `denied_paths`).

**Required dependencies** (require statements at top of file):

```elisp
(require 'gptel)
(require 'gptel-agent)            ; for gptel-agent--task-overlay,
                                  ; --indicate-wait, --indicate-tool-call
(require 'gptel-chat-parser)      ; for gptel-chat-parse-buffer,
                                  ; gptel-chat-turns-to-messages
(require 'gptel-chat-send)        ; for gptel-chat-open-assistant-block,
                                  ; gptel-chat-fsm-handlers
(require 'gptel-chat-stream)      ; for gptel-chat-stream-callback
(require 'gptel-session-constants)
(require 'gptel-session-filesystem) ; for jf/gptel--create-agent-directory,
                                    ; jf/gptel--context-file-path,
                                    ; jf/gptel--session-id-from-directory
(require 'gptel-session-commands)   ; for jf/gptel--create-session-core
(require 'gptel-session-logging)
```

**Orchestration body** (sketch — final form in implementation):

```elisp
(defun jf/gptel-persistent-agent--task (main-cb preset description prompt
                                                 &optional allowed-paths)
  ;; 1. Validate parent + preset (signal user-error before any side effects).
  (unless jf/gptel--session-dir
    (user-error "PersistentAgent requires parent persistent session"))
  (let ((preset-sym (intern preset)))
    (unless (gptel-get-preset preset-sym)
      (user-error "Preset '%s' not found in gptel--known-presets" preset))

    ;; 2. Capture parent's response-tracking position for overlay placement.
    (let* ((info (and (boundp 'gptel--fsm-last)
                      gptel--fsm-last
                      (gptel-fsm-info gptel--fsm-last)))
           (where (or (plist-get info :tracking-marker)
                      (plist-get info :position)
                      (point-marker)))
           (parent-id   jf/gptel--session-id)
           (allowed-paths-list (if (vectorp allowed-paths)
                                   (append allowed-paths nil)
                                 allowed-paths))

           ;; 3. Create directory + scope.yml + session.org.
           (session-dir (jf/gptel--create-agent-directory
                         jf/gptel--branch-dir preset description))
           (session-id  (jf/gptel--session-id-from-directory session-dir))
           (_           (jf/gptel-persistent-agent--write-scope-file
                         session-dir allowed-paths-list))
           (initial-content
            (jf/gptel-persistent-agent--initial-content
             preset-sym parent-id prompt))
           (session-info
            (jf/gptel--create-session-core
             session-id session-dir preset-sym initial-content
             nil nil parent-id))
           (session-file (plist-get session-info :session-file))

           ;; 4. Open the file (auto-init handles mode/preset/registry/save).
           (agent-buffer (find-file-noselect session-file))

           ;; 5. Build overlay in the parent buffer.
           (overlay (gptel-agent--task-overlay where preset description)))

      ;; 6. Send: parse, messages, open assistant, request.
      (with-current-buffer agent-buffer
        (let* ((turns     (gptel-chat-parse-buffer))
               (user-turn (cl-loop for turn in (reverse turns)
                                   when (eq (plist-get turn :role) 'user)
                                   return turn))
               (messages  (gptel-chat-turns-to-messages turns))
               (insertion (gptel-chat-open-assistant-block user-turn))
               (stream-cb (gptel-chat-stream-callback insertion))
               (handlers  (jf/gptel-persistent-agent--build-fsm-handlers
                           gptel-chat-fsm-handlers main-cb agent-buffer))
               (fsm       (gptel-make-fsm :handlers handlers)))
          (gptel-request messages
            :stream   t
            :callback stream-cb
            :context  overlay
            :fsm      fsm))))))
```

**Handler builder**:

```elisp
(defun jf/gptel-persistent-agent--build-fsm-handlers (base main-cb agent-buf)
  "Return an FSM handler alist composing agent handlers atop BASE.
BASE is the chat-mode handler alist (gptel-chat-fsm-handlers).
MAIN-CB is the parent's tool callback.
AGENT-BUF is the agent's session buffer (for final-text extraction)."
  (mapcar
   (lambda (entry)
     (pcase (car entry)
       ('WAIT `(WAIT ,#'gptel-agent--indicate-wait ,@(cdr entry)))
       ('TOOL `(TOOL ,#'gptel-agent--indicate-tool-call ,@(cdr entry)))
       ('DONE `(DONE ,(jf/gptel-persistent-agent--make-on-done main-cb agent-buf)
                     ,@(cdr entry)))
       ('ERRS `(ERRS ,(jf/gptel-persistent-agent--make-on-errs main-cb)
                     ,@(cdr entry)))
       ('ABRT `(ABRT ,(jf/gptel-persistent-agent--make-on-abrt main-cb)
                     ,@(cdr entry)))
       (_     entry)))
   base))
```

The `--make-on-done` etc. wrappers close over `main-cb` and `agent-buf` so the handlers are real closures. The completion handlers read the overlay from `(plist-get (gptel-fsm-info fsm) :context)` to delete it.

**Final-text extraction**:

```elisp
(defun jf/gptel-persistent-agent--extract-final-text (agent-buffer)
  "Return the trailing text of the last assistant turn in AGENT-BUFFER.
Returns the empty string when no text segment exists (Decision 2)."
  (with-current-buffer agent-buffer
    (let* ((turns     (gptel-chat-parse-buffer))
           (last-asst (cl-loop for turn in (reverse turns)
                               when (eq (plist-get turn :role) 'assistant)
                               return turn))
           (segments  (and last-asst (plist-get last-asst :segments)))
           (last-text (cl-loop for seg in (reverse (or segments '()))
                               when (eq (plist-get seg :type) 'text)
                               return (plist-get seg :content))))
      (or last-text ""))))
```

**Scope-file writer** (Decision 1):

```elisp
(defun jf/gptel-persistent-agent--write-scope-file (session-dir allowed-paths)
  "Write SESSION-DIR/scope.yml with read paths from ALLOWED-PATHS.
ALLOWED-PATHS is a list (already normalized from vector). Empty/nil → read: []."
  (let ((scope-file (expand-file-name jf/gptel-session--scope-file session-dir)))
    (with-temp-file scope-file
      (insert "paths:\n")
      (insert "  read:\n")
      (if allowed-paths
          (dolist (p allowed-paths)
            (insert (format "    - \"%s\"\n" p)))
        (insert "    []\n"))
      (insert "  write:\n")
      (insert "    - \"/tmp/**\"\n")
      (insert "  deny:\n")
      (dolist (p '("**/.git/**" "**/runtime/**" "**/.env" "**/node_modules/**"))
        (insert (format "    - \"%s\"\n" p))))))
```

This is the same byte-shape as the current implementation, just behind a name.

**Initial-content builder** (Decision 4):

```elisp
(defun jf/gptel-persistent-agent--initial-content (preset-sym parent-id prompt)
  "Build the initial session.org content for an agent.
Format mirrors `jf/gptel--initial-session-content' (cross-reference: keep
shapes aligned; the auto-init-reload test asserts on the full layout)."
  (format ":PROPERTIES:\n:GPTEL_PRESET: %s\n:GPTEL_PARENT_SESSION_ID: %s\n:END:\n#+begin_user\n%s\n#+end_user\n"
          (symbol-name preset-sym) parent-id prompt))
```

## Test Strategy

(See `architecture.md` "Testing Approach" for framework, organization, and patterns. This section maps spec scenarios to specific test cases.)

### Layer 1: Chat-mode public-API contract

`config/gptel/chat/test/parser/public-api-spec.el`
- `it "is callable from a non-chat-mode buffer"` → spec scenario "Public parse function callable from outside chat-mode"
- `it "produces gptel-request prompt-shaped output"` → spec scenario "Public turns-to-messages produces gptel-request input"
- `it "exposes no double-dash aliases after load"` → spec scenario "No double-dash internal aliases remain"

`config/gptel/chat/test/send/public-api-spec.el`
- `it "opens an assistant block and returns an advance marker"` → spec scenario "Public open-assistant-block returns insertion marker"
- `it "fsm-handlers exposes the documented per-state chain"` → spec scenario "Public fsm-handlers usable with gptel-make-fsm"

`config/gptel/chat/test/stream/public-api-spec.el`
- `it "stream callback inserts at the supplied marker"` → spec scenario "Public stream-callback usable with gptel-request"

### Layer 2: Persistent-agent

`config/gptel/tools/test/persistent-agent/creation-spec.el`
- `it "creates the agent directory under the parent branch"` → spec "Agent directory created under parent branch"
- `it "writes session.org with a self-describing :PROPERTIES: drawer"` → spec "session.org carries a self-describing :PROPERTIES: drawer"
- `it "writes scope.yml with allowed paths"` → spec "scope.yml written via scope-module helper"
- `it "writes scope.yml with empty read paths when allowed-paths is omitted"` → spec "Explicit path configuration (zero inheritance)"
- `it "rejects an unknown preset before any directory is created"` → spec "Unknown preset rejected before any side effect"
- `it "rejects invocation outside a parent session"` → spec "Parent session requirement"

`config/gptel/tools/test/persistent-agent/auto-init-reload-spec.el`
- `it "the agent buffer is in gptel-chat-mode after creation"` → spec "Agent buffer auto-initializes via find-file-hook"
- `it "the saved session.org reloads as an interactive chat session"` → spec "Saved agent session reloads as interactive"

`config/gptel/tools/test/persistent-agent/send-and-completion-spec.el`
- `it "issues a gptel-request with messages, stream-callback, and composed FSM"` → spec "gptel-request invocation shape"
- `it "the FSM handler alist composes WAIT/TOOL with overlay handlers ahead of chat lifecycle and upstream"` → spec "WAIT state updates the parent overlay" + "TOOL state updates the parent overlay with cumulative count"
- `it "DONE returns the last assistant text segment"` → spec "DONE returns the final assistant text segment"
- `it "DONE returns an empty string when no text segment exists"` → Decision 2 edge case
- `it "no agent-specific auto-save hook is installed"` → spec "No agent-specific auto-save hook"

`config/gptel/tools/test/persistent-agent/error-handling-spec.el`
- `it "ERRS deletes the overlay and returns an error string"` → spec "Network failure cleanup"
- `it "ABRT deletes the overlay and returns an abort string"` → spec "User abort cleanup"
- `it "the parent overlay is gone after every terminal state"` → spec "Overlay never leaks"

### Mock pattern for `gptel-request`

The send-and-completion tests need to drive an FSM through state transitions without actually calling a backend. Pattern:

```elisp
(let ((captured-args nil)
      (captured-fsm  nil))
  (cl-letf (((symbol-function 'gptel-request)
             (lambda (prompt &rest args)
               (setq captured-args (cons prompt args))
               (setq captured-fsm (plist-get args :fsm))
               nil)))
    ;; Run the agent's task entry point …
    ;; … then drive the captured FSM by calling its handlers in order.
    ))
```

Handler-by-handler invocation lets each test assert on overlay state and `main-cb` calls without depending on real network or process I/O.

## Trade-offs and Risks

### Trade-off: hard dependency on `gptel-agent--`-prefixed upstream symbols

Architecture flagged this. Decision: accept the dependency. Mitigation: a load-time check and a single-line fallback comment in the agent module noting that if upstream renames these, we copy them locally as a stop-gap. Not adding the fallback code in this change — YAGNI.

### Risk: order-dependence of FSM handler list

The composed handler alist relies on the agent's handler running *first* in each state's chain. If a future chat-mode change inserts a new handler ahead of the existing chat-mode lifecycle handler, the agent's handler still runs first because the agent prepends. The risk is the reverse: if the agent ever stops prepending (e.g., a refactor `nconc`'s instead of `cons`'ing), overlay updates fall behind upstream's state-driving work. Mitigated by the FSM-composition test asserting on exact alist shape.

### Risk: `find-file-noselect` and `find-file-hook`

`find-file-hook` runs in `find-file-noselect`. Verified by reading Emacs source. Documented here so a future maintainer doesn't second-guess the choice.

### Risk: byte-compile order during the rename

Layer 1 must complete (and its `.org` files re-tangled) before layer 2's source references the new names. The task graph enforces this dependency.

## Migration

No automated migration of pre-existing on-disk agent sessions (declared out of scope in proposal.md and architecture.md). Files written by the old implementation:

- Lack the `:PROPERTIES:` drawer → auto-init won't apply a preset
- May be in `.md` rather than `.org` → auto-init regex doesn't match
- Were written assuming `gptel-mode` not `gptel-chat-mode`

Affected files are archive-only after this change. The tools log (`tools.org`) and any user-curated content remain readable by hand.

## Sequencing for Task Generation

When `/opsx-tasks generate` runs against this design, the recommended dependency graph is:

```
T1: Rename chat-mode internals to public                    (no deps)
    ├─ rename in parser.org/el
    ├─ rename in send.org/el
    ├─ rename in stream.org/el
    ├─ update existing chat-mode test references
    └─ tangle + run config/gptel/chat tests (regression)

T2: Add public-API contract tests                           (deps: T1)
    ├─ chat/test/parser/public-api-spec.el
    ├─ chat/test/send/public-api-spec.el
    └─ chat/test/stream/public-api-spec.el

T3: Add agent test fixtures                                 (deps: T1)
    └─ tools/test/persistent-agent/helpers-spec.el

T4: Rebuild persistent-agent module                         (deps: T1)
    ├─ rewrite persistent-agent.org (all 8 functions above)
    ├─ tangle, byte-compile clean
    └─ remove byte-identical local copies

T5: Add agent creation/scope tests                          (deps: T3, T4)
    └─ tools/test/persistent-agent/creation-spec.el

T6: Add agent auto-init reload tests                        (deps: T3, T4)
    └─ tools/test/persistent-agent/auto-init-reload-spec.el

T7: Add agent send/completion tests                         (deps: T3, T4)
    └─ tools/test/persistent-agent/send-and-completion-spec.el

T8: Add agent error-handling tests                          (deps: T3, T4)
    └─ tools/test/persistent-agent/error-handling-spec.el

T9: Scrub denied_paths references in in-tree agent prompts  (deps: T4)
    └─ grep config/gptel/agents/ + any docs
```

T2, T3 can run in parallel. T5–T8 can run in parallel after T3 + T4. T9 is small and can run any time after T4.
