## ADDED Requirements

### Requirement: Public programmatic-send API

The system SHALL expose the buffer-parse → messages → assistant-block → stream-callback → fsm-handlers pipeline as a documented public API under the `gptel-chat-` prefix (no double-dash). The public surface enables non-interactive callers — such as `PersistentAgent` — to drive a chat-mode buffer through a `gptel-request` programmatically without depending on internal-by-naming functions.

The public surface consists of exactly five symbols:

1. `gptel-chat-parse-buffer` — function `(&optional BUFFER) → list of turn plists`. Parses a chat-mode buffer (current buffer or BUFFER) and returns an ordered list of turn plists with the same shape consumed by `gptel-chat-send`. Signals `user-error` on parser-detected malformations (e.g., unclosed user block).
2. `gptel-chat-turns-to-messages` — function `(TURNS) → cons-list suitable as gptel-request's :prompt`. Converts the turn-list returned by `gptel-chat-parse-buffer` into the alternating `(prompt . STR)` / `(response . STR)` / `(tool . PLIST)` cons-list shape that `gptel-request` accepts as its `:prompt` argument.
3. `gptel-chat-open-assistant-block` — function `(TURN) → live advance marker`. Given a populated user-turn plist whose `:end` marker points at the start of its `#+end_user` line, opens a fresh `#+begin_assistant` block immediately after that line and returns a live advance-insertion marker positioned at the assistant block's body, ready to be used as the streaming-insertion point.
4. `gptel-chat-stream-callback` — function `(INSERTION-MARKER) → callback function`. Returns a callback suitable for passing as `gptel-request`'s `:callback` keyword. The returned callback handles streaming token insertion, tool-block rendering, post-completion assistant-block closing, and the post-completion append-empty-user-block flow that interactive sessions rely on.
5. `gptel-chat-fsm-handlers` — variable. A handler alist suitable for passing as `:handlers` to `gptel-make-fsm`. Composes chat-mode's lifecycle indicator handlers with gptel's upstream state-driving handlers for `WAIT`, `TYPE`, `TOOL`, `DONE`, `ERRS`, and `ABRT`.

The public API SHALL be stable in the sense that in-tree callers (the `PersistentAgent` tool, future programmatic-send callers) MAY depend on the names and contracts above without coordinating with the chat-mode module's internal refactors.

The interactive command `gptel-chat-send` SHALL be implemented as a thin wrapper that composes calls to these five public symbols (plus its in-flight guard and user-block resolution).

The previously-named internal symbols `gptel-chat--parse-buffer`, `gptel-chat--turns-to-messages`, `gptel-chat--open-assistant-block`, `gptel-chat--stream-callback`, and `gptel-chat--fsm-handlers` SHALL be renamed to their public-prefixed counterparts. All in-tree callers SHALL be updated. No `defalias` shims SHALL remain.

#### Scenario: Public parse function callable from outside chat-mode
- **WHEN** a non-interactive caller (e.g., `PersistentAgent`) invokes `gptel-chat-parse-buffer` against a chat-mode buffer
- **THEN** the function returns the same turn-plist list that `gptel-chat-send` constructs internally for the same buffer state
- **AND** the function does not require chat-mode's interactive context (point inside a user block, etc.)

#### Scenario: Public turns-to-messages produces gptel-request input
- **WHEN** a caller passes the result of `gptel-chat-parse-buffer` to `gptel-chat-turns-to-messages`
- **THEN** the return value is a cons-list of `(prompt . STR)` / `(response . STR)` / `(tool . PLIST)` entries in document order
- **AND** the return value is directly usable as `gptel-request`'s `:prompt` keyword argument

#### Scenario: Public open-assistant-block returns insertion marker
- **WHEN** a caller passes a user turn plist (whose `:end` marker points at `#+end_user`) to `gptel-chat-open-assistant-block`
- **THEN** the buffer gains a fresh `#+begin_assistant` line immediately after that user turn's `#+end_user` line
- **AND** the function returns a live marker with insertion-type `t`, positioned at the assistant block's body line
- **AND** the marker remains valid for use as a streaming-insertion target

#### Scenario: Public stream-callback usable with gptel-request
- **WHEN** a caller passes the result of `gptel-chat-stream-callback` as `:callback` to `gptel-request` (with `:stream t`)
- **THEN** streaming response chunks are inserted at the supplied insertion marker, sanitized for delimiter collisions, and wrapped in tool blocks where appropriate
- **AND** on stream completion, the assistant block is closed with `#+end_assistant`
- **AND** an empty `#+begin_user` / `#+end_user` block is appended after the closed assistant block (the post-completion append flow)

#### Scenario: Public fsm-handlers usable with gptel-make-fsm
- **WHEN** a caller passes `gptel-chat-fsm-handlers` as `:handlers` to `gptel-make-fsm`
- **THEN** the resulting FSM has chat-mode's lifecycle handlers chained ahead of gptel's upstream state-driving handlers
- **AND** entering each of `WAIT`, `TYPE`, `TOOL`, `DONE`, `ERRS`, `ABRT` runs both layers in order

#### Scenario: gptel-chat-send is a thin composition over the public API
- **WHEN** the user invokes `gptel-chat-send` interactively from a chat-mode buffer
- **THEN** `gptel-chat-send` calls (in order) the in-flight guard, user-turn resolution, `gptel-chat-parse-buffer`, `gptel-chat-turns-to-messages`, `gptel-chat-open-assistant-block`, `gptel-chat-stream-callback`, and `gptel-request` with `:fsm` constructed from `gptel-chat-fsm-handlers`
- **AND** the interactive command's behavior is observably identical to its previous behavior (same buffer mutations, same `gptel-request` invocation shape)

#### Scenario: No double-dash internal aliases remain
- **WHEN** the chat-mode module is loaded
- **THEN** the symbols `gptel-chat--parse-buffer`, `gptel-chat--turns-to-messages`, `gptel-chat--open-assistant-block`, `gptel-chat--stream-callback`, and `gptel-chat--fsm-handlers` are unbound (or have been removed from the source)
- **AND** no `defalias` from the old names to the new names exists in the codebase
