# GPTEL Chat Mode

## Purpose

A dedicated major mode for multi-turn chat and work-log interaction with an LLM, complementary to upstream `gptel-mode`. Where `gptel-mode` treats LLM interaction as invisible assistance inside arbitrary documents, `gptel-chat-mode` treats the sequence of turns as the primary artifact and gives each turn a first-class structural container. The mode provides:

- A canonical on-disk format using symmetric `#+begin_user` / `#+begin_assistant` special blocks with nested `#+begin_tool` blocks
- Message construction from buffer state without relying on text properties or `gptel--parse-buffer`
- Streaming response insertion with delimiter-collision sanitization
- Mode-provided send, navigation, and regenerate commands
- A minimal display layer for visual role distinction

## Key Concepts

### Buffer Format

A chat-mode buffer is a sequence of **turns**, each represented by an org special block whose `#+begin_*` and `#+end_*` delimiter lines begin at the start of a line with no leading whitespace (the `#` is the first character of the line). Turn blocks are **outer blocks** — not nested inside another user or assistant block. Org headings, paragraphs, and drawers that appear outside turn blocks are permitted as organizational/commentary content for the human reader; they do NOT participate in message construction. A short buffer typically has no headings:

```org
#+begin_user
What's the capital of France?
#+end_user

#+begin_assistant
Paris.
#+end_assistant

#+begin_user
Can you run `uname`?
#+end_user

#+begin_assistant
Sure.

#+begin_tool (run_bash_command :command "uname")
(:name "run_bash_command" :args (:command "uname"))

{"success":true,"output":"Darwin\n","exit_code":0}
#+end_tool

You're on Darwin.
#+end_assistant
```

A longer buffer may use org headings to organize sections. Turn blocks nested inside heading sections at any depth are still part of the turn sequence; the message list is the union of all outer user/assistant blocks in document order, independent of outline depth:

```org
* Initial exploration

Free-form notes the human keeps for themselves. Ignored by message construction.

#+begin_user
...
#+end_user

#+begin_assistant
...
#+end_assistant

* Follow-up thread

#+begin_user
...
#+end_user
```

### Metadata and Commentary

Optional `#+<keyword>:` lines and a top-of-buffer `:PROPERTIES:` drawer are treated as file metadata. Org headlines and paragraph content outside turn blocks are treated as human commentary/organization. All such content is excluded from message construction — the LLM sees only turn blocks.

### Prompt Composition

The body of a `#+begin_user` block supports the full org-mode editing experience. Users composing a prompt may use org headings, source blocks, lists, tables, links, emphasis, footnotes, or any other org feature; the entire block body — from the line after `#+begin_user` to the line before `#+end_user` — is included verbatim in the constructed user message. Heading-like text inside a user block (e.g., `* Context`) is part of the prompt's body; it does NOT partition the document-level turn structure.

Users who need to include a literal `#+end_user`, `#+end_assistant`, or `#+end_tool` line in their prompt prefix it with `,` (the same escape convention the assistant write-path uses automatically). The parser strips the leading `,` on send.

### Role Taxonomy

| Block | Role in API message list |
|-------|--------------------------|
| `#+begin_user`...`#+end_user` | `user` message with block content |
| `#+begin_assistant`...`#+end_assistant` | Zero or more assistant / tool-call / tool-result messages (see Message Construction) |
| `#+begin_tool (<name> <plist...>)`...`#+end_tool` | Tool call + tool result pair, nested inside assistant block. Header contract: a single sexp whose `car` is the tool-name symbol and whose `cdr` is the arguments plist (see design.md Decision 10) |

### Delimiter Collision

Any response content line matching `^#\+\(end_user\|end_assistant\|end_tool\)\b` would prematurely terminate a containing block. The mode sanitizes streamed assistant content using the same convention as upstream `gptel` (prepending `,` to offending lines), but with a targeted three-delimiter regex rather than a call to `org-escape-code-in-string` — see design.md Decision 4 for rationale. On read-back, the sanitization is undone before sending to the model.

## Requirements

### Requirement: Mode definition and activation

The system SHALL define `gptel-chat-mode` as a major mode derived from `org-mode`. Activation SHALL be possible via `M-x gptel-chat-mode`, file-local mode cookies (`-*- gptel-chat -*-`), or auto-mode-alist configuration. auto-mode-alist activation is a user-side configuration — users add their own pattern as desired; chat-mode does not register auto-mode-alist entries by default.

#### Scenario: Interactive activation
- **WHEN** running `M-x gptel-chat-mode` in a buffer
- **THEN** the major mode becomes `gptel-chat-mode`
- **AND** the buffer inherits `org-mode` features
- **AND** chat-mode keybindings are active

#### Scenario: File-local cookie activation
- **WHEN** opening a file with first line `# -*- gptel-chat -*-`
- **THEN** the buffer is in `gptel-chat-mode` after load

### Requirement: Buffer format validation

The system SHALL accept any org document whose `#+begin_user`, `#+begin_assistant`, and `#+begin_tool` special blocks are structurally well-formed. Specifically:

- Every `#+begin_user` / `#+begin_assistant` / `#+begin_tool` SHALL have a matching `#+end_*` delimiter of the same type.
- `#+begin_tool` blocks SHALL appear only inside an `#+begin_assistant` block.
- `#+begin_user` and `#+begin_assistant` blocks SHALL NOT be nested inside another user or assistant block.
- Content outside turn blocks — org headings (at any depth), paragraphs, drawers, `#+keyword:` lines, blank lines — is permitted and is treated as human organization/commentary. It does NOT participate in message construction.

#### Scenario: Empty buffer is valid
- **WHEN** the buffer contains only whitespace
- **THEN** message construction yields an empty message list
- **AND** no error is raised

#### Scenario: Metadata-only buffer is valid
- **WHEN** the buffer contains only `#+gptel-model: foo` and blank lines
- **THEN** message construction yields an empty message list

#### Scenario: Headings and commentary outside turn blocks are valid
- **WHEN** the buffer contains `* A heading`, free-form prose, and two turn blocks
- **THEN** message construction yields two messages (one per turn block)
- **AND** the heading and prose are not included in the message list

#### Scenario: Turn blocks nested under org headings are valid
- **WHEN** the buffer contains turn blocks inside org heading sections at arbitrary depth (e.g., under `* A` and `** A.1`)
- **THEN** all such blocks participate in message construction in document order
- **AND** heading depth has no effect on the message list

#### Scenario: Unmatched delimiter is invalid
- **WHEN** the buffer contains `#+begin_user` without a matching `#+end_user`
- **THEN** message construction signals a user-visible error identifying the offending line

#### Scenario: Tool block outside an assistant block is invalid
- **WHEN** the buffer contains a `#+begin_tool` block that is not inside any `#+begin_assistant` block
- **THEN** message construction signals a user-visible error identifying the tool block's start line

#### Scenario: Turn nested inside another turn is invalid
- **WHEN** a `#+begin_user` block appears inside an open `#+begin_assistant` block (or vice versa)
- **THEN** message construction signals a user-visible error identifying the inner block's start line

### Requirement: Message construction from buffer

The system SHALL construct an ordered list of API messages by walking the buffer for **outer** `#+begin_user` and `#+begin_assistant` blocks in document order. An outer block is a user or assistant block that is not nested inside another user or assistant block; its position relative to org heading structure (at document root, or inside a heading section at any depth) is irrelevant. Each block produces messages as follows:

- `#+begin_user` block → one `user` message with the block's content (verbatim, delimiter lines excluded)
- `#+begin_assistant` block → a sequence of messages: assistant-text segments and tool call / tool result pairs, in the order they appear inside the block
- Nested `#+begin_tool (<name> <plist...>)` block → one tool-call message (name + arguments plist) followed by one tool-result message (the block's remaining content)
- Delimiter-collision-escaped lines SHALL be un-escaped before inclusion in the outbound message

Content outside turn blocks (headlines, paragraphs, drawers, keywords) SHALL be skipped. Text that appears inside a user or assistant block, including lines that resemble `#+begin_*` delimiters, is treated as block body and included verbatim in the emitted message (subject to the escape/un-escape round-trip for the three closing delimiters).

**Empty-turn contract.** A turn is "empty" (and produces no outbound message) in exactly these cases:

- **User turn:** `:content` is `nil`, empty, or whitespace-only (zero non-whitespace characters — a block containing only newlines or spaces is still empty).
- **Assistant turn:** `:segments` is `nil` **OR** every segment in `:segments` is a `text` segment whose `:content` is `nil`, empty, or whitespace-only. A single `tool-call` segment is never empty — an assistant turn with at least one `tool-call` segment always emits at least one `(tool . PLIST)` message even if all surrounding text segments are blank.

This definition is what `gptel-chat--turn-to-messages` enforces. An empty assistant block (`#+begin_assistant\n#+end_assistant\n`) is parsed as an assistant turn with `:segments nil`; an assistant block containing only whitespace (`#+begin_assistant\n   \n#+end_assistant\n`) is parsed as an assistant turn with one or more whitespace-only `text` segments. Both shapes are skipped.

Message construction SHALL NOT depend on text properties or on `gptel--parse-buffer`.

#### Scenario: Single user-assistant turn
- **WHEN** the buffer contains one `#+begin_user`/`#+end_user` block followed by one `#+begin_assistant`/`#+end_assistant` block
- **THEN** the constructed message list is `[user: <user content>, assistant: <assistant content>]`

#### Scenario: Assistant turn with one tool call
- **WHEN** an `#+begin_assistant` block contains prose, then a `#+begin_tool` block with call args and result, then more prose
- **THEN** the message list for that turn is `[assistant: <pre-tool prose>, tool_call: <args>, tool_result: <result>, assistant: <post-tool prose>]`

#### Scenario: Delimiter escape round-trip
- **WHEN** an `#+begin_assistant` block content contains the literal line `,#+end_assistant`
- **THEN** the constructed assistant message contains the line `#+end_assistant` (leading comma stripped)

#### Scenario: Empty blocks are skipped
- **WHEN** a `#+begin_user` block contains no non-whitespace content
- **THEN** no `user` message is emitted for that block

#### Scenario: Empty assistant block is skipped
- **WHEN** a `#+begin_assistant` block is completely empty (no body between delimiters) — parsed as a turn with `:segments nil`
- **THEN** no `assistant` message is emitted for that block

#### Scenario: Whitespace-only assistant block is skipped
- **WHEN** a `#+begin_assistant` block contains only whitespace (blank lines, spaces, tabs) — parsed as a turn whose `:segments` list is either empty or contains only whitespace-only `text` segments
- **THEN** no `assistant` message is emitted for that block

#### Scenario: Assistant block with tool call and blank prose is kept
- **WHEN** a `#+begin_assistant` block contains only a `#+begin_tool` block and surrounding whitespace — parsed as a turn whose `:segments` contains one `tool-call` segment and possibly blank `text` segments
- **THEN** the message list contains the `(tool . PLIST)` message; the blank text segments are dropped but the assistant turn as a whole is not skipped

#### Scenario: User block body containing a `#+begin_assistant` literal
- **WHEN** a `#+begin_user` block contains a line beginning with `#+begin_assistant` as part of the user's prose
- **THEN** the constructed user message includes that literal line as body content
- **AND** no additional assistant message is emitted for that line

#### Scenario: Turns distributed across org headings
- **WHEN** the buffer contains one turn pair under `* Section A` and one user turn under `* Section B`
- **THEN** the constructed message list is `[user_A, assistant_A, user_B]` in document order
- **AND** the headings themselves are not represented in the message list

#### Scenario: User prompt composed with org structural features
- **WHEN** a `#+begin_user` block body contains an `* Heading` line, a `#+begin_src` block, a list, and emphasized text
- **THEN** the constructed user message contains all of that content verbatim
- **AND** the parser treats the whole block as a single user turn (the `* Heading` does not partition it)
- **AND** the `#+begin_src` / `#+end_src` pair inside the user block does not interfere with the outer `#+end_user` match

### Requirement: Send command

The system SHALL provide a command (`gptel-chat-send`, bound to `C-c C-c` by default) that sends the current buffer's turn sequence to the LLM via `gptel-request`. Invocation SHALL be valid from inside any `#+begin_user` block or from the point after the last `#+end_assistant` block when a pending user block exists. The command SHALL close the current user block if open, emit a new `#+begin_assistant` block, and stream the response into it.

#### Scenario: Send from inside open user block
- **WHEN** point is inside `#+begin_user`/`#+end_user` with non-empty content and the user runs `gptel-chat-send`
- **THEN** a `gptel-request` is issued with the constructed message list
- **AND** a new `#+begin_assistant` block is inserted immediately after the user block
- **AND** response chunks stream into the assistant block as they arrive

#### Scenario: Send with empty user block is a no-op
- **WHEN** point is inside an empty `#+begin_user` block and the user runs `gptel-chat-send`
- **THEN** no request is issued and the user sees a message indicating the prompt is empty

#### Scenario: Send from inside assistant block is rejected
- **WHEN** point is inside a `#+begin_assistant` block and the user runs `gptel-chat-send`
- **THEN** an error is signaled instructing the user to send from a user block

### Requirement: Response streaming and sanitization

The system SHALL insert streamed response chunks between an open `#+begin_assistant` marker and its eventual `#+end_assistant`. Each chunk SHALL be scanned for lines matching `^#\+end_\(user\|assistant\|tool\)\b` and rewritten to `,#+end_...` before insertion. On stream completion, the `#+end_assistant` delimiter SHALL be inserted and point positioned after it. On stream abort or error, the block SHALL be closed with a visible error marker and `#+end_assistant` appended.

#### Scenario: Normal stream completion
- **WHEN** the model returns a multi-chunk response with no delimiter collisions
- **THEN** all chunks appear in order inside the assistant block
- **AND** the block terminates with `#+end_assistant` on its own line
- **AND** the block is well-formed (parseable by `org-element`)

#### Scenario: Response contains collision with end_assistant
- **WHEN** a streamed response contains a line starting with `#+end_assistant`
- **THEN** that line is rewritten to start with `,#+end_assistant` when inserted
- **AND** the containing assistant block remains well-formed

#### Scenario: Response contains collision split across chunks
- **WHEN** one chunk ends with `#+end_ass` and the next begins with `istant\nmore`
- **THEN** the completed line is recognized as a collision and escaped before final insertion

#### Scenario: Streaming inserter rejects markers that do not advance past inserted text
- **WHEN** a caller constructs a streaming inserter with a marker whose `insertion-type` is not `t`
- **THEN** the factory signals an error at construction rather than producing reversed-order output downstream

#### Scenario: Stream abort
- **WHEN** the user cancels a streaming request mid-response
- **THEN** the assistant block is closed with `#+end_assistant`
- **AND** a visible marker records that the response was interrupted

#### Scenario: Multi-round tool-use preserves the assistant block across rounds
- **WHEN** the model completes a tool-use round and upstream fires the `t` completion signal with `:tool-use` set on the INFO plist
- **THEN** the callback flushes any holdback characters into the open assistant block
- **AND** the assistant block remains open — no `#+end_assistant` is appended and no new empty user block is started
- **AND** subsequent `(tool-result . ...)` events and the next request's streamed text land inside the same assistant block
- **WHEN** the model completes the final round (no further tool use pending) and upstream fires `t` with `:tool-use` unset or absent on the INFO plist
- **THEN** the holdback is flushed, `#+end_assistant` is appended, and an empty `#+begin_user`/`#+end_user` block is started with point positioned for the next human turn

### Requirement: Tool-call rendering inside assistant blocks

When the model emits a tool call during a streaming response, the system SHALL insert a `#+begin_tool` block nested inside the current assistant block. The opening delimiter SHALL include the call identifier and arguments in a form consistent with the existing `#+begin_tool` convention used elsewhere in the repository. When the tool result is available, the result SHALL be inserted inside the same tool block, and `#+end_tool` appended. Multiple tool calls in a single assistant turn SHALL be rendered as sequential sibling `#+begin_tool` blocks within the outer assistant block.

#### Scenario: Single tool call in a response
- **WHEN** an assistant response includes exactly one tool call
- **THEN** the assistant block contains one `#+begin_tool`/`#+end_tool` block with the call arguments and result
- **AND** the tool block appears in the correct position relative to surrounding prose

#### Scenario: Multiple tool calls in a response
- **WHEN** an assistant response includes three sequential tool calls interleaved with prose
- **THEN** the assistant block contains three sibling `#+begin_tool`/`#+end_tool` blocks in order
- **AND** prose segments appear in their correct positions between tool blocks

### Requirement: Turn navigation

The system SHALL provide `gptel-chat-next-turn` and `gptel-chat-previous-turn` commands that move point to the beginning of the next or previous outer turn block (either `user` or `assistant`) from the current position, regardless of org heading context. When no further turn exists in the direction of travel, the command SHALL signal a user-visible message and leave point unchanged. Org heading navigation is not shadowed by these commands (see design.md Decision 7).

#### Scenario: Next turn from inside a user block
- **WHEN** point is inside a user block followed by an assistant block
- **THEN** running `gptel-chat-next-turn` moves point to the start of `#+begin_assistant`

#### Scenario: Previous turn from inside an assistant block
- **WHEN** point is inside an assistant block preceded by a user block
- **THEN** running `gptel-chat-previous-turn` moves point to the start of `#+begin_user`

#### Scenario: Next turn at end of buffer
- **WHEN** point is after the last turn block
- **THEN** running `gptel-chat-next-turn` emits "No next turn" and does not move point

### Requirement: Regenerate last response

The system SHALL provide a `gptel-chat-regenerate` command that removes the most recent `#+begin_assistant` block and re-issues the request for the preceding user turn. If the buffer's last block is a `#+begin_user` block (no response yet), the command SHALL signal that there is no response to regenerate.

#### Scenario: Regenerate after a completed response
- **WHEN** the buffer ends in `#+begin_user`...`#+begin_assistant`...`#+end_assistant` and the user runs `gptel-chat-regenerate`
- **THEN** the trailing assistant block is removed
- **AND** a new request is issued and streamed into a fresh assistant block

#### Scenario: Regenerate when no response exists
- **WHEN** the buffer's last block is an unanswered `#+begin_user`
- **THEN** `gptel-chat-regenerate` emits "No response to regenerate" and does not modify the buffer

### Requirement: Display-layer role distinction

The system SHALL apply a display layer that visually distinguishes user-block content from assistant-block content without modifying buffer text. The v1 implementation SHALL use either a face applied to the block body via font-lock or a `line-prefix` overlay per role. The display layer SHALL be toggleable via a mode-level command and SHALL have no effect on message construction, persistence, or block delimiter positions.

#### Scenario: Display layer is active by default
- **WHEN** a chat-mode buffer is freshly opened
- **THEN** user-block content and assistant-block content have visually distinct presentation
- **AND** the underlying buffer text is unchanged (e.g., `buffer-substring-no-properties` matches the on-disk file)

#### Scenario: Display layer can be toggled off
- **WHEN** the user runs the display-toggle command
- **THEN** the visual distinction is removed
- **AND** block delimiters and content remain unchanged

### Requirement: gptel-request backend usage

The system SHALL issue requests to the LLM exclusively via `gptel-request`, passing a constructed `:prompt` message list and a streaming `:callback`. The system SHALL NOT invoke `gptel--parse-buffer`, rely on `gptel-prompt-prefix-alist` / `gptel-response-prefix-alist`, or mutate `:GPTEL_BOUNDS:` property drawers.

#### Scenario: Send uses gptel-request with an explicit prompt
- **WHEN** `gptel-chat-send` is invoked
- **THEN** `gptel-request` is called with a `:prompt` argument containing the constructed message list
- **AND** no buffer-parsing calls to `gptel--parse-buffer` are made during send

### Requirement: Preset system integration

The system SHALL integrate with upstream gptel's preset mechanism. On mode activation, the mode SHALL detect a preset declaration in one of two places (in order of precedence):

1. A `:GPTEL_PRESET: <name>` entry in an Org `:PROPERTIES:` drawer at point-min
2. A file-local `gptel--preset: <name>` variable (via `-*- ... -*-` header or `Local Variables:` block)

When a preset is found, the system SHALL call `gptel--apply-preset` with a buffer-local setter function, installing the preset's `:backend`, `:model`, `:system`, `:tools`, and other keys as buffer-local values. Subsequent `gptel-request` calls in that buffer use the applied values.

When no preset is declared, the mode SHALL take no preset-related action; the buffer inherits whatever global or dir-local configuration is in effect.

The system SHALL NOT enable `gptel-mode` (minor mode) as part of preset application. Preset application is buffer-local and does not alter the major mode.

#### Scenario: Preset applied from Org property drawer
- **WHEN** a buffer opens with `:PROPERTIES:\n:GPTEL_PRESET: coding\n:END:` at point-min
- **AND** the mode is activated
- **THEN** `gptel--apply-preset` is called with the symbol `coding`
- **AND** the preset's `:model`, `:backend`, `:tools` etc. are set as buffer-local values

#### Scenario: Preset applied from file-local variable
- **WHEN** a buffer opens with `# -*- gptel--preset: coding -*-` on line 1
- **AND** the mode is activated
- **THEN** `gptel--apply-preset` is called with the symbol `coding`

#### Scenario: No preset declared
- **WHEN** a buffer is activated with no preset property or file-local
- **THEN** no `gptel--apply-preset` call is made
- **AND** `gptel--preset` remains at its inherited value (global or nil)

#### Scenario: Property drawer wins over file-local
- **WHEN** both a `GPTEL_PRESET` property drawer and a file-local `gptel--preset` are present and name different presets
- **THEN** the property drawer value is applied

### Requirement: gptel-menu integration with rebound Send

The system SHALL support `M-x gptel-menu` for interactive per-buffer configuration (preset selection, model, backend, tools, system message, context). Configuration suffixes SHALL function unchanged from upstream `gptel-menu`.

The system SHALL provide `gptel-chat-menu` — a transient prefix mirroring `gptel-menu`'s configuration layout but with the Send suffix replaced by one that invokes `gptel-chat-send`. The chat-mode keymap SHALL bind this prefix to the same key that upstream users associate with `gptel-menu`.

Upstream `M-x gptel-menu` invoked directly (by key or by `M-x`) SHALL remain available and unmodified; the rebinding affects only the chat-mode keymap's binding.

#### Scenario: Menu configuration works in chat-mode buffer
- **WHEN** point is in a `gptel-chat-mode` buffer and the user invokes `M-x gptel-menu`
- **THEN** the menu appears with the upstream layout
- **AND** configuration actions (preset pick, model change, tool selection) mutate buffer-local variables as upstream does

#### Scenario: chat-mode menu Send invokes gptel-chat-send
- **WHEN** the user invokes `gptel-chat-menu` via its chat-mode keybinding
- **AND** presses Send
- **THEN** `gptel-chat-send` is called (not `gptel--suffix-send`)
- **AND** the response streams into a `#+begin_assistant` block

#### Scenario: chat-mode menu omits Send-coupled groups
- **WHEN** the user invokes `gptel-chat-menu` in a `gptel-chat-mode` buffer
- **THEN** the prefix layout shows configuration groups (system-prompt, context, tools, request-parameters), logging, and Send
- **AND** Prompt-from, Response-to, and Dry-Run groups are not present in the layout

### Requirement: Session-file auto-initialization

When a chat-mode buffer visits a file whose absolute path matches `*/branches/<branch>/session.org` or `*/agents/<agent-name>/session.org` (the session directory layout defined in `sessions-persistence`), the session auto-initialization hook SHALL:

1. Extract `session-id` and `branch-name` from the path
2. Set the five buffer-local session variables (`jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`, and `jf/gptel--parent-session-id` when applicable)
3. Register the buffer in `jf/gptel--session-registry` keyed `"<session-id>/<branch-name>"`
4. Read `metadata.yml` from the branch directory and apply its `preset` via `gptel--apply-preset` with a buffer-local setter
5. Ensure `gptel-chat-mode` is the active major mode (no-op if already active)
6. Update the `current` symlink to point at this branch

The hook SHALL NOT enable `gptel-mode` (minor mode), SHALL NOT invoke `gptel--save-state`, and SHALL NOT invoke `gptel--restore-state`.

#### Scenario: Opening a session.org file activates chat-mode and registers the session
- **WHEN** the user opens `~/.gptel/sessions/foo-20260420000000/branches/main/session.org`
- **THEN** `gptel-chat-mode` is the active major mode
- **AND** the five session buffer-local vars are set
- **AND** the buffer is registered in `jf/gptel--session-registry`
- **AND** `gptel-mode` is NOT enabled as a minor mode

#### Scenario: Auto-init applies preset from metadata.yml
- **WHEN** a `session.org` auto-inits and `metadata.yml` contains `preset: coding`
- **THEN** `gptel--apply-preset` is called with `coding` and a buffer-local setter
- **AND** buffer-local `gptel-model`, `gptel-backend`, `gptel-tools`, etc. reflect the coding preset

#### Scenario: Path outside session layout does not auto-init session state
- **WHEN** a `.org` file at an unrelated path (e.g., `~/notes/chat.org`) is opened in chat-mode
- **THEN** the session auto-init hook does NOT fire
- **AND** no session vars are set
- **AND** no registry entry is created
- **AND** the buffer is still a fully functional chat-mode buffer (preset may still apply via Requirement: Preset system integration)

### Requirement: Session file format and persistence

Session files (those under `branches/<branch>/session.org` or `agents/<agent>/session.org`) SHALL use the chat-mode block format defined by this specification. Saving a session buffer SHALL use plain `save-buffer`; the file's on-disk content is the chat-mode format, nothing else.

The sessions subsystem SHALL NOT write `gptel--bounds` Local Variables, SHALL NOT append `gptel-mode`-style Local Variables blocks, and SHALL NOT invoke `gptel--save-state`.

The sessions subsystem MAY maintain sidecar metadata (`metadata.yml`, `scope.yml`, `branch-metadata.yml`) separately from the session file. Updating `metadata.yml`'s `:updated` timestamp on save is permitted; it affects metadata only, not the session file contents.

#### Scenario: Fresh session file is a chat-mode buffer
- **WHEN** `jf/gptel-persistent-session` creates a new session
- **THEN** `session.org` contains `#+begin_user\n\n#+end_user\n` (and only that)
- **AND** no Local Variables block is present

#### Scenario: Saving a session buffer does not add gptel-mode artifacts
- **WHEN** the user edits a session buffer and invokes `save-buffer`
- **THEN** the file on disk contains only chat-mode block structure
- **AND** no `:: Local Variables:` block is appended
- **AND** no `gptel--bounds` property is written
