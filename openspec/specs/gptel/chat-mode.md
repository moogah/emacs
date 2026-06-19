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

The system SHALL define `gptel-chat-mode` as a major mode derived from `org-mode`. Activation SHALL be possible via `M-x gptel-chat-mode`, file-local mode cookies (`-*- gptel-chat -*-`), and **content-addressed recognition through `magic-mode-alist`**.

The system SHALL register one `magic-mode-alist` entry whose predicate (the *session signature*) recognizes a gptel session by its content, not by its filename or path:

- The predicate SHALL match a buffer whose first non-blank content at `point-min` is an Org `:PROPERTIES:` drawer that contains at least one property key whose name begins with `GPTEL_` (e.g. `:GPTEL_PRESET:`, `:GPTEL_SESSION_ID:`).
- The predicate SHALL be anchored to a *real* drawer at `point-min` (a `:PROPERTIES:` line followed, before its `:END:`, by a `:GPTEL_…:` property line). Prose that merely quotes the string `:GPTEL_PRESET:` inside body text SHALL NOT match.
- The predicate SHALL inspect only the buffer head (no whole-buffer scan) and SHALL be safe on non-org buffers (no parse error).

Because `set-auto-mode` consults `magic-mode-alist` before `auto-mode-alist`, a signature-bearing `.org` file SHALL open in `gptel-chat-mode` rather than the default `org-mode`. As `gptel-chat-mode` derives from `org-mode`, all org features remain available.

The system SHALL NOT register any `auto-mode-alist` entry of its own (activation is content-addressed, not filename-addressed). Users MAY still add their own `auto-mode-alist` pattern.

#### Scenario: Interactive activation
- **WHEN** running `M-x gptel-chat-mode` in a buffer
- **THEN** the major mode becomes `gptel-chat-mode`
- **AND** the buffer inherits `org-mode` features
- **AND** chat-mode keybindings are active

#### Scenario: File-local cookie activation
- **WHEN** opening a file with first line `# -*- gptel-chat -*-`
- **THEN** the buffer is in `gptel-chat-mode` after load

#### Scenario: Content-addressed activation by session signature
- **WHEN** any `.org` file is opened (via `find-file`, `find-file-noselect`, dired, recentf, or a bookmark) whose `point-min` `:PROPERTIES:` drawer carries a `:GPTEL_`-prefixed key
- **THEN** `magic-mode-alist` selects `gptel-chat-mode` as the major mode
- **AND** the selection wins over the default `.org → org-mode` mapping

#### Scenario: Signature does not false-match a quoting org file
- **WHEN** opening an ordinary org file that mentions the text `:GPTEL_PRESET:` only inside a paragraph or source block (not in a `point-min` `:PROPERTIES:` drawer)
- **THEN** the session signature predicate returns nil
- **AND** the file opens in ordinary `org-mode`

#### Scenario: Old session without identity keys still activates
- **WHEN** opening a pre-existing `session.org` whose drawer carries `:GPTEL_PRESET:` but no `:GPTEL_SESSION_ID:`
- **THEN** the signature still matches (any `:GPTEL_`-prefixed key qualifies)
- **AND** the buffer activates in `gptel-chat-mode`

### Requirement: Buffer format validation

The system SHALL accept any org document whose `#+begin_user`, `#+begin_assistant`, and `#+begin_tool` special blocks are structurally well-formed. Specifically:

- Every `#+begin_user` / `#+begin_assistant` / `#+begin_tool` SHALL have a matching `#+end_*` delimiter of the same type.
- `#+begin_tool` blocks SHALL appear only inside an `#+begin_assistant` block.
- `#+begin_user` and `#+begin_assistant` blocks SHALL NOT be nested inside another user or assistant block.
- Content outside turn blocks — org headings (at any depth), paragraphs, drawers, `#+keyword:` lines, blank lines — is permitted and is treated as human organization/commentary. It does NOT participate in message construction.
- Content inside turn-block bodies SHALL be stored indented: each body line is offset by `gptel-chat-content-indentation` spaces, so no body line begins with an org structural token (`*`, `#+end_*`, `#+begin_src`, a drawer, a keyword) at column 0. Such content is automatically indented on insertion (see Requirement: Chat-block body indentation) and dedented on send. A buffer that contains un-indented column-0 body content is invalid in the strict sense — `org-element-parse-buffer` may lose the block — and SHALL be normalized on read (see Requirement: Migration of pre-indentation session files).
- Delimiter lines themselves — `#+begin_user`, `#+begin_assistant`, `#+end_user`, `#+end_assistant`, and the nested `#+begin_tool` / `#+end_tool` — SHALL remain at column 0; only body content between delimiter lines is indented.

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

#### Scenario: Heading-shaped lines inside turn body are indented, not invalid
- **WHEN** a `#+begin_user` block body contains a line whose content is `* My heading`
- **AND** the buffer is in `gptel-chat-mode` (so indentation is applied on insert, on type, on paste, or on read-migration)
- **THEN** the on-disk content for that line is the indented form (e.g., `  * My heading`)
- **AND** `org-element-parse-buffer` parses the chat block as a well-formed `special-block`
- **AND** message construction emits the dedented form (`* My heading`) as part of the user message

### Requirement: Message construction from buffer

The system SHALL construct an ordered list of API messages by walking the buffer for **outer** `#+begin_user` and `#+begin_assistant` blocks in document order. An outer block is a user or assistant block that is not nested inside another user or assistant block; its position relative to org heading structure (at document root, or inside a heading section at any depth) is irrelevant. Each block produces messages as follows:

- `#+begin_user` block → one `user` message with the block's content (verbatim, delimiter lines excluded)
- `#+begin_assistant` block → a sequence of messages: assistant-text segments and tool call / tool result pairs, in the order they appear inside the block
- Nested `#+begin_tool (<name> <plist...>)` block → one tool-call message (name + arguments plist) followed by one tool-result message (the block's remaining content)
- The body indentation SHALL be removed before content is included in an outbound message: for each message segment, the common minimum leading indentation across its non-blank lines is measured and stripped (the `org-do-remove-indentation` model). The model never sees the indentation.

Content outside turn blocks (headlines, paragraphs, drawers, keywords) SHALL be skipped. Text that appears inside a user or assistant block, including lines that resemble `#+begin_*` delimiters or org headings, is treated as block body and included in the emitted message after dedenting.

**Empty-turn contract.** A turn is "empty" (and produces no outbound message) in exactly these cases:

- **User turn:** `:content` is `nil`, empty, or whitespace-only (zero non-whitespace characters — a block containing only newlines or spaces is still empty).
- **Assistant turn:** `:segments` is `nil` **OR** every segment in `:segments` is a `text` segment whose `:content` is `nil`, empty, or whitespace-only. A single `tool-call` segment is never empty — an assistant turn with at least one `tool-call` segment always emits at least one `(tool . PLIST)` message even if all surrounding text segments are blank.

This definition is what `gptel-chat--turn-to-messages` enforces. An empty assistant block (`#+begin_assistant\n#+end_assistant\n`) is parsed as an assistant turn with `:segments nil`; an assistant block containing only whitespace (`#+begin_assistant\n   \n#+end_assistant\n`) is parsed as an assistant turn with one or more whitespace-only `text` segments. Both shapes are skipped.

Message construction SHALL NOT depend on text properties or on `gptel--parse-buffer`.

#### Scenario: Single user-assistant turn
- **WHEN** the buffer contains one `#+begin_user`/`#+end_user` block followed by one `#+begin_assistant`/`#+end_assistant` block
- **THEN** the constructed message list is `[user: <user content>, assistant: <assistant content>]`
- **AND** each message's content has the body indentation removed

#### Scenario: Assistant turn with one tool call
- **WHEN** an `#+begin_assistant` block contains prose, then a `#+begin_tool` block with call args and result, then more prose
- **THEN** the message list for that turn is `[assistant: <pre-tool prose>, tool_call: <args>, tool_result: <result>, assistant: <post-tool prose>]`
- **AND** each text segment and the tool result are dedented independently (the column-0 `#+begin_tool` / `#+end_tool` lines do not affect any segment's measured indent)

#### Scenario: Body indentation round-trip
- **WHEN** an `#+begin_assistant` block body contains the indented line `  * My Heading`
- **THEN** the constructed assistant message contains the line `* My Heading` (indentation stripped)
- **AND** the model sees `* My Heading` in conversation history on subsequent requests

#### Scenario: Round-trip preserves intentional inner indentation
- **WHEN** a chat block body contains, indented at the body width, a nested list or code whose lines carry additional indentation relative to the body
- **THEN** message construction strips only the common body indentation
- **AND** the relative indentation of the nested content is preserved in the outbound message

#### Scenario: Round-trip is robust to a changed indentation width
- **WHEN** a session was written with `gptel-chat-content-indentation` = 2 and is re-opened with the value 4
- **THEN** message construction still produces correctly dedented content (the dedent measures actual indentation rather than assuming the current width)

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
- **WHEN** a `#+begin_user` block body contains, indented, a line whose content is `#+begin_assistant`
- **THEN** the constructed user message includes that line as body content (dedented)
- **AND** no additional assistant message is emitted for that line

#### Scenario: Turns distributed across org headings
- **WHEN** the buffer contains one turn pair under `* Section A` and one user turn under `* Section B`
- **THEN** the constructed message list is `[user_A, assistant_A, user_B]` in document order
- **AND** the headings themselves are not represented in the message list

#### Scenario: User prompt composed with org structural features
- **WHEN** a `#+begin_user` block body contains a heading-shaped line (`* Heading`), a `#+begin_src` block, a list, and emphasized text
- **THEN** every body line is stored on disk indented by `gptel-chat-content-indentation`
- **AND** the constructed user message contains the dedented content, including `* Heading` at column 0 of its line
- **AND** the parser treats the whole block as a single user turn
- **AND** `org-element-parse-buffer` parses the user block as a well-formed `special-block`

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

The system SHALL insert streamed response chunks between an open `#+begin_assistant` marker and its eventual `#+end_assistant`. Each chunk SHALL be scanned line-by-line, and each complete line SHALL be indented by `gptel-chat-content-indentation` spaces before insertion (blank lines are left empty). This keeps every line of streamed assistant content off column 0, so a model-emitted org heading or a model-emitted `#+end_*` delimiter line is body content, not document structure.

A nested `#+begin_tool` / `#+end_tool` block opened during streaming SHALL place its delimiter lines at column 0; the tool result content between them SHALL be indented like other body content.

On stream completion, the `#+end_assistant` delimiter SHALL be inserted at column 0 and point positioned after it. On stream abort or error, the block SHALL be closed with a visible error marker and `#+end_assistant` appended.

#### Scenario: Normal stream completion
- **WHEN** the model returns a multi-chunk response
- **THEN** all chunks appear in order inside the assistant block, each line indented by the body width
- **AND** the block terminates with `#+end_assistant` at column 0 on its own line
- **AND** the block is well-formed (parseable by `org-element`)

#### Scenario: Response contains heading-shaped lines
- **WHEN** a streamed response contains lines `* Heading One`, `** Sub`, and `*** Deep`
- **THEN** each line is inserted indented by the body width (e.g., `  * Heading One`)
- **AND** the containing assistant block remains a well-formed `special-block` per `org-element`
- **AND** the host buffer's outline structure is unaffected by the assistant content

#### Scenario: Response contains an end-delimiter-shaped line
- **WHEN** a streamed response contains a line whose content is `#+end_assistant`
- **THEN** that line is inserted indented by the body width
- **AND** the indented line is not recognized as a block closer; the containing assistant block remains well-formed

#### Scenario: Response contains a line split across chunks
- **WHEN** one chunk ends with `\n* He` and the next begins with `ading\nmore`
- **THEN** the per-line holdback completes the line as `* Heading` and indents it before insertion
- **AND** the inserted text is the indented form

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
- **THEN** the holdback is flushed, `#+end_assistant` is appended at column 0, and an empty `#+begin_user`/`#+end_user` block is started with point positioned for the next human turn

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

#### Scenario: Auto-approved tool surfaces only as a tool-result event
- **WHEN** the model emits a tool call for an auto-approved tool (one that does NOT go through the upstream `:confirm` path)
- **AND** upstream fires exactly one `(tool-result . ((TOOL-STRUCT ARGS RESULT)))` event with no preceding `(tool-call . ...)` event
- **THEN** the assistant block contains one `#+begin_tool`/`#+end_tool` block rendered from the tool-result 3-list's TOOL-STRUCT and ARGS, with the result as the body
- **AND** subsequent streamed prose lands at the assistant-level marker (not inside the closed tool block)

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

After the preset is applied, the system SHALL overlay every drawer-present configuration property as buffer-local bindings (Requirement: Configuration drawer overlay on restore). The drawer wins over the preset for every key it carries. For keys the drawer does not carry, the preset's value remains in effect.

For `:system` the preset is the lowest tier only. After preset application and the drawer overlay, the system SHALL read `:GPTEL_SYSTEM_PROMPT_FILE:` (Requirement: System prompt sibling file is authoritative); when that property resolves to a readable file with non-empty contents, the file body supersedes the preset's `:system`. The preset's `:system` is used as the buffer's system prompt only for sessions whose sibling file is absent or empty and whose drawer carries no legacy `:GPTEL_SYSTEM:` entry.

When no preset is declared, the mode SHALL take no preset-related action; the buffer inherits whatever global or dir-local configuration is in effect. A drawer that contains only non-preset keys (e.g., a `:GPTEL_MODEL:` and nothing else) SHALL still trigger the overlay.

The system SHALL NOT enable `gptel-mode` (minor mode) as part of preset application. Preset application is buffer-local and does not alter the major mode.

#### Scenario: Preset applied from Org property drawer
- **WHEN** a buffer opens with `:PROPERTIES:\n:GPTEL_PRESET: coding\n:END:` at point-min
- **AND** the mode is activated
- **THEN** `gptel--apply-preset` is called with the symbol `coding`
- **AND** the preset's `:model`, `:backend`, `:tools`, `:system` etc. are set as buffer-local values

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

#### Scenario: Drawer overlay wins over preset for shared keys
- **WHEN** a buffer opens with `:GPTEL_PRESET: coding` AND `:GPTEL_MODEL: claude-haiku-4-5` in the drawer
- **AND** the `coding` preset resolves `:model` to `claude-sonnet-4-6`
- **AND** the mode is activated
- **THEN** `gptel--apply-preset` is called with `coding` first
- **THEN** `gptel-model` is then overlaid to `claude-haiku-4-5` from the drawer

#### Scenario: Preset's :system is the bottom of the three-tier precedence
- **WHEN** a buffer opens with `:GPTEL_PRESET: coding`, no `:GPTEL_SYSTEM:` in the drawer, and no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **AND** the `coding` preset resolves `:system` to `"You are a coding assistant."`
- **AND** the mode is activated
- **THEN** buffer-local `gptel--system-message` is `"You are a coding assistant."`

### Requirement: gptel-menu integration with rebound Send

The system SHALL support `M-x gptel-menu` for interactive per-buffer configuration (preset selection, model, backend, tools, system message, context). Configuration suffixes SHALL function unchanged from upstream `gptel-menu`.

The system SHALL provide `gptel-chat-menu` — a transient prefix mirroring `gptel-menu`'s configuration layout but with the Send suffix replaced by one that invokes `gptel-chat-send`. The chat-mode keymap SHALL bind this prefix to the same key that upstream users associate with `gptel-menu`.

`gptel-chat-menu` SHALL replace the upstream `gptel-system-prompt` minibuffer-editing infix with an "Edit system prompt" affordance that opens the session's sibling system-prompt file in another window (Requirement: Edit system prompt menu affordance opens the sibling file) — the file is the source of truth, so the menu routes the user to the file rather than offering an in-menu text field. The menu's Send command SHALL route through the chat-mode request submission path, which performs the pre-send sibling-file refresh (Requirement: System prompt sibling file is authoritative).

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
- **AND** Rewrite and Tweak-Response groups are not present in the layout (their underlying predicates depend on gptel-mode response-insertion artifacts chat-mode does not produce)

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

### Requirement: Session file format and persistence

Session files (those under `branches/<branch>/session.org` or `agents/<agent>/session.org`) SHALL use the chat-mode block format defined by this specification. Saving a session buffer SHALL use plain `save-buffer`; the file's on-disk content is the chat-mode block structure plus a `:PROPERTIES:` configuration drawer at point-min written by the chat-mode save hook (Requirement: Configuration drawer save on buffer save).

The drawer carries a full snapshot of the buffer's upstream-compatible configuration (`GPTEL_PRESET`, `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND`), the chat-mode extension `GPTEL_PARENT_SESSION_ID` (when set), and the scope keys `GPTEL_SCOPE_*` (managed by the scope subsystem). It carries `:GPTEL_SYSTEM_PROMPT_FILE:` when the session has a system prompt. It does NOT carry `:GPTEL_SYSTEM:` (the system prompt lives in a sibling file referenced by `:GPTEL_SYSTEM_PROMPT_FILE:` — Requirement: System prompt sibling file is authoritative) or `:GPTEL_BOUNDS:`.

The sessions subsystem SHALL NOT write `gptel--bounds` Local Variables, SHALL NOT append `gptel-mode`-style Local Variables blocks, and SHALL NOT invoke `gptel--save-state` or `gptel-org-set-properties`. The chat-mode save hook is the only writer of configuration properties.

The sessions subsystem SHALL NOT maintain `metadata.yml`. `scope.yml` is no longer used for per-session storage (replaced by drawer scope keys). `branch-metadata.yml` remains a separate sidecar for non-main branches.

#### Scenario: Fresh session file is a chat-mode buffer with full preset snapshot drawer
- **WHEN** `jf/gptel-persistent-session` creates a new session with preset `coding`
- **THEN** `session.org` contains a `:PROPERTIES:` drawer at point-min carrying `:GPTEL_PRESET: coding`, `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, and any other non-nil keys from the preset
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the file ends with `#+begin_user\n\n#+end_user\n`
- **AND** no Local Variables block is present

#### Scenario: Agent session.org has parent-session-id and full preset snapshot in drawer
- **WHEN** an agent session is created with preset `executor` under parent `p-abc-20260424000000`
- **THEN** the agent's `session.org` drawer contains `:GPTEL_PRESET: executor`, `:GPTEL_PARENT_SESSION_ID: p-abc-20260424000000`, and the `executor` preset's snapshot keys (`:GPTEL_MODEL:`, `:GPTEL_TOOLS:`, etc.)
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`

#### Scenario: Saving a session buffer writes full-snapshot drawer, no gptel-mode artifacts
- **WHEN** the user edits a session buffer and invokes `save-buffer`
- **THEN** the file on disk contains a `:PROPERTIES:` / `:END:` drawer at point-min carrying the full configuration snapshot (system excluded)
- **AND** the drawer contains no `:GPTEL_SYSTEM:` line
- **AND** the drawer contains no `:GPTEL_BOUNDS:` line
- **AND** no `:: Local Variables:` block is appended
- **AND** no `metadata.yml` exists alongside the session file

### Requirement: Configuration drawer save on buffer save

`gptel-chat-mode` SHALL install a buffer-local `before-save-hook` that writes a configuration `:PROPERTIES:` drawer at point-min before the buffer is written to disk. The drawer SHALL contain a **full snapshot** of the upstream-compatible configuration keys read from the buffer's current state — `GPTEL_PRESET`, `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND` — written via `org-entry-put` (and `org-entry-put-multivalued-property` for list-valued keys) with no delta-from-preset deletion. Each key is written when its source buffer-local variable is non-nil; nil-valued keys are deleted via `org-entry-delete` so the drawer never carries stale values from a prior save.

Additionally, the drawer SHALL contain `GPTEL_PARENT_SESSION_ID` when the buffer-local variable `jf/gptel--parent-session-id` is a non-empty string.

The save hook SHALL preserve `:GPTEL_SYSTEM_PROMPT_FILE:` when present — it is written by session creation (and by the "Edit system prompt" menu affordance) and read by the system-prompt restore step (Requirement: System prompt sibling file is authoritative). The save path does not generate or rewrite the property; it simply does not delete it.

The save hook SHALL NOT write a `:GPTEL_SYSTEM:` drawer line. Long, multi-line, special-character-heavy strings are unwieldy as a single property value, so the system prompt is never persisted as a drawer *property*. The system prompt lives in the sibling file referenced by `:GPTEL_SYSTEM_PROMPT_FILE:`, which is canonical and is not written by the save hook (Requirement: System prompt sibling file is authoritative). If a user authored `:GPTEL_SYSTEM:` in the drawer manually, the read-time overlay still respects it (back-compat); the writer simply never emits or replaces the drawer property.

The save hook SHALL NOT serialize the system prompt into a `* System Prompt` heading body. The heading-based authoritative-source contract is removed (see Requirement: System prompt sibling file is authoritative); the sibling file replaces it.

The save hook SHALL NOT write `:GPTEL_BOUNDS:`. `GPTEL_BOUNDS` is incompatible with chat-mode's block-based format and remains explicitly excluded.

The save hook SHALL NOT delegate to upstream `gptel-org-set-properties`. That function deletes properties whose buffer-local values match the active preset, defeating the WYSIWYG goal of this change. The chat-mode writer is independent and emits the full snapshot regardless of preset matching.

The save hook SHALL NOT enable `gptel-mode` (minor mode). Chat-mode owns the major-mode role exclusively.

#### Scenario: Drawer carries full preset snapshot on save
- **WHEN** a chat-mode buffer has the `coding` preset applied and `save-buffer` is invoked
- **AND** the buffer-local `gptel-tools`, `gptel-model`, `gptel-backend`, `gptel-temperature`, `gptel-max-tokens`, `gptel--num-messages-to-send` all match the preset's values exactly
- **THEN** the saved file contains a `:PROPERTIES:` / `:END:` drawer at point-min
- **AND** the drawer contains `:GPTEL_PRESET: coding`
- **AND** the drawer contains `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, and any other non-nil preset key
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the drawer does NOT contain `:GPTEL_BOUNDS:`

#### Scenario: Drawer carries user overrides on top of preset snapshot
- **WHEN** the `coding` preset is applied and the user adds one tool to `gptel-tools` via `gptel-chat-menu`
- **AND** `save-buffer` is invoked
- **THEN** the drawer contains `:GPTEL_PRESET: coding`
- **AND** the drawer contains a `:GPTEL_TOOLS:` line listing the preset's tools plus the one added tool

#### Scenario: Drawer carries parent-session-id for agent sessions
- **WHEN** the buffer is an agent session with `jf/gptel--parent-session-id` set to `"parent-abc-20260424000000"`
- **AND** `save-buffer` is invoked
- **THEN** the drawer contains `:GPTEL_PARENT_SESSION_ID: parent-abc-20260424000000`

#### Scenario: Drawer omits parent-session-id for non-agent buffers
- **WHEN** the buffer has no `jf/gptel--parent-session-id` (branch session or standalone chat buffer)
- **AND** `save-buffer` is invoked
- **THEN** the drawer does NOT contain a `:GPTEL_PARENT_SESSION_ID:` line

#### Scenario: GPTEL_SYSTEM never written by the save path
- **WHEN** any chat-mode buffer is saved, regardless of preset or buffer-local `gptel--system-message` content
- **THEN** the saved drawer contains no `:GPTEL_SYSTEM:` line

#### Scenario: Save preserves sibling-file drawer link
- **WHEN** a chat-mode buffer's drawer carries `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md` and `save-buffer` is invoked
- **THEN** the saved drawer still contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`

#### Scenario: Save does not emit `* System Prompt` heading
- **WHEN** a chat-mode buffer with no `* System Prompt` heading is saved
- **THEN** the saved file still contains no `* System Prompt` heading
- **AND** the saved file still contains no `* Chat` heading

#### Scenario: Save path never writes GPTEL_BOUNDS
- **WHEN** a chat-mode buffer contains any number of assistant blocks and `save-buffer` is invoked
- **THEN** the saved file contains no `:GPTEL_BOUNDS:` line under any circumstance

#### Scenario: Save with no preset still writes a snapshot of buffer-local config
- **WHEN** a fresh chat-mode buffer with no preset has the user set `gptel-model` to `claude-opus-4-7` buffer-locally
- **AND** `save-buffer` is invoked
- **THEN** the drawer contains `:GPTEL_MODEL: claude-opus-4-7`
- **AND** the drawer does NOT contain `:GPTEL_PRESET:`
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`

### Requirement: Configuration drawer overlay on restore

On `gptel-chat-mode` activation, after a declared preset has been applied (Requirement: Preset system integration), the mode-activation hook SHALL read the configuration drawer via `gptel-org--entry-properties` and overlay every drawer-present value as buffer-local bindings. The overlaid keys are `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_SYSTEM`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, and `GPTEL_NUM_MESSAGES_TO_SEND`.

The drawer wins over the preset for every key it carries — this is the WYSIWYG contract: what is in the drawer is what the buffer uses. Even when a drawer key matches the preset's value, the overlay still installs the buffer-local binding (a harmless no-op semantically; load-bearing for `gptel-org-set-properties`-incompatible test scenarios).

The mode-activation hook SHALL additionally read `GPTEL_PARENT_SESSION_ID` via `org-entry-get` and, when present, set `jf/gptel--parent-session-id` buffer-locally.

The overlay SHALL be applied with a buffer-local setter so overlaid values do not leak into other buffers.

When the drawer carries no entry for a given key, the buffer-local value remains whatever the preset installed (or whatever the global default is in the no-preset case).

For `:system` specifically, the drawer overlay is the back-compat middle tier. After the overlay runs, a separate restore step reads `:GPTEL_SYSTEM_PROMPT_FILE:` and, when set, installs the sibling file contents as `gptel--system-message` (Requirement: System prompt sibling file is authoritative) — superseding both a legacy `:GPTEL_SYSTEM:` drawer entry and the preset's `:system`. The restore precedence for the system prompt is therefore: sibling file > legacy `:GPTEL_SYSTEM:` drawer entry > preset `:system`. The overlay's role for `:system` is to honor a hand-authored `:GPTEL_SYSTEM:` drawer line for old sessions that have no sibling file.

When the buffer has no drawer at all, neither the preset nor the overlay path fires.

#### Scenario: Drawer overlay restores user tools after reopen
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_TOOLS: tool-a tool-b tool-c`
- **AND** the `coding` preset resolves `:tools` to `(tool-a tool-b)`
- **THEN** after mode activation, buffer-local `gptel-tools` contains `(tool-a tool-b tool-c)`
- **AND** buffer-local `gptel--preset` is `coding`

#### Scenario: Drawer model wins over preset model
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_MODEL: claude-haiku-4-5`
- **AND** the `coding` preset resolves `:model` to `claude-sonnet-4-6`
- **THEN** after mode activation, buffer-local `gptel-model` is `claude-haiku-4-5`

#### Scenario: System prompt comes from preset when drawer omits it and there is no sibling file
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and no `:GPTEL_SYSTEM:` line
- **AND** the buffer has no `:GPTEL_SYSTEM_PROMPT_FILE:` line (or its referenced file is absent)
- **AND** the `coding` preset resolves `:system` to `"You are a coding assistant."`
- **THEN** after mode activation, buffer-local `gptel--system-message` is `"You are a coding assistant."`

#### Scenario: Drawer-authored system prompt still respected on restore when there is no sibling file
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PRESET: coding` and `:GPTEL_SYSTEM: Custom override prompt.`
- **AND** the buffer has no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **THEN** after mode activation, buffer-local `gptel--system-message` is `"Custom override prompt."`
- **NOTE**: the next save will NOT re-write `:GPTEL_SYSTEM:` (writer never emits it); the legacy property sits in the file untouched.

#### Scenario: Drawer overlay restores parent-session-id for agent sessions
- **WHEN** a chat-mode buffer is opened whose drawer contains `:GPTEL_PARENT_SESSION_ID: parent-abc-20260424000000`
- **THEN** after mode activation, `jf/gptel--parent-session-id` is `"parent-abc-20260424000000"` buffer-locally

#### Scenario: Drawer overlay does not leak globally
- **WHEN** a chat-mode buffer with a drawer overlay finishes activation
- **THEN** the overlaid keys (`gptel-tools`, `gptel-model`, `gptel-backend`, `jf/gptel--parent-session-id`, ...) are buffer-local
- **AND** their global (default) values are unchanged

#### Scenario: No drawer at all triggers no overlay
- **WHEN** a chat-mode buffer with no `:PROPERTIES:` drawer and no file-local `gptel--preset` is activated
- **THEN** no preset is applied and no overlay runs
- **AND** the buffer inherits global or dir-local configuration unchanged

### Requirement: System prompt sibling file is authoritative

The buffer-local `gptel--system-message` for a chat-mode session SHALL be sourced from a sibling file in the session's branch directory, referenced from the configuration drawer via a `:GPTEL_SYSTEM_PROMPT_FILE:` property. The system prompt is carried as a separate file — outside `session.org` entirely — so its content (typically markdown with code, XML-like tags, and other special characters) does not interact with org-mode parsing, fontification, or folding.

**Drawer link.** When session creation persists a system prompt, it SHALL emit `:GPTEL_SYSTEM_PROMPT_FILE:` into the configuration drawer carrying a path. The path is typically a basename (e.g. `system-prompt.md`) resolved relative to `(file-name-directory buffer-file-name)`. The property MAY carry an absolute path (or one with directory components) and the resolver SHALL honor it. The save path SHALL NOT delete this property.

**Restore.** On `gptel-chat-mode` activation, after preset application and the drawer overlay have run, the mode SHALL read `:GPTEL_SYSTEM_PROMPT_FILE:` from the configuration drawer, resolve it relative to the session.org file's directory, and — when the resolved file exists and is non-empty — install its contents as the buffer-local `gptel--system-message`. The body SHALL be read verbatim (no trimming, no transformation) so prompts authored with intentional leading/trailing whitespace survive the round trip.

**Pre-send refresh.** Before dispatching a chat request, the chat-mode request submission path SHALL re-read the sibling file (when `:GPTEL_SYSTEM_PROMPT_FILE:` is set and the file exists) and refresh `gptel--system-message` from its current on-disk contents. The buffer-local value functions as a per-request cache; the file is the source of truth. A user edit to the sibling file is picked up on the next request without explicitly reopening `session.org`.

**Restore precedence.** With the heading tier removed, the system-prompt restore precedence is:
1. `:GPTEL_SYSTEM_PROMPT_FILE:` sibling file contents (when the property is set and the file exists and is non-empty)
2. legacy `:GPTEL_SYSTEM:` drawer entry (when present and the property/file above is absent — back-compat)
3. preset `:system`

**Save.** The save path SHALL NOT write the sibling file. The sibling file is canonical and is mutated only by the user (typically through the "Edit system prompt" menu affordance, which opens the file in another window). On `before-save-hook`, the chat-mode save path SHALL continue to never emit a `:GPTEL_SYSTEM:` drawer line (the existing write-exclusion stands); the sibling file is independent of the save hook.

**Missing file / missing property.** When `:GPTEL_SYSTEM_PROMPT_FILE:` is unset, or set to a path that does not resolve to a readable file, the restore step SHALL be a no-op — whatever the preset / drawer overlay already installed remains in effect. The mode SHALL NOT signal an error or warning for a missing sibling file (the session may legitimately have no system prompt; or the property may reference a path the user has not yet created).

#### Scenario: Restore reads sibling file when property and file are present
- **WHEN** a chat-mode buffer opens whose drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** the file `system-prompt.md` in the same directory as `session.org` exists with content `"Custom prompt from the file."`
- **AND** `:GPTEL_PRESET: coding` resolves `:system` to `"Preset text."`
- **THEN** `gptel--system-message` is `"Custom prompt from the file."`

#### Scenario: Restore falls back to preset when sibling file is absent
- **WHEN** a chat-mode buffer opens whose drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** no file named `system-prompt.md` exists in the session's branch directory
- **AND** `:GPTEL_PRESET: coding` resolves `:system` to `"Preset text."`
- **THEN** `gptel--system-message` is `"Preset text."`
- **AND** no error or warning is signaled

#### Scenario: Restore falls back to preset when property is unset
- **WHEN** a chat-mode buffer opens whose drawer carries no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **AND** `:GPTEL_PRESET: coding` resolves `:system` to `"Preset text."`
- **THEN** `gptel--system-message` is `"Preset text."`

#### Scenario: Restore respects legacy drawer entry when no sibling file
- **WHEN** a chat-mode buffer opens whose drawer contains `:GPTEL_SYSTEM: Legacy drawer prompt.` and no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **THEN** `gptel--system-message` is `"Legacy drawer prompt."` (legacy back-compat tier)

#### Scenario: Sibling file wins over legacy drawer entry
- **WHEN** a chat-mode buffer opens whose drawer contains both `:GPTEL_SYSTEM: Legacy.` and `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** the file `system-prompt.md` exists with content `"File wins."`
- **THEN** `gptel--system-message` is `"File wins."`

#### Scenario: Empty sibling file is a no-op
- **WHEN** a chat-mode buffer opens whose drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** the file `system-prompt.md` exists but is empty (or whitespace-only)
- **AND** `:GPTEL_PRESET: coding` resolves `:system` to `"Preset text."`
- **THEN** `gptel--system-message` is `"Preset text."` (empty file falls through, does not wipe the prompt)

#### Scenario: Pre-send refresh picks up file edit
- **WHEN** a chat-mode buffer is loaded with `gptel--system-message` cached as `"Old prompt."`
- **AND** the sibling file referenced by `:GPTEL_SYSTEM_PROMPT_FILE:` is modified on disk to `"New prompt."`
- **AND** the user submits a chat request from the buffer (without reopening or reverting `session.org`)
- **THEN** the request is dispatched with `gptel--system-message` equal to `"New prompt."`

#### Scenario: Save does not write the sibling file
- **WHEN** a chat-mode buffer has `gptel--system-message` set to `"In-buffer override."` (e.g., set programmatically) and `save-buffer` is invoked
- **AND** the sibling file on disk contains `"On-disk prompt."`
- **THEN** the sibling file on disk is unchanged — still `"On-disk prompt."`
- **AND** the saved drawer contains no `:GPTEL_SYSTEM:` line

### Requirement: Edit system prompt menu affordance opens the sibling file

The chat-mode menu SHALL provide an "Edit system prompt" affordance that opens the session's sibling system-prompt file in another window via `find-file-other-window`. This affordance replaces the upstream `gptel-system-prompt` minibuffer-editing infix in the chat-mode menu — the file is the source of truth for the system prompt, so the menu routes the user to the file rather than offering an in-menu text field.

When the chat-mode buffer's drawer carries `:GPTEL_SYSTEM_PROMPT_FILE:`, the affordance SHALL resolve and open that file. When the property is unset (e.g., the session was created from a preset with no `:system`), the affordance SHALL prompt the user for a filename (defaulting to `system-prompt.md`), write the property into the drawer, create the file (empty), save the session buffer, and open the new file.

Upstream `gptel-system-prompt` (invoked outside the chat-mode menu) is unchanged.

#### Scenario: Edit opens the resolved sibling file
- **WHEN** a chat-mode buffer's drawer carries `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
- **AND** the file `system-prompt.md` exists in the session's branch directory
- **AND** the user invokes the "Edit system prompt" menu entry
- **THEN** `system-prompt.md` is opened in another window via `find-file-other-window`

#### Scenario: Edit creates and opens a file when property is unset
- **WHEN** a chat-mode buffer's drawer carries no `:GPTEL_SYSTEM_PROMPT_FILE:` line
- **AND** the user invokes the "Edit system prompt" menu entry and confirms the default filename
- **THEN** `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md` is added to the configuration drawer
- **AND** a new empty `system-prompt.md` is created in the session's branch directory
- **AND** the new file is opened in another window

### Requirement: Chat-menu defaults configuration scope to buffer-local

`gptel-chat-menu` SHALL set `gptel--set-buffer-locally` to `t` for the lifetime of the menu invocation. This causes the upstream tool / model / temperature / system / etc. infixes (which read this variable to decide whether `gptel--set-with-scope` should set globally, buffer-locally, or oneshot) to write buffer-locally by default when invoked from the chat menu.

The buffer-local default applies only to `gptel-chat-menu`. Upstream `gptel-menu` (invoked directly via `M-x gptel-menu` or its upstream binding) is unchanged and retains its global default.

The user MAY still override per-invocation via the menu's scope toggle (the upstream `gptel--infix-variable-scope`), cycling through global / buffer-local / oneshot.

#### Scenario: Tool toggle from chat menu writes buffer-locally
- **WHEN** the user invokes `gptel-chat-menu` in a chat-mode buffer and toggles a tool via "Select tools" without changing the scope toggle
- **THEN** `gptel-tools` is set buffer-locally in the chat-mode buffer
- **AND** the global `gptel-tools` value is unchanged
- **AND** `default-value 'gptel-tools` is unchanged

#### Scenario: Upstream gptel-menu retains global default
- **WHEN** the user invokes `M-x gptel-menu` (not the chat-menu binding) in any buffer and toggles a tool
- **THEN** the upstream global behaviour is unchanged

#### Scenario: Buffer-local tool change persists to drawer on save
- **WHEN** the user toggles a tool via `gptel-chat-menu` and then invokes `save-buffer`
- **THEN** the saved drawer contains a `:GPTEL_TOOLS:` line listing the new buffer-local tool list

### Requirement: Chat-block body indentation

The system SHALL maintain an invariant that every line of content inside a chat-block body — the content between `#+begin_user` / `#+begin_assistant` / `#+begin_tool` and its matching `#+end_*` — is indented by `gptel-chat-content-indentation` spaces, so no body line begins with an org structural token at column 0. The invariant is maintained at every write path:

- **Streaming insertion** — see Requirement: Response streaming and sanitization.
- **User typing** — a buffer-local `indent-line-function` (delimiter line → column 0; body line → the body indent) together with electric-indent causes new body lines to start at the body indent. Implementation does NOT use a per-keystroke hook.
- **Paste / yank / programmatic insertion** — when a region is inserted inside a chat-block body, the system SHALL shift the inserted region so its minimum-indented line lands at the body indent (`shift = max(0, body-indent − region-minimum-indent)`). The shift preserves the region's relative indentation and is idempotent. Implementation: an `after-change-functions` recorder captures the inserted region and a one-shot `post-command-hook` performs the shift after the inserting command returns, outside the change dispatch.
- **File read** — see Requirement: Migration of pre-indentation session files.

The indentation width SHALL be controlled by the `gptel-chat-content-indentation` defcustom (default 2, matching `org-edit-src-content-indentation`; runtime floor clamped to 1). The indentation is symmetric by intent: the parser strips each segment's common leading indentation on send (Requirement: Message construction from buffer).

Delimiter lines — `#+begin_user`, `#+begin_assistant`, `#+begin_tool`, `#+end_user`, `#+end_assistant`, `#+end_tool` — SHALL remain at column 0. The nested `#+begin_tool` / `#+end_tool` lines, although they sit inside an assistant body, are delimiter lines and stay at column 0; the display layer MAY render them with a `line-prefix` so they appear aligned with the indented body.

#### Scenario: User types a heading-shaped line inside a user block
- **WHEN** point is inside a `#+begin_user` block body and the user opens a new line and types `* heading`
- **THEN** the new line starts at the body indent (electric-indent / `indent-line-function`)
- **AND** the resulting buffer line is `  * heading` (indented), not a column-0 heading

#### Scenario: User types outside any chat block
- **WHEN** point is at column 0 outside any chat block (e.g., on a free-form section-divider heading)
- **THEN** no body indentation is applied
- **AND** the `*` is inserted as a normal heading character

#### Scenario: Paste of multi-line text into a chat-block body
- **WHEN** the user pastes a multi-line string whose first line is `* H1` and which contains a nested list at deeper indentation, at any position inside a chat-block body
- **THEN** the inserted region is shifted so its least-indented line lands at the body indent
- **AND** the relative indentation of the nested list is preserved
- **AND** the chat block remains a well-formed `special-block`

#### Scenario: Paste of already-indented content is idempotent
- **WHEN** the user pastes a region whose minimum indentation already equals or exceeds the body indent
- **THEN** the region is inserted unchanged (shift is zero)

#### Scenario: Paste crossing a block boundary
- **WHEN** an inserted region straddles a `#+end_*` delimiter
- **THEN** only the portion inside the body is shifted; the delimiter line and content past it are left at column 0

#### Scenario: Streaming insertion maintains the invariant
- **WHEN** the streaming sanitizer inserts assistant content containing heading-shaped or delimiter-shaped lines
- **THEN** the resulting buffer text has every body line indented; no column-0 structural token appears inside the block body

#### Scenario: Tool delimiter lines stay at column 0
- **WHEN** an assistant block contains a nested `#+begin_tool` / `#+end_tool` block
- **THEN** the `#+begin_tool` and `#+end_tool` lines are at column 0 in the buffer and on disk
- **AND** the tool result content between them is indented by the body width
- **AND** the parser recognizes the tool block (its column-0 anchors are unaffected)

### Requirement: Migration of pre-indentation session files

When `gptel-chat-mode` activates on a buffer whose chat-block bodies are not in the indented form — pre-escape sessions (column-0 content), escape-era sessions (1-space `*` escapes and `,#+end_*` escapes), or content edited away from the indented form — the mode SHALL normalize each chat-block body to the indented form as part of activation. Per content-region (a run of lines between delimiter lines within a body), the migration SHALL first strip legacy escape-era artifacts (a leading `,` from `,#+end_*` lines, the old leading-whitespace prefix from `*` lines) and then shift the region so its minimum indent equals `gptel-chat-content-indentation`.

The migration is read-time and in-buffer; there is no destructive on-disk rewrite. The migration SHALL only modify content inside chat-block bodies; delimiter lines (including nested `#+begin_tool` / `#+end_tool`) and content outside chat blocks SHALL NOT be modified. The migration SHALL mark the buffer modified IF and only IF it changed the buffer; a buffer already in the indented form opens unmodified. The migration SHALL be idempotent.

#### Scenario: Open a pre-escape session with column-0 body content
- **WHEN** a chat-mode buffer activates on a file whose `#+begin_assistant` block body contains `* My Heading` at column 0
- **THEN** the buffer text is updated so that line is indented by the body width
- **AND** the buffer is marked as modified
- **AND** the next `save-buffer` persists the indented form

#### Scenario: Open an escape-era session
- **WHEN** a chat-mode buffer activates on a file whose chat-block bodies contain 1-space-escaped `*` lines and `,#+end_*` lines
- **THEN** the legacy escapes are stripped and the body is re-indented to the current body width
- **AND** message construction from the migrated buffer yields content identical (modulo the escape artifacts) to the pre-migration content

#### Scenario: Open a session already in the indented form
- **WHEN** a chat-mode buffer activates on a file whose chat-block bodies are already indented by the body width
- **THEN** no migration change is applied
- **AND** the buffer is not marked as modified

#### Scenario: Migration leaves content outside chat blocks alone
- **WHEN** a chat-mode buffer activates on a file whose top-level content includes `* Section A` outside any chat block
- **THEN** `* Section A` remains a column-0 top-level document heading
- **AND** only chat-block body content is indented

#### Scenario: Migration leaves tool delimiter lines at column 0
- **WHEN** a migrated assistant block contains a nested `#+begin_tool` / `#+end_tool` block
- **THEN** the `#+begin_tool` and `#+end_tool` lines remain at column 0
- **AND** the tool result content and the surrounding prose segments are indented
