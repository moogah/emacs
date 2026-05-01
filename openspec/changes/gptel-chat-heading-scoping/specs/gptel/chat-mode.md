## OVERVIEW

This delta narrows the "full org-mode editing experience" promise (Decision 13 of the original chat-mode design) for one specific feature: column-0 `*` lines inside chat blocks. The original promise was structurally unachievable — an `*` at column 0 inside a special block destroys the block in `org-element-parse-buffer` (the AST loses the special-block entirely; the heading absorbs subsequent content into its outline subtree). See `research.md` for the empirical investigation.

The fix mirrors the existing `,#+end_*` delimiter-collision escape: column-0 `*` lines inside chat-block bodies are escaped on the write path (streaming insertion, user typing, paste) and un-escaped on the send path (before content reaches the LLM). The escape is symmetric and round-trip transparent for message content. The escape is visible in the buffer; rendering it as a heading visually is a separate concern (deferred).

The escape mechanism choice (leading-space vs `,*`) is documented in `design.md`. Default: leading-space, matching `org-edit-src-content-indentation`'s established pattern for src blocks.

## MODIFIED Requirements

### Requirement: Buffer format validation

The system SHALL accept any org document whose `#+begin_user`, `#+begin_assistant`, and `#+begin_tool` special blocks are structurally well-formed. Specifically:

- Every `#+begin_user` / `#+begin_assistant` / `#+begin_tool` SHALL have a matching `#+end_*` delimiter of the same type.
- `#+begin_tool` blocks SHALL appear only inside an `#+begin_assistant` block.
- `#+begin_user` and `#+begin_assistant` blocks SHALL NOT be nested inside another user or assistant block.
- Content outside turn blocks — org headings (at any depth), paragraphs, drawers, `#+keyword:` lines, blank lines — is permitted and is treated as human organization/commentary. It does NOT participate in message construction.
- Content inside turn-block bodies SHALL NOT contain `*` characters at column 0 of any line. Such content is automatically escaped on insertion (see Requirement: Heading-collision escape) and un-escaped on send. A buffer that contains unescaped column-0 `*` lines inside a chat block is invalid in the strict sense — `org-element-parse-buffer` loses the block — and SHALL be normalized on read (see Requirement: Migration of pre-escape session files).

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

#### Scenario: Heading-shaped lines inside turn body are escaped, not invalid
- **WHEN** a `#+begin_user` block body contains a line `* My heading`
- **AND** the buffer is in `gptel-chat-mode` (so the escape is applied on insert, on type, on paste, or on read-migration)
- **THEN** the on-disk content for that line is the escaped form (e.g., ` * My heading` with leading whitespace)
- **AND** `org-element-parse-buffer` parses the chat block as a well-formed `special-block`
- **AND** message construction emits the un-escaped form (`* My heading`) as part of the user message

### Requirement: Message construction from buffer

The system SHALL construct an ordered list of API messages by walking the buffer for **outer** `#+begin_user` and `#+begin_assistant` blocks in document order. An outer block is a user or assistant block that is not nested inside another user or assistant block; its position relative to org heading structure (at document root, or inside a heading section at any depth) is irrelevant. Each block produces messages as follows:

- `#+begin_user` block → one `user` message with the block's content (verbatim, delimiter lines excluded, escapes un-escaped)
- `#+begin_assistant` block → a sequence of messages: assistant-text segments and tool call / tool result pairs, in the order they appear inside the block
- Nested `#+begin_tool (<name> <plist...>)` block → one tool-call message (name + arguments plist) followed by one tool-result message (the block's remaining content)
- Delimiter-collision-escaped lines (`,#+end_user` / `,#+end_assistant` / `,#+end_tool`) SHALL be un-escaped before inclusion in the outbound message
- Heading-collision-escaped lines (column-0 `*` lines escaped per Requirement: Heading-collision escape) SHALL be un-escaped before inclusion in the outbound message

Content outside turn blocks (headlines, paragraphs, drawers, keywords) SHALL be skipped. Text that appears inside a user or assistant block, including lines that resemble `#+begin_*` delimiters or escaped headings, is treated as block body and included verbatim in the emitted message (subject to the escape/un-escape round-trip for the three closing delimiters and for `*` lines).

**Empty-turn contract.** Unchanged from the existing chat-mode spec.

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

#### Scenario: Heading escape round-trip
- **WHEN** an `#+begin_assistant` block body contains the escaped line ` * My Heading` (leading space)
- **THEN** the constructed assistant message contains the line `* My Heading` (leading space stripped)
- **AND** the LLM sees `* My Heading` in conversation history on subsequent requests

#### Scenario: Heading escape round-trip, multiple stars
- **WHEN** a chat block body contains escaped lines ` ** Sub-heading` and ` *** Sub-sub-heading`
- **THEN** the constructed message contains `** Sub-heading` and `*** Sub-sub-heading` (escape stripped from each)

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
- **WHEN** a `#+begin_user` block body contains a heading-shaped line (e.g., `* Heading`), a `#+begin_src` block, a list, and emphasized text
- **THEN** the heading-shaped line is stored on disk in escaped form (e.g., ` * Heading`)
- **AND** the constructed user message contains the un-escaped form `* Heading` as part of the prose
- **AND** the parser treats the whole block as a single user turn (the escaped `* Heading` does not partition it)
- **AND** the `#+begin_src` / `#+end_src` pair inside the user block does not interfere with the outer `#+end_user` match
- **AND** `org-element-parse-buffer` parses the user block as a well-formed `special-block`

### Requirement: Response streaming and sanitization

The system SHALL insert streamed response chunks between an open `#+begin_assistant` marker and its eventual `#+end_assistant`. Each chunk SHALL be scanned line-by-line and rewritten before insertion such that:

- Lines matching `^#\+end_\(user\|assistant\|tool\)\b` (case-insensitive) SHALL be rewritten to `,#+end_...` to prevent premature block closure.
- Lines matching `^\*+ ` SHALL be rewritten with the configured heading-collision escape (default: a single leading space, controlled by `gptel-chat-content-indentation`) to prevent the heading from destroying the enclosing chat block.

On stream completion, the `#+end_assistant` delimiter SHALL be inserted and point positioned after it. On stream abort or error, the block SHALL be closed with a visible error marker and `#+end_assistant` appended.

The two escape rules SHALL be independent: a chunk that contains both delimiter-collision lines and heading-collision lines SHALL have both escapes applied per line.

#### Scenario: Normal stream completion
- **WHEN** the model returns a multi-chunk response with no delimiter or heading collisions
- **THEN** all chunks appear in order inside the assistant block
- **AND** the block terminates with `#+end_assistant` on its own line
- **AND** the block is well-formed (parseable by `org-element`)

#### Scenario: Response contains collision with end_assistant
- **WHEN** a streamed response contains a line starting with `#+end_assistant`
- **THEN** that line is rewritten to start with `,#+end_assistant` when inserted
- **AND** the containing assistant block remains well-formed

#### Scenario: Response contains heading-shaped lines
- **WHEN** a streamed response contains lines `* Heading One`, `** Sub`, and `*** Deep`
- **THEN** each line is rewritten with the heading-collision escape on insertion (e.g., ` * Heading One`, ` ** Sub`, ` *** Deep`)
- **AND** the containing assistant block remains a well-formed `special-block` per `org-element`
- **AND** the host buffer's outline structure is unaffected by the assistant content

#### Scenario: Response contains heading collision split across chunks
- **WHEN** one chunk ends with `\n* He` and the next begins with `ading\nmore`
- **THEN** the per-line holdback completes the line as `* Heading` and applies the escape before insertion
- **AND** the inserted text is the escaped form

#### Scenario: Response contains both delimiter and heading collisions in one chunk
- **WHEN** a chunk contains `#+end_assistant` on one line and `* Heading` on another
- **THEN** the first line gets the comma escape and the second line gets the heading escape
- **AND** both are inserted in the correct order

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

## ADDED Requirements

### Requirement: Heading-collision escape

The system SHALL maintain an invariant that no line inside a chat-block body (the content between `#+begin_user` / `#+begin_assistant` / `#+begin_tool` and its matching `#+end_*`) begins with `*` at column 0. This invariant is enforced at every write path:

- **Streaming insertion** — see Requirement: Response streaming and sanitization.
- **User typing** — when the user types `*` at column 0 with point inside a chat-block body (and not on a delimiter line), the system SHALL apply the escape automatically. Implementation via `post-self-insert-hook`.
- **Paste / yank / programmatic insertion** — when text is inserted into a chat-block body that contains one or more `*` characters at column 0 of any line within the inserted text, the system SHALL apply the escape to those lines. Implementation via `after-change-functions`.
- **File read** — see Requirement: Migration of pre-escape session files.

The escape character(s) SHALL be controlled by the `gptel-chat-content-indentation` defcustom (default: a single space, matching `org-edit-src-content-indentation`). The escape is symmetric: the parser strips the same prefix on send (Requirement: Message construction from buffer).

Delimiter lines themselves (`#+begin_user`, `#+begin_assistant`, `#+begin_tool`, `#+end_user`, `#+end_assistant`, `#+end_tool`) SHALL remain at column 0; the escape applies only to body content.

#### Scenario: User types `*` at column 0 inside a user block
- **WHEN** point is at column 0 inside a `#+begin_user` block body and the user types `*`
- **THEN** the inserted character is preceded by the configured escape (e.g., a space)
- **AND** the resulting buffer text is ` *` (or the configured escape) at the start of that line
- **AND** point is positioned immediately after the inserted `*`

#### Scenario: User types `*` at column 0 outside any chat block
- **WHEN** point is at column 0 outside any chat block (e.g., on a free-form heading the user is writing as a section divider)
- **THEN** no escape is applied
- **AND** the `*` is inserted as a normal heading character

#### Scenario: User types `*` on a delimiter line
- **WHEN** point is on a `#+begin_user` or `#+end_user` line itself (not inside the body)
- **THEN** no escape is applied to that line
- **AND** the delimiter remains at column 0 as required for parser recognition

#### Scenario: Paste of multi-line text containing headings
- **WHEN** the user pastes a multi-line string `* H1\n- list\n** H2` at any position inside a chat-block body
- **THEN** the resulting buffer text has the escape applied to the `* H1` and `** H2` lines
- **AND** the `- list` line is unchanged
- **AND** the chat block remains a well-formed `special-block`

#### Scenario: Programmatic insert from streaming preserves invariant
- **WHEN** the streaming sanitizer inserts assistant content containing `*` lines into the assistant block
- **THEN** the resulting buffer text matches the invariant (no column-0 `*` inside the block body)

#### Scenario: Escape is independent of delimiter-collision escape
- **WHEN** a chat block body contains both an escaped delimiter line (`,#+end_assistant`) and an escaped heading line (` * Heading`)
- **THEN** message construction strips both escapes independently and produces clean message content (`#+end_assistant` and `* Heading`)

### Requirement: Migration of pre-escape session files

When `gptel-chat-mode` activates on a buffer whose chat blocks contain unescaped `*` lines at column 0 inside their bodies, the mode SHALL apply the heading-collision escape to those lines as part of activation. The migration is read-time, in-buffer (no destructive on-disk rewrite); the next `save-buffer` persists the normalized form.

The migration SHALL only modify content inside chat-block bodies. Headings outside chat blocks (free-form section dividers, etc.) SHALL NOT be modified.

The migration SHALL NOT mark the buffer as modified solely from migration. The user's first edit (or `save-buffer` while migration applied changes) is what triggers the on-disk rewrite. Implementation note: the migration applies changes and then calls `set-buffer-modified-p t` IF and only IF migration actually changed the buffer; otherwise the buffer remains unmodified.

#### Scenario: Open a session file with unescaped headings inside a chat block
- **WHEN** a chat-mode buffer activates on a file whose `#+begin_assistant` block body contains `* My Heading` at column 0
- **THEN** the buffer text is updated so that line reads ` * My Heading` (with the configured escape)
- **AND** the buffer is marked as modified
- **AND** the next `save-buffer` persists the escaped form

#### Scenario: Open a session file with no unescaped headings
- **WHEN** a chat-mode buffer activates on a file whose chat blocks have no column-0 `*` lines
- **THEN** no migration is applied
- **AND** the buffer is not marked as modified

#### Scenario: Open a session file with column-0 headings outside chat blocks
- **WHEN** a chat-mode buffer activates on a file whose top-level content includes `* Section A` (outside any chat block) and whose chat blocks have no column-0 `*` lines
- **THEN** no migration is applied
- **AND** the buffer is not marked as modified
- **AND** `* Section A` remains a top-level document heading
