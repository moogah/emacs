## OVERVIEW

This delta narrows the "full org-mode editing experience" promise (Decision 13 of the original chat-mode design) for one class of feature: **column-0 org structural tokens inside chat blocks**. The original promise was structurally unachievable — an `*` at column 0 inside a special block destroys the block in `org-element-parse-buffer`, and the same failure is latent for any column-0 structural token (`#+end_*`, `#+begin_src`, drawers, keywords). See `research.md` for the empirical investigation.

The fix makes a chat-block body an **indented region**: every body line is indented by `gptel-chat-content-indentation` spaces. Indentation moves all content off column 0, where org's structural scanners are anchored, so no body line is mistaken for document structure. The indentation is applied on the write path (streaming, typing, paste, read-migration) and stripped on the send path (before content reaches the model) by measuring and removing each segment's common leading indentation. The round-trip is transparent for message content and visible in the buffer.

The governing model is `column 0 = structure (delimiters)` · `indented = content`. `design.md` records the decisions; this delta supersedes the earlier "heading-collision escape" delta of the same change.

## MODIFIED Requirements

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

The system SHALL construct an ordered list of API messages by walking the buffer for **outer** `#+begin_user` and `#+begin_assistant` blocks in document order. An outer block is a user or assistant block that is not nested inside another user or assistant block; its position relative to org heading structure is irrelevant. Each block produces messages as follows:

- `#+begin_user` block → one `user` message with the block's content
- `#+begin_assistant` block → a sequence of messages: assistant-text segments and tool call / tool result pairs, in the order they appear inside the block
- Nested `#+begin_tool (<name> <plist...>)` block → one tool-call message (name + arguments plist) followed by one tool-result message (the block's remaining content)
- The body indentation SHALL be removed before content is included in an outbound message: for each message segment, the common minimum leading indentation across its non-blank lines is measured and stripped (the `org-do-remove-indentation` model). The model never sees the indentation.

Content outside turn blocks (headlines, paragraphs, drawers, keywords) SHALL be skipped. Text that appears inside a user or assistant block, including lines that resemble `#+begin_*` delimiters or org headings, is treated as block body and included in the emitted message after dedenting.

**Empty-turn contract.** Unchanged from the existing chat-mode spec.

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
- **WHEN** a `#+begin_assistant` block is completely empty (no body between delimiters)
- **THEN** no `assistant` message is emitted for that block

#### Scenario: Assistant block with tool call and blank prose is kept
- **WHEN** a `#+begin_assistant` block contains only a `#+begin_tool` block and surrounding whitespace
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
- **WHEN** the model completes the final round (no further tool use pending) and upstream fires `t` with `:tool-use` unset or absent
- **THEN** the holdback is flushed, `#+end_assistant` is appended at column 0, and an empty `#+begin_user`/`#+end_user` block is started with point positioned for the next human turn

## ADDED Requirements

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
