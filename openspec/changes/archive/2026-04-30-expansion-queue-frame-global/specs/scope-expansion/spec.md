## MODIFIED Requirements

### Requirement: Expansion queue serialization

When multiple scoped tools fire in parallel and more than one is denied, the expansion UI SHALL serialize prompts so no callback is lost. Serialization is **frame-global**: the queue and active flag live as `defvar` (not `defvar-local`) so a flip-set in one buffer is observed and cleared from any buffer. Per-entry `:chat-buffer` continues to route drawer writes (via `jf/gptel-scope--current-chat-buffer` and the writer pipeline) to the session that owns each pending prompt.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Expansion Queue, §Entry Point

#### Scenario: First call shows, later calls queue

- **WHEN** `jf/gptel-scope-prompt-expansion` is called while `jf/gptel-scope--expansion-active` is nil
- **THEN** it sets the active flag and opens the transient immediately
- **AND** subsequent calls while active is set append their `{:violation :callback :patterns :tool-name :chat-buffer}` plist to `jf/gptel-scope--expansion-queue` instead of opening a second transient

#### Scenario: Each resolution pops the next queued prompt

- **WHEN** the user resolves the active prompt
- **THEN** the suffix action funcalls its callback and then calls `jf/gptel-scope--process-expansion-queue`
- **AND** if the queue has entries, the head is popped and a new transient is opened with that scope
- **AND** if the queue is empty, the active flag is cleared

#### Scenario: Queue and active flag are frame-global

- **WHEN** `jf/gptel-scope-prompt-expansion` is called from buffer A and the action handler later runs in buffer B (e.g. a PersistentAgent fires the prompt from its invisible `session.org` buffer while the user answers from the parent's overlay buffer)
- **THEN** the buffer-A flip-set of `jf/gptel-scope--expansion-active` is observed by `--process-expansion-queue` running in buffer B because both variables are declared with `defvar` (not `defvar-local`)
- **AND** a queue entry pushed from buffer A is drainable from buffer B without buffer-switching
- **AND** drawer writes for that entry still target buffer A's session because the entry's `:chat-buffer` plist key is consulted by `jf/gptel-scope--current-chat-buffer`, not `(current-buffer)`

#### Scenario: Frame-modality enforces single in-flight prompt

- **WHEN** any expansion prompt is currently shown via `transient-setup`
- **THEN** any further call to `jf/gptel-scope-prompt-expansion` (from any buffer, any session, any agent) appends to the global queue rather than opening a second transient
- **AND** the user always sees exactly one prompt at a time, in the order the underlying tool calls were authorized
