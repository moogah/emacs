## MODIFIED Requirements

### Requirement: Authorization entrypoint

`jf/gptel-scope-authorize-tool-call` SHALL be the single public authorization entrypoint. All scoped tool dispatches go through it; no other code path consults the validation engine.

The entrypoint captures `(current-buffer)` at entry as `origin-buffer` and re-establishes it via `with-current-buffer` around the wrapper-callback delivered to the expansion UI. Without this, the action handler's expansion-resolution synchronously calls back into the wrapper-callback in the buffer the user clicked from — typically the parent's overlay buffer for PersistentAgent flows, NOT the buffer that owns the pending tool call. `jf/gptel-scope--load-config` reads buffer-local `jf/gptel--branch-dir` to pick a session's drawer, and `jf/gptel-scope-prompt-expansion` captures `(current-buffer)` as the queued entry's `:chat-buffer` for the writer; both must read the request's original buffer or re-validation loads the wrong drawer and writes contaminate the wrong session.

**Implementation**: `config/gptel/scope/scope-validation.org`

#### Scenario: Entrypoint loads config, validates, dispatches

- **WHEN** the entrypoint is invoked with tool-name, operation, args, on-allow, on-deny
- **THEN** it loads scope from the session's `:PROPERTIES:` drawer, validates, and on success funcalls on-allow
- **AND** on failure triggers the inline expansion UI

#### Scenario: Entrypoint routes filesystem vs bash by :operation

- **WHEN** `:operation` is a filesystem symbol (`read`/`write`/`modify`/`execute`)
- **THEN** the entrypoint gathers file metadata and calls `validate-filesystem-tool`
- **AND** tags the result with `:validation-type 'path`

#### Scenario: Entrypoint routes nil operation to bash validator

- **WHEN** `:operation` is nil
- **THEN** the entrypoint calls `validate-bash-tool`
- **AND** tags the result with `:validation-type 'bash`

#### Scenario: Add-to-scope re-invokes the entrypoint

- **WHEN** the user chooses "add to scope" in the expansion UI
- **THEN** the dispatcher calls itself again with the same arguments
- **AND** the pipeline re-validates against the now-updated drawer
- **AND** if another operation is still denied, the UI prompts again

#### Scenario: Allow-once skips re-validation

- **WHEN** the expansion UI returns `:allowed-once t`
- **THEN** the dispatcher funcalls on-allow directly without re-invoking itself

#### Scenario: Expansion re-entry preserves origin buffer

- **WHEN** `jf/gptel-scope-authorize-tool-call` is invoked from buffer A and the expansion UI's wrapper-callback fires from inside the action handler in buffer B (a PersistentAgent flow's typical case: A is the agent's invisible session buffer, B is the parent's overlay buffer)
- **THEN** the wrapper-callback runs in `(with-current-buffer A …)` so on-allow, on-deny, and the recursive `authorize-tool-call` call all observe `(current-buffer) = A`
- **AND** the recursive `authorize-tool-call`'s `--load-config` reads buffer A's `jf/gptel--branch-dir` and loads buffer A's session drawer
- **AND** any follow-up `jf/gptel-scope-prompt-expansion` captures `:chat-buffer = A` so subsequent drawer writes target the original session, not buffer B
