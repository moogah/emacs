## REMOVED Requirements

### Requirement: Filesystem plugin implementation
**Reason**: The filesystem plugin is eliminated. Its responsibilities are split: grammar-level extraction (redirections, compound walking) moves to the unconditional grammar extraction layer; positional argument extraction is already handled by per-command handlers.
**Migration**: Remove `jf/bash-plugin-filesystem`, `jf/bash-plugin-filesystem--has-file-tokens-p`, and `jf/bash-register-filesystem-plugin`. Redirection extraction is provided by grammar extraction layer. Command-specific file operations are provided by command handlers.

### Requirement: Backward compatible wrapping
**Reason**: The legacy `jf/bash-extract-file-operations` API is preserved as a thin wrapper around `jf/bash-extract-semantics`, extracting only the `:filesystem` domain operations from the result. The wrapper no longer calls the filesystem plugin directly.
**Migration**: `jf/bash-extract-file-operations` calls `jf/bash-extract-semantics` and returns `(alist-get :filesystem (plist-get result :domains))`. Callers should migrate to `jf/bash-extract-semantics` directly.

## MODIFIED Requirements

### Requirement: Extract file operations from parsed commands
The system SHALL extract all file operations from a parsed bash command. File operations come from two sources: grammar-level redirection extraction (unconditional) and command handler positional argument extraction (handler-based). These are merged in the orchestrator, not in a filesystem plugin.

#### Scenario: Simple read command
- **WHEN** extracting operations from "cat /workspace/foo.txt"
- **THEN** result contains domain `:filesystem` with one operation: file "/workspace/foo.txt", operation `:read`, confidence `:high`, source `:positional-arg`

#### Scenario: Command with redirection
- **WHEN** extracting operations from "echo 'content' > output.txt"
- **THEN** result contains domain `:filesystem` with one operation: file "output.txt", operation `:write`, confidence `:high`, source `:redirection`

#### Scenario: Command with both positional args and redirection
- **WHEN** extracting operations from "cat input.txt > output.txt"
- **THEN** result contains domain `:filesystem` with two operations: file "input.txt" `:read` from `:positional-arg`, and file "output.txt" `:write` from `:redirection`

#### Scenario: Command with no file operations
- **WHEN** extracting operations from "echo hello"
- **THEN** result contains no `:filesystem` domain operations

#### Scenario: Non-filesystem command with redirection
- **WHEN** extracting operations from "aws s3 ls > log.txt"
- **THEN** result contains domain `:filesystem` with operation: file "log.txt", operation `:write`, source `:redirection`
- **AND** the `aws` command handler MAY contribute additional domain operations

#### Scenario: Compound structure with mixed commands
- **WHEN** extracting operations from "rm temp.txt && echo done"
- **THEN** result contains domain `:filesystem` with operation: file "temp.txt", operation `:delete`, source `:positional-arg`
- **AND** "echo done" contributes no filesystem operations (no redirection, no handler)
