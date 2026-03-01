## Why

LLM agents need to execute bash commands but must be restricted to allowed file paths for security. Currently, bash-parser can parse command structure but cannot identify which files would be read, created, or modified. This prevents programmatic validation of commands against allowlist patterns before execution.

## What Changes

- Add command semantics database mapping 20+ core commands to their file operation patterns (read/write/delete/modify)
- Add file operation extraction from parsed commands (positional args, redirections, exec blocks)
- Add glob pattern matching to validate file paths against allowlist rules
- Add security checker to evaluate commands against file operation allowlists
- Support operation-specific permissions (read vs write vs delete)
- Handle multi-command constructs (pipelines and chains) by extracting operations from all commands
- Provide high-confidence classification with fallback for unknown commands

## Capabilities

### New Capabilities
- `bash-command-semantics`: Command-to-file-operation mapping database defining how commands interact with files (which args are files, what operations they perform)
- `bash-file-operations`: Extraction of file operations from parsed bash commands with confidence levels and operation types
- `bash-sandbox-security`: Security validation of bash commands against glob-pattern-based allowlists with operation-specific permissions

### Modified Capabilities
- `bash-parser`: Extend existing parser to expose file operations (no breaking changes to existing API)

## Impact

- **Code**: Extends `config/experiments/bash-parser/bash-parser.org` with 3-4 new major sections
- **API**: Adds new functions (`jf/bash-extract-file-operations`, `jf/bash-sandbox-check`) while maintaining backward compatibility
- **Testing**: Extends test corpus with file operation and security test cases
- **Dependencies**: Leverages existing tree-sitter parsing, redirections, and exec block support
- **Use Case**: Enables LLM agent sandbox systems to programmatically validate bash commands before execution
