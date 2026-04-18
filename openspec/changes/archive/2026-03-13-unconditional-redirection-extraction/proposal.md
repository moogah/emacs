## Why

The command-based semantic handlers refactoring introduced a two-gate architecture where the filesystem plugin's predicate decides whether to enter the recursive compound-walking engine. This collapses a grammar-level concern (redirections, pipelines, chains) into a domain-specific plugin, causing silent extraction failures for: (1) any command not in the filesystem whitelist that uses redirections (e.g., `echo hello > file.txt`), (2) any compound structure (pipeline, chain) whose top-level has no `:command-name` and no redirection tokens, even when subcommands perform file operations (e.g., `rm file.txt && echo done`). The old `jf/bash-extract-file-operations` API still works correctly because it enters the recursive engine unconditionally, masking the problem — corpus tests pass on the old path while integration tests fail on the new one.

## What Changes

- **BREAKING**: Remove the filesystem plugin (`jf/bash-plugin-filesystem`) and its predicate-gated entry into the recursive engine
- **BREAKING**: Restructure `jf/bash-extract-semantics` into a three-layer architecture:
  - **Layer 0 (Grammar)**: Unconditional recursive compound decomposition + redirection extraction. Walks pipelines, chains, loops, conditionals, subshells, and command substitutions. Extracts redirections from each simple command as `:filesystem` domain operations. This layer replaces the filesystem plugin's role as gateway to the recursive engine.
  - **Layer 1 (Command handlers)**: Per-simple-command handler dispatch. For each simple command produced by Layer 0, look up registered command handlers and execute them. Command handlers emit domain-specific operations from positional arguments (unchanged from current design).
  - **Layer 2 (Merge)**: Combine grammar-level filesystem operations with command-handler operations across all domains. Deduplicate, calculate coverage.
- Deprecate `jf/bash-extract-file-operations` (old API) — it becomes a thin wrapper around the new pipeline
- Migrate corpus tests to exercise `jf/bash-extract-semantics` instead of the old direct path
- Remove the filesystem plugin predicate and registration machinery

## Capabilities

### New Capabilities
- `grammar-extraction`: Unconditional grammar-level extraction layer that decomposes compound bash structures and extracts shell I/O operations (redirections) independently of any domain plugin or command handler

### Modified Capabilities
- `bash-parser/plugins`: Plugin orchestration no longer uses filesystem plugin as gateway to recursive engine; plugins become purely domain-specific analyzers for non-grammar concerns (e.g., cloud-auth pattern matching)
- `bash-parser/filesystem`: Filesystem plugin removed; its responsibilities split between grammar extraction (redirections, compound walking) and existing command handlers (positional arg operations)
- `bash-parser/architecture`: Data flow changes from plugin-gated to layer-based; new component boundaries
- `bash-parser/semantics`: `jf/bash-extract-semantics` orchestrates three layers instead of running plugins then handlers

## Impact

- **Code**: `bash-parser-plugins.el` (orchestrator restructured), `bash-parser-file-ops.el` (filesystem plugin removed, old API becomes wrapper), `bash-parser-recursive.el` (recursive engine called from Layer 0 instead of from plugin), `bash-parser-semantics.el` (handler dispatch called per-simple-command from Layer 0)
- **Tests**: Corpus tests in `config/bash-parser/test/corpus/` must migrate from `jf/bash-extract-file-operations` to `jf/bash-extract-semantics`. Integration tests in `config/gptel/scope/test/integration/` should pass without modification once the new pipeline is correct.
- **API**: `jf/bash-extract-semantics` signature and return structure unchanged. `jf/bash-extract-file-operations` deprecated but preserved as wrapper. Plugin protocol unchanged for remaining plugins (cloud-auth).
- **Downstream**: gptel scope validation consumes `jf/bash-extract-semantics` — no changes needed as the return structure is preserved.
