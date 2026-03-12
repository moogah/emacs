## Context

The bash parser currently extracts file operations (read, write, delete, modify, create) from parsed commands using a semantics database that maps command names to their file operation patterns. This system works well for traditional file manipulation commands but cannot distinguish between reading a file as data (`cat script.py`) versus executing it as code (`python script.py`).

This limitation creates a security gap: we cannot enforce policies around which scripts are allowed to run because we don't detect execution operations. The system needs to be extended to recognize when commands execute scripts and capture this as a distinct operation type.

The existing architecture provides the foundation:
- `bash-parser-semantics.el` defines command semantics in `jf/bash-command-file-semantics` variable
- `bash-parser-file-ops.el` orchestrates extraction from multiple sources (redirections, positional args, exec blocks)
- Both modules use literate programming (`.org` files that tangle to `.el`)

This change extends both modules while maintaining backward compatibility with existing extraction logic.

## Goals / Non-Goals

**Goals:**
- Add `:execute` operation type to distinguish script execution from file reading
- Detect interpreter commands (python, node, bash, go) executing their first positional argument
- Detect self-executing path-based commands (`./script.sh`, `/usr/bin/tool`)
- Capture script arguments as `:script-args` metadata for future analysis
- Maintain backward compatibility with existing operation types and tests
- Follow existing literate programming patterns and code organization

**Non-Goals:**
- Analyze what script arguments mean (file paths vs URLs vs other data)
- Support multiple script files in single command (`python script1.py script2.py`)
- Handle complex interpreter flags (`python -m module`, `node --inspect`)
- Detect compilation operations (`gcc`, `go build` as distinct from execution)
- Perform static analysis of script contents
- Distinguish between compiled binaries and shell scripts in self-execution
- Filesystem access to verify if paths exist or are executable

## Decisions

### Decision 1: Add :execute operation type to semantics database

**Choice**: Extend existing operation type enum with `:execute` alongside `:read`, `:write`, `:delete`, `:modify`, `:create`, `:create-or-modify`, `:append`.

**Rationale**:
- Execution is fundamentally different from reading for security purposes
- Clean separation of concerns (read = data access, execute = code execution)
- Enables policies like "allow read all files, execute only approved scripts"

**Alternatives Considered**:
- **Option A**: Use `:read` with `:executed t` metadata flag
  - Rejected: Execution is not a variant of reading, it's a distinct operation
  - Would require special-casing in security policies
- **Option B**: Use `:read` + `:execute` (two operations per script)
  - Rejected: Creates noise in operation lists
  - Execution implies reading, don't need both explicitly

**Implementation**: Add interpreter entries to `jf/bash-command-file-semantics` with `:operation :execute`.

### Decision 2: Use :index 0 to extract only script file, not arguments

**Choice**: Interpreter semantics specify `:index 0` to extract only the first positional argument (the script file), ignoring subsequent arguments.

**Rationale**:
- Script arguments are unknown data types (might be files, URLs, database names, flags)
- Extracting them as file operations would create false positives
- We don't want `python process.py data.csv` to report `:read` on `data.csv` unless we know that's what the script does

**Alternatives Considered**:
- **Option A**: Extract all positional args with `:unknown` confidence
  - Rejected: Too noisy, many false positives
  - Security policies would need to allowlist everything
- **Option B**: Static analysis of script to determine what args mean
  - Rejected: Out of scope (deferred to future work)
  - Would require parsing Python/JavaScript/etc. code

**Implementation**:
```elisp
(python . (:operations ((:source :positional-args :operation :execute :index 0))))
```

### Decision 3: Capture script arguments as :script-args metadata

**Choice**: Store remaining positional arguments in `:script-args` field of execute operation, but don't extract them as separate file operations.

**Rationale**:
- Preserves information for future analysis without creating false operations
- Enables future features (annotation files, static analysis) to use this data
- Metadata approach keeps operations list clean and focused

**Alternatives Considered**:
- **Option A**: Discard script arguments entirely
  - Rejected: Loses potentially useful information
  - Hard to add back later without re-parsing
- **Option B**: Extract as separate operations with `:unknown` type
  - Rejected: See Decision 2, too noisy

**Implementation**: Capture in extraction logic when building execute operation plist.

### Decision 4: Self-execution detection via command-name prefix matching

**Choice**: Detect self-executing commands by checking if command-name starts with `./`, `/`, or `../`. Assign `:low` confidence to these operations.

**Rationale**:
- Simple heuristic, no filesystem access needed
- Matches common shell idioms for path-based execution
- Low confidence signals uncertainty to security policies

**Alternatives Considered**:
- **Option A**: Filesystem stat() to check if path is executable
  - Rejected: Breaks pure function design (no I/O in extraction)
  - Performance cost, failures for non-existent paths
  - Command might execute on different machine than analysis
- **Option B**: Check file extension (`.sh`, `.py`, `.js`)
  - Rejected: Compiled binaries have no extension
  - Shebang scripts can have any extension
- **Option C**: Only detect if extension matches known script types
  - Rejected: Misses `./binary-with-no-extension`
  - False negatives are dangerous for security

**Implementation**: New function `jf/bash--command-executes-self-p` in `bash-parser-file-ops.el`.

### Decision 5: Subcommand-based semantics for go command

**Choice**: Use existing `:complex` subcommand handler mechanism for `go`, where `go run` and `go test` return `:execute`, but `go build` returns `:read`.

**Rationale**:
- Leverages existing architecture (already supports `git`, `docker`, `npm` subcommands)
- `go build` reads source files for compilation, doesn't execute them
- `go run` compiles and executes in one step
- `go test` compiles and executes test files

**Alternatives Considered**:
- **Option A**: Treat all go subcommands as `:execute`
  - Rejected: `go build` doesn't run the code
- **Option B**: Create new `:compile` operation type
  - Rejected: Out of scope (deferred), compilation is complex

**Implementation**: Add to existing `jf/bash-command-file-semantics` under `go` entry.

### Decision 6: Shell built-ins (source, .) as regular commands

**Choice**: Treat `source` and `.` as regular interpreter commands in semantics database with `:execute` operation, despite being shell built-ins.

**Rationale**:
- From extraction perspective, they behave like interpreters
- Execute first positional argument in current shell context
- No special handling needed, fits existing pattern

**Alternatives Considered**:
- **Option A**: Special-case handling for built-ins
  - Rejected: Unnecessary complexity
  - They fit the interpreter pattern perfectly

**Implementation**: Add entries to `jf/bash-command-file-semantics`:
```elisp
(source . (:operations ((:source :positional-args :operation :execute :index 0))))
(|.| . (:operations ((:source :positional-args :operation :execute :index 0))))
```

Note: The `.` command needs to be represented as symbol, likely `|.|` in Elisp.

### Decision 7: Integration point in extraction orchestrator

**Choice**: Add self-execution check after existing extraction sources (redirections, positional args, exec blocks) in `jf/bash-extract-file-operations`.

**Rationale**:
- Maintains existing extraction order and logic
- Self-execution is independent of other sources (based on command-name, not args)
- Can be added without modifying existing extraction paths

**Alternatives Considered**:
- **Option A**: Integrate into positional args extraction
  - Rejected: Self-execution doesn't use positional args as source
  - Would complicate existing logic
- **Option B**: Separate extraction function called by consumer
  - Rejected: Breaks single entry point pattern
  - Consumers would need to remember to call both functions

**Implementation**: Add section to `jf/bash-extract-file-operations` after exec blocks extraction, before deduplication.

### Decision 8: Test organization with dedicated file and corpus

**Choice**: Create `test-script-execution.el` and `script-execution-corpus.el` following existing pattern from `test-file-operations.el` and `file-operations-corpus.el`.

**Rationale**:
- Consistent with existing test organization
- Keeps execution tests separate from file operation tests
- Corpus enables comprehensive test coverage without cluttering test file
- Easy to run execution tests independently

**Alternatives Considered**:
- **Option A**: Add tests to existing `test-file-operations.el`
  - Rejected: File is already large, mixing concerns
- **Option B**: No corpus file, inline all test data
  - Rejected: Makes test file harder to navigate
  - Corpus pattern proven effective in existing code

**Implementation**: Follow architecture.md testing approach specification.

## Risks / Trade-offs

### Risk: False negatives in self-execution detection
**Description**: Path-based heuristic might miss some execution patterns (symlinks, unusual paths, PATH-resolved commands).

**Mitigation**:
- Assign `:low` confidence to signal uncertainty
- Document limitation in specs and comments
- Security policies can be conservative (deny unknown commands)
- Future enhancement: user-configurable execution patterns

### Risk: Command name collision with "." symbol
**Description**: The dot command `.` is both a shell built-in and a symbol that may require special handling in Elisp.

**Mitigation**:
- Use escaped symbol syntax `|.|` in Elisp if needed
- Test specifically for dot command execution
- Verify symbol lookup works correctly in alist

### Risk: Script arguments captured but not validated
**Description**: `:script-args` metadata may contain sensitive data or be misinterpreted as file paths by future code.

**Mitigation**:
- Document that script-args are unknown data types
- Do NOT use script-args for file operations extraction (by design)
- If future features use script-args, they must validate/sanitize

### Trade-off: Low confidence for self-execution
**Description**: `:low` confidence may cause security policies to reject legitimate scripts.

**Rationale**:
- False negatives (missing execution) are more dangerous than false positives
- Users can explicitly allowlist self-executing scripts if needed
- Better to be conservative for security-critical detection

### Trade-off: No support for complex interpreter flags
**Description**: Commands like `python -m module` or `node --inspect script.js` deferred to future work.

**Rationale**:
- Requires per-interpreter flag parsing knowledge
- Out of scope for initial implementation
- Covers 80% of common cases (simple interpreter + script pattern)
- Can be added incrementally as interpreter-specific handlers

### Trade-off: No distinction between script types in self-execution
**Description**: Cannot tell if `./tool` is a shell script, Python script with shebang, or compiled binary.

**Rationale**:
- Would require filesystem access or shebang parsing
- All three cases are execution operations for security purposes
- Distinction not needed for initial use case

## Migration Plan

### Phase 1: Implementation (Beads-tracked work)
1. Extend semantics database with interpreter entries
2. Implement self-execution detection function
3. Integrate into extraction orchestrator with script-args capture
4. Create test file and corpus with comprehensive scenarios

### Phase 2: Validation
1. Run existing test suite to verify backward compatibility
2. Run new script execution tests to verify functionality
3. Use snapshot testing to track test results over time
4. Validate with corpus of real-world commands

### Phase 3: Integration
1. Verify security validation consumers can handle `:execute` operation type
2. Update any hardcoded operation type checks if needed
3. No API changes required (fully backward compatible)

### Rollback Strategy
- Changes are additive (new operation type, new function, new semantics entries)
- Removing new code returns to previous behavior
- No data migration needed (pure function enhancement)
- Existing tests must continue passing throughout

### Deployment Notes
- Tangle `.org` files to generate `.el` files before testing
- Commit both `.org` and `.el` files together
- Test in isolated worktree before merging to main
- No runtime configuration changes needed

## Open Questions

**Q1: How should the dot command "." be represented in Elisp alist?**

Need to verify if `(|.| . (...))` syntax works or if alternative approach needed. Will be discovered during implementation.

**Q2: Should we track which interpreter executed the script in operation metadata?**

Currently captured as `:command-name` in metadata. May want explicit `:interpreter` field for easier querying. Can be added if needed during implementation without breaking changes.

**Q3: Should execution operations include parent command info in pipelines?**

Example: `python gen.py | python process.py` - should each execute operation know it's in a pipeline? Current metadata likely sufficient, but may refine during testing.

**Q4: Do we need helper test functions or is corpus sufficient?**

Architecture mentions optional `assert-execute-operation` and `make-execute-operation` helpers. Will determine during test writing if they reduce duplication enough to justify.

These questions will be resolved during implementation and documented in commit messages or code comments.
