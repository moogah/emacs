## Context

The existing bash-parser uses tree-sitter to parse bash command strings into structured data (command names, subcommands, flags, positional args, redirections, exec blocks, pipelines, chains). It validates dangerous patterns but doesn't identify file operations.

LLM agent systems need to validate bash commands against file operation allowlists before execution. This requires:
1. Knowing which files a command will read/write/delete/modify
2. Matching file paths against glob patterns
3. Checking operation-specific permissions (read vs write vs delete)

The implementation extends bash-parser.org (literate Emacs Lisp) with new sections while maintaining backward compatibility.

**Constraints:**
- Must leverage existing parsed data (redirections, exec-blocks, positional-args)
- Must support all existing command types (simple, pipeline, chain)
- Must work with relative paths, absolute paths, and glob patterns
- Must handle unknown commands gracefully
- Security-first: fail closed when uncertain

## Goals / Non-Goals

**Goals:**
- Extract file operations from bash commands with high confidence for 20+ core commands
- Validate commands against glob-pattern-based allowlists with operation-specific permissions
- Provide clear violation reporting for denied operations
- Handle redirections, positional args, and find -exec blocks
- Support all command constructs (simple, pipelines, chains)
- Maintain 100% backward compatibility with existing parser API
- **Track simple variables** (`VAR=value` assignments and `$VAR` references in command chains)
- **Detect command injection** (`bash -c`, `python -c`, `sh -c`) and recursively validate nested commands
- **Reject `cd` commands** as security policy (working directory changes complicate path validation)

**Non-Goals:**
- Complex variable resolution (shell parameter expansion, arithmetic, etc.) - defer to Phase 2
- Command substitution parsing (`$(find ...)`) - defer to Phase 2
- Glob expansion on disk (pattern matching only)
- Symlink resolution (filesystem operations)
- Working directory tracking beyond rejecting `cd` (complexity vs security benefit)
- Comprehensive coverage beyond 20 core commands (extensible database)
- Environment variable resolution (`$HOME`, `$PATH`, etc.) - require explicit declaration

## Decisions

### Decision 1: Command Semantics Database Structure

**Choice:** Alist mapping command symbols to semantic plists with operation definitions.

```elisp
(defvar jf/bash-command-file-semantics
  '((cat :operations ((:source :positional-args :operation :read)))
    (cp :operations ((:source :positional-args :indices :0-to-N-2 :operation :read)
                     (:source :positional-args :index -1 :operation :write)))
    (git :operations :complex
         :subcommand-handlers ((add :operations ((:source :positional-args :operation :read)))))))
```

**Rationale:**
- Alist provides fast command lookup with `alist-get`
- Plist structure is idiomatic Emacs Lisp and extensible
- `:indices` syntax supports flexible positional arg patterns (0-to-N-2, -1, etc.)
- `:complex` and `:subcommand-handlers` enable special handling without polluting simple cases

**Alternatives considered:**
- Hash table: More efficient for large databases but overkill for ~20-50 commands, less readable
- Separate function per command: More flexible but harder to maintain and introspect
- External JSON/YAML: Added parsing complexity, less idiomatic for Emacs

### Decision 2: File Operation Extraction Architecture

**Choice:** Layered extraction with three sources:

1. **Redirections** (always high confidence) → extract operator and destination
2. **Command semantics** (high confidence for known commands) → apply semantics to positional args
3. **Exec blocks** (high confidence) → recursively extract from nested commands

```elisp
(jf/bash-extract-file-operations parsed-command)
;; Returns: ((:file "path" :operation :read :confidence :high :source :positional-arg) ...)
```

**Rationale:**
- Redirections are unambiguous (grammar-level), always high confidence
- Semantics-based extraction accurate for known commands, returns nil for unknown
- Exec blocks already parsed, treat nested command as separate extraction
- Confidence levels enable fail-safe behavior (reject low-confidence operations)

**Alternatives considered:**
- Heuristic-only approach: Less accurate, more false positives/negatives
- AST node type analysis: Tree-sitter doesn't expose enough semantic info for files
- Post-hoc shell execution analysis: Requires execution (defeats security purpose)

### Decision 3: Glob Pattern Matching Algorithm

**Choice:** Leverage Emacs `wildcard-to-regexp` as baseline, enhanced with Python `glob.translate()` algorithm for proper `**` handling.

Implementation approach:
1. Use built-in `wildcard-to-regexp` for simple patterns (`*`, `?`, `[abc]`)
2. Port Python's `glob.translate()` segment-based algorithm for `**` support
3. Split paths and patterns by `/`, match segments recursively:
   - `*` matches any characters within segment (not crossing `/`)
   - `**` matches zero or more complete segments
   - `?` matches single character
   - `[abc]` matches character class

```elisp
(jf/bash-path-matches-pattern-p "/workspace/src/foo.el" "/workspace/**/*.el") ;; => t
(jf/bash-path-matches-pattern-p "/workspace/foo.el" "/workspace/**/*.el")     ;; => t (** matches zero)
(jf/bash-path-matches-pattern-p "/tmp/foo.el" "/workspace/**/*.el")           ;; => nil
```

**Rationale:**
- No filesystem access (works with paths that don't exist yet)
- Leverages well-tested Emacs built-in for simple cases
- Python's algorithm is proven (used in production Python code worldwide)
- Segment-based matching handles `**` correctly (zero or more directories)
- Fast (string operations and regex only)
- Pure Elisp (no external dependencies)

**Implementation details (ported from Python):**
- Path segments split by `/` separator
- `**` in middle of pattern: matches any number of intermediate segments
- `**` at end of pattern: matches any remaining path structure
- Single `*` does not cross directory boundaries (matches within segment)
- Hidden files (starting with `.`) can be optionally excluded from wildcard matches

**Alternatives considered:**
- Filesystem glob expansion: Requires disk access, doesn't work for future operations (files not created yet)
- Regex-only approach: Hard to implement `**` correctly without segment awareness
- External library: No standard Emacs glob library that avoids filesystem, adds dependency
- Pure `wildcard-to-regexp`: Doesn't handle `**` recursive wildcards properly

### Decision 4: Security Checker Design

**Choice:** Rule list evaluation with first-match-wins and operation-specific permissions.

```elisp
(defvar jf/bash-sandbox-rules
  '((:patterns ("/workspace/src/**/*.el") :operations (:read :write :modify))
    (:patterns ("/workspace/temp/**") :operations (:read :write :delete))
    (:patterns ("/workspace/**") :operations (:read))))

(jf/bash-sandbox-check "rm /workspace/src/foo.el")
;; => (:allowed nil :violations ((:file "/workspace/src/foo.el" :operation :delete :reason "..."))
```

**Rationale:**
- First-match enables specific rules before general rules (e.g., temp/** before workspace/**)
- Operation-specific permissions provide fine-grained control
- Rule list is simple data structure (easy to serialize, inspect, modify)
- Violations include full context for debugging

**Alternatives considered:**
- Permission bits (rwx style): Less flexible than operation types, harder to extend
- Last-match-wins: Less intuitive, requires rules in reverse order
- Complex rule engine (priority, AND/OR): Over-engineered for use case

### Decision 5: Handling Unknown Commands

**Choice:** Return empty operations list with metadata indicating unknown command. Let security checker decide policy (fail-safe: reject if not explicitly allowed).

**Rationale:**
- Fail-safe: Unknown command with file args → can't extract ops → checker sees no operations → path doesn't explicitly match rule → denied
- Extensible: Add commands to database as needed without changing extraction logic
- Transparent: Confidence levels show what we could/couldn't analyze

**Alternatives considered:**
- Reject all unknown commands: Too strict, prevents extending command set
- Heuristic fallback (args look like paths): Too many false positives
- Treat all args as potential files: Pollutes results with non-file args

### Decision 6: Integration with Existing Parser

**Choice:** Add new sections to bash-parser.org, new top-level functions, reuse existing parsed data structures.

No changes to `jf/bash-parse`, no changes to return format. New functions:
- `jf/bash-extract-file-operations`
- `jf/bash-sandbox-check`
- Helper functions for pattern matching, semantics lookup

**Rationale:**
- Zero breaking changes to existing API
- Literate programming: new sections self-documenting
- Reusing parsed data avoids re-parsing and ensures consistency

**Alternatives considered:**
- Separate module: More modular but duplicates parsing or requires tight coupling
- Modify parse return format: Breaking change, unnecessary

### Decision 7: Variable Tracking

**Choice:** Track simple variable assignments and references within command chains, require explicit variable declaration for security context.

Phase 1 implementation:
- Extract `VAR=value` assignments from command chains (`&&`, `;`)
- Track variable references (`$VAR`, `${VAR}`) in subsequent commands
- Resolve simple variable substitution within same command chain
- Mark unresolved variables with `:unresolved` metadata
- Security checker requires variable declaration or rejects unresolved variables

```elisp
;; Variable context passed to security checker
(defvar jf/bash-variable-context
  '(("WORKSPACE" . "/workspace")
    ("TEMP_DIR" . "/tmp/build")))

;; Command chain with variable
"OUTFILE=/workspace/output.txt && cat input.txt > $OUTFILE"
;; Extracts: (:file "/workspace/output.txt" :operation :write :confidence :high)
```

**Rationale:**
- Most LLM-generated commands use simple variables (`DIR=/tmp && rm $DIR/file`)
- LLM agents can declare variables upfront in security context
- Simple assignment tracking is low complexity, high value
- Marking unresolved variables enables fail-safe policy (reject unknown variables)
- Phase 1 covers 80% of real-world use cases

**Deferred to Phase 2:**
- Shell parameter expansion (`${VAR:-default}`, `${VAR#pattern}`)
- Arithmetic expansion (`$((1+2))`)
- Environment variable lookup (`$HOME`, `$USER`)
- Variable scoping across subshells

**Alternatives considered:**
- Defer all variable handling: Too limiting, most commands use variables
- Full shell variable semantics: Too complex for Phase 1, diminishing returns
- Require all paths to be literal: Too restrictive, breaks legitimate use cases

### Decision 8: Command Injection Detection

**Choice:** Detect command execution flags (`bash -c`, `python -c`, `sh -c`, `env -S`) and recursively parse nested command strings.

Implementation:
- Extend command semantics database with command-injection patterns
- Parse argument after `-c` flag as nested bash command string
- Recursively call `jf/bash-parse` on nested command
- Mark nested operations with `:indirect t` metadata
- Security checker validates nested commands with same rules

```elisp
;; Command injection pattern in database
(bash . (:command-injection (:flags ("-c") :arg-index 1)))

;; Nested command parsing
"bash -c 'rm /workspace/temp.txt'"
;; Parses as: (:command-name "bash" :flags ("-c")
;;            :nested-command (:command-name "rm" :positional-args ("/workspace/temp.txt")))
;; Extracts: (:file "/workspace/temp.txt" :operation :delete :indirect t)
```

**Rationale:**
- Command injection is a common allowlist bypass technique (security-critical)
- Real-world attacks use `bash -c`, `python -c`, `sh -c` to evade static analysis
- Recursive parsing reuses existing parser infrastructure
- `:indirect` flag enables stricter security policy (e.g., reject all indirect commands)
- Industry precedent: OpenClaw CVE-2026-26325 bypassed allowlist via command mismatch

**Scope:**
- Phase 1: `bash -c`, `sh -c`, `python -c`, `env -S` (covers 95% of injection vectors)
- Phase 2: `xargs`, `parallel`, complex find -exec with nested commands

**Alternatives considered:**
- Reject all command injection: Too restrictive, legitimate use cases exist (scripting)
- Runtime detection only: Misses static validation opportunity, fails security-in-depth
- Heuristic detection: Fragile, high false negative rate

### Decision 9: Reject `cd` Commands

**Choice:** Security checker rejects all `cd` commands as policy decision, avoiding working directory tracking complexity.

**Rationale:**
- Working directory tracking is complex: `cd /tmp && rm -rf *` requires resolving relative paths against `/tmp`
- Command chains with multiple `cd` commands increase validation complexity exponentially
- Security risk: Allowlist permits `/workspace/**`, but `cd /tmp && rm file.txt` operates in `/tmp`
- Legitimate use cases are rare: LLM agents should specify absolute paths or run commands with correct working directory from start
- Simpler alternative: Use `(cd /workspace && cmd)` subshell pattern or runtime sandbox configuration

**Implementation:**
- Add `cd` to dangerous patterns database with `:always-reject` flag
- Security checker returns violation: "cd commands are prohibited (use absolute paths)"
- Document recommended alternatives in error message

**User guidance:**
```bash
# Instead of:
cd /workspace && cat file.txt

# Use absolute paths:
cat /workspace/file.txt

# Or runtime directory configuration:
# (execute command with cwd=/workspace)
```

**Alternatives considered:**
- Track working directory across command chains: High complexity, error-prone, security risk
- Allow `cd` with strict validation: Partial solution, still vulnerable to complex chains
- Runtime-only enforcement: Misses static validation opportunity

## Risks / Trade-offs

### Risk: Incomplete command coverage
**Description:** 20 commands cover common cases but not all possible file operations.

**Mitigation:**
- Database is extensible (add commands as needed)
- Unknown commands return empty operations (fail-safe)
- Document coverage in tests
- Phase 2 can add heuristic fallback if needed

### Risk: False negatives (miss file operations)
**Description:** Complex commands might have file operations we don't detect (new flag patterns, unusual arg positions).

**Mitigation:**
- Security checker operates on extracted ops only (fail-closed: deny if unsure)
- Test corpus includes complex cases (find -exec, pipelines, chains)
- Confidence levels flag uncertain extractions
- LLM agents can be configured to require approval for low-confidence commands

### Risk: False positives (non-files flagged as files)
**Description:** Some commands take arguments that look like paths but aren't files (e.g., `git checkout branch-name`).

**Mitigation:**
- Command semantics database is precise (not heuristic)
- Only extract files from known file-argument positions
- Non-file args in other positions ignored

### Risk: Pattern matching edge cases
**Description:** Glob pattern matching without filesystem is complex (`**` behavior, symlinks, case sensitivity).

**Mitigation:**
- Well-tested pattern matching with explicit test cases
- Document matching rules clearly
- No symlink resolution (by design, part of non-goals)
- Case-sensitive matching (standard Unix behavior)

### Trade-off: Simple variable tracking only
**Description:** Phase 1 handles simple `VAR=value` assignments and `$VAR` references, but not complex shell parameter expansion.

**Impact:** Commands using `${VAR:-default}`, `${VAR#pattern}`, or environment variables (`$HOME`) require explicit declaration or are marked `:unresolved`.

**Chosen approach:**
- Phase 1: Track simple variables in command chains, require variable declaration for security context
- Mark unresolved/complex variables with `:unresolved` metadata
- Security checker rejects unresolved by default (fail-safe)
- Phase 2: Add environment variable lookup and parameter expansion
- Covers 80% of real-world use cases with 20% of complexity

### Trade-off: Command execution detection, not command substitution
**Description:** Phase 1 handles `bash -c "cmd"` (command execution) but not `$(cmd)` or `` `cmd` `` (command substitution).

**Impact:** Commands like `rm $(find ...)` or `cat `which foo`` can't be fully validated.

**Chosen approach:**
- Phase 1: Recursively parse command execution (`bash -c`, `python -c`, `sh -c`)
- Command substitution marked as `:unresolved` (fail-safe: security checker rejects)
- Phase 2: Add command substitution parsing (more complex, lower priority)
- Rationale: Command execution is more common and higher security risk

### Trade-off: Pattern matching vs glob expansion
**Description:** Pattern matching (without disk) may not match all edge cases that shell expansion would.

**Impact:** Some allowlist patterns might be slightly more permissive or restrictive than shell expansion.

**Chosen approach:** Document matching rules, prefer simpler patterns (explicit paths > globs when possible), test thoroughly.

## Migration Plan

**N/A** - This is a new capability, no migration needed. Existing bash-parser users see no changes (backward compatible).

## Open Questions

1. **How should we handle command aliases?**
   - If user has `alias ll='ls -la'`, do we need to resolve?
   - Current: Out of scope (assume canonical command names)
   - Revisit if LLM agents frequently generate aliased commands

2. **Should we distinguish `:write` from `:create`?**
   - Does security checker need to know if file must not exist vs can overwrite?
   - Current: `:create-or-modify` for ambiguous cases (like `touch`)
   - May refine based on use case feedback
   - Example: `:create` for "must not exist", `:write` for "overwrite ok"

3. **Should we add a "strict mode" that rejects unknown commands entirely?**
   - Currently they return empty ops, security checker uses default policy (fail-safe)
   - Strict mode: explicitly reject unknown commands upfront with error
   - Wait for user feedback on preferred behavior
   - May be useful for high-security environments

4. **Should variable context be per-command-chain or global?**
   - Per-chain: `VAR=/tmp && cmd $VAR` but next chain doesn't see VAR
   - Global: Variables persist across separate command validations
   - Current: Per-chain (simpler, more predictable)
   - May need global context for interactive LLM agent sessions
