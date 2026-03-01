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

**Non-Goals:**
- Variable resolution (`$FILE` → actual value) - defer to Phase 2
- Command substitution parsing (`$(find ...)`) - defer to Phase 2
- Glob expansion on disk (pattern matching only)
- Symlink resolution (filesystem operations)
- Working directory tracking (`cd` commands)
- Comprehensive coverage beyond 20 core commands (extensible database)

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

**Choice:** Path segment matching without filesystem access.

Split paths and patterns by `/`, match segments recursively:
- `*` matches any characters within segment
- `**` matches zero or more segments
- `?` matches single character
- `[abc]` matches character class

```elisp
(jf/bash-path-matches-pattern-p "/workspace/src/foo.el" "/workspace/**/*.el") ;; => t
```

**Rationale:**
- No filesystem access (works with paths that don't exist yet)
- Supports standard glob patterns LLM agents expect
- Segment-based matching handles `**` correctly
- Fast (string operations only)

**Alternatives considered:**
- Filesystem glob expansion: Requires disk access, doesn't work for future operations
- Regex-only: Hard to implement `**` correctly
- External library: No standard Emacs glob library that avoids filesystem

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

### Trade-off: No variable resolution
**Description:** Commands with `$VAR` can't be validated without variable values.

**Impact:** LLM agent must either:
1. Reject all commands with unresolved variables (fail-safe)
2. Provide variable resolution mechanism
3. Accept risk and allow variables

**Chosen approach:** Extract variables as-is, mark with `:unresolved` metadata, let security checker decide policy. Default: reject unresolved.

### Trade-off: No command substitution
**Description:** Commands like `rm $(find ...)` require parsing nested commands.

**Impact:** Can't validate nested command operations.

**Chosen approach:** Phase 1 treats command substitution as unknown. Phase 2 can add recursive parsing (parser already supports it).

### Trade-off: Pattern matching vs glob expansion
**Description:** Pattern matching (without disk) may not match all edge cases that shell expansion would.

**Impact:** Some allowlist patterns might be slightly more permissive or restrictive than shell expansion.

**Chosen approach:** Document matching rules, prefer simpler patterns (explicit paths > globs when possible), test thoroughly.

## Migration Plan

**N/A** - This is a new capability, no migration needed. Existing bash-parser users see no changes (backward compatible).

## Open Questions

1. **Should we handle `cd` commands specially?**
   - `cd /tmp && rm file.txt` - does relative path resolution matter for security?
   - Current: Treat each command independently (don't track working directory)
   - May need to revisit if LLM agents use `cd` frequently

2. **How should we handle command aliases?**
   - If user has `alias ll='ls -la'`, do we need to resolve?
   - Current: Out of scope (assume canonical command names)

3. **Should we distinguish `:write` from `:create`?**
   - Does security checker need to know if file must not exist vs can overwrite?
   - Current: `:create-or-modify` for ambiguous cases (like `touch`)
   - May refine based on use case feedback

4. **Should we add a "strict mode" that rejects unknown commands entirely?**
   - Currently they return empty ops, security checker uses default policy
   - Strict mode: explicitly reject unknown commands upfront
   - Wait for user feedback on preferred behavior
