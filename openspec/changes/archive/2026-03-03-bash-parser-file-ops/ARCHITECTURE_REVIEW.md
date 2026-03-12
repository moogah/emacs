# Architecture Review: Bash Parser File Operations

## Overview

This review analyzes the bash-parser-file-ops implementation against the conceptual model of three independent, composable components.

## Conceptual Model (Your Mental Framework)

You described three main components:

1. **Tree-sitter Parsing** - Parse bash command strings into data structures
2. **Semantic Database** - Extract file operations using command semantics
3. **Glob Pattern Matching** - Compare file paths against patterns

The ideal architecture would:
- Make each component independently useful
- Allow composition to create the full security validation system
- Enable reuse of components in other contexts

## Current Implementation Structure

The implementation is organized into **7 modules** within `config/experiments/bash-parser/`:

```
bash-parser.org/el                   (Main loader - coordinates all subsystems)
├── bash-parser-core.org/el          (Tree-sitter parsing infrastructure)
├── bash-parser-glob.org/el          (Glob pattern matching - no filesystem)
├── bash-parser-semantics.org/el     (Command semantics database)
├── bash-parser-variables.org/el     (Variable context management)
├── bash-parser-file-ops.org/el      (File operations extraction)
├── bash-parser-security.org/el      (Security validation & rule matching)
└── bash-parser-extensions.org/el    (Command injection detection, etc.)
```

### Module Dependencies

```
bash-parser-glob.el           ← No dependencies (pure glob matching)
bash-parser-semantics.el      ← No dependencies (pure database)
bash-parser-variables.el      ← No dependencies (variable resolution)
bash-parser-security.el       ← Depends on: glob
bash-parser-core.el           ← Depends on: security (dangerous patterns)
bash-parser-file-ops.el       ← Depends on: core, semantics, variables
bash-parser-extensions.el     ← Depends on: file-ops
```

## Mapping to Conceptual Components

### Component 1: Tree-sitter Parsing → `bash-parser-core.el`

**Public API:**
```elisp
(jf/bash-parse "cat file.txt | grep pattern")
;; => (:success t
;;     :type :pipeline
;;     :all-commands (...)
;;     :command-count 2)
```

**Responsibilities:**
- Parse bash strings using tree-sitter
- Handle pipelines (`|`), chains (`&&`, `||`, `;`)
- Extract command structure (command-name, flags, positional-args, redirections)
- Detect wrapper commands (sudo, env, time)
- Find exec blocks in find commands

**Independence:** ✅ **Fully independent**
- Can be used standalone for any bash parsing task
- No knowledge of file operations or security
- Returns pure data structures

**Reusability examples:**
- Command validation (syntax checking)
- Command analysis tools
- Shell script linters
- Command documentation generators

---

### Component 2: Semantic Database → `bash-parser-semantics.el` + `bash-parser-file-ops.el`

**Split into two modules:**

#### 2a. `bash-parser-semantics.el` - The Database

**Public API:**
```elisp
(jf/bash-lookup-command-semantics "cat")
;; => (:operations ((:source :positional-args :operation :read)))

(jf/bash-lookup-command-semantics "cp")
;; => (:operations ((:source :positional-args :indices (0 . -2) :operation :read)
;;                  (:source :positional-args :index -1 :operation :write)))
```

**Responsibilities:**
- Define file operation semantics for 20+ commands
- Support simple, flag-dependent, and subcommand-based commands
- Pure database lookup (no extraction logic)

**Independence:** ✅ **Fully independent**
- No dependencies on parsing or glob matching
- Can be queried standalone
- Extensible (add new commands easily)

**Reusability examples:**
- Command documentation ("what does this command do?")
- Teaching tools (explain command behavior)
- Alternative security systems
- File operation prediction

#### 2b. `bash-parser-file-ops.el` - The Extractor

**Public API:**
```elisp
(jf/bash-extract-file-operations parsed-command var-context)
;; => ((:file "/workspace/foo.txt" :operation :read
;;      :confidence :high :source :positional-arg) ...)
```

**Responsibilities:**
- Orchestrate extraction from all sources (redirections, positional-args, exec-blocks)
- Apply semantics to parsed commands
- Resolve variables in file paths
- Handle multi-command constructs
- Track confidence levels
- Deduplicate operations

**Independence:** ⚠️ **Partially independent**
- Requires parsed command from `bash-parser-core`
- Uses `bash-parser-semantics` for lookup
- Uses `bash-parser-variables` for resolution
- **No dependency on glob matching or security**
- Can be used for analysis without security validation

**Reusability examples:**
- File operation auditing
- Dependency tracking
- Impact analysis
- Documentation generation

---

### Component 3: Glob Pattern Matching → `bash-parser-glob.el`

**Public API:**
```elisp
(jf/bash-glob-match-p "/workspace/src/foo.el" "/workspace/**/*.el")
;; => t

(jf/bash-glob-match-p "/tmp/foo.el" "/workspace/**/*.el")
;; => nil
```

**Responsibilities:**
- String-based glob matching (no filesystem access)
- Support `*`, `**`, `?`, `[abc]` patterns
- Handle recursive wildcards correctly
- O(n) complexity in path segments

**Independence:** ✅ **Fully independent**
- No dependencies on bash parsing or semantics
- Pure string matching algorithm
- Can be used for any glob pattern matching task

**Reusability examples:**
- File filtering in any context
- Path allowlist/blocklist systems
- Configuration matching
- Resource access control

---

## Composition: The Full Security System

### `bash-parser-security.el` - The Orchestrator

**Public API:**
```elisp
(jf/bash-sandbox-check command-string rules var-context indirect-policy)
;; => (:allowed t/nil
;;     :violations (...)
;;     :unhandled (...)
;;     :operations (...))
```

**Composition Flow:**
```
1. jf/bash-parse (core)
   └─> Parse command string to data structure

2. jf/bash-extract-file-operations (file-ops)
   └─> Extract operations using semantics
   └─> Resolve variables
   └─> Return list of operations

3. For each operation:
   ├─> jf/bash-match-rule (security)
   │   └─> jf/bash-glob-match-p (glob) ← Compare file against patterns
   │       └─> Return matching rule or nil
   └─> jf/bash-check-operation-permission (security)
       └─> Validate operation type allowed by rule
       └─> Return violation or nil

4. Aggregate results
   └─> Return comprehensive validation result
```

**This is the glue code** that composes all three conceptual components:
- Uses **Component 1** to parse
- Uses **Component 2** to extract operations
- Uses **Component 3** to match patterns
- Adds security-specific logic (rule matching, permission checking, policies)

---

## Supporting Components

Beyond the three core components, two supporting modules handle cross-cutting concerns:

### `bash-parser-variables.el`

**Purpose:** Variable resolution and tracking

**API:**
```elisp
(jf/bash-resolve-variables "$WORKSPACE/file.txt" '((WORKSPACE . "/workspace")))
;; => (:path "/workspace/file.txt" :resolved t)

(jf/bash-resolve-variables "$UNKNOWN/file.txt" nil)
;; => (:path "$UNKNOWN/file.txt" :resolved nil :unresolved (UNKNOWN))
```

**Why separate:** Variables are used by file-ops but could be used by other consumers of parsed commands.

### `bash-parser-extensions.el`

**Purpose:** Advanced features like command injection detection

**API:**
```elisp
;; Detect and recursively parse: bash -c "rm /tmp/file.txt"
;; Returns operations with :indirect t flag
```

**Why separate:** Optional advanced features that extend core functionality without bloating it.

---

## Architecture Assessment

### ✅ Strengths

1. **Component 1 (Parsing) is fully independent**
   - Can be used standalone for any bash parsing
   - Clean data structure output
   - No coupling to file operations

2. **Component 3 (Glob matching) is fully independent**
   - Zero dependencies
   - Can be used in completely unrelated contexts
   - Well-tested, correct algorithm

3. **Component 2 (Semantics) database is independent**
   - Pure data structure
   - Easy to extend
   - Can be queried without parsing

4. **Clear separation of concerns**
   - Each module has a single responsibility
   - Well-documented interfaces
   - Literate programming makes each module readable

5. **Composability demonstrated**
   - `jf/bash-sandbox-check` successfully composes all components
   - Each component usable in isolation or composition

### ⚠️ Areas for Consideration

1. **Component 2 (Extraction) is not fully independent**

   **Current state:**
   ```elisp
   ;; Requires parsed command structure
   (jf/bash-extract-file-operations parsed-command var-context)
   ```

   **This is actually good design** - the extractor operates on parsed data, not raw strings. It keeps parsing and extraction separate.

   **Alternative (worse):**
   ```elisp
   ;; Bad: would re-parse internally, creating hidden dependency
   (jf/bash-extract-file-operations command-string var-context)
   ```

   **Verdict:** Current design is correct. Independence means "can be used without the security layer", not "has no dependencies at all".

2. **Module naming could be clearer**

   The split between `bash-parser-semantics` (database) and `bash-parser-file-ops` (extractor) is logical but might not be obvious from names.

   **Possible improvement:**
   ```
   bash-parser-semantics.el     → bash-parser-command-database.el
   bash-parser-file-ops.el      → bash-parser-operations-extractor.el
   ```

   But this is a minor naming issue, not an architectural problem.

3. **Some coupling between core and security**

   `bash-parser-core` depends on `bash-parser-security` for dangerous pattern detection:

   ```elisp
   ;; In bash-parser-core.el
   (require 'bash-parser-security)  ;; For dangerous pattern database
   ```

   **Why this exists:** The parser sets `:dangerous-p` flag during parsing for convenience.

   **Improvement opportunity:** Move dangerous pattern detection to security layer, keep parser purely structural.

---

## Independence Testing

Can each component be used independently?

### ✅ Glob Matching (Component 3)

```elisp
;; Standalone usage - no other bash-parser modules needed
(require 'bash-parser-glob)

(jf/bash-glob-match-p "/home/user/docs/readme.txt" "/home/**/*.txt")
;; => t

;; Use cases: path filtering, access control, file organization
```

**Verdict:** Fully independent ✅

---

### ✅ Tree-sitter Parsing (Component 1)

```elisp
;; Standalone usage - just parse commands
(require 'bash-parser-core)

(let ((parsed (jf/bash-parse "git add src/*.el")))
  (plist-get parsed :command-name))      ;; => "git"
  (plist-get parsed :subcommand))        ;; => "add"
  (plist-get parsed :positional-args))   ;; => ("src/*.el")

;; Use cases: command analysis, linting, documentation
```

**Verdict:** Fully independent ✅
*Note: Currently has unnecessary dependency on security for dangerous patterns. Could be even more independent.*

---

### ✅ Semantics Database (Component 2a)

```elisp
;; Standalone usage - query command semantics
(require 'bash-parser-semantics)

(jf/bash-lookup-command-semantics "cp")
;; => (:operations ((:source :positional-args :indices (0 . -2) :operation :read)
;;                  (:source :positional-args :index -1 :operation :write)))

;; Use cases: command documentation, teaching tools, analysis
```

**Verdict:** Fully independent ✅

---

### ⚠️ File Operations Extraction (Component 2b)

```elisp
;; Requires parsed command, but not security/glob
(require 'bash-parser-core)
(require 'bash-parser-file-ops)

(let* ((parsed (jf/bash-parse "cat input.txt > output.txt"))
       (ops (jf/bash-extract-file-operations parsed)))
  ops)
;; => ((:file "input.txt" :operation :read :confidence :high :source :positional-arg)
;;     (:file "output.txt" :operation :write :confidence :high :source :redirection))

;; Use cases: impact analysis, dependency tracking, auditing
```

**Verdict:** Independent from security/glob, depends on parsing ✅
*This is appropriate - extraction needs parsed structure to work with.*

---

## Recommendations

### 1. Remove dangerous pattern coupling from core parser

**Current:**
```elisp
;; bash-parser-core.el
(require 'bash-parser-security)  ;; ← Unnecessary coupling
```

**Proposed:**
- Remove `:dangerous-p` flag from parser output
- Move dangerous pattern detection to security layer
- Parser becomes purely structural (better independence)

**Impact:**
- Core parser truly independent
- Security layer still has all needed info
- Breaking change: consumers using `:dangerous-p` need to call security check

---

### 2. Consider renaming for clarity (optional)

**Current names:**
```
bash-parser-semantics.el   (the database)
bash-parser-file-ops.el    (the extractor)
```

**Possible alternatives:**
```
bash-parser-command-database.el   (clearer: it's a database)
bash-parser-operations.el         (clearer: it extracts operations)
```

**Verdict:** Low priority - current names work fine, docs are clear.

---

### 3. Document independent usage patterns

Create examples showing each component used independently:

```elisp
;; Example: Use glob matching for file filtering (no bash involved)
(defun my-filter-paths (paths pattern)
  (seq-filter (lambda (path) (jf/bash-glob-match-p path pattern))
              paths))

;; Example: Use parser for command documentation
(defun my-explain-command (cmd-string)
  (let ((parsed (jf/bash-parse cmd-string)))
    (format "Command: %s, Flags: %s"
            (plist-get parsed :command-name)
            (plist-get parsed :flags))))

;; Example: Use semantics for teaching
(defun my-command-help (cmd-name)
  (let ((semantics (jf/bash-lookup-command-semantics cmd-name)))
    (format "%s operates on files as follows: %s"
            cmd-name
            (plist-get semantics :operations))))
```

---

## Summary

### Architecture Alignment with Conceptual Model

| Conceptual Component | Implementation | Independence | Reusability |
|---------------------|----------------|--------------|-------------|
| 1. Tree-sitter Parsing | `bash-parser-core` | ⚠️ Good (has 1 unnecessary coupling) | ✅ High |
| 2. Semantic Database | `bash-parser-semantics` | ✅ Fully independent | ✅ High |
| 2. Operations Extraction | `bash-parser-file-ops` | ✅ Independent from security | ✅ Medium-High |
| 3. Glob Pattern Matching | `bash-parser-glob` | ✅ Fully independent | ✅ Very High |
| Composition | `bash-parser-security` | N/A (orchestrator) | ✅ Complete system |

### Overall Assessment

**The implementation successfully achieves the conceptual model's goals:**

1. ✅ **Three independent components** - Each can be used standalone
2. ✅ **Clean composition** - Security layer combines them effectively
3. ✅ **High reusability** - Components useful beyond original purpose
4. ✅ **Good separation** - Clear boundaries, minimal coupling
5. ⚠️ **One improvement opportunity** - Remove dangerous pattern coupling from parser

**The architecture is sound and well-executed.** The minor coupling between core and security is the only area that could be improved for even better independence.

### Visual Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  bash-parser-security.el (Orchestrator)                     │
│  ┌──────────────────────────────────────────────────────┐   │
│  │ jf/bash-sandbox-check                                │   │
│  │ - Parse command                                      │   │
│  │ - Extract operations                                 │   │
│  │ - Match rules                                        │   │
│  │ - Validate permissions                               │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
          │                    │                    │
          ▼                    ▼                    ▼
┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐
│ Component 1      │  │ Component 2      │  │ Component 3      │
│ PARSING          │  │ SEMANTICS        │  │ GLOB MATCHING    │
├──────────────────┤  ├──────────────────┤  ├──────────────────┤
│ bash-parser-core │  │ bash-parser-     │  │ bash-parser-glob │
│                  │  │   semantics      │  │                  │
│ Tree-sitter      │  │ bash-parser-     │  │ Pattern matching │
│ Command parsing  │  │   file-ops       │  │ No filesystem    │
│ Structure        │  │ bash-parser-     │  │ Pure algorithm   │
│ extraction       │  │   variables      │  │                  │
│                  │  │                  │  │                  │
│ Fully reusable   │  │ Fully reusable   │  │ Fully reusable   │
│ (1 minor issue)  │  │                  │  │                  │
└──────────────────┘  └──────────────────┘  └──────────────────┘
