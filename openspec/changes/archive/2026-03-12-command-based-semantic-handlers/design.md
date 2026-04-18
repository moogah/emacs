# Design: Command-Based Semantic Handlers

## Context

The current bash-parser semantic analysis uses a **monolithic database** (`jf/bash-command-file-semantics`) that maps commands to extraction specifications. This database is interpreted by category-specific extraction code, creating artificial boundaries between semantic domains.

**Current state:**
- 60+ commands defined in single database variable
- Filesystem semantics mixed with cloud auth patterns
- Complex commands (git, tar, find) have custom handler functions
- Plugin system exists but underutilized (only cloud-auth uses it)
- No clean way to express multi-domain commands

**Constraints:**
- MUST preserve `jf/bash-extract-semantics` public API (gptel dependency)
- MUST pass all existing scope validation tests
- NO backwards compatibility with old database
- Existing test suite must continue working

**Stakeholders:**
- gptel scope validation system (primary consumer)
- bash-parser maintainers
- Future command handler contributors

## Goals / Non-Goals

**Goals:**

1. **Command-centric architecture**: Each command owns its semantic extraction logic
2. **Multi-domain support**: Commands contribute to multiple semantic domains naturally
3. **Auto-discovery**: Drop a file in `commands/`, it's automatically loaded
4. **Interface preservation**: gptel scope validation works without changes
5. **Testability**: Test individual command handlers in isolation

**Non-Goals:**

1. **Backwards compatibility**: No support for old database format
2. **Migration utilities**: No automated migration of existing commands
3. **Parser changes**: Parsing logic (`jf/bash-parse`) remains unchanged
4. **Scope validation changes**: No modifications to validation system
5. **Performance optimization**: Not a performance-focused refactor

## Decisions

### Decision 1: Command files over declarative database

**Choice:** Individual `.el` files per command rather than declarative database entries.

**Rationale:**
- **Locality**: All `aws` knowledge in one file makes it easier to understand and modify
- **Extensibility**: Adding Terraform support is just creating `terraform.el`
- **Multi-domain**: Easy for a command to register multiple handlers for different domains
- **Testability**: Test `aws.el` handlers independently with dedicated test file

**Alternatives considered:**
- **Keep database, add plugin layer**: Would maintain asymmetry between filesystem (database) and cloud-auth (plugin)
- **Hybrid approach**: Keep database for simple commands, handlers for complex ones → Creates two patterns to maintain
- **Single mega-plugin**: One plugin for all commands → Loses locality benefits

**Trade-off:** More files to manage (60+ command files vs 1 database file), but better organization and modularity.

### Decision 2: Hash table registry over alist

**Choice:** Use hash table for command → domain → handlers mapping.

**Rationale:**
- O(1) lookup by command name (though realistically not a bottleneck)
- Nested hash tables for domain mapping: `{cmd => {domain => [handlers]}}`
- More scalable than nested alists as command count grows

**Implementation:**
```elisp
(defvar jf/bash-command-handlers (make-hash-table :test 'equal)
  "Registry: {command-name => {domain => [handler-fns]}}")
```

**Alternatives considered:**
- **Alist**: Simpler but O(n) lookup, awkward nested structure
- **Flat hash table**: `{(cmd . domain) => [handlers]}` → Harder to iterate by command

**Trade-off:** Slightly more complex than alist, but better performance characteristics and cleaner API.

### Decision 3: Auto-discovery via directory scan

**Choice:** `commands/index.el` scans directory and loads all `.el` files.

**Rationale:**
- **Zero registration**: Drop `terraform.el` in directory, done
- **Error isolation**: Failed loads don't block other handlers
- **Simple implementation**: `directory-files` + loop + `load`

**Implementation approach:**
```elisp
(defun jf/bash-commands--discover-and-load ()
  (let ((files (directory-files commands-dir t "\\.el$")))
    (dolist (file files)
      (unless (string-match-p "index\\.el$" file)
        (condition-case err
            (load file nil t)
          (error (message "Failed to load %s: %s" file err)))))))
```

**Alternatives considered:**
- **Manual registration**: Require each command explicitly in index → More control but defeats the purpose
- **Autoload**: Use Emacs autoload mechanism → Overly complex for this use case
- **Lazy loading**: Load handlers on-demand → Premature optimization, adds complexity

**Trade-off:** All handlers loaded upfront (one-time cost), but simpler mental model.

### Decision 4: Preserve plugin infrastructure

**Choice:** Keep `bash-parser-plugins.el` for universal plugins, integrate with command handlers.

**Rationale:**
- **Universal concerns**: Redirections, pipes, variables apply to ALL commands
- **Domain vs command**: Plugins for cross-cutting concerns, handlers for command-specific
- **No duplication**: Don't replicate redirection handling in every command handler

**Integration approach:**
- Universal plugins execute first (redirections, variables)
- Command handlers execute next (command-specific semantics)
- Results merged by domain in `jf/bash-extract-semantics`

**Alternatives considered:**
- **Remove plugins entirely**: Would require every command to handle redirections
- **Commands become plugins**: Conceptual mismatch (command ≠ domain)
- **Separate systems**: Keep them isolated → Harder to get unified results

**Trade-off:** Two systems to maintain (plugins + handlers), but each serves a clear purpose.

### Decision 5: Inline test data

**Choice:** Define `parsed-command` test data inline in each test rather than shared fixtures.

**Rationale:**
- **Simplicity**: Each test is self-contained and readable
- **Flexibility**: Easy to customize input for specific scenarios
- **No indirection**: Don't need to look up fixture definitions

**Example:**
```elisp
(it "extracts read operation from cat command"
  (let ((parsed-command '(:command-name "cat"
                          :positional-args ("file.txt")
                          :tokens [...])))
    (expect (jf/bash-command-cat--filesystem-handler parsed-command)
            :to-have-operations ...)))
```

**Alternatives considered:**
- **Shared fixture module**: `test-helper.el` with common inputs → DRY but adds complexity
- **Test data files**: JSON/elisp data files → Overkill for simple structures

**Trade-off:** Some duplication of similar `parsed-command` structures, but tests are more understandable.

### Decision 6: Buttercup for new tests

**Choice:** Use Buttercup (BDD framework) for all command handler tests.

**Rationale:**
- Modern describe/it/expect syntax
- Built-in before-each/after-each for setup
- Spy system for mocking
- Better for scenario-driven tests from specs
- Aligns with project direction (Buttercup preferred for new code)

**Test structure:**
```elisp
(describe "aws command handler"
  (describe "filesystem extraction"
    (it "identifies local file reads in s3 cp"
      (expect result :to-match expected))))
```

**Alternatives considered:**
- **ERT**: Would work but less expressive, no built-in mocking
- **Mix both**: Unnecessarily complex, prefer consistency

**Trade-off:** Developers need to know Buttercup syntax, but it's already used elsewhere in the project.

### Decision 7: Migration strategy - clean break

**Choice:** Remove old database entirely, no backwards compatibility.

**Rationale:**
- **Interface preservation**: `jf/bash-extract-semantics` stays same, that's what matters
- **Simpler**: No compatibility shims, no fallback logic
- **Clear direction**: Forces complete migration, no lingering old code

**Migration approach:**
1. Implement new system completely
2. Migrate all commands to handler files
3. Remove `jf/bash-command-file-semantics` and related functions
4. Verify scope validation tests pass
5. Done

**Alternatives considered:**
- **Dual-mode support**: Try handlers, fall back to database → Technical debt, delays removal
- **Gradual migration**: Keep both systems → Complexity during transition

**Trade-off:** All-or-nothing approach requires full implementation before merge, but cleaner long-term.

## Risks / Trade-offs

### Risk: Breaking scope validation system

**Likelihood:** Medium
**Impact:** Critical (blocks gptel usage)

**Mitigation:**
- **Contract testing**: Test that `jf/bash-extract-semantics` returns exact structure
- **Integration tests**: Run full scope validation test suite before merge
- **Careful review**: Double-check interface preservation in architecture doc

**Detection:** Existing test suite will catch this immediately.

### Risk: Handler registration order affecting results

**Likelihood:** Low
**Impact:** Medium (inconsistent extraction)

**Mitigation:**
- **Document ordering**: Handlers execute in registration order within same domain
- **Independent handlers**: Handlers should not depend on execution order
- **Test coverage**: Test scenarios with multiple handlers for same domain

**Example:** Two `:filesystem` handlers for `aws` should both contribute operations independently.

### Risk: Performance degradation from many small files

**Likelihood:** Low
**Impact:** Low (one-time initialization cost)

**Mitigation:**
- **Lazy loading not needed**: 60 command files load quickly on modern machines
- **Benchmark if needed**: Can add lazy loading later if proven necessary

**Reality check:** Loading 60 small elisp files is negligible compared to Emacs init time.

### Risk: Incomplete command migration

**Likelihood:** Medium
**Impact:** High (missing command support)

**Mitigation:**
- **Audit database**: List all commands in current database
- **Create checklist**: Track migration status for each command
- **Test coverage**: Verify each migrated command has tests

**Process:** Systematic migration with verification, not ad-hoc.

### Trade-off: File count vs locality

**Choice:** 60+ command files instead of 1 database file.

**Benefits:**
- Better locality (all `aws` logic in one place)
- Independent testability
- Clear ownership
- Easy to add new commands

**Costs:**
- More files to navigate
- Directory clutter
- Slightly longer initialization

**Verdict:** Benefits outweigh costs for maintainability.

### Trade-off: Flexibility vs simplicity

**Choice:** Simple hash table registry instead of plugin-based architecture for commands.

**Benefits:**
- Easy to understand (just a registry)
- Straightforward lookup
- No plugin complexity for command-specific logic

**Costs:**
- Less flexible than full plugin system
- Commands can't have predicates/priorities like plugins

**Verdict:** Commands don't need plugin-level complexity; simple registry is sufficient.

## Open Questions

### Q1: Naming convention for command files?

**Options:**
- `commands/cat.el` (simple, matches command name)
- `commands/bash-command-cat.el` (namespaced, more explicit)

**Recommendation:** Use simple names (`cat.el`). The `commands/` directory provides namespace.

### Q2: How to organize git subcommands?

**Options:**
- Single file: `commands/git.el` with subcommand handlers
- Directory: `commands/git/clone.el`, `commands/git/checkout.el`

**Recommendation:** Start with single file. Split only if it grows unwieldy (>500 lines).

### Q3: Should handlers be composable/chainable?

**Current design:** Handlers are independent, results merged at orchestration level.

**Alternative:** Handlers could call other handlers, pass results along.

**Recommendation:** Keep handlers independent for simplicity. Composition can be added later if needed.

### Q4: How to handle command aliases (egrep → grep)?

**Options:**
- Duplicate handlers for each alias
- Register same handler under multiple names
- Add alias resolution in lookup

**Recommendation:** Register same handler function under multiple command names. Simple and explicit.

**Example:**
```elisp
(jf/bash-register-command-handler :command "egrep" :domain :filesystem :handler #'jf/bash-grep-handler)
(jf/bash-register-command-handler :command "grep" :domain :filesystem :handler #'jf/bash-grep-handler)
```

### Q5: Error handling for handler execution?

**Current design:** `condition-case` in orchestrator logs errors and continues.

**Question:** Should handlers be able to signal semantic errors (e.g., "cannot parse this command")?

**Recommendation:** Handlers return `nil` for "not applicable", never error. Orchestrator logs unexpected errors.
