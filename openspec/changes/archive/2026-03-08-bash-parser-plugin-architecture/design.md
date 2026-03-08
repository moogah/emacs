## Context

The bash-parser currently extracts only filesystem operations, limiting its utility for security analysis and AI context understanding. Commands operate across multiple domains (cloud, databases, network, containers), and security decisions require understanding the full scope of impact.

**Current State:**
- `jf/bash-extract-file-operations` extracts filesystem operations only
- No visibility into coverage - cannot determine what was not understood
- Monolithic extraction logic - difficult to extend to new domains
- No token-level tracking - cannot report blindspots

**Constraints:**
- Breaking API change acceptable for clean design
- Must maintain high performance (parser called frequently)
- Must support incremental domain addition without core changes
- Emacs Lisp environment (no multithreading, limited concurrency)

**Stakeholders:**
- gptel scope system (primary consumer - needs migration)
- Security analysis (benefits from multi-domain understanding)
- Future plugin authors (need clear, stable protocol)

## Goals / Non-Goals

**Goals:**
- Enable multi-domain semantic extraction through plugin architecture
- Provide coverage metrics to identify blindspots in command understanding
- Support incremental capability growth (add domains without changing core)
- Maintain high performance despite multiple plugin execution
- Clean API design (breaking change acceptable)

**Non-Goals:**
- Language-specific parsing (python -c, node -e) - not bash commands
- Cloud resource extraction beyond authentication (future extension)
- Real-time command execution or sandboxing
- Backward compatibility with old API (deprecated, not maintained)
- Plugin hot-reloading or dynamic plugin discovery

## Decisions

### Decision 1: Breaking API Change - Unified Semantic Extraction

**Choice:** Replace `jf/bash-extract-file-operations` with `jf/bash-extract-semantics` (breaking change)

**Rationale:**
- Clean design trumps compatibility for internal API
- Single consumer (gptel) makes migration tractable
- Avoids dual API maintenance burden
- New API communicates multi-domain purpose

**Alternatives Considered:**
- **Parallel APIs**: Keep old API, add new API
  - Rejected: Confuses consumers, dual maintenance, unclear deprecation path
- **Extend old API**: Add optional domain parameter
  - Rejected: API name implies filesystem-only, incompatible with multi-domain result structure

**Migration Plan:**
- Update gptel to use new API
- Mark old API as deprecated with docstring warning
- Remove old API in future release after migration verified

### Decision 2: Plugin Registry with Priority-Based Execution

**Choice:** Global plugin registry (`jf/bash-semantic-plugins`) with priority field for ordering

**Rationale:**
- Simple registration model - add to list with metadata
- Priority enables conflict resolution if needed
- Registration order fallback provides predictability
- Easy to inspect registry for debugging

**Alternatives Considered:**
- **Dependency-based ordering**: Plugins declare dependencies on other plugins
  - Rejected: Over-engineered for initial use case, adds topological sort complexity
- **Hook-based system**: Use Emacs advice or hook mechanism
  - Rejected: Less explicit, harder to debug, unnecessary indirection

**Implementation:**
```elisp
(defvar jf/bash-semantic-plugins '()
  "List of registered semantic extraction plugins.
  Each entry: (:name symbol :priority int :extractor fn :predicates (fns))")

(defun jf/bash-register-plugin (&rest args)
  "Register plugin. Args: :name :priority :extractor :predicates"
  (push args jf/bash-semantic-plugins))
```

### Decision 3: Shared Token Claiming Strategy

**Choice:** Multiple plugins can claim the same token (shared claiming)

**Rationale:**
- Compositional semantics - `scp` is both network AND filesystem
- Coverage metric: "% of tokens claimed by at least one plugin"
- Avoids artificial conflicts between complementary domains
- Simpler plugin implementation - no coordination needed

**Alternatives Considered:**
- **Exclusive claiming**: First plugin to claim token "owns" it
  - Rejected: Prevents compositional semantics, requires arbitrary priority decisions
- **Weighted claiming**: Plugins "bid" on tokens with confidence scores
  - Rejected: Over-engineered, adds complexity without clear benefit

**Coverage Calculation:**
```elisp
;; Token is "claimed" if ANY plugin claimed it
(claimed-unique (seq-uniq (seq-mapcat #'get-claimed-ids plugin-results)))
(coverage-ratio (/ (float (length claimed-unique)) total-tokens))
```

### Decision 4: Token Granularity - Word-Level with Metadata

**Choice:** One token per syntactic element (word-level) with rich metadata (type, position, associations)

**Rationale:**
- Balances precision and complexity
- Enables accurate coverage reporting without overwhelming detail
- Position tracking enables source mapping and visualization
- Type classification supports coverage-by-type diagnostics

**Alternatives Considered:**
- **Character-level tokens**: Every character is a token
  - Rejected: Excessive granularity, huge token lists, minimal benefit
- **Coarse tokens**: Command-level aggregates (e.g., "argument-block")
  - Rejected: Insufficient precision for coverage reporting, loses blindspot visibility

**Token Structure:**
```elisp
(:id 5
 :type :flag-arg
 :value "/aws/lambda/logs"
 :start 80
 :end 110
 :pipe-segment 1
 :associated-flag 7)
```

### Decision 5: Plugin Result as Struct vs Plist

**Choice:** Use `cl-struct` for plugin results (not plist)

**Rationale:**
- Type safety - compile-time checks via struct accessors
- Self-documenting - struct definition documents fields
- Performance - struct access slightly faster than plist
- Emacs convention for structured data

**Alternatives Considered:**
- **Plist**: Flexible but lacks type safety
  - Rejected: Typos in keywords silently return nil, harder to debug
- **Hash table**: Fast lookup but heavyweight
  - Rejected: Overkill for 4-field structure

**Implementation:**
```elisp
(cl-defstruct jf/bash-plugin-result
  domain
  operations
  claimed-token-ids
  metadata)
```

### Decision 6: Parse Completeness as Boolean Flag

**Choice:** Simple `:parse-complete t/nil` flag in parsed output

**Rationale:**
- Boolean clearly communicates success/failure
- Error details separate in `:parse-errors` list
- Consumers can gate semantic extraction on parse success
- Simple to implement and test

**Alternatives Considered:**
- **Confidence score**: 0.0-1.0 indicating partial parse success
  - Rejected: Ambiguous - what does 0.6 mean? Binary is clearer for syntax parsing
- **Status enum**: `:complete`, `:partial`, `:failed`
  - Rejected: Partial parsing not implemented, enum adds unnecessary states

**Usage Pattern:**
```elisp
(if (plist-get parsed :parse-complete)
    (jf/bash-extract-semantics parsed)
  (error "Cannot extract semantics from incomplete parse: %s"
         (plist-get parsed :parse-errors)))
```

### Decision 7: Filesystem Plugin as First-Class Plugin (No Special Case)

**Choice:** Refactor filesystem extraction as regular plugin, no special treatment in orchestrator

**Rationale:**
- Proves plugin architecture works for complex domain
- Sets pattern for future plugins
- No special cases in orchestrator simplifies code
- Dogfooding ensures plugin protocol is sufficient

**Alternatives Considered:**
- **Builtin filesystem extraction**: Orchestrator always runs filesystem extraction, plugins for other domains
  - Rejected: Creates two-tier system, violates plugin abstraction
- **Composite plugin**: Filesystem plugin wraps other plugins
  - Rejected: Inverts responsibility, confuses orchestration logic

**Migration Strategy:**
1. Implement `jf/bash-plugin-filesystem` wrapping existing extraction logic
2. Add token claiming logic to plugin
3. Register plugin with high priority
4. Deprecate `jf/bash-extract-file-operations` as thin wrapper calling plugin

### Decision 8: Cloud Auth Plugin Focuses on Authentication Only

**Choice:** Cloud auth plugin extracts authentication scope (accounts, profiles, projects) but NOT resource operations

**Rationale:**
- Bounded scope - authentication patterns are finite and well-defined
- High value - auth context is critical for security decisions
- Defers complexity - resource extraction (S3 buckets, log groups) is larger scope, future work
- Clear separation - auth is cross-cutting, resources are service-specific

**Alternatives Considered:**
- **Full cloud extraction**: Auth + resources in one plugin
  - Rejected: Too large for initial implementation, mixes concerns
- **No cloud plugin**: Wait until resource extraction is designed
  - Rejected: Authentication is immediately valuable, provides second plugin for testing architecture

**Supported Patterns:**
```elisp
(defvar jf/bash-cloud-auth-patterns
  '((aws-vault . (:subcommand "exec" :account-arg :first-positional :provider :aws))
    (aws . (:profile-flag "--profile" :region-flag "--region" :provider :aws))
    (gcloud . (:project-flag "--project" :zone-flag "--zone" :provider :gcp))
    (az . (:subscription-flag "--subscription" :provider :azure))))
```

### Decision 9: Error Isolation via Condition-Case

**Choice:** Wrap each plugin invocation in `condition-case` to isolate errors

**Rationale:**
- Single plugin failure shouldn't break entire extraction
- Partial results better than no results
- Error logged but doesn't propagate
- Debugging: `:plugin-results` shows which plugins succeeded

**Alternatives Considered:**
- **Fail fast**: Let plugin errors propagate
  - Rejected: Loses results from successful plugins, poor user experience
- **Plugin sandboxing**: Run plugins in isolated Emacs process
  - Rejected: Massive overhead, unnecessary for pure function plugins

**Implementation:**
```elisp
(dolist (plugin applicable-plugins)
  (condition-case err
      (when-let ((result (funcall (plist-get plugin :extractor) parsed-command)))
        (push result plugin-results))
    (error
     (message "Plugin %s error: %s" (plist-get plugin :name) err))))
```

### Decision 10: Coverage Visualization as Separate Function

**Choice:** `jf/bash-visualize-coverage` is separate from `jf/bash-calculate-coverage`

**Rationale:**
- Separation of concerns - calculation vs presentation
- Programmatic consumers use calculation, humans use visualization
- Visualization can evolve without affecting calculation
- Testing: easier to test calculation without dealing with output formatting

**Alternatives Considered:**
- **Combined function**: Calculate and print in one function
  - Rejected: Mixes concerns, harder to test, inflexible for consumers
- **No visualization**: Just return coverage data structure
  - Rejected: Loses debugging utility, coverage data is hard to interpret raw

**Output Format:**
```
Parse Complete: ✓ Yes
Semantic Coverage: 70% (7/10 tokens)

Coverage by Token Type:
  command-name:        100% (2/2)  ████████████████████
  positional-arg:      100% (3/3)  ████████████████████
  flag:                50%  (1/2)  ██████████

Unclaimed Tokens:
  [Token 9]  :flag  "--start-time"  Position: 111-123
```

## Risks / Trade-offs

### Risk: Plugin Execution Performance
**Risk:** Multiple plugins executing sequentially could slow extraction for complex commands

**Mitigation:**
- Plugins are pure functions with minimal overhead
- Predicate filtering reduces unnecessary plugin invocations
- Typical commands have <10 plugins and <100 tokens - acceptable performance
- Future optimization: memoize plugin results if same command parsed multiple times

**Trade-off:** Accepted - multi-domain semantics worth small performance cost

### Risk: Token Tracking Memory Overhead
**Risk:** Token inventory adds memory overhead to parsed command structures

**Mitigation:**
- Token list is lightweight (10-20 tokens typical, 100 max for complex commands)
- Each token is small plist (~100 bytes)
- Total overhead: ~1-10KB per parsed command - acceptable for transient data

**Trade-off:** Accepted - coverage visibility requires token tracking

### Risk: Breaking API Change Disrupts Consumers
**Risk:** Removing `jf/bash-extract-file-operations` breaks existing code

**Mitigation:**
- Single known consumer (gptel) - controlled migration
- Deprecation warning in docstring
- Backward-compatible wrapper provided temporarily
- Migration tracked as part of this change

**Trade-off:** Accepted - clean API design worth migration cost for single consumer

### Risk: Plugin Protocol Ossification
**Risk:** Plugin protocol too rigid, prevents future plugin needs

**Mitigation:**
- Protocol is minimal - domain, operations, claimed-token-ids, metadata
- Metadata field is extensible plist for plugin-specific data
- Early plugins (filesystem, cloud-auth) validate protocol sufficiency
- Breaking changes to protocol acceptable before ecosystem grows

**Trade-off:** Accept risk - protocol can evolve, no external plugins yet

### Risk: Coverage Metrics Misinterpretation
**Risk:** Users misinterpret coverage ratio (e.g., expect 100% always)

**Mitigation:**
- Documentation explains coverage is about semantic understanding, not parsing
- Unclaimed tokens listed explicitly - users see what's missing
- Coverage-by-type breakdown identifies specific gaps
- Visualization emphasizes blindspot identification over percentage

**Trade-off:** Accepted - metric is useful despite interpretation risk

### Risk: Shared Claiming Complexity
**Risk:** Multiple plugins claiming same token could cause confusion

**Mitigation:**
- Documented as intentional design - compositional semantics
- Coverage calculation handles shared claiming correctly
- Plugin results list shows which plugins claimed which tokens (debugging)
- Examples in documentation show expected behavior (e.g., scp as network + filesystem)

**Trade-off:** Accepted - compositional semantics worth potential confusion

### Risk: Cloud Auth Plugin Scope Creep
**Risk:** Plugin grows to include resource extraction, becomes unwieldy

**Mitigation:**
- Explicit non-goal: resource extraction deferred to future plugin
- Plugin name (`bash-parser-cloud-auth`) reinforces limited scope
- Pattern database focuses on authentication flags only
- Future: separate `bash-parser-cloud-resources` plugin

**Trade-off:** Accepted - clear boundaries prevent scope creep

## Migration Plan

### Phase 1: Parser Enhancement (Week 1)
1. Add token tracking to `bash-parser-core.org`
   - Modify parse functions to emit token structs
   - Add `:tokens` field to parsed command output
2. Add `:parse-complete` and `:parse-errors` fields
3. Update existing parser tests to verify token inventory
4. **Verification**: All existing parser tests pass with new fields present

### Phase 2: Plugin Infrastructure (Week 1-2)
1. Create `bash-parser-plugins.org` module
   - Define `jf/bash-plugin-result` struct
   - Implement plugin registry (`jf/bash-semantic-plugins`)
   - Implement `jf/bash-register-plugin`
   - Implement `jf/bash-extract-semantics` orchestrator
2. Create `bash-parser-coverage.org` module
   - Implement `jf/bash-calculate-coverage`
   - Implement `jf/bash-visualize-coverage`
3. Write tests for plugin infrastructure
4. **Verification**: Plugin registration and orchestration tests pass

### Phase 3: Filesystem Plugin Refactor (Week 2)
1. Create `jf/bash-plugin-filesystem` function
   - Wrap existing `jf/bash-extract-file-operations` logic
   - Add token claiming logic
   - Return `jf/bash-plugin-result` struct
2. Register filesystem plugin with high priority
3. Deprecate `jf/bash-extract-file-operations` (mark with warning, keep as wrapper)
4. Write tests for filesystem plugin
5. **Verification**: All existing filesystem extraction tests pass through plugin

### Phase 4: Cloud Auth Plugin (Week 2-3)
1. Create `bash-parser-cloud-auth.org` module
   - Define `jf/bash-cloud-auth-patterns` database
   - Implement `jf/bash-plugin-cloud-auth`
   - Add token claiming logic
2. Register cloud auth plugin
3. Write tests for cloud auth extraction
4. **Verification**: Cloud auth tests pass for AWS, GCP, Azure patterns

### Phase 5: Integration & Migration (Week 3)
1. Update gptel scope system to use `jf/bash-extract-semantics`
   - Replace calls to `jf/bash-extract-file-operations`
   - Handle new result structure (`:domains` alist)
   - Use coverage metrics for security decisions
2. Integration tests for gptel + new API
3. **Verification**: gptel scope system works with new semantic extraction

### Rollback Strategy
- Keep old `jf/bash-extract-file-operations` as wrapper during migration
- If issues found, revert gptel to old API temporarily
- Plugin architecture independent of old API - can revert consumer without removing plugins
- Parser enhancement (tokens) is additive - can be ignored by old API

## Open Questions

### Q1: Should coverage threshold trigger warnings?
**Question:** Should extraction warn or error if coverage falls below threshold (e.g., <50%)?

**Options:**
- A) No automatic warnings - coverage is informational only
- B) Warning at configurable threshold (user can set)
- C) Error if coverage < 50% (fail-safe for security)

**Recommendation:** Start with (A), add (B) if users request it. Avoid (C) - too restrictive.

### Q2: Plugin discovery mechanism?
**Question:** Should we add automatic plugin discovery (e.g., load all `bash-parser-*-plugin.el` files)?

**Current:** Manual registration via `jf/bash-register-plugin`

**Recommendation:** Manual registration sufficient for now. Discovery adds complexity (where to search, naming conventions, errors). Revisit if plugin ecosystem grows beyond 5-10 plugins.

### Q3: Plugin priority conflicts?
**Question:** What happens if two plugins claim same token with conflicting semantics?

**Current:** Both claims recorded, no conflict resolution

**Recommendation:** Document as acceptable (compositional semantics). If real conflicts emerge, add confidence scoring or explicit conflict resolution. No evidence this is needed yet.

### Q4: Coverage visualization format?
**Question:** Should visualization support multiple output formats (ASCII art, JSON, org-mode table)?

**Current:** ASCII art only

**Recommendation:** Start with ASCII art for human debugging. Add JSON output if programmatic consumers need it. Org-mode table optional enhancement.

### Q5: Token type extensibility?
**Question:** Should plugins be able to define custom token types?

**Current:** Fixed set of token types defined by parser

**Recommendation:** No - token types are syntactic categories from parser, not semantic. Plugins operate on existing token types. Custom semantics go in operation plists, not token types.
