# Design

## Context

The current scope-shell-tools implementation uses simple regex-based parsing (`split-string` on metacharacters) to extract the base command from bash strings. This approach has known limitations:

1. **Pipeline bypass**: Only the first command in pipelines is validated, allowing `ls | xargs rm` to bypass the deny list for `rm`
2. **No file path validation**: Commands operate in scoped directories, but file arguments can reference absolute paths outside scope (e.g., `cat /etc/passwd` in `/workspace`)
3. **Coarse-grained categorization**: Commands are categorized as read-only or safe-write based on command name alone, not on actual operations performed
4. **No cloud awareness**: Cloud authentication commands (aws-vault, gcloud, az) are treated like any other command

Meanwhile, bash-parser has matured in `config/experiments/bash-parser/` with 683 passing tests, proving its capability to:
- Parse complex bash syntax with tree-sitter (pipelines, chains, command substitution, redirects)
- Extract semantic information via plugins (file operations, cloud auth, security patterns)
- Track parse completeness and coverage metrics
- Handle edge cases documented in comprehensive behavioral specs

This design integrates bash-parser into scope-shell-tools as the validation layer, replacing regex-based parsing and enabling operation-specific path scoping, pipeline validation, and cloud authentication detection.

**Current state**:
- `jf/gptel-bash--parse-command` - regex-based, returns single command string
- Directory validation only - no file path extraction
- Single-command validation - pipelines bypassed
- No cloud auth detection
- No coverage metrics

**Target state**:
- `jf/bash-parse` + `jf/bash-extract-semantics` - tree-sitter-based, returns full AST + semantic extraction
- File path validation - operation-specific scoping (read/write/execute/modify)
- All-command validation - full pipeline and chain validation
- Cloud auth detection and policy enforcement
- Coverage metrics for debugging and warnings

## Goals / Non-Goals

**Goals:**

1. **Graduate bash-parser** from experiments to production (`config/bash-parser/`)
2. **Integrate bash-parser** into scope-shell-tools validation pipeline
3. **Close security gaps**: Validate all pipeline commands, validate file paths against scope
4. **Enable operation-specific scoping**: Separate paths.read, paths.write, paths.execute, paths.modify
5. **Add cloud authentication awareness**: Detect and enforce policy for AWS/GCP/Azure commands
6. **Provide rich error messages**: Include parse details, coverage metrics, actionable suggestions
7. **Maintain test quality**: Migrate 683 bash-parser tests, add 100% spec scenario coverage
8. **Break cleanly**: No backward compatibility - v4 schema required, old validation removed

**Non-Goals:**

1. **Automatic v3 → v4 migration**: Users must manually update scope.yml (migration guide provided, but no automated tool)
2. **Scope expansion UI changes**: request_scope_expansion remains unchanged, works with new validation errors
3. **Bash-parser enhancements**: Using bash-parser as-is, not adding new capabilities during integration
4. **Command execution changes**: Timeout, output truncation, and execution logic unchanged
5. **Preset registration changes**: Existing preset system works with v4 schema without modification

## Decisions

### Decision 1: Bash-parser location - Top-level production module

**Choice**: Move bash-parser to `config/bash-parser/` (top-level)

**Alternatives considered**:
- `config/gptel/bash-parser/` - Co-locate with gptel as subsystem
- `config/core/bash-parser/` - Place in core as shared utility

**Rationale**:
- Bash-parser is a substantial module (14 files, 683 tests, plugin architecture) deserving first-class status
- Not gptel-specific - could be used by other Emacs modules for bash analysis
- Top-level placement signals graduation from experiment to production
- Clear namespace: `jf/bash-*` functions are bash-parser, `jf/gptel-*` functions are gptel

**Implementation**:
```bash
git mv config/experiments/bash-parser config/bash-parser
# Update load paths in config/bash-parser/bash-parser.el
# Add to module load order in init.org or gptel.org
```

### Decision 2: Validation pipeline architecture - Sequential stages with early exit

**Choice**: Seven sequential validation stages, each can reject command and return error immediately

**Alternatives considered**:
- Parallel validation stages with result aggregation (complex, harder to debug)
- Single monolithic validation function (hard to test, poor separation of concerns)
- Plugin-based validation system (over-engineered for current needs)

**Rationale**:
- Sequential stages are easy to understand, test, and debug
- Early exit on first failure provides fastest feedback to LLM
- Each stage is independently testable (unit test per stage)
- Clear ordering: parse → extract → validate structure → validate semantics
- Matches mental model: "Can I parse this? Can I understand it? Is it allowed? Do the paths check out?"

**Implementation**:
```elisp
(defun jf/gptel-scope--validate-command-semantics (command directory scope-config)
  "Seven-stage validation pipeline with early exit on failure."
  (let* ((parsed (jf/bash-parse command))
         (semantics (jf/bash-extract-semantics parsed)))

    ;; Stage 1: Parse completeness
    (when-let ((error (jf/gptel-scope--check-parse-completeness parsed scope-config)))
      (cl-return-from jf/gptel-scope--validate-command-semantics error))

    ;; Stage 2: Pipeline command extraction
    (let ((commands (jf/gptel-scope--extract-pipeline-commands parsed)))

      ;; Stage 3: Command categorization (all pipeline commands)
      (when-let ((error (jf/gptel-scope--validate-pipeline-commands commands scope-config)))
        (cl-return-from jf/gptel-scope--validate-command-semantics error))

      ;; Stage 4: File operations extraction and validation
      (when-let ((file-ops (plist-get (plist-get semantics :domains) :filesystem)))
        (when-let ((error (jf/gptel-scope--validate-file-operations file-ops directory scope-config)))
          (cl-return-from jf/gptel-scope--validate-command-semantics error)))

      ;; Stage 5: Cloud auth detection and policy enforcement
      (when-let ((cloud-auth (plist-get (plist-get semantics :domains) :cloud-auth)))
        (when-let ((error (jf/gptel-scope--validate-cloud-auth cloud-auth scope-config)))
          (cl-return-from jf/gptel-scope--validate-command-semantics error)))

      ;; Stage 6: Coverage check (warning only, doesn't block)
      (jf/gptel-scope--check-coverage-threshold semantics scope-config)

      ;; Stage 7: All validations passed
      nil)))  ; nil = success, proceed to execution
```

### Decision 3: Operation-specific path validation - Hierarchical permission model

**Choice**: Hierarchy: execute ⊂ modify ⊂ write ⊃ read, with explicit path lists for each

**Alternatives considered**:
- Flat model: Independent read/write/execute/modify lists (confusing overlap, hard to configure)
- Single "allowed paths" list with operation annotations (verbose, hard to read)
- Permission levels: level 1 = read, level 2 = write, level 3 = execute (non-intuitive)

**Rationale**:
- Write permission logically includes read (you can read what you can write)
- Modify is a subset of write (in-place editing is still writing)
- Execute requires explicit permission (running code is higher risk than reading)
- Explicit lists are clear: "These paths allow read, these allow execute"
- Easy to audit: grep for `paths.execute` to see what's executable

**Permission hierarchy**:
```
Operation    | Requires paths      | Why
-------------|---------------------|----------------------------------------
:read        | .read OR .write     | Write includes read capability
:write       | .write              | Must explicitly allow writing
:modify      | .modify OR .write   | In-place edit is subset of write
:execute     | .execute            | Execution requires explicit permission
:delete      | .write              | Deletion is write operation
```

**Implementation**:
```elisp
(defun jf/gptel-scope--validate-operation (operation path scope-config)
  "Check if OPERATION on PATH is allowed by SCOPE-CONFIG."
  (let ((paths (plist-get scope-config :paths)))
    ;; Deny takes precedence
    (when (jf/gptel-scope--matches-any-pattern path (plist-get paths :deny))
      (cl-return-from jf/gptel-scope--validate-operation
        (list :error "path_denied" :path path :operation operation)))

    ;; Operation-specific validation
    (pcase operation
      (:read
       (or (jf/gptel-scope--matches-any-pattern path (plist-get paths :read))
           (jf/gptel-scope--matches-any-pattern path (plist-get paths :write))))

      (:write
       (jf/gptel-scope--matches-any-pattern path (plist-get paths :write)))

      (:modify
       (or (jf/gptel-scope--matches-any-pattern path (plist-get paths :modify))
           (jf/gptel-scope--matches-any-pattern path (plist-get paths :write))))

      (:execute
       (jf/gptel-scope--matches-any-pattern path (plist-get paths :execute)))

      (:delete
       (jf/gptel-scope--matches-any-pattern path (plist-get paths :write))))))
```

### Decision 4: Schema v4 loading - Defaults for missing sections, strict validation for present sections

**Choice**: Missing sections get safe defaults, present sections must be valid or error

**Alternatives considered**:
- Require all sections (forces users to specify every field, verbose)
- Lenient validation (accept any structure, risk runtime errors)
- Warn on missing sections but proceed (unclear behavior when sections missing)

**Rationale**:
- Gradual adoption: Users can add new sections incrementally
- Safe defaults: Missing `cloud` defaults to "warn" mode (informative but not blocking)
- Fail fast: Invalid values in present sections caught at load time, not runtime
- Clear contract: If you specify `cloud.auth_detection`, it must be valid

**Default values**:
```elisp
(defconst jf/gptel-scope-v4-defaults
  '(:paths (:read ()
            :write ()
            :execute ()
            :modify ()
            :deny ())
    :cloud (:auth-detection "warn"
            :allowed-providers ())
    :security (:enforce-parse-complete t
               :max-coverage-threshold 0.8))
  "Default values for missing scope.yml v4 sections.")
```

**Validation rules**:
```elisp
(defun jf/gptel-scope--validate-schema-v4 (config)
  "Validate SCOPE-CONFIG structure. Error if present sections invalid."
  ;; If cloud section present, auth-detection must be valid
  (when-let ((cloud (plist-get config :cloud)))
    (let ((mode (plist-get cloud :auth-detection)))
      (unless (member mode '("allow" "warn" "deny"))
        (error "Invalid cloud.auth_detection: %s (must be allow/warn/deny)" mode))))

  ;; If security section present, threshold must be in range
  (when-let ((security (plist-get config :security)))
    (let ((threshold (plist-get security :max-coverage-threshold)))
      (when threshold
        (unless (and (numberp threshold) (>= threshold 0.0) (<= threshold 1.0))
          (error "Invalid security.max_coverage_threshold: %s (must be 0.0-1.0)" threshold)))))

  ;; Paths must be lists if present
  (when-let ((paths (plist-get config :paths)))
    (dolist (section '(:read :write :execute :modify :deny))
      (when-let ((value (plist-get paths section)))
        (unless (listp value)
          (error "Invalid paths.%s: must be list, got %s" section (type-of value))))))

  config)
```

### Decision 5: Pipeline validation - Extract all commands, validate each independently

**Choice**: Parse pipeline structure to extract all commands, validate each against bash_tools categories

**Alternatives considered**:
- Regex split on pipes (fragile, can't handle nested structures)
- Validate only dangerous commands in pipelines (inconsistent, confusing rules)
- Blacklist approach - only check for known-bad commands (easy to bypass)

**Rationale**:
- Bash-parser provides pipeline structure reliably (handles nested command substitutions, complex chains)
- Consistent security model: ALL commands must pass categorization
- Closes documented bypass: "ls | xargs rm" now properly rejects "rm"
- Clear error messages: "Command 'rm' at pipeline position 1 is denied"

**Implementation**:
```elisp
(defun jf/gptel-scope--extract-pipeline-commands (parsed-command)
  "Extract all command names from PARSED-COMMAND pipeline/chain."
  (let ((commands '()))
    ;; Primary command
    (push (plist-get parsed-command :command-name) commands)

    ;; Pipeline commands (if present)
    (when-let ((pipeline (plist-get parsed-command :pipeline)))
      (dolist (segment pipeline)
        (push (plist-get segment :command-name) commands)))

    ;; Command chains (semicolon, &&, ||)
    (when-let ((chain (plist-get parsed-command :chain)))
      (dolist (segment chain)
        (push (plist-get segment :command-name) commands)))

    (nreverse commands)))

(defun jf/gptel-scope--validate-pipeline-commands (commands scope-config)
  "Validate each command in COMMANDS list against SCOPE-CONFIG categories."
  (let ((bash-tools (plist-get scope-config :bash-tools))
        (position 0))
    (dolist (cmd commands)
      (let ((result (jf/gptel-scope--validate-command-category cmd bash-tools)))
        (when (plist-get result :error)
          ;; Add pipeline position to error
          (cl-return-from jf/gptel-scope--validate-pipeline-commands
            (plist-put result :pipeline-position position))))
      (cl-incf position))
    nil))  ; All commands valid
```

### Decision 6: Parse completeness handling - Strict by default, configurable relaxation

**Choice**: Reject incomplete parses by default (`security.enforce_parse_complete: true`), allow override for permissive environments

**Alternatives considered**:
- Always reject incomplete parses (too strict, may break valid-but-complex commands)
- Always allow with warning (too permissive, defeats purpose of semantic validation)
- Never check parse completeness (ignores valuable signal from bash-parser)

**Rationale**:
- Incomplete parse = bash-parser couldn't understand command = can't validate semantics properly
- Strict default is safer: If we can't parse it, we can't validate it
- Override available for edge cases: Some environments may need to allow unparseable commands
- Warning always included: Even in permissive mode, user sees parse incompleteness

**Implementation**:
```elisp
(defun jf/gptel-scope--check-parse-completeness (parsed scope-config)
  "Check if PARSED command is complete. Error or warn based on SCOPE-CONFIG."
  (unless (plist-get parsed :parse-complete)
    (let* ((security (plist-get scope-config :security))
           (enforce (plist-get security :enforce-parse-complete))
           (errors (plist-get parsed :parse-errors)))

      (if enforce
          ;; Strict mode: reject
          (list :error "incomplete_parse"
                :parse-errors errors
                :message "Cannot validate command: parse incomplete"
                :suggestion "Simplify command or set security.enforce_parse_complete: false")

        ;; Permissive mode: warn but proceed
        (list :warning "parse_incomplete"
              :parse-errors errors
              :message "Warning: Command not fully parsed, validation may be incomplete")))))
```

### Decision 7: Test organization - Capability-based test files with 100% scenario coverage

**Choice**: One test file per capability (scope-validation-file-paths, scope-validation-pipelines, etc.), every spec scenario mapped to test

**Alternatives considered**:
- Single large test file for all validation logic (hard to navigate, slow to run subset)
- Test files by component (bash-parser tests, validation tests) - (poor mapping to specs)
- No scenario mapping (risk missing edge cases documented in specs)

**Rationale**:
- Capability-based organization matches spec structure (easy to find tests for a requirement)
- 100% scenario coverage ensures all documented behavior is tested
- Grep-able scenario references: `grep "Scenario: specs/scope-validation" test/*.el` shows coverage
- Characterization tests capture legacy behavior before refactoring (safety net for breaking changes)

**Test file structure** (from architecture.md):
```
config/gptel/tools/test/
├── test-helper.el                           # Shared helpers
├── test-scope-validation-pipeline.el        # End-to-end pipeline tests
├── test-scope-validation-file-paths.el      # 45+ scenarios from spec
├── test-scope-validation-pipelines.el       # 25+ scenarios from spec
├── test-scope-validation-cloud-auth.el      # 30+ scenarios from spec
├── test-scope-schema-v4.el                  # 35+ scenarios from spec
├── test-scope-shell-tools-integration.el    # Integration tests
└── test-scope-shell-tools-legacy.el         # Characterization tests
```

**Scenario mapping example**:
```elisp
(ert-deftest test-file-path-validation-read-operation-in-scope ()
  "Scenario: specs/scope-validation-file-paths/spec.md § 'Read operation matches paths.read'"
  (let ((scope-config (test-make-scope-config
                       :paths (:read ("/workspace/**"))))
        (file-op '(:operation :read
                   :path "/workspace/file.txt"
                   :absolute-path "/workspace/file.txt")))
    (should-validate-file-operation file-op scope-config)))
```

### Decision 8: Migration strategy - Big bang replacement, no incremental rollout

**Choice**: Single PR that moves bash-parser, replaces validation logic, requires v4 schema - no dual code paths

**Alternatives considered**:
- Feature flag for new validation (complex, dual maintenance burden)
- Gradual rollout (validate with both systems, compare results) - (double validation overhead)
- Keep old validation as fallback (confusing, which system is actually enforcing?)

**Rationale**:
- Clean break is simpler than dual code paths
- Breaking change already decided (no backward compatibility goal)
- Single validation system avoids confusion about which rules apply
- Easier to test: Only one behavior to validate, not two modes
- Forces v4 schema adoption: Clear migration point for users

**Migration checklist**:
1. Move bash-parser to production location
2. Update module load order (bash-parser before gptel)
3. Replace validation functions in scope-shell-tools.org
4. Update all example presets to v4 schema
5. Run full test suite (683 bash-parser + ~200 new integration tests)
6. Update CLAUDE.md with v4 schema documentation
7. Merge and deploy

**No rollback plan**: Once merged, old validation is gone. Rollback requires git revert of entire change.

## Risks / Trade-offs

### Risk 1: Breaking change requires manual v4 migration
**Severity**: High (affects all users)

**Impact**: All scope.yml files must be manually updated to v4 schema. Commands requiring execute/modify paths will be denied until users add those sections.

**Mitigation**:
- Provide clear migration guide in PR description and CLAUDE.md
- Update all example presets first as migration templates
- Schema validation errors include helpful messages pointing to missing sections
- Safe defaults: Missing sections don't crash, they deny operations (fail-closed)

### Risk 2: Tree-sitter parsing overhead (~10-50ms per command)
**Severity**: Low (acceptable latency)

**Impact**: Command validation slower than regex (1-2ms → 10-50ms). In practice, this is negligible compared to network latency and LLM inference time.

**Mitigation**:
- Accept this cost - 50ms is imperceptible to users
- Bash-parser is already optimized (683 tests run in <10 seconds)
- No caching needed - commands execute infrequently (user-driven, not hot path)

### Risk 3: Pipeline validation may break existing workflows
**Severity**: Medium (intentional breaking change)

**Impact**: Users relying on pipeline bypass (e.g., `find | xargs rm` to clean files) will see commands rejected.

**Mitigation**:
- This is a security fix, not a bug - pipeline bypass was documented as intentional limitation
- Error messages clearly identify which pipeline command failed
- Users can request scope expansion for needed commands
- Migration guide includes examples of updating scope.yml to allow pipeline commands

### Risk 4: Incomplete parse strictness may be too aggressive
**Severity**: Medium (usability vs security trade-off)

**Impact**: Commands with edge-case syntax that bash-parser can't fully parse will be rejected, even if safe.

**Mitigation**:
- `security.enforce_parse_complete: false` escape hatch available
- Bash-parser is mature (683 tests, handles most real-world bash)
- Error messages suggest simplifying command or disabling strict mode
- Can whitelist specific commands via scope expansion if parse incomplete but trusted

### Risk 5: Test suite size may slow down development
**Severity**: Low (manageable)

**Impact**: 883 total tests (683 bash-parser + 200 integration) take ~60 seconds to run full suite.

**Mitigation**:
- Most development runs subset: `make test-file FILE=test-scope-validation-pipeline.el`
- CI runs full suite, not local development
- Test discovery allows focused runs: `make test-pattern PATTERN='^test-file-path-'`
- Parallel test execution possible if suite grows beyond 60 seconds

### Risk 6: Complex validation pipeline harder to debug
**Severity**: Low (offset by better error messages)

**Impact**: Seven validation stages means more places for bugs to hide. Errors in middle stages may not be obvious.

**Mitigation**:
- Each stage independently testable (unit tests per stage)
- Error messages include stage context (parse_incomplete, path_out_of_scope, cloud_auth_denied)
- Coverage metrics in responses help debug semantic extraction issues
- Integration tests validate full pipeline end-to-end

## Migration Plan

### Phase 1: Pre-merge validation
1. **Characterization tests**: Capture current validation behavior in test-scope-shell-tools-legacy.el
2. **Bash-parser test migration**: Verify all 683 tests pass after move to config/bash-parser/
3. **Integration test development**: Implement all scenario-mapped tests (100% coverage target)
4. **Example preset updates**: Update system-explorer.md, bash-tools-example.md to v4 schema

### Phase 2: Code migration
1. **Move bash-parser**: `git mv config/experiments/bash-parser config/bash-parser`
2. **Update load paths**: Modify bash-parser.el and init.org/gptel.org module loader
3. **Replace validation functions**: Rewrite scope-shell-tools.org validation pipeline
4. **Add schema v4 loading**: Implement load-cloud-config, load-security-config, validate-schema-v4
5. **Remove old code**: Delete jf/gptel-bash--parse-command and regex-based logic

### Phase 3: Testing and validation
1. **Run characterization tests**: Verify intentional behavior changes documented
2. **Run integration tests**: Verify all new capabilities work (file paths, pipelines, cloud auth)
3. **Run bash-parser tests**: Verify parser quality maintained after move
4. **Manual testing**: Test with example presets in real gptel sessions

### Phase 4: Documentation
1. **Update CLAUDE.md**: Document v4 schema, new validation behavior, migration guide
2. **Update specs**: Sync openspec/specs/gptel/bash-tools.md with implementation
3. **PR description**: Include migration checklist for users
4. **Breaking change notice**: Clearly mark as breaking in commit message

### Rollback Strategy
**Rollback is git revert** - no incremental rollback possible.

If critical issues discovered post-merge:
1. `git revert <merge-commit>` - Restores old validation logic
2. Users revert scope.yml to v3 schema
3. Fix issues in feature branch
4. Re-merge when stable

**No dual-mode fallback**: Maintaining both old and new validation is out of scope. Clean revert or move forward.

## Open Questions

### Q1: Should execute permission be more granular?
**Question**: Should `paths.execute` distinguish between different script types (shell scripts vs Python vs binaries)?

**Options**:
- Keep simple: All executable paths in one list
- Granular: `paths.execute-shell`, `paths.execute-python`, `paths.execute-binary`

**Current decision**: Keep simple. If granularity needed, can add in future without breaking v4 schema (backward compatible extension).

### Q2: Should coverage warnings be enabled by default?
**Question**: Should low coverage warnings show in every response, or only when coverage < threshold?

**Options**:
- Always show coverage metrics (verbose but informative)
- Only warn when < threshold (cleaner responses)
- Never warn, only include in error responses (minimal noise)

**Current decision**: Only warn when < threshold. Coverage metrics always included in error responses for debugging. Can be overridden with `max_coverage_threshold: 0.0` (never warn) or `1.0` (always warn).

### Q3: Should file path validation resolve shell variables?
**Question**: If command is `cat $HOME/file.txt`, should we expand `$HOME` before validation?

**Options**:
- Resolve variables using shell expansion (security risk: command injection)
- Resolve variables using static context (complex, limited utility)
- Treat variables as literals (current decision)

**Current decision**: No variable expansion. Security boundary is literal paths only. If user wants `$HOME/file.txt` allowed, they must specify the expanded path in scope.yml or use wildcard patterns. Variable expansion opens command injection risk and complexity not justified by use cases.

### Q4: Should deny list support patterns or only literal commands?
**Question**: Should `bash_tools.deny` support glob patterns like `rm*` or only exact matches?

**Options**:
- Literal only: `["rm", "rmdir", "sudo"]` (simple, explicit)
- Pattern support: `["rm*", "sudo*"]` (flexible, but may catch unintended commands)

**Current decision**: Literal only for now. Deny list is security-critical - explicit is safer than pattern matching. If pattern support needed, can add `bash_tools.deny_patterns` in future.
