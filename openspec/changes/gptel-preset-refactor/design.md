## Context

Current preset configuration handling is monolithic - `jf/gptel--load-preset-from-file` performs file I/O, YAML parsing, type conversions (backend names → structs, model strings → symbols, tool names → structs), and buffer-local variable assignment in a single function. Similar mixing occurs in serialization via `jf/gptel--normalize-preset-for-serialization`.

This makes it difficult to test individual concerns, reason about data transformations, or reuse components. Error messages are generic because failures can occur at multiple layers. The internal representation (transient plist during operations) is not explicitly documented, leading to inconsistent handling (e.g., tools as arrays vs plists vs structs).

Affected areas:
- **Sessions persistence** (`config/gptel/sessions/commands.org`) - loads presets during session initialization
- **PersistentAgent tool** (`config/gptel/tools/persistent-agent.org`) - loads agent presets with strict isolation
- **Scope validation** (`config/gptel/scope/scope-core.org`) - parses preset config for scope checks

Current dependencies:
- `gptel--known-backends` alist for backend lookups
- `gptel-get-tool` for tool struct resolution
- `yaml-parse-string` for parsing
- `gptel-backend-name`, `gptel-tool-name` for serialization

## Goals / Non-Goals

**Goals:**
- Separate concerns: Parse (file → data), Resolve (names → objects), Apply (objects → buffer-local vars), Serialize (objects → data)
- Define internal representation contract: Parsed plist (strings/numbers/lists), Resolved plist (structs/symbols/lists)
- Improve error handling: Each phase signals errors with appropriate context (file path, field name, buffer)
- Maintain compatibility: No changes to preset.md file format, no breaking changes to user-facing behavior
- Enable testing: Each phase is independently testable without file I/O or gptel state

**Non-Goals:**
- Change preset.md file format (YAML frontmatter + markdown body is canonical)
- Eliminate gptel-agent package dependency (optional stretch goal, not required)
- Support plist/alist tool formats in preset files (standardize on array format during serialization)
- Cache parsed/resolved presets (file is source of truth, buffer-local vars hold applied state)
- Add new preset fields or configuration options

## Decisions

### Decision 1: Three-phase architecture (Parse → Resolve → Apply)

**Chosen approach:** Separate parsing, resolution, and application into distinct functions with explicit intermediate representations.

**Rationale:**
- Parsing phase (`jf/gptel-preset-parse`) outputs pure-data plist: `:backend "Claude"`, `:model "claude-opus-4-5"`, `:tools ("tool1" "tool2")`
- Resolution phase (`jf/gptel-preset-resolve`) converts names to objects using gptel registries: `:backend <struct>`, `:model 'symbol`, `:tools (<struct> <struct>)`
- Application phase (`jf/gptel-preset-apply`) sets buffer-local variables from resolved plist
- Each phase has single responsibility, clear input/output contract, and isolated error handling

**Alternatives considered:**
- Keep monolithic function, add internal helper functions: Still mixes concerns, hard to test phases independently
- Two-phase (Parse+Resolve, Apply): Loses ability to inspect parsed data before resolution, complicates error attribution
- Four-phase (separate file I/O from parsing): Adds complexity without benefit, file I/O is trivial

**Trade-offs:**
- Slightly more function calls during preset loading (negligible performance impact)
- More explicit code (benefit: easier to understand data flow)

### Decision 2: Resolver functions for each type (backend, model, tools)

**Chosen approach:** Create dedicated converter functions:
- `jf/gptel-preset--resolve-backend`: name string → gptel-backend struct
- `jf/gptel-preset--resolve-model`: name string → symbol
- `jf/gptel-preset--resolve-tools`: name strings → tool struct list

**Rationale:**
- Centralizes type conversion logic with consistent error handling
- Idempotent (if value is already correct type, return unchanged)
- Tools resolver filters out missing tools, logs warnings, returns nil if all missing
- Backend resolver signals error (backend is required, no sensible default)

**Alternatives considered:**
- Generic `resolve-field` function with type dispatch: Overengineered, each type has different semantics
- Inline resolution in `jf/gptel-preset-resolve`: Harder to test, duplicates error handling logic
- Let gptel handle coercion: gptel APIs don't provide consistent error behavior

**Trade-offs:**
- Additional functions in namespace (benefit: testability, clarity)
- Backend resolution errors stop the pipeline (intentional: fail fast on invalid config)

### Decision 3: Serialization as inverse of resolution

**Chosen approach:** Create `jf/gptel-preset-serialize` that converts resolved plist back to pure-data form:
- Backend struct → name string via `(gptel-backend-name backend)`
- Model symbol → string via `(symbol-name model)`
- Tool structs → name strings via `(gptel-tool-name tool)`

**Rationale:**
- Mirrors resolution phase conceptually (makes code easier to understand)
- Normalizes internal plist tool format (`:tool1 (:allowed t)`) to canonical array format
- Used during session initialization when writing preset.md files
- Phase separation: serialization is pure data transformation, no file I/O

**Alternatives considered:**
- Inline serialization logic where needed: Duplicates normalization logic across call sites
- Store parsed format in buffer-local vars: Complicates buffer state, loses type safety
- Skip serialization, write buffer-local vars directly: Breaks canonical preset.md format

**Trade-offs:**
- Serialization + resolution creates round-trip overhead (acceptable: only used during session creation)
- Normalizes tool format unconditionally (benefit: eliminates plist/alist variants in files)

### Decision 4: Explicit intermediate representation contract

**Chosen approach:** Document plist structure at each phase in docstrings and spec:

**Parsed plist** (output of `jf/gptel-preset-parse`):
```elisp
(:backend "Claude"
 :model "claude-opus-4-5"
 :temperature 1.0
 :tools ("read_file" "write_file_in_scope")
 :system "message text...")
```

**Resolved plist** (output of `jf/gptel-preset-resolve`):
```elisp
(:backend <gptel-backend struct>
 :model 'claude-opus-4-5
 :temperature 1.0
 :tools (<gptel-tool struct> <gptel-tool struct>)
 :system "message text...")
```

**Rationale:**
- Makes data flow explicit (no hidden state or assumptions)
- Enables validation at phase boundaries
- Documents exactly what each function accepts/returns
- Guides callers on when to use which representation

**Alternatives considered:**
- Defstruct for each representation: Overengineered for short-lived plists, adds overhead
- Keep representation implicit: Current problem - leads to inconsistent handling
- Use alists instead of plists: Breaks convention in gptel codebase (uses plists)

**Trade-offs:**
- Documentation burden (benefit: clarity, correctness)
- No static type checking (acceptable: elisp convention, runtime errors clear)

### Decision 5: Update call sites to use new architecture

**Chosen approach:** Replace monolithic calls with three-phase pipeline:

**Sessions persistence** (`jf/gptel--session--open-hook`):
```elisp
(let* ((parsed (jf/gptel-preset-parse preset-path))
       (resolved (jf/gptel-preset-resolve parsed)))
  (jf/gptel-preset-apply resolved))
```

**PersistentAgent** (`jf/gptel-persistent-agent-call`):
```elisp
;; In agent buffer context
(let* ((parsed (jf/gptel-preset-parse agent-preset-path))
       (resolved (jf/gptel-preset-resolve parsed)))
  (jf/gptel-preset-apply resolved))
```

**Rationale:**
- Makes data flow visible at call sites (see parse → resolve → apply clearly)
- Each call site can choose how to handle errors (sessions: graceful fallback, agents: fail fast)
- Preserves existing buffer-local variable semantics
- No changes to when/where presets are loaded

**Alternatives considered:**
- Single `load-preset` wrapper function: Hides phase boundaries, harder to customize error handling
- Update call sites incrementally: Risk of inconsistent behavior during transition
- Add compatibility shim: Technical debt, delays cleanup

**Trade-offs:**
- More verbose at call sites (benefit: explicit, debuggable)
- Risk of call sites using wrong phase (mitigated by clear naming, docstrings)

### Decision 6: Error handling strategy per phase

**Chosen approach:**

**Parse errors** (missing file, invalid YAML):
- Return `nil` from `jf/gptel-preset-parse`
- Log error with file path and parse failure details
- Call sites check for nil and handle gracefully (sessions: use defaults, agents: fail)

**Resolution errors** (unknown backend, missing tools):
- Backend resolution: Signal error immediately (backend is required)
- Tool resolution: Log warnings for missing tools, return nil if all missing, return partial list if some succeed
- Model resolution: Never fails (string → symbol always succeeds)

**Application errors** (buffer-local var setting):
- Rare (only if buffer no longer exists)
- Let error propagate with buffer context in message

**Rationale:**
- Parse failures are user-correctable (fix YAML syntax, create missing file) → graceful
- Backend failures are show-stoppers (no sensible default) → fail fast
- Tool failures are partial (some tools missing is acceptable) → warn and continue
- Application failures are system errors (buffer gone) → propagate

**Alternatives considered:**
- Signal errors in all phases: Too aggressive, breaks existing graceful fallback behavior
- Return nil in all phases: Loses error context, silent failures
- Return error objects instead of nil: Overengineered, elisp convention is nil + logging

**Trade-offs:**
- Inconsistent error handling across phases (intentional: matches failure semantics)
- Call sites must check for nil after parsing (benefit: explicit control)

## Risks / Trade-offs

**[Risk: Call sites using wrong phase or skipping resolution]**
→ Mitigation: Clear naming (`parse`/`resolve`/`apply`), comprehensive docstrings, validate at phase boundaries (type checks for plist structure)

**[Risk: Performance regression from multiple function calls]**
→ Mitigation: Profile preset loading in typical session workflow. Expect negligible impact (parsing and resolution are not hot paths). Consider caching if profiling shows issues (unlikely).

**[Risk: Inconsistent error handling across call sites]**
→ Mitigation: Document error handling strategy in spec and docstrings. Unit test each phase's error behavior. Ensure sessions and agents handle parse failures correctly.

**[Risk: Breaking changes if internal plist structure changes]**
→ Mitigation: Internal representation is not public API. Document contract in spec. Update all call sites atomically in same commit. Add validation helpers if needed.

**[Risk: Tool format normalization breaks existing workflow]**
→ Mitigation: Serialization eliminates plist/alist variants, standardizes on array format. This is intentional cleanup, not a bug. Verify during testing that preset files round-trip correctly.

**[Risk: Forgetting to update all call sites during refactor]**
→ Mitigation: Grep for `jf/gptel--load-preset-from-file` and `jf/gptel--apply-session-preset` to find all usages. Update in single atomic commit. Test sessions and agents workflows after refactor.

## Migration Plan

**Implementation order:**

1. **Create phase functions** in new file `config/gptel/preset-configuration.org`:
   - `jf/gptel-preset-parse` (file → parsed plist)
   - `jf/gptel-preset--resolve-backend` (name → struct)
   - `jf/gptel-preset--resolve-model` (name → symbol)
   - `jf/gptel-preset--resolve-tools` (names → struct list)
   - `jf/gptel-preset-resolve` (parsed plist → resolved plist)
   - `jf/gptel-preset-apply` (resolved plist → buffer-local vars)
   - `jf/gptel-preset-serialize` (resolved plist → parsed plist)

2. **Update sessions persistence** (`config/gptel/sessions/commands.org`):
   - Replace `jf/gptel--load-preset-from-file` with parse → resolve → apply pipeline
   - Replace `jf/gptel--normalize-preset-for-serialization` with `jf/gptel-preset-serialize`
   - Test: Open existing session, verify preset loads correctly, verify system message applied

3. **Update PersistentAgent** (`config/gptel/tools/persistent-agent.org`):
   - Replace preset loading with parse → resolve → apply pipeline
   - Ensure agent buffer context wraps all three phases
   - Test: Create agent, verify configuration isolated from parent, verify tools captured correctly

4. **Remove deprecated functions**:
   - Delete `jf/gptel--load-preset-from-file`
   - Delete `jf/gptel--apply-session-preset`
   - Delete `jf/gptel--normalize-preset-for-serialization`
   - Grep codebase to ensure no remaining references

5. **Load new module** in `config/gptel/gptel.org`:
   - Add `(jf/load-module "gptel/preset-configuration")` after skills modules, before sessions modules

**Testing strategy:**

- **Unit tests** (if test framework added): Test each resolver function with valid/invalid inputs
- **Integration tests**:
  - Open session with existing preset → verify backend/model/tools/system message loaded
  - Create new session → verify preset.md written in canonical format
  - Create agent → verify configuration isolated, tools captured from agent buffer
  - Parse preset with missing tools → verify warnings logged, partial list returned
  - Parse preset with invalid YAML → verify nil returned, error logged with file path

**Rollback strategy:**

If issues discovered post-merge:
- Revert commit atomically (all call sites updated together)
- Old functions still exist in git history, can be restored
- No data migration required (preset.md format unchanged)
- Buffer-local variable semantics unchanged

**Deployment:**

- Merge to development worktree, test locally
- No runtime configuration changes required
- No user-facing changes (internal refactoring only)
- Can deploy incrementally across worktrees (development → testing → production)

## Open Questions

None - design is complete and ready for implementation.
