## Components

### Session Constants Module (`config/gptel/sessions/constants.org`)
**Responsibility**: Define centralized constants for session file names, directories, and configuration.

**Changes**:
- Update `jf/gptel-session--context-file` from `"session.md"` to `"session.org"`
- Maintain backward compatibility by supporting both extensions in detection logic

### Session Commands Module (`config/gptel/sessions/commands.org`)
**Responsibility**: Provide user-facing commands for session creation and auto-initialization hooks.

**Changes**:
- Update auto-init hook to detect both `.org` and `.md` file extensions
- Modify pattern matching regex to support `session.org` and `session.md`
- Update initial content generation to use org-mode heading (`* Session\n\n`) for new sessions
- Maintain markdown handling for legacy sessions

**Key Functions**:
- `jf/gptel--auto-init-session-buffer` - Hook that detects and initializes session files
- `jf/gptel-persistent-session` - Interactive session creation command
- `jf/gptel--create-session-core` - Core session creation logic

### Session Branching Module (`config/gptel/sessions/branching.org`)
**Responsibility**: Handle branch creation with context truncation and metadata preservation.

**Changes**:
- Rewrite Local Variables handling to support both formats:
  - Emacs Local Variables format (`# gptel-model: ...`) for `.org` files
  - HTML comment format (`<!-- gptel-model: ... -->`) for `.md` files
- Update context truncation to preserve file format
- Maintain bounds filtering logic (format-agnostic)

**Key Functions**:
- `jf/gptel-branch-session` - Interactive branch creation command
- `jf/gptel--copy-truncated-context` - Context truncation with Local Variables
- `jf/gptel--filter-bounds` - Bounds filtering (unchanged)

### Activities Integration Module (`config/gptel/sessions/activities-integration.org`)
**Responsibility**: Auto-create persistent sessions when activities are created.

**Changes**:
- Update initial content to use org-mode format
- Modify worktree path storage (consider keeping HTML comments for compatibility)
- Use `.org` extension for new activity sessions

### Org-Roam Tools Module (`config/gptel/tools/org-roam-tools.org`)
**Responsibility**: Provide tools for creating and managing org-roam nodes from sessions.

**Changes**:
- **Simplification**: Remove `jf/markdown-to-org` conversion from `create_roam_node` tool
- Content from org-mode sessions is already in org-mode format (converted by gptel)
- Maintain tool interface unchanged

### Persistent Agent Module (`config/gptel/tools/persistent-agent.org`)
**Responsibility**: Create and manage sub-agent sessions.

**Changes**:
- Remove hardcoded `(markdown-mode)` call
- Let Emacs auto-detect major mode from file extension
- Create agent sessions with `.org` extension

### Preset Registration Module (`config/gptel/preset-registration.org`)
**Responsibility**: Parse and register presets from `.md` files.

**Changes** (Optional):
- Add support for `:mode` parameter in preset frontmatter
- Extract mode configuration to `jf/gptel-preset--mode-defaults` alist
- Use mode default during session creation to determine file format

## Interfaces

### Component Interactions

```
┌─────────────────────────────────────────────────────────────┐
│                    User Actions                              │
└────────────┬────────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────────┐
│  Session Commands (jf/gptel-persistent-session)             │
│  - Creates session directory                                 │
│  - Determines file format (org-mode default)                │
│  - Calls filesystem utilities                                │
└────────────┬────────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────────┐
│  Session Filesystem (jf/gptel--create-session-core)         │
│  - Creates directory structure                               │
│  - Writes session.org with initial content                   │
│  - Uses constants for file names                             │
└────────────┬────────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────────┐
│  Emacs file-visiting (find-file)                            │
│  - Auto-detects org-mode from .org extension               │
│  - Triggers find-file-hook                                   │
└────────────┬────────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────────┐
│  Auto-Init Hook (jf/gptel--auto-init-session-buffer)        │
│  - Detects session.org or session.md pattern               │
│  - Sets buffer-local session variables                       │
│  - Enables gptel-mode                                        │
│  - Applies preset configuration                              │
└────────────┬────────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────────┐
│  gptel Package (upstream)                                    │
│  - Detects buffer is org-mode                               │
│  - Enables gptel-org-convert-response (default)             │
│  - Converts LLM markdown → org-mode automatically           │
└─────────────────────────────────────────────────────────────┘
```

### Key Data Flows

**Session Creation Flow**:
1. User invokes `jf/gptel-persistent-session`
2. Commands module determines file format (org-mode default, or from preset mode config)
3. Filesystem creates directory with `session.org` file
4. Initial content written as org-mode heading
5. Metadata written to `metadata.yml` (format-independent)
6. Scope configuration written to `scope.yml` (format-independent)

**Session Opening Flow**:
1. User opens `session.org` file (via any method)
2. Emacs auto-detects org-mode from extension
3. find-file-hook triggers auto-init
4. Auto-init detects session pattern (`.org` or `.md`)
5. Sets buffer-local variables from path and metadata
6. Enables gptel-mode
7. gptel enables markdown-to-org conversion automatically

**Branching Flow**:
1. User invokes `jf/gptel-branch-session` in session buffer
2. Branching module detects source file format (`.org` or `.md`)
3. Creates new branch directory with same format
4. Copies truncated content preserving format
5. Writes Local Variables in format-appropriate syntax
6. Opens new branch file (auto-detects mode from extension)

### Public APIs

**Session Creation**:
```elisp
(jf/gptel-persistent-session &optional session-name preset-name)
;; Creates new session, returns session-id
;; Now creates session.org by default
```

**Branch Creation**:
```elisp
(jf/gptel-branch-session &optional branch-name)
;; Creates new branch from current session
;; Inherits file format from parent
```

**Session Auto-Init** (internal):
```elisp
(jf/gptel--auto-init-session-buffer)
;; Hook function, detects and initializes both .org and .md sessions
```

### External Integrations

**gptel Package (Upstream)**:
- **Input**: Receives user prompts from org-mode buffers
- **Output**: Returns markdown responses
- **Conversion**: Automatically converts markdown to org-mode via `gptel-org-convert-response`
- **Integration Point**: No changes needed - feature already exists and is enabled by default

**activities Package**:
- **Input**: Activity creation event
- **Output**: Creates persistent gptel session with `.org` file
- **Integration Point**: Update activities-integration module to use org-mode format

**org-roam Package**:
- **Input**: Content from session buffers
- **Output**: Creates org-roam node files
- **Integration Point**: Simplified - no conversion needed (content already org-mode)

## Boundaries

### In Scope

**File Format Changes**:
- Session file extension: `.md` → `.org`
- Initial content format: markdown heading → org-mode heading
- Local Variables format: HTML comments → Emacs Local Variables

**Auto-Detection**:
- Dual-format session detection (`.org` and `.md`)
- Fast-path optimization for both extensions
- Pattern matching for both formats

**Backward Compatibility**:
- Existing `.md` sessions continue to work
- HTML comment Local Variables still supported for markdown
- No forced migration of existing sessions

**Integration Updates**:
- Activities integration uses org-mode format
- Org-roam tools simplified (remove redundant conversion)
- Persistent agent uses org-mode format

### Out of Scope

**Forced Migration**:
- No automatic conversion of existing `.md` files to `.org`
- Users can manually rename if desired, but not required

**Format Deprecation**:
- Markdown format not deprecated
- Both formats supported indefinitely
- No removal of markdown-specific code paths

**LLM Output Format**:
- LLMs continue to output markdown (natural format)
- No attempt to instruct LLMs to output org-mode syntax
- Rely on gptel's converter (well-tested, mature)

**Custom Conversion Logic**:
- No new markdown-to-org conversion code (use gptel's built-in)
- Remove existing custom conversion (in org-roam tools)

### Internal vs External

**Internal** (our code):
- Session creation and initialization
- Auto-detection hooks
- Local Variables format handling
- File format selection logic
- Activities integration
- Org-roam tools simplification

**External** (dependencies):
- gptel's markdown-to-org conversion (already exists)
- Emacs Local Variables reading (built-in)
- org-mode major mode (built-in)
- YAML parsing for metadata files (unchanged)

## Testing Approach

### Test Framework

**Buttercup (preferred)** or **ERT (legacy)** - Dual framework support.

**Recommendation**: Use Buttercup for new tests in this change. Buttercup provides:
- BDD-style syntax with `describe`/`it`/`expect`
- Built-in setup/teardown hooks (`before-each`, `after-each`)
- Spy system for mocking and call verification
- Better test organization for integration tests

**ERT is acceptable** for simple unit tests or when:
- Maintaining consistency with nearby test files
- Simple assertion-based tests with minimal setup
- Quick test development

**Both frameworks**:
- Integrate with existing test infrastructure (`make test`, `./bin/run-tests.sh`)
- Support interactive and batch testing
- Available throughout the configuration codebase

### Test Organization

**Location**: Tests co-located with modules in `config/gptel/sessions/test/`

**Structure**:
```
config/gptel/sessions/
├── commands.org/el
├── commands-test.el           ← Extend with org-mode tests
├── branching.org/el
├── branching-test.el          ← Extend with org-mode tests
├── filesystem.org/el
├── filesystem-test.el         ← Extend with org-mode tests
└── test/
    └── (additional test helpers if needed)
```

**Approach**: Extend existing test files with org-mode variants rather than creating new files. This keeps tests co-located with the modules they test and makes it easy to compare markdown vs org-mode behavior.

### Naming Conventions

**Test File Naming**: `<module>-test.el` (existing convention)

**Test Function Naming**: Add `-org` suffix to create org-mode variants of existing tests.

**Pattern**:
```elisp
;; Existing markdown test
(ert-deftest test-session-creation ()
  "Test session creation with markdown format."
  ...)

;; New org-mode variant
(ert-deftest test-session-creation-org ()
  "Test session creation with org-mode format."
  ...)
```

**Benefits**:
- Clear relationship between markdown and org-mode tests
- Easy to find corresponding tests for each format
- Alphabetically grouped by functionality

**Compatibility Tests**: Use explicit naming for dual-format tests.
```elisp
(ert-deftest test-session-dual-format-detection ()
  "Test that both .org and .md sessions are detected."
  ...)

(ert-deftest test-session-format-inheritance-branching ()
  "Test that branches inherit parent format."
  ...)
```

### Running Tests

**All tests**:
```bash
make test                                    # Run all tests
./bin/run-tests.sh                           # Alternative CLI
```

**Session-specific tests**:
```bash
make test-directory DIR=config/gptel/sessions/test
./bin/run-tests.sh -d config/gptel/sessions
```

**Specific module tests**:
```bash
# Run tests for commands module
./bin/run-tests.sh -p '^test-session-'

# Run only org-mode tests
./bin/run-tests.sh -p '.*-org$'
```

**Individual test**:
```elisp
;; In Emacs, with test file loaded:
M-x ert RET test-session-creation-org RET
```

**Test Infrastructure**: Use existing Makefile and `run-tests.sh` infrastructure (no changes needed).

### Test Patterns

**Setup Pattern**: Use helper functions to create temporary session directories.

```elisp
(defun test-helper-create-org-session (session-id)
  "Create a temporary org-mode session for testing."
  (let* ((session-dir (make-temp-file "gptel-test-" t))
         (branch-dir (expand-file-name "branches/main" session-dir))
         (session-file (expand-file-name "session.org" branch-dir)))
    (make-directory branch-dir t)
    (with-temp-file session-file
      (insert "* Session\n\n"))
    session-dir))

(defun test-helper-create-md-session (session-id)
  "Create a temporary markdown session for testing."
  ;; Similar pattern for markdown
  ...)
```

**Cleanup Pattern**: Use `unwind-protect` to ensure cleanup.

```elisp
(ert-deftest test-session-creation-org ()
  (let ((session-dir nil))
    (unwind-protect
        (progn
          (setq session-dir (test-helper-create-org-session "test-session"))
          ;; Test assertions here
          ...)
      ;; Cleanup
      (when (and session-dir (file-exists-p session-dir))
        (delete-directory session-dir t)))))
```

**Assertion Pattern**: Use ERT's assertion macros.

```elisp
(should (file-exists-p session-file))
(should (string= (file-name-extension session-file) "org"))
(should (string-match-p "^\\* Session" (with-temp-buffer
                                          (insert-file-contents session-file)
                                          (buffer-string))))
```

**Mocking/Stubbing**: Minimal mocking needed (mostly file system operations).
- Use temporary directories for isolation
- Use `cl-letf` to override functions if needed (e.g., preset lookup)

### Scenario Mapping

**Strategy**: Map spec scenarios to test functions with 1:1 or many:1 relationship.

**Mapping Pattern**:
```elisp
(ert-deftest test-scenario-name ()
  "Scenario: specs/org-mode-sessions/spec.md § 'New org-mode session creation'"
  ;; Test implementation
  ...)
```

**Scenario Categories**:

1. **File Format Tests** (`specs/org-mode-sessions/spec.md`):
   - `test-session-creation-org` → "New org-mode session creation"
   - `test-session-file-extension-org` → "New org-mode session creation"
   - `test-activities-session-org-format` → "Activities session uses org-mode format"

2. **Auto-Detection Tests** (`specs/org-mode-sessions/spec.md`):
   - `test-auto-init-org-detection` → "Auto-initialize org-mode session"
   - `test-auto-init-md-detection` → "Auto-initialize markdown session (legacy)"
   - `test-fast-path-dual-format` → "Fast-path detection for both formats"

3. **Conversion Tests** (`specs/org-mode-sessions/spec.md`):
   - `test-gptel-markdown-to-org-conversion` → "LLM markdown response converted to org-mode"
   - `test-mixed-markup-handling` → "Mixed markdown/org markup handled gracefully"

4. **Local Variables Tests** (`specs/sessions-branching/spec.md`):
   - `test-local-variables-org-format` → "Writing bounds to org-mode Local Variables"
   - `test-local-variables-md-format` → "Writing bounds to markdown Local Variables (legacy)"
   - `test-local-variables-read-both` → "Reading Local Variables from both formats"

5. **Branching Tests** (`specs/sessions-branching/spec.md`):
   - `test-branch-format-inheritance-org` → "Copying org-mode content up to branch point"
   - `test-branch-format-inheritance-md` → "Copying markdown content up to branch point (legacy)"

6. **Backward Compatibility Tests** (`specs/org-mode-sessions/spec.md`):
   - `test-legacy-md-session-open` → "Open legacy markdown session"
   - `test-dual-format-coexistence` → "Manual migration is optional"

**Test Coverage Goals**:
- All new org-mode scenarios: 100%
- All modified scenarios (dual-format): 100%
- Backward compatibility: Explicit tests for each legacy path
- Integration points: gptel conversion, activities, org-roam

## Dependencies

### External Packages

**gptel** (upstream):
- Version: Latest (0.9.x as of 2026-03)
- Feature: `gptel-org-convert-response` (enabled by default)
- Purpose: Automatic markdown-to-org conversion
- Integration: Zero changes needed - feature already exists

**org-mode** (built-in Emacs):
- Version: 9.7+ (for full gptel-org support, but works with older versions)
- Purpose: Major mode for `.org` files
- Integration: Auto-detected by Emacs from file extension

**yaml.el** (straight.el package):
- Purpose: Parse YAML metadata files (unchanged)
- Integration: Existing, no changes needed

### Internal Dependencies

**Session Modules** (order matters):
1. `constants.el` - Must load first (defines file names)
2. `filesystem.el` - Uses constants
3. `metadata.el` - Uses filesystem utilities
4. `branching.el` - Uses metadata and filesystem
5. `commands.el` - Uses all above modules
6. `activities-integration.el` - Uses commands module

**Load Order Constraint**: No new constraints introduced. Existing load order in `config/gptel/gptel.org` remains valid.

## Constraints

### Technical Limitations

**gptel Conversion Limitations**:
- Conversion is heuristic-based (regex patterns)
- Edge cases may not convert perfectly (documented in gptel issues)
- Streaming conversion is incremental (may have brief display artifacts)
- **Mitigation**: Let LLMs output markdown naturally, don't try to fix converter issues (rely on upstream)

**Emacs Local Variables Format**:
- Must appear at end of file
- Emacs enforces specific syntax (`# Local Variables:`, `# End:`)
- Can't mix HTML comments and Emacs Local Variables in same file
- **Mitigation**: Use format appropriate for file extension (org → Emacs, md → HTML or Emacs)

**File Extension Detection**:
- Depends on Emacs file-name-handler-alist and auto-mode-alist
- User customizations could interfere
- **Mitigation**: Use explicit extension checks in auto-init hook

### Performance Requirements

**Auto-Init Performance**:
- Fast-path check must be efficient (executed on every find-file-hook)
- Use `string-suffix-p` for quick extension check before regex matching
- **Target**: < 1ms for non-session files, < 10ms for session files

**Conversion Performance**:
- gptel's converter is regex-based (fast)
- Streaming conversion is incremental (no buffering overhead)
- **Target**: Imperceptible delay (handled by gptel)

### Compatibility Requirements

**Backward Compatibility**:
- MUST support existing `.md` sessions indefinitely
- MUST read HTML comment Local Variables from `.md` files
- MUST handle both file formats in all commands (create, branch, auto-init)

**Forward Compatibility**:
- Design allows adding more file formats in future (unlikely but possible)
- Mode configuration in presets enables format flexibility
- Format-agnostic metadata (YAML) enables format changes

### User Experience Constraints

**No Disruption**:
- Existing workflows must continue unchanged
- No error messages for legacy sessions
- No performance degradation for any format

**Clear Migration Path**:
- Users can manually rename `.md` → `.org` if desired
- No forced migration ever
- Documentation explains benefits of org-mode format
