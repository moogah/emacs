## Context

The current gptel session implementation uses `session.md` files in markdown format. This creates several issues:
1. **Redundant conversion logic**: Custom `jf/markdown-to-org` converter needed for org-roam integration
2. **Missing org-mode features**: Can't use org-mode's folding, properties, agenda integration
3. **Misalignment with upstream**: gptel package has mature org-mode support via `gptel-org-convert-response` that we're not leveraging

The upstream gptel package includes a well-tested, built-in markdown-to-org converter (`gptel-org-convert-response`, enabled by default) that handles the impedance mismatch between LLM output (natural markdown) and org-mode buffers. This converter:
- Handles streaming responses incrementally
- Converts code blocks (` ``` ` → `#+begin_src`)
- Converts headings (`#` → `*`)
- Handles mixed markup gracefully
- Has been battle-tested and actively maintained

By transitioning to org-mode as the primary session format, we can:
- Remove custom conversion code (simplify)
- Leverage gptel's built-in converter (align with upstream)
- Enable org-mode features (fold, properties, agenda)
- Maintain backward compatibility (support markdown indefinitely)

**Stakeholders**:
- Users with existing markdown sessions (must not break)
- Users wanting org-mode features (benefit from change)
- Future maintenance (simplified codebase)

## Goals / Non-Goals

**Goals:**
1. **Primary format transition**: New sessions created as `session.org` by default
2. **Automatic conversion**: Leverage gptel's `gptel-org-convert-response` (no custom conversion)
3. **Backward compatibility**: Existing `.md` sessions continue working indefinitely
4. **Code simplification**: Remove redundant `jf/markdown-to-org` from org-roam tools
5. **Dual-format support**: Both `.org` and `.md` sessions detected and initialized correctly
6. **Format inheritance**: Branches inherit parent format automatically
7. **Testing coverage**: Comprehensive tests for org-mode paths and backward compatibility

**Non-Goals:**
1. **Forced migration**: No automatic `.md` → `.org` conversion
2. **Markdown deprecation**: Markdown format remains fully supported
3. **LLM instruction changes**: Don't instruct LLMs to output org-mode (let them use markdown)
4. **Custom converter improvements**: Use gptel's converter as-is (don't fork or enhance)
5. **Format-specific features**: Don't add features that only work in one format

## Decisions

### Decision 1: Use `.org` as default extension for new sessions

**Rationale**: org-mode offers superior features and aligns with gptel's design intent.

**Alternatives considered**:
- **Keep markdown as default**: Would maintain status quo but miss org-mode benefits
- **User-configurable default**: Adds complexity for marginal benefit
- **Preset-specific format**: Implemented as optional enhancement (see Decision 6)

**Implementation**:
```elisp
;; In config/gptel/sessions/constants.org
(defconst jf/gptel-session--context-file "session.org"
  "File name for main conversation context.
Uses org-mode format with automatic markdown-to-org conversion.")
```

**Trade-off**: Users preferring markdown can create sessions manually with `.md` extension or use preset configuration.

---

### Decision 2: Rely on gptel's built-in converter (don't build custom logic)

**Rationale**:
- gptel's converter is mature (actively maintained since 2023)
- Handles streaming responses incrementally
- Well-tested with comprehensive edge case handling
- Documented issues (GitHub #81, #296) are resolved
- Building custom converter would duplicate effort and add maintenance burden

**Alternatives considered**:
- **Build custom converter**: Would give us control but add complexity and maintenance
- **Use pandoc for session conversion**: Slower, requires external dependency, overkill for simple conversion
- **Fork gptel's converter**: Would break on upstream updates

**Implementation**: Zero code changes. Feature already exists and is enabled by default:
```elisp
;; In gptel-request.el (upstream)
(defcustom gptel-org-convert-response t
  "Whether gptel should convert Markdown responses to Org markup.
This only affects requests originating from Org mode buffers."
  :type 'boolean)
```

When buffer is org-mode, gptel automatically applies conversion before inserting responses.

**Trade-off**: We depend on upstream's converter quality. Mitigation: It's already well-tested and issues are rare.

---

### Decision 3: Use Emacs Local Variables format for org-mode sessions

**Rationale**:
- Works in both org-mode and markdown-mode (forward/backward compatible)
- Read automatically by Emacs (no manual parsing)
- Standard Emacs feature (well-documented)
- Simpler than HTML comments (no conditional logic)

**Alternatives considered**:
- **Continue using HTML comments**: Markdown-specific, doesn't work naturally in org-mode
- **Org-mode property drawers**: Would only work in org-mode, not universal
- **Custom metadata format**: Would require manual parsing

**Implementation**:
```elisp
;; Emacs Local Variables format (works in both .org and .md)
# Local Variables:
# gptel-model: claude-sonnet-4-6
# gptel--backend-name: Claude
# gptel--bounds: ((response . ((100 500) (800 1200))))
# End:
```

**Migration strategy**:
- Branching module reads BOTH formats (HTML comments and Emacs Local Variables)
- Writes format appropriate for target file extension
- Gradual transition as users create new branches

**Trade-off**: Mixed format during transition period. Mitigation: Both formats are readable.

---

### Decision 4: Detect both formats in auto-init hook (dual-format support)

**Rationale**:
- Maintains backward compatibility
- Enables format coexistence
- Supports gradual migration
- No user-visible breaking changes

**Implementation**:
```elisp
;; In config/gptel/sessions/commands.org
(defun jf/gptel--auto-init-session-buffer ()
  "Auto-initialize session buffer for both .org and .md files."
  (when-let* ((file-path (buffer-file-name))
              ;; Fast path: check extension first
              ((or (string-suffix-p ".org" file-path)
                   (string-suffix-p ".md" file-path)))
              ;; Pattern match for session files
              ((or (string-match "/branches/\\([^/]+\\)/session\\.org$" file-path)
                   (string-match "/branches/\\([^/]+\\)/session\\.md$" file-path)
                   (string-match "/agents/\\([^/]+\\)/session\\.org$" file-path)
                   (string-match "/agents/\\([^/]+\\)/session\\.md$" file-path))))
    ;; Initialize session (format-agnostic logic)
    ...))
```

**Alternatives considered**:
- **Org-mode only**: Would break existing sessions
- **Separate functions for each format**: Code duplication
- **Format auto-detection from content**: Slower, unnecessary

**Trade-off**: Slightly more complex pattern matching. Mitigation: Fast-path check prevents overhead.

---

### Decision 5: Inherit format in branching operations

**Rationale**:
- Predictable behavior (branches match parent)
- Avoids format confusion
- Enables mixed-format session trees if needed

**Implementation**:
```elisp
;; In config/gptel/sessions/branching.org
(defun jf/gptel--get-session-file-extension (buffer)
  "Get session file extension (.org or .md) from BUFFER."
  (when-let ((file-path (buffer-file-name buffer)))
    (file-name-extension file-path)))

(defun jf/gptel-branch-session (&optional branch-name)
  "Create branch using same format as parent."
  (let* ((source-ext (jf/gptel--get-session-file-extension (current-buffer)))
         (session-file-name (concat "session." source-ext))
         ...)
    ;; Create branch with inherited format
    ...))
```

**Alternatives considered**:
- **Always use .org for branches**: Confusing if parent is .md
- **Ask user for format**: Adds friction to workflow
- **Use preset default**: Doesn't account for manually created markdown sessions

**Trade-off**: Prevents automatic format upgrade during branching. Mitigation: Users can manually migrate if desired.

---

### Decision 6: Optional mode configuration in presets (future-proofing)

**Rationale**:
- Enables format-specific presets if needed
- Follows same pattern as scope configuration
- Optional (doesn't affect existing presets)
- Extensible to other formats

**Implementation**:
```elisp
;; In preset frontmatter (optional)
---
description: Research assistant
mode: org-mode  # or markdown-mode
---

;; Extracted during preset registration
(defvar jf/gptel-preset--mode-defaults nil
  "Alist mapping preset names to mode plists.")

;; Example: ((research . (:mode "org-mode"))
;;           (legacy . (:mode "markdown-mode")))
```

**Alternatives considered**:
- **No preset configuration**: Would prevent format-specific presets
- **Required mode field**: Would break existing presets
- **Global configuration variable**: Less flexible than per-preset

**Trade-off**: Adds complexity for edge cases. Mitigation: Optional feature, defaults to org-mode.

---

### Decision 7: Remove org-roam conversion in create_roam_node

**Rationale**:
- Content from org-mode sessions is already in org-mode format (converted by gptel)
- Removes redundant conversion overhead
- Simplifies code
- Maintains identical behavior (org → org, no conversion needed)

**Implementation**:
```elisp
;; OLD: In config/gptel/tools/org-roam-tools.org
(gptel-make-tool
 "create_roam_node"
 ...
 (let ((content-org (jf/markdown-to-org content)))  ; ← REMOVE THIS
   (insert content-org)))

;; NEW: Direct insertion (content already org-mode)
(gptel-make-tool
 "create_roam_node"
 ...
 (insert content))  ; Content already in org-mode format
```

**Alternatives considered**:
- **Keep conversion as no-op**: Unnecessary overhead
- **Conditional conversion**: Adds complexity

**Trade-off**: Markdown sessions would send markdown to org-roam (but sessions will be org-mode). Mitigation: Not an issue since new sessions are org-mode.

---

### Decision 8: Update activities integration to use org-mode format

**Rationale**:
- Activities are long-lived, structured sessions (org-mode benefits apply)
- Consistent with default format
- Enables org-mode features in activities workflow

**Implementation**:
```elisp
;; In config/gptel/sessions/activities-integration.org
(defun jf/gptel--activities-create-session (activity)
  "Create session for ACTIVITY using org-mode format."
  (let ((session-file (expand-file-name "session.org" session-dir))  ; .org extension
        (initial-content "* Session\n\n"))  ; org-mode heading
    (with-temp-file session-file
      (insert initial-content))
    ...))
```

**Alternatives considered**:
- **Keep markdown for activities**: Inconsistent with new default
- **Make it configurable**: Unnecessary complexity

**Trade-off**: Existing activities with markdown sessions continue using markdown. Mitigation: Only affects new activities.

---

### Decision 9: Extend existing test files (not create new ones)

**Rationale**:
- Co-locates tests with modules they test
- Enables comparison between markdown and org-mode tests
- Uses existing test infrastructure
- Follows user preference (from architecture discussion)

**Implementation**:
```elisp
;; In config/gptel/sessions/commands-test.el

;; Existing markdown test
(ert-deftest test-session-creation ()
  "Test session creation with markdown format."
  ...)

;; New org-mode variant
(ert-deftest test-session-creation-org ()
  "Test session creation with org-mode format.
Scenario: specs/org-mode-sessions/spec.md § 'New org-mode session creation'"
  (let ((session-dir (test-helper-create-org-session "test-session")))
    (unwind-protect
        (progn
          (should (file-exists-p (expand-file-name "branches/main/session.org" session-dir)))
          (should (string= (test-helper-read-file-content session-file)
                           "* Session\n\n")))
      (delete-directory session-dir t))))
```

**Test naming convention**: `-org` suffix
- `test-session-creation` → `test-session-creation-org`
- `test-auto-init-detection` → `test-auto-init-detection-org`

**Alternatives considered**:
- **New test files**: Would duplicate infrastructure
- **Prefix with org-mode-**: Less clear relationship to original test

**Trade-off**: Test files become longer. Mitigation: Clear organization by function.

## Risks / Trade-offs

### Risk 1: gptel converter edge cases

**Risk**: gptel's markdown-to-org converter may have edge cases that cause conversion artifacts.

**Evidence**: GitHub issues #81 and #296 documented edge cases (now resolved).

**Mitigation**:
- Rely on upstream's mature converter (actively maintained)
- Don't try to fix converter issues ourselves (report upstream)
- Let LLMs output natural markdown (don't instruct them to output org syntax)
- Edge cases are rare in practice (converter is well-tested)

**Impact**: Low. Converter has been battle-tested and issues are rare.

---

### Risk 2: Mixed format during transition

**Risk**: Sessions may have mixed formats (parent .md, child .org, or vice versa if user manually changes).

**Mitigation**:
- Format inheritance in branching (children match parent)
- Both formats supported indefinitely (no pressure to migrate)
- Auto-detection handles both formats transparently
- Documentation explains format inheritance behavior

**Impact**: Low. Mixed formats work correctly, just not aesthetically consistent.

---

### Risk 3: Emacs Local Variables parsing differences

**Risk**: Emacs Local Variables format has strict syntax requirements. Malformed blocks won't be read.

**Mitigation**:
- Use Emacs built-in mechanism (reliable)
- Validate format when writing (ensure correct syntax)
- Fall back to reading from metadata.yml if Local Variables fail
- Use `condition-case` around Local Variables writes

**Impact**: Low. Emacs Local Variables are well-tested, syntax is simple.

---

### Risk 4: Performance impact of dual-format detection

**Risk**: Checking both `.org` and `.md` extensions could slow down find-file-hook.

**Mitigation**:
- Fast-path check: `(or (string-suffix-p ".org" ...) (string-suffix-p ".md" ...))`
- Exit early for non-session files
- Regex matching only after extension check passes
- Target: < 1ms for non-session files

**Impact**: Very low. Fast-path prevents overhead.

**Measurement**: Use `benchmark-run` to verify performance.

---

### Risk 5: Breaking existing markdown sessions

**Risk**: Changes to session initialization could break legacy `.md` sessions.

**Mitigation**:
- Comprehensive backward compatibility tests
- Explicit tests for markdown format
- Pattern matching supports both `.org` and `.md`
- Local Variables reading supports both HTML comments and Emacs format
- No removal of markdown-specific code paths

**Impact**: Very low with proper testing.

---

### Risk 6: User confusion about format choice

**Risk**: Users may not understand when to use .org vs .md.

**Mitigation**:
- Clear default (new sessions are .org)
- Documentation explains benefits of org-mode
- Preset configuration enables format choice if needed
- Both formats work identically (no feature disparity)

**Impact**: Low. Most users won't care about format.

---

### Trade-off 1: Format flexibility vs consistency

**Trade-off**: Supporting both formats enables flexibility but reduces consistency.

**Decision**: Prioritize backward compatibility over consistency. Both formats will coexist indefinitely.

**Rationale**: Breaking existing sessions is unacceptable. Format inconsistency is acceptable since both work identically.

---

### Trade-off 2: Simplicity vs feature-richness

**Trade-off**: org-mode offers richer features but adds complexity (property drawers, heading structure, etc.).

**Decision**: Use org-mode's simple features initially (headings, code blocks). Advanced features (properties, agenda) are opt-in.

**Rationale**: Users can leverage advanced features if desired, but basic use remains simple.

---

### Trade-off 3: Custom converter vs upstream dependency

**Trade-off**: Building custom converter gives control but adds maintenance burden.

**Decision**: Rely on upstream's converter (dependency over control).

**Rationale**: Upstream converter is mature, actively maintained, and well-tested. Building custom would be wasteful duplication.

## Migration Plan

### Phase 1: Core implementation (Breaking changes to constants and auto-init)

**Changes**:
1. Update `jf/gptel-session--context-file` constant to `"session.org"`
2. Modify auto-init hook to detect both `.org` and `.md`
3. Update initial content to `"* Session\n\n"` for org-mode
4. Rewrite Local Variables handling in branching module

**Testing**:
- Run existing test suite (ensure no regressions)
- Add org-mode test variants (`-org` suffix)
- Add backward compatibility tests

**Risk**: Breaking changes to constants could affect hardcoded references.
**Mitigation**: Search codebase for `session.md` references, update all.

**Rollback**: Revert constant change, restore markdown as default.

---

### Phase 2: Org-roam simplification

**Changes**:
1. Remove `jf/markdown-to-org` call from `create_roam_node` tool
2. Update org-roam tools documentation

**Testing**:
- Test org-roam node creation from org-mode session
- Verify node content is valid org-mode

**Risk**: Low. Content is already org-mode format.
**Mitigation**: Explicit tests for org-roam integration.

**Rollback**: Restore conversion call (safe, just adds redundant processing).

---

### Phase 3: Activities and persistent agent updates

**Changes**:
1. Update activities integration to use `session.org`
2. Remove `(markdown-mode)` call from persistent agent
3. Update preset registration to support optional `:mode` parameter

**Testing**:
- Test activity creation with new sessions
- Test agent session creation
- Test preset mode configuration

**Risk**: Low. Changes are localized.
**Mitigation**: Test each integration point separately.

**Rollback**: Revert to markdown format for activities/agents.

---

### Deployment strategy

**Approach**: Gradual rollout, no forced migration.

**Steps**:
1. Deploy core changes (Phase 1)
2. Monitor for issues with new org-mode sessions
3. Deploy simplifications (Phase 2)
4. Deploy integration updates (Phase 3)

**User communication**:
- Document org-mode benefits in CHANGELOG
- Explain format inheritance in documentation
- Note that existing sessions continue working

**Rollback strategy**:
- Revert constant to `"session.md"`
- Existing org-mode sessions will need manual handling
- Both formats work, so rollback is safe

**Success criteria**:
- All existing tests pass
- New org-mode tests pass
- No user-reported issues with markdown sessions
- Org-roam integration works without conversion

## Open Questions

### Q1: Should we provide a migration tool for markdown → org sessions?

**Options**:
1. **No tool** (let users manually rename if desired)
2. **Simple rename script** (just changes extension and initial content)
3. **Full conversion tool** (converts content to org-mode format)

**Recommendation**: Start with no tool. If users request it, provide simple rename script. Full conversion is complex and low-value (gptel converter handles LLM responses anyway).

**Resolution strategy**: Monitor user feedback, implement if requested.

---

### Q2: Should we add org-mode specific features (e.g., agenda integration)?

**Options**:
1. **No** (keep parity between formats)
2. **Yes, as opt-in** (org-mode sessions can use agenda)
3. **Yes, as default** (leverage org-mode fully)

**Recommendation**: Opt-in approach. Document how users can leverage org-mode features without building custom integrations.

**Resolution strategy**: Let user demand drive feature additions.

---

### Q3: Should we warn users if they create markdown sessions?

**Options**:
1. **No warning** (both formats are first-class)
2. **Soft warning** (suggest org-mode but allow markdown)
3. **Hard warning** (require confirmation for markdown)

**Recommendation**: No warning. Both formats are fully supported and work identically. User choice should be respected.

**Resolution strategy**: Document org-mode benefits, let users choose without friction.

---

### Q4: How to handle worktree paths in activities integration?

**Current**: Stored as HTML comments in session.md
**Issue**: HTML comments don't feel natural in org-mode

**Options**:
1. **Keep HTML comments** (works in org-mode, treated as pass-through)
2. **Use org-mode comments** (`# ` prefix)
3. **Use property drawer** (org-mode native)
4. **Store in metadata.yml** (format-independent)

**Recommendation**: Keep HTML comments for now (works in both formats). Consider metadata.yml in future if it becomes an issue.

**Resolution strategy**: Test that HTML comments work in org-mode sessions, document behavior.

---

## Implementation Notes

### Key file changes

**constants.org** (1 change):
```elisp
(defconst jf/gptel-session--context-file "session.org"  ; was "session.md"
```

**commands.org** (3 changes):
1. Fast-path: `(or (string-suffix-p ".org" ...) (string-suffix-p ".md" ...))`
2. Pattern matching: `session\\.org$` and `session\\.md$`
3. Initial content: `"* Session\n\n"` for org-mode

**branching.org** (major rewrite):
- Local Variables extraction: Support both HTML and Emacs formats
- Local Variables writing: Choose format based on target extension
- Context truncation: Format-agnostic (unchanged)

**activities-integration.org** (2 changes):
1. File extension: `session.org`
2. Initial content: `"* Session\n\n"`

**org-roam-tools.org** (1 removal):
- Remove `jf/markdown-to-org` call (line ~629)

**persistent-agent.org** (1 removal):
- Remove `(markdown-mode)` call (line ~637)

**preset-registration.org** (optional, 3 additions):
1. Extract `:mode` during parsing
2. Store in `jf/gptel-preset--mode-defaults`
3. Use during session creation

### Test changes

**Add to commands-test.el**:
- `test-session-creation-org`
- `test-auto-init-detection-org`
- `test-dual-format-detection`

**Add to branching-test.el**:
- `test-branch-format-inheritance-org`
- `test-local-variables-org-format`
- `test-local-variables-compatibility`

**Add to filesystem-test.el**:
- `test-directory-creation-org`

**Add to activities-integration-test.el** (if exists):
- `test-activities-session-org-format`

### Code patterns

**Detecting file format**:
```elisp
(defun jf/gptel--session-file-format (buffer-or-file)
  "Get session file format (:org or :md)."
  (let ((ext (file-name-extension
              (if (bufferp buffer-or-file)
                  (buffer-file-name buffer-or-file)
                buffer-or-file))))
    (cond ((string= ext "org") :org)
          ((string= ext "md") :md)
          (t nil))))
```

**Writing Local Variables**:
```elisp
(defun jf/gptel--write-local-variables (format vars)
  "Write Local Variables in FORMAT (:org or :md) with VARS."
  (pcase format
    (:org
     (insert "# Local Variables:\n")
     (dolist (var vars)
       (insert (format "# %s: %S\n" (car var) (cdr var))))
     (insert "# End:\n"))
    (:md
     (insert "<!-- Local Variables: -->\n")
     (dolist (var vars)
       (insert (format "<!-- %s: %S -->\n" (car var) (cdr var))))
     (insert "<!-- End: -->\n"))))
```

**Reading Local Variables**:
```elisp
;; Let Emacs handle it automatically (both formats)
;; No manual parsing needed
```
