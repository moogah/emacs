## 1. Verify Core Scope Spec Against Implementation

- [x] 1.1 Verify tool categorization mapping against jf/gptel-scope--tool-categories in scope-core.el
- [x] 1.2 Verify configuration loading behavior against jf/gptel-scope--load-config in scope-core.el
- [x] 1.3 Verify path-based validation logic against jf/gptel-scope--validate-path-tool in scope-core.el
- [x] 1.4 Verify pattern-based validation logic against jf/gptel-scope--validate-pattern-tool in scope-core.el
- [x] 1.5 Verify command-based validation logic against jf/gptel-scope--validate-command-tool in scope-core.el
- [x] 1.6 Verify allow-once mechanism against allow-once functions in scope-core.el
- [x] 1.7 Verify permission priority order against jf/gptel-scope--check-tool-permission in scope-core.el
- [x] 1.8 Verify macro behavior against gptel-make-scoped-tool implementation in scope-core.el
- [x] 1.9 Verify glob pattern matching against jf/gptel-scope--matches-pattern in scope-core.el
- [x] 1.10 Verify structured error format against jf/gptel-scope--format-tool-error in scope-core.el

## 2. Verify Preset Spec Against Configuration Files

- [x] 2.1 Verify YAML frontmatter structure against config/gptel/presets/claude-plan.md
- [x] 2.2 Verify paths section format (read/write/deny) against multiple preset examples
- [x] 2.3 Verify org_roam_patterns section format against preset examples
- [x] 2.4 Verify shell_commands section format against preset examples
- [x] 2.5 Verify tools array format against preset examples
- [x] 2.6 Verify preset metadata fields (description, backend, model, temperature) against presets
- [x] 2.7 Verify deny-by-default patterns (**/.git/**, **/runtime/**, **/.env, **/node_modules/**) are standard
- [x] 2.8 Verify glob syntax examples (/** vs * vs ?) match actual usage in presets
- [x] 2.9 Verify markdown body usage as system prompt in preset files
- [x] 2.10 Note any preset format discrepancies or evolution patterns

## 3. Verify Expansion Spec Against UI Implementation

- [x] 3.1 Verify request_scope_expansion tool definition and meta categorization
- [x] 3.2 Verify transient menu structure against jf/gptel-scope-expansion-menu in scope-expansion.el
- [x] 3.3 Verify three-choice workflow (Deny/Add/Allow-once) against suffix commands in scope-expansion.el
- [x] 3.4 Verify deny action behavior against jf/gptel-scope--deny-expansion in scope-expansion.el
- [x] 3.5 Verify add-to-scope behavior against jf/gptel-scope--add-to-scope in scope-expansion.el
- [x] 3.6 Verify allow-once action against jf/gptel-scope--allow-once-action in scope-expansion.el
- [x] 3.7 Verify preset update routing against validation-type dispatch in scope-expansion.el
- [x] 3.8 Verify path updater behavior against jf/gptel-scope--add-path-to-preset in scope-expansion.el
- [x] 3.9 Verify pattern updater behavior against jf/gptel-scope--add-pattern-to-preset in scope-expansion.el
- [x] 3.10 Verify command updater behavior against jf/gptel-scope--add-command-to-preset in scope-expansion.el
- [x] 3.11 Verify callback integration and JSON response format in expansion functions
- [x] 3.12 Verify transient scope data passing mechanism in scope-expansion.el

## 4. Cross-Check Tool Integration

- [x] 4.1 Verify scope-filesystem-tools.el uses gptel-make-scoped-tool macro correctly
- [x] 4.2 Verify scope-org-roam-tools.el uses gptel-make-scoped-tool macro correctly
- [x] 4.3 Verify scope-shell-tools.el uses gptel-make-scoped-tool macro correctly
- [x] 4.4 Verify tool categorization includes all tools from these modules
- [x] 4.5 Verify first-argument convention for resource extraction across tool types

## 5. Document Legacy Code Status

- [x] 5.1 Confirm scope-manager.el (v1.0 advice-based) is not actively used
- [x] 5.2 Note any template functions in scope-commands.el that are legacy
- [x] 5.3 Verify no active code references v1.0 validation approach
- [x] 5.4 Add notes to specs distinguishing v2.0 (active) from v1.0 (legacy) if needed

## 6. Validate Spec Completeness

- [x] 6.1 Check all scenarios in gptel/scope.md have corresponding implementation behavior
- [x] 6.2 Check all scenarios in gptel/scope-presets.md match actual preset formats
- [x] 6.3 Check all scenarios in gptel/scope-expansion.md match UI implementation
- [x] 6.4 Identify any behaviors in implementation not covered by specs (document or defer)
- [x] 6.5 Ensure specs focus on behavior/contracts, not implementation details

## 7. Final Review and Archive Preparation

- [x] 7.1 Review all three specs for consistency in terminology and style
- [x] 7.2 Verify specs use SHALL/MUST for normative requirements consistently
- [x] 7.3 Verify all scenarios follow WHEN/THEN format with 4-hashtag headers
- [x] 7.4 Confirm .el files are source of truth, note any .org file discrepancies
- [x] 7.5 Run verification protocol: compare each spec requirement against actual code behavior
- [x] 7.6 Document any discovered mismatches between specs and implementation for future fixes
