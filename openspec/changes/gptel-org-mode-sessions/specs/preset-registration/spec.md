# Preset Registration (Delta Spec)

This delta spec documents changes to preset-registration for optional mode configuration support.

## ADDED Requirements

### Requirement: Optional mode configuration per preset

The system SHALL support optional `:mode` parameter in preset frontmatter to specify session file format (org-mode or markdown-mode).

Mode configuration SHALL:
1. Be optional (defaults to org-mode if not specified)
2. Accept values: `"org-mode"` or `"markdown-mode"`
3. Be extracted during preset parsing (alongside scope keys)
4. Be stored in side table `jf/gptel-preset--mode-defaults` (similar to scope defaults)
5. Be used during session creation to determine file format

The mode parameter enables preset authors to specify which file format sessions should use, allowing format-specific presets while defaulting to org-mode for new presets.

#### Scenario: Preset with org-mode specification
- **WHEN** parsing preset file with frontmatter:
  ```yaml
  ---
  description: Research assistant
  backend: Claude
  model: claude-opus-4-6
  mode: org-mode
  ---
  ```
- **THEN** extracts `:mode "org-mode"` from frontmatter
- **AND** stores in `jf/gptel-preset--mode-defaults` as `(research . (:mode "org-mode"))`
- **AND** removes `:mode` key from preset plist before calling `gptel-make-preset`

#### Scenario: Preset with markdown-mode specification
- **WHEN** parsing preset file with frontmatter:
  ```yaml
  ---
  description: Legacy executor
  mode: markdown-mode
  ---
  ```
- **THEN** extracts `:mode "markdown-mode"`
- **AND** stores in mode defaults table
- **AND** session creation uses markdown format for this preset

#### Scenario: Preset without mode specification
- **WHEN** parsing preset file with no `:mode` key
- **THEN** no mode entry is created in defaults table
- **AND** session creation defaults to org-mode format
- **AND** backward compatible with existing preset files

#### Scenario: Mode defaults lookup during session creation
- **WHEN** creating session with preset "research"
- **AND** preset has mode default `:mode "org-mode"`
- **THEN** session creation logic checks `jf/gptel-preset--mode-defaults`
- **AND** creates `session.org` file
- **AND** uses org-mode initial content format

### Requirement: Mode defaults storage structure

The system SHALL maintain `jf/gptel-preset--mode-defaults` alist mapping preset names to mode plists, following the same pattern as `jf/gptel-preset--scope-defaults`.

Storage structure:
```elisp
(defvar jf/gptel-preset--mode-defaults nil
  "Alist mapping preset name symbols to mode plists.
Each entry is (PRESET-NAME . MODE-PLIST) where MODE-PLIST contains
:mode key with value \"org-mode\" or \"markdown-mode\".")
```

Example data:
```elisp
((research . (:mode "org-mode"))
 (legacy-executor . (:mode "markdown-mode")))
```

#### Scenario: Mode defaults extraction during registration
- **WHEN** registering all presets via `jf/gptel-preset-register-all`
- **AND** some presets have `:mode` keys
- **THEN** builds `jf/gptel-preset--mode-defaults` alist
- **AND** each preset with `:mode` has an entry
- **AND** presets without `:mode` have no entry

#### Scenario: Mode defaults lookup
- **WHEN** creating session with preset name `'research`
- **THEN** looks up `(alist-get 'research jf/gptel-preset--mode-defaults)`
- **AND** returns `(:mode "org-mode")` if configured
- **AND** returns `nil` if not configured (defaults to org-mode)

### Requirement: Pipeline integration

The mode extraction SHALL integrate into the existing 5-stage preset registration pipeline.

Pipeline SHALL:
1. **Parse** — Extract YAML frontmatter (unchanged)
2. **Normalize** — Convert `:mode` to kebab-case (already kebab-case, no change needed)
3. **Coerce** — No coercion needed for mode string values
4. **Extract scope and mode** — Extract both `:mode` and scope keys, store in separate tables
5. **Register** — Call `gptel-make-preset` with cleaned plist (mode and scope keys removed)

Mode extraction follows identical pattern to scope extraction, using the same pipeline stage.

#### Scenario: Extract mode alongside scope keys
- **WHEN** processing preset with both scope and mode configuration:
  ```yaml
  mode: org-mode
  scope_profile: coding
  ```
- **THEN** extracts both keys in stage 4
- **AND** stores `:mode "org-mode"` in `jf/gptel-preset--mode-defaults`
- **AND** stores `:scope-profile "coding"` in `jf/gptel-preset--scope-defaults`
- **AND** removes both from preset plist before registration

#### Scenario: Extract mode without scope
- **WHEN** processing preset with only mode configuration
- **THEN** extracts `:mode` to mode defaults table
- **AND** scope defaults entry may be empty or missing
- **AND** both extractions are independent operations

## MODIFIED Requirements

None. The preset registration pipeline structure remains unchanged; mode configuration is an additive feature following the same extraction pattern as scope configuration.
