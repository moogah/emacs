## MODIFIED Requirements

### Requirement: Scope key extraction

The system SHALL extract bash_tools along with other scope-related keys from parsed preset plists before registration with `gptel-make-preset`.

After key normalization, scope keys include: `:paths`, `:org-roam-patterns`, `:shell-commands`, `:bash-tools`, `:scope-profile`.

**Note:** This extends the scope key extraction documented in `openspec/changes/gptel-preset-upstream-alignment/specs/preset-registration/spec.md`.

#### Scenario: Bash tools extracted from preset plist
- **WHEN** a parsed and normalized plist contains `:bash-tools`
- **THEN** this key is removed from the plist before calling `gptel-make-preset`
- **AND** the bash_tools config is stored in `jf/gptel-preset--scope-defaults`
- **AND** the registered preset contains only gptel-recognized keys

#### Scenario: Bash tools structure preserved
- **WHEN** extracting bash_tools with nested categories structure
- **THEN** the system preserves the nested plist structure:
  ```elisp
  (:bash-tools
    (:categories
      (:read-only (:commands ["ls" "cat" "grep"])
       :safe-write (:commands ["mkdir" "touch"])
       :dangerous (:commands []))
     :deny ["rm" "sudo"]))
  ```

#### Scenario: Bash tools key normalization
- **WHEN** YAML contains `bash_tools` with snake_case keys
- **THEN** after normalization the key is `:bash-tools` (kebab-case)
- **AND** nested keys like `read_only` become `:read-only`
- **AND** nested keys like `safe_write` become `:safe-write`

#### Scenario: Preset with bash_tools and scope_profile
- **WHEN** a preset defines both inline bash_tools and a scope_profile reference
- **THEN** both are extracted to scope defaults
- **AND** the scope_profile takes precedence during session creation (inline bash_tools ignored if profile has bash_tools)

#### Scenario: Preset with only bash_tools (no scope_profile)
- **WHEN** a preset defines bash_tools but no scope_profile reference
- **THEN** bash_tools is extracted and stored in scope defaults
- **AND** during session creation, the inline bash_tools is used directly

#### Scenario: Preset with neither bash_tools nor scope_profile
- **WHEN** a preset defines neither bash_tools nor scope_profile
- **THEN** no bash_tools entry is added to scope defaults
- **AND** sessions created from that preset have no bash command access (deny by default)

### Requirement: Bash tools scope defaults structure

The system SHALL store extracted bash_tools in `jf/gptel-preset--scope-defaults` using the same structure as scope profiles.

#### Scenario: Scope defaults alist entry
- **WHEN** bash_tools is extracted from preset "executor"
- **THEN** `jf/gptel-preset--scope-defaults` contains:
  ```elisp
  (executor . (:bash-tools
                (:categories
                  (:read-only (:commands ["ls" "grep"])
                   :safe-write (:commands ["mkdir"]))
                 :deny ["rm" "sudo"])))
  ```

#### Scenario: Multiple presets with bash_tools
- **WHEN** multiple presets define bash_tools
- **THEN** each has a separate entry in `jf/gptel-preset--scope-defaults`
- **AND** presets do not interfere with each other

**Related specs:** See `bash-tools/spec.md` for bash tools behavior and `scope-profiles/spec.md` for how bash_tools flows from profiles to scope.yml.
