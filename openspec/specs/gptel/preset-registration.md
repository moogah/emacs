# Preset Registration

## Purpose

Provides the pipeline for registering gptel presets from `.md` files in `config/gptel/presets/`. The system parses YAML frontmatter, normalizes keys from snake_case to kebab-case, applies type coercion fixes, extracts scope configuration, and registers presets with upstream's `gptel-make-preset`.

This capability bridges the gap between user-friendly preset files and upstream's preset API, handling format conversion and scope separation. Scope configuration keys are extracted and stored in a side table (`jf/gptel-preset--scope-defaults`) for later use by the scope subsystem during session initialization.

## Key Concepts

### Preset Files

Markdown files in `config/gptel/presets/` with YAML frontmatter and markdown body:
- **Frontmatter**: Between `---` delimiters at file start. Contains YAML with preset configuration (model, backend, tools, temperature, etc.) plus scope keys (paths, org-roam-patterns, shell-commands, bash-tools, scope-profile).
- **Markdown body**: Text after closing `---` delimiter. Becomes the `:system` prompt message.
- **File name**: Basename (sans `.md`) becomes the preset name symbol (e.g., `executor.md` → symbol `executor`).

Example: `config/gptel/presets/executor.md`
```yaml
---
description: Autonomous executor for well-defined tasks
backend: Claude
model: claude-sonnet-4-6
temperature: 0.3
confirm-tool-calls: auto
scope_profile: coding
tools:
  - Read
  - Write
  - Bash
---
You are an autonomous executor agent...
```

### Pipeline Stages

Pure function pipeline transforming parsed YAML into a registered preset:

1. **Parse** — Read file, extract YAML frontmatter (between `---`) and markdown body. Returns plist with YAML keys as keywords, markdown body as `:system` value.
2. **Normalize** — Convert all snake_case keyword names to kebab-case. Example: `:confirm_tool_calls` → `:confirm-tool-calls`, `:org_roam_patterns` → `:org-roam-patterns`.
3. **Coerce** — Fix YAML-to-elisp type mismatches. String models become symbols, `"nil"`/`"auto"`/`"always"` strings become proper elisp values, YAML `:false` becomes `nil`.
4. **Extract scope** — Strip scope keys (`:paths`, `:org-roam-patterns`, `:shell-commands`, `:bash-tools`, `:scope-profile`) from plist and store in `jf/gptel-preset--scope-defaults` alist keyed by preset name.
5. **Register** — Call `gptel-make-preset` with cleaned plist (scope keys removed, coercion applied).

### Scope Defaults Storage

Variable `jf/gptel-preset--scope-defaults` is an alist mapping preset name symbols to scope configuration plists:

```elisp
((executor . (:scope-profile "coding"))
 (research . (:paths (:read ("/**") :write ("/tmp/**"))
              :org-roam-patterns ("projects" "research"))))
```

These defaults are used during session creation to initialize the session's scope configuration when the preset is applied.

## Requirements

### Requirement: Preset file parsing

The system SHALL parse preset `.md` files from `config/gptel/presets/` by extracting YAML frontmatter (between `---` delimiters) and the markdown body (as `:system` message).

**Implementation:** `jf/gptel-preset--parse-file` in `/Users/jefffarr/emacs/config/gptel/preset-registration.org` lines 61–98.

The parser SHALL NOT depend on the `gptel-agent` package.

The parser SHALL:
- Check file is readable, return nil if not (log warning)
- Expect opening `---` at start of file (lines 75–77)
- Find closing `---` on its own line (lines 80–83)
- Parse YAML content between delimiters using `yaml-parse-string` with `:object-type 'plist`, `:object-key-type 'keyword`, `:sequence-type 'list` (lines 88–91)
- Extract markdown body after closing `---` (lines 84–87)
- Set `:system` key to the markdown body if body is non-empty (lines 92–94)
- Use condition-case to catch and log parse errors (line 66), returning nil on error

#### Scenario: Parse valid preset file

**WHEN** parsing `config/gptel/presets/executor.md` containing YAML frontmatter and a body

**THEN** the system returns a plist like:
```elisp
(:description "Autonomous executor..."
 :backend "Claude"
 :model "claude-sonnet-4-6"
 :temperature 0.3
 :confirm_tool_calls "auto"
 :scope_profile "coding"
 :tools ("PersistentAgent" "Read" "Write" ...)
 :system "You are an autonomous executor agent...")
```

**Location:** Parser extracts body at lines 84–87, sets `:system` at lines 92–94 via `plist-put`.

#### Scenario: Parse file with missing opening delimiter

**WHEN** parsing a `.md` file that lacks opening `---` delimiter

**THEN** the system returns nil

**AND** logs warning: `"Preset file missing opening frontmatter delimiter: <filepath>"`

**Location:** Lines 75–77 check for opening `---` at point-min.

#### Scenario: Parse file with missing closing delimiter

**WHEN** parsing a `.md` file where YAML is not terminated with `---`

**THEN** the system returns nil

**AND** logs warning: `"Preset file missing closing frontmatter delimiter: <filepath>"`

**Location:** Lines 80–83 search for closing `---`, return nil if not found.

#### Scenario: Parse file that does not exist

**WHEN** parsing a path that does not exist

**THEN** the system returns nil

**AND** logs warning: `"Preset file not readable: <filepath>"`

**AND** does not signal an error

**Location:** Lines 67–70 check `file-readable-p`, return early if false.

#### Scenario: YAML parsing error handled gracefully

**WHEN** the YAML frontmatter contains invalid syntax (e.g., unquoted special characters)

**THEN** the system returns nil

**AND** logs error: `"Error parsing preset file <filepath>: <error-message>"`

**Location:** Lines 96–98 catch-error handler logs and returns nil.

### Requirement: YAML key normalization

After `yaml-parse-string`, the parser SHALL normalize YAML keys from snake_case to kebab-case keywords. This is required because YAML convention uses underscores while elisp convention uses hyphens.

**Implementation:** `jf/gptel-preset--normalize-keys` in `/Users/jefffarr/emacs/config/gptel/preset-registration.org` lines 110–128.

The normalization SHALL:
1. Convert each keyword's symbol-name from snake_case to kebab-case using `replace-regexp-in-string` (line 119)
2. Apply to ALL keys in the parsed plist (while loop at lines 115–127)
3. Leave keys already using hyphens unchanged (transform is idempotent)
4. Apply BEFORE coercion fixes (coercion uses kebab-case key names)
5. Recursively process nested plists (lines 122–126 detect and normalize nested plists)

**Note:** This normalization applies to all YAML parsing throughout the system — preset files, scope.yml, metadata.yml, and scope profiles all use the same convention.

#### Scenario: Snake_case YAML keys converted to kebab-case

**WHEN** a parsed plist contains `:include_tool_results t` and `:confirm_tool_calls "auto"`

**THEN** after normalization the keys are `:include-tool-results` and `:confirm-tool-calls`

**Example transformation:**
```elisp
;; Input (from yaml-parse-string)
(:include_tool_results t :confirm_tool_calls "auto" :model "claude-opus")

;; Output (after normalize)
(:include-tool-results t :confirm-tool-calls "auto" :model "claude-opus")
```

**Location:** Line 119 performs the replacement: `(replace-regexp-in-string "_" "-" key-name)`.

#### Scenario: Scope keys normalized

**WHEN** a parsed plist contains `:org_roam_patterns` and `:shell_commands`

**THEN** after normalization the keys are `:org-roam-patterns` and `:shell-commands`

**Location:** Same replacement handles all underscore-containing keys uniformly.

#### Scenario: Simple keys unchanged

**WHEN** a parsed plist contains `:model`, `:backend`, `:tools` (no underscores)

**THEN** these keys pass through normalization unchanged

**Location:** `replace-regexp-in-string` is idempotent for strings without underscores.

#### Scenario: Nested plists normalized recursively

**WHEN** a plist contains nested structure like `:bash_tools (:read_only ("ls") :safe_write ("mkdir"))`

**THEN** after normalization both outer and nested keys are converted:
```elisp
(:bash-tools (:read-only ("ls") :safe-write ("mkdir")))
```

**Location:** Lines 122–126 check if a value is a keyword-keyed plist and recursively normalize it.

### Requirement: YAML coercion fixes

The system SHALL fix known YAML-to-elisp coercion issues after parsing and key normalization, before registration.

**Implementation:** `jf/gptel-preset--coerce-values` in `/Users/jefffarr/emacs/config/gptel/preset-registration.org` lines 137–168.

Coercion SHALL handle:

- **`:model`** — string values SHALL be interned to symbols. Example: `"claude-opus-4-6"` becomes symbol `claude-opus-4-6` (lines 150–152)
- **`:confirm-tool-calls`** — Special string mappings:
  - `"nil"` becomes `nil` (line 156)
  - `:false` keyword becomes `nil` (line 157, caught by general rule)
  - `"auto"` becomes symbol `auto` (line 158)
  - `"always"` becomes `t` (line 159)
  - Other values pass through (line 160)
  (lines 154–160)
- **General boolean coercion** — Any key whose YAML value is the `:false` keyword SHALL be coerced to `nil` (line 164). This covers `:stream`, `:include-tool-results`, and any future boolean keys parsed from YAML `false`.
- **`:true` keyword** — YAML `true` parses as `:true` keyword. This is converted to `t` (line 162)
- **All other values pass through unchanged** (line 166)

#### Scenario: Model string converted to symbol

**WHEN** a parsed plist contains `:model "claude-opus-4-5"`

**THEN** after coercion the value is the symbol `claude-opus-4-5`

**Example transformation:**
```elisp
;; Input (after normalize)
(:model "claude-opus-4-5" :temperature 0.3)

;; Output (after coerce)
(:model claude-opus-4-5 :temperature 0.3)
```

**Location:** Lines 150–152 check `(eq key :model) (stringp val)` and intern the string.

#### Scenario: Confirm-tool-calls nil string converted

**WHEN** a parsed plist contains `:confirm-tool-calls "nil"`

**THEN** after coercion the value is `nil` (not the string `"nil"`)

**Location:** Line 156 checks `(equal val "nil")` and returns nil.

#### Scenario: Confirm-tool-calls auto string converted

**WHEN** a parsed plist contains `:confirm-tool-calls "auto"`

**THEN** after coercion the value is the symbol `auto`

**Location:** Line 158 checks `(equal val "auto")` and returns `'auto`.

#### Scenario: Confirm-tool-calls always string converted

**WHEN** a parsed plist contains `:confirm-tool-calls "always"`

**THEN** after coercion the value is `t`

**Location:** Line 159 checks `(equal val "always")` and returns `t`.

#### Scenario: Include-tool-results false converted

**WHEN** a parsed plist contains `:include-tool-results :false` (YAML `false` parsed as `:false` keyword)

**THEN** after coercion the value is `nil`

**Location:** Line 164 general rule: `((eq val :false) nil)`.

#### Scenario: Stream false converted via general boolean rule

**WHEN** a parsed plist contains `:stream :false`

**THEN** after coercion the value is `nil`

**Location:** Same general rule at line 164 applies to any key.

#### Scenario: YAML true converted

**WHEN** YAML frontmatter contains `confirm: true` (parses as `:true` keyword)

**THEN** after coercion the value is `t`

**Location:** Line 162 checks `(eq val :true)` and returns `t`.

#### Scenario: Unknown keys pass through unchanged

**WHEN** a parsed plist contains keys not in the coercion table and their values are not `:false` or `:true`

**THEN** their values pass through without modification

**Example:**
```elisp
;; Input (after normalize)
(:description "Some text" :temperature 0.5 :model "gpt-4")

;; Output (after coerce, assuming gpt-4 not known to coerce)
(:description "Some text" :temperature 0.5 :model gpt-4)
```

**Location:** Line 166 default case: `(t val)` returns the value unchanged.

### Requirement: Scope key extraction

The system SHALL extract scope-related keys from parsed preset plists before registration with `gptel-make-preset`. After key normalization, scope keys are: `:paths`, `:org-roam-patterns`, `:shell-commands`, `:bash-tools`, `:scope-profile`.

**Implementation:** `jf/gptel-preset--extract-scope` in `/Users/jefffarr/emacs/config/gptel/preset-registration.org` lines 177–201.

Extracted scope data SHALL be stored in `jf/gptel-preset--scope-defaults`, an alist mapping preset name symbols to scope plists.

The extraction function SHALL:
1. Separate scope keys from non-scope keys (lines 188–194)
2. Store scope keys only if at least one is present (line 196)
3. Update or insert preset entry in `jf/gptel-preset--scope-defaults` (lines 197–200)
4. Return a new plist with scope keys removed for registration (line 201)

#### Scenario: Scope keys stripped from preset plist

**WHEN** a parsed and normalized plist contains `:paths (:read ("/**"))`, `:org-roam-patterns ("projects")`, and `:shell-commands ("ls" "grep")`

**THEN** these keys are removed from the plist before calling `gptel-make-preset`

**AND** the returned plist for registration contains only non-scope keys:
```elisp
;; Input (after normalize and coerce)
(:model claude-sonnet-4-6
 :backend "Claude"
 :system "You are..."
 :paths (:read ("/**"))
 :org-roam-patterns ("projects")
 :shell-commands ("ls" "grep"))

;; Output (after extract-scope)
(:model claude-sonnet-4-6
 :backend "Claude"
 :system "You are...")
```

**Location:** Lines 188–194 partition keys; line 201 returns result with scope keys removed.

#### Scenario: Scope defaults stored by preset name

**WHEN** parsing preset "executor" with scope configuration `:paths (:read ("/**") :write ("/tmp/**"))`

**THEN** `jf/gptel-preset--scope-defaults` contains entry:
```elisp
(executor . (:paths (:read ("/**") :write ("/tmp/**"))))
```

**Location:** Lines 197–200 check if preset entry exists and update or create it.

#### Scenario: Scope profile reference extracted

**WHEN** a preset contains `scope_profile: "coding"` in YAML (normalized to `:scope-profile`)

**THEN** the scope defaults entry includes `:scope-profile "coding"`
```elisp
(executor . (:scope-profile "coding"))
```

**AND** `:scope-profile` is removed from the preset plist before registration

**Location:** `:scope-profile` is in `scope-keys` list at line 184; extraction removes it.

#### Scenario: Multiple scope keys in one preset

**WHEN** a preset contains `:scope-profile "coding"` AND `:paths (...)` AND `:bash-tools (...)`

**THEN** all three are extracted together into one scope-plist entry:
```elisp
(mypreset . (:scope-profile "coding"
             :paths (...)
             :bash-tools (...)))
```

**Location:** Lines 188–194 collect all matching keys; line 200 stores complete plist.

#### Scenario: Preset with no scope keys

**WHEN** a preset contains no scope-related keys

**THEN** no entry is added to `jf/gptel-preset--scope-defaults` for that preset

**AND** the preset plist is passed to `gptel-make-preset` unchanged (except coercion fixes)

**Location:** Line 196 check `(when scope-plist ...)` ensures nothing is stored if scope-plist is nil.

#### Scenario: Scope defaults used during session creation

**WHEN** a session is created with preset "executor"

**THEN** the scope subsystem looks up `(alist-get executor jf/gptel-preset--scope-defaults)` to initialize the session's scope

**Location:** `scope-profiles.org` lines 224–231 implement scope lookup for presets.

### Requirement: Preset registration via gptel-make-preset

The system SHALL register each parsed, normalized, coerced, and scope-stripped preset with upstream's `gptel-make-preset`, using the preset file's basename (sans extension) as the name symbol.

**Implementation:** `jf/gptel-preset-register-all` in `/Users/jefffarr/emacs/config/gptel/preset-registration.org` lines 210–234.

Value formats passed to `gptel-make-preset`:
- **`:backend`** — string name (e.g., `"Claude"`). Upstream's `gptel--apply-preset` resolves to a backend object at application time.
- **`:model`** — symbol (e.g., `claude-opus-4-6`). Already coerced from string by the parser.
- **`:tools`** — list of tool name strings (e.g., `("PersistentAgent" "Read" "Write")`). Upstream resolves to tool objects via `gptel-get-tool` at application time.
- **`:system`** — string (the markdown body from the preset file).
- **`:temperature`, `:description`** — passed through as-is.
- **No scope keys** — `:paths`, `:org-roam-patterns`, `:shell-commands`, `:bash-tools`, `:scope-profile` are excluded and stored separately.

The registration function SHALL:
1. Check presets directory exists (line 221)
2. Scan for `.md` files in directory (line 223)
3. For each file, extract basename as preset name (line 225)
4. Parse → normalize → coerce → extract scope → register (lines 227–232)
5. Count successful registrations and log (line 233)
6. Be idempotent (re-registration updates existing entries)

#### Scenario: All presets in directory registered

**WHEN** `jf/gptel-preset-register-all` runs at init time

**THEN** every `.md` file in `config/gptel/presets/` is parsed, normalized, coerced, scope-stripped, and registered

**AND** each preset is accessible via `(gptel-get-preset 'executor)`, `(gptel-get-preset 'research)`, etc.

**AND** logging shows: `"Registered 9 presets from <path>"`

**Location:** Lines 223–234 iterate `directory-files`, process each, count and log results.

#### Scenario: Preset name derived from filename

**WHEN** registering `config/gptel/presets/executor.md`

**THEN** the preset name symbol is `executor` (file-name-sans-extension, interned)

**AND** callable via `(gptel-get-preset 'executor)`

**Location:** Line 225 derives basename, line 226 interns to symbol.

#### Scenario: Invalid preset file skipped

**WHEN** a preset file fails to parse (e.g., invalid YAML)

**THEN** the file is skipped (parsed returns nil)

**AND** a warning was already logged by the parser

**AND** the count is not incremented for that file

**Location:** Line 228 checks `(when parsed ...)` to skip parsing failures.

#### Scenario: Presets visible in transient menu

**WHEN** presets are registered via `gptel-make-preset`

**THEN** they appear in gptel's transient preset menu (`@` key)

**AND** their `:description` is shown as an annotation during completion

**AND** can be selected by the user

**Location:** Upstream `gptel-make-preset` stores entries in `gptel--known-presets`; transient UI reads from that.

#### Scenario: Presets usable inline via @preset-name syntax

**WHEN** presets are registered in `gptel--known-presets`

**THEN** typing `@executor` in a prompt automatically applies the `executor` preset before sending the request

**AND** `@executor` is removed from the prompt text

**AND** completion is available for preset names via `gptel-preset-capf`

**Location:** Upstream gptel implements `@mention` syntax; registration populates the registry.

#### Scenario: Presets usable with gptel-with-preset

**WHEN** a preset `executor` is registered

**THEN** `(gptel-with-preset 'executor ...)` correctly applies the preset's configuration

**Location:** Upstream gptel's `gptel-with-preset` macro looks up the preset name in `gptel--known-presets`.

#### Scenario: Duplicate registration updates existing

**WHEN** `jf/gptel-preset-register-all` is called again (e.g., manual refresh via M-x)

**THEN** existing entries in `gptel--known-presets` are updated (not duplicated)

**AND** changes to preset files are reflected on re-registration

**Location:** `gptel-make-preset` is idempotent; calling it twice with same name updates the entry.

### Requirement: Registration timing and dependencies

Preset registration SHALL run during gptel initialization, after `yaml.el` and `gptel` are loaded, but before any session hooks fire.

**Implementation:** Initialization sequencing in `/Users/jefffarr/emacs/config/gptel/gptel.org` lines 220–234.

The initialization order is:
1. Load constants module (defines `jf/gptel-presets-directory`) — line 221
2. Load logging module (used by preset parser warnings) — line 222
3. Require `yaml` — line 225
4. Load preset-registration module — line 228
5. Call `jf/gptel-preset-register-all` — line 231
6. Call `jf/gptel-preset--expand-all-preset-skills` (skills expansion) — line 234
7. Load scope-profiles — line 237
8. Load scope-core — line 240
9. Load session modules (which may open existing sessions) — later

#### Scenario: Registration before session open

**WHEN** a user opens an existing session.md file (e.g., via find-file)

**THEN** `gptel--restore-state` can find the preset by name in `gptel--known-presets`

**AND** preset was registered before the find-file-hook ran

**AND** session can apply the preset during restore

**Location:** Init sequence ensures registration (line 231) runs before session hooks can fire.

#### Scenario: yaml.el required before parsing

**WHEN** `jf/gptel-preset-register-all` runs at line 231

**THEN** `(require 'yaml)` has already been called at line 225

**AND** `yaml-parse-string` is available

**AND** parser does not fail with "Unknown function: yaml-parse-string"

**Location:** Line 225 ensures yaml is loaded before registration at line 231.

#### Scenario: Constants available for path resolution

**WHEN** preset registration runs

**THEN** `jf/gptel-presets-directory` is defined and accessible

**AND** can be customized by user before init completes

**Location:** Constants loaded at line 221 define `jf/gptel-presets-directory` (from `constants.org` lines 46–51).

#### Scenario: Skills expansion runs after registration

**WHEN** presets are registered at line 231

**THEN** `jf/gptel-preset--expand-all-preset-skills` at line 234 iterates `gptel--known-presets` to expand `@skill` tokens in `:system` strings

**AND** runs once after all presets are registered and before user sessions open

**AND** skill content is injected into preset system prompts

**Location:** Lines 231–234 show explicit sequencing: registration then skill expansion.

### Requirement: Module location

The preset registration pipeline SHALL be defined in `config/gptel/preset-registration.org` (tangled to `preset-registration.el`), providing feature `gptel-preset-registration`.

**Implementation:** `/Users/jefffarr/emacs/config/gptel/preset-registration.org` and tangled `/Users/jefffarr/emacs/config/gptel/preset-registration.el`.

The module provides:
- **`jf/gptel-preset-register-all`** — Main orchestration function (lines 210–234)
- **`jf/gptel-preset--parse-file`** — File parsing (lines 61–98)
- **`jf/gptel-preset--normalize-keys`** — Key normalization (lines 110–128)
- **`jf/gptel-preset--coerce-values`** — Type coercion (lines 137–168)
- **`jf/gptel-preset--extract-scope`** — Scope extraction (lines 177–201)
- **`jf/gptel-preset--scope-defaults`** — Side table variable (lines 47–51)

#### Scenario: Module loaded during gptel init

**WHEN** `config/gptel/gptel.org` initializes

**THEN** it loads `config/gptel/preset-registration.el` at line 228 after `yaml.el` and `gptel` are available

**AND** before session module loading (constants and logging are loaded first, then preset-registration, then scope-profiles)

**Location:** Lines 225–228 of `gptel.org` show dependency ordering.

#### Scenario: Module provides all registration functions

**WHEN** `gptel-preset-registration` is loaded

**THEN** calling `(jf/gptel-preset-register-all)` works

**AND** these functions are defined: `jf/gptel-preset--parse-file`, `jf/gptel-preset--normalize-keys`, `jf/gptel-preset--coerce-values`, `jf/gptel-preset--extract-scope`

**AND** variable `jf/gptel-preset--scope-defaults` is initialized as `nil`

**Location:** Lines 240–241 of `preset-registration.org` provide the feature.

#### Scenario: Module feature declaration

**WHEN** the module is tangled and loaded

**THEN** `(require 'gptel-preset-registration)` succeeds

**AND** feature `gptel-preset-registration` is registered

**Location:** Line 240 includes `(provide 'gptel-preset-registration)`.

### Requirement: Presets directory configuration

The system SHALL use a configurable directory path for preset file discovery.

**Implementation:** Variable `jf/gptel-presets-directory` in `/Users/jefffarr/emacs/config/gptel/sessions/constants.org` lines 46–51 (tangled to `constants.el`).

#### Scenario: Default presets directory

**WHEN** `jf/gptel-presets-directory` is not customized

**THEN** it defaults to `(expand-file-name "config/gptel/presets/" jf/emacs-dir)`

**AND** resolves to absolute path `/Users/jefffarr/emacs/config/gptel/presets/` (or equivalent on other machines)

**AND** function `jf/gptel-preset-register-all` uses this directory to scan for presets

**Location:** `constants.org` line 47 defines the expansion; `preset-registration.org` line 219 uses the variable.

#### Scenario: Only .md files scanned

**WHEN** scanning the presets directory

**THEN** only files with `.md` extension are processed (via `directory-files` with pattern `"\\.md\\'"`)

**AND** subdirectories, dotfiles, and non-markdown files are ignored

**Location:** Line 223 of `preset-registration.org`: `(directory-files presets-dir t "\\.md\\'" t)`.

#### Scenario: Directory configuration is customizable

**WHEN** a user customizes `jf/gptel-presets-directory` to a different path

**THEN** preset registration scans that path instead of the default

**AND** custom presets in the alternate location are registered

**Location:** `defcustom` declaration at line 46 of `constants.org` allows customization via `M-x customize-variable`.

### Requirement: Skills expansion integration

After preset registration, the system SHALL expand `@skill` mentions in preset system prompts.

**Implementation:** Function `jf/gptel-preset--expand-all-preset-skills` in `/Users/jefffarr/emacs/config/gptel/gptel.org` lines 193–211.

This function SHALL:
1. Check skills system is loaded (functions `jf/gptel-skills--detect-mentions`, `jf/gptel-skills--discover` are defined) and registry is bound (line 196)
2. Discover skills via `jf/gptel-skills--discover` (line 199)
3. Iterate all registered presets in `gptel--known-presets` (line 202)
4. For each preset's `:system` string, expand `@skill` mentions by replacing them with skill content (lines 207–210)
5. Update the preset's `:system` in-place if any expansions occurred (line 209)

#### Scenario: Skills expanded in preset system prompts

**WHEN** a preset's `:system` contains text like `"See @agile-methodology for details..."` (where `@agile-methodology` is a registered skill)

**THEN** after expansion, `:system` is updated to include the full skill content in place of the `@agile-methodology` mention

**Location:** Lines 185–188 replace `@mention` with skill content.

#### Scenario: Expansion happens after all presets registered

**WHEN** `jf/gptel-preset-register-all` completes (line 231)

**THEN** `jf/gptel-preset--expand-all-preset-skills` runs immediately (line 234)

**AND** operates on the complete `gptel--known-presets` registry

**Location:** Lines 231 and 234 show explicit sequencing.

#### Scenario: Skills not available — expansion skipped

**WHEN** skills subsystem is not loaded (functions not defined or registry not bound)

**THEN** `jf/gptel-preset--expand-all-preset-skills` skips processing (line 196 condition fails)

**AND** preset system prompts remain unchanged

**AND** no errors are signaled

**Location:** Lines 196–197 check for skills availability before proceeding.

#### Scenario: No @mentions in preset — unchanged

**WHEN** a preset's `:system` contains no `@mention` syntax

**THEN** the preset remains unchanged after expansion

**Location:** Line 171 checks `(string-match-p "@" system-text)` before processing.

### Requirement: Upstream preset features — out of scope

Upstream's `gptel-make-preset` supports advanced features that this registration pipeline SHALL NOT use in the current implementation. These are documented here for awareness and possible future adoption:

- **`:parents`** — preset inheritance chains (apply parent presets before this one)
- **`:pre` / `:post`** — hook functions run before/after preset application
- **Value modifiers** — `(:append LIST)`, `(:prepend LIST)`, `(:eval FORM)`, `(:function FUNC)`, `(:merge PLIST)` for dynamic value composition
- **`:system` as symbol** — resolves against `gptel-directives` alist

These features may be adopted in future iterations. For now, presets are registered with flat plists containing concrete values only.

**Location:** `preset-registration.org` lines 205–214 document this intentional limitation.
