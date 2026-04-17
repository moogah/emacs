# Scope System

## Purpose

The scope system gates gptel tool invocations against a `scope.yml` permission file living in the session's branch directory. Filesystem tools (read, write, edit) and the `run_bash_command` tool share a single authorization entrypoint that loads config, dispatches to the appropriate validator, and — on denial — routes through an inline expansion UI where the user can deny, allow for the pending invocation, or write a new pattern to `scope.yml`. Validation is semantic, not categorical: bash commands are judged by the file operations bash-parser extracts from them, not by command-name allowlists.

## Module Overview

All modules live under `config/gptel/scope/`.

| File                        | Responsibilities                                                               |
|-----------------------------|---------------------------------------------------------------------------------|
| `scope-tool-wrapper.el`     | `gptel-make-scoped-tool` macro (always async; delegates to the authorize entry) |
| `scope-validation.el`       | Config load, validators, bash pipeline, glob match, authorize dispatcher, violation-info, error formatter, expansion trigger |
| `scope-yaml.el`             | YAML parse, vector→list, snake_case→kebab-case normalization, schema defaults, merge |
| `scope-metadata.el`         | File metadata gathering (exists, git-tracked, git-repo, type)                   |
| `scope-filesystem-tools.el` | `read_file_in_scope`, `write_file_in_scope`, `edit_file_in_scope`               |
| `scope-shell-tools.el`      | `run_bash_command` tool, `request_scope_expansion` tool, command execution helpers |
| `scope-expansion.el`        | Transient menu, `scope.yml` writer, queue (referenced here; full spec in scope-expansion) |
| `interfaces.el`             | Executable contracts: canonical error codes, validation-result/violation-info validators, glob test cases |

Config loading, metadata gathering, validation dispatch, expansion escalation, and error formatting all live in `scope-validation.el`. `scope-tool-wrapper.el` is a pure macro shim. `scope-shell-tools.el` contains no validation logic.

## Requirements

### Requirement: Scope configuration loading

The scope system SHALL load `scope.yml` fresh on every tool invocation from the session's branch directory. Configuration is never cached; the filesystem is the source of truth.

**Implementation**: `config/gptel/scope/scope-validation.org` (loader), `config/gptel/scope/scope-yaml.org` (parser)

#### Scenario: Config resolved from buffer-local branch directory
- **WHEN** a scoped tool is invoked
- **THEN** the loader uses buffer-local `jf/gptel--branch-dir` when bound
- **AND** falls back to the current buffer's file directory when that variable is unset

#### Scenario: Missing configuration denies with no_scope_config
- **WHEN** no `scope.yml` exists at the resolved location
- **THEN** the authorization entrypoint invokes the on-deny thunk with `:error "no_scope_config"`
- **AND** the response bypasses the expansion UI (missing config is not a scope violation)

#### Scenario: Configuration read fresh every call
- **WHEN** the same tool is called twice in a session after `scope.yml` is edited
- **THEN** each call parses the file again; there is no in-memory cache

### Requirement: Scope configuration shape

Parsed `scope.yml` SHALL produce a plist with `:paths`, `:cloud`, and `:security` sections. Missing sections are merged with `jf/gptel-scope-yaml--schema-defaults`. YAML keys are snake_case on disk and kebab-case keywords in elisp.

**Implementation**: `config/gptel/scope/scope-yaml.org`

The canonical shape:

```
(:paths (:read    (PATTERN ...)
         :write   (PATTERN ...)
         :modify  (PATTERN ...)
         :execute (PATTERN ...)
         :deny    (PATTERN ...))
 :cloud (:auth-detection     "allow"|"warn"|"deny"
         :allowed-providers  (PROVIDER ...))
 :security (:enforce-parse-complete  BOOL
            :max-coverage-threshold  FLOAT))
```

#### Scenario: Snake_case keys normalized to kebab-case
- **WHEN** YAML contains `auth_detection: warn` inside `cloud:`
- **THEN** the elisp plist exposes `(:cloud (:auth-detection "warn" ...))`

#### Scenario: YAML booleans normalized
- **WHEN** YAML contains `enforce_parse_complete: true`
- **THEN** the elisp plist value is `t` (not the `:true` keyword)
- **AND** `false` or `null` become `nil`

#### Scenario: Missing sections filled from defaults
- **WHEN** `scope.yml` omits the `security` section
- **THEN** the merged config carries `:enforce-parse-complete t` and `:max-coverage-threshold 0.8`

#### Scenario: Invalid auth_detection rejected on load
- **WHEN** `cloud.auth_detection` is anything other than `"allow"`, `"warn"`, or `"deny"`
- **THEN** `jf/gptel-scope-yaml--validate-cloud-config` signals a schema error

#### Scenario: Invalid coverage threshold rejected on load
- **WHEN** `security.max_coverage_threshold` is not a number in `[0.0, 1.0]`
- **THEN** `jf/gptel-scope-yaml--validate-security-config` signals a schema error

### Requirement: Scoped tool macro

The `gptel-make-scoped-tool` macro SHALL be the only way to define scope-aware tools. It binds arguments, delegates to the authorization dispatcher, and routes the result through the gptel async callback.

**Implementation**: `config/gptel/scope/scope-tool-wrapper.org`

#### Scenario: Tools are always async
- **WHEN** a tool is defined with `gptel-make-scoped-tool`
- **THEN** the resulting `gptel-make-tool` form carries `:async t`
- **AND** the body runs only when the on-allow thunk is invoked (possibly on a later turn, after the expansion UI resolves)

#### Scenario: Declared operation for filesystem tools
- **WHEN** the macro is given `:operation read` (or `write`/`modify`/`execute`)
- **THEN** the authorize dispatcher routes to filesystem path validation with that operation

#### Scenario: Omitted operation for bash-style tools
- **WHEN** the macro is invoked with no `:operation` keyword
- **THEN** the authorize dispatcher routes to bash semantic validation and operations are extracted from input

#### Scenario: Tool body exceptions caught as tool_exception
- **WHEN** the body signals an elisp error after authorization passes
- **THEN** the callback is invoked with `:error "tool_exception"` and the error message

#### Scenario: No validation logic in the macro
- **WHEN** authoring a new scoped tool
- **THEN** neither the macro nor the tool body loads config, gathers metadata, or consults the expansion UI — all of that happens inside `jf/gptel-scope-authorize-tool-call`

### Requirement: Authorization entrypoint

`jf/gptel-scope-authorize-tool-call` SHALL be the single public authorization entrypoint. All scoped tool dispatches go through it; no other code path consults the validation engine.

**Implementation**: `config/gptel/scope/scope-validation.org`

#### Scenario: Entrypoint loads config, validates, dispatches
- **WHEN** the entrypoint is invoked with tool-name, operation, args, on-allow, on-deny
- **THEN** it loads `scope.yml`, validates, and on success funcalls on-allow
- **AND** on failure triggers the inline expansion UI

#### Scenario: Entrypoint routes filesystem vs bash by :operation
- **WHEN** `:operation` is a filesystem symbol (`read`/`write`/`modify`/`execute`)
- **THEN** the entrypoint gathers file metadata and calls `validate-filesystem-tool`
- **AND** tags the result with `:validation-type 'path`

#### Scenario: Entrypoint routes nil operation to bash validator
- **WHEN** `:operation` is nil
- **THEN** the entrypoint calls `validate-bash-tool`
- **AND** tags the result with `:validation-type 'bash`

#### Scenario: Add-to-scope re-invokes the entrypoint
- **WHEN** the user chooses "add to scope" in the expansion UI
- **THEN** the dispatcher calls itself again with the same arguments
- **AND** the pipeline re-validates against the now-updated `scope.yml`
- **AND** if another operation is still denied, the UI prompts again

#### Scenario: Allow-once skips re-validation
- **WHEN** the expansion UI returns `:allowed-once t`
- **THEN** the dispatcher funcalls on-allow directly without re-invoking itself

### Requirement: Path operation validation

`jf/gptel-scope--validate-path-operation` SHALL be the single path validator used by both filesystem tools and bash Stage 3. It implements the permission hierarchy and deny precedence.

**Implementation**: `config/gptel/scope/scope-validation.org`

Permission hierarchy:
- read-like (`:read`, `:read-directory`, `:read-metadata`, `:match-pattern`) → requires `paths.read` OR `paths.write` (write implies read)
- write-like (`:write`, `:create`, `:create-or-modify`, `:append`, `:delete`) → requires `paths.write`
- `:modify` → requires `paths.modify` OR `paths.write`
- `:execute` → requires `paths.execute` only
- `paths.deny` takes absolute precedence over every allow pattern

#### Scenario: Deny pattern overrides allow pattern
- **WHEN** a path matches both an allow pattern and a deny pattern
- **THEN** the validator returns `:error "denied-pattern"`

#### Scenario: Read allowed via paths.write
- **WHEN** operation is `:read` on `/tmp/file.txt`
- **AND** `paths.write` contains `/tmp/**` but `paths.read` is empty
- **THEN** the validator allows the operation

#### Scenario: Write denied when only read is scoped
- **WHEN** operation is `:write` on `/workspace/output.txt`
- **AND** `paths.read` contains `/workspace/**` but `paths.write` does not
- **THEN** the validator returns `:error "not-in-scope"`

#### Scenario: Execute requires paths.execute alone
- **WHEN** operation is `:execute` on `/workspace/scripts/deploy.py`
- **AND** `paths.read` and `paths.write` cover the path but `paths.execute` does not
- **THEN** the validator returns `:error "not-in-scope"`

#### Scenario: Filesystem tool retries with resolved symlink
- **WHEN** the original expanded path fails validation
- **AND** its `file-truename` resolves to a different path
- **THEN** `validate-filesystem-tool` re-runs the validator against the resolved path
- **AND** accepts whichever path form passes; otherwise the original denial is returned annotated with `:tool`

### Requirement: Glob pattern matching

There SHALL be exactly one glob-to-regex implementation (`jf/gptel-scope--glob-to-regex`) and one top-level matcher (`jf/gptel-scope--path-matches-any-pattern-p`) in the scope system. Both filesystem and bash code paths reach them through the shared path validator.

**Implementation**: `config/gptel/scope/scope-validation.org`

The pattern vocabulary:

| Pattern | Matches                           |
|---------|-----------------------------------|
| `*`     | Any characters except `/`         |
| `**`    | Any characters including `/`      |
| `?`     | Exactly one character             |
| `/**/`  | Zero or more directory components |

#### Scenario: Recursive wildcard matches across directories
- **WHEN** pattern is `/workspace/**` and path is `/workspace/sub/file.txt`
- **THEN** the matcher returns true

#### Scenario: Single-segment wildcard does not cross slash
- **WHEN** pattern is `/tmp/*` and path is `/tmp/sub/file`
- **THEN** the matcher returns false

#### Scenario: Extension pattern respects slash boundary
- **WHEN** pattern is `/workspace/*.el` and path is `/workspace/sub/init.el`
- **THEN** the matcher returns false

#### Scenario: Middle-of-path recursive segment
- **WHEN** pattern is `**/.git/**` and path is `/workspace/sub/.git/HEAD`
- **THEN** the matcher returns true

### Requirement: Filesystem tools

`scope-filesystem-tools.el` SHALL define three scoped tools that read, write, and edit files. Each declares its operation and delegates all scope checks to the macro.

**Implementation**: `config/gptel/scope/scope-filesystem-tools.org`

#### Scenario: read_file_in_scope reads when allowed
- **WHEN** `read_file_in_scope` is invoked with a filepath that passes `:read` validation
- **AND** the file exists
- **THEN** the tool returns `:success t`, `:content <file contents>`, `:full_path <expanded>`

#### Scenario: read_file_in_scope reports file_not_found
- **WHEN** validation passes but the file does not exist
- **THEN** the tool returns `:success nil`, `:error "file_not_found"`

#### Scenario: write_file_in_scope creates parent directory
- **WHEN** `write_file_in_scope` is authorized for a path whose parent directory does not exist
- **THEN** the tool creates the parent directory and writes the content

#### Scenario: edit_file_in_scope requires existing file
- **WHEN** `edit_file_in_scope` is invoked on a non-existent file
- **THEN** the tool returns `:success nil`, `:error "file_not_found"` and directs the caller to `write_file_in_scope`

#### Scenario: edit_file_in_scope reports string_not_found
- **WHEN** the `old_string` argument is not present in the file
- **THEN** the tool returns `:success nil`, `:error "string_not_found"` and leaves the file unchanged

### Requirement: Bash validation pipeline

`jf/gptel-scope--validate-command-semantics` SHALL run four stages in order with early exit on the first failure, followed by a non-blocking coverage warning. Each stage returns nil on success and an error plist on denial.

**Implementation**: `config/gptel/scope/scope-validation.org`

| Stage | Function                        | Purpose                                                      |
|-------|---------------------------------|--------------------------------------------------------------|
| 1     | `validate-parse-completeness`   | Refuse to validate commands bash-parser couldn't fully parse |
| 2     | `check-no-op`                   | Zero-op commands exit the pipeline early                     |
| 3     | `validate-file-operations`      | Route each extracted file op through `validate-path-operation` |
| 4     | `validate-cloud-auth`           | Apply `cloud.auth-detection` mode and `allowed-providers`    |
| —     | `check-coverage-threshold`      | Non-blocking warning below `security.max-coverage-threshold` |

#### Scenario: Early exit on first failure
- **WHEN** Stage 1 returns a parse_incomplete error
- **THEN** the pipeline does not run Stages 2, 3, or 4

#### Scenario: Successful command returns nil
- **WHEN** every stage passes
- **THEN** the orchestrator returns nil and `validate-bash-tool` reports `:allowed t`

#### Scenario: Relative paths resolved against default-directory
- **WHEN** Stage 3 validates a file op with a relative path
- **THEN** the path is expanded against `default-directory` (bound from session context)

#### Scenario: First denied file op ends the stage
- **WHEN** Stage 3 encounters multiple file ops and the first is denied
- **THEN** the validator returns that denial and does not check the remaining ops
- **AND** the expansion UI prompts for that op; an "add to scope" choice re-runs the pipeline and surfaces the next violation (if any)

### Requirement: Parse completeness gate

Stage 1 SHALL refuse to validate commands when `bash-parser` reports `:parse-complete nil` and `security.enforce-parse-complete` is true.

**Implementation**: `config/gptel/scope/scope-validation.org` (Stage 1)

#### Scenario: Incomplete parse rejected when enforced
- **WHEN** the command has a syntax error (e.g. unclosed quote)
- **AND** `security.enforce-parse-complete` is `t`
- **THEN** Stage 1 returns `:error "parse_incomplete"` with `:parse-errors` and `:command`

#### Scenario: Incomplete parse warned when not enforced
- **WHEN** `security.enforce-parse-complete` is `nil`
- **AND** the parse is incomplete
- **THEN** Stage 1 emits an elisp `warn` and returns nil (pipeline continues)

#### Scenario: Complete parse proceeds silently
- **WHEN** `:parse-complete` is `t`
- **THEN** Stage 1 returns nil regardless of the enforce flag

### Requirement: No-op allowance

Stage 2 SHALL short-circuit the pipeline when the parsed command produces zero filesystem operations. This is a deliberate policy gate, not an optimization: zero-op commands also bypass cloud-auth checks.

**Implementation**: `config/gptel/scope/scope-validation.org` (Stage 2)

#### Scenario: Version check allowed with no config entries
- **WHEN** command is `git --version`
- **AND** bash-parser extracts zero file ops
- **THEN** the pipeline returns nil at Stage 2 without consulting `paths` or `cloud`

#### Scenario: Help flag allowed
- **WHEN** command is `ls --help`
- **AND** bash-parser extracts zero file ops
- **THEN** Stage 2 short-circuits and the command is permitted

#### Scenario: Pipeline of zero-op commands allowed
- **WHEN** command is `echo hello | wc -c`
- **AND** the whole pipeline extracts zero file ops
- **THEN** Stage 2 short-circuits

#### Scenario: Command with any file op skips the no-op gate
- **WHEN** command is `cat /workspace/file.txt`
- **AND** bash-parser extracts at least one file op
- **THEN** Stage 2 returns control to the orchestrator and Stage 3 runs

### Requirement: Operation-specific file path validation

Stage 3 SHALL route every extracted file op through `jf/gptel-scope--validate-path-operation`, guaranteeing that bash `cat /etc/passwd` and `read_file_in_scope "/etc/passwd"` resolve to the same allow/deny decision.

**Implementation**: `config/gptel/scope/scope-validation.org` (Stage 3)

#### Scenario: Read op matches paths.read
- **WHEN** command is `cat /workspace/file.txt`
- **AND** `paths.read` contains `/workspace/**`
- **THEN** Stage 3 returns nil

#### Scenario: Cp extracts read and write ops
- **WHEN** command is `cp /workspace/src.txt /tmp/dst.txt`
- **AND** `paths.read` covers `/workspace/**` and `paths.write` covers `/tmp/**`
- **THEN** both ops validate and Stage 3 returns nil

#### Scenario: Denied path rejected
- **WHEN** command is `cat /etc/passwd`
- **AND** `paths.deny` contains `/etc/**`
- **THEN** Stage 3 returns `:error "denied-pattern"` with `:resource "/etc/passwd"`

#### Scenario: Pipeline bypass closed
- **WHEN** command is `find . | xargs rm /workspace/secrets/*`
- **AND** the extracted `rm` operation targets a path outside `paths.write`
- **THEN** Stage 3 returns `:error "not-in-scope"` for the `rm` target (not just the pipeline head)

#### Scenario: Wrapper preserves inner :resource
- **WHEN** `validate-file-operation` produces a denial plist with `:resource`
- **THEN** `validate-bash-tool` prepends only `:tool` and `:command` and does not overwrite `:resource`

### Requirement: Cloud authentication policy

Stage 4 SHALL classify any detected cloud authentication against `cloud.auth-detection` (allow/warn/deny) and filter by `cloud.allowed-providers`.

**Implementation**: `config/gptel/scope/scope-validation.org` (Stage 4)

#### Scenario: Allow mode permits any provider
- **WHEN** `cloud.auth-detection` is `"allow"`
- **AND** command is `aws-vault exec prod -- aws s3 ls`
- **THEN** Stage 4 returns nil

#### Scenario: Warn mode annotates but does not deny
- **WHEN** `cloud.auth-detection` is `"warn"`
- **AND** cloud auth is detected
- **THEN** Stage 4 returns a plist with `:warning "cloud_auth_detected"` (not `:error`)
- **AND** the pipeline continues

#### Scenario: Deny mode rejects cloud commands
- **WHEN** `cloud.auth-detection` is `"deny"`
- **AND** command is `gcloud auth login`
- **THEN** Stage 4 returns `:error "cloud_auth_denied"` with `:provider` and `:command`

#### Scenario: Provider filter rejects disallowed provider
- **WHEN** `cloud.allowed-providers` is `["aws"]`
- **AND** command uses GCP
- **THEN** Stage 4 returns `:error "cloud_provider_denied"` with `:allowed-providers`

#### Scenario: Provider filter permits listed provider
- **WHEN** `cloud.allowed-providers` contains the detected provider
- **AND** `auth-detection` is `"deny"`
- **THEN** the provider filter passes and Stage 4's mode branch runs (here, denies only if mode is still `"deny"`; when the provider is explicitly allowed the caller's policy typically sets mode to `"allow"` or `"warn"`)

### Requirement: Coverage threshold warning

The coverage check SHALL be non-blocking: it emits an elisp `warn` when `bash-parser`'s semantic plugin coverage ratio is below `security.max-coverage-threshold` and otherwise returns nil.

**Implementation**: `config/gptel/scope/scope-validation.org`

#### Scenario: Below-threshold coverage warns
- **WHEN** `security.max-coverage-threshold` is `0.8`
- **AND** `:coverage-ratio` is `0.5`
- **THEN** the validator emits a `warn` and the pipeline still succeeds

#### Scenario: At-or-above threshold silent
- **WHEN** `:coverage-ratio` meets or exceeds the threshold
- **THEN** no warning is emitted

### Requirement: Canonical error codes

Validators SHALL produce only codes from the canonical set. Every consumer (`build-violation-info`, `format-tool-error`, the expansion UI) SHALL handle every code in the set.

**Implementation**: `config/gptel/scope/interfaces.org`

Canonical codes:
- `denied-pattern` — path matched `paths.deny`
- `not-in-scope` — path not covered by any allow pattern for the operation
- `parse_incomplete` — bash-parser incomplete and enforcement is on
- `cloud_auth_denied` — cloud auth detected and policy is deny
- `cloud_provider_denied` — provider not in `allowed-providers`

Macro-level codes outside the validation vocabulary: `no_scope_config`, `tool_exception`.

Error-code to resource-field mapping used by `build-violation-info`:

| Error code               | Resource field |
|--------------------------|----------------|
| `denied-pattern`         | `:resource`    |
| `not-in-scope`           | `:resource`    |
| `parse_incomplete`       | `:command`     |
| `cloud_auth_denied`      | `:provider`    |
| `cloud_provider_denied`  | `:provider`    |

#### Scenario: Every validator denial carries a canonical code
- **WHEN** any validator returns `(:allowed nil ...)`
- **THEN** `:error` is a member of the canonical set

#### Scenario: no_scope_config surfaces verbatim
- **WHEN** the authorization entrypoint finds no config
- **THEN** the on-deny plist carries `:error "no_scope_config"` and is NOT transformed by `format-tool-error`

### Requirement: Violation-info transformation

`jf/gptel-scope--build-violation-info` SHALL transform a validation-result plist into the violation-info shape required by the expansion UI: `:tool`, `:resource`, `:operation`, `:reason`, `:validation-type`, `:metadata`. The `:validation-type` key is attached by the authorization entrypoint; the builder must not recompute it.

**Implementation**: `config/gptel/scope/scope-validation.org`

#### Scenario: Validation type vocabulary is exactly {path, bash}
- **WHEN** `build-violation-info` runs
- **THEN** `:validation-type` is either `path` or `bash`; no other values are produced

#### Scenario: Resource pulled by error-code mapping
- **WHEN** the validation error carries `:error "cloud_provider_denied"` and `:provider "gcp"`
- **THEN** the violation-info `:resource` is `"gcp"`

#### Scenario: Reason is the human-readable :message
- **WHEN** `build-violation-info` produces a plist
- **THEN** `:reason` is copied from the validator's `:message` (not from `:error`)

### Requirement: Allow-once semantics

"Allow once" SHALL be a stateless expansion-UI choice. No list is maintained; no permission is persisted or consumed on a subsequent call. A following invocation of the same resource that fails validation will prompt again.

**Implementation**: `config/gptel/scope/scope-expansion.org` (UI choice), `config/gptel/scope/scope-validation.org` (dispatcher)

#### Scenario: Allow-once funcalls on-allow for the pending invocation
- **WHEN** the user picks "Allow once" in the expansion transient
- **THEN** the expansion UI resolves with `:success t :allowed_once t`
- **AND** the dispatcher funcalls the wrapper's on-allow thunk (runs the body)

#### Scenario: No persistence across invocations
- **WHEN** a tool is allowed once and the same tool+resource is invoked again later
- **AND** `scope.yml` has not been updated
- **THEN** validation denies again and the expansion UI prompts again

#### Scenario: Allow-once bypasses re-validation within a single prompt cycle
- **WHEN** the user picks "Allow once"
- **THEN** the dispatcher does NOT re-invoke `authorize-tool-call` (unlike add-to-scope)
- **AND** the body runs exactly once, covering all remaining violations in the pending call

### Requirement: File metadata gathering

For filesystem (path) tools the authorization entrypoint SHALL gather a metadata plist before validation. Metadata is I/O isolated from validation logic.

**Implementation**: `config/gptel/scope/scope-metadata.org`

The metadata plist carries: `:path`, `:real-path`, `:exists`, `:git-tracked`, `:git-repo`, `:type` (`file` / `directory` / `other`).

#### Scenario: Metadata gathered for filesystem tools only
- **WHEN** the dispatcher handles a tool with a declared `:operation`
- **THEN** it calls `jf/gptel-scope--gather-file-metadata` against the first argument and passes the plist to `validate-filesystem-tool`

#### Scenario: Bash path skips metadata gathering
- **WHEN** the dispatcher handles a bash-style tool (operation nil)
- **THEN** metadata is not gathered; the bash validator consults the shared path validator with paths alone

#### Scenario: Missing git degrades gracefully
- **WHEN** `git` is not available or the path is not inside a repo
- **THEN** `:git-tracked` and `:git-repo` are nil and metadata gathering does not error

## Integration Points

- **Preset system** — scope profiles are extracted during preset registration and written into `scope.yml` at session creation; see `openspec/specs/gptel/scope-profiles.md` and `preset-registration.md`.
- **Session persistence** — `scope.yml` lives in `~/.gptel/sessions/<id>/branches/<branch>/`; the loader resolves it via buffer-local `jf/gptel--branch-dir`. See `openspec/specs/gptel/sessions-persistence.md`.
- **Expansion UI** — denied calls funnel into the inline transient defined in `openspec/specs/gptel/scope-expansion.md`. `request_scope_expansion` in `scope-shell-tools.el` is a plain gptel tool (registered via `gptel-make-tool`) that the LLM invokes directly; it calls the same `jf/gptel-scope-prompt-expansion` the dispatcher uses.
- **gptel package** — scoped tools register through `gptel-make-tool` under the `"scope"` category and run with `:async t`; `scope-tool-wrapper.el` is their only entry point.
