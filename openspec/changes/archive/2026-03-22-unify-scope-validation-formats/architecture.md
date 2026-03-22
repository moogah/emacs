## Components

### Dispatch validators (scope-core)

Three tool-type-specific validators called by `check-tool-permission`:

- **validate-path-tool** — Extracts filepath from first argument, validates against `paths.read`/`paths.write`/`paths.deny` globs. Returns `(:allowed t)` or denial plist with `:error` + `:message`.
- **validate-pattern-tool** — Extracts subdirectory/tags/node-ids from arguments, validates against `org_roam_patterns`. Returns `(:allowed t)` or denial plist with `:error` + `:message`.
- **validate-bash-tool** — Gate validator only. Checks for bash_tools config presence and categories migration error. Returns `(:allowed t :validation 'semantic)` to defer real validation to tool body. Denial plist uses `:error` + `:message`.

### Pipeline validators (scope-shell-tools)

Seven-stage pipeline called from `run_bash_command` tool body via `validate-command-semantics`:

- **validate-parse-completeness** — Stage 1. Returns nil or `(:error "parse_incomplete" :message ...)`.
- **validate-pipeline-commands** — Stage 3. Returns nil or `(:error "command_denied" :command ... :message ...)`.
- **validate-operation** — Stage 5 (per file-op). Returns nil or `(:error "path_denied"|"path_out_of_scope" :path ... :message ...)`.
- **validate-cloud-auth** — Stage 6. Returns nil or `(:error "cloud_auth_denied" :provider ... :message ...)`.

These already use the target format (`:error` + `:message`). No changes needed.

### Violation-info transformer (scope-core)

- **build-violation-info** — Transforms validator denial plists into the violation-info format consumed by the expansion UI. Reads `:error` for resource extraction routing, reads `:message` for the `:reason` output field. No fallback chains.

### Expansion UI consumer (scope-expansion)

- **scope-expansion transient menu** — Reads `:tool`, `:resource`, `:reason`, `:validation-type`, `:metadata` from violation-info. No changes needed — it already reads the correct fields.

## Interfaces

### Unified validator denial format

All validators that deny an operation return a plist with these fields:

```
Required:
  :error    STRING   Machine-readable code (e.g., "denied-pattern", "path_out_of_scope", "command_denied")
  :message  STRING   Human-readable explanation for the user

Contextual (varies by error type):
  :path       STRING   For path-based denials
  :command    STRING   For command denials
  :provider   STRING   For cloud auth denials
  :operation  KEYWORD  The operation type (:read, :write, etc.)
  :tool       STRING   Tool name (dispatch validators only)
  :resource   STRING   Resource identifier (dispatch validators only)

Forbidden:
  :reason              NOT used in validator returns (reserved for violation-info output)
```

### Dispatch validator return format

```
Success: (:allowed t)  — or (:allowed t :validation 'semantic) for bash
Denial:  (:allowed nil :error CODE :message TEXT :resource R :tool T ...)
```

### Pipeline validator return format

```
Success: nil
Denial:  (:error CODE :message TEXT :path P :operation O ...)
```

### Violation-info format (output of build-violation-info)

```
(:tool STRING :resource STRING :reason STRING :validation-type SYMBOL :metadata PLIST)
```

Where `:reason` comes from the input `:message` field. This is the only place `:reason` exists in the system.

### build-violation-info transformation rules

| Input `:error` value | `:resource` extracted from |
|---|---|
| `"path_out_of_scope"` | `:path` |
| `"path_denied"` | `:path` |
| `"denied-pattern"` | `:resource` |
| `"not-in-scope"` | `:resource` |
| `"command_denied"` | `:command` |
| `"cloud_auth_denied"` | `:provider` |
| `"command-not-allowed"` | `:resource` or `:command` |
| other | `:resource` or `:path` (fallback) |

`:reason` always comes from `:message`. No fallback to `:error`.

## Boundaries

### In scope

- Dispatch validators in scope-core: validate-path-tool, validate-pattern-tool, validate-bash-tool
- build-violation-info in scope-core
- build-expansion-info in scope-core (if it reads `:reason`)
- Tests that construct hand-built validator plists with `:reason` field

### Out of scope

- Pipeline validators in scope-shell-tools (already use `:error` + `:message`)
- Expansion UI (already reads correct fields from violation-info)
- Contract test infrastructure in config/test/contracts/ (guards bash-parser boundary, not validator format)
- scope.yml format (unchanged)
- Tool signatures (unchanged)

## Testing Approach

### Test Framework

Buttercup — all affected tests are already Buttercup specs.

### Test Organization

Tests live in existing files:
- `config/gptel/scope/test/expansion/expansion-integration-spec.el` — validator → build-violation-info → UI integration tests (extend with unified format coverage)
- `config/gptel/scope/test-violation-info-spec.el` — unit tests for build-violation-info (update hand-built plists to new format)
- `config/gptel/scope/test/expansion/expansion-ui-handlers-spec.el` — expansion UI action tests (update `:reason` → `:error` in constructed plists)
- `config/gptel/scope/test/expansion/expansion-ui-spec.el` — expansion UI rendering tests (update constructed plists)
- `config/gptel/scope/test/core/path-validation-spec.el` — path validator tests (verify new return format)

### Naming Conventions

Follow existing `*-spec.el` convention. No new test files needed.

### Running Tests

```bash
# All scope tests (full regression)
./bin/run-tests.sh -d config/gptel/scope --report

# Expansion integration (primary test surface)
./bin/run-tests.sh -d config/gptel/scope/test/expansion

# Violation-info unit tests
./bin/run-tests.sh -d config/gptel/scope  # includes test-violation-info-spec.el
```

### Test Patterns

- **Real validator calls** for integration tests: call actual validate-path-tool, validate-operation, validate-pipeline-commands with real inputs, feed real output through build-violation-info, assert on violation-info fields
- **No mocking of validators** in integration tests — the point is to test the real format
- **Contract-validated mock helpers** (helpers-spec--make-file-op) for tests that need mock bash-parser data upstream of validation

### Scenario Mapping

| Spec scenario | Test location |
|---|---|
| All validators use :error and :message fields | expansion-integration-spec.el — "cross-validator format consistency" |
| Path denied uses :error + :message | expansion-integration-spec.el — "path_denied from real validate-operation" |
| Command denied uses :error + :message | expansion-integration-spec.el — "command_denied from real validate-pipeline-commands" |
| Violation-info reason is always human-readable | expansion-integration-spec.el — assert `:reason` is human text, not machine code |
| Expansion UI receives human-readable reason | expansion-integration-spec.el — "all real validator types produce compatible violation-info" |
| Pattern validation fails with :error + :message | expansion-integration-spec.el — "pattern validator integration" |

## Dependencies

- No new dependencies. All changes are internal to scope-core and scope-shell-tools.

## Constraints

- Dispatch validators and pipeline validators live in different files (scope-core.org vs scope-shell-tools.org). Both must be tangled after changes.
- The `gptel-make-scoped-tool` macro reads check-result format — verify it handles the new format (`:error` instead of `:reason` for error type detection).
- `request_scope_expansion` meta tool builds violation-info directly (bypasses build-violation-info) — verify it uses compatible field names.
