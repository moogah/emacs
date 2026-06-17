## ADDED Requirements

### Requirement: Working-directory path resolution and cwd–scope agreement

Relative file paths the model emits — filesystem-tool path arguments and paths inside `run_bash_command` commands — SHALL be resolved against the session buffer's `default-directory` (the work root) before scope validation, so the path the validator judges is the path the model meant. Tool invocations SHALL run in the originating session buffer's context (gptel evaluates each tool call within `with-current-buffer` of the request's buffer), so this `default-directory` is in effect for both validation and execution. The work root used for resolution SHALL be the same root used to expand the session's `GPTEL_SCOPE_*` patterns; consequently a relative path located inside the work root resolves inside the read/write scope, making the allow/deny decision meaningful rather than incidental.

This requirement constrains *which directory* relative paths resolve against; it does not change the validator's allow/deny logic, the deny-precedence rule, or the fail-closed handling of absolute paths and missing configuration, all of which remain as specified elsewhere in this capability.

**Implementation**: `config/gptel/scope/scope-validation.org` (relative-path resolution against `default-directory`), `config/gptel/scope/scope-filesystem-tools.org`, `config/gptel/scope/scope-shell-tools.org`.

#### Scenario: Relative path resolves against the work root
- **WHEN** a filesystem tool is called with the relative path `config/x.el`
- **AND** the session buffer's `default-directory` (work root) is `/Users/x/proj/`
- **AND** `paths.read` contains `/Users/x/proj/**`
- **THEN** the path resolves to `/Users/x/proj/config/x.el`
- **AND** the operation is allowed

#### Scenario: Resolution follows the work root, not the bookkeeping dir
- **WHEN** the same relative path `config/x.el` is judged in a session whose work root is `/Users/x/other/`
- **THEN** it resolves to `/Users/x/other/config/x.el`
- **AND** the decision is made against that path, not against the session's `branches/<branch>/` metadata directory

#### Scenario: Tool executes in the work-root context
- **WHEN** a scope-validated tool (filesystem or bash) runs for a session
- **THEN** it executes with `default-directory` bound to the session buffer's work root
- **AND** that is the same `default-directory` the validator used to resolve the path

#### Scenario: A relative write inside the work root is in scope by construction
- **WHEN** the work root and the session's `GPTEL_SCOPE_*` patterns derive from a single `project-root` input
- **AND** the model writes the relative path `out.txt`
- **THEN** the resolved path falls inside `paths.write`
- **AND** the write is allowed without scope expansion
