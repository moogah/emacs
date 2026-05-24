## ADDED Requirements

### Requirement: Scope subsystem never reads or writes scope.yml

The scope loader and the scope writers SHALL never open, read, or write a `scope.yml` file in the session directory or elsewhere on disk. The post-cycle-3 contract that "drawer is the source of truth" SHALL be enforceable by negative invariant: the on-disk presence or absence of a `scope.yml` artifact has no effect on scope authorization.

This requirement codifies a contract the production code already honors (the `scope-yaml.el` module was deleted in `delete-yaml-and-security-residue`, the writer routes through `jf/gptel-scope--write-pattern-to-drawer`, the loader routes through `jf/gptel-scope--load-from-buffer` / `--load-from-file`). The integration test corpus established by `migrate-integration-tests-to-drawer-helpers` is the regression guard: no integration test creates a `scope.yml` file, mocks one, or asserts on one. A future code change that reintroduces YAML I/O would be visible as a new file write in test setup or a new file path in the loader; both would surface in code review.

#### Scenario: Loader does not open scope.yml

- **WHEN** any scope-aware tool invocation routes through `jf/gptel-scope--load-config` in any buffer
- **THEN** the loader reads only the session's `:PROPERTIES:` drawer (via `org-entry-get-multivalued-property` against the chat buffer, or via `with-temp-buffer` + `insert-file-contents` of `session.org` for the file-fallback path)
- **AND** no `scope.yml` file is opened on disk under any circumstance

#### Scenario: Writers do not create scope.yml

- **WHEN** any expansion action handler (`--add-to-scope`, `--add-path-to-scope`, `--add-bash-to-scope`) routes through `jf/gptel-scope--write-pattern-to-drawer`
- **THEN** the new pattern lands in the buffer's `:PROPERTIES:` drawer via `org-entry-put-multivalued-property`
- **AND** no `scope.yml` file is created or modified on disk under any circumstance
