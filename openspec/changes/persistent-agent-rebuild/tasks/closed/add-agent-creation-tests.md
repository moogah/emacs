---
name: add-agent-creation-tests
description: Buttercup specs for agent directory + drawer + scope.yml creation, including validation errors
change: persistent-agent-rebuild
status: needs-review
relations:
  - blocked-by:fix-agent-flat-layout
---

## Files to modify

- `config/gptel/tools/test/persistent-agent/creation-spec.el` (new)

## Implementation steps

1. **File header**:
   ```elisp
   ;;; creation-spec.el --- Persistent-agent creation-flow tests -*- lexical-binding: t; -*-

   (require 'buttercup)
   (require 'jf-persistent-agent-test-helpers)
   (require 'gptel-persistent-agent)
   ```

2. **Required `it` cases** (each with the leading scenario-mapping comment):

   - `it "creates the agent directory under the parent branch"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Agent session creation" → "Agent directory created under parent branch"
     - Setup with `with-mock-parent-session` + `with-mock-preset 'test-preset` + `with-mock-gptel-request 'captured`.
     - Call `(jf/gptel-persistent-agent--task #'ignore "test-preset" "analyze code" "do the thing")`.
     - Expect a directory exists matching `<branch-dir>/agents/test-preset-<14-digit-timestamp>-analyze-code/`.
     - Expect that directory contains no `branches/` subdirectory and no `current` symlink.

   - `it "writes session.org with a self-describing :PROPERTIES: drawer"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Agent session creation" → "session.org carries a self-describing :PROPERTIES: drawer"
     - Same setup. Read the resulting `session.org` from disk.
     - Expect content matches the expected layout: `:PROPERTIES:` drawer with `:GPTEL_PRESET: test-preset` and `:GPTEL_PARENT_SESSION_ID: <mock-session-id>`, followed by `#+begin_user\nDO THE THING\n#+end_user\n` (the prompt embedded in the user block).
     - Use a regex assertion or full-string equality against the expected layout.

   - `it "writes scope.yml with allowed paths"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Agent session creation" → "scope.yml written via scope-module helper"
     - Same setup but pass `["/path/to/project/**" "/another/**"]` as `allowed_paths`.
     - Read `<agent-dir>/scope.yml` from disk.
     - Expect `paths.read` contains exactly those two patterns.
     - Expect `paths.write` is `["/tmp/**"]`.
     - Expect `paths.deny` includes `**/.git/**`, `**/runtime/**`, `**/.env`, `**/node_modules/**`.

   - `it "writes scope.yml with empty read paths when allowed-paths is omitted"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Tool invocation and validation" → "Explicit path configuration (zero inheritance)"
     - Same setup, no `allowed_paths` argument.
     - Expect scope.yml contains `read:\n    []` literally.
     - Expect scope.yml does NOT contain any pattern from the parent's scope (the mock-parent-session fixture writes no parent scope.yml; this test asserts that absence of parent paths flows through).

   - `it "rejects an unknown preset before any directory is created"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Error handling" → "Unknown preset rejected before any side effect"
     - With `with-mock-parent-session` (no `with-mock-preset`), call the task with preset name `"nonexistent"`.
     - Expect a `user-error` signal containing `"Preset 'nonexistent' not found"`.
     - Expect `<branch-dir>/agents/` is empty (no directories created before the error).

   - `it "rejects invocation outside a parent session"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Tool invocation and validation" → "Parent session requirement"
     - Without `with-mock-parent-session` (i.e., `jf/gptel--session-dir` unbound or nil).
     - Call the task. Expect `user-error` matching `"PersistentAgent requires parent persistent session"`.
     - Expect no temp dirs remain (this is implicit since no dir creation happens — sanity check via `(file-directory-p any-expected-path)` returning nil).

   - `it "tool registration drops denied_paths from the args"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Tool invocation and validation" → "Tool argument schema"
     - `(let ((tool (gptel-get-tool "PersistentAgent")))`
     - Expect `(gptel-tool-args tool)` contains entries with `:name` values `"preset"`, `"description"`, `"prompt"`, `"allowed_paths"` (in any order or that specific order).
     - Expect NO entry with `:name "denied_paths"`.

3. **Smoke-test the file**: load and run via Buttercup batch mode.

## Design rationale

Creation tests are real-file-IO: the agent module touches filesystem (mkdir, write files). Mocking that out would just test "we called X with Y" — but the actual contract is the on-disk shape, so we exercise it for real against a temp directory cleaned up in `after-each`.

Each `it` maps to a single spec scenario via comment so that drift between spec and implementation surfaces here first. The full-file-content assertion in "writes session.org with a self-describing :PROPERTIES: drawer" is the drift-catching guard for Decision 4 (initial-content shape duplication between `jf/gptel-persistent-agent--initial-content` and `jf/gptel--initial-session-content`).

The "rejects unknown preset before any side effect" test is the most important error-handling assertion: the agent must NEVER leave half-created directories on disk. The order of validation in `--task` (preset check before any I/O) is what makes this work.

## Design pattern

Buttercup `before-each` / `after-each` for the temp-dir lifecycle (the fixture macros handle `unwind-protect`, but tests that need to inspect post-cleanup state can capture paths into `let`-bound vars before exiting the macro).

For file-content assertions against drawer + user-block layout, use full-string equality for a small, fixed format string. For larger files, use `should-match` style with regex.

## Verification

- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` includes the new file and all `it` blocks pass.
- Each `it` block has a leading comment naming its spec scenario.
- `make test-buttercup-directory DIR=config/gptel/tools/test/persistent-agent` succeeds.
- After all tests, `/tmp/pa-test-*` (or wherever `make-temp-file 'directory` puts them) is empty — confirms cleanup works.

**Done means**: 7 `it` blocks, all green, creation-flow contract pinned to disk.

## Context

specs/persistent-agent/spec.md (delta) § "Tool invocation and validation", "Agent session creation", "Error handling"
design.md § "Decisions" 1, 4
architecture.md § "Testing Approach" → "Scenario Mapping"
