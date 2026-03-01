# TDD Integration Summary

This document summarizes the changes made to integrate Test-Driven Development into the OpenSpec workflow.

## Overview

The OpenSpec workflow now supports TDD with two new artifacts:
1. **architecture.md** - Defines components, interfaces, testability, and testing approach
2. **tests/** - Actual test files that implement test cases for spec scenarios

## New Workflow

### spec-driven-tdd Schema

```
proposal.md
  ↓
specs/*.md (scenarios and requirements)
  ↓
architecture.md (components, interfaces, boundaries, testing approach)
  ↓
tests/ (actual test files - TDD red phase)
  ↓
design.md (implementation approach to make tests pass)
  ↓
create-beads (implementation beads with test coverage info)
  ↓
apply (implement - TDD green phase)
```

### Legacy spec-driven Schema

The original workflow (proposal → specs → design) continues to work for non-TDD changes.

## Key Concepts

### architecture.md

Created through **user dialog** to determine:
- Test framework (e.g., ERT for Emacs, Jest for JS, pytest for Python)
- Test file location (e.g., `test/`, `tests/`, `spec/`)
- Naming conventions (files and test functions)
- How to run tests (commands)
- Mock/stub patterns
- Test helpers location

### tests/

- **Actual test files**, not documentation
- Location determined by architecture.md
- Maps spec scenarios to test cases via:
  - Function names referencing scenarios
  - Comments referencing spec sections
  - No separate mapping document needed
- Tests may fail initially (TDD red phase - expected)
- Use `:expected-result :failed` or `ert-skip` for tests requiring future implementation

Example:
```elisp
(ert-deftest gptel-session-export-basic ()
  "Export session to markdown file.
   Scenario: specs/gptel/sessions.md § 'Export to file'"
  ;; Test implementation...
  :expected-result :failed)  ;; TDD: red phase
```

## Updated Commands

### /opsx:continue

- Added guidance for creating **architecture.md**:
  - Engages user in dialog about testing approach
  - Uses AskUserQuestion to gather preferences
  - Documents all decisions in architecture.md

- Added guidance for creating **tests/**:
  - Creates actual test files, not documentation
  - Uses architecture.md decisions for location/framework
  - Maps scenarios to tests via naming and comments
  - Tests may fail initially (TDD red phase)

### /opsx:ff

- Updated to handle architecture and tests artifacts
- Engages user in testing dialog when creating architecture.md
- Creates test files based on architecture decisions

### /opsx:apply

- Now runs tests during implementation (if architecture.md exists)
- Checks architecture.md for test command
- Runs tests related to bead changes
- Creates discovery beads for missing tests if new scenarios found

### /opsx:verify

- Added **Test Coverage** verification:
  - Checks if each spec scenario has corresponding test
  - Runs tests using command from architecture.md
  - Reports failing/skipped tests as CRITICAL/WARNING
  - Checks for orphaned tests (tests without spec scenarios)

- Added **Test Execution** verification:
  - Attempts to run tests
  - Reports pass/fail counts
  - Failing tests generate CRITICAL issues

### /opsx:create-beads

- Now reads architecture.md and tests/ when available
- Updated bead description template to include:
  - **Test coverage** section listing related tests and their status
  - Test execution in implementation steps
  - Test-based verification criteria

- Updated guardrails:
  - **Test-aware**: Include which tests each bead makes pass
  - **TDD-compatible**: Beads can be "write test + implement" or just "implement"

## Benefits

1. **True TDD workflow**: Red (tests exist) → Green (implement) → Refactor
2. **Project-specific**: Test location/framework determined by user dialog
3. **No extra documentation**: Tests are actual runnable code
4. **Implicit mapping**: Through naming and comments, not separate docs
5. **Flexible**: Works for both TDD and non-TDD changes
6. **Discovery-friendly**: Accumulate discovery beads for missing tests

## Discovery Workflow

During implementation, if you discover:
- Missing tests for edge cases
- Architecture inefficiencies
- Design issues

Create discovery beads with `--deps "discovered-from:<current-bead-id>"`:
- Types: bugs, questions, related work, blockers, **missing tests**
- Don't expand scope of current bead
- User reviews discoveries after completing expected work

## Backward Compatibility

- Legacy **spec-driven** schema continues to work (without architecture/tests)
- Projects can choose TDD or non-TDD workflow
- Commands gracefully handle missing architecture.md or tests/
- Verification adapts based on available artifacts

## Examples

### TDD Change Example

```bash
/opsx:new gptel-session-export
# Creates proposal

/opsx:continue
# Creates specs with scenarios

/opsx:continue
# User dialog about testing:
# - Framework: ERT
# - Location: test/gptel/
# - Naming: test-<module>-<aspect>.el
# Creates architecture.md

/opsx:continue
# Creates test/gptel/test-sessions.el with failing tests

/opsx:continue
# Creates design.md explaining how to make tests pass

/opsx:create-beads
# Creates implementation beads with test coverage info

/opsx:apply
# Implements to make tests pass (TDD green phase)

/opsx:verify
# Verifies all tests pass and scenarios covered

/opsx:archive
# Archives completed change
```

### Non-TDD Change Example

```bash
/opsx:new simple-fix
# Uses spec-driven schema (no architecture/tests)

/opsx:continue  # proposal
/opsx:continue  # specs
/opsx:continue  # design (directly)
/opsx:create-beads
/opsx:apply
/opsx:archive
```

## Future Enhancements

Potential future additions:
- Test coverage metrics
- Automatic test generation from specs
- Test execution in CI/CD integration
- Test isolation and mocking utilities
- Performance test support
