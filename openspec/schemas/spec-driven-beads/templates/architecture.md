## Components

<!-- List major building blocks and their responsibilities -->

## Interfaces

<!-- Define how components interact - APIs, data flows, contracts -->

## Boundaries

<!-- Define system boundaries - what's in/out of scope, internal/external -->

## Testing Approach

### Test Framework
<!--
Buttercup (preferred) - BDD framework with describe/it/expect syntax, built-in setup/teardown, and spy system.
Use for: New test suites, tests requiring fixtures/mocks, behavioral tests.

ERT (legacy) - Built-in Emacs testing framework with should/should-not assertions.
Use for: Maintaining existing tests, simple unit tests.

For other languages: Jest (JavaScript), pytest (Python), etc.
-->

### Test Organization
<!--
Preferred: config/MODULE/test/ directory with *-spec.el files for Buttercup
Legacy: config/MODULE/*-test.el co-located files for ERT
Other: test/, tests/, __tests__/ depending on ecosystem conventions
-->

### Naming Conventions
<!--
Buttercup: *-spec.el files with (describe "module" (it "behavior" ...))
ERT: *-test.el files with (ert-deftest test-module-behavior ...)
Other: Follow ecosystem conventions (*.test.js, test_*.py, etc.)
-->

### Running Tests
<!--
Buttercup:
  make test-buttercup                      # All Buttercup tests
  make test-buttercup-directory DIR=...    # Specific directory
  ./bin/run-tests.sh -f buttercup          # Via CLI

ERT:
  make test                                # All ERT tests
  make test-directory DIR=...              # Specific directory
  ./bin/run-tests.sh -f ert                # Via CLI

Interactive: C-c t (transient menu with both ERT and Buttercup options)
-->

### Test Patterns
<!--
Buttercup:
  - Setup/teardown: before-each, after-each, before-all, after-all
  - Mocking: (spy-on 'function :and-return-value value)
  - Assertions: (expect result :to-equal expected)
  - Nested contexts: (describe "outer" (describe "inner" ...))

ERT:
  - Setup/teardown: Manual with let bindings or helper functions
  - Assertions: (should (equal result expected))
  - No built-in mocking (use manual stubs)
-->

### Scenario Mapping
<!--
Map spec scenarios to test cases:
  - Scenario: "User does X" → (it "does X when..." ...)
  - Given/When/Then → before-each setup, action in test, expect assertions
  - Each scenario becomes at least one test case
  - Edge cases and error conditions get additional test cases
-->

## Dependencies

<!-- External libraries, services, or systems -->

## Constraints

<!-- Technical limitations, performance requirements, compatibility -->
