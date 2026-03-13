## Components

### scope-expansion.org — Transient UI Layer

The only file that changes. Contains all transient suffix command definitions and the menu layout.

- **Existing suffixes**: deny, add-to-scope (exact), allow-once, edit-scope
- **New suffixes to add**:
  - `jf/gptel-scope--add-wildcard-to-scope` — derives wildcard pattern from resource and calls existing path/bash/pattern updaters
  - `jf/gptel-scope--add-custom-to-scope` — prompts user via `read-string` with pre-populated resource value, then calls existing updaters with edited string
- **Menu layout**: Expand from 3 primary choices to 5; group new options near "Add exact to scope" (they are variants of the same action)

### Pattern Derivation Helper

A pure function `jf/gptel-scope--derive-wildcard-pattern (resource)` that:
- Strips trailing slash from resource
- If resource ends in a filename (has extension or no trailing `/`), returns `(file-name-directory resource)/**`
- If resource is already a directory path, returns `resource/**`

This is the only new logic; the resulting pattern feeds into existing validation-type routing unchanged.

## Interfaces

### Existing updater contract (unchanged)

All new suffixes call the same routing layer as the exact add-to-scope suffix:

```elisp
;; Routing pcase (already exists in scope-expansion.org)
(pcase validation-type
  ("path"    (jf/gptel-scope--add-path-to-scope pattern context-dir))
  ("pattern" (jf/gptel-scope--add-pattern-to-scope pattern context-dir))
  ("bash"    (jf/gptel-scope--add-bash-to-scope pattern context-dir)))
```

New suffixes compute `pattern` differently (wildcard or user-edited) but pass it through identically. No changes to updater functions.

### Transient scope data (unchanged)

Suffixes read `(transient-scope)` to get `(:tool :resource :reason :validation-type :callback :patterns)` — no new keys needed. The `:resource` key is the pre-seed value for custom prompt and the input to wildcard derivation.

### Callback protocol (unchanged)

Both new suffixes invoke callback with the same shape as exact add-to-scope:
- Success: `(:success t :patterns_added [pattern])`
- Cancelled custom prompt: `(:success nil :user_denied t)`

## Boundaries

**In scope:**
- `config/gptel/scope/scope-expansion.org` — suffix definitions and menu layout
- `config/gptel/scope/test/expansion/expansion-ui-spec.el` — new test cases for both new suffixes

**Out of scope:**
- Scope updater functions (no changes)
- Scope core / allow-once logic (no changes)
- Shell tools validation pipeline (no changes)
- Any other gptel subsystem

## Testing Approach

### Test Framework

**Buttercup** — established framework for all gptel scope tests. New tests follow existing `expansion-ui-spec.el` patterns.

### Test Organization

New tests go into the existing file:
```
config/gptel/scope/test/expansion/expansion-ui-spec.el
```

Two new `describe` blocks appended to the file:
- `"Transient action handlers — wildcard add-to-scope"`
- `"Transient action handlers — custom pattern add-to-scope"`

The wildcard derivation helper also gets a small focused `describe` block (pure function, easy to unit test).

### Naming Conventions

- File: `expansion-ui-spec.el` (existing — no new file needed)
- Describe blocks: `"Transient action handlers — <variant>"`
- Test functions (it): mirror spec scenario names, e.g. `"derives parent directory wildcard from file path"`

### Running Tests

```bash
# Expansion tests only
./bin/run-tests.sh -d config/gptel/scope/test/expansion

# All scope tests
./bin/run-tests.sh -d config/gptel/scope

# Via make
make test-buttercup-directory DIR=config/gptel/scope/test/expansion
```

### Test Patterns

Existing tests mock transient scope and stub updater functions via `cl-letf`. New tests follow the same pattern:

```elisp
(describe "Transient action handlers — wildcard add-to-scope"
  (before-each
    (setq test-scope (list :tool "run_bash_command"
                           :resource "/home/user/project/file.txt"
                           :validation-type "path"
                           :callback callback-fn
                           :patterns '("/home/user/project/file.txt"))))

  (it "derives parent directory wildcard from file path"
    (cl-letf (((symbol-function 'transient-scope) (lambda () test-scope))
              ((symbol-function 'jf/gptel-scope--add-path-to-scope) mock-updater))
      (jf/gptel-scope--add-wildcard-to-scope)
      (expect mock-updater :to-have-been-called-with "/home/user/project/**" ...))))
```

For the custom pattern suffix, `read-string` is stubbed to return a controlled value (or signal `quit` to simulate C-g cancellation).

### Scenario Mapping

| Spec Scenario | Test location | Test description |
|---|---|---|
| Wildcard option only appears for file resources | `expansion-ui-spec.el` | "wildcard option available when resource is a file" |
| Wildcard option not shown for directory resources | `expansion-ui-spec.el` | "wildcard option not available when resource is a directory" |
| Wildcard option derives parent directory from denied file | `expansion-ui-spec.el` | "derives parent directory wildcard from file path" |
| Wildcard route follows existing validation-type routing | `expansion-ui-spec.el` | "routes through validation-type for wildcard pattern" |
| Custom option prompts with pre-populated value | `expansion-ui-spec.el` | "pre-populates read-string prompt with denied resource" |
| Accepted custom pattern written to scope.yml | `expansion-ui-spec.el` | "writes edited custom pattern to scope" |
| Cancelled custom prompt denies | `expansion-ui-spec.el` | "invokes callback with user_denied when prompt cancelled" |

## Dependencies

- `transient` package — existing dependency, no new capabilities required
- `jf/gptel-scope--add-path-to-scope`, `jf/gptel-scope--add-pattern-to-scope`, `jf/gptel-scope--add-bash-to-scope` — existing functions, called unchanged

## Constraints

- No changes to existing suffix commands or their keybindings — new options extend, not replace
- `read-string` (not a custom transient infix) for custom pattern prompt — simpler and avoids transient state complexity
- Wildcard derivation must handle both `path` and `bash` validation types (bash resources that are directory paths already route through the path updater)
- Menu layout must remain navigable; group new variants under a clear label to avoid confusion with the existing exact "Add to scope"
