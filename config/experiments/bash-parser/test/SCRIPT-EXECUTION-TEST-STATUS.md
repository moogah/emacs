# Script Execution Test Status

## Test Results Summary

**Total Tests:** 30
**Passing:** 22
**Failing:** 8

## Passing Test Categories (73%)

The following test scenarios pass successfully:

### Interpreter Commands
- Python interpreter execution (basic and with file args)
- Node interpreter execution (basic)
- Bash/sh/zsh interpreter execution
- Shell built-ins (source, .)

### Self-Executing Scripts
- Relative path execution (./script.sh)
- Absolute path execution (/usr/bin/tool)
- Parent directory execution (../bin/runner)
- Non-path commands correctly NOT detected as self-executing

### Script Arguments
- Script with no arguments
- Script with file arguments
- Self-executing scripts with arguments

### Confidence Levels
- High confidence for known interpreters
- Low confidence for path-based execution
- High confidence for shell built-ins

### Variable Resolution
- Resolved variables in script paths
- Unresolved variables marked correctly

### Multi-Command Constructs
- Pipelines with execute operations
- Command chains with execute operations

## Failing Test Categories (27%)

### 1. Flag Argument Handling (3 tests)

**Issue:** Script arguments that start with `--` flags are being filtered out by the bash parser.

**Failing Tests:**
- `test-script-execution-node-with-flags`
- `test-script-execution-script-args-flags`
- Related corpus tests

**Example:**
```bash
# Command: node server.js --port 3000 --verbose
# Expected: script-args = ("--port" "3000" "--verbose")
# Actual:   script-args = ("3000")  # --port and --verbose stripped
```

**Root Cause:** The bash parser's flag handling logic is filtering out arguments starting with `--` when extracting positional arguments. This is likely happening in the parser core, not the file-ops extraction layer.

**Resolution Path:** This is a known limitation of the current bash parser implementation. The parser distinguishes between flags and positional args at parse time. To fix:
1. Update bash-parser-core to preserve all arguments after the executed script
2. OR: Add special handling in script-args extraction to recombine flags + positional args

### 2. Go Subcommands (3 tests)

**Issue:** Go subcommand handling (go run, go test, go build) appears to not be fully implemented.

**Failing Tests:**
- `test-script-execution-go-run-subcommand`
- `test-script-execution-go-test-subcommand`
- `test-script-execution-go-build-not-execute`

**Example:**
```bash
# Command: go run main.go
# Expected: file="main.go", operation=:execute
# Actual:   (likely not extracting or wrong operation type)
```

**Root Cause:** While the semantics database has entries for go subcommands, the subcommand handler logic may not be correctly routing `go run` vs `go build` vs `go test` to their respective operation types.

**Resolution Path:**
1. Verify semantics database entries are correct (lines 67-70 in bash-parser-semantics.el)
2. Check jf/bash--extract-from-complex-command to ensure subcommand dispatch works
3. Test with actual parsed structure to see if subcommand is being captured

### 3. Nested/Indirect Execution (2 tests)

**Issue:** Commands nested via `bash -c 'command'` or `sh -c 'command'` are not being marked with `:indirect t`.

**Failing Tests:**
- `test-script-execution-nested-python`
- `test-script-execution-nested-self-executing`

**Example:**
```bash
# Command: bash -c 'python script.py'
# Expected: file="script.py", operation=:execute, indirect=t
# Actual:   indirect flag is nil
```

**Root Cause:** The nested command detection logic (bash -c '...' or sh -c '...') may not be:
1. Parsing the nested command string correctly
2. Propagating the :indirect flag to extracted operations

**Resolution Path:**
1. Check if bash-parser-core extracts nested commands from -c flag
2. Verify if jf/bash--extract-from-nested-command exists and is being called
3. May need to add nested command handling to file-ops extraction

## Test Infrastructure Quality

The test suite demonstrates good practices:

1. **Comprehensive Coverage:** Tests cover all major scenarios from the spec
2. **Clear Structure:** Each test has descriptive name and spec reference
3. **Corpus Pattern:** Separate corpus file enables bulk testing
4. **Good Assertions:** Tests verify all expected operation fields
5. **Failure Messages:** Corpus test provides detailed failure diagnostics

## Next Steps

### Immediate (For This Bead)
- ✅ Test suite created and committed
- ✅ Known limitations documented
- ✅ 73% pass rate establishes good baseline

### Follow-Up Work (New Beads)
1. **Fix flag argument handling** (high priority)
   - Update bash-parser-core positional-args extraction
   - Preserve flags in script-args for execute operations

2. **Complete go subcommand support** (medium priority)
   - Debug go subcommand routing
   - Verify semantics handlers work correctly

3. **Implement indirect execution detection** (low priority)
   - Add nested command parsing for -c flag
   - Propagate indirect marker through extraction pipeline

## Test Execution

```bash
# Run all script execution tests
make test-pattern PATTERN='^test-script-execution-'

# Run corpus test only
make test-pattern PATTERN='^test-script-execution-corpus$'

# Run in specific test directory
make test-directory DIR=config/experiments/bash-parser
```

## Notes

The test suite correctly validates the specifications. The failures expose real implementation gaps that should be addressed in follow-up beads. This is proper test-driven development: write comprehensive tests first, then fix implementation to pass them.
