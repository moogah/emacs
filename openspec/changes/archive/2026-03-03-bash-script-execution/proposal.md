## Why

The bash parser currently tracks file read/write/delete/modify operations but cannot distinguish when a file is being **executed as code** versus read as data. This security distinction is critical: running `python malicious.py` has vastly different implications than `cat malicious.py`. We need to detect script execution to enable proper security policies around which scripts can run.

## What Changes

- Add `:execute` operation type to classify files being run as code (python, node, bash, go, ruby, etc.)
- Detect interpreter commands (python, node, bash) that execute their first positional argument
- Detect self-executing commands (`./script.sh`) where the command-name itself is the executed file
- Capture script arguments as metadata (`:script-args`) for potential future analysis
- Add semantics database entries for common interpreters and runtimes
- Create dedicated test suite for script execution detection

## Capabilities

### New Capabilities
- `script-execution`: Detect when bash commands execute script files and extract execution operations with appropriate confidence levels

### Modified Capabilities
- `bash-command-semantics`: Add `:execute` operation type and semantics entries for interpreter commands (python, node, bash, sh, go, source, etc.)
- `bash-file-operations`: Extend extraction to detect self-executing commands and capture script arguments metadata

## Impact

**Code affected:**
- `config/experiments/bash-parser/bash-parser-semantics.el` - Add interpreter command entries
- `config/experiments/bash-parser/bash-parser-file-ops.el` - Add self-execution detection logic
- `config/experiments/bash-parser/test/` - New test file for script execution scenarios

**Security implications:**
- Enables stricter policies around script execution vs. file reading
- Provides visibility into what code will actually run, not just what files are accessed
- Script arguments metadata enables future analysis of what data scripts receive

**Out of scope (deferred):**
- Multiple script files in single command (e.g., `python script1.py script2.py`)
- Complex interpreter flags (e.g., `python -m module`, `node --inspect`)
- Compilation operations (e.g., `gcc`, `go build`)
