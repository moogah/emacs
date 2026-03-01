# Pipeline Parsing Issue - Executive Summary

## The Problem

**Symptom:** Parser only sees first command in pipelines
```bash
ls -la | grep test
# Sees: ls -la
# Misses: grep test
```

## Root Cause

### 1. Tree-sitter Creates Correct AST

For `ls -la | grep test`, tree-sitter produces:
```
program
  └── pipeline
      ├── command [ls -la]
      ├── | operator
      └── command [grep test]
```

Both commands are in the tree!

### 2. Parser Only Extracts First Command

The function `jf/bash-parse--find-first-command` (bash-parser.el:70-89):
- Searches for first "command" node
- Returns immediately when found
- Never continues to siblings

```elisp
((string= node-type "command")
 node)  ; ← Returns here, never checks remaining children!
```

### 3. Security Hole

```bash
echo "safe" | rm -rf /tmp/dangerous
# Parser says: safe (only sees "echo")
# Actually runs: DANGEROUS (rm -rf executes!)
```

## Solution

### Recommended: Reject Multi-Command Constructs

**Why:** Matches intended use case (gptel command validation)

**How:** Pre-flight check before parsing

```elisp
(when (string-match-p "\\(|\\|&&\\|||\\|;\\)" command-string)
  (list :success nil
        :error "Multi-command constructs not supported"))
```

**Benefits:**
- Simple (5 lines of code)
- Secure (prevents evasion)
- Clear about limitations
- Matches FINDINGS.md recommendation

### Alternative: Full Pipeline Parsing

**Why:** Complete solution for complex use cases

**How:** Detect pipeline nodes, extract all commands

```elisp
(if (pipeline-node-p root)
    (parse-all-pipeline-stages root)
  (parse-single-command root))
```

**Trade-offs:**
- More complex (~100 lines)
- Breaking API change
- Handles all cases correctly

## Files Created

1. **PIPELINE-ANALYSIS.md** - Detailed root cause analysis
2. **pipeline-fix-poc.el** - Proof-of-concept implementations
3. **debug-pipelines.el** - AST inspection tool
4. **PIPELINE-SUMMARY.md** - This file

## Next Steps

1. **Review** PIPELINE-ANALYSIS.md for complete details
2. **Choose** solution approach (recommend: reject multi-command)
3. **Implement** fix in bash-parser.org
4. **Test** with security-focused test cases
5. **Update** FINDINGS.md with resolution

## Key References

- **Root cause:** bash-parser.el:70-89 (`jf/bash-parse--find-first-command`)
- **Grammar:** https://github.com/tree-sitter/tree-sitter-bash/blob/master/grammar.js
- **Node types:** https://github.com/tree-sitter/tree-sitter-bash/blob/master/src/node-types.json
- **Security note:** FINDINGS.md:186-188 (pipeline evasion)
