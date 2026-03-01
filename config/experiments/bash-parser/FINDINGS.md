# Bash Parser Exploration Findings

## Summary

Tested **51 commands** (11 baseline, 32 exploratory, 8 variable expansion) with **100% parse success rate**. Tree-sitter bash grammar is robust and never crashes, but has specific behaviors for complex constructs.

**Update (2026-03-01)**:
- Implemented full pipeline and command chain parsing. All commands in pipelines (|) and chains (&&, ||, ;) are now extracted and validated individually.
- Implemented full redirection parsing. All redirection operators (>, >>, <, 2>&1, etc.) with descriptors and targets are now extracted.
- **FIXED Variable Expansion**: All variable expansion scenarios now work correctly. Added support for `simple_expansion` ($VAR) and `expansion` (${VAR}) node types, plus proper handling of concatenations to prevent duplication.
- **FIXED ANSI-C Quoting**: Added support for `ansi_c_string` node type. Commands like `echo $'text\n'` now parse correctly with escape sequences preserved.
- **CLARIFIED Escaped Quotes**: The command `echo 'it\'s working'` is **invalid bash syntax** - you cannot escape single quotes within single-quoted strings. This is not a parser bug but correct behavior. Use ANSI-C quoting (`$'it\'s'`) or double quotes instead.

## Critical Findings

### ✅ What Works Perfectly

1. **Simple commands**: `ls -la /tmp` ✓
2. **Subcommands**: `git log --oneline` ✓
3. **Quoted strings**: `echo "hello 'world'"` ✓
4. **Dangerous pattern detection**: `rm -rf`, `git push --force`, `python -c` ✓
5. **Glob patterns**: `ls *.txt` (preserved as literal strings) ✓
6. **Variables**: `echo $PATH`, `rm -rf $HOME/tmp`, `echo ${HOME}` - all forms correctly extracted ✓
7. **Variable concatenations**: `echo $VAR/suffix`, `ls $HOME/*.txt` - preserved as single units ✓
8. **Pipelines**: `ls -la | grep test` - all commands fully parsed ✓
9. **Command chains**: `git add . && git commit -m 'test'` - all commands fully parsed ✓
10. **Redirections**: `echo 'hello' > output.txt` - operators, descriptors, targets extracted ✓
11. **Multiple redirections**: `git log > /dev/null 2>&1` - all redirects parsed ✓
12. **ANSI-C quoting**: `echo $'line1\nline2'` - escape sequences preserved with $'' wrapper ✓


### ❌ Broken/Unexpected Behavior

#### **Escaped Single Quotes Within Single Quotes** - Invalid Bash Syntax
```bash
echo 'it\'s working'
# Tree-sitter correctly reports ERROR nodes
# This is INVALID bash syntax - not a parser bug
```

**Explanation**: In bash, you cannot escape a single quote within single-quoted strings. Use one of these alternatives:
- ANSI-C quoting: `echo $'it\'s working'` ✓
- Double quotes: `echo "it's working"` ✓
- Concatenation: `echo 'it'\''s working'` ✓

**Impact**: Parser correctly rejects invalid syntax. No fix needed.

#### **Brace Expansion** - Splits Incorrectly
```bash
git add *.{el,org}
# Sees: args=["*.{el,org}", "*.", "{", "el,org", "}"]
# BUG: Splits into 5 tokens instead of treating as single pattern
```

**Impact**: Cannot handle brace expansion in glob patterns

#### **Find -exec** - Misclassified
```bash
find . -name '*.log' -exec rm {} \;
# Sees: flags=["-name", "-exec"],
#       args=[".", "*.log", "rm", "{}", "{", "}", "\\;"]
# BUG: Treats "-exec" as a flag instead of recognizing exec block
```

**Impact**: Cannot properly parse find -exec constructs

## Parser Capabilities Summary

| Feature | Support | Notes |
|---------|---------|-------|
| Simple commands | ✅ Perfect | Core functionality works great |
| Flags | ✅ Perfect | Both short and long flags detected |
| Positional args | ✅ Perfect | Non-flag arguments extracted |
| Subcommands | ✅ Perfect | Git, docker, npm, etc. |
| Quoted strings | ✅ Perfect | Double quotes, single quotes, and ANSI-C quotes work |
| Dangerous patterns | ✅ Perfect | Flag and subcommand matching works |
| Pipelines | ✅ Perfect | All commands extracted and validated |
| Command chains | ✅ Perfect | All commands extracted and validated |
| Redirections | ✅ Perfect | Operators, descriptors, and targets extracted |
| Variables | ✅ Perfect | $VAR, ${VAR}, concatenations all work correctly |
| ANSI-C quoting | ✅ Perfect | $'string\n' with escape sequences fully supported |
| Command substitution | ⚠️ Partial | Text extracted but not parsed recursively |
| Escaped quotes in single quotes | 🚫 N/A | Invalid bash syntax (correctly rejected) |
| Brace expansion | ❌ Broken | Splits into multiple tokens |
| Find -exec | ❌ Broken | Misclassifies structure |
| Background `&` | 🚫 Ignored | Stripped but doesn't break |
| Heredocs | 🚫 Ignored | Stripped but doesn't break |

## Recommended Use Cases

### ✅ Good For:

1. **Single-command validation** - Works great for simple commands
2. **Multi-command validation** - Pipelines and chains fully parsed
3. **Dangerous flag detection** - Reliable for rm -rf, git --force, etc. across all commands
4. **Variable handling** - Variables preserved for validation (e.g., `$HOME`, `${VAR}`)
5. **Basic argument extraction** - Good for simple tools
6. **Subcommand routing** - Perfect for git/docker/npm workflows
7. **File redirection analysis** - Full extraction of operators, descriptors, and targets

### ⚠️ Use With Caution:

1. **Complex quoting** - May misbehave with escaped quotes
2. **Command substitution** - Nested commands not recursively parsed

### ❌ Don't Use For:

1. **Script parsing** - Too many unsupported constructs
2. **Complex find commands** - Structure not recognized

## Security Implications

### 🔒 Safe:
- Detecting dangerous flags in simple commands
- Validating git/docker/npm operations
- Detecting dangerous commands in pipelines and chains
- Extracting redirection targets for file validation
- Variable extraction - variables preserved for validation (e.g., detect `rm -rf $HOME`)

### ⚠️ Unsafe:
- **Command substitution**: `rm -rf $(dangerous)` → nested command not validated (text extracted but not recursively parsed)

### 🎯 New Security Capabilities:
With redirection support, you can now:
- Detect suspicious file targets (`/etc/passwd`, `/dev/sda`, etc.)
- Warn on destructive redirections (`> /important/config`)
- Validate output destinations before execution

## Recommendations

### For Command Validation Use Case:

1. **Use pipeline/chain parsing**: Parser extracts all commands from pipelines (|) and chains (&&, ||, ;)
2. **Use redirection parsing**: Parser extracts all redirection operators, descriptors, and file targets
3. **Use variable extraction**: Variables are preserved as-is (e.g., `$PATH`, `${HOME}`) for validation or expansion
4. **Use ANSI-C quoting extraction**: Escape sequences in `$'...'` strings are preserved for analysis
5. **Pattern matching**: Use dangerous pattern database across all commands in multi-command strings
6. **Pre-flight check for unsupported constructs**: Consider rejecting:
   - Command substitution (`$()`, backticks) if recursive validation needed

### For Future Enhancement:

If you need full command parsing, consider:

1. **Recursive command substitution**: Parse nested commands in $() and backticks
2. **Advanced redirect validation**: Add security rules for dangerous file targets
3. **Brace expansion**: Handle `{a,b,c}` patterns correctly
