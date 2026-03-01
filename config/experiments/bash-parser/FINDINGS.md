# Bash Parser Exploration Findings

## Summary

Tested **43 commands** (11 baseline, 32 exploratory) with **100% parse success rate**. Tree-sitter bash grammar is robust and never crashes, but has specific behaviors for complex constructs.

**Update (2026-03-01)**: Implemented full pipeline and command chain parsing. All commands in pipelines (|) and chains (&&, ||, ;) are now extracted and validated individually.

## Critical Findings

### ✅ What Works Perfectly

1. **Simple commands**: `ls -la /tmp` ✓
2. **Subcommands**: `git log --oneline` ✓
3. **Quoted strings**: `echo "hello 'world'"` ✓
4. **Dangerous pattern detection**: `rm -rf`, `git push --force`, `python -c` ✓
5. **Glob patterns**: `ls *.txt` (preserved as literal strings) ✓
6. **Variables in quotes**: `git commit -m "$message"` ✓
7. **Pipelines**: `ls -la | grep test` - all commands fully parsed ✓
8. **Command chains**: `git add . && git commit -m 'test'` - all commands fully parsed ✓

### ⚠️ Known Limitations

#### **Redirections** - Operators and Targets Stripped
```bash
echo 'hello' > output.txt
# Sees: command="echo", args=["hello"]
# Missing: "> output.txt" ignored
```

**Impact**: Cannot extract file targets from redirections (not a security issue for single-command validation)

### ❌ Broken/Unexpected Behavior

#### **Variable Expansion** - Inconsistent
```bash
echo $PATH
# Sees: command="echo", args=[]
# BUG: $PATH completely disappears!
```

```bash
rm -rf $HOME/tmp
# Sees: command="rm", flags=["-rf"], args=["$HOME/tmp", "/tmp"]
# BUG: Extracts BOTH "$HOME/tmp" AND "/tmp" as separate args
```

**Impact**: Unpredictable - variables sometimes disappear, sometimes duplicated

#### **Escaped Quotes** - Breaks Parsing
```bash
echo 'it\'s working'
# Sees: args=["'it\\'s", "it\\", "s", "working"]
# BUG: Splits into 4 separate tokens instead of 1 string
```

**Impact**: Escaped single quotes within single quotes are not handled correctly

#### **ANSI-C Quoting** - Completely Ignored
```bash
git commit -m $'test\nwith\nnewlines'
# Sees: command="git", subcommand="commit", flags=["-m"], args=[]
# BUG: $'...' string completely disappears
```

**Impact**: ANSI-C quoted strings are silently dropped

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
| Quoted strings | ✅ Good | Double quotes and basic single quotes work |
| Dangerous patterns | ✅ Perfect | Flag and subcommand matching works |
| Pipelines | ✅ Perfect | All commands extracted and validated |
| Command chains | ✅ Perfect | All commands extracted and validated |
| Redirections | ⚠️ Partial | Operators/targets stripped |
| Command substitution | ⚠️ Partial | Text extracted but not parsed recursively |
| Variables | ❌ Broken | Inconsistent (disappear or duplicate) |
| Escaped quotes | ❌ Broken | Splits incorrectly |
| ANSI-C quoting | ❌ Broken | Completely ignored |
| Brace expansion | ❌ Broken | Splits into multiple tokens |
| Find -exec | ❌ Broken | Misclassifies structure |
| Background `&` | 🚫 Ignored | Stripped but doesn't break |
| Heredocs | 🚫 Ignored | Stripped but doesn't break |

## Recommended Use Cases

### ✅ Good For:

1. **Single-command validation** - Works great for simple commands
2. **Multi-command validation** - Pipelines and chains fully parsed
3. **Dangerous flag detection** - Reliable for rm -rf, git --force, etc. across all commands
4. **Basic argument extraction** - Good for simple tools
5. **Subcommand routing** - Perfect for git/docker/npm workflows

### ⚠️ Use With Caution:

1. **File target extraction** - Cannot extract redirection targets
2. **Complex quoting** - May misbehave with escaped quotes
3. **Command substitution** - Nested commands not recursively parsed

### ❌ Don't Use For:

1. **Script parsing** - Too many unsupported constructs
2. **Variable expansion** - Unpredictable behavior
3. **Complex find commands** - Structure not recognized

## Security Implications

### 🔒 Safe:
- Detecting dangerous flags in simple commands
- Validating git/docker/npm operations
- Detecting dangerous commands in pipelines and chains

### ⚠️ Unsafe:
- **Variable evasion**: `rm -rf $HOME` → unpredictable (might not detect)
- **Command substitution**: `rm -rf $(dangerous)` → nested command not validated

## Recommendations

### For Command Validation Use Case:

1. **Use pipeline/chain parsing**: Parser now extracts all commands from pipelines (|) and chains (&&, ||, ;)
2. **Pattern matching**: Use dangerous pattern database across all commands in multi-command strings
3. **Pre-flight check for unsupported constructs**: Consider rejecting:
   - Command substitution (`$()`, backticks) if recursive validation needed
   - Complex redirections (`2>&1`, etc.) if file targets must be validated
   - Bare variables (`$VAR`) due to unpredictable behavior

### For Future Enhancement:

If you need full command parsing, consider:

1. **Redirection extraction**: Post-process AST to find redirection nodes and targets
2. **Variable expansion**: Pre-expand variables before parsing (if safe context)
3. **Recursive command substitution**: Parse nested commands in $() and backticks
