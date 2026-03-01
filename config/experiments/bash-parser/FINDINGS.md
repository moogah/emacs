# Bash Parser Exploration Findings

## Summary

Tested **43 commands** (11 baseline, 32 exploratory) with **100% parse success rate**. Tree-sitter bash grammar is robust and never crashes, but has specific behaviors for complex constructs.

## Critical Findings

### ✅ What Works Perfectly

1. **Simple commands**: `ls -la /tmp` ✓
2. **Subcommands**: `git log --oneline` ✓
3. **Quoted strings**: `echo "hello 'world'"` ✓
4. **Dangerous pattern detection**: `rm -rf`, `git push --force`, `python -c` ✓
5. **Glob patterns**: `ls *.txt` (preserved as literal strings) ✓
6. **Variables in quotes**: `git commit -m "$message"` ✓

### ⚠️ Partial Support (Usable but Limited)

#### **Pipelines** - Only First Command Visible
```bash
ls -la | grep test
# Sees: command="ls", flags=["-la"]
# Missing: "grep test" completely ignored
```

**Impact**: Cannot detect dangerous commands in pipeline stages
- `rm safe.txt | rm -rf /` → would NOT detect the dangerous second command

#### **Command Chains** - Only First Command Visible
```bash
git add . && git commit -m 'test' && git push
# Sees: command="git", subcommand="add", args=["."]
# Missing: commit and push completely ignored
```

**Impact**: Cannot validate multi-command workflows
- `mkdir /tmp/test && cd /tmp/test && rm -rf *` → would NOT see the dangerous rm

#### **Redirections** - Operators and Targets Stripped
```bash
echo 'hello' > output.txt
# Sees: command="echo", args=["hello"]
# Missing: "> output.txt" ignored
```

```bash
grep error < logfile.txt
# Sees: command="grep", args=["error"]
# Missing: "< logfile.txt" ignored
```

**Impact**: Cannot extract file targets from redirections

#### **Command Substitution** - Extracts Inner Text
```bash
echo $(date)
# Sees: command="echo", args=["date"]
# Note: extracts "date" but doesn't mark it as substitution
```

```bash
git commit -m "$(cat message.txt)"
# Sees: command="git", subcommand="commit",
#       flags=["-m"],
#       args=["$(cat message.txt)", "cat", "message.txt"]
# Note: extracts BOTH the full string AND the inner command parts (messy!)
```

**Impact**: Cannot distinguish between literal strings and command substitution

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

### 🚫 Not Supported (By Design)

These constructs are completely ignored but don't break parsing:

1. **Background processes**: `&` operator stripped
2. **Heredocs**: `<< EOF` blocks ignored
3. **Second/third commands in pipelines**: Only first command seen
4. **Second/third commands in chains**: Only first command seen

## Parser Capabilities Summary

| Feature | Support | Notes |
|---------|---------|-------|
| Simple commands | ✅ Perfect | Core functionality works great |
| Flags | ✅ Perfect | Both short and long flags detected |
| Positional args | ✅ Perfect | Non-flag arguments extracted |
| Subcommands | ✅ Perfect | Git, docker, npm, etc. |
| Quoted strings | ✅ Good | Double quotes and basic single quotes work |
| Dangerous patterns | ✅ Perfect | Flag and subcommand matching works |
| Pipelines | ⚠️ Partial | Only first command visible |
| Command chains | ⚠️ Partial | Only first command visible |
| Redirections | ⚠️ Partial | Operators/targets stripped |
| Command substitution | ⚠️ Partial | Text extracted but not marked special |
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
2. **Dangerous flag detection** - Reliable for rm -rf, git --force, etc.
3. **Basic argument extraction** - Good for simple tools
4. **Subcommand routing** - Perfect for git/docker/npm workflows

### ⚠️ Use With Caution:

1. **Multi-command strings** - Only validates first command in pipeline/chain
2. **File target extraction** - Cannot extract redirection targets
3. **Complex quoting** - May misbehave with escaped quotes

### ❌ Don't Use For:

1. **Script parsing** - Too many unsupported constructs
2. **Variable expansion** - Unpredictable behavior
3. **Complex find commands** - Structure not recognized
4. **Validation of command chains** - Only sees first command

## Security Implications

### 🔒 Safe:
- Detecting dangerous flags in simple commands
- Validating single git/docker/npm operations

### ⚠️ Unsafe:
- **Pipeline evasion**: `echo safe | rm -rf /tmp` → would NOT detect rm -rf
- **Chain evasion**: `ls && rm -rf /tmp` → would NOT detect rm -rf
- **Variable evasion**: `rm -rf $HOME` → unpredictable (might not detect)

## Recommendations

### For Command Validation Use Case:

1. **Pre-flight check**: Reject commands containing:
   - Pipeline operators (`|`)
   - Chain operators (`&&`, `||`, `;`)
   - Command substitution (`$()`, backticks)
   - Complex redirections (`2>&1`, etc.)

2. **Allow list approach**: Only permit simple, single commands
   - Example: `git add file.txt` ✓
   - Example: `git add . && git commit` ✗ (reject)

3. **Pattern matching**: Use dangerous pattern database for allowed commands

### For Future Enhancement:

If you need full command parsing, consider:

1. **Multi-command detection**: Add explicit detection for `|`, `&&`, `||`, `;`
2. **Redirection extraction**: Post-process AST to find redirection nodes
3. **Variable expansion**: Pre-expand variables before parsing (if safe context)
4. **Recursive parsing**: Parse pipeline/chain stages separately

## Next Steps

1. **Document limitations** in main README
2. **Add pre-flight validation** for disallowed constructs
3. **Extend dangerous patterns** database
4. **Consider recursive parser** for multi-command support (if needed)
