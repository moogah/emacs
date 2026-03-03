# Bash Command Research Corpus

This directory contains a corpus of actual bash commands executed by LLM agents (Claude Code sessions), extracted for studying command patterns, edge cases, and complexity.

## Purpose

To build a library of real-world bash commands that LLM agents construct and execute, focusing on:
- Command chaining (`&&`, `||`, `;`)
- Pipes (`|`)
- Redirection (`>`, `>>`, `<`, `2>`, `2>&1`, `&>`)
- Here-documents (`<<EOF`)
- Command/process substitution (`$(...)`, `<(...)`)
- Loops and conditionals
- Complex quoting and escaping
- Variable expansion and glob patterns

## Files

### bash-commands-from-sessions.jsonl (~973KB)
Raw data in JSONL format. Each line is a JSON object with:
```json
{
  "session": "session-uuid",
  "timestamp": "2026-03-03T19:08:42.123Z",
  "gitBranch": "branch-name",
  "command": "the actual bash command",
  "description": "what the command does",
  "complexity_score": 5.8,
  "complexity_patterns": {
    "pipe": true,
    "redirect_stderr": true,
    "loop": true,
    ...
  }
}
```

**Query examples:**
```bash
# Find all commands with pipes
jq -r 'select(.complexity_patterns.pipe) | .command' bash-commands-from-sessions.jsonl

# High complexity commands (score >= 7)
jq -r 'select(.complexity_score >= 7) | .command' bash-commands-from-sessions.jsonl

# Commands with here-docs
jq -r 'select(.complexity_patterns.heredoc) | .command' bash-commands-from-sessions.jsonl

# Count commands by pattern
jq -r '.complexity_patterns | keys[]' bash-commands-from-sessions.jsonl | sort | uniq -c | sort -rn
```

### bash-commands-summary.md (~10KB)
Overview statistics:
- Total commands extracted (1,552 from 90 sessions)
- Command categories (git, filesystem, custom scripts, etc.)
- Most common command patterns
- Examples by category
- Complexity score distribution

### bash-commands-complexity-analysis.md (~195KB)
Detailed complexity analysis:
- Complexity score distribution (0-10 scale)
- Pattern frequency (pipes, redirection, loops, etc.)
- Examples grouped by complexity pattern
- **Top 20 most complex commands** with full pattern breakdowns

## Complexity Scoring

Commands are scored 0-10 based on detected patterns:

| Pattern | Weight | Example |
|---------|--------|---------|
| Function definition | 3.0 | `function foo() { ... }` |
| Here-document | 2.0 | `cat <<EOF` |
| Process substitution | 2.0 | `diff <(cmd1) <(cmd2)` |
| Loops | 2.0 | `for x in *; do ... done` |
| Conditionals | 2.0 | `if [ -f file ]; then ... fi` |
| Stderr redirection | 1.5 | `cmd 2>&1` |
| Command substitution | 1.5 | `$(command)` |
| Multiline | 1.5 | Commands with `\` continuation |
| Test constructs | 1.0 | `[[ condition ]]` |
| Pipes | 1.0 | `cmd1 \| cmd2` |
| And/or chains | 1.0 | `cmd1 && cmd2`, `cmd1 \|\| cmd2` |
| Background jobs | 1.0 | `cmd &` |
| Brace expansion | 1.0 | `{a,b,c}` |
| Glob patterns | 0.5 | `*.txt`, `[a-z]*` |
| Variable expansion | 0.5 | `$VAR`, `${VAR}` |
| Redirection | 0.5 | `>`, `>>`, `<` |
| Semicolon chains | 0.5 | `cmd1; cmd2` |
| Escapes/quotes | 0.25-0.5 | `\`, `'...'`, `"..."` |

## Regenerating the Corpus

The extraction script is in this directory:

```bash
# From this directory
./extract-bash-commands.py

# Or from repository root
./config/experiments/bash-parser/research/extract-bash-commands.py

# Or run from anywhere
~/emacs/config/experiments/bash-parser/research/extract-bash-commands.py
```

This will re-scan all Claude Code session files at `~/.claude/projects/-Users-jefffarr-emacs/*.jsonl` and regenerate all three files.

## Dataset Statistics

**Current corpus (as of 2026-03-03):**
- 1,552 total commands
- 90 sessions with bash usage
- 881 unique command patterns
- Date range: Feb 5 - Mar 3, 2026 (26 days)
- Average complexity: 2.47/10

**Complexity distribution:**
- Simple (0): 37.6%
- Low (1-2): 27.6%
- Medium (2-4): 8.4%
- High (4-6): 11.8%
- Very high (6+): 14.7%

**Top complexity patterns:**
1. Double quotes: 45.2%
2. Pipes: 23.1%
3. Output redirection: 22.9%
4. Multiline: 18.8%
5. Stderr redirection: 15.1%
6. Loops: 12.4%
7. Command substitution: 11.8%
8. Here-docs: 7.9%

## Use Cases

This corpus is useful for:
- Training bash parsers on real-world LLM-generated commands
- Testing command execution validators
- Understanding LLM agent bash usage patterns
- Studying edge cases in command construction
- Building safety/security filters for agent tool use
- Analyzing common vs. rare bash idioms
