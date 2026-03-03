#!/usr/bin/env python3
"""
Extract bash commands from Claude Code session history with complexity analysis.

This tool parses JSONL session files and extracts Bash tool usage, categorizing
commands by their complexity patterns (pipes, redirection, chaining, etc.) to
help study how LLM agents construct and use bash commands.
"""

import json
import os
import sys
import re
from pathlib import Path
from collections import Counter, defaultdict
from datetime import datetime

def detect_complexity_patterns(command):
    """
    Detect complexity patterns in a bash command.

    Returns a dict of pattern names to boolean values indicating presence.
    """
    patterns = {
        'pipe': '|' in command and '\\|' not in command,  # Pipe to another command
        'redirect_output': bool(re.search(r'[^>]>(?!>)', command)),  # > (not >>)
        'redirect_append': '>>' in command,  # >>
        'redirect_input': '<' in command and '<<' not in command,  # < (not <<)
        'redirect_stderr': '2>' in command,  # Stderr redirection
        'redirect_both': '&>' in command or '2>&1' in command,  # Both stdout and stderr
        'heredoc': '<<' in command,  # Here-document
        'command_substitution': '$(' in command or '`' in command,  # $(cmd) or `cmd`
        'process_substitution': '<(' in command or '>(' in command,  # <(cmd) or >(cmd)
        'and_chain': '&&' in command,  # Sequential execution with failure stop
        'or_chain': '||' in command,  # Alternative execution
        'semicolon_chain': ';' in command,  # Sequential execution regardless
        'background_job': command.strip().endswith('&'),  # Background execution
        'variable_expansion': bool(re.search(r'\$\{?\w+\}?', command)),  # $VAR or ${VAR}
        'arithmetic_expansion': '$((' in command,  # $((expr))
        'brace_expansion': bool(re.search(r'\{[^}]+,[^}]+\}', command)),  # {a,b,c}
        'glob_pattern': bool(re.search(r'[*?]|\[[^\]]+\]', command)),  # *, ?, [...]
        'escape_sequences': '\\' in command,  # Backslash escaping
        'single_quotes': "'" in command,  # Single quotes
        'double_quotes': '"' in command,  # Double quotes
        'multiline': '\n' in command or '\\' in command and '\n' in command,  # Multi-line command
        'conditional': bool(re.search(r'\b(if|then|else|elif|fi|case|esac)\b', command)),  # Conditionals
        'loop': bool(re.search(r'\b(for|while|until|do|done)\b', command)),  # Loops
        'function_def': bool(re.search(r'\bfunction\s+\w+|^\w+\s*\(\)', command)),  # Function definition
        'test_construct': bool(re.search(r'\[\[.*\]\]|\[.*\]', command)),  # [[ ]] or [ ]
    }

    return patterns

def calculate_complexity_score(patterns):
    """Calculate a complexity score (0-10) based on detected patterns."""
    # Weight different patterns by their complexity contribution
    weights = {
        'pipe': 1,
        'redirect_output': 0.5,
        'redirect_append': 0.5,
        'redirect_input': 0.5,
        'redirect_stderr': 1,
        'redirect_both': 1.5,
        'heredoc': 2,
        'command_substitution': 1.5,
        'process_substitution': 2,
        'and_chain': 1,
        'or_chain': 1,
        'semicolon_chain': 0.5,
        'background_job': 1,
        'variable_expansion': 0.5,
        'arithmetic_expansion': 1,
        'brace_expansion': 1,
        'glob_pattern': 0.5,
        'escape_sequences': 0.5,
        'single_quotes': 0.25,
        'double_quotes': 0.25,
        'multiline': 1.5,
        'conditional': 2,
        'loop': 2,
        'function_def': 3,
        'test_construct': 1,
    }

    score = sum(weights.get(pattern, 0) for pattern, present in patterns.items() if present)
    return min(10, score)  # Cap at 10

def extract_bash_commands(session_file):
    """Extract bash commands from a single session file."""
    commands = []

    with open(session_file, 'r', encoding='utf-8') as f:
        for line in f:
            try:
                record = json.loads(line)
                timestamp = record.get('timestamp', '')
                msg = record.get('message')
                if not msg:
                    continue

                if msg.get('role') != 'assistant':
                    continue

                content = msg.get('content', [])
                if not isinstance(content, list):
                    continue

                for item in content:
                    if not isinstance(item, dict):
                        continue

                    if item.get('type') == 'tool_use' and item.get('name') == 'Bash':
                        input_data = item.get('input', {})
                        command = input_data.get('command', '')
                        description = input_data.get('description', '')

                        if command:
                            # Detect complexity patterns
                            complexity_patterns = detect_complexity_patterns(command)
                            complexity_score = calculate_complexity_score(complexity_patterns)

                            commands.append({
                                'timestamp': timestamp,
                                'command': command,
                                'description': description,
                                'complexity_score': complexity_score,
                                'complexity_patterns': {k: v for k, v in complexity_patterns.items() if v}
                            })
            except json.JSONDecodeError:
                continue
            except Exception as e:
                print(f"Error processing line in {session_file}: {e}", file=sys.stderr)
                continue

    return commands

def get_git_branch_from_session(session_file):
    """Extract git branch from session's gitBranch field if available."""
    with open(session_file, 'r', encoding='utf-8') as f:
        for line in f:
            try:
                record = json.loads(line)
                git_branch = record.get('gitBranch')
                if git_branch:
                    return git_branch
            except:
                continue
    return None

def categorize_command(command):
    """Categorize a command by its primary tool/operation."""
    cmd = command.strip().split()[0] if command.strip() else ''

    # Handle complex commands - get first command
    if '|' in command:
        cmd = command.split('|')[0].strip().split()[0]
    if '&&' in command:
        cmd = command.split('&&')[0].strip().split()[0]
    if ';' in command:
        cmd = command.split(';')[0].strip().split()[0]

    # Common categories
    if cmd in ['git']:
        return 'git'
    elif cmd in ['make']:
        return 'make'
    elif cmd in ['npm', 'yarn', 'pnpm', 'node']:
        return 'npm/node'
    elif cmd in ['python', 'python3', 'pip', 'pip3']:
        return 'python'
    elif cmd in ['ls', 'cd', 'pwd', 'mkdir', 'rm', 'cp', 'mv', 'touch']:
        return 'filesystem'
    elif cmd in ['grep', 'find', 'ag', 'rg']:
        return 'search'
    elif cmd in ['cat', 'less', 'more', 'head', 'tail']:
        return 'file-reading'
    elif cmd in ['sed', 'awk', 'cut', 'tr', 'sort', 'uniq']:
        return 'text-processing'
    elif cmd in ['docker', 'docker-compose']:
        return 'docker'
    elif cmd in ['cargo', 'rustc']:
        return 'rust'
    elif cmd in ['go']:
        return 'go'
    elif cmd in ['emacs']:
        return 'emacs'
    elif cmd in ['./bin/emacs-isolated.sh', './bin/run-tests.sh', './bin/tangle-org.sh']:
        return 'custom-scripts'
    elif cmd.startswith('./'):
        return 'custom-scripts'
    else:
        return 'other'

def normalize_command_pattern(command):
    """Normalize a command to identify patterns."""
    # Normalize paths
    pattern = re.sub(r'/[^\s]+', '<path>', command)
    # Normalize quoted strings
    pattern = re.sub(r'"[^"]+"', '<string>', pattern)
    pattern = re.sub(r"'[^']+'", '<string>', pattern)
    # Normalize numbers
    pattern = re.sub(r'\b\d+\b', '<num>', pattern)
    # Normalize git hashes
    pattern = re.sub(r'\b[0-9a-f]{7,40}\b', '<hash>', pattern)
    return pattern

def main():
    session_dir = Path.home() / '.claude' / 'projects' / '-Users-jefffarr-emacs'
    research_dir = Path.home() / 'emacs' / 'config' / 'experiments' / 'bash-parser' / 'research'
    output_jsonl = research_dir / 'bash-commands-from-sessions.jsonl'
    output_summary = research_dir / 'bash-commands-summary.md'
    output_complexity = research_dir / 'bash-commands-complexity-analysis.md'

    print("Finding session files...", file=sys.stderr)
    session_files = list(session_dir.glob('*.jsonl'))
    print(f"Found {len(session_files)} session files", file=sys.stderr)

    all_commands = []
    sessions_with_commands = 0
    command_patterns = Counter()
    command_categories = Counter()
    complexity_pattern_counts = Counter()

    for i, session_file in enumerate(session_files, 1):
        if i % 10 == 0:
            print(f"Processing session {i}/{len(session_files)}...", file=sys.stderr)

        session_id = session_file.stem
        git_branch = get_git_branch_from_session(session_file)
        commands = extract_bash_commands(session_file)

        if commands:
            sessions_with_commands += 1

        for cmd_data in commands:
            cmd_data['session'] = session_id
            cmd_data['gitBranch'] = git_branch
            all_commands.append(cmd_data)

            # Track patterns and categories
            pattern = normalize_command_pattern(cmd_data['command'])
            command_patterns[pattern] += 1

            category = categorize_command(cmd_data['command'])
            command_categories[category] += 1

            # Track complexity patterns
            for cp in cmd_data.get('complexity_patterns', {}).keys():
                complexity_pattern_counts[cp] += 1

    print(f"Extracted {len(all_commands)} commands from {sessions_with_commands} sessions", file=sys.stderr)

    # Write JSONL output
    print(f"Writing commands to {output_jsonl}...", file=sys.stderr)
    with open(output_jsonl, 'w', encoding='utf-8') as f:
        for cmd in all_commands:
            f.write(json.dumps(cmd) + '\n')

    # Generate summary report
    print(f"Writing summary to {output_summary}...", file=sys.stderr)
    generate_summary_report(output_summary, all_commands, len(session_files),
                          sessions_with_commands, command_patterns, command_categories)

    # Generate complexity analysis report
    print(f"Writing complexity analysis to {output_complexity}...", file=sys.stderr)
    generate_complexity_report(output_complexity, all_commands, complexity_pattern_counts)

    print("Done!", file=sys.stderr)
    print(f"Commands saved to: {output_jsonl}", file=sys.stderr)
    print(f"Summary saved to: {output_summary}", file=sys.stderr)
    print(f"Complexity analysis saved to: {output_complexity}", file=sys.stderr)

def generate_summary_report(output_file, all_commands, total_sessions,
                           sessions_with_commands, command_patterns, command_categories):
    """Generate the main summary report."""
    commands_by_category = defaultdict(list)
    for cmd in all_commands:
        category = categorize_command(cmd['command'])
        commands_by_category[category].append(cmd)

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write("# Bash Commands from Claude Code Sessions\n\n")
        f.write(f"**Analysis Date:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")

        f.write("## Overview\n\n")
        f.write(f"- **Total sessions analyzed:** {total_sessions}\n")
        f.write(f"- **Sessions with bash commands:** {sessions_with_commands} ({sessions_with_commands/total_sessions*100:.1f}%)\n")
        f.write(f"- **Total commands extracted:** {len(all_commands)}\n")
        f.write(f"- **Unique command patterns:** {len(command_patterns)}\n")

        # Add complexity overview
        scores = [cmd['complexity_score'] for cmd in all_commands]
        avg_score = sum(scores) / len(scores) if scores else 0
        f.write(f"- **Average complexity score:** {avg_score:.2f}/10\n")
        f.write(f"- **High complexity commands (≥5):** {sum(1 for s in scores if s >= 5)} ({sum(1 for s in scores if s >= 5)/len(scores)*100:.1f}%)\n\n")

        f.write("## Command Categories\n\n")
        f.write("| Category | Count | Percentage |\n")
        f.write("|----------|-------|------------|\n")
        for category, count in command_categories.most_common():
            percentage = (count / len(all_commands) * 100) if all_commands else 0
            f.write(f"| {category} | {count} | {percentage:.1f}% |\n")
        f.write("\n")

        f.write("## Top 20 Most Common Command Patterns\n\n")
        f.write("| Rank | Pattern | Count |\n")
        f.write("|------|---------|-------|\n")
        for i, (pattern, count) in enumerate(command_patterns.most_common(20), 1):
            pattern_display = pattern.replace('|', '\\|')
            f.write(f"| {i} | `{pattern_display}` | {count} |\n")
        f.write("\n")

        f.write("## Example Commands by Category\n\n")
        for category, cmds in sorted(commands_by_category.items()):
            f.write(f"### {category.title()} ({len(cmds)} commands)\n\n")
            for cmd in cmds[:5]:
                command = cmd['command'].replace('|', '\\|')
                desc = cmd.get('description', 'No description')
                f.write(f"- `{command}`\n")
                if desc:
                    f.write(f"  - {desc}\n")
            if len(cmds) > 5:
                f.write(f"\n*...and {len(cmds) - 5} more*\n")
            f.write("\n")

def generate_complexity_report(output_file, all_commands, complexity_pattern_counts):
    """Generate a detailed complexity analysis report."""
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write("# Bash Command Complexity Analysis\n\n")
        f.write(f"**Analysis Date:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        f.write("This report focuses on command complexity patterns to help understand how LLM agents construct bash commands.\n\n")

        # Complexity score distribution
        f.write("## Complexity Score Distribution\n\n")
        scores = [cmd['complexity_score'] for cmd in all_commands]
        score_buckets = Counter()
        for score in scores:
            if score == 0:
                score_buckets['0 (simple)'] += 1
            elif score < 2:
                score_buckets['1-2 (low)'] += 1
            elif score < 4:
                score_buckets['2-4 (medium)'] += 1
            elif score < 6:
                score_buckets['4-6 (high)'] += 1
            else:
                score_buckets['6+ (very high)'] += 1

        f.write("| Score Range | Count | Percentage |\n")
        f.write("|-------------|-------|------------|\n")
        for bucket in ['0 (simple)', '1-2 (low)', '2-4 (medium)', '4-6 (high)', '6+ (very high)']:
            count = score_buckets[bucket]
            pct = count / len(all_commands) * 100 if all_commands else 0
            f.write(f"| {bucket} | {count} | {pct:.1f}% |\n")
        f.write("\n")

        # Pattern frequency
        f.write("## Complexity Pattern Frequency\n\n")
        f.write("| Pattern | Count | Percentage |\n")
        f.write("|---------|-------|------------|\n")
        for pattern, count in sorted(complexity_pattern_counts.items(), key=lambda x: x[1], reverse=True):
            pct = count / len(all_commands) * 100 if all_commands else 0
            f.write(f"| {pattern.replace('_', ' ').title()} | {count} | {pct:.1f}% |\n")
        f.write("\n")

        # Examples by complexity pattern
        f.write("## Examples by Complexity Pattern\n\n")
        f.write("This section shows real commands grouped by their complexity patterns.\n\n")

        # Group commands by their most interesting pattern
        pattern_examples = defaultdict(list)
        for cmd in all_commands:
            patterns = cmd.get('complexity_patterns', {})
            if not patterns:
                continue
            # Add to multiple pattern groups
            for pattern in patterns.keys():
                if len(pattern_examples[pattern]) < 5:  # Limit examples per pattern
                    pattern_examples[pattern].append(cmd)

        for pattern in sorted(complexity_pattern_counts.keys(), key=lambda x: complexity_pattern_counts[x], reverse=True):
            if pattern not in pattern_examples:
                continue

            f.write(f"### {pattern.replace('_', ' ').title()} ({complexity_pattern_counts[pattern]} commands)\n\n")

            for cmd in pattern_examples[pattern][:3]:
                command = cmd['command'].replace('|', '\\|')
                desc = cmd.get('description', 'No description')
                score = cmd['complexity_score']
                patterns = ', '.join(cmd.get('complexity_patterns', {}).keys())

                f.write(f"**Complexity: {score:.1f}/10** - Patterns: {patterns}\n\n")
                f.write(f"```bash\n{cmd['command']}\n```\n\n")
                if desc:
                    f.write(f"*Description: {desc}*\n\n")

            f.write("\n")

        # Most complex commands
        f.write("## Top 20 Most Complex Commands\n\n")
        most_complex = sorted(all_commands, key=lambda x: x['complexity_score'], reverse=True)[:20]

        for i, cmd in enumerate(most_complex, 1):
            command = cmd['command'].replace('|', '\\|')
            desc = cmd.get('description', 'No description')
            score = cmd['complexity_score']
            patterns = ', '.join(cmd.get('complexity_patterns', {}).keys())

            f.write(f"### {i}. Complexity: {score:.1f}/10\n\n")
            f.write(f"**Patterns:** {patterns}\n\n")
            f.write(f"```bash\n{cmd['command']}\n```\n\n")
            if desc:
                f.write(f"*{desc}*\n\n")

if __name__ == '__main__':
    main()
