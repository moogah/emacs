#!/usr/bin/env python3
"""
Extract bash commands from Claude Code session history.

This tool parses JSONL session files and extracts Bash tool usage for
research into bash parser completeness.
"""

import json
import os
import sys
from pathlib import Path

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
                            commands.append({
                                'timestamp': timestamp,
                                'command': command,
                                'description': description
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

def main():
    session_dir = Path.home() / '.claude' / 'projects' / '-Users-jefffarr-emacs'
    research_dir = Path.home() / 'emacs' / 'config' / 'experiments' / 'bash-parser' / 'research'
    output_jsonl = research_dir / 'bash-commands-from-sessions.jsonl'

    print("Finding session files...", file=sys.stderr)
    session_files = list(session_dir.glob('*.jsonl'))
    print(f"Found {len(session_files)} session files", file=sys.stderr)

    all_commands = []
    sessions_with_commands = 0

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

    print(f"Extracted {len(all_commands)} commands from {sessions_with_commands} sessions", file=sys.stderr)

    # Write JSONL output
    print(f"Writing commands to {output_jsonl}...", file=sys.stderr)
    with open(output_jsonl, 'w', encoding='utf-8') as f:
        for cmd in all_commands:
            f.write(json.dumps(cmd) + '\n')

    print("Done!", file=sys.stderr)
    print(f"Commands saved to: {output_jsonl}", file=sys.stderr)

if __name__ == '__main__':
    main()
