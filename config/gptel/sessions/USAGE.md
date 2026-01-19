# GPTEL Session Browser - User Guide

## Overview

The GPTEL Session Browser provides filesystem-based conversation management for gptel sessions. Each conversation is stored as a directory tree where you can browse history, create branches, and resume from any point.

## Architecture

### Directory Structure

```
~/gptel-sessions/
└── 20260119170835-6b58/              # Session ID
    ├── current -> msg-1/.../context.md   # Symlink to active position
    ├── metadata.json                     # Session info (model, backend, created)
    ├── msg-1/                            # First user message
    │   ├── context.md                    # Complete conversation to this point
    │   └── response-1/                   # Assistant's first response
    │       ├── context.md                # Conversation including response
    │       └── msg-2/                    # Second user message
    │           ├── context.md
    │           └── response-2/
    │               └── context.md
    └── traces/                           # Agent execution traces
        └── trace-1/
            ├── metadata.json
            └── tool-results/
```

### Key Concepts

**Each node is a directory** - `msg-N` (user messages) and `response-N` (assistant responses)

**context.md contains complete history** - Each node stores the full conversation up to that point

**Branches are copies** - Create `msg-2-alt1` by copying `msg-2` directory

**Current symlink tracks position** - Points to the active conversation node

### Module Dependencies

```
┌─────────────────────────────────────────────────────────────┐
│                      gptel.el (loader)                       │
└─────────────────────────────────────────────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        │                     │                     │
        ▼                     ▼                     ▼
┌──────────────┐      ┌──────────────┐     ┌──────────────┐
│ constants.el │      │  logging.el  │     │filesystem.el │
└──────────────┘      └──────────────┘     └──────────────┘
        │                     │                     │
        └─────────────────────┼─────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        │                     │                     │
        ▼                     ▼                     ▼
┌──────────────┐      ┌──────────────┐     ┌──────────────┐
│ registry.el  │      │ metadata.el  │     │  tracing.el  │
│ (session ID, │      │ (simplified) │     │ (subagents)  │
│  node path)  │      └──────────────┘     └──────────────┘
└──────────────┘              │                     │
        │                     └─────────────────────┘
        ▼                                 │
┌──────────────┐                          │
│   hooks.el   │◄─────────────────────────┘
│ (autosave,   │
│  persistence)│
└──────────────┘
        │
        ├──────────────────┬──────────────────┐
        ▼                  ▼                  ▼
┌──────────────┐   ┌──────────────┐  ┌──────────────┐
│  browser.el  │   │ branching.el │  │   scope/     │
│ (navigation) │   │ (resume,     │  │ (optional)   │
│              │   │  branching)  │  │              │
└──────────────┘   └──────────────┘  └──────────────┘
```

## Common Workflows

### 1. Starting a New Session

```
┌─────────────────┐
│ Start new       │
│ gptel session   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ SPC l           │
│ (jf/gptel-     │
│  launcher)      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Send messages   │
│ with C-c RET    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Session auto-   │
│ saved to:       │
│ ~/gptel-        │
│  sessions/      │
│  SESSION-ID/    │
└─────────────────┘
```

### 2. Browsing Sessions

```
┌─────────────────┐
│ M-x jf/gptel-   │
│ browse-sessions │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Opens dirvish   │
│ in sessions dir │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Navigate with   │
│ j/k or arrows   │
│ RET to enter    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ View context:   │
│ C-c C-v         │
└─────────────────┘
```

### 3. Creating a Branch

```
┌─────────────────┐
│ Browse to node  │
│ where you want  │
│ to branch       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Put cursor on   │
│ msg-N directory │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Press C-c C-b   │
│ (branch from    │
│  point)         │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Creates         │
│ msg-N-alt1/     │
│ directory copy  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Navigate into   │
│ msg-N-alt1/     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Edit context.md │
│ (change message)│
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Press C-c C-r   │
│ (resume from    │
│  context)       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Loads context   │
│ into gptel      │
│ buffer          │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Send with       │
│ C-c RET         │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Response saved  │
│ in branch:      │
│ msg-N-alt1/     │
│   response-N-   │
│   alt1/         │
└─────────────────┘
```

### 4. Resuming a Session

```
┌─────────────────┐
│ Browse to any   │
│ context.md file │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Press C-c C-r   │
│ on directory or │
│ context.md file │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Context loaded  │
│ in gptel buffer │
│ with session    │
│ state restored  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Continue        │
│ conversation    │
│ with C-c RET    │
└─────────────────┘
```

## User Instructions

### Getting Started

1. **Start a new gptel conversation**:
   ```
   SPC l  (or M-x jf/gptel-launcher)
   ```
   Choose your model and send messages with `C-c RET`

2. **Sessions are automatically saved** to `~/gptel-sessions/SESSION-ID/`

### Browsing Your Sessions

**List all sessions**:
```
M-x jf/gptel-browse-sessions
```

This opens dirvish in the sessions directory. Each subdirectory is a session.

**Navigate the tree**:
- `RET` - Enter directory
- `j/k` - Move up/down
- `h` - Go up one level
- `TAB` - Expand/collapse in tree view

**View conversation at any point**:
- Put cursor on a `msg-N` or `response-N` directory
- Press `C-c C-v` - Opens context.md in read-only window
- This shows the complete conversation up to that point

**View tool calls**:
- Press `C-c C-t` on a node with tool calls
- Opens tools.md showing agent invocations and results

**Find current position**:
- In an active gptel buffer, run: `M-x jf/gptel-show-current-position`
- Opens dirvish at the current conversation node

### Creating Branches

A branch lets you try a different message at any point in the conversation.

**Step-by-step**:

1. **Browse to the point where you want to branch**:
   ```
   M-x jf/gptel-browse-sessions
   Navigate to: session-id/msg-1/response-1/
   ```

2. **Put cursor on the next message directory** (e.g., `msg-2`)

3. **Press `C-c C-b`** to create a branch
   - Creates `msg-2-alt1/` as a copy of `msg-2/`
   - Cursor moves to the new branch directory

4. **Navigate into the branch** and open `context.md`:
   ```
   RET (on msg-2-alt1)
   RET (on context.md)
   ```

5. **Edit the message** in context.md
   - Change the user message to try a different approach
   - Save the file

6. **Close context.md** and **return to dired**

7. **Put cursor on the `msg-2-alt1` directory**

8. **Press `C-c C-r`** to resume from this context
   - Loads the edited context into a gptel buffer
   - Sets up session state (model, backend, node path)
   - Buffer is ready to send

9. **Send with `C-c RET`**
   - Response is saved as `msg-2-alt1/response-2-alt1/`
   - You now have two conversation branches!

### Resuming from Any Point

You can resume a conversation from **any context.md file**:

1. **Navigate to the node** in dirvish/dired

2. **Press `C-c C-r`** on:
   - A `msg-N` or `response-N` directory, OR
   - A `context.md` file

3. **The conversation loads** into a gptel buffer

4. **Continue the conversation** with `C-c RET`

### Comparing Branches

To see different conversation paths side-by-side:

1. **Open main branch**:
   ```
   Navigate to msg-2/response-2/context.md
   C-c C-v (view context)
   ```

2. **Open alternate branch**:
   ```
   Navigate to msg-2-alt1/response-2-alt1/context.md
   C-c C-v (view context)
   ```

3. **Use window commands** to view side-by-side:
   ```
   C-x 3 (split window vertically)
   ```

## Keybinding Reference

### In Session Tree Mode (dired/dirvish)

Automatically enabled when browsing `~/gptel-sessions/`.

| Key         | Command                         | Description                           |
|-------------|---------------------------------|---------------------------------------|
| `C-c C-v`   | `jf/gptel-view-context-at-point` | View context.md (read-only)          |
| `C-c C-t`   | `jf/gptel-view-tools-at-point`   | View tools.md (agent invocations)    |
| `C-c C-p`   | `jf/gptel-show-current-position` | Navigate to current symlink          |
| `C-c C-r`   | `jf/gptel-resume-from-context`   | Resume session (live gptel buffer)   |
| `C-c C-b`   | `jf/gptel-branch-from-point`     | Copy node to create branch           |

### In GPTEL Buffer

| Key         | Command      | Description                    |
|-------------|--------------|--------------------------------|
| `C-c RET`   | `gptel-send` | Send message to API            |
| `C-c C-k`   | Kill request | Cancel ongoing request         |

### Standard Dired Navigation

| Key         | Action                              |
|-------------|-------------------------------------|
| `RET`       | Enter directory or open file        |
| `j/k`       | Move down/up                        |
| `h`         | Go up one directory level           |
| `TAB`       | Expand/collapse tree (in dirvish)   |
| `q`         | Quit dired buffer                   |

## Common Tasks

### Task: Review a Previous Conversation

```
M-x jf/gptel-browse-sessions
Navigate to session
Navigate through msg-1/response-1/msg-2/...
Press C-c C-v on any node to view context
```

### Task: Continue an Old Conversation

```
M-x jf/gptel-browse-sessions
Navigate to the session
Navigate to the last response node
Press C-c C-r to resume
Type new message and C-c RET to send
```

### Task: Try a Different Question at Message 3

```
M-x jf/gptel-browse-sessions
Navigate to msg-2/response-2/
Put cursor on msg-3
Press C-c C-b (creates msg-3-alt1)
Navigate into msg-3-alt1/
Open context.md and edit the message
Return to dired, put cursor on msg-3-alt1
Press C-c C-r to resume
Press C-c RET to send
```

### Task: Export a Conversation

All conversations are plain markdown files. To export:

```
From terminal:
cat ~/gptel-sessions/SESSION-ID/msg-1/response-1/.../context.md > export.md

Or copy context.md files directly - they're complete conversations!
```

## Tips and Best Practices

### Understanding View vs Resume

- **`C-c C-v` (View)**: Read-only, for browsing conversation history
- **`C-c C-r` (Resume)**: Live gptel buffer, for continuing conversation

**Use view** when you just want to see what was said.

**Use resume** when you want to continue or send a message.

### Branch Naming

Branches are automatically named:
- First branch: `msg-2-alt1`
- Second branch: `msg-2-alt2`
- Third branch: `msg-2-alt3`

The filesystem enforces this naming automatically.

### Current Symlink

The `current` symlink in each session points to your last message/response. It's updated automatically after each send.

You can follow it manually:
```bash
ls -l ~/gptel-sessions/SESSION-ID/current
```

### Session Metadata

Each session's `metadata.json` contains:
- `session_id`: Unique identifier
- `model`: Model used (e.g., claude-sonnet-4-5-20250929)
- `backend`: Backend name (e.g., Claude, ChatGPT)
- `created`: ISO timestamp of session creation

### Tool Calls and Agent Traces

When you use subagents (e.g., explorer, bash), their execution is traced:
- Tool invocations stored in `traces/trace-N/`
- Individual tool results in `traces/trace-N/tool-results/`
- Can be viewed with `C-c C-t` on nodes with tools.md

## Troubleshooting

### "No active session to branch"

**Problem**: Trying to branch without a session loaded.

**Solution**: Use `C-c C-b` in dired, not in a gptel buffer.

### "Must be in dired/dirvish to create branches"

**Problem**: Pressed `C-c C-b` in wrong buffer type.

**Solution**: Navigate to session directory in dired first, then use `C-c C-b`.

### Markdown keybinding conflicts

**Problem**: `C-c C-b` runs markdown command instead of branch.

**Solution**: The branching keybinding only works in dired/dirvish when viewing sessions. In markdown buffers, use the standard markdown keybindings.

### Context not sending

**Problem**: Edited context.md but nothing happens when sending.

**Solution**: You must **resume** from the context first (`C-c C-r`), which sets up session state. Only then can you send with `C-c RET`.

### Lost in the directory tree

**Problem**: Can't find where you are in the conversation.

**Solution**:
1. Follow the `current` symlink: `ls -l session-dir/current`
2. Or use `M-x jf/gptel-show-current-position` from active gptel buffer

## Advanced Usage

### Manual Branch Creation

You can create branches manually without using `C-c C-b`:

```bash
cd ~/gptel-sessions/SESSION-ID/msg-1/response-1/
cp -r msg-2 msg-2-alt1
cd msg-2-alt1
# Edit context.md
```

Then resume from the edited context with `C-c C-r`.

### Searching Conversations

Since everything is plain text markdown:

```bash
# Find conversations mentioning "docker"
grep -r "docker" ~/gptel-sessions/*/msg-*/context.md

# Find all sessions using a specific model
jq '.model' ~/gptel-sessions/*/metadata.json
```

### Version Control

You can put session directories under git:

```bash
cd ~/gptel-sessions/
git init
git add .
git commit -m "Archive gptel conversations"
```

Each context.md shows complete conversation state, making diffs meaningful.

### Exporting for LLM Context

Context files are designed to be fed back to LLMs:

```bash
# Get full conversation history
cat ~/gptel-sessions/SESSION-ID/current

# This can be pasted into any LLM interface
```

## File Structure Reference

### context.md Format

```markdown
# User

First message here

# Assistant

First response here

# User

Second message here

# Assistant

Second response here
```

Each context.md is a complete, self-contained conversation from the session start to that node.

### metadata.json Format

```json
{
  "session_id": "20260119170835-6b58",
  "model": "claude-sonnet-4-5-20250929",
  "backend": "Claude",
  "created": "2026-01-19T17:08:35Z"
}
```

## Getting Help

- Check this guide for common workflows
- Use `describe-key` (`C-h k`) to see what a keybinding does
- Use `describe-function` (`C-h f`) to see function documentation
- File issues at: [repository URL]

## Summary

**The filesystem IS the database**. Each directory is a conversation node, each context.md is a complete conversation state. Browse with dired, create branches by copying directories, resume by loading context files.

**Three key operations**:
1. **View** (`C-c C-v`) - Browse history
2. **Resume** (`C-c C-r`) - Continue conversation
3. **Branch** (`C-c C-b`) - Create alternate path

**Everything is plain text** - easy to read, search, version control, and export.
