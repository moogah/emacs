# GPTEL Persistent Sessions Architecture

## Overview

This architecture simplification removes ~40% of custom complexity by leveraging gptel's native tool persistence capabilities and replacing invisible subagents with visible buffer-based execution.

## Core Principles

1. **Leverage gptel's native capabilities** - Don't reimplement what gptel already does
2. **Visible buffers for all interactions** - Subagents are first-class citizens with their own buffers
3. **Nested session directories** - Clear parent-child relationships via filesystem structure
4. **Preset-based configuration** - Consistent settings via preset.json files

## Directory Structure

```
sessions/
├── parent-session-TIMESTAMP/
│   ├── session.md              # Conversation + tool results (gptel native persistence)
│   ├── metadata.json           # Session config + relationships
│   ├── scope-plan.yml          # Access control policy
│   ├── preset.json             # Backend/model/temperature + gptel-include-tool-results
│   ├── system-prompts.org      # System prompt history
│   └── subagents/
│       ├── bash-TIMESTAMP-description/
│       │   ├── session.md
│       │   ├── metadata.json   # Includes parent-session-id
│       │   ├── scope-plan.yml  # Permissive for agents
│       │   └── preset.json
│       └── explore-TIMESTAMP-description/
│           └── (same structure)
```

## Key Components

### 1. Native Tool Persistence (`gptel-include-tool-results`)

**What it is:** A gptel setting that automatically saves tool inputs/outputs in the buffer text.

**How it works:**
- When `gptel-include-tool-results` is `t`, tool calls are marked with `(gptel tool)` text property
- Results are persisted automatically via gptel's native `gptel--save-state`
- Restored automatically when buffer is reopened
- File-local variables preserve state across sessions

**Configuration:**
- Set in `preset.json` for each session
- Defaults to `t` for all new sessions
- Applied via `jf/gptel--apply-session-preset`

**What was removed:**
- ~config/gptel/sessions/tracing.el~ - Custom tool logging (200+ lines)
- ~config/gptel/sessions/hooks.el~ - FSM transition advice (150+ lines)
- ~tools.org~ generation - Tool call logs in separate file

### 2. VisibleAgent Tool

**What it is:** A gptel tool that creates subagents in visible buffers with full session tracking.

**Location:** `config/gptel/tools/visible-agent.el`

**Key functions:**
- `jf/gptel-visible-agent-execute` - Main tool function
- Creates nested directory under parent's `subagents/` folder
- Initializes visible buffer with agent system prompt
- Creates metadata with `parent-session-id` linking
- Auto-generates permissive scope plan for agents
- Inherits preset from parent session

**Tool registration:**
```elisp
(gptel-make-tool
 :name "VisibleAgent"
 :description "Execute specialized agent in visible buffer..."
 :function #'jf/gptel-visible-agent-execute
 :args (list agent-type prompt)
 :category "agent")
```

**Workflow:**
1. LLM calls `VisibleAgent` with agent-type and prompt
2. Tool creates nested directory: `parent/subagents/agent-type-TIMESTAMP-slug/`
3. Tool creates visible buffer with agent system prompt
4. User can switch to subagent buffer anytime
5. Subagent conversation auto-saves to `session.md`
6. Tool returns summary (buffer name + directory path)

### 3. Metadata System (Flexible)

**Location:** `config/gptel/sessions/metadata.el`

**Core fields:**
- `:session-id` - Unique session identifier
- `:created` - ISO8601 timestamp
- `:modified` - Last modification timestamp
- `:backend` - Backend name (e.g., "Claude")
- `:model` - Model name/symbol
- `:tree` - Conversation tree structure

**Subagent fields:**
- `:type` - "subagent" (vs "session")
- `:parent-session-id` - Links to parent
- `:agent-type` - Agent type (e.g., "Bash")
- `:prompt-preview` - Task preview

**Serialization:** Flexible plist ↔ JSON conversion handles any additional fields:
```elisp
(defun jf/gptel--metadata-to-json (metadata)
  "Converts ANY plist fields to JSON with snake_case keys")

(defun jf/gptel--metadata-from-json (json-string)
  "Converts ANY JSON fields to plist with kebab-case keywords")
```

### 4. Filesystem Discovery (Recursive)

**Location:** `config/gptel/sessions/filesystem.el`

**Key function:** `jf/gptel--find-all-sessions-recursive`

**What it does:**
- Recursively finds all session directories
- Discovers nested subagents under `subagents/` folders
- Tracks depth and parent-path relationships

**Returns:** List of plists with:
```elisp
(:path "/path/to/session"
 :id "session-name-TIMESTAMP"
 :depth 0  ; 0 for top-level, 1+ for subagents
 :parent-path "/path/to/parent")  ; nil for top-level
```

**Used by:** Hierarchical browser, subagent resume, session tree rendering

### 5. Hierarchical Session Browser

**Location:** `config/gptel/sessions/commands.el`

**Key functions:**
- `jf/gptel--build-session-tree` - Converts flat list to tree structure
- `jf/gptel--render-session-tree` - Renders tree with indentation
- `jf/gptel-browse-sessions-hierarchical` - Interactive command

**Display format:**
```
# GPTEL Persistent Sessions

Total sessions: 5 (including subagents)

▸ parent-session-20260121150530
   Created: 2026-01-21T15:05:30Z
  └─ bash-20260121150612-fix-tests [Bash subagent]
     Created: 2026-01-21T15:06:12Z
  └─ explore-20260121150648-find-api [Explore subagent]
     Created: 2026-01-21T15:06:48Z
```

### 6. Subagent Module (Simplified)

**Location:** `config/gptel/sessions/subagent.el`

**What's left:**
- `jf/gptel--link-subagent-to-parent` - Updates parent metadata
- `jf/gptel--session-id-from-directory` - Path utility

**What was removed:**
- All FSM advice functions
- Callback wrapping logic
- Overlay-based progress feedback
- Complex subagent session creation (moved to VisibleAgent tool)

## Session Lifecycle

### Creating a New Session

```elisp
(jf/gptel-persistent-session "My Task")
```

**What happens:**
1. Generate session ID: `my-task-TIMESTAMP`
2. Create directory with metadata.json
3. Create preset.json with `gptel-include-tool-results: true`
4. Create scope-plan.yml (default: deny-all)
5. Create session.md buffer with gptel-mode
6. Apply preset to buffer (sets `gptel-include-tool-results` buffer-locally)

### Creating a Subagent

**From LLM in parent session:**
```
Can you use VisibleAgent to explore the codebase for API endpoints?
```

**LLM calls:**
```json
{
  "tool": "VisibleAgent",
  "args": {
    "agent-type": "Explore",
    "prompt": "Find all API endpoints in the codebase"
  }
}
```

**What happens:**
1. Tool creates nested directory: `parent/subagents/explore-TIMESTAMP-find-all-api/`
2. Tool generates permissive scope-plan.yml
3. Tool creates preset.json (inherits parent backend/model)
4. Tool creates visible buffer: `*gptel-agent: explore-TIMESTAMP-find-all-api*`
5. Buffer initialized with Explore agent system prompt
6. User sees buffer pop up
7. Tool returns summary to LLM

### Resuming a Session

```elisp
(jf/gptel-resume-session "my-task-TIMESTAMP")
```

**What happens:**
1. Load `session.md` into buffer
2. Enable `gptel-mode` (triggers `gptel--restore-state`)
3. Apply preset from `preset.json` (includes `gptel-include-tool-results`)
4. Tool results and conversation history restored automatically

### Resuming a Subagent

```elisp
(jf/gptel-resume-subagent)
```

**Interactive prompt:** `[parent-id] subagent-id (agent-type)`

**What happens:**
1. Discover all subagents via `jf/gptel--find-all-sessions-recursive`
2. Filter to depth > 0
3. User selects from list
4. Load subagent's `session.md`
5. Apply subagent's preset.json
6. Restore conversation + tool results

## Module Organization

### Core Modules (Kept)
- `constants.el` - Buffer-local variables, file names
- `logging.el` - Leveled logging system
- `filesystem.el` - Directory creation, recursive discovery
- `registry.el` - In-memory session tracking
- `metadata.el` - Flexible JSON serialization with parent linking
- `commands.el` - User-facing commands, hierarchical browser

### Simplified Modules
- `subagent.el` - Reduced to 80 lines (was 256 lines)
  - Removed: FSM advice, callback wrapping, overlay feedback
  - Kept: Parent-child linking helper

### New Modules
- `../tools/visible-agent.el` - VisibleAgent tool (190 lines)

### Archived Modules (Moved to archive/)
- `tracing.el` - Custom tool logging (replaced by gptel native)
- `hooks.el` - FSM transition advice (no longer needed)

## Loading Order

**In config/gptel/gptel.org:**

```elisp
;; 1. Load foundational modules
(jf/load-module "config/gptel/sessions/constants.el")
(jf/load-module "config/gptel/sessions/logging.el")
(jf/load-module "config/gptel/sessions/filesystem.el")
(jf/load-module "config/gptel/sessions/registry.el")
(jf/load-module "config/gptel/sessions/metadata.el")

;; 2. Load simplified subagent helpers
(jf/load-module "config/gptel/sessions/subagent.el")

;; 3. Load VisibleAgent tool (after subagent helpers)
(jf/load-module "config/gptel/tools/visible-agent.el")

;; 4. Load user commands
(jf/load-module "config/gptel/sessions/commands.el")
```

## Benefits of This Architecture

### Complexity Reduction
- **Before:** 400+ lines of custom FSM advice, tool logging, callback wrapping
- **After:** ~190 lines for VisibleAgent tool, leverage gptel native persistence
- **Reduction:** ~40% less custom code

### User Experience
- **Visibility:** Subagents execute in visible buffers users can see and interact with
- **Transparency:** No hidden execution, users can switch to subagent buffers anytime
- **Debuggability:** Conversation + tool results visible in real-time

### Maintainability
- **Leverage native features:** Uses gptel's built-in tool persistence
- **Simpler mental model:** Subagents are just sessions with parent links
- **No fragile advice:** No hooking into gptel FSM internals

### Extensibility
- **Flexible metadata:** Can add any fields without changing serialization
- **Nested subagents:** Subagents can create their own subagents (recursive)
- **Multiple persistence methods:** Tool results in session.md, separate metadata/preset files

## Migration Path

### For Existing Sessions

**Option 1: Continue using old sessions**
- Old sessions without `gptel-include-tool-results` continue to work
- Tool results won't be persisted, but conversation history is preserved

**Option 2: Migrate to new format**
```bash
# Use migration script
./bin/migrate-sessions.sh
```

**What migration does:**
1. Removes `tools.org` (no longer generated)
2. Updates `preset.json` to add `"include-tool-results": true`
3. Preserves all conversation history in `session.md`

### For New Sessions

All new sessions automatically:
- Have `gptel-include-tool-results: t` in preset.json
- Persist tool results via gptel native mechanism
- Support VisibleAgent tool for subagent creation

## Testing Checklist

### Unit Tests
- [x] Native tool persistence works (create session, call tool, close/reopen)
- [x] VisibleAgent creates visible buffer
- [x] Subagent directory created under parent/subagents/
- [x] Metadata includes parent-session-id
- [x] Recursive discovery finds nested subagents

### Integration Tests
- [ ] End-to-end: Create session → VisibleAgent → tool call → resume
- [ ] Multiple nesting levels: parent → subagent → nested subagent
- [ ] Hierarchical browser shows correct tree structure

### Regression Tests
- [ ] Existing sessions still work
- [ ] Session resume restores tool results
- [ ] Multiple sessions don't interfere

## Future Enhancements

### Potential Improvements
1. **Session templates** - Pre-configured scope plans + presets for common workflows
2. **Subagent collaboration** - Multiple subagents sharing context
3. **Branching support** - Conversation tree branching at any point
4. **Export/import** - Package sessions for sharing

### Not Planned (Out of Scope)
- Real-time collaboration (multiple users in same session)
- Cloud sync (sessions are local-first)
- Version control integration (sessions are git-friendly as-is)
