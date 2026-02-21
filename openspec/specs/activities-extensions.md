# Activities Extensions Specification

## Purpose and Architecture

The activities extensions enhance the `activities.el` package with project-aware workflow capabilities, enabling activities to integrate with git projects, documentation systems, and session management.

### Non-Invasive Design

**Integration Pattern:**
- Extensions SHALL store all metadata in the activity's `etc` slot under the `:activity-extensions` key
- Extensions SHALL NOT modify `activities.el` internals or private state
- Extensions SHALL integrate via public API: `activities-activity-etc`, `activities-new`, `activities-save`, `activities-activities`, `activities--current`

**Lifecycle Hooks:**
- Resume: Extensions hook via `activities-before-resume-functions` (provided by activities.el)
- Suspend: Extensions use advice on `activities-suspend` (activities.el lacks before-suspend hook)

**Directory Structure:**
- Activity directories follow pattern: `~/emacs-activities/SLUG-YYYY-MM-DD/`
- Within each activity directory:
  - `worktrees/PROJECT-NAME/` - Git worktrees for projects
  - `session/` - gptel session files
- Date stamp (YYYY-MM-DD) ensures uniqueness for same-named activities created on different days

**Module Organization:**
- `activities.el` - Main loader, configuration, module orchestration
- `core.el` - Metadata system, lifecycle hooks, directory management, utility functions
- `projectile.el` - Git operations (branches, worktrees), file tracking
- `org-roam.el` - Document creation, resolution, synchronization
- `transient.el` - Interactive UI for activity creation

**Dependencies:**
- Required: `activities`, `transient`, `cl-lib`
- Optional (runtime-checked): `projectile`, `org-roam`, `recentf`, `jf/gptel-session-*`

---

## Metadata System

The metadata system defines the data model for extension state stored in the activity's `etc` slot.

### Schema Version 1

**Root Structure (`:activity-extensions` value):**
```elisp
(:version 1
 :projects (project-plist ...)
 :org-roam org-roam-plist
 :settings settings-plist
 :gptel-session gptel-session-plist
 :activity-directory "/path/to/activity-dir")
```

**Project Plist:**
```elisp
(:path "/path/to/project"
 :name "project-name"
 :branch "activity-branch-name"  ; nil if none
 :worktree "/path/to/worktree"   ; nil if none
 :recent-files ("/file1" "/file2" ...))
```

**Org-Roam Plist:**
```elisp
(:node-id "org-roam-node-id"
 :file "/path/to/org-file"
 :title "Activity: Name"
 :created t
 :last-synced (current-time))
```

**Settings Plist:**
```elisp
(:auto-open-recent t)  ; From activities-ext-auto-open-recent
```

**Gptel Session Plist:**
```elisp
(:session-id "session-identifier"
 :session-file "/path/to/session.md"
 :session-dir "/path/to/session-dir")
```

### Versioning Guarantees

**Version Compatibility:**
- WHEN metadata version > code version THEN warn user about format mismatch
- WHEN metadata version < code version THEN attempt forward-compatible read
- WHEN metadata is missing THEN create with defaults from config variables

**Version Changes:**
- Adding optional keys: backward-compatible, no version increment
- Removing keys: requires version increment
- Changing key semantics: requires version increment
- Restructuring nested plists: requires version increment

**Effective Path Semantics:**
- For projects: effective path is worktree if present, else path
- For validation: check effective path, not both separately

---

## Lifecycle Integration

The lifecycle integration defines behavioral guarantees for the suspend/resume cycle.

### Resume Behavior

**Hook:** `activities-before-resume-functions` (provided by activities.el)

**Execution Order:**
1. Open gptel session buffer (if session exists and jf/gptel-session--open-existing bound)
2. Open org-roam document in current window (if org-roam data exists)
3. Open recent files for each project (if auto-open-recent enabled in settings)

**Scenarios:**

```
WHEN resuming activity WITH org-roam data
THEN find and open document using three-way resolution strategy
  (see Org-Roam Integration section)

WHEN resuming activity WITH auto-open-recent enabled
THEN open recent files for each project up to activities-ext-recent-files-limit
AND handle missing files gracefully (report count but continue)

WHEN resuming activity WITH gptel-session data AND session file exists
THEN open session buffer in background

WHEN resuming activity WITHOUT metadata
THEN silently skip (no error)
```

**Error Handling:**
- File not found: report but continue with remaining files
- Optional dependency unavailable: skip that integration silently
- Metadata corrupt: log warning but do not block resume

### Suspend Behavior

**Hook:** advice on `activities-suspend` (activities.el lacks before-suspend hook)

**Execution Order:**
1. For each project: capture recent files from `recentf-list` that belong to project path
2. Update metadata with new `:recent-files` for each project
3. Save metadata back to activity

**Scenarios:**

```
WHEN suspending activity
THEN for each project: extract up to activities-ext-recent-files-limit files from recentf-list
  WHERE file path starts with project path (or worktree path)
  AND file exists on filesystem
AND update project plist with :recent-files list
AND save metadata to activity

WHEN recentf-list is empty
THEN set :recent-files to nil (not an error)

WHEN recentf package not loaded
THEN skip file capture (degrade gracefully)
```

**Recent Files Algorithm:**
- Iterate `recentf-list` in order (most recent first)
- Match files to projects using `string-prefix-p` on truename-expanded paths
- Stop when limit reached per project
- Filter out non-existent files

### Hook Asymmetry Note

Activities.el provides `activities-before-resume-functions` but NOT `activities-before-suspend-functions`. This is why suspend integration uses advice rather than a hook. If activities.el adds a before-suspend hook in the future, the advice should be replaced with the hook for consistency.

---

## Git Operations Contract

The git operations define how projects are integrated with activity-specific branches and worktrees.

### Git Actions

**Available Actions:**
- `worktree` - Create new branch in separate worktree directory
- `branch` - Create new branch in original project directory
- `none` - No git operations, use project as-is

**Worktree Creation:**
- Branch name: activity slug (sanitized activity name)
- Worktree path: `~/emacs-activities/SLUG-YYYY-MM-DD/worktrees/PROJECT-NAME/`
- Command: `git worktree add WORKTREE-PATH -b BRANCH-NAME`

**Branch Creation:**
- Branch name: activity slug (sanitized activity name)
- Created in: original project directory
- Command: `git checkout -b BRANCH-NAME`

**Effective Path:**
- When worktree exists: effective path is worktree path
- When worktree nil: effective path is original project path
- File operations use effective path for matching

### Scenarios

```
WHEN git action is 'worktree
THEN create worktree at ~/emacs-activities/ACTIVITY-SLUG-DATE/worktrees/PROJECT-NAME/
AND create new branch with name = activity slug
AND store :worktree path and :branch name in project plist
AND return result plist with :action, :branch, :worktree, :created

WHEN git action is 'branch
THEN create branch in original project directory
AND checkout new branch
AND store :branch name in project plist (worktree remains nil)
AND return result plist with :action, :branch, :worktree nil, :created

WHEN git action is 'none
THEN perform no git operations
AND store project with :branch nil, :worktree nil
AND return result plist with :action 'none

WHEN git operation fails (non-zero exit, error)
THEN log error message to user
AND return result plist with :created nil
AND continue activity creation (do not block)
```

### Degradation Guarantees

**Failure Handling:**
- Git command failure SHALL NOT block activity creation
- Failed git operations SHALL be logged to user via message
- Result plists SHALL indicate success via `:created` key
- Activity creation SHALL complete with partial success

**Project Validation:**
- Validation checks effective path (worktree if present, else path)
- Invalid projects are flagged but not removed
- User commands like `activities-ext-validate` report validation status
- Resume behavior attempts to open files even if validation warns

---

## Org-Roam Integration

The org-roam integration provides automatic documentation creation and robust file resolution.

### Document Creation

**Filename Pattern:** `YYYYMMDDHHMMSS-ACTIVITY-SLUG.org`
- Timestamp: creation time (format-time-string "%Y%m%d%H%M%S")
- Slug: sanitized activity name (lowercase, hyphens, no special chars)
- Location: `org-roam-directory/SUBDIRECTORY/` (subdirectory from `activities-ext-org-roam-subdirectory`)

**Document Structure:**
```org
#+title: Activity: NAME
#+filetags: :activity:SLUG:
#+property: activity_directory /path/to/activity-dir
#+property: activity_name NAME
#+property: gptel_session_id SESSION-ID  ; if session created
#+property: gptel_session_dir SESSION-DIR

* Overview
* Gptel Sessions
** Session: SESSION-ID  ; if session exists
:PROPERTIES:
:GPTEL_SESSION_ID: id
:GPTEL_SESSION_FILE: file
:GPTEL_SESSION_DIR: dir
:END:
[[file:PATH][Link]]

* Projects
** Project: NAME
:PROPERTIES:
:PROJECT_PATH: path
:PROJECT_NAME: name
:PROJECT_BRANCH: branch  ; if present
:PROJECT_WORKTREE: worktree  ; if present
:END:
[[file:EFFECTIVE-PATH][Link]]

* Tasks
** TODO Task 1
* Notes
* Session Log
```

**Database Integration:**
- After file creation: call `org-roam-db-update-file` to index
- Extract node-id: call `org-roam-node-from-file` to get node
- Store node-id in metadata for stable resolution

### Three-Way Resolution Strategy

**Strategy Order:**
1. **Node-ID lookup** (most stable) - `org-roam-node-from-id`
2. **Cached file path** (fallback) - use stored `:file` path if org-roam unavailable
3. **Search by title** (not yet implemented) - query org-roam-db for matching title

**Scenarios:**

```
WHEN resolving document WITH valid node-id AND org-roam available
THEN query org-roam-node-from-id for current file path
AND if path differs from cached path: mark needs-sync
AND return resolved-file with sync flag

WHEN resolving document WITH node-id AND node-id not found
THEN fallback to cached file path
AND if cached file exists: return it
AND if cached file missing: return nil with 'missing status

WHEN resolving document WITH org-roam unavailable
THEN use cached file path directly (no sync possible)
AND return resolved-file without sync flag

WHEN resolving document WITH file path changed (needs-sync true)
THEN update metadata :file to new path
AND update :last-synced to current-time
AND save metadata back to activity
```

**Sync Trigger Points:**
- On resume: opportunistic sync if needs-sync detected
- On validate: explicit sync with status report
- On open: opportunistic sync if needs-sync detected

**Sync Status Values:**
- `'synced` - file resolved successfully, metadata current
- `'missing` - node-id lookup failed, file not found
- `'unavailable` - org-roam not loaded, cannot sync
- `'no-metadata` - no org-roam data in activity

### Bidirectional Linking

**Activity → Document:**
- Metadata stores node-id and file path
- Activities can open document via `activities-ext-open-document`

**Document → Activity:**
- Document contains `#+property: activity_name NAME`
- Projects stored as org headings with PROPERTIES for programmatic access
- Gptel session stored with properties and file link

---

## Transient UI Behavior

The transient UI provides an interactive workflow for activity creation with project and preset selection.

### Two-Column Interaction Pattern

**Left Column: Project Selection (keys a-z)**
- Each known project assigned a letter key (prefer first letter of name)
- Pressing key toggles project on/off
- Newly added projects use default git action from scope
- Display shows `(✓ W)` for selected with worktree, `(✓ B)` for branch, `(✓ ·)` for none

**Right Column: Selected Projects (keys 1-9, 0)**
- Shows only currently selected projects (up to 10)
- Each project assigned a number key
- Pressing number key cycles git action: worktree → branch → none → worktree
- Display shows `NAME [W]`, `NAME [B]`, or `NAME [·]`

**Key Assignment Algorithm:**
```
FOR each project in projectile-relevant-known-projects:
  - TRY to use first character of project name
  - IF character already assigned: use next available from a-z pool
  - IF pool exhausted: skip project (>26 projects not supported)
  - RESERVE q for quit, 0-9 for Selected Projects column
```

### Scope State Management

**Scope Plist Structure:**
```elisp
(:projects ((path . action) ...)  ; alist of selected projects
 :git-action worktree              ; default action for new selections
 :selected-project nil             ; unused in current implementation
 :activity-name "Name"
 :sanitized-name "name"
 :gptel-preset "programming-assistant")
```

**Scope Persistence:**
- Scope persists across transient refreshes (suffix regeneration)
- Selected projects retain their individual git actions
- Default git action affects only newly selected projects

### Scenarios

```
WHEN pressing project key (left column) AND project not selected
THEN add project to :projects with current :git-action from scope
AND regenerate transient suffixes to show selection

WHEN pressing project key (left column) AND project already selected
THEN remove project from :projects alist
AND regenerate transient suffixes to hide from right column

WHEN pressing number key (right column)
THEN cycle that specific project's action in :projects alist
  worktree → branch → none → worktree
AND regenerate transient suffixes to show new indicator

WHEN pressing RET to create activity
THEN read :projects alist from scope
AND apply each project's specific git action (not default action)
AND create activity with resulting metadata
```

### Gptel Preset Selection

**Display:** Shows currently selected preset with highlight
**Keys:** Letter keys not used by projects (avoid collisions)
**Behavior:** Selecting preset updates `:gptel-preset` in scope
**Integration:** Preset name passed to `jf/gptel-session-create-persistent`

**Availability:** Column only shown if `jf/gptel--list-preset-templates` bound

### Creation Options

**Flags:**
- `--no-gptel` - Skip gptel session creation
- `--no-org-roam` - Skip org-roam document creation

**Creation Order:**
1. Process projects (apply git actions, collect metadata)
2. Create org-roam document (if not skipped)
3. Create gptel session (if not skipped and jf/gptel-session-create-persistent available)
4. Update org-roam doc with gptel session info (if both created)
5. Create activity via `activities-new`
6. Store metadata in activity
7. Set up window layout (doc left, session right)
8. Save activity as default and last

---

## Optional Dependencies

The extensions degrade gracefully when optional dependencies are unavailable.

### Runtime Feature Detection

**Pattern:**
```elisp
(when (fboundp 'optional-function)
  ;; Use optional feature
  (optional-function args))
```

**Checked Dependencies:**
- `projectile-relevant-known-projects` - Project selection in transient
- `org-roam-db-update-file` - Database indexing after doc creation
- `org-roam-node-from-file` - Node-id extraction
- `org-roam-node-from-id` - Three-way resolution strategy
- `jf/gptel-session-create-persistent` - Session creation
- `jf/gptel-session--open-existing` - Session buffer opening
- `jf/gptel--list-preset-templates` - Preset selection in transient
- `recentf-list` - Recent files capture on suspend

**Degradation Behavior:**

```
WHEN projectile unavailable
THEN project selection column not shown in transient

WHEN org-roam unavailable
THEN skip document creation entirely
AND skip three-way resolution (use cached path only)

WHEN gptel session functions unavailable
THEN skip session creation
AND skip session opening on resume

WHEN recentf unavailable
THEN skip recent files capture on suspend
AND skip recent files opening on resume
```

**Gptel Integration Details:**
- Gptel session creation details are specified in gptel OpenSpec specs
- Activities extensions only document the integration contract (session-id, session-file, session-dir plist)
- Session lifecycle (creation, persistence, buffer management) is gptel's responsibility

### Configuration Variables

**Documented in activities.org, not this spec:**
- `activities-ext-auto-open-recent` - Resume behavior setting
- `activities-ext-recent-files-limit` - File capture limit
- `activities-ext-base-directory` - Directory structure root
- `activities-ext-prompt-for-org-roam` - Creation prompt behavior
- `activities-ext-org-roam-subdirectory` - Document location
- `activities-ext--default-git-action` - Transient default action

---

## Integration Summary

**With activities.el:**
- Metadata storage: `activities-activity-etc` slot (`:activity-extensions` key)
- Resume hook: `activities-before-resume-functions`
- Suspend hook: advice on `activities-suspend` (package limitation)
- Activity creation: `activities-new`, `activities-save`
- State access: `activities--current`, `activities-activities`

**With projectile:**
- Project listing: `projectile-relevant-known-projects`
- Runtime check: `(fboundp 'projectile-relevant-known-projects)`

**With org-roam:**
- Database update: `org-roam-db-update-file`
- Node resolution: `org-roam-node-from-file`, `org-roam-node-from-id`
- Directory: `org-roam-directory`
- Runtime checks for all functions

**With gptel sessions:**
- Session creation: `jf/gptel-session-create-persistent`
- Session opening: `jf/gptel-session--open-existing`
- Preset listing: `jf/gptel--list-preset-templates`
- Details: see gptel OpenSpec specs (not duplicated here)

**With recentf:**
- File list: `recentf-list` (standard Emacs)
- Runtime check: `(bound-and-true-p recentf-list)`
