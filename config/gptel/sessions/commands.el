;;; commands.el --- GPTEL Session Commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Interactive commands for gptel session management.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-metadata)
(require 'jf-gptel-scope-commands)

(defun jf/gptel--generate-preset-plist (session-name backend model)
  "Generate preset plist for session SESSION-NAME.
Uses BACKEND and MODEL, with current gptel values as fallbacks.
System message defaults to empty, tools default to empty list.
Tool result persistence is enabled by default via include-tool-results."
  (list :description (format "Session preset for %s" session-name)
        :backend (or backend gptel-backend)
        :model (or model gptel-model)
        :system ""  ; Empty - use gptel default
        :temperature (or (bound-and-true-p gptel-temperature) 1.0)
        :include-tool-results t  ; Enable native tool persistence
        :tools '()))  ; Empty - user adds via preset.json editing

(defun jf/gptel--write-preset-file (session-dir preset-plist)
  "Write PRESET-PLIST to preset.json in SESSION-DIR.
Converts backend objects to names, converts keywords to strings."
  (require 'json)
  (let* ((backend (plist-get preset-plist :backend))
         (backend-name (if (gptel-backend-p backend)
                          (gptel-backend-name backend)
                        backend))
         (model (plist-get preset-plist :model))
         (model-name (if (symbolp model) (symbol-name model) model))
         (preset-alist `(("description" . ,(plist-get preset-plist :description))
                        ("backend" . ,backend-name)
                        ("model" . ,model-name)
                        ("system" . ,(plist-get preset-plist :system))
                        ("temperature" . ,(plist-get preset-plist :temperature))
                        ("include-tool-results" . ,(plist-get preset-plist :include-tool-results))
                        ("tools" . ,(plist-get preset-plist :tools))))
         (preset-file (expand-file-name "preset.json" session-dir)))
    (with-temp-file preset-file
      (insert (json-encode preset-alist)))
    (jf/gptel--log 'info "Created preset file: %s" preset-file)))

(defun jf/gptel--load-preset-from-file (session-dir)
  "Load preset from SESSION-DIR/preset.json and return plist.
Converts backend names to objects, converts JSON keys to keywords."
  (require 'json)
  (let* ((preset-file (expand-file-name "preset.json" session-dir)))
    (unless (file-exists-p preset-file)
      (error "Preset file not found: %s" preset-file))
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (preset-alist (json-read-file preset-file))
           (backend-name (alist-get "backend" preset-alist nil nil #'equal))
           (backend (alist-get backend-name gptel--known-backends nil nil #'equal))
           (model-name (alist-get "model" preset-alist nil nil #'equal))
           (model (if (stringp model-name) (intern model-name) model-name)))
      (list :description (alist-get "description" preset-alist nil nil #'equal)
            :backend backend
            :model model
            :system (alist-get "system" preset-alist "" nil #'equal)
            :temperature (alist-get "temperature" preset-alist 1.0 nil #'equal)
            :include-tool-results (alist-get "include-tool-results" preset-alist t nil #'equal)
            :tools (alist-get "tools" preset-alist nil nil #'equal)))))

(defun jf/gptel-scope--template-programming-assistant (session-name backend model)
  "Generate programming assistant preset for SESSION-NAME.
Uses BACKEND and MODEL (defaults to Claude Opus 4.5).
Includes scoped filesystem tools, projectile navigation, and detailed system prompt."
  (let ((backend-obj (or backend (alist-get "Claude" gptel--known-backends nil nil #'equal)))
        (model-sym (or model 'claude-opus-4-5-20251101))
        (system-prompt "You are a programming assistant running inside Emacs. Your purpose is to help with software development tasks within the constraints of a scoped permission system.

## Environment

You are running in Emacs via gptel with access to:
- Scoped filesystem tools (read_file, write_file_in_scope, edit_file_in_scope)
- Projectile project navigation tools
- Scope management tools

## Scoped Tools System

All write/edit operations are governed by a scope plan document (scope-plan.yml) that defines:
- Which tools are enabled (allowed: true/false)
- What file patterns each tool can access
- What patterns are explicitly denied

**Key principles**:
1. **Scope plan is authority**: The scope-plan.yml file determines what operations are permitted
2. **Tool-level permissions**: Each tool (write_file_in_scope, edit_file_in_scope, etc.) has independent configuration
3. **Deny patterns override**: Explicit denials take precedence over allows
4. **Git-safe editing**: edit_file_in_scope only works on git-tracked files (prevents editing node_modules/, build artifacts, etc.)

**When you encounter scope violations**:
1. Use `inspect_scope_plan()` to see current permissions
2. Check if alternative paths match existing patterns
3. If needed, use `request_scope_expansion(tool_name, patterns, justification)` to ask user for approval
4. Be specific in justification—explain WHAT you're trying to accomplish and WHY

## Projectile Navigation

Projectile provides project-aware navigation. A \"project\" is typically a git repository or directory with project markers.

**Note**: When users refer to \"repo\", \"repository\", \"codebase\", or \"app\", they mean a projectile project.

**Critical workflow**:
1. **Start with discovery**: `list_known_projects()` shows all available projects
2. **Get context**: `get_project_info(directory)` shows project type, VCS, etc.
3. **Navigate**: Use project-specific tools with the project directory path

**Important**: When running in a gptel buffer (not visiting a file), there is NO current project context. Always use `list_known_projects()` first to discover available projects, then pass the project path explicitly to other projectile tools.

**Key tools**:
- `list_known_projects()`: Discover available projects (use this FIRST)
- `get_project_info(directory)`: Get project metadata
- `list_project_files(directory, limit, filter_pattern)`: List files (respects .gitignore)
- `search_project_content(directory, search_term, file_pattern)`: Find code (more efficient than reading every file)
- `list_test_files(directory)`: Find all tests
- `find_related_test(directory, file_path)`: Find test for specific file

**Efficient search patterns**:
- Use `search_project_content` to find where functions/classes are defined
- Use filter_pattern with `list_project_files` to find specific file types
- Use projectile tools instead of reading every file

## Scope and Projectile Integration

When a session is created with projectile projects selected:
- Scope plan restricts operations to those project directories
- read_file patterns limit reading to project paths
- write/edit patterns scope modifications to project paths
- Git-safe editing prevents modifying ignored files

**Workflow**:
1. Check scope plan: `inspect_scope_plan()` to see what projects are in scope
2. Navigate with projectile: Find files, search content, understand structure
3. Read within scope: Use `read_file` on files within allowed patterns
4. Modify within scope: Use `edit_file_in_scope` (git-tracked only) or `write_file_in_scope`
5. Request expansion: Use `request_scope_expansion` if you need access outside current scope

## Best Practices

1. **Inspect before acting**: Call `inspect_scope_plan()` early to understand your permissions
2. **Use projectile for discovery**: Don't guess at file locations—use search and list tools
3. **Respect scope boundaries**: Work within approved patterns when possible
4. **Request thoughtfully**: When requesting scope expansion, explain the specific task
5. **Git-safe editing**: Remember that `edit_file_in_scope` only works on git-tracked files
6. **Efficient search**: Use `search_project_content` instead of reading many files

## Error Handling

When a scoped tool returns an error:
- **scope_violation**: Operation not in allowed patterns → inspect scope, request expansion if needed
- **file_not_git_tracked**: File not tracked by git → use `write_file_in_scope` instead, or ask user to add file to git
- **no_scope_plan**: No scope plan exists → notify user to create one
- **tool_not_configured**: Tool not in scope plan → request scope expansion with specific tool name

Your goal is to assist with programming tasks effectively while respecting the permission boundaries defined by the scope plan.")
        (tools-list '("read_file"
                     "write_file_in_scope"
                     "edit_file_in_scope"
                     "request_scope_expansion"
                     "inspect_scope_plan"
                     "list_known_projects"
                     "get_project_info"
                     "list_project_files"
                     "list_project_directories"
                     "expand_project_path"
                     "search_project_content"
                     "list_test_files"
                     "find_related_test"
                     "find_related_files")))
    (list :description (format "Programming assistant preset for %s" session-name)
          :backend backend-obj
          :model model-sym
          :system system-prompt
          :temperature 1.0
          :include-tool-results t
          :tools tools-list)))

(defun jf/gptel--apply-session-preset (preset-plist)
  "Apply PRESET-PLIST to current buffer buffer-locally.
Includes tools from preset and gptel-include-tool-results for native tool persistence."
  (let ((backend (plist-get preset-plist :backend))
        (model (plist-get preset-plist :model))
        (system (plist-get preset-plist :system))
        (temperature (plist-get preset-plist :temperature))
        (include-tool-results (plist-get preset-plist :include-tool-results))
        (tools (plist-get preset-plist :tools)))
    (when backend
      (setq-local gptel-backend backend))
    (when model
      (setq-local gptel-model model))
    (when system
      (setq-local gptel--system-message system))
    (when temperature
      (setq-local gptel-temperature temperature))
    (when (not (null include-tool-results))
      (setq-local gptel-include-tool-results include-tool-results))
    ;; Apply tools from preset
    (when tools
      (let ((resolved-tools
             (cl-loop for tool-name in tools
                      for tool = (gptel-get-tool tool-name)
                      when tool collect tool
                      else do (jf/gptel--log 'warn "Tool not found: %s" tool-name))))
        (when resolved-tools
          (setq-local gptel-tools resolved-tools)
          (jf/gptel--log 'info "Applied %d tools from preset" (length resolved-tools)))))
    (jf/gptel--log 'info "Applied session preset (include-tool-results: %s)" include-tool-results)))

(defun jf/gptel-persistent-session (session-name &optional backend model)
  "Create a new persistent gptel programming session named SESSION-NAME.
Uses programming assistant preset with scoped tools and projectile integration.

Optional BACKEND and MODEL default to Claude Opus 4.5.
Prompts user to select projectile projects (0 or more).
If projects selected, creates project-aware scope plan.
Otherwise creates deny-all scope plan.

Creates session directory, metadata, preset, scope plan, and opens session buffer.
The session will auto-save to ~/.gptel/sessions/SESSION-NAME-TIMESTAMP/session.md"
  (interactive "sSession name: ")
  (let* ((session-id (jf/gptel--generate-session-id session-name))
         (session-dir (jf/gptel--create-session-directory session-id))
         (session-file (jf/gptel--context-file-path session-dir))
         (backend (or backend (alist-get "Claude" gptel--known-backends nil nil #'equal)))
         (model (or model 'claude-opus-4-5-20251101))
         (backend-name (gptel-backend-name backend))
         (model-name (if (symbolp model) (symbol-name model) model))
         ;; Project selection
         (selected-projects (when (y-or-n-p "Select projectile projects for this session? ")
                             (jf/gptel--select-projects)))
         (project-names (when selected-projects
                         (mapcar #'jf/gptel--project-display-name selected-projects))))

    ;; Create metadata with project information
    (let ((metadata (jf/gptel--create-metadata session-dir session-id model backend-name)))
      ;; Add project fields to metadata if projects selected
      (when selected-projects
        (setq metadata (plist-put metadata :projects selected-projects))
        (setq metadata (plist-put metadata :project-names project-names)))

      (jf/gptel--write-metadata session-dir metadata)
      (jf/gptel--register-session session-dir metadata nil session-id))

    ;; Create session file with initial content
    (with-temp-file session-file
      (insert "# " session-name "\n\n"))

    ;; Open session file in buffer
    (let ((buffer (find-file session-file)))
      (with-current-buffer buffer
        ;; Enable gptel-mode
        (unless gptel-mode
          (gptel-mode 1))

        ;; Set session variables
        (setq-local jf/gptel--session-id session-id)
        (setq-local jf/gptel--session-dir session-dir)

        ;; Set backend and model
        (setq-local gptel-backend backend)
        (setq-local gptel-model model)

        ;; Enable auto-save
        (setq-local jf/gptel-autosave-enabled t)

        ;; Update registry with buffer
        (jf/gptel--update-session-buffer session-id buffer)

        ;; Create project-aware or deny-all scope plan
        (let ((scope-yaml (if selected-projects
                             (jf/gptel--generate-scope-plan-yaml
                              session-id "project-aware" selected-projects)
                           (jf/gptel--generate-scope-plan-yaml session-id "deny-all")))
              (scope-file (expand-file-name "scope-plan.yml" session-dir)))
          (with-temp-file scope-file
            (insert scope-yaml))
          (jf/gptel--log 'info "Created scope plan: %s" scope-file))

        ;; Create and apply programming assistant preset
        (let ((preset-plist (jf/gptel-scope--template-programming-assistant
                            session-name backend model)))
          ;; Write preset to file
          (jf/gptel--write-preset-file session-dir preset-plist)
          ;; Apply preset to buffer
          (jf/gptel--apply-session-preset preset-plist))

        (jf/gptel--log 'info "Created programming session: %s%s"
                      session-id
                      (if selected-projects
                          (format " with %d project(s)" (length selected-projects))
                        ""))
        (message "Created programming session: %s\nDirectory: %s%s\nModel: Claude Opus 4.5\nTools: Scoped filesystem + Projectile navigation"
                session-name
                session-dir
                (if project-names
                    (format "\nProjects: %s" (string-join project-names ", "))
                  ""))

        buffer))))

(defun jf/gptel-list-sessions ()
  "List all persistent gptel sessions."
  (interactive)
  (let ((sessions (jf/gptel--all-sessions)))
    (if (null sessions)
        (message "No persistent sessions found")
      (with-current-buffer (get-buffer-create "*GPTEL Sessions*")
        (erase-buffer)
        (insert "GPTEL Persistent Sessions\n")
        (insert "=========================\n\n")
        (dolist (session sessions)
          (let ((session-id (plist-get session :session-id))
                (created (plist-get session :created))
                (metadata (plist-get session :metadata))
                (buffer (plist-get session :buffer)))
            (insert (format "• %s\n" session-id))
            (when created
              (insert (format "  Created: %s\n" created)))
            (when metadata
              (insert (format "  Backend: %s\n" (plist-get metadata :backend)))
              (insert (format "  Model: %s\n" (plist-get metadata :model))))
            (when buffer
              (insert (format "  Active in: %s\n" (buffer-name buffer))))
            (insert "\n")))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun jf/gptel-resume-session (session-id)
  "Resume persistent session SESSION-ID.
Opens the session's context file and restores session state."
  (interactive
   (list (completing-read "Resume session: "
                         (mapcar (lambda (s) (plist-get s :session-id))
                                (jf/gptel--all-sessions))
                         nil t)))
  (let* ((session (jf/gptel-session-find session-id))
         (session-dir (plist-get session :directory))
         (session-file (jf/gptel--context-file-path session-dir))
         (metadata (plist-get session :metadata)))

    (unless (file-exists-p session-file)
      (user-error "Session file not found: %s" session-file))

    ;; Open session file
    (let ((buffer (find-file session-file)))
      (with-current-buffer buffer
        ;; Enable gptel-mode if needed
        (unless gptel-mode
          (gptel-mode 1))

        ;; Set session variables
        (setq-local jf/gptel--session-id session-id)
        (setq-local jf/gptel--session-dir session-dir)

        ;; Restore backend/model from metadata
        (when-let ((backend-name (plist-get metadata :backend)))
          (setq-local gptel-backend (alist-get backend-name gptel--known-backends
                                              nil nil #'equal)))
        (when-let ((model-name (plist-get metadata :model)))
          (setq-local gptel-model (if (stringp model-name)
                                     (intern model-name)
                                   model-name)))

        ;; Enable auto-save
        (setq-local jf/gptel-autosave-enabled t)

        ;; Update registry
        (jf/gptel--update-session-buffer session-id buffer)

        (jf/gptel--log 'info "Resumed session: %s" session-id)
        (message "Resumed session: %s" session-id)

        buffer))))

(defun jf/gptel-refresh-sessions ()
  "Refresh the session registry by scanning session directories.
Useful if sessions were created outside Emacs or after startup."
  (interactive)
  (jf/gptel--init-registry)
  (message "Refreshed session registry: %d sessions found"
          (jf/gptel--session-count)))

(defun jf/gptel-session-diagnostics ()
  "Show diagnostic information about current session and advice installation."
  (interactive)
  (with-current-buffer (get-buffer-create "*GPTEL Session Diagnostics*")
    (erase-buffer)
    (insert "GPTEL Session Diagnostics\n")
    (insert "=========================\n\n")

    ;; Current buffer info
    (insert "Current Buffer:\n")
    (insert (format "  Name: %s\n" (buffer-name)))
    (insert (format "  Session ID: %s\n" (or jf/gptel--session-id "NOT SET")))
    (insert (format "  Session Dir: %s\n" (or jf/gptel--session-dir "NOT SET")))
    (insert (format "  GPTel Mode: %s\n" (if (bound-and-true-p gptel-mode) "enabled" "disabled")))
    (insert (format "  Auto-save: %s\n" (if jf/gptel-autosave-enabled "enabled" "disabled")))
    (insert "\n")

    ;; Advice installation
    (insert "Advice Installation:\n")
    (insert (format "  gptel--fsm-transition (tool logging): %s\n"
                    (if (advice-member-p #'jf/gptel--advice-log-fsm-transition 'gptel--fsm-transition)
                        "INSTALLED"
                      "NOT INSTALLED")))
    (insert (format "  gptel--set-with-scope (system prompt): %s\n"
                    (if (and (fboundp 'gptel--set-with-scope)
                            (advice-member-p #'jf/gptel--advice-track-system-prompt 'gptel--set-with-scope))
                        "INSTALLED"
                      "NOT INSTALLED")))
    (insert "\n")

    ;; Log level
    (insert "Configuration:\n")
    (insert (format "  Log Level: %s\n" jf/gptel-log-level))
    (insert (format "  Sessions Directory: %s\n" jf/gptel-sessions-directory))
    (insert (format "  Registry Count: %d\n" (jf/gptel--session-count)))

    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun jf/gptel--build-session-tree (sessions)
  "Build tree structure from flat SESSIONS list.
SESSIONS is a list of plists from jf/gptel--find-all-sessions-recursive.
Returns tree with :children property added to each node."
  (let ((by-parent (make-hash-table :test 'equal))
        (tree nil))
    ;; Group sessions by parent-path
    (dolist (session sessions)
      (let ((parent-path (plist-get session :parent-path)))
        (if parent-path
            (push session (gethash parent-path by-parent))
          (push session tree))))
    ;; Attach children to their parents
    (dolist (session sessions)
      (let* ((path (plist-get session :path))
             (children (gethash path by-parent)))
        (when children
          (plist-put session :children (nreverse children)))))
    (nreverse tree)))

(defun jf/gptel--render-session-tree (nodes depth)
  "Render session tree NODES at DEPTH with indentation.
Inserts formatted text into current buffer."
  (dolist (node nodes)
    (let* ((indent (make-string (* depth 2) ?\s))
           (prefix (if (zerop depth) "▸ " "└─ "))
           (session-id (plist-get node :id))
           (path (plist-get node :path))
           (metadata-file (jf/gptel--metadata-file-path path))
           (metadata (when (file-exists-p metadata-file)
                      (jf/gptel--read-metadata path)))
           (type-str (if (jf/gptel--metadata-is-subagent-p metadata)
                        (format " [%s subagent]" (plist-get metadata :agent-type))
                      ""))
           (created (when metadata
                     (plist-get metadata :created))))
      (insert (format "%s%s%s%s\n" indent prefix session-id type-str))
      (when created
        (insert (format "%s   Created: %s\n" indent created)))
      (when-let ((children (plist-get node :children)))
        (jf/gptel--render-session-tree children (1+ depth))))))

(defun jf/gptel-browse-sessions-hierarchical ()
  "Browse sessions in hierarchical tree view showing subagent relationships."
  (interactive)
  (let* ((sessions (jf/gptel--find-all-sessions-recursive))
         (tree (jf/gptel--build-session-tree sessions)))
    (with-current-buffer (get-buffer-create "*GPTEL Sessions*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# GPTEL Persistent Sessions\n\n")
        (if (null tree)
            (insert "No sessions found.\n\n")
          (insert (format "Total sessions: %d (including subagents)\n\n" (length sessions)))
          (jf/gptel--render-session-tree tree 0))
        (goto-char (point-min))
        (special-mode))
      (pop-to-buffer (current-buffer)))))

(defun jf/gptel-resume-subagent ()
  "Resume work in a subagent session buffer."
  (interactive)
  (let* ((sessions (jf/gptel--find-all-sessions-recursive))
         (subagents (seq-filter
                    (lambda (s)
                      (> (plist-get s :depth) 0))
                    sessions))
         (choices (mapcar
                  (lambda (s)
                    (let* ((path (plist-get s :path))
                           (metadata (jf/gptel--read-metadata path))
                           (parent-id (jf/gptel--metadata-get-parent-id metadata))
                           (agent-type (plist-get metadata :agent-type)))
                      (cons (format "[%s] %s (%s)"
                              parent-id
                              (plist-get s :id)
                              (or agent-type "unknown"))
                           s)))
                  subagents)))
    (if (null choices)
        (message "No subagent sessions found")
      (let* ((choice (completing-read "Resume subagent: " choices nil t))
             (session (alist-get choice choices nil nil #'equal))
             (session-dir (plist-get session :path))
             (session-file (jf/gptel--context-file-path session-dir))
             (session-id (plist-get session :id)))
        (unless (file-exists-p session-file)
          (user-error "Session file not found: %s" session-file))
        ;; Open session file
        (let ((buffer (find-file session-file)))
          (with-current-buffer buffer
            ;; Enable gptel-mode if needed
            (unless gptel-mode
              (gptel-mode 1))
            ;; Set session variables
            (setq-local jf/gptel--session-id session-id)
            (setq-local jf/gptel--session-dir session-dir)
            ;; Load and apply preset
            (when-let ((preset-plist (jf/gptel--load-preset-from-file session-dir)))
              (jf/gptel--apply-session-preset preset-plist))
            ;; Enable auto-save
            (setq-local jf/gptel-autosave-enabled t)
            ;; Update registry
            (when-let ((metadata (jf/gptel--read-metadata session-dir)))
              (jf/gptel--register-session session-dir metadata buffer session-id))
            (jf/gptel--log 'info "Resumed subagent session: %s" session-id)
            (message "Resumed subagent: %s" session-id)
            buffer))))))

(provide 'gptel-session-commands)
;;; commands.el ends here
