;; Dependencies


;; [[file:scope-commands.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-core)
;; Dependencies:1 ends here

;; Deny-All Template (Secure Default)

;; Nothing allowed by default. User must explicitly add patterns.


;; [[file:scope-commands.org::*Deny-All Template (Secure Default)][Deny-All Template (Secure Default):1]]
(defun jf/gptel-scope--template-deny-all (session-id &optional type parent-id preset-tools worktree-paths)
  "Secure default template - nothing allowed, user must add patterns.
Recommended for new sessions where you want explicit control.
Optional TYPE, PARENT-ID for agent sessions.
Optional PRESET-TOOLS filters which tools are included.
Optional WORKTREE-PATHS adds activity worktree isolation."
  (let* ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
         (agent-fields (when type
                         (concat
                          (format "type: \"%s\"\n" type)
                          (when parent-id
                            (format "parent_session_id: \"%s\"\n" parent-id)))))
         ;; Format worktree paths for read_file patterns
         (worktree-patterns (when worktree-paths
                             (mapconcat (lambda (path)
                                         (format "      - \"%s/**\"" path))
                                       worktree-paths
                                       "\n")))
         ;; Base repo deny pattern to block access to straight.el repos
         (base-repo-deny "      - \"**/runtime/straight/repos/**\"\n")
         ;; Decide if we should include each tool based on preset
         (include-tool-p (lambda (tool-name)
                          (or (null preset-tools)
                              (member tool-name preset-tools)))))
    (concat
     (format "version: \"2.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
%sdefault_policy: deny

tools:
"
             session-id
             timestamp
             timestamp
             (or agent-fields ""))
     ;; read_file - always include if in preset (or no preset filtering)
     (when (funcall include-tool-p "read_file")
       (format "  read_file:
    allowed: true
    patterns:
%s    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
%s      - \"**/.env\"

"
               (if worktree-patterns
                   (concat worktree-patterns "\n")
                 "      - \"/**\"\n")
               (if worktree-paths base-repo-deny "")))
     ;; write_file_in_scope
     (when (funcall include-tool-p "write_file_in_scope")
       (format "  write_file_in_scope:
    allowed: %s
    patterns:
%s    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/node_modules/**\"
      - \"**/.env\"

"
               (if worktree-paths "true" "false")
               (if worktree-patterns
                   (concat worktree-patterns "\n")
                 "      []\n")))
     ;; edit_file_in_scope
     (when (funcall include-tool-p "edit_file_in_scope")
       (format "  edit_file_in_scope:
    allowed: %s
    patterns:
%s    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/node_modules/**\"
      - \"**/.env\"

"
               (if worktree-paths "true" "false")
               (if worktree-patterns
                   (concat worktree-patterns "\n")
                 "      []\n")))
     ;; Roam tools (if in preset)
     (when (funcall include-tool-p "create_roam_node_in_scope")
       "  create_roam_node_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      []

")
     (when (funcall include-tool-p "add_roam_tags_in_scope")
       "  add_roam_tags_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      []

")
     (when (funcall include-tool-p "link_roam_nodes_in_scope")
       "  link_roam_nodes_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      []

")
     ;; run_approved_command (if in preset)
     (when (funcall include-tool-p "run_approved_command")
       "  run_approved_command:
    allowed: false
    patterns:
      []
    deny_patterns:
      - \"rm -rf\"
      - \"sudo\"
      - \"chmod\"
      - \"chown\"

"))))
;; Deny-All Template (Secure Default):1 ends here

;; Codebase Read Template

;; Read operations allowed, limited safe shell commands for exploration.


;; [[file:scope-commands.org::*Codebase Read Template][Codebase Read Template:1]]
(defun jf/gptel-scope--template-codebase-read (session-id &optional type parent-id preset)
  "Read-only exploration template.
Allows safe shell commands for code exploration (ls, find, grep, git log).
No write operations allowed - LLM must request permission.
Optional TYPE, PARENT-ID, and PRESET for agent sessions."
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (agent-fields (when type
                          (concat
                           (format "type: \"%s\"\n" type)
                           (when parent-id
                             (format "parent_session_id: \"%s\"\n" parent-id))
                           (when preset
                             (format "preset: \"%s\"\n" preset))))))
    (format "version: \"2.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
%sdefault_policy: deny

tools:
  read_file:
    allowed: true
    patterns:
      - \"/**\"
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"

  write_file_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/node_modules/**\"
      - \"**/.env\"

  edit_file_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/node_modules/**\"
      - \"**/.env\"

  create_roam_node_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      []

  add_roam_tags_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      []

  link_roam_nodes_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      []

  run_approved_command:
    allowed: true
    patterns:
      - \"ls\"
      - \"find\"
      - \"grep\"
      - \"git\"
      - \"cat\"
      - \"head\"
      - \"tail\"
    deny_patterns:
      - \"rm\"
      - \"sudo\"
      - \"chmod\"
      - \"chown\"
      - \"mv\"
      - \"cp\"

"
            session-id
            timestamp
            timestamp
            (or agent-fields ""))))
;; Codebase Read Template:1 ends here

;; Org-Roam Safe Template

;; Allow org-roam operations in gptel/ subdirectory with gptel tag.


;; [[file:scope-commands.org::*Org-Roam Safe Template][Org-Roam Safe Template:1]]
(defun jf/gptel-scope--template-org-roam-safe (session-id &optional type parent-id preset)
  "Org-roam safe template.
Allows creating and linking nodes in gptel/ subdirectory with gptel tag.
Safe for LLM-generated notes that are clearly separated from personal notes.
Optional TYPE, PARENT-ID, and PRESET for agent sessions."
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (agent-fields (when type
                          (concat
                           (format "type: \"%s\"\n" type)
                           (when parent-id
                             (format "parent_session_id: \"%s\"\n" parent-id))
                           (when preset
                             (format "preset: \"%s\"\n" preset))))))
    (format "version: \"2.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
%sdefault_policy: deny

tools:
  read_file:
    allowed: true
    patterns:
      - \"/**\"
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"

  write_file_in_scope:
    allowed: true
    patterns:
      - \"%s/gptel/**/*.org\"
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"

  edit_file_in_scope:
    allowed: true
    patterns:
      - \"%s/gptel/**/*.org\"
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"

  create_roam_node_in_scope:
    allowed: true
    patterns:
      - \"subdirectory:gptel/**\"
      - \"tag:gptel\"
    deny_patterns:
      []

  add_roam_tags_in_scope:
    allowed: true
    patterns:
      - \"tag:gptel\"
    deny_patterns:
      []

  link_roam_nodes_in_scope:
    allowed: true
    patterns:
      - \"node_id:*\"
    deny_patterns:
      []

  run_approved_command:
    allowed: true
    patterns:
      - \"ls\"
      - \"find\"
      - \"grep\"
    deny_patterns:
      - \"rm\"
      - \"sudo\"

"
            session-id
            timestamp
            timestamp
            (or agent-fields "")
            org-roam-directory
            org-roam-directory)))
;; Org-Roam Safe Template:1 ends here

;; Permissive Template

;; Broad access for trusted workflows. Use with caution.


;; [[file:scope-commands.org::*Permissive Template][Permissive Template:1]]
(defun jf/gptel-scope--template-permissive (session-id &optional type parent-id preset)
  "Permissive template - broad access.
USE WITH CAUTION: Allows wide filesystem access and many shell commands.
Only use when you trust the LLM workflow and want minimal restrictions.
Optional TYPE, PARENT-ID, and PRESET for agent sessions."
  (let ((project-root (or (projectile-project-root) default-directory))
        (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (agent-fields (when type
                          (concat
                           (format "type: \"%s\"\n" type)
                           (when parent-id
                             (format "parent_session_id: \"%s\"\n" parent-id))
                           (when preset
                             (format "preset: \"%s\"\n" preset))))))
    (format "version: \"2.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
%sdefault_policy: deny

tools:
  read_file:
    allowed: true
    patterns:
      - \"/**\"
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"

  write_file_in_scope:
    allowed: true
    patterns:
      - \"%s/**\"
      - \"/tmp/**\"
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/node_modules/**\"
      - \"**/.env\"

  edit_file_in_scope:
    allowed: true
    patterns:
      - \"%s/**\"
      - \"/tmp/**\"
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/node_modules/**\"
      - \"**/.env\"

  create_roam_node_in_scope:
    allowed: true
    patterns:
      - \"subdirectory:**\"
      - \"tag:*\"
    deny_patterns:
      []

  add_roam_tags_in_scope:
    allowed: true
    patterns:
      - \"tag:*\"
    deny_patterns:
      []

  link_roam_nodes_in_scope:
    allowed: true
    patterns:
      - \"node_id:*\"
    deny_patterns:
      []

  run_approved_command:
    allowed: true
    patterns:
      - \"ls\"
      - \"find\"
      - \"grep\"
      - \"git\"
      - \"cat\"
      - \"head\"
      - \"tail\"
      - \"npm\"
      - \"node\"
      - \"python\"
    deny_patterns:
      - \"rm -rf\"
      - \"sudo\"

"
            session-id
            timestamp
            timestamp
            (or agent-fields "")
            project-root
            project-root)))
;; Permissive Template:1 ends here

;; Project-Aware Template

;; Allows read/write/edit within selected projectile projects with git-safe defaults.


;; [[file:scope-commands.org::*Project-Aware Template][Project-Aware Template:1]]
(defun jf/gptel-scope--template-project-aware (session-id project-roots &optional type parent-id preset)
  "Project-aware scope template.
Allows read/write/edit operations within PROJECT-ROOTS only.
Blocks sensitive paths (.git, .env, runtime, node_modules).

PROJECT-ROOTS is a list of absolute project directory paths.
Optional TYPE, PARENT-ID, and PRESET for agent sessions.

This is 'git-safe':
- READ: Only files within selected project directories
- WRITE: New files anywhere in project patterns
- EDIT: Only git-tracked files (enforced by edit_file_in_scope tool)

The git-tracked requirement for editing prevents accidental modification
of ignored files like node_modules/, build artifacts, etc."
  (let ((project-patterns (jf/gptel--format-project-patterns project-roots))
        (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (agent-fields (when type
                          (concat
                           (format "type: \"%s\"\n" type)
                           (when parent-id
                             (format "parent_session_id: \"%s\"\n" parent-id))
                           (when preset
                             (format "preset: \"%s\"\n" preset))))))
    (format "version: \"2.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
%sdefault_policy: deny

# This scope plan grants access to %d project(s):
%s

tools:
  read_file:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  write_file_in_scope:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  edit_file_in_scope:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  list_project_files:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  list_project_directories:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  search_project_content:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  list_test_files:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  find_related_test:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  find_related_files:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  check_ggtags_project:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  find_definition:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  find_references:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  find_symbol:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  create_ggtags_project:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  update_ggtags_project:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  explain_ggtags_indexing:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  get_node_at_position:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  get_node_info:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  get_node_context:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  get_syntax_tree:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  list_functions:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  list_classes:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  list_imports:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  extract_definition:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  query_nodes:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  find_nodes_by_type:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  find_nodes_in_range:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  get_scope_structure:
    allowed: true
    patterns:%s
    deny_patterns:
      - \"**/.git/**\"
      - \"**/runtime/**\"
      - \"**/.env\"
      - \"**/node_modules/**\"

  create_roam_node_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      []

  add_roam_tags_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      []

  link_roam_nodes_in_scope:
    allowed: false
    patterns:
      []
    deny_patterns:
      []

  run_approved_command:
    allowed: true
    patterns:
      - \"ls\"
      - \"find\"
      - \"grep\"
      - \"git\"
    deny_patterns:
      - \"rm -rf\"
      - \"sudo\"
      - \"chmod\"
      - \"chown\"

"
            session-id
            timestamp
            timestamp
            (or agent-fields "")
            (length project-roots)
            (mapconcat (lambda (root)
                        (format "#   - %s (%s)"
                               root
                               (jf/gptel--project-display-name root)))
                      project-roots
                      "\n")
            project-patterns  ; read_file patterns
            project-patterns  ; write_file_in_scope patterns
            project-patterns  ; edit_file_in_scope patterns
            project-patterns  ; list_project_files patterns
            project-patterns  ; list_project_directories patterns
            project-patterns  ; search_project_content patterns
            project-patterns  ; list_test_files patterns
            project-patterns  ; find_related_test patterns
            project-patterns  ; find_related_files patterns
            project-patterns  ; check_ggtags_project patterns
            project-patterns  ; find_definition patterns
            project-patterns  ; find_references patterns
            project-patterns  ; find_symbol patterns
            project-patterns  ; create_ggtags_project patterns
            project-patterns  ; update_ggtags_project patterns
            project-patterns  ; explain_ggtags_indexing patterns
            project-patterns  ; get_node_at_position patterns
            project-patterns  ; get_node_info patterns
            project-patterns  ; get_node_context patterns
            project-patterns  ; get_syntax_tree patterns
            project-patterns  ; list_functions patterns
            project-patterns  ; list_classes patterns
            project-patterns  ; list_imports patterns
            project-patterns  ; extract_definition patterns
            project-patterns  ; query_nodes patterns
            project-patterns  ; find_nodes_by_type patterns
            project-patterns  ; find_nodes_in_range patterns
            project-patterns)))  ; get_scope_structure patterns
;; Project-Aware Template:1 ends here

;; Project Selection

;; Interactive selection of 0 or more projectile projects.


;; [[file:scope-commands.org::*Project Selection][Project Selection:1]]
(defun jf/gptel--select-projects ()
  "Interactively select 0 or more projectile projects.
Returns list of absolute project root paths."
  (let ((projects (projectile-known-projects)))
    (if (null projects)
        (progn
          (message "No known projectile projects found")
          nil)
      (let* ((selection (completing-read-multiple
                        "Select projects (RET when done): "
                        projects
                        nil t)))
        (when selection
          (mapcar #'expand-file-name selection))))))
;; Project Selection:1 ends here

;; Project Display Name

;; Extract display name from project root path.


;; [[file:scope-commands.org::*Project Display Name][Project Display Name:1]]
(defun jf/gptel--project-display-name (project-root)
  "Extract display name from PROJECT-ROOT path.
Uses projectile-project-name if available, else basename."
  (condition-case nil
      (let ((default-directory project-root))
        (projectile-project-name))
    (error (file-name-nondirectory (directory-file-name project-root)))))
;; Project Display Name:1 ends here

;; Format Project Patterns

;; Format project roots as YAML pattern list for scope plans.


;; [[file:scope-commands.org::*Format Project Patterns][Format Project Patterns:1]]
(defun jf/gptel--format-project-patterns (project-roots)
  "Format PROJECT-ROOTS as YAML pattern list.
Returns indented YAML list string like:
      - \"/path/to/project1/**\"
      - \"/path/to/project2/**\""
  (if (null project-roots)
      "[]"
    (concat "\n"
            (mapconcat (lambda (root)
                        (format "      - \"%s/**\""
                               (directory-file-name root)))
                      project-roots
                      "\n"))))
;; Format Project Patterns:1 ends here

;; Parse Preset Tools

;; Extract tool list from preset file for scope plan filtering.


;; [[file:scope-commands.org::*Parse Preset Tools][Parse Preset Tools:1]]
(defun jf/gptel-scope--parse-preset-tools (branch-dir)
  "Extract enabled tools from preset file in BRANCH-DIR.

Reads preset.md or preset.org from BRANCH-DIR, parses YAML frontmatter
or org properties to extract :tools field.

Returns list of tool name strings, or nil if:
- Preset file doesn't exist
- File has no tools field
- Error parsing file

Used for filtering scope plans to only include tools from preset."
  (when branch-dir
    (let* ((md-file (expand-file-name "preset.md" branch-dir))
           (org-file (expand-file-name "preset.org" branch-dir))
           (preset-file (cond
                         ((file-exists-p md-file) md-file)
                         ((file-exists-p org-file) org-file)
                         (t nil))))
      (when preset-file
        (condition-case err
            (progn
              (require 'gptel-agent nil t)
              (when (fboundp 'gptel-agent-read-file)
                (let* ((preset-data (gptel-agent-read-file preset-file '((dummy . dummy))))
                       (preset-plist (cdr preset-data))
                       (tools (plist-get preset-plist :tools)))
                  (cond
                   ;; List of strings - return as-is
                   ((and (listp tools) (stringp (car tools)))
                    tools)
                   ;; Space-separated string (from org properties) - split
                   ((stringp tools)
                    (split-string tools))
                   ;; Nil or unknown format
                   (t nil)))))
          (error
           (message "Warning: Failed to parse preset tools from %s: %s"
                    preset-file (error-message-string err))
           nil))))))
;; Parse Preset Tools:1 ends here

;; Generate Scope Plan YAML

;; Helper function to generate scope plan YAML string without user interaction.
;; Used for auto-initialization and programmatic plan creation.


;; [[file:scope-commands.org::*Generate Scope Plan YAML][Generate Scope Plan YAML:1]]
(defun jf/gptel--generate-scope-plan-yaml (session-id &optional template projects branch-dir worktree-paths)
  "Generate scope plan YAML for SESSION-ID using TEMPLATE.
TEMPLATE can be deny-all, codebase-read, org-roam-safe, permissive, or project-aware.
PROJECTS are passed to project-aware template.
BRANCH-DIR is path to branch directory for reading preset file.
WORKTREE-PATHS is list of worktree paths for activity isolation.
Defaults to deny-all.

This is an internal helper used for auto-initialization of session scope plans.
Returns YAML string directly without writing to file."
  (let* ((template (or template "deny-all"))
         (template-fn (intern (format "jf/gptel-scope--template-%s" template)))
         ;; Parse preset tools if branch-dir provided
         (preset-tools (when branch-dir
                        (jf/gptel-scope--parse-preset-tools branch-dir))))
    (unless (fboundp template-fn)
      (error "Unknown scope template: %s" template))
    (if (eq (intern template) 'project-aware)
        (funcall template-fn session-id projects nil nil preset-tools worktree-paths)
      (funcall template-fn session-id nil nil preset-tools worktree-paths))))
;; Generate Scope Plan YAML:1 ends here

;; Initialize Scope Plan

;; Create new scope plan for current gptel session.


;; [[file:scope-commands.org::*Initialize Scope Plan][Initialize Scope Plan:1]]
(defun jf/gptel-scope-init-plan (&optional template)
  "Create scope plan for current gptel session.
Requires being in gptel buffer, auto-initializes session if needed.

TEMPLATE: Symbol for template to use
         (deny-all, codebase-read, org-roam-safe, permissive, project-aware)
         If nil, prompts user to select template."
  (interactive
   (list (intern (completing-read "Template: "
                                  '("deny-all" "codebase-read" "org-roam-safe"
                                    "permissive" "project-aware")
                                  nil t nil nil "deny-all"))))
  (unless gptel-mode
    (user-error "Not in a gptel buffer. Start one with M-x gptel first"))

  ;; Auto-initialize session if needed
  (when (and (not jf/gptel--session-id) jf/gptel-autosave-enabled)
    (message "Initializing new session...")
    (jf/gptel--initialize-session))

  (unless jf/gptel--session-id
    (user-error "No session ID. Enable autosave with M-x jf/gptel-toggle-autosave"))

  (let* ((session-id jf/gptel--session-id)
         (session-data (jf/gptel--get-session-data session-id))
         (session-dir (plist-get session-data :directory))
         (plan-file (expand-file-name "scope-plan.yml" session-dir))
         (template-fn (intern (format "jf/gptel-scope--template-%s" template)))
         ;; For project-aware, prompt for projects
         (template-args (when (eq template 'project-aware)
                         (list (jf/gptel--select-projects))))
         (plan-content (apply template-fn session-id template-args)))

    ;; Check if plan already exists
    (when (file-exists-p plan-file)
      (unless (yes-or-no-p "Scope plan already exists. Overwrite? ")
        (user-error "Cancelled")))

    ;; Write plan file
    (with-temp-file plan-file
      (insert plan-content))

    ;; Open for editing
    (find-file plan-file)
    (message "Scope plan created: %s (Edit and save to customize)" plan-file)))
;; Initialize Scope Plan:1 ends here

;; Edit Scope Plan

;; Open existing scope plan for editing.


;; [[file:scope-commands.org::*Edit Scope Plan][Edit Scope Plan:1]]
(defun jf/gptel-scope-edit-plan (&optional session-id)
  "Open scope plan in editor.
Works from anywhere - prompts for session if not in gptel buffer.

SESSION-ID: Optional session ID. If nil, uses current session or prompts."
  (interactive)
  (let* ((session-id (or session-id
                        (and (boundp 'jf/gptel--session-id) jf/gptel--session-id)
                        (let ((sessions (jf/gptel--list-all-sessions)))
                          (when sessions
                            (completing-read "Select session: " sessions nil t)))))
         (session-data (when session-id (jf/gptel--get-session-data session-id)))
         (session-dir (when session-data (plist-get session-data :directory)))
         (plan-file (when session-dir (expand-file-name "scope-plan.yml" session-dir))))

    (unless session-id
      (user-error "No session selected or found"))

    (unless (file-exists-p plan-file)
      (if (yes-or-no-p (format "No plan found for session %s. Create one? " session-id))
          (let* ((template (intern (completing-read "Template: "
                                                   '("deny-all" "codebase-read" "org-roam-safe"
                                                     "permissive" "project-aware")
                                                   nil t nil nil "deny-all")))
                 (template-args (when (eq template 'project-aware)
                                 (list (jf/gptel--select-projects)))))
            (with-temp-file plan-file
              (insert (apply (intern (format "jf/gptel-scope--template-%s" template))
                           session-id template-args)))
            (find-file plan-file)
            (message "Scope plan created: %s" plan-file))
        (user-error "Cancelled")))

    (find-file plan-file)))
;; Edit Scope Plan:1 ends here

;; List Session Helpers (For Completion)

;; Helper functions to list sessions for command completion.


;; [[file:scope-commands.org::*List Session Helpers (For Completion)][List Session Helpers (For Completion):1]]
(defun jf/gptel--list-all-sessions ()
  "List all gptel session IDs (active and archived).
Returns list of session ID strings."
  (let ((session-dir (expand-file-name "~/gptel-sessions"))
        (sessions nil))
    (when (file-directory-p session-dir)
      (dolist (entry (directory-files session-dir nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"))
        (when (file-directory-p (expand-file-name entry session-dir))
          (push entry sessions))))
    (nreverse sessions)))
;; List Session Helpers (For Completion):1 ends here

;; Provide Feature


;; [[file:scope-commands.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-commands)
;;; scope-commands.el ends here
;; Provide Feature:1 ends here
