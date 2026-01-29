;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'yaml)

(defun jf/gptel-scope--glob-to-regex (glob)
  "Convert glob pattern GLOB to regex.
Supports **, *, and ? wildcards."
  (let ((regex "^")
        (chars (string-to-list glob))
        (i 0))
    (while (< i (length chars))
      (let ((char (nth i chars)))
        (cond
         ;; Handle ** (match any including /)
         ((and (eq char ?*)
               (< (1+ i) (length chars))
               (eq (nth (1+ i) chars) ?*))
          (setq regex (concat regex ".*"))
          (setq i (1+ i))) ; Skip next *

         ;; Handle * (match any except /)
         ((eq char ?*)
          (setq regex (concat regex "[^/]*")))

         ;; Handle ? (single char except /)
         ((eq char ??)
          (setq regex (concat regex "[^/]")))

         ;; Escape regex special chars
         ((memq char '(?. ?+ ?^ ?$ ?| ?\( ?\) ?\[ ?\] ?{ ?} ?\\))
          (setq regex (concat regex "\\\\" (char-to-string char))))

         ;; Regular character
         (t
          (setq regex (concat regex (char-to-string char))))))
      (setq i (1+ i)))
    (concat regex "$")))

(defun jf/gptel-scope--matches-pattern (path pattern)
  "Check if PATH matches glob PATTERN.
Returns t if matches, nil otherwise."
  (let ((regex (jf/gptel-scope--glob-to-regex pattern)))
    (string-match-p regex path)))

(defun jf/gptel-scope--matches-any-pattern (path patterns)
  "Check if PATH matches any pattern in PATTERNS.
PATTERNS is a list of glob patterns.
Returns t if any pattern matches, nil otherwise."
  (when patterns
    (cl-some (lambda (pattern)
               (jf/gptel-scope--matches-pattern path pattern))
             patterns)))

(defun jf/gptel-scope--vectors-to-lists (obj)
  "Recursively convert vectors to lists in OBJ.
YAML parser returns arrays as vectors, but elisp code expects lists."
  (cond
   ;; Vector → list
   ((vectorp obj)
    (mapcar #'jf/gptel-scope--vectors-to-lists (append obj nil)))
   ;; Plist → process values recursively
   ((and (listp obj) (keywordp (car-safe obj)))
    (let ((result nil))
      (while obj
        (let ((key (car obj))
              (val (cadr obj)))
          (push key result)
          (push (jf/gptel-scope--vectors-to-lists val) result)
          (setq obj (cddr obj))))
      (nreverse result)))
   ;; List → process elements recursively
   ((listp obj)
    (mapcar #'jf/gptel-scope--vectors-to-lists obj))
   ;; Scalar → return as-is
   (t obj)))

(defun jf/gptel-scope--load-plan (session-id)
  "Load scope plan for SESSION-ID.
Returns parsed plan as plist with vectors converted to lists, or nil if no plan exists."
  (when session-id
    (let* ((session-data (jf/gptel--get-session-data session-id))
           (session-dir (when session-data (plist-get session-data :directory)))
           (plan-file (when session-dir (expand-file-name "scope-plan.yml" session-dir))))
      (when (and plan-file (file-exists-p plan-file))
        (condition-case err
            (with-temp-buffer
              (insert-file-contents plan-file)
              (let ((parsed (yaml-parse-string (buffer-string) :object-type 'plist)))
                ;; Convert all vectors to lists for easier manipulation
                (jf/gptel-scope--vectors-to-lists parsed)))
          (error
           (message "Error loading scope plan: %s" (error-message-string err))
           nil))))))

(defun jf/gptel-scope--save-plan (session-id plan-yaml-string)
  "Save PLAN-YAML-STRING for SESSION-ID to YAML file.
PLAN-YAML-STRING is a YAML-formatted string. Returns t on success, nil on error."
  (when session-id
    (let* ((session-data (jf/gptel--get-session-data session-id))
           (session-dir (when session-data (plist-get session-data :directory)))
           (plan-file (when session-dir (expand-file-name "scope-plan.yml" session-dir))))
      (when plan-file
        (condition-case err
            (progn
              ;; Write YAML string directly
              (with-temp-file plan-file
                (insert plan-yaml-string))
              (message "Scope plan saved: %s" plan-file)
              t)
          (error
           (message "Error saving scope plan: %s" (error-message-string err))
           nil))))))

(defun jf/gptel-scope--classify-tool (tool-name)
  "Classify TOOL-NAME as read or write operation.
Returns 'read, 'write, or nil (unknown)."
  (cond
   ;; Write operations (require scope checking)
   ((member tool-name '("create_file" "Write" "Edit"
                        "create_roam_node" "create_reference_node"
                        "link_roam_nodes" "add_roam_tags"
                        "Bash"))
    'write)

   ;; Read operations (always allowed)
   ((member tool-name '("read_file" "list_directory" "list_project_files"
                        "search_roam_nodes" "list_roam_nodes"
                        "list_known_projects" "get_project_info"
                        "find_project_files" "grep_project"
                        "get_file_info" "search_definitions"
                        "get_treesitter_node"))
    'read)

   ;; Meta tools (delegation, don't scope directly)
   ((member tool-name '("Agent" "create_gptel_tool"))
    nil)

   ;; Unknown tool - default to write (conservative)
   (t
    (message "Warning: Unknown tool classification for: %s, treating as write" tool-name)
    'write)))

(defun jf/gptel-scope--check-resource (plan resource)
  "Check if RESOURCE is allowed by PLAN.
RESOURCE is (type operation . path-parts) tuple.
Returns t if allowed, nil if denied."
  (if (null plan)
      ;; No plan exists - allow by default (plan is opt-in)
      t
    (pcase-let ((`(,type ,operation . ,path-parts) resource))
      (let* ((scope-section (plist-get plan (intern (format ":%s" type))))
             (deny-patterns (plist-get scope-section :deny))
             (allow-patterns (plist-get scope-section (intern (format ":%s" operation))))
             (default-policy (plist-get plan :default_policy)))

        ;; Combine path-parts into single path string for matching
        (let ((path (if (consp path-parts)
                        (mapconcat #'identity path-parts " ")
                      path-parts)))

          ;; 1. Check explicit denials first (highest priority)
          (when (and deny-patterns (jf/gptel-scope--matches-any-pattern path deny-patterns))
            (message "Scope: DENY %s %s (explicit denial)" type path)
            (cl-return-from jf/gptel-scope--check-resource nil))

          ;; 2. Check allow patterns
          (if (and allow-patterns (jf/gptel-scope--matches-any-pattern path allow-patterns))
              (progn
                (message "Scope: ALLOW %s %s (matched allow pattern)" type path)
                t)

            ;; 3. Fall back to default policy
            (let ((allowed (eq default-policy 'allow)))
              (message "Scope: %s %s %s (default policy)"
                       (if allowed "ALLOW" "DENY") type path)
              allowed)))))))

(defun jf/gptel-scope--extract-resources (tool-name arg-values)
  "Extract resource identifiers from TOOL-NAME's ARG-VALUES.
Returns list of (type operation resource-path) tuples.
Returns nil for unknown or read-only tools."
  (pcase tool-name
    ;; Filesystem tools - create_file
    ("create_file"
     (let ((path (nth 0 arg-values))
           (filename (nth 1 arg-values)))
       (when (and path filename)
         (let ((full-path (expand-file-name filename path)))
           ;; Resolve symlinks to real path
           (when (file-exists-p (file-name-directory full-path))
             (setq full-path (file-truename full-path)))
           `((filesystem write ,full-path))))))

    ;; Community tools - Write (same signature as create_file)
    ("Write"
     (let ((path (nth 0 arg-values))
           (filename (nth 1 arg-values)))
       (when (and path filename)
         (let ((full-path (expand-file-name filename path)))
           ;; Resolve symlinks to real path
           (when (file-exists-p (file-name-directory full-path))
             (setq full-path (file-truename full-path)))
           `((filesystem write ,full-path))))))

    ;; Community tools - Edit (takes full filepath as single arg)
    ("Edit"
     (let ((filepath (nth 0 arg-values)))
       (when filepath
         (let ((full-path (expand-file-name filepath)))
           (when (file-exists-p (file-name-directory full-path))
             (setq full-path (file-truename full-path)))
           `((filesystem write ,full-path))))))

    ;; Org-roam - create_roam_node
    ("create_roam_node"
     (let ((subdirectory (nth 4 arg-values))) ; 5th arg
       `((org_roam write ,(concat "subdirectory:" (or subdirectory ""))))))

    ;; Org-roam - create_reference_node
    ("create_reference_node"
     (let ((subdirectory (nth 3 arg-values))) ; 4th arg
       `((org_roam write ,(concat "subdirectory:" (or subdirectory ""))))))

    ;; Org-roam - link_roam_nodes
    ("link_roam_nodes"
     (let ((source-id (nth 0 arg-values))
           (target-id (nth 1 arg-values)))
       `((org_roam link ,(concat "node_id:" source-id))
         (org_roam link ,(concat "node_id:" target-id)))))

    ;; Org-roam - add_roam_tags
    ("add_roam_tags"
     (let ((node-id (nth 0 arg-values)))
       `((org_roam write ,(concat "node_id:" node-id)))))

    ;; Shell commands - Bash
    ("Bash"
     (let ((command (nth 0 arg-values)))
       `((shell exec ,command))))

    ;; Unknown or read-only tool
    (_ nil)))

(defun jf/gptel-scope--check-shell-command (plan command-string)
  "Check if shell COMMAND-STRING is allowed by PLAN.
Conservative approach: default deny, explicit whitelist only.
Returns t if allowed, nil if denied."
  (let* ((shell-scope (plist-get plan :shell))
         (allowed-commands (plist-get shell-scope :allow_commands))
         (deny-commands (plist-get shell-scope :deny_commands))
         (parts (split-string-and-unquote command-string))
         (cmd (car parts))
         (full-cmd command-string))

    ;; 1. Check explicit denials first (substring match for danger patterns)
    (when (and deny-commands
               (cl-some (lambda (deny) (string-match-p deny full-cmd))
                        deny-commands))
      (message "Scope: DENY shell command (explicit denial): %s" command-string)
      (cl-return-from jf/gptel-scope--check-shell-command nil))

    ;; 2. Check whitelist (exact command match or wildcard)
    (let ((allowed (or (member cmd allowed-commands)
                       (member "*" allowed-commands))))
      (if allowed
          (progn
            (message "Scope: ALLOW shell command: %s" command-string)
            t)
        (progn
          (message "Scope: DENY shell command (not in whitelist): %s" command-string)
          nil)))))

(defun jf/gptel-scope--check-tool-calls (orig-fn response ov)
  "Advice around gptel--accept-tool-calls to enforce scope checking.
RESPONSE is list of (tool-spec arg-values callback).
OV is overlay showing tool calls."
  ;; Extract session-id from multiple sources (works for main buffer and agents)
  (let* ((session-id (or
                      ;; 1. Try buffer-local variable (main buffer)
                      jf/gptel--session-id
                      ;; 2. Try overlay property (agent context)
                      (when (overlayp ov) (overlay-get ov 'jf/session-id))
                      ;; 3. Try overlay buffer's session-id
                      (when (overlayp ov)
                        (with-current-buffer (overlay-buffer ov)
                          jf/gptel--session-id))))
         (plan (when session-id (jf/gptel-scope--load-plan session-id)))
         (violations nil))

    (message "DEBUG scope-check: session-id=%s, ov=%s, ov-props=%S"
             session-id (if (overlayp ov) "overlay" "nil")
             (when (overlayp ov) (overlay-properties ov)))

    ;; Scan all tool calls for violations
    (dolist (tool-call-spec response)
      (let* ((tool-spec (nth 0 tool-call-spec))
             (arg-values (nth 1 tool-call-spec))
             (tool-name (gptel-tool-name tool-spec))
             (classification (jf/gptel-scope--classify-tool tool-name)))

        (message "DEBUG scope-check: tool=%s, classification=%s" tool-name classification)

        ;; Only check write operations
        (when (eq classification 'write)
          (when-let ((resources (jf/gptel-scope--extract-resources tool-name arg-values)))
            (message "DEBUG scope-check: resources=%S" resources)
            (dolist (resource resources)
              (pcase-let ((`(,type ,_operation . ,_path) resource))
                ;; Special handling for shell commands
                (let ((allowed (if (eq type 'shell)
                                   (jf/gptel-scope--check-shell-command
                                    plan (nth 0 arg-values))
                                 (jf/gptel-scope--check-resource plan resource))))
                  (message "DEBUG scope-check: resource=%S, allowed=%s" resource allowed)
                  (unless allowed
                    (push (list tool-name resource) violations)))))))))

    (if (null violations)
        ;; All within scope or no plan - execute normally
        (progn
          (message "DEBUG scope-check: No violations, executing tools")
          (funcall orig-fn response ov))

      ;; Out of scope - prompt user
      (message "DEBUG scope-check: %d violations detected" (length violations))
      (jf/gptel-scope--prompt-violations
       violations session-id
       (lambda () (funcall orig-fn response ov))  ; on-approve
       (lambda ()
         ;; on-deny - reject tool calls
         (when (overlayp ov)
           (delete-overlay ov))
         (message "Tool execution denied by scope manager"))))))

(defun jf/gptel-scope--prompt-violations (violations session-id on-approve on-deny)
  "Prompt user to approve VIOLATIONS.
VIOLATIONS is list of (tool-name resource) pairs.
SESSION-ID is the current session.
ON-APPROVE and ON-DENY are callbacks."
  (let* ((buffer (get-buffer-create "*gptel-scope-violations*"))
         (plan (jf/gptel-scope--load-plan session-id)))

    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "TOOL SCOPE VIOLATIONS\n" 'face 'error))
        (insert (propertize "═════════════════════\n\n" 'face 'shadow))

        (insert "The following tools want to access resources OUTSIDE the approved scope:\n\n")

        ;; List violations
        (dolist (violation violations)
          (pcase-let ((`(,tool-name ,resource) violation))
            (pcase-let ((`(,type ,op . ,path-parts) resource))
              (let ((path (if (consp path-parts)
                              (mapconcat #'identity path-parts " ")
                            path-parts)))
                (insert (format "  • %s: %s %s %s\n"
                                (propertize tool-name 'face 'font-lock-function-name-face)
                                (propertize (format "%s" type) 'face 'font-lock-keyword-face)
                                (propertize (format "%s" op) 'face 'font-lock-type-face)
                                (propertize path 'face 'font-lock-string-face)))))))

        (insert "\n")
        (if plan
            (progn
              (insert (propertize "Current scope plan:\n" 'face 'font-lock-comment-face))
              (insert (propertize (format "  Session: %s\n" session-id) 'face 'shadow))
              (insert (propertize (format "  Policy: %s\n\n"
                                          (plist-get plan :default_policy))
                                  'face 'shadow)))
          (insert (propertize "No scope plan exists for this session.\n\n" 'face 'warning)))

        ;; Action buttons
        (insert-button "[ Allow Once ]"
                       'action (lambda (_)
                                 (kill-buffer buffer)
                                 (funcall on-approve))
                       'follow-link t)
        (insert "  ")
        (insert-button "[ Add to Plan & Allow ]"
                       'action (lambda (_)
                                 (jf/gptel-scope--add-violations-to-plan violations session-id)
                                 (kill-buffer buffer)
                                 (funcall on-approve))
                       'follow-link t)
        (insert "  ")
        (insert-button "[ Deny ]"
                       'action (lambda (_)
                                 (kill-buffer buffer)
                                 (funcall on-deny))
                       'follow-link t)
        (insert "\n\n")
        (insert-button "[ Edit Scope Plan ]"
                       'action (lambda (_)
                                 (jf/gptel-scope-edit-plan session-id))
                       'follow-link t)

        (goto-char (point-min))
        (view-mode 1)))

    (pop-to-buffer buffer)))

(defun jf/gptel-scope--add-violations-to-plan (violations session-id)
  "Add VIOLATIONS to scope plan for SESSION-ID.
Extracts patterns from violations and updates plan file by editing YAML text directly."
  (let* ((session-data (jf/gptel--get-session-data session-id))
         (session-dir (when session-data (plist-get session-data :directory)))
         (plan-file (when session-dir (expand-file-name "scope-plan.yml" session-dir))))

    (unless (and plan-file (file-exists-p plan-file))
      (user-error "Scope plan file not found for session: %s" session-id))

    ;; Process each violation and add to YAML file
    (with-temp-buffer
      (insert-file-contents plan-file)

      ;; Process each violation
      (dolist (violation violations)
        (pcase-let ((`(,_tool-name ,resource) violation))
          (pcase-let ((`(,type ,operation . ,path-parts) resource))
            ;; Extract clean path
            (let* ((path (if (consp path-parts)
                             (car path-parts)  ; Use first element, not concatenated
                           path-parts))
                   (section-name (symbol-name type))
                   (operation-name (symbol-name operation)))

              (message "DEBUG add-to-plan: type=%s, operation=%s, path=%s" type operation path)

              ;; Find the section (e.g., "filesystem:")
              (goto-char (point-min))
              (if (re-search-forward (format "^%s:" section-name) nil t)
                  (progn
                    ;; Find the operation subsection (e.g., "  write:")
                    (let ((section-start (point)))
                      (if (re-search-forward (format "^  %s:" operation-name) nil t)
                          (progn
                            ;; Found operation section, add pattern
                            (forward-line 1)
                            ;; Check if it's empty ([] or no items)
                            (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
                              (if (string-match-p "\\[\\]\\|^$" line)
                                  ;; Empty list, add first item
                                  (progn
                                    (beginning-of-line)
                                    (insert (format "    - \"%s\"\n" path)))
                                ;; Has items, add to list
                                (progn
                                  ;; Move to end of list (before next section or end of file)
                                  (while (and (not (eobp))
                                              (looking-at "^    - "))
                                    (forward-line 1))
                                  (beginning-of-line)
                                  (insert (format "    - \"%s\"\n" path))))))
                        ;; Operation section not found, add it
                        (goto-char section-start)
                        (forward-line 1)
                        (insert (format "  %s:\n    - \"%s\"\n" operation-name path)))))
                ;; Section not found - shouldn't happen with templates
                (message "Warning: Section '%s' not found in plan file" section-name))))))

      ;; Update timestamp
      (goto-char (point-min))
      (when (re-search-forward "^updated: \".*\"" nil t)
        (replace-match (format "updated: \"%s\"" (format-time-string "%Y-%m-%dT%H:%M:%SZ"))))

      ;; Write back to file
      (write-region (point-min) (point-max) plan-file))

    (message "Added %d pattern(s) to scope plan" (length violations))))

(defun jf/gptel-scope--template-deny-all (session-id &optional type parent-id agent-type)
  "Create deny-all template for SESSION-ID.
Secure default - nothing allowed.
Optional TYPE, PARENT-ID, and AGENT-TYPE for agent sessions."
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (agent-fields (when type
                          (concat
                           (format "type: \"%s\"\n" type)
                           (when parent-id
                             (format "parent_session_id: \"%s\"\n" parent-id))
                           (when agent-type
                             (format "agent_type: \"%s\"\n" agent-type))))))
    (format "version: \"1.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
%sdefault_policy: deny

# Filesystem write patterns (reads always allowed)
filesystem:
  write: []
  deny:
    - \"**/.git/**\"
    - \"**/runtime/**\"

# Org-roam write patterns
org_roam:
  write: []
  link: []

# Shell command allowlist (conservative whitelist)
shell:
  allow_commands: []
  deny_commands:
    - \"rm -rf\"
    - \"sudo\"
    - \"chmod\"
    - \"chown\"
"
            session-id
            timestamp
            timestamp
            (or agent-fields ""))))

(defun jf/gptel-scope--template-codebase-read (session-id &optional type parent-id agent-type)
  "Create codebase-read template for SESSION-ID.
Reads always allowed (no scoping), writes require patterns.
Optional TYPE, PARENT-ID, and AGENT-TYPE for agent sessions."
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (agent-fields (when type
                          (concat
                           (format "type: \"%s\"\n" type)
                           (when parent-id
                             (format "parent_session_id: \"%s\"\n" parent-id))
                           (when agent-type
                             (format "agent_type: \"%s\"\n" agent-type))))))
    (format "version: \"1.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
%sdefault_policy: deny

# Filesystem write patterns (reads always allowed)
filesystem:
  write: []
  deny:
    - \"**/.git/**\"
    - \"**/runtime/**\"

# Org-roam write patterns
org_roam:
  write: []
  link: []

# Shell command allowlist (read-only commands)
shell:
  allow_commands:
    - \"ls\"
    - \"find\"
    - \"grep\"
    - \"git status\"
    - \"git diff\"
    - \"git log\"
  deny_commands:
    - \"rm -rf\"
    - \"sudo\"
"
            session-id
            timestamp
            timestamp
            (or agent-fields ""))))

(defun jf/gptel-scope--template-org-roam-safe (session-id &optional type parent-id agent-type)
  "Create org-roam-safe template for SESSION-ID.
Allows org-roam in gptel/ subdirectory only.
Optional TYPE, PARENT-ID, and AGENT-TYPE for agent sessions."
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (agent-fields (when type
                          (concat
                           (format "type: \"%s\"\n" type)
                           (when parent-id
                             (format "parent_session_id: \"%s\"\n" parent-id))
                           (when agent-type
                             (format "agent_type: \"%s\"\n" agent-type))))))
    (format "version: \"1.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
%sdefault_policy: deny

# Filesystem write patterns (gptel subdirectory only)
filesystem:
  write:
    - \"~/org/roam/gptel/**/*.org\"
  deny:
    - \"**/.git/**\"

# Org-roam write patterns (gptel subdirectory only)
org_roam:
  write:
    - \"subdirectory:gptel/**\"
    - \"tag:gptel\"
  link:
    - \"node_id:*\"

# Shell command allowlist (read-only)
shell:
  allow_commands:
    - \"ls\"
    - \"grep\"
  deny_commands:
    - \"rm -rf\"
    - \"sudo\"
"
            session-id
            timestamp
            timestamp
            (or agent-fields ""))))

(defun jf/gptel-scope--template-permissive (session-id &optional type parent-id agent-type)
  "Create permissive template for SESSION-ID.
Allow most operations, deny only dangerous.
Optional TYPE, PARENT-ID, and AGENT-TYPE for agent sessions."
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (agent-fields (when type
                          (concat
                           (format "type: \"%s\"\n" type)
                           (when parent-id
                             (format "parent_session_id: \"%s\"\n" parent-id))
                           (when agent-type
                             (format "agent_type: \"%s\"\n" agent-type))))))
    (format "version: \"1.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
%sdefault_policy: allow

# Filesystem - deny only sensitive directories
filesystem:
  deny:
    - \"**/.git/**\"
    - \"**/runtime/**\"
    - \"~/.ssh/**\"
    - \"~/.gnupg/**\"

# Org-roam - allow everything
org_roam:
  write:
    - \"*\"
  link:
    - \"*\"

# Shell - allow most commands, deny dangerous
shell:
  allow_commands:
    - \"*\"
  deny_commands:
    - \"rm -rf\"
    - \"sudo\"
    - \"chmod 777\"
    - \"chown\"
"
            session-id
            timestamp
            timestamp
            (or agent-fields ""))))

(defun jf/gptel--list-all-sessions ()
  "List all session IDs (active + on-disk).
Returns alist of (session-id . session-dir)."
  (let ((sessions nil))
    ;; Add active sessions from registry
    (maphash (lambda (id data)
               (let ((dir (plist-get data :directory)))
                 (push (cons id dir) sessions)))
             jf/gptel--session-registry)

    ;; Add sessions from disk (that aren't already in registry)
    (let ((sessions-base (expand-file-name jf/gptel-sessions-directory)))
      (when (file-directory-p sessions-base)
        (dolist (dir (directory-files sessions-base t "^[^.]"))
          (when (file-directory-p dir)
            (let* ((metadata (jf/gptel--read-metadata dir))
                   (session-id (when metadata (plist-get metadata :session_id))))
              (when (and session-id (not (assoc session-id sessions)))
                (push (cons session-id dir) sessions)))))))

    (nreverse sessions)))

(defun jf/gptel-scope-init-plan (template)
  "Create scope plan for current gptel session using TEMPLATE.
TEMPLATE is one of: deny-all, codebase-read, org-roam-safe, permissive.

This command MUST be called from within a gptel buffer.
If you're not in a gptel session, start one first with `M-x gptel`."
  (interactive
   (list (intern (completing-read "Template: "
                                  '("deny-all" "codebase-read" "org-roam-safe" "permissive")
                                  nil t nil nil "deny-all"))))

  ;; Check if we're in gptel-mode
  (unless gptel-mode
    (user-error "Not in a gptel buffer. Start one with M-x gptel first"))

  ;; Initialize session if needed (session created on first request normally)
  (when (and (not jf/gptel--session-id)
             jf/gptel-autosave-enabled)
    (message "Initializing new session...")
    (jf/gptel--initialize-session))

  ;; Now we should have a session-id
  (unless jf/gptel--session-id
    (user-error "Failed to initialize session. Is autosave enabled?"))

  (let ((plan (pcase template
                ('deny-all (jf/gptel-scope--template-deny-all jf/gptel--session-id))
                ('codebase-read (jf/gptel-scope--template-codebase-read jf/gptel--session-id))
                ('org-roam-safe (jf/gptel-scope--template-org-roam-safe jf/gptel--session-id))
                ('permissive (jf/gptel-scope--template-permissive jf/gptel--session-id))
                (_ (jf/gptel-scope--template-deny-all jf/gptel--session-id)))))

    (if (jf/gptel-scope--save-plan jf/gptel--session-id plan)
        (progn
          (message "Created scope plan for session %s (template: %s)"
                   jf/gptel--session-id template)
          (jf/gptel-scope-edit-plan))
      (message "Failed to create scope plan"))))

(defun jf/gptel-scope-edit-plan (&optional session-id)
  "Open scope plan in editor.
If called from a gptel buffer, uses current session.
Otherwise, prompts to select from all available sessions.

Optional SESSION-ID can be provided programmatically."
  (interactive)

  ;; Determine which session to use
  (let* ((target-session-id
          (or session-id
              jf/gptel--session-id  ; Use current if in gptel buffer
              ;; Otherwise, let user select from all sessions
              (let ((all-sessions (jf/gptel--list-all-sessions)))
                (if (null all-sessions)
                    (user-error "No gptel sessions found. Start one with M-x gptel")
                  (completing-read "Select session: "
                                   (mapcar #'car all-sessions)
                                   nil t)))))
         (session-data (jf/gptel--get-session-data target-session-id))
         (session-dir (if session-data
                          (plist-get session-data :directory)
                        ;; Not in registry, look up from disk
                        (cdr (assoc target-session-id (jf/gptel--list-all-sessions)))))
         (plan-file (when session-dir (expand-file-name "scope-plan.yml" session-dir))))

    (unless plan-file
      (user-error "Session directory not found for: %s" target-session-id))

    ;; Create plan if doesn't exist
    (unless (file-exists-p plan-file)
      (if (y-or-n-p (format "No scope plan for session %s. Create one? " target-session-id))
          (let* ((template (intern (completing-read "Template: "
                                                    '("deny-all" "codebase-read"
                                                      "org-roam-safe" "permissive")
                                                    nil t nil nil "deny-all")))
                 (plan-yaml (pcase template
                              ('deny-all (jf/gptel-scope--template-deny-all target-session-id))
                              ('codebase-read (jf/gptel-scope--template-codebase-read target-session-id))
                              ('org-roam-safe (jf/gptel-scope--template-org-roam-safe target-session-id))
                              ('permissive (jf/gptel-scope--template-permissive target-session-id)))))
            (with-temp-file plan-file
              (insert plan-yaml))
            (message "Created scope plan: %s" plan-file))
        (user-error "Cancelled")))

    ;; Open plan file
    (find-file plan-file)
    (when (fboundp 'yaml-mode)
      (yaml-mode))))

(defun jf/gptel-scope-check-tool (tool-name &rest args)
  "Manually test TOOL-NAME with ARGS against current session scope.
Useful for debugging patterns."
  (interactive
   (list (completing-read "Tool: "
                          '("create_file" "Write" "Edit"
                            "create_roam_node" "link_roam_nodes"
                            "Bash")
                          nil t)
         ;; TODO: Prompt for args based on tool
         ))

  (let* ((session-id jf/gptel--session-id)
         (plan (when session-id (jf/gptel-scope--load-plan session-id))))

    (if (not session-id)
        (message "Not in a gptel session")
      (if (not plan)
          (message "No scope plan for session: %s" session-id)
        (let* ((resources (jf/gptel-scope--extract-resources tool-name args)))
          (if (not resources)
              (message "No resources extracted (read-only or unknown tool)")
            (dolist (resource resources)
              (let ((allowed (jf/gptel-scope--check-resource plan resource)))
                (message "Tool: %s, Resource: %s, Allowed: %s"
                         tool-name resource allowed)))))))))

(with-eval-after-load 'gptel
  (advice-add 'gptel--accept-tool-calls :around #'jf/gptel-scope--check-tool-calls)
  (message "Scope manager: Installed advice on gptel--accept-tool-calls"))

(provide 'jf/gptel-scope-manager)
