;;; scope-expansion.el --- GPTEL Scope Expansion UI -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Interactive UI for handling scope violations with 3-choice menu.

;;; Code:

(require 'transient)
(require 'jf-gptel-scope-core)
(require 'yaml)

(defun jf/gptel-scope--get-scope-file-path ()
  "Get the path to scope.yml for the current buffer context.
Returns absolute path to scope.yml, or nil if context cannot be determined."
  (let ((context-dir (or (and (boundp 'jf/gptel--branch-dir) jf/gptel--branch-dir)
                         (and (buffer-file-name)
                              (file-name-directory (buffer-file-name))))))
    (when context-dir
      (expand-file-name jf/gptel-session--scope-file context-dir))))

(transient-define-prefix jf/gptel-scope-expansion-menu ()
  "Handle scope violation with 3-choice UI."
  [:description
   (lambda ()
     (let* ((scope (transient-scope))
            (violation (plist-get scope :violation))
            (tool (plist-get violation :tool))
            (resource (plist-get violation :resource))
            (reason (plist-get violation :reason)))
       (format "Scope Violation: %s\n  Tool: %s\n  Resource: %s\n  Reason: %s"
               (propertize "Access Denied" 'face 'error)
               (propertize tool 'face 'font-lock-function-name-face)
               (propertize resource 'face 'font-lock-string-face)
               (propertize reason 'face 'warning))))
   [("d" "Deny (reject tool call)" jf/gptel-scope--deny-expansion
     :transient nil)
    ("a" "Add to scope (permanent)" jf/gptel-scope--add-to-scope
     :transient nil)
    ("o" "Allow once (temporary)" jf/gptel-scope--allow-once-action
     :transient nil)]
   [""
    ("e" "Edit scope manually" jf/gptel-scope--edit-scope)
    ("q" "Cancel" transient-quit-one)]])

(defun jf/gptel-scope--deny-expansion ()
  "Reject the tool call completely."
  (interactive)
  (condition-case err
      (let* ((scope (transient-scope))
             (callback (plist-get scope :callback)))
        (if callback
            (condition-case callback-err
                (funcall callback
                         (json-serialize
                          (list :success nil
                                :user_denied t
                                :message "User denied scope expansion request.")))
              (error
               (message "Error invoking callback: %s" (error-message-string callback-err))))
          (message "Warning: No callback provided for scope expansion"))
        (transient-quit-one))
    (error
     (message "Error in deny-expansion: %s" (error-message-string err))
     (transient-quit-one))))

(defun jf/gptel-scope--add-to-scope ()
  "Add violated resource to scope.yml permanently."
  (interactive)
  (let* ((scope (transient-scope))
         (violation (plist-get scope :violation))
         (callback (plist-get scope :callback))
         (validation-type (plist-get violation :validation-type))
         (resource (plist-get violation :resource))
         (tool (plist-get violation :tool))
         (scope-file (jf/gptel-scope--get-scope-file-path)))

    (unless scope-file
      (user-error "No scope.yml found - unable to determine context directory"))

    (unless (file-exists-p scope-file)
      (user-error "No scope.yml found at %s" scope-file))

    (unless (file-writable-p scope-file)
      (user-error "scope.yml is not writable: %s" scope-file))

    ;; Route to appropriate updater based on validation type
    (pcase validation-type
      ('path
       (jf/gptel-scope--add-path-to-scope scope-file resource tool))
      ('pattern
       (jf/gptel-scope--add-pattern-to-scope scope-file resource tool))
      ('command
       (jf/gptel-scope--add-command-to-scope scope-file resource))
      ('bash
       (jf/gptel-scope--add-bash-to-scope scope-file resource tool))
      (_
       (user-error "Unknown validation type: %s" validation-type)))

    ;; Notify callback with JSON response
    (condition-case err
        (if callback
            (let* ((patterns (plist-get scope :patterns))
                   (tool-name (plist-get scope :tool-name)))
              (funcall callback
                       (json-serialize
                        (list :success t
                              :patterns_added (vconcat patterns)  ; Convert list to vector for JSON array
                              :message (format "Scope expanded. Added %d pattern(s) to %s"
                                             (length patterns) tool-name)))))
          (message "Warning: No callback provided for scope expansion"))
      (error
       (message "Error invoking callback: %s" (error-message-string err))))

    (message "Added %s to scope" resource)
    (transient-quit-one)))

(defun jf/gptel-scope--allow-once-action ()
  "Add resource to temporary allow-once list."
  (interactive)
  (let* ((scope (transient-scope))
         (violation (plist-get scope :violation))
         (callback (plist-get scope :callback))
         (tool (plist-get violation :tool))
         (resource (plist-get violation :resource)))

    ;; Add to allow-once list for this buffer
    (jf/gptel-scope-add-to-allow-once-list tool resource)

    ;; Notify callback with JSON response
    (condition-case err
        (if callback
            (funcall callback
                     (json-serialize
                      (list :success t
                            :allowed_once t
                            :message "Permission granted for this turn only.")))
          (message "Warning: No callback provided for scope expansion"))
      (error
       (message "Error invoking callback: %s" (error-message-string err))))

    (message "Allowed %s once for this LLM turn" resource)
    (transient-quit-one)))

(defun jf/gptel-scope--edit-scope ()
  "Open scope.yml for manual editing."
  (interactive)
  (let ((scope-file (jf/gptel-scope--get-scope-file-path)))
    (if (and scope-file (file-exists-p scope-file))
        (progn
          (find-file scope-file)
          (transient-quit-one))
      (user-error "No scope.yml found - unable to determine context directory"))))

(defun jf/gptel-scope--add-path-to-scope (scope-file path tool)
  "Add PATH to scope.yml under appropriate section based on TOOL operation.
SCOPE-FILE is the path to scope.yml.
PATH is the file/directory path to add.
TOOL is the tool name (used to determine read vs write)."
  (unless (file-exists-p scope-file)
    (user-error "scope.yml not found: %s" scope-file))
  (unless (file-writable-p scope-file)
    (user-error "scope.yml is not writable: %s" scope-file))
  (let* ((content (with-temp-buffer
                    (insert-file-contents scope-file)
                    (buffer-string)))
         ;; Determine if this is a read or write operation
         (category (cdr (assoc tool jf/gptel-scope--tool-categories)))
         (operation (plist-get category :operation))
         (target-section (if (eq operation 'read) :read :write)))

    ;; Parse entire file as YAML (no frontmatter delimiters)
    (let* ((parsed (condition-case err
                       (yaml-parse-string content
                                          :object-type 'plist
                                          :sequence-type 'list)
                     (error
                      (user-error "Failed to parse scope.yml (%s): %s"
                                  scope-file (error-message-string err)))))
           (normalized (jf/gptel-scope--normalize-plist-keys parsed))
           (paths (or (plist-get normalized :paths) (list)))
           (section-paths (or (plist-get paths target-section) '())))

      ;; Add path if not already present (with /** suffix for directories)
      (let ((normalized-path (if (string-suffix-p "/" path)
                                (concat (directory-file-name path) "/**")
                              path)))
        (unless (member normalized-path section-paths)
          (setq section-paths (append section-paths (list normalized-path)))
          (setq paths (plist-put paths target-section section-paths))
          (setq normalized (plist-put normalized :paths paths))

          ;; Write updated content (plain YAML, no delimiters)
          (with-temp-buffer
            (jf/gptel-scope--write-yaml-plist normalized)
            (write-region (point-min) (point-max) scope-file nil 'silent)))))))

(defun jf/gptel-scope--add-pattern-to-scope (scope-file pattern tool)
  "Add PATTERN to org_roam_patterns section in SCOPE-FILE.
PATTERN is a string describing the pattern (format: \"subdirectory:path\" or \"tags:tag\").
TOOL is the org-roam tool name."
  (unless (file-exists-p scope-file)
    (user-error "scope.yml not found: %s" scope-file))
  (unless (file-writable-p scope-file)
    (user-error "scope.yml is not writable: %s" scope-file))
  (let* ((content (with-temp-buffer
                    (insert-file-contents scope-file)
                    (buffer-string)))
         ;; Parse entire file as YAML (no frontmatter delimiters)
         (parsed (condition-case err
                     (yaml-parse-string content
                                        :object-type 'plist
                                        :sequence-type 'list)
                   (error
                    (user-error "Failed to parse scope.yml (%s): %s"
                                scope-file (error-message-string err)))))
         (normalized (jf/gptel-scope--normalize-plist-keys parsed))
         (org-roam (or (plist-get normalized :org-roam-patterns) (list))))

    ;; Parse pattern format and add to appropriate list
    (cond
     ((string-prefix-p "subdirectory:" pattern)
      (let* ((subdir (substring pattern 13))
             (subdirs (or (plist-get org-roam :subdirectory) '())))
        (unless (member subdir subdirs)
          (setq subdirs (append subdirs (list subdir)))
          (setq org-roam (plist-put org-roam :subdirectory subdirs)))))

     ((string-prefix-p "tags:" pattern)
      (let* ((tags-str (substring pattern 5))
             (tags (split-string tags-str ","))
             (existing-tags (or (plist-get org-roam :tags) '())))
        (dolist (tag tags)
          (unless (member tag existing-tags)
            (setq existing-tags (append existing-tags (list tag)))))
        (setq org-roam (plist-put org-roam :tags existing-tags)))))

    (setq normalized (plist-put normalized :org-roam-patterns org-roam))

    ;; Write updated content (plain YAML, no delimiters)
    (with-temp-buffer
      (jf/gptel-scope--write-yaml-plist normalized)
      (write-region (point-min) (point-max) scope-file nil 'silent))))

(defun jf/gptel-scope--add-command-to-scope (scope-file command)
  "Add COMMAND to shell_commands.allow section in SCOPE-FILE."
  (unless (file-exists-p scope-file)
    (user-error "scope.yml not found: %s" scope-file))
  (unless (file-writable-p scope-file)
    (user-error "scope.yml is not writable: %s" scope-file))
  (let* ((content (with-temp-buffer
                    (insert-file-contents scope-file)
                    (buffer-string)))
         (cmd-name (car (split-string command)))
         ;; Parse entire file as YAML (no frontmatter delimiters)
         (parsed (condition-case err
                     (yaml-parse-string content
                                        :object-type 'plist
                                        :sequence-type 'list)
                   (error
                    (user-error "Failed to parse scope.yml (%s): %s"
                                scope-file (error-message-string err)))))
         (normalized (jf/gptel-scope--normalize-plist-keys parsed))
         (shell-cmds (or (plist-get normalized :shell-commands) (list)))
         (allow-list (or (plist-get shell-cmds :allow) '())))

    ;; Add command to allow list if not present
    (unless (member cmd-name allow-list)
      (setq allow-list (append allow-list (list cmd-name)))
      (setq shell-cmds (plist-put shell-cmds :allow allow-list))
      (setq normalized (plist-put normalized :shell-commands shell-cmds))

      ;; Write updated content (plain YAML, no delimiters)
      (with-temp-buffer
        (jf/gptel-scope--write-yaml-plist normalized)
        (write-region (point-min) (point-max) scope-file nil 'silent)))))

(defun jf/gptel-scope--add-bash-to-scope (scope-file resource tool)
  "Add bash command to bash_tools section in SCOPE-FILE.
RESOURCE is the command pattern or directory path.
TOOL is the tool name (used to determine read vs write operation)."
  (unless (file-exists-p scope-file)
    (user-error "scope.yml not found: %s" scope-file))
  (unless (file-writable-p scope-file)
    (user-error "scope.yml is not writable: %s" scope-file))

  ;; Check if resource is a directory path or command pattern
  (if (or (file-directory-p resource)
          (string-suffix-p "/" resource)
          (string-match-p "/" resource))
      ;; Directory path - delegate to path expansion
      (jf/gptel-scope--add-path-to-scope scope-file resource tool)

    ;; Command pattern - add to bash_tools
    (let* ((content (with-temp-buffer
                      (insert-file-contents scope-file)
                      (buffer-string)))
           ;; Extract base command name
           (cmd-name (car (split-string resource "[ |><;&]" t)))
           ;; Parse entire file as YAML (no frontmatter delimiters)
           (parsed (condition-case err
                       (yaml-parse-string content
                                          :object-type 'plist
                                          :sequence-type 'list)
                     (error
                      (user-error "Failed to parse scope.yml (%s): %s"
                                  scope-file (error-message-string err)))))
           ;; Normalize YAML keys from snake_case to kebab-case
           (normalized (jf/gptel-scope--normalize-plist-keys parsed))
           (bash-tools (or (plist-get normalized :bash-tools) (list)))
           (categories (or (plist-get bash-tools :categories) (list)))

           ;; Determine target category based on tool operation
           (category (cdr (assoc tool jf/gptel-scope--tool-categories)))
           (operation (plist-get category :operation))
           ;; Use kebab-case internally (will be converted to snake_case on write)
           (target-category (if (eq operation 'read) :read-only :safe-write))

           ;; Get existing command list for target category
           (category-config (or (plist-get categories target-category) (list)))
           (command-list (or (plist-get category-config :commands) '())))

      ;; Add command to target category if not present
      (unless (member cmd-name command-list)
        (setq command-list (append command-list (list cmd-name)))
        (setq category-config (plist-put category-config :commands command-list))
        (setq categories (plist-put categories target-category category-config))
        (setq bash-tools (plist-put bash-tools :categories categories))
        (setq normalized (plist-put normalized :bash-tools bash-tools))

        ;; Write updated content (plain YAML, no delimiters)
        (with-temp-buffer
          (jf/gptel-scope--write-yaml-plist normalized)
          (write-region (point-min) (point-max) scope-file nil 'silent))))))

(defun jf/gptel-scope--kebab-to-snake (key)
  "Convert KEY from kebab-case to snake_case for YAML output.
E.g., :org-roam-patterns becomes org_roam_patterns.

Inverse function: jf/gptel-scope--normalize-plist-keys (scope-core.org)
Round-trip property: Writing and reading YAML preserves key names."
  (replace-regexp-in-string "-" "_" (substring (symbol-name key) 1)))

(defun jf/gptel-scope--write-yaml-plist (plist)
  "Write PLIST as YAML to current buffer.
Handles nested structures for paths, org_roam_patterns, shell_commands, and bash_tools.
Converts kebab-case keys to snake_case for YAML output."
  (cl-loop for (key value) on plist by #'cddr
           do (let ((key-name (jf/gptel-scope--kebab-to-snake key)))
                (cond
                 ;; Handle paths (nested structure)
                 ((eq key :paths)
                  (insert (format "%s:\n" key-name))
                  (cl-loop for (subkey subvalue) on value by #'cddr
                           do (let ((subkey-name (jf/gptel-scope--kebab-to-snake subkey)))
                                (insert (format "  %s:\n" subkey-name))
                                (if subvalue
                                    (dolist (item subvalue)
                                      (insert (format "    - \"%s\"\n" item)))
                                  (insert "    []\n")))))

                 ;; Handle org_roam_patterns (nested structure)
                 ((eq key :org-roam-patterns)
                  (insert (format "%s:\n" key-name))
                  (cl-loop for (subkey subvalue) on value by #'cddr
                           do (let ((subkey-name (jf/gptel-scope--kebab-to-snake subkey)))
                                (insert (format "  %s:\n" subkey-name))
                                (if subvalue
                                    (dolist (item subvalue)
                                      (insert (format "    - \"%s\"\n" item)))
                                  (insert "    []\n")))))

                 ;; Handle shell_commands (nested structure)
                 ((eq key :shell-commands)
                  (insert (format "%s:\n" key-name))
                  (cl-loop for (subkey subvalue) on value by #'cddr
                           do (let ((subkey-name (jf/gptel-scope--kebab-to-snake subkey)))
                                (insert (format "  %s:\n" subkey-name))
                                (if subvalue
                                    (dolist (item subvalue)
                                      (insert (format "    - \"%s\"\n" item)))
                                  (insert "    []\n")))))

                 ;; Handle bash_tools (nested structure with categories)
                 ((eq key :bash-tools)
                  (insert (format "%s:\n" key-name))
                  (cl-loop for (subkey subvalue) on value by #'cddr
                           do (let ((subkey-name (jf/gptel-scope--kebab-to-snake subkey)))
                                (cond
                                 ;; Handle categories (triple-nested)
                                 ((eq subkey :categories)
                                  (insert (format "  %s:\n" subkey-name))
                                  (cl-loop for (cat-key cat-value) on subvalue by #'cddr
                                           do (let ((cat-name (jf/gptel-scope--kebab-to-snake cat-key)))
                                                (insert (format "    %s:\n" cat-name))
                                                (cl-loop for (prop-key prop-value) on cat-value by #'cddr
                                                         do (let ((prop-name (jf/gptel-scope--kebab-to-snake prop-key)))
                                                              (insert (format "      %s:\n" prop-name))
                                                              (if prop-value
                                                                  (dolist (cmd prop-value)
                                                                    (insert (format "        - \"%s\"\n" cmd)))
                                                                (insert "        []\n")))))))
                                 ;; Handle deny (simple list)
                                 ((eq subkey :deny)
                                  (insert (format "  %s:\n" subkey-name))
                                  (if subvalue
                                      (dolist (item subvalue)
                                        (insert (format "    - \"%s\"\n" item)))
                                    (insert "    []\n")))))))

                 ;; Handle tools (can be list or nested map)
                 ((eq key :tools)
                  (insert (format "%s:\n" key-name))
                  (cond
                   ;; List of strings
                   ((and (listp value) (stringp (car value)))
                    (dolist (item value)
                      (insert (format "  - %s\n" item))))
                   ;; Nested map (tool-name: {allowed: true})
                   ((and (listp value) (keywordp (car value)))
                    (cl-loop for (tool-key tool-props) on value by #'cddr
                             do (let ((tool-name (jf/gptel-scope--kebab-to-snake tool-key)))
                                  (insert (format "  %s:\n" tool-name))
                                  (when (listp tool-props)
                                    (cl-loop for (prop-key prop-val) on tool-props by #'cddr
                                             do (let ((prop-name (jf/gptel-scope--kebab-to-snake prop-key)))
                                                  (insert (format "    %s: %s\n" prop-name prop-val))))))))))

                 ;; Simple string value
                 ((stringp value)
                  (insert (format "%s: \"%s\"\n" key-name value)))

                 ;; Simple number value
                 ((numberp value)
                  (insert (format "%s: %s\n" key-name value)))

                 ;; Simple symbol value
                 ((symbolp value)
                  (insert (format "%s: %s\n" key-name value)))

                 ;; Simple list of strings
                 ((and (listp value) (stringp (car value)))
                  (insert (format "%s:\n" key-name))
                  (dolist (item value)
                    (insert (format "  - %s\n" item))))

                 ;; Unknown structure - fail loudly
                 (t
                  (error "Unknown YAML structure for key '%s': %S. Expected known nested structure or simple value"
                         key-name value))))))

(defun jf/gptel-scope-prompt-expansion (violation-info callback patterns tool-name)
  "Show expansion UI for VIOLATION-INFO.
CALLBACK is the gptel async callback to invoke with JSON result.
PATTERNS is the list of patterns to add if approved.
TOOL-NAME is the tool requesting expansion.
VIOLATION-INFO is a plist with :tool, :resource, :reason, :validation-type.

This is a public API function used by scope-shell-tools and other modules."
  (transient-setup 'jf/gptel-scope-expansion-menu nil nil
                   :scope (list :violation violation-info
                               :callback callback
                               :patterns patterns
                               :tool-name tool-name)))

(provide 'jf-gptel-scope-expansion)
;;; scope-expansion.el ends here
