;;; scope-profiles.el --- GPTEL Scope Profiles -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Scope profile templates for gptel sessions.
;; Loads YAML profile templates, resolves them for presets,
;; expands variables, and writes scope.yml into session directories.

;;; Code:

(require 'cl-lib)
(require 'yaml)
(require 'gptel-session-constants)
(require 'gptel-session-logging)

(defun jf/gptel-scope-profile--normalize-keys (parsed)
  "Normalize snake_case keys in PARSED plist to kebab-case.
Recursively processes nested plists.

NOTE: Duplicated in preset-registration.org and scope-shell-tools.org
due to module loading dependencies."
  (let ((result nil))
    (cl-loop for (key val) on parsed by #'cddr
             do (let* ((key-name (symbol-name key))
                       (normalized-name (replace-regexp-in-string "_" "-" key-name))
                       (normalized-key (intern normalized-name))
                       (normalized-val (if (and (listp val)
                                               (not (null val))
                                               (keywordp (car val)))
                                          (jf/gptel-scope-profile--normalize-keys val)
                                        val)))
                  (setq result (plist-put result normalized-key normalized-val))))
    result))

(defun jf/gptel-scope-profile--kebab-to-snake (key)
  "Convert KEY from kebab-case keyword to snake_case keyword.
E.g., :org-roam-patterns becomes :org_roam_patterns."
  (intern (replace-regexp-in-string "-" "_" (symbol-name key))))

(defun jf/gptel-scope-profile--load (profile-name)
  "Load scope profile PROFILE-NAME from the profiles directory.
PROFILE-NAME is the base name without .yml extension.
Returns a plist with :paths, :org-roam-patterns, :shell-commands, :bash-tools.
Returns nil and logs a warning if the file is missing or cannot be parsed.

VALIDATION: Rejects profiles with bash_tools.categories section.
Migration: Remove categories section, keep only deny list."
  (let ((profile-file (expand-file-name
                       (concat profile-name ".yml")
                       jf/gptel--scope-profiles-directory)))
    (if (not (file-exists-p profile-file))
        (progn
          (jf/gptel--log 'warn "Scope profile file not found: %s" profile-file)
          nil)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents profile-file)
            (let* ((parsed (yaml-parse-string (buffer-string) :object-type 'plist))
                   (normalized (jf/gptel-scope-profile--normalize-keys parsed))
                   (bash-tools (plist-get normalized :bash-tools))
                   (categories (when bash-tools (plist-get bash-tools :categories))))
              ;; Validate: reject if categories section present
              (when categories
                (error "bash_tools.categories section no longer supported. Migration: Remove categories section, keep only deny list. See CLAUDE.md for migration guide"))
              (jf/gptel--log 'debug "Loaded scope profile: %s" profile-name)
              normalized))
        (error
         (jf/gptel--log 'warn "Failed to parse scope profile %s: %s"
                       profile-name (error-message-string err))
         nil)))))

(defun jf/gptel-scope-profile--resolve (preset-name)
  "Resolve scope profile for PRESET-NAME.
Looks up `jf/gptel-preset--scope-defaults' for the preset.
Priority:
  1. Named profile via :scope-profile key -> load from file
  2. Inline scope defaults (plist with :paths etc.) -> return directly
  3. No scope defaults -> return nil

Returns a scope plist or nil."
  (let ((scope-config (alist-get preset-name jf/gptel-preset--scope-defaults)))
    (cond
     ;; No scope configuration for this preset
     ((null scope-config)
      (jf/gptel--log 'debug "No scope defaults for preset: %s" preset-name)
      nil)

     ;; Named profile reference
     ((plist-get scope-config :scope-profile)
      (let ((profile-name (plist-get scope-config :scope-profile)))
        (jf/gptel--log 'debug "Resolving named scope profile '%s' for preset: %s"
                      profile-name preset-name)
        (let ((loaded (jf/gptel-scope-profile--load profile-name)))
          (unless loaded
            (jf/gptel--log 'warn "Profile file missing for preset %s, profile: %s"
                          preset-name profile-name))
          loaded)))

     ;; Inline scope defaults (the plist itself is the scope config)
     (t
      (jf/gptel--log 'debug "Using inline scope defaults for preset: %s" preset-name)
      scope-config))))

(defun jf/gptel-scope-profile--expand-string (str project-root)
  "Expand ${project_root} in STR with PROJECT-ROOT.
If PROJECT-ROOT is nil and STR contains ${project_root}, return nil
to signal the pattern should be removed."
  (if (not (stringp str))
      str
    (if (string-match-p "\\${project_root}" str)
        (if project-root
            (replace-regexp-in-string "\\${project_root}" project-root str t t)
          nil)
      str)))

(defun jf/gptel-scope-profile--expand-list (lst project-root)
  "Expand variables in all strings in LST.
Remove entries where expansion returns nil (unresolvable variables).
Returns a new list."
  (if (not (listp lst))
      lst
    (cl-remove nil (mapcar (lambda (item)
                             (jf/gptel-scope-profile--expand-string item project-root))
                           lst))))

(defun jf/gptel-scope-profile--expand-variables (scope-plist project-root)
  "Expand ${project_root} in all string values within SCOPE-PLIST.
If PROJECT-ROOT is nil, patterns containing ${project_root} are removed
and a warning is logged.
Returns a new plist with expanded values."
  (when (and (null project-root)
             scope-plist)
    (jf/gptel--log 'warn "No project-root provided; patterns with ${project_root} will be removed"))
  (let ((result nil))
    (cl-loop for (key val) on scope-plist by #'cddr
             do (let ((expanded
                       (cond
                        ;; Nested plist (e.g., :paths has :read, :write, :deny)
                        ((and (listp val) (keywordp (car-safe val)))
                         (jf/gptel-scope-profile--expand-variables val project-root))
                        ;; List of strings (e.g., list of patterns)
                        ((and (listp val) (or (null val) (stringp (car-safe val))))
                         (jf/gptel-scope-profile--expand-list val project-root))
                        ;; Vector (yaml arrays sometimes parse as vectors)
                        ((vectorp val)
                         (jf/gptel-scope-profile--expand-list (append val nil) project-root))
                        ;; Single string
                        ((stringp val)
                         (or (jf/gptel-scope-profile--expand-string val project-root) ""))
                        ;; Anything else, pass through
                        (t val))))
                  (setq result (plist-put result key expanded))))
    result))

(defun jf/gptel-scope-profile--plist-to-yaml (plist indent)
  "Convert PLIST to YAML string with INDENT level.
Converts kebab-case keys back to snake_case for on-disk format.
Handles nested plists and lists of strings."
  (let ((lines nil)
        (prefix (make-string (* indent 2) ?\s)))
    (cl-loop for (key val) on plist by #'cddr
             do (let ((yaml-key (substring
                                 (symbol-name
                                  (jf/gptel-scope-profile--kebab-to-snake key))
                                 1)))
                  (cond
                   ;; Nested plist
                   ((and (listp val) (keywordp (car-safe val)))
                    (push (format "%s%s:" prefix yaml-key) lines)
                    (push (jf/gptel-scope-profile--plist-to-yaml val (1+ indent)) lines))
                   ;; List of strings - inline YAML array
                   ((and (listp val) (or (null val) (stringp (car-safe val))))
                    (if (null val)
                        (push (format "%s%s: []" prefix yaml-key) lines)
                      (let ((items (mapconcat (lambda (s) (format "\"%s\"" s)) val ", ")))
                        (push (format "%s%s: [%s]" prefix yaml-key items) lines))))
                   ;; Single string
                   ((stringp val)
                    (push (format "%s%s: \"%s\"" prefix yaml-key val) lines))
                   ;; Nil
                   ((null val)
                    (push (format "%s%s: []" prefix yaml-key) lines))
                   ;; Anything else
                   (t
                    (push (format "%s%s: %s" prefix yaml-key val) lines)))))
    (string-join (nreverse lines) "\n")))

(defun jf/gptel-scope-profile--write-scope-yml (target-dir scope-plist)
  "Write SCOPE-PLIST as YAML to scope.yml in TARGET-DIR.
Uses `jf/gptel-session--scope-file' constant for the filename.
If SCOPE-PLIST is nil or empty, writes minimal YAML with empty sections."
  (let ((scope-file (expand-file-name jf/gptel-session--scope-file target-dir))
        (effective-plist (or scope-plist
                             (list :paths (list :read nil :write nil :deny nil)
                                   :org-roam-patterns (list :subdirectory nil :tags nil :node-ids nil)
                                   :bash-tools (list :categories (list :read-only (list :commands nil)
                                                                       :safe-write (list :commands nil)
                                                                       :dangerous (list :commands nil))
                                                     :deny nil)))))
    (with-temp-file scope-file
      (insert (jf/gptel-scope-profile--plist-to-yaml effective-plist 0))
      (insert "\n"))
    (jf/gptel--log 'info "Wrote scope file: %s" scope-file)))

(defun jf/gptel-scope-profile--merge-lists (list1 list2)
  "Merge LIST1 and LIST2, removing duplicates.
Preserves order: LIST1 items first, then new items from LIST2.
Returns nil if both lists are nil."
  (when (or list1 list2)
    (let ((result (copy-sequence list1)))
      (dolist (item list2)
        (unless (member item result)
          (setq result (append result (list item)))))
      result)))

(defun jf/gptel-scope-profile--deep-merge (base override)
  "Deep merge two plists, with OVERRIDE taking precedence.

Merging rules:
- Nested plists: recursively merge
- Lists: concatenate and deduplicate (base first, then override)
- Scalars: override wins
- nil: treated as absence (override's nil doesn't clear base's value)

Returns a new plist with merged values.

This is schema-agnostic - works for any plist structure including
future additions to scope configuration."
  (if (null override)
      base
    (let ((result (copy-sequence base)))
      ;; Iterate over all keys in override
      (cl-loop for (key val) on override by #'cddr
               do (let ((base-val (plist-get base key)))
                    (setq result
                          (plist-put result key
                                     (cond
                                      ;; Both are plists: recursively merge
                                      ((and (listp base-val) (listp val)
                                            (keywordp (car-safe base-val))
                                            (keywordp (car-safe val)))
                                       (jf/gptel-scope-profile--deep-merge base-val val))

                                      ;; Both are lists (not plists): merge and deduplicate
                                      ((and (listp base-val) (listp val)
                                            (not (keywordp (car-safe base-val)))
                                            (not (keywordp (car-safe val))))
                                       (jf/gptel-scope-profile--merge-lists base-val val))

                                      ;; Override is nil: keep base value (nil means absence)
                                      ((null val)
                                       base-val)

                                      ;; Otherwise: override wins
                                      (t val))))))
      result)))

(defun jf/gptel-scope-profile--create-for-session (preset-name target-dir &optional project-root worktree-paths)
  "Create scope.yml for a session in TARGET-DIR.
PRESET-NAME is used to resolve the scope profile.
PROJECT-ROOT is used for variable expansion.
WORKTREE-PATHS, if provided, is deep-merged with the preset's scope
configuration (paths are concatenated, bash_tools preserved, etc.).

Flow:
  1. Resolve profile for PRESET-NAME (get bash_tools, org_roam_patterns, etc.)
  2. Expand variables with PROJECT-ROOT
  3. If WORKTREE-PATHS provided: deep-merge into preset's scope config
  4. Write scope.yml to TARGET-DIR

Deep merge is schema-agnostic and works for any future scope configuration:
- Lists (read/write paths): concatenated and deduplicated
- Nested plists: recursively merged
- Scalars: worktree-paths wins"
  (let* ((resolved (jf/gptel-scope-profile--resolve preset-name))
         (expanded (when resolved
                     (jf/gptel-scope-profile--expand-variables resolved project-root)))
         (scope-plist
          (cond
           ;; Deep merge worktree paths with preset's scope configuration
           ((and worktree-paths expanded)
            (jf/gptel--log 'info "Deep-merging worktree paths with preset scope configuration")
            (jf/gptel-scope-profile--deep-merge expanded worktree-paths))

           ;; Only worktree paths (no preset scope config)
           (worktree-paths
            (jf/gptel--log 'info "Using explicit worktree paths (preset has no scope config)")
            worktree-paths)

           ;; Only preset scope config (no worktree paths)
           (expanded
            (jf/gptel--log 'info "Using preset scope configuration")
            expanded)

           ;; Neither available
           (t
            (jf/gptel--log 'warn "No scope configuration available for preset: %s" preset-name)
            nil))))
    (jf/gptel-scope-profile--write-scope-yml target-dir scope-plist)))

(provide 'gptel-scope-profiles)
;;; scope-profiles.el ends here
