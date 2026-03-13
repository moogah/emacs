;;; scope-filesystem-tools.el --- GPTEL Scope Filesystem Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Scope-aware filesystem tools for gptel sessions.

;;; Code:

;; Dependencies


;; [[file:scope-filesystem-tools.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-core)
;; Dependencies:1 ends here

;; Git-Tracked File Validation

;; Check if a file is tracked by git. Used to enforce git-safe editing in project-aware sessions.


;; [[file:scope-filesystem-tools.org::*Git-Tracked File Validation][Git-Tracked File Validation:1]]
(defun jf/gptel--file-is-git-tracked-p (file)
  "Check if FILE is tracked by git.
Returns t if file is tracked, nil if untracked or ignored.
Returns nil if file is not in a git repository."
  (when-let* ((git-dir (locate-dominating-file file ".git")))
    (let ((default-directory git-dir)
          (relative-file (file-relative-name file git-dir)))
      (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" relative-file)))))
;; Git-Tracked File Validation:1 ends here

;; Read File Tool (Scope-Aware)

;; Read file contents, checking scope patterns. Templates typically grant very permissive read access.


;; [[file:scope-filesystem-tools.org::*Read File Tool (Scope-Aware)][Read File Tool (Scope-Aware):1]]
(gptel-make-scoped-tool
 "read_file"
 "Read contents of a file at the specified path.
Checks scope plan patterns before reading.
Most scope plans allow broad read access (e.g., all project files).
Use request_scope_expansion if you need to read files outside approved patterns."

 (list '(:name "filepath"
         :type string
         :description "Full path to file (can be relative or absolute)"))

 "filesystem"

 :async

 ;; Tool body - executed only if scope check passes
 (let ((full-path (expand-file-name filepath)))
   (if (file-exists-p full-path)
       (list :success t
             :content (with-temp-buffer
                       (insert-file-contents full-path)
                       (buffer-string))
             :full_path full-path)
     (list :success nil
           :error "file_not_found"
           :message (format "File not found: %s" full-path)))))
;; Read File Tool (Scope-Aware):1 ends here

;; Write File Tool (Scope-Aware)

;; Write content to a file, checking scope before execution.


;; [[file:scope-filesystem-tools.org::*Write File Tool (Scope-Aware)][Write File Tool (Scope-Aware):1]]
(gptel-make-scoped-tool
 "write_file_in_scope"
 "Write content to a file, respecting scope plan.
Returns error if path is not in approved scope patterns.
Use request_scope_expansion tool to ask user for approval if needed.

Example error response:
{:success nil :error \"scope_violation\" :tool \"write_file_in_scope\"
 :resource \"/path/to/file\" :allowed_patterns [\"/approved/**/*.el\"]
 :message \"Tool denied. Use request_scope_expansion to ask user.\"}

When you receive a scope violation:
1. Check the allowed_patterns to see what IS approved
2. Try an alternative path that matches allowed patterns, OR
3. Use request_scope_expansion to ask user to add the path to scope"

 (list '(:name "filepath"
         :type string
         :description "Full path to file (can be relative or absolute)")
       '(:name "content"
         :type string
         :description "File contents to write"))

 "filesystem"

 :async

 ;; Tool body - executed only if scope check passes
 (let ((full-path (expand-file-name filepath)))
   ;; Ensure directory exists
   (let ((dir (file-name-directory full-path)))
     (unless (file-exists-p dir)
       (make-directory dir t)))
   ;; Write file
   (with-temp-file full-path
     (insert content))
   (list :success t
         :full_path full-path
         :message (format "File written: %s" full-path))))
;; Write File Tool (Scope-Aware):1 ends here

;; Edit File Tool (Scope-Aware)

;; Edit a file by replacing strings, checking scope before execution.


;; [[file:scope-filesystem-tools.org::*Edit File Tool (Scope-Aware)][Edit File Tool (Scope-Aware):1]]
(gptel-make-scoped-tool
 "edit_file_in_scope"
 "Edit a file by replacing old_string with new_string, respecting scope plan.
Returns error if path is not in approved scope patterns.

The file must already exist. Use write_file_in_scope to create new files.

Example usage:
- Replace function implementation
- Update variable values
- Modify configuration settings

Returns scope violation error if path not approved."

 (list '(:name "filepath"
         :type string
         :description "Full path to file (can be relative or absolute)")
       '(:name "old_string"
         :type string
         :description "Text to replace (must match exactly)")
       '(:name "new_string"
         :type string
         :description "Replacement text"))

 "filesystem"

 :async

 ;; Tool body - executed only if scope check passes
 ;; Note: cl-return-from nil must NOT be used here - it would exit the macro's
 ;; outer cl-block nil, bypassing the funcall callback call entirely.
 ;; Use if/cond instead of early returns.
 (let ((full-path (expand-file-name filepath)))
   (if (not (file-exists-p full-path))
       (list :success nil
             :error "file_not_found"
             :message (format "File does not exist: %s. Use write_file_in_scope to create new files." full-path))
     ;; Read content, check for old_string, then write if found
     (let* ((original-content (with-temp-buffer
                                 (insert-file-contents full-path)
                                 (buffer-string)))
            (found (string-match-p (regexp-quote old_string) original-content)))
       (if (not found)
           (list :success nil
                 :error "string_not_found"
                 :message (format "String not found in file: '%s'" old_string))
         (with-temp-file full-path
           (insert (replace-regexp-in-string
                    (regexp-quote old_string) new_string original-content t t)))
         (list :success t
               :full_path full-path
               :message "File edited successfully"))))))
;; Edit File Tool (Scope-Aware):1 ends here

;; Provide Feature


;; [[file:scope-filesystem-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-filesystem-tools)
;;; scope-filesystem-tools.el ends here
;; Provide Feature:1 ends here
