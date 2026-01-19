;; Dependencies


;; [[file:scope-filesystem-tools.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-core)
;; Dependencies:1 ends here

;; Read File Tool (No Scope Check)

;; Read operations are always allowed regardless of scope plan.


;; [[file:scope-filesystem-tools.org::*Read File Tool (No Scope Check)][Read File Tool (No Scope Check):1]]
(gptel-make-tool
 :name "read_file"
 :description "Read contents of a file at the specified path.
Always allowed regardless of scope plan (read operations are unrestricted).
Use this to read any file content you need to examine."
 :args (list '(:name "path"
               :type string
               :description "Directory path")
             '(:name "filename"
               :type string
               :description "Name of file"))
 :category "filesystem"
 :function
 (lambda (path filename)
   (let ((full-path (expand-file-name filename path)))
     (if (file-exists-p full-path)
         (list :success t
               :content (with-temp-buffer
                         (insert-file-contents full-path)
                         (buffer-string))
               :full_path full-path)
       (list :success nil
             :error "file_not_found"
             :message (format "File not found: %s" full-path))))))
;; Read File Tool (No Scope Check):1 ends here

;; Write File Tool (Scope-Aware)

;; Write content to a file, checking scope before execution.


;; [[file:scope-filesystem-tools.org::*Write File Tool (Scope-Aware)][Write File Tool (Scope-Aware):1]]
(gptel-make-tool
 :name "write_file_in_scope"
 :description "Write content to a file, respecting scope plan.
Returns error if path is not in approved scope patterns.
Use request_scope_expansion tool to ask user for approval if needed.

Example error response:
{:success nil :error \"scope_violation\" :resource \"/path/to/file\"
 :allowed_patterns [\"/approved/**/*.el\"]
 :message \"Write operation denied. Use request_scope_expansion to ask user.\"}

When you receive a scope violation:
1. Check the allowed_patterns to see what IS approved
2. Try an alternative path that matches allowed patterns, OR
3. Use request_scope_expansion to ask user to add the path to scope"
 :args (list '(:name "path"
               :type string
               :description "Directory path")
             '(:name "filename"
               :type string
               :description "Name of file")
             '(:name "content"
               :type string
               :description "File contents to write"))
 :category "filesystem"
 :function
 (lambda (path filename content)
   (let* ((full-path (expand-file-name filename path))
          (session-id (jf/gptel-scope--get-session-id))
          (plan (jf/gptel-scope--load-plan session-id)))

     (unless plan
       ;; No plan = deny by default
       (cl-return-from nil
         (list :success nil
               :error "no_scope_plan"
               :message "No scope plan found for this session. Create one with M-x jf/gptel-scope-init-plan or ask user to create one.")))

     ;; Check scope
     (let ((check-result (jf/gptel-scope--check-filesystem-write plan full-path)))
       (if (plist-get check-result :allowed)
           ;; Allowed - execute write
           (progn
             ;; Ensure directory exists
             (let ((dir (file-name-directory full-path)))
               (unless (file-exists-p dir)
                 (make-directory dir t)))
             ;; Write file
             (with-temp-file full-path
               (insert content))
             (list :success t
                   :full_path full-path
                   :message (format "File written: %s" full-path)))

         ;; Denied - return error with scope info
         (jf/gptel-scope--format-error "filesystem" "write" full-path check-result))))))
;; Write File Tool (Scope-Aware):1 ends here

;; Edit File Tool (Scope-Aware)

;; Edit a file by replacing strings, checking scope before execution.


;; [[file:scope-filesystem-tools.org::*Edit File Tool (Scope-Aware)][Edit File Tool (Scope-Aware):1]]
(gptel-make-tool
 :name "edit_file_in_scope"
 :description "Edit a file by replacing old_string with new_string, respecting scope plan.
Returns error if path is not in approved scope patterns.

The file must already exist. Use write_file_in_scope to create new files.

Example usage:
- Replace function implementation
- Update variable values
- Modify configuration settings

Returns scope violation error if path not approved."
 :args (list '(:name "filepath"
               :type string
               :description "Full path to file (can be relative or absolute)")
             '(:name "old_string"
               :type string
               :description "Text to replace (must match exactly)")
             '(:name "new_string"
               :type string
               :description "Replacement text"))
 :category "filesystem"
 :function
 (lambda (filepath old-string new-string)
   (let* ((full-path (expand-file-name filepath))
          (session-id (jf/gptel-scope--get-session-id))
          (plan (jf/gptel-scope--load-plan session-id)))

     (unless plan
       (cl-return-from nil
         (list :success nil
               :error "no_scope_plan"
               :message "No scope plan found for this session.")))

     ;; Check if file exists
     (unless (file-exists-p full-path)
       (cl-return-from nil
         (list :success nil
               :error "file_not_found"
               :message (format "File does not exist: %s. Use write_file_in_scope to create new files." full-path))))

     ;; Check scope
     (let ((check-result (jf/gptel-scope--check-filesystem-write plan full-path)))
       (if (plist-get check-result :allowed)
           ;; Allowed - execute edit
           (let ((replaced nil))
             (with-temp-file full-path
               (insert-file-contents full-path)
               (goto-char (point-min))
               (if (search-forward old-string nil t)
                   (progn
                     (replace-match new-string t t)
                     (setq replaced t))
                 (cl-return-from nil
                   (list :success nil
                         :error "string_not_found"
                         :message (format "String not found in file: '%s'" old-string)))))
             (when replaced
               (list :success t
                     :full_path full-path
                     :message "File edited successfully")))

         ;; Denied - return error
         (jf/gptel-scope--format-error "filesystem" "write" full-path check-result))))))
;; Edit File Tool (Scope-Aware):1 ends here

;; Provide Feature


;; [[file:scope-filesystem-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-filesystem-tools)
;;; scope-filesystem-tools.el ends here
;; Provide Feature:1 ends here
