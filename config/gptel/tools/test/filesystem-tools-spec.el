;;; filesystem-tools-spec.el --- Behavioral tests for filesystem tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; BEHAVIORAL TESTS: filesystem tools (read_file, write_file_in_scope,
;; edit_file_in_scope, list_directory)
;;
;; Tests verify the actual tool function behavior by calling the registered
;; tool functions and checking what happens.
;;
;; Key behaviors under test:
;; - On scope validation success: tool executes its file operation
;; - On scope validation failure: tool triggers expansion UI (not just returns error)
;; - After user approves: tool retries and executes on success
;; - After user denies: tool returns error without touching filesystem
;;
;; Mocked at system boundaries:
;; - jf/gptel-scope--load-config: scope.yml loading
;; - jf/gptel-scope--check-tool-permission: path validation dispatch
;; - jf/gptel-scope--gather-file-metadata: git/fs metadata
;; - jf/gptel-scope--check-allow-once: pre-existing allow-once grants
;; - jf/gptel-scope--trigger-inline-expansion: simulates user choice
;;
;; Real filesystem operations (temp files) used for success path tests.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tools-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name "../scope" tools-dir)))
  (add-to-list 'load-path test-dir)
  (require 'tool-test-helpers-spec (expand-file-name "helpers-spec.el" test-dir))
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir))
  (require 'jf-gptel-scope-filesystem-tools (expand-file-name "scope-filesystem-tools.el" scope-dir)))

;;; Test Helpers

(defun fs-tools-spec--get-tool-fn (name)
  "Get registered tool function for NAME from the 'filesystem' category."
  (when-let* ((fs-tools (alist-get "filesystem" gptel--known-tools nil nil #'equal))
              (tool (alist-get name fs-tools nil nil #'equal)))
    (gptel-tool-function tool)))

(defun fs-tools-spec--write-temp-file (content)
  "Create a temp file with CONTENT, return its path."
  (let ((temp-file (make-temp-file "fs-spec-" nil ".txt")))
    (with-temp-file temp-file (insert content))
    temp-file))

(defmacro fs-tools-spec--with-temp-file (var content &rest body)
  "Bind VAR to a temp file containing CONTENT, run BODY, then delete it."
  (declare (indent 2))
  `(let ((,var (fs-tools-spec--write-temp-file ,content)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(defmacro fs-tools-spec--with-temp-dir (var &rest body)
  "Bind VAR to a new temp directory, run BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (make-temp-file "fs-spec-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var) (delete-directory ,var t)))))

;;; Shared mock setup

(defun fs-tools-spec--setup-common-mocks ()
  "Set up mocks common to all filesystem tool tests."
  (spy-on 'jf/gptel-scope--load-config
          :and-return-value (tool-test--scope-config-minimal))
  (spy-on 'jf/gptel-scope--gather-file-metadata :and-return-value nil)
  (spy-on 'jf/gptel-scope--check-allow-once :and-return-value nil))

;;; Test Suite: read_file

(describe "read_file: behavioral contract"

  (describe "tool is registered"

    (it "is registered in gptel--known-tools under 'filesystem' category"
      (let* ((fs-tools (alist-get "filesystem" gptel--known-tools nil nil #'equal))
             (tool (alist-get "read_file" fs-tools nil nil #'equal)))
        (expect tool :not :to-be nil)
        (expect (gptel-tool-async tool) :to-be t)))

    (it "has path validation with read operation in tool categories"
      (let ((category (cdr (assoc "read_file" jf/gptel-scope--tool-categories))))
        (expect (plist-get category :validation) :to-equal 'path)
        (expect (plist-get category :operation) :to-equal 'read))))

  (describe "on validation success"

    (before-each
      (fs-tools-spec--setup-common-mocks)
      (spy-on 'jf/gptel-scope--check-tool-permission
              :and-return-value (tool-test--scope-allowed)))

    (it "reads and returns file content"
      (fs-tools-spec--with-temp-file temp-file "hello world"
        (let* ((tool-fn (fs-tools-spec--get-tool-fn "read_file"))
               (received nil)
               (callback (lambda (json)
                           (setq received (json-parse-string json :object-type 'plist)))))
          (funcall tool-fn callback temp-file)
          (expect (plist-get received :success) :to-be t)
          (expect (plist-get received :content) :to-equal "hello world"))))

    (it "returns file_not_found error when file does not exist"
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "read_file"))
             (received nil)
             (callback (lambda (json)
                         (setq received (json-parse-string json :object-type 'plist)))))
        (funcall tool-fn callback "/nonexistent/path/does-not-exist.txt")
        (expect (plist-get received :success) :to-be nil)
        (expect (plist-get received :error) :to-equal "file_not_found")))

    (it "does not call trigger-inline-expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (fs-tools-spec--with-temp-file temp-file "data"
        (let* ((tool-fn (fs-tools-spec--get-tool-fn "read_file"))
               (callback (lambda (_json) nil)))
          (funcall tool-fn callback temp-file)
          (expect 'jf/gptel-scope--trigger-inline-expansion :not :to-have-been-called)))))

  (describe "on validation failure"

    (before-each
      (fs-tools-spec--setup-common-mocks)
      (spy-on 'jf/gptel-scope--check-tool-permission
              :and-return-value (tool-test--scope-denied
                                 "path_out_of_scope" "/outside/file.txt" "read_file")))

    (it "triggers inline expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "read_file"))
             (callback (lambda (_json) nil)))
        (funcall tool-fn callback "/outside/file.txt")
        (expect 'jf/gptel-scope--trigger-inline-expansion :to-have-been-called)))

    (it "passes tool name 'read_file' to expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "read_file"))
             (callback (lambda (_json) nil)))
        (funcall tool-fn callback "/outside/file.txt")
        (let ((call-args (spy-calls-args-for 'jf/gptel-scope--trigger-inline-expansion 0)))
          (expect (nth 1 call-args) :to-equal "read_file"))))

    (it "does not read file before user decision"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (spy-on 'insert-file-contents)
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "read_file"))
             (callback (lambda (_json) nil)))
        (funcall tool-fn callback "/outside/file.txt")
        (expect 'insert-file-contents :not :to-have-been-called))))

  (describe "after user approves via expansion UI"

    (before-each
      (fs-tools-spec--setup-common-mocks)
      (spy-on 'jf/gptel-scope--trigger-inline-expansion
              :and-call-fake
              (lambda (_violation _tool wrapper-callback)
                (funcall wrapper-callback (list :approved t)))))

    (it "reads file after approval when retry passes"
      (let* ((call-count 0))
        (spy-on 'jf/gptel-scope--check-tool-permission
                :and-call-fake
                (lambda (&rest _)
                  (setq call-count (1+ call-count))
                  (if (= call-count 1) (tool-test--scope-denied) (tool-test--scope-allowed))))
        (fs-tools-spec--with-temp-file temp-file "approved content"
          (let* ((tool-fn (fs-tools-spec--get-tool-fn "read_file"))
                 (received nil)
                 (callback (lambda (json)
                             (setq received (json-parse-string json :object-type 'plist)))))
            (funcall tool-fn callback temp-file)
            (expect (plist-get received :success) :to-be t)
            (expect (plist-get received :content) :to-equal "approved content"))))))

  (describe "after user denies via expansion UI"

    (before-each
      (fs-tools-spec--setup-common-mocks)
      (spy-on 'jf/gptel-scope--check-tool-permission
              :and-return-value (tool-test--scope-denied
                                 "path_out_of_scope" "/outside.txt" "read_file"))
      (spy-on 'jf/gptel-scope--trigger-inline-expansion
              :and-call-fake
              (lambda (_violation _tool wrapper-callback)
                (funcall wrapper-callback (list :approved nil)))))

    (it "invokes callback with error response after denial"
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "read_file"))
             (received nil)
             (callback (lambda (json)
                         (setq received (json-parse-string json :object-type 'plist)))))
        (funcall tool-fn callback "/outside.txt")
        (expect (plist-get received :success) :to-be nil)))

    (it "does not read file after denial"
      (spy-on 'insert-file-contents)
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "read_file"))
             (callback (lambda (_json) nil)))
        (funcall tool-fn callback "/outside.txt")
        (expect 'insert-file-contents :not :to-have-been-called)))))


;;; Test Suite: write_file_in_scope

(describe "write_file_in_scope: behavioral contract"

  (describe "tool is registered"

    (it "is registered with write operation"
      (let ((category (cdr (assoc "write_file_in_scope" jf/gptel-scope--tool-categories))))
        (expect (plist-get category :validation) :to-equal 'path)
        (expect (plist-get category :operation) :to-equal 'write))))

  (describe "on validation success"

    (before-each
      (fs-tools-spec--setup-common-mocks)
      (spy-on 'jf/gptel-scope--check-tool-permission
              :and-return-value (tool-test--scope-allowed)))

    (it "creates file with specified content"
      (fs-tools-spec--with-temp-dir temp-dir
        (let* ((target-file (expand-file-name "newfile.txt" temp-dir))
               (tool-fn (fs-tools-spec--get-tool-fn "write_file_in_scope"))
               (received nil)
               (callback (lambda (json)
                           (setq received (json-parse-string json :object-type 'plist)))))
          (funcall tool-fn callback target-file "written content")
          (expect (plist-get received :success) :to-be t)
          (expect (file-exists-p target-file) :to-be t)
          (expect (with-temp-buffer
                    (insert-file-contents target-file)
                    (buffer-string))
                  :to-equal "written content"))))

    (it "does not call trigger-inline-expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (fs-tools-spec--with-temp-dir temp-dir
        (let* ((target-file (expand-file-name "newfile.txt" temp-dir))
               (tool-fn (fs-tools-spec--get-tool-fn "write_file_in_scope"))
               (callback (lambda (_json) nil)))
          (funcall tool-fn callback target-file "content")
          (expect 'jf/gptel-scope--trigger-inline-expansion :not :to-have-been-called)))))

  (describe "on validation failure"

    (before-each
      (fs-tools-spec--setup-common-mocks)
      (spy-on 'jf/gptel-scope--check-tool-permission
              :and-return-value (tool-test--scope-denied
                                 "path_out_of_scope" "/outside/file.txt" "write_file_in_scope")))

    (it "triggers inline expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "write_file_in_scope"))
             (callback (lambda (_json) nil)))
        (funcall tool-fn callback "/outside/file.txt" "content")
        (expect 'jf/gptel-scope--trigger-inline-expansion :to-have-been-called)))

    (it "does not write file before user decision"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (spy-on 'write-region)
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "write_file_in_scope"))
             (callback (lambda (_json) nil)))
        (funcall tool-fn callback "/outside/file.txt" "content")
        (expect 'write-region :not :to-have-been-called))))

  (describe "after user denies"

    (before-each
      (fs-tools-spec--setup-common-mocks)
      (spy-on 'jf/gptel-scope--check-tool-permission
              :and-return-value (tool-test--scope-denied
                                 "path_out_of_scope" "/outside.txt" "write_file_in_scope"))
      (spy-on 'jf/gptel-scope--trigger-inline-expansion
              :and-call-fake
              (lambda (_violation _tool wrapper-callback)
                (funcall wrapper-callback (list :approved nil)))))

    (it "returns error without writing"
      (spy-on 'write-region)
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "write_file_in_scope"))
             (received nil)
             (callback (lambda (json)
                         (setq received (json-parse-string json :object-type 'plist)))))
        (funcall tool-fn callback "/outside.txt" "content")
        (expect (plist-get received :success) :to-be nil)
        (expect 'write-region :not :to-have-been-called)))))


;;; Test Suite: edit_file_in_scope

(describe "edit_file_in_scope: behavioral contract"

  (describe "tool is registered"

    (it "is registered with write operation"
      (let ((category (cdr (assoc "edit_file_in_scope" jf/gptel-scope--tool-categories))))
        (expect (plist-get category :validation) :to-equal 'path)
        (expect (plist-get category :operation) :to-equal 'write))))

  (describe "on validation success"

    (before-each
      (fs-tools-spec--setup-common-mocks)
      (spy-on 'jf/gptel-scope--check-tool-permission
              :and-return-value (tool-test--scope-allowed)))

    (it "replaces old_string with new_string in file"
      (fs-tools-spec--with-temp-file temp-file "hello old world"
        (let* ((tool-fn (fs-tools-spec--get-tool-fn "edit_file_in_scope"))
               (received nil)
               (callback (lambda (json)
                           (setq received (json-parse-string json :object-type 'plist)))))
          (funcall tool-fn callback temp-file "old" "new")
          (expect (plist-get received :success) :to-be t)
          (expect (with-temp-buffer
                    (insert-file-contents temp-file)
                    (buffer-string))
                  :to-equal "hello new world"))))

    (it "returns file_not_found when file does not exist"
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "edit_file_in_scope"))
             (received nil)
             (callback (lambda (json)
                         (setq received (json-parse-string json :object-type 'plist)))))
        (funcall tool-fn callback "/nonexistent/path/file.txt" "old" "new")
        (expect (plist-get received :success) :to-be nil)
        (expect (plist-get received :error) :to-equal "file_not_found")))

    (it "returns string_not_found when old_string is not in file"
      (fs-tools-spec--with-temp-file temp-file "actual content"
        (let* ((tool-fn (fs-tools-spec--get-tool-fn "edit_file_in_scope"))
               (received nil)
               (callback (lambda (json)
                           (setq received (json-parse-string json :object-type 'plist)))))
          (funcall tool-fn callback temp-file "not present" "replacement")
          (expect (plist-get received :success) :to-be nil)
          (expect (plist-get received :error) :to-equal "string_not_found")))))

  (describe "on validation failure"

    (before-each
      (fs-tools-spec--setup-common-mocks)
      (spy-on 'jf/gptel-scope--check-tool-permission
              :and-return-value (tool-test--scope-denied
                                 "path_out_of_scope" "/outside/file.txt" "edit_file_in_scope")))

    (it "triggers inline expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let* ((tool-fn (fs-tools-spec--get-tool-fn "edit_file_in_scope"))
             (callback (lambda (_json) nil)))
        (funcall tool-fn callback "/outside/file.txt" "old" "new")
        (expect 'jf/gptel-scope--trigger-inline-expansion :to-have-been-called)))

    (it "does not modify file before user decision"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (fs-tools-spec--with-temp-file temp-file "original content"
        (let* ((tool-fn (fs-tools-spec--get-tool-fn "edit_file_in_scope"))
               (callback (lambda (_json) nil)))
          (funcall tool-fn callback temp-file "original" "modified")
          (expect (with-temp-buffer
                    (insert-file-contents temp-file)
                    (buffer-string))
                  :to-equal "original content"))))))


;;; Test Suite: list_directory (non-scoped sync tool)

(describe "list_directory: behavioral contract"

  (it "lists files in an existing directory"
    (fs-tools-spec--with-temp-dir temp-dir
      (with-temp-file (expand-file-name "alpha.txt" temp-dir) (insert "a"))
      (with-temp-file (expand-file-name "beta.txt" temp-dir) (insert "b"))
      (let* ((fs-tools (alist-get "filesystem" gptel--known-tools nil nil #'equal))
             (tool (alist-get "list_directory" fs-tools nil nil #'equal))
             (tool-fn (gptel-tool-function tool))
             (result (funcall tool-fn temp-dir)))
        (expect result :to-match "alpha.txt")
        (expect result :to-match "beta.txt"))))

  (it "returns error message for non-existent directory"
    (let* ((fs-tools (alist-get "filesystem" gptel--known-tools nil nil #'equal))
           (tool (alist-get "list_directory" fs-tools nil nil #'equal))
           (tool-fn (gptel-tool-function tool))
           (result (funcall tool-fn "/nonexistent/directory/path")))
      (expect result :to-match "Error"))))

(provide 'filesystem-tools-spec)

;;; filesystem-tools-spec.el ends here
