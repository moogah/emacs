;;; test-violation-info-spec.el --- Tests for violation info construction -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Unit tests for jf/gptel-scope--build-violation-info function.
;; Verifies correct transformation of validation errors into violation-info plists.

;;; Code:

(require 'buttercup)
(require 'jf-gptel-scope-core)

(describe "jf/gptel-scope--infer-validation-type"
  (it "returns path for read_file tool"
    (expect (jf/gptel-scope--infer-validation-type "read_file")
            :to-equal 'path))

  (it "returns path for write_file_in_scope tool"
    (expect (jf/gptel-scope--infer-validation-type "write_file_in_scope")
            :to-equal 'path))

  (it "returns pattern for create_roam_node_in_scope tool"
    (expect (jf/gptel-scope--infer-validation-type "create_roam_node_in_scope")
            :to-equal 'pattern))

  (it "returns bash for run_bash_command tool"
    (expect (jf/gptel-scope--infer-validation-type "run_bash_command")
            :to-equal 'bash))

  (it "returns meta for request_scope_expansion tool"
    (expect (jf/gptel-scope--infer-validation-type "request_scope_expansion")
            :to-equal 'meta))

  (it "returns nil for unknown tool"
    (expect (jf/gptel-scope--infer-validation-type "nonexistent_tool")
            :to-be nil)))

(describe "jf/gptel-scope--build-violation-info"
  (describe "error type: path_out_of_scope"
    (it "extracts path as resource"
      (let* ((validation-error '(:error "path_out_of_scope"
                                 :path "/tmp/file.txt"
                                 :operation read
                                 :message "Path not in scope"))
             (result (jf/gptel-scope--build-violation-info validation-error "read_file")))
        (expect (plist-get result :tool) :to-equal "read_file")
        (expect (plist-get result :resource) :to-equal "/tmp/file.txt")
        (expect (plist-get result :operation) :to-equal 'read)
        (expect (plist-get result :reason) :to-equal "Path not in scope")
        (expect (plist-get result :validation-type) :to-equal 'path))))

  (describe "error type: command_denied"
    (it "extracts command as resource"
      (let* ((validation-error '(:error "command_denied"
                                 :command "rm -rf /tmp/file"
                                 :operation write
                                 :message "Command denied"))
             (result (jf/gptel-scope--build-violation-info validation-error "run_bash_command")))
        (expect (plist-get result :tool) :to-equal "run_bash_command")
        (expect (plist-get result :resource) :to-equal "rm -rf /tmp/file")
        (expect (plist-get result :operation) :to-equal 'write)
        (expect (plist-get result :reason) :to-equal "Command denied")
        (expect (plist-get result :validation-type) :to-equal 'bash))))

  (describe "error type: cloud_auth_denied"
    (it "extracts provider as resource"
      (let* ((validation-error '(:error "cloud_auth_denied"
                                 :provider "aws"
                                 :operation write
                                 :message "Cloud authentication denied"))
             (result (jf/gptel-scope--build-violation-info validation-error "run_bash_command")))
        (expect (plist-get result :tool) :to-equal "run_bash_command")
        (expect (plist-get result :resource) :to-equal "aws")
        (expect (plist-get result :operation) :to-equal 'write)
        (expect (plist-get result :reason) :to-equal "Cloud authentication denied")
        (expect (plist-get result :validation-type) :to-equal 'bash))))

  (describe "error type: incomplete_parse"
    (it "extracts command as resource"
      (let* ((validation-error '(:error "incomplete_parse"
                                 :command "echo 'incomplete"
                                 :operation write
                                 :message "Parse incomplete"))
             (result (jf/gptel-scope--build-violation-info validation-error "run_bash_command")))
        (expect (plist-get result :tool) :to-equal "run_bash_command")
        (expect (plist-get result :resource) :to-equal "echo 'incomplete")
        (expect (plist-get result :operation) :to-equal 'write)
        (expect (plist-get result :reason) :to-equal "Parse incomplete")
        (expect (plist-get result :validation-type) :to-equal 'bash))))

  (describe "default error type with :resource field"
    (it "extracts resource field when error type doesn't match specific cases"
      (let* ((validation-error '(:error "unknown_error"
                                 :resource "/some/path.txt"
                                 :operation read
                                 :message "Unknown error"))
             (result (jf/gptel-scope--build-violation-info validation-error "read_file")))
        (expect (plist-get result :tool) :to-equal "read_file")
        (expect (plist-get result :resource) :to-equal "/some/path.txt")
        (expect (plist-get result :operation) :to-equal 'read)
        (expect (plist-get result :reason) :to-equal "Unknown error")
        (expect (plist-get result :validation-type) :to-equal 'path))))

  (describe "default error type with :path field fallback"
    (it "falls back to :path when no :resource field"
      (let* ((validation-error '(:error "unknown_error"
                                 :path "/fallback/path.txt"
                                 :operation read
                                 :message "Unknown error"))
             (result (jf/gptel-scope--build-violation-info validation-error "read_file")))
        (expect (plist-get result :tool) :to-equal "read_file")
        (expect (plist-get result :resource) :to-equal "/fallback/path.txt")
        (expect (plist-get result :operation) :to-equal 'read)
        (expect (plist-get result :reason) :to-equal "Unknown error")
        (expect (plist-get result :validation-type) :to-equal 'path))))

  (describe "using :reason field instead of :error"
    (it "handles validation errors with :reason field"
      (let* ((validation-error '(:reason "directory-not-in-scope"
                                 :resource "/tmp/dir"
                                 :operation write
                                 :message "Directory not in scope"))
             (result (jf/gptel-scope--build-violation-info validation-error "run_bash_command")))
        (expect (plist-get result :tool) :to-equal "run_bash_command")
        (expect (plist-get result :resource) :to-equal "/tmp/dir")
        (expect (plist-get result :operation) :to-equal 'write)
        (expect (plist-get result :reason) :to-equal "Directory not in scope")
        (expect (plist-get result :validation-type) :to-equal 'bash))))

  (describe "all fields populated correctly"
    (it "returns plist with all required fields"
      (let* ((validation-error '(:error "path_out_of_scope"
                                 :path "/test/file.el"
                                 :operation read
                                 :message "Test message"))
             (result (jf/gptel-scope--build-violation-info validation-error "read_file")))
        (expect result :to-have-same-items-as
                (list :tool "read_file"
                      :resource "/test/file.el"
                      :operation 'read
                      :reason "Test message"
                      :validation-type 'path))))))

(provide 'test-violation-info-spec)
;;; test-violation-info-spec.el ends here
