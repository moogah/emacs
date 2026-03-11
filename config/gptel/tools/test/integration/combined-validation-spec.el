;;; combined-validation-spec.el --- Integration tests for combined validation scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INTEGRATION TESTS: Combined validation scenarios
;;
;; Tests validation when multiple validators are triggered simultaneously:
;; - Pipeline + file operations
;; - File operations + cloud auth
;; - Pipeline + file operations + cloud auth (all three layers)
;; - Parse incomplete with file operations (early exit)
;; - Coverage warning with successful validation (non-blocking)
;;
;; Test organization:
;; 1. Pipeline + file operations (1 test)
;; 2. File operations + cloud auth (1 test)
;; 3. All three layers together (1 test)
;; 4. Parse incomplete with file operations (1 test)
;; 5. Coverage warning with successful validation (1 test)
;;
;; Total: 5 tests
;;
;; These tests verify validators don't interfere with each other.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name "../.." test-dir))
       (tools-dir test-root-dir))
  (require 'bash-parser-core)
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Helper Functions

(defun test-combined--make-paths-config (&rest args)
  "Create paths configuration plist from keyword arguments."
  (list :read (plist-get args :read)
        :write (plist-get args :write)
        :execute (plist-get args :execute)
        :modify (plist-get args :modify)
        :deny (plist-get args :deny)))

(defun test-combined--make-test-config (&rest args)
  "Create full scope configuration from keyword arguments."
  (list :paths (or (plist-get args :paths)
                   (test-combined--make-paths-config))
        :bash-tools (or (plist-get args :bash-tools)
                        '(:deny ()))
        :cloud (or (plist-get args :cloud)
                   '(:auth-detection "warn"))
        :security (or (plist-get args :security)
                      '(:enforce-parse-complete t
                       :max-coverage-threshold 0.8))))

;;; Combined Validation Scenarios

(describe "Combined validation scenarios"

  (describe "pipeline + file operations"
    (it "validates both pipeline commands and file paths"
      (let* ((scope-config (test-combined--make-test-config
                            :paths (test-combined--make-paths-config
                                    :read '("/workspace/**"))
                            :bash-tools '(:deny ())))
             (result nil))
        ;; Mock parse to return pipeline
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "cat")
                                (:command-name "grep"))))
        ;; Mock semantics to return file operation
        (spy-on 'jf/bash-extract-semantics :and-return-value
                '(:domains (:filesystem ((:operation :read
                                         :path "/workspace/file.txt"
                                         :command-name "cat")))))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "cat /workspace/file.txt | grep pattern" "/workspace" scope-config))
        ;; Both pipeline and file operations valid - should pass
        (expect result :to-be nil))))

  (describe "file operations + cloud auth"
    (it "validates both file paths and cloud auth policy"
      (let* ((scope-config (test-combined--make-test-config
                            :paths (test-combined--make-paths-config
                                    :read '("/workspace/**"))
                            :cloud '(:auth-detection "warn")
                            :bash-tools '(:deny ())))
             (result nil))
        ;; Mock parse
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "aws-vault")
                                (:command-name "cat"))))
        ;; Mock semantics to return both file ops and cloud auth
        (spy-on 'jf/bash-extract-semantics :and-return-value
                '(:domains (:filesystem ((:operation :read
                                         :path "/workspace/file.txt"
                                         :command-name "cat"))
                           :cloud-auth (:provider :aws
                                       :command "aws-vault exec prod"))))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "aws-vault exec prod -- cat /workspace/file.txt" "/workspace" scope-config))
        ;; Should return warning plist (cloud auth in warn mode)
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
        (expect (plist-get result :provider) :to-equal :aws))))

  (describe "pipeline + file operations + cloud auth"
    (it "validates all three layers together"
      (let* ((scope-config (test-combined--make-test-config
                            :paths (test-combined--make-paths-config
                                    :read '("/workspace/**")
                                    :write '("/tmp/**"))
                            :cloud '(:auth-detection "allow")
                            :bash-tools '(:deny ())))
             (result nil))
        ;; Mock parse with complex pipeline
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "aws")
                                (:command-name "grep")
                                (:command-name "echo"))))
        ;; Mock semantics with all three domains
        (spy-on 'jf/bash-extract-semantics :and-return-value
                '(:domains (:filesystem ((:operation :read
                                         :path "/workspace/data.json"
                                         :command-name "aws")
                                        (:operation :write
                                         :path "/tmp/output.txt"
                                         :command-name "echo"))
                           :cloud-auth (:provider :aws
                                       :command "aws s3 ls"))))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "aws s3 cp s3://bucket/data.json /workspace/data.json | grep pattern | echo result > /tmp/output.txt"
                      "/workspace" scope-config))
        ;; All three layers valid - should pass
        (expect result :to-be nil))))

  (describe "parse incomplete with file operations"
    (it "fails on parse before checking file operations"
      (let* ((scope-config (test-combined--make-test-config
                            :security '(:enforce-parse-complete t
                                       :max-coverage-threshold 0.8)))
             (result nil))
        ;; Mock incomplete parse
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete nil :parse-errors ("Syntax error")))
        ;; File operations validator should not be called
        (spy-on 'jf/gptel-scope--validate-file-operations)
        (setq result (jf/gptel-scope--validate-command-semantics
                      "cat <" "/workspace" scope-config))
        ;; Should fail at stage 1 (parse)
        (expect (plist-get result :error) :to-equal "parse_incomplete")
        ;; File operations validator should not have been called (early exit)
        (expect 'jf/gptel-scope--validate-file-operations :not :to-have-been-called))))

  (describe "coverage warning with successful validation"
    (xit "warns about low coverage but allows command"
      (let* ((scope-config (test-combined--make-test-config
                            :security '(:max-coverage-threshold 0.8
                                       :enforce-parse-complete t)
                            :bash-tools '(:deny ())))
             (result nil)
             (warning-called nil))
        ;; Mock parse
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "ls"))))
        ;; Mock semantics with low coverage - coverage is direct property
        (spy-on 'jf/bash-extract-semantics :and-return-value
                '(:coverage (:coverage-ratio 0.6)
                  :domains nil))  ; Below 0.8 threshold
        ;; Capture warning
        (spy-on 'warn :and-call-fake (lambda (&rest args) (setq warning-called t)))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "ls" "/workspace" scope-config))
        ;; Should warn but still pass (stage 7 is non-blocking)
        (expect warning-called :to-be-truthy)
        (expect result :to-be nil)))))

(provide 'combined-validation-spec)

;;; combined-validation-spec.el ends here
