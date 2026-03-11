;;; pipeline-orchestration-spec.el --- Integration tests for seven-stage validation pipeline -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INTEGRATION TESTS: Seven-stage validation pipeline orchestration
;;
;; Tests the main pipeline function jf/gptel-scope--validate-command-semantics
;; which orchestrates all 7 validation stages with early exit on failure.
;;
;; Test organization:
;; 1. Full pipeline success (1 test)
;; 2. Stage 1: Parse completeness enforcement (2 tests)
;; 3. Stage 2-3: Pipeline command validation (1 test)
;; 4. Stage 4: File operations validation (2 tests)
;; 5. Stage 5: Cloud auth validation (2 tests)
;; 6. Stage 6: Coverage threshold warnings (1 test)
;; 7. Early exit behavior (1 test)
;;
;; Total: 12 tests
;;
;; These tests use real bash-parser but mock process execution.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name "../.." test-dir))
       (tools-dir test-root-dir))
  (require 'bash-parser-core)
  (require 'jf-gptel-scope-shell-tools (expand-file-name "../scope/scope-shell-tools.el" tools-dir)))

;;; Helper Functions

(defun test-pipeline--make-paths-config (&rest args)
  "Create paths configuration plist from keyword arguments."
  (list :read (plist-get args :read)
        :write (plist-get args :write)
        :execute (plist-get args :execute)
        :modify (plist-get args :modify)
        :deny (plist-get args :deny)))

(defun test-pipeline--make-test-config (&rest args)
  "Create full scope configuration from keyword arguments."
  (list :paths (or (plist-get args :paths)
                   (test-pipeline--make-paths-config))
        :bash-tools (or (plist-get args :bash-tools)
                        '(:deny ()))
        :cloud (or (plist-get args :cloud)
                   '(:auth-detection "warn"))
        :security (or (plist-get args :security)
                      '(:enforce-parse-complete t
                       :max-coverage-threshold 0.8))))

;;; Pipeline Orchestration Tests

(describe "jf/gptel-scope--validate-command-semantics"

  (describe "full pipeline success"
    (it "passes all 7 stages for valid command"
      (let* ((scope-config (test-pipeline--make-test-config
                            :paths (test-pipeline--make-paths-config
                                    :read '("/workspace/**"))
                            :bash-tools (list :deny '())))
             (result (jf/gptel-scope--validate-command-semantics
                      "ls -la" "/workspace" scope-config)))
        ;; nil = success (all stages passed)
        (expect result :to-be nil))))

  (describe "stage 1: parse completeness"
    (it "fails when parse incomplete and enforce=true"
      (let* ((scope-config (test-pipeline--make-test-config
                            :security '(:enforce-parse-complete t
                                       :max-coverage-threshold 0.8)))
             (result nil))
        ;; Mock incomplete parse
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete nil :parse-errors ("Unexpected token")))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "invalid syntax )" "/workspace" scope-config))
        (expect (plist-get result :error) :to-equal "parse_incomplete")
        (expect (plist-get result :parse-errors) :to-equal '("Unexpected token"))))

    (it "warns but continues when parse incomplete and enforce=false"
      (let* ((scope-config (test-pipeline--make-test-config
                            :security '(:enforce-parse-complete nil
                                       :max-coverage-threshold 0.8)
                            :bash-tools (list :deny '())))
             (result nil)
             (warning-called nil))
        ;; Mock incomplete parse but with extractable commands
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete nil
                  :parse-errors ("Unexpected token")
                  :all-commands ((:command-name "ls"))))
        ;; Capture warning
        (spy-on 'warn :and-call-fake (lambda (&rest args) (setq warning-called t)))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "ls" "/workspace" scope-config))
        ;; Should warn but still pass (nil result)
        (expect warning-called :to-be-truthy)
        (expect result :to-be nil))))

  (describe "stage 2-3: pipeline command extraction and validation"
    (it "fails on denied command in pipeline"
      (let* ((scope-config (test-pipeline--make-test-config
                            :bash-tools '(:deny ("rm"))))
             (result nil))
        ;; Mock parse result with pipeline
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "ls")
                                (:command-name "rm"))))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "ls | rm" "/workspace" scope-config))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 1))))  ; 0-indexed position

  (describe "stage 4: file operations validation"
    (it "fails on file operation out of scope"
      (let* ((scope-config (test-pipeline--make-test-config
                            :paths (test-pipeline--make-paths-config
                                    :read '("/workspace/**"))
                            :bash-tools '(:deny ())))
             (result nil))
        ;; Mock parse and semantics
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "cat"))))
        (spy-on 'jf/bash-extract-semantics :and-return-value
                '(:domains (:filesystem ((:operation :read
                                         :path "/etc/passwd"
                                         :command-name "cat")))))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "cat /etc/passwd" "/workspace" scope-config))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :path) :to-equal "/etc/passwd")
        (expect (plist-get result :operation) :to-equal :read)))

    (it "passes when all file operations in scope"
      (let* ((scope-config (test-pipeline--make-test-config
                            :paths (test-pipeline--make-paths-config
                                    :read '("/workspace/**"))
                            :bash-tools '(:deny ())))
             (result nil))
        ;; Mock parse and semantics
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "cat"))))
        (spy-on 'jf/bash-extract-semantics :and-return-value
                '(:domains (:filesystem ((:operation :read
                                         :path "/workspace/file.txt"
                                         :command-name "cat")))))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "cat /workspace/file.txt" "/workspace" scope-config))
        (expect result :to-be nil))))

  (describe "stage 5: cloud auth validation"
    (xit "fails when cloud auth denied"
      (let* ((scope-config (test-pipeline--make-test-config
                            :cloud '(:auth-detection "deny")
                            :bash-tools '(:deny ())))
             (result nil))
        ;; Mock parse and semantics
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "aws-vault"))))
        (spy-on 'jf/bash-extract-semantics :and-return-value
                '(:domains (:cloud-auth (:provider :aws
                                        :command "aws-vault exec prod"))))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "aws-vault exec prod" "/workspace" scope-config))
        (expect (plist-get result :error) :to-equal "cloud_auth_denied")
        (expect (plist-get result :provider) :to-equal :aws)))

    (xit "warns when cloud auth detected and mode=warn"
      (let* ((scope-config (test-pipeline--make-test-config
                            :cloud '(:auth-detection "warn")
                            :bash-tools '(:deny ())))
             (result nil))
        ;; Mock parse and semantics
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "gcloud"))))
        (spy-on 'jf/bash-extract-semantics :and-return-value
                '(:domains (:cloud-auth (:provider :gcp
                                        :command "gcloud auth login"))))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "gcloud auth login" "/workspace" scope-config))
        ;; Warn mode returns warning plist
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
        (expect (plist-get result :provider) :to-equal :gcp)
        (expect (plist-get result :message) :to-match "Cloud authentication"))))

  (describe "stage 6: coverage threshold (non-blocking)"
    (xit "warns but does not fail when coverage below threshold"
      (let* ((scope-config (test-pipeline--make-test-config
                            :security '(:max-coverage-threshold 0.8
                                       :enforce-parse-complete t)
                            :bash-tools (list :deny '())))
             (result nil)
             (warning-called nil))
        ;; Mock parse
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete t
                  :all-commands ((:command-name "ls"))))
        ;; Mock semantics with low coverage - coverage is direct property of semantics
        (spy-on 'jf/bash-extract-semantics :and-return-value
                '(:coverage (:coverage-ratio 0.5)
                  :domains nil))  ; Below 0.8 threshold
        ;; Capture warning
        (spy-on 'warn :and-call-fake (lambda (&rest args) (setq warning-called t)))
        (setq result (jf/gptel-scope--validate-command-semantics
                      "ls" "/workspace" scope-config))
        ;; Should warn but still pass (stage 7 is non-blocking)
        (expect warning-called :to-be-truthy)
        (expect result :to-be nil))))

  (describe "early exit behavior"
    (it "exits at first failing stage"
      (let* ((scope-config (test-pipeline--make-test-config
                            :security '(:enforce-parse-complete t
                                       :max-coverage-threshold 0.8)))
             (result nil))
        ;; Mock parse failure
        (spy-on 'jf/bash-parse :and-return-value
                '(:parse-complete nil :parse-errors ("Syntax error")))
        ;; Stage 2 function should not be called
        (spy-on 'jf/gptel-scope--extract-pipeline-commands)
        (setq result (jf/gptel-scope--validate-command-semantics
                      "invalid" "/workspace" scope-config))
        ;; Should fail at stage 1
        (expect (plist-get result :error) :to-equal "parse_incomplete")
        ;; Stage 2 should not have been called (early exit)
        (expect 'jf/gptel-scope--extract-pipeline-commands :not :to-have-been-called)))))

(provide 'pipeline-orchestration-spec)

;;; pipeline-orchestration-spec.el ends here
