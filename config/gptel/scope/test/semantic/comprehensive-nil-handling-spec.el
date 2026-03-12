;;; comprehensive-nil-handling-spec.el --- Comprehensive nil edge case tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; COMPREHENSIVE NIL EDGE CASE COVERAGE
;;
;; This test suite documents and validates all scenarios where nil/empty values
;; could cause issues in the bash scope validation pipeline.
;;
;; Context: Investigation of "Wrong type argument: consp, nil" error seen in
;; production but not reproducible in test harness. These tests ensure robust
;; nil handling throughout the validation pipeline.
;;
;; Coverage:
;; - Bash-parser returning nil/empty domains
;; - Config sections with nil/empty values
;; - File operations with nil paths
;; - Empty deny lists and pattern lists
;; - Malformed plists and data structures

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (semantic-dir test-dir)
       (scope-test-dir (expand-file-name ".." semantic-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))

(describe "Comprehensive nil handling in bash scope validation"

  (describe "Stage 1: Parse completeness with nil values"

    (it "handles nil parse-complete gracefully"
      (let* ((parse-result '(:parse-complete nil :parse-errors "Syntax error"))
             (security-config '(:enforce-parse-complete t))
             (result (jf/gptel-scope--validate-parse-completeness parse-result security-config)))
        ;; Should return error plist, not crash
        (expect (plist-get result :error) :to-equal "parse_incomplete")))

    (it "handles nil security-config"
      (let* ((parse-result '(:parse-complete t))
             (security-config nil)
             (result (jf/gptel-scope--validate-parse-completeness parse-result security-config)))
        ;; Should return nil (success) - no enforcement when security-config is nil
        (expect result :to-be nil))))

  (describe "Stage 2: Extract commands with nil values"

    (it "handles parsed-command with nil all-commands"
      (let* ((parsed-command '(:all-commands nil))
             (result (jf/gptel-scope--extract-pipeline-commands parsed-command)))
        ;; Should return empty list, not crash
        (expect result :to-equal '())))

    (it "handles parsed-command with empty all-commands"
      (let* ((parsed-command '(:all-commands ()))
             (result (jf/gptel-scope--extract-pipeline-commands parsed-command)))
        (expect result :to-equal '()))))

  (describe "Stage 3: Pipeline command validation with nil values"

    (it "allows all commands when bash-tools is nil"
      (let* ((commands '("ls" "grep" "head"))
             (bash-tools nil)
             (result (jf/gptel-scope--validate-pipeline-commands commands bash-tools)))
        ;; Should allow (no deny list to check)
        (expect result :to-be nil)))

    (it "allows all commands when deny list is nil"
      (let* ((commands '("ls" "grep"))
             (bash-tools '(:deny nil))
             (result (jf/gptel-scope--validate-pipeline-commands commands bash-tools)))
        (expect result :to-be nil)))

    (it "allows all commands when deny list is empty"
      (let* ((commands '("ls" "grep"))
             (bash-tools '(:deny ()))
             (result (jf/gptel-scope--validate-pipeline-commands commands bash-tools)))
        (expect result :to-be nil)))

    (it "handles nil commands list"
      (let* ((commands nil)
             (bash-tools '(:deny ("sudo" "rm")))
             (result (jf/gptel-scope--validate-pipeline-commands commands bash-tools)))
        ;; Should allow (no commands to validate)
        (expect result :to-be nil)))

    (it "handles empty commands list"
      (let* ((commands '())
             (bash-tools '(:deny ("sudo")))
             (result (jf/gptel-scope--validate-pipeline-commands commands bash-tools)))
        (expect result :to-be nil))))

  (describe "Stage 4: No-op check with nil values"

    (it "allows when semantics is nil"
      (let ((result (jf/gptel-scope--check-no-op nil)))
        ;; nil semantics → no file ops → allow
        (expect result :to-be nil)))

    (it "allows when domains is nil"
      (let* ((semantics '(:domains nil))
             (result (jf/gptel-scope--check-no-op semantics)))
        (expect result :to-be nil)))

    (it "allows when domains is empty plist"
      (let* ((semantics '(:domains ()))
             (result (jf/gptel-scope--check-no-op semantics)))
        (expect result :to-be nil)))

    (it "allows when filesystem is nil"
      (let* ((semantics '(:domains ((:filesystem . nil))))
             (result (jf/gptel-scope--check-no-op semantics)))
        (expect result :to-be nil)))

    (it "allows when filesystem is empty list"
      (let* ((semantics '(:domains ((:filesystem))))
             (result (jf/gptel-scope--check-no-op semantics)))
        (expect result :to-be nil)))

    (it "continues validation when filesystem has operations"
      (let* ((semantics '(:domains ((:filesystem . ((:file "/tmp" :operation :read :command "cat" :confidence :high))))))
             (result (jf/gptel-scope--check-no-op semantics)))
        ;; Has file ops → return t to continue validation
        (expect result :to-be t))))

  (describe "Stage 5: File operations validation with nil values"

    (it "allows when file-ops is nil"
      (let* ((file-ops nil)
             (directory "/")
             (scope-config '(:paths (:read ("/**") :write () :deny ())))
             (result (jf/gptel-scope--validate-file-operations file-ops directory scope-config)))
        ;; No operations to validate → allow
        (expect result :to-be nil)))

    (it "allows when file-ops is empty list"
      (let* ((file-ops '())
             (directory "/")
             (scope-config '(:paths (:read ("/**") :write () :deny ())))
             (result (jf/gptel-scope--validate-file-operations file-ops directory scope-config)))
        (expect result :to-be nil)))

    (it "handles file-op with nil path by attempting expansion"
      (let* ((file-ops '((:file nil :operation :read :command "cat" :confidence :high)))
             (directory "/tmp")
             (scope-config '(:paths (:read ("/tmp/**") :write () :deny ())))
             (result nil)
             (error-caught nil))
        (condition-case err
            (setq result (jf/gptel-scope--validate-file-operations file-ops directory scope-config))
          (error
           (setq error-caught (error-message-string err))))
        ;; Should either succeed or fail gracefully, not crash with consp error
        (when error-caught
          (expect error-caught :not :to-match "Wrong type argument: consp"))))

    (it "handles paths-config with nil lists"
      (let* ((file-ops '((:file "/tmp/file.txt" :operation :read :command "cat" :confidence :high)))
             (directory "/tmp")
             (scope-config '(:paths (:read nil :write nil :deny nil)))
             (result (jf/gptel-scope--validate-file-operations file-ops directory scope-config)))
        ;; Should deny (path not in read list)
        (expect (plist-get result :error) :to-equal "path_out_of_scope"))))

  (describe "Path validation with nil values"

    (it "denies operation when patterns list is nil"
      (let* ((operation :read)
             (path "/tmp/file.txt")
             (paths-config '(:read nil :write nil :deny nil))
             (result (jf/gptel-scope--validate-operation operation path paths-config)))
        ;; Nil pattern list → deny
        (expect (plist-get result :error) :to-equal "path_out_of_scope")))

    (it "denies operation when patterns list is empty"
      (let* ((operation :read)
             (path "/tmp/file.txt")
             (paths-config '(:read () :write () :deny ()))
             (result (jf/gptel-scope--validate-operation operation path paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")))

    (it "handles nil path gracefully"
      (let* ((operation :read)
             (path nil)
             (paths-config '(:read ("/**") :write () :deny ()))
             (result nil)
             (error-caught nil))
        (condition-case err
            (setq result (jf/gptel-scope--validate-operation operation path paths-config))
          (error
           (setq error-caught (error-message-string err))))
        ;; Should not crash with consp error
        (when error-caught
          (expect error-caught :not :to-match "Wrong type argument: consp")))))

  (describe "Full pipeline with nil configurations"

    (it "handles command with no config sections"
      (let* ((command "ls -la")
             (directory "/tmp")
             (scope-config '(:paths nil :bash-tools nil :cloud nil :security nil))
             (result nil)
             (error-caught nil))
        (condition-case err
            (setq result (jf/gptel-scope--validate-command-semantics command directory scope-config))
          (error
           (setq error-caught (error-message-string err))))
        ;; Should handle gracefully
        (when error-caught
          (expect error-caught :not :to-match "Wrong type argument: consp"))))

    (it "handles completely empty scope-config"
      (let* ((command "python3 --version")
             (directory "/")
             (scope-config '())
             (result nil)
             (error-caught nil))
        (condition-case err
            (setq result (jf/gptel-scope--validate-command-semantics command directory scope-config))
          (error
           (setq error-caught (error-message-string err))))
        ;; Should handle gracefully
        (when error-caught
          (expect error-caught :not :to-match "Wrong type argument: consp"))))))

(provide 'comprehensive-nil-handling-spec)
;;; comprehensive-nil-handling-spec.el ends here
