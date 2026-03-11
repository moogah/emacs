;;; no-op-allowance-spec.el --- Unit tests for no-op command allowance -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; UNIT TESTS: No-op command allowance (zero file operations)
;;
;; Tests the validation pipeline's no-op short-circuit behavior.
;; Each test maps to exactly one scenario in no-op-command-allowance/spec.md.
;;
;; Test organization:
;; 1. Commands with no file operations are allowed by default (4 tests)
;; 2. No-op validation occurs before file operation validation (2 tests)
;; 3. Empty operations list defines no-op commands (3 tests)
;; 4. No-op allowance is independent of command name (2 tests)
;; 5. No-op check applies to all pipeline commands (2 tests)
;; 6. Error messages distinguish no-op allowance from category validation (2 tests)
;;
;; Total: 15 tests covering all 6 requirements from spec.md

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name "../.." test-dir))
       (tools-dir test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "../scope/scope-shell-tools.el" tools-dir)))

;;; Helper Functions

(defun test-no-op--make-scope-config ()
  "Create minimal scope config for testing.
Includes basic read/write paths, deny list with sudo, and cloud/security settings."
  (list :paths (list :read '("/workspace/**")
                     :write '("/workspace/**")
                     :execute '("/workspace/scripts/**")
                     :modify '("/workspace/config/**")
                     :deny '("/etc/**"))
        :bash-tools (list :deny '("rm" "sudo" "mv" "chmod"))
        :cloud (list :auth-detection "warn")
        :security (list :enforce-parse-complete t
                        :max-coverage-threshold 0.8)))

(defun test-no-op--make-no-op-semantics ()
  "Return semantics with empty filesystem domain (no file operations).
Simulates bash-parser extraction returning zero file operations."
  (list :domains (list :filesystem '())))

(defun test-no-op--make-file-op-semantics (operation path)
  "Return semantics with one file operation.
OPERATION is a keyword like :read or :write.
PATH is the file path string."
  (list :domains (list :filesystem (list (list :operation operation
                                                :path path)))))

(defun test-no-op--make-semantics-no-filesystem-domain ()
  "Return semantics with no filesystem domain at all.
Simulates bash-parser extraction when no filesystem plugin runs."
  (list :domains (list)))

(defun test-no-op--make-parse-result (&optional command-name)
  "Return mock parse result from jf/bash-parse.
COMMAND-NAME is the name of the command being parsed (e.g., \"python3\", \"sudo\").
Returns a complete parse result with proper :all-commands structure."
  (list :parse-complete t
        :ast (list :type 'command)
        :tokens '()
        :all-commands (list (list :command-name (or command-name "mock-command")))))

;;; Test Suite

(describe "No-Op Command Allowance"
  :var (test-config)

  (before-each
    (setq test-config (test-no-op--make-scope-config)))

  (describe "Requirement: Commands with no file operations are allowed by default"

    (it "allows version check commands"
      "Scenario: no-op-command-allowance/spec.md § 'Version check command allowed'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "python3"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-no-op-semantics))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "python3 --version"
                     "/tmp"
                     test-config)))
        (expect result :to-be nil)))  ; nil = success

    (it "allows help flag commands"
      "Scenario: no-op-command-allowance/spec.md § 'Help flag command allowed'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "node"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-no-op-semantics))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "node --help"
                     "/tmp"
                     test-config)))
        (expect result :to-be nil)))

    (it "allows unknown commands with no operations"
      "Scenario: no-op-command-allowance/spec.md § 'Unknown command with no operations allowed'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "my-custom-tool"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-no-op-semantics))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "my-custom-tool --info"
                     "/tmp"
                     test-config)))
        (expect result :to-be nil)))

    (it "blocks no-op commands in deny list"
      "Scenario: no-op-command-allowance/spec.md § 'No-op command in deny list still blocked'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "sudo"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-no-op-semantics))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "sudo --version"
                     "/tmp"
                     test-config)))
        (expect result :not :to-be nil)  ; Should have error
        (expect (plist-get result :error) :to-equal "command_denied"))))

  (describe "Requirement: No-op validation occurs before file operation validation"

    (it "short-circuits validation when no file operations"
      "Scenario: no-op-command-allowance/spec.md § 'No-op check short-circuits validation'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "python3"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-no-op-semantics))
      ;; Spy on file operation validator to ensure it's NOT called
      (spy-on 'jf/gptel-scope--validate-file-operations)
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "python3 --version"
                     "/tmp"
                     test-config)))
        (expect result :to-be nil)
        (expect 'jf/gptel-scope--validate-file-operations :not :to-have-been-called)))

    (it "proceeds to path validation when operations exist"
      "Scenario: no-op-command-allowance/spec.md § 'Commands with operations proceed to path validation'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "python3"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-file-op-semantics :execute "/workspace/script.py"))
      ;; Spy on file operation validator to ensure it IS called
      (spy-on 'jf/gptel-scope--validate-file-operations :and-return-value nil)
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "python3 script.py"
                     "/workspace"
                     test-config)))
        (expect 'jf/gptel-scope--validate-file-operations :to-have-been-called))))

  (describe "Requirement: Empty operations list defines no-op commands"

    (it "classifies empty filesystem operations as no-op"
      "Scenario: no-op-command-allowance/spec.md § 'Empty filesystem operations list'"
      (let* ((semantics (test-no-op--make-no-op-semantics))
             (result (jf/gptel-scope--check-no-op semantics)))
        ;; check-no-op returns nil when no operations (allowed)
        (expect result :to-be nil)))

    (it "classifies non-empty filesystem operations as not no-op"
      "Scenario: no-op-command-allowance/spec.md § 'Non-empty filesystem operations list'"
      (let* ((semantics (test-no-op--make-file-op-semantics :read "/workspace/file.txt"))
             (result (jf/gptel-scope--check-no-op semantics)))
        ;; check-no-op returns t when operations exist (continue validation)
        (expect result :to-be t)))

    (it "classifies no filesystem domain as no-op"
      "Scenario: no-op-command-allowance/spec.md § 'No filesystem domain present'"
      (let* ((semantics (test-no-op--make-semantics-no-filesystem-domain))
             (result (jf/gptel-scope--check-no-op semantics)))
        ;; check-no-op returns nil when no filesystem domain (allowed)
        (expect result :to-be nil))))

  (describe "Requirement: No-op allowance is independent of command name"

    (it "allows unknown command names with no operations"
      "Scenario: no-op-command-allowance/spec.md § 'Unknown command name with no operations'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "never-before-seen-command"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-no-op-semantics))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "never-before-seen-command --status"
                     "/tmp"
                     test-config)))
        (expect result :to-be nil)))

    (it "allows commands without checking categories when no operations"
      "Scenario: no-op-command-allowance/spec.md § 'Command previously required allowlist'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "ruby"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-no-op-semantics))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ruby --version"
                     "/tmp"
                     test-config)))
        ;; ruby --version allowed despite ruby not being in any category
        (expect result :to-be nil))))

  (describe "Requirement: No-op check applies to all pipeline commands"

    (it "allows simple pipeline with no operations"
      "Scenario: no-op-command-allowance/spec.md § 'Simple command pipeline with no operations'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "echo"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-no-op-semantics))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "echo \"hello\" | wc -l"
                     "/tmp"
                     test-config)))
        (expect result :to-be nil)))

    (it "validates pipeline with file operations in later stage"
      "Scenario: no-op-command-allowance/spec.md § 'Pipeline with file operations in later stage'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "echo"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-file-op-semantics :write "/tmp/file.txt"))
      (spy-on 'jf/gptel-scope--validate-file-operations :and-return-value nil)
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "echo \"test\" > /tmp/file.txt"
                     "/tmp"
                     test-config)))
        ;; Should call file operation validator (not short-circuited)
        (expect 'jf/gptel-scope--validate-file-operations :to-have-been-called))))

  (describe "Requirement: Error messages distinguish no-op allowance from category validation"

    (it "returns success without mentioning categories for no-op commands"
      "Scenario: no-op-command-allowance/spec.md § 'No-op command success message'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "python3"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-no-op-semantics))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "python3 --version"
                     "/tmp"
                     test-config)))
        ;; Success is nil (no error structure)
        (expect result :to-be nil)))

    (it "references operation and path for validation failures"
      "Scenario: no-op-command-allowance/spec.md § 'Operation validation failure message'"
      (spy-on 'jf/bash-parse
              :and-return-value (test-no-op--make-parse-result "cat"))
      (spy-on 'jf/bash-extract-semantics
              :and-return-value (test-no-op--make-file-op-semantics :read "/etc/passwd"))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /etc/passwd"
                     "/tmp"
                     test-config)))
        ;; Should fail with path_denied (in deny list)
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-equal "path_denied")
        (expect (plist-get result :path) :to-equal "/etc/passwd")
        (expect (plist-get result :operation) :to-equal :read)))))

(provide 'no-op-allowance-spec)

;;; no-op-allowance-spec.el ends here
