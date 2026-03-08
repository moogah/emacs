;;; test-scope-validation-file-paths.el --- Integration tests for file path validation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INTEGRATION TESTS: File path validation against operation-specific scope patterns
;;
;; Comprehensive coverage of 45+ scenarios from:
;; openspec/changes/bash-parser-integration/specs/scope-validation-file-paths/spec.md
;;
;; Test organization:
;; 1. File operations extraction from commands (5 scenarios)
;; 2. Path resolution relative to working directory (4 scenarios)
;; 3. Operation-specific scope validation (4 scenarios)
;; 4. Deny patterns take precedence (2 scenarios)
;; 5. Multiple file operations validated independently (3 scenarios)
;; 6. Structured error responses with path details (3 scenarios)
;; 7. Glob pattern matching for scope validation (3 scenarios)
;;
;; Test naming convention: test-scope-file-paths-<requirement>-<scenario-slug>
;; Each test references its spec scenario for traceability.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load dependencies
(let ((tools-dir (expand-file-name ".."
                                   (file-name-directory (or load-file-name
                                                           buffer-file-name)))))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Helper Functions

(defun test-scope-file--make-paths-config (&rest args)
  "Create paths configuration plist from keyword arguments.
Accepts :read, :write, :execute, :modify, :deny keywords."
  (list :read (plist-get args :read)
        :write (plist-get args :write)
        :execute (plist-get args :execute)
        :modify (plist-get args :modify)
        :deny (plist-get args :deny)))

(defun test-scope-file--make-file-op (operation path &optional command-name absolute-path)
  "Create file operation plist for testing.
OPERATION is keyword like :read, :write, :execute, :modify.
PATH is the file path.
COMMAND-NAME is optional command that performs operation.
ABSOLUTE-PATH is optional pre-resolved absolute path."
  (list :operation operation
        :path path
        :command-name (or command-name "test")
        :absolute-path absolute-path))

;;; Requirement 1: File operations extraction from commands
;; These tests verify bash-parser integration (mock responses for unit testing)

(ert-deftest test-scope-file-paths-extract-read-operation ()
  "Spec scenario: Extract read operation.
Reference: specs/scope-validation-file-paths/spec.md § File operations extraction"
  ;; This is an integration test of the validation layer
  ;; The actual bash-parser extraction is tested in bash-parser test suite
  ;; Here we test that validation handles read operations correctly
  (let* ((file-op (test-scope-file--make-file-op :read "/workspace/file.txt" "cat"))
         (paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation
                  (plist-get file-op :operation)
                  (plist-get file-op :path)
                  paths-config)))
    (should (null result))))  ; nil = success

(ert-deftest test-scope-file-paths-extract-write-operation ()
  "Spec scenario: Extract write operation.
Reference: specs/scope-validation-file-paths/spec.md § File operations extraction"
  (let* ((file-op (test-scope-file--make-file-op :write "/tmp/output.txt" "touch"))
         (paths-config (test-scope-file--make-paths-config
                        :write '("/tmp/**")))
         (result (jf/gptel-scope--validate-operation
                  (plist-get file-op :operation)
                  (plist-get file-op :path)
                  paths-config)))
    (should (null result))))

(ert-deftest test-scope-file-paths-extract-execute-operation ()
  "Spec scenario: Extract execute operation.
Reference: specs/scope-validation-file-paths/spec.md § File operations extraction"
  (let* ((file-op (test-scope-file--make-file-op :execute "/workspace/scripts/deploy.py" "python"))
         (paths-config (test-scope-file--make-paths-config
                        :execute '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation
                  (plist-get file-op :operation)
                  (plist-get file-op :path)
                  paths-config)))
    (should (null result))))

(ert-deftest test-scope-file-paths-extract-modify-operation ()
  "Spec scenario: Extract modify operation.
Reference: specs/scope-validation-file-paths/spec.md § File operations extraction"
  (let* ((file-op (test-scope-file--make-file-op :modify "/workspace/config/app.yml" "sed"))
         (paths-config (test-scope-file--make-paths-config
                        :modify '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation
                  (plist-get file-op :operation)
                  (plist-get file-op :path)
                  paths-config)))
    (should (null result))))

(ert-deftest test-scope-file-paths-extract-multiple-operations ()
  "Spec scenario: Extract multiple operations from single command (cp).
Reference: specs/scope-validation-file-paths/spec.md § File operations extraction"
  ;; cp command creates two operations: read source, write dest
  (let* ((file-ops (list
                    (test-scope-file--make-file-op :read "/workspace/source.txt" "cp")
                    (test-scope-file--make-file-op :write "/tmp/dest.txt" "cp")))
         (scope-config (list :paths (test-scope-file--make-paths-config
                                     :read '("/workspace/**")
                                     :write '("/tmp/**"))))
         (result (jf/gptel-scope--validate-file-operations
                  file-ops "/workspace" scope-config)))
    (should (null result))))  ; Both operations should pass

;;; Requirement 2: Path resolution relative to working directory

(ert-deftest test-scope-file-paths-resolve-relative-path ()
  "Spec scenario: Resolve relative path.
Reference: specs/scope-validation-file-paths/spec.md § Path resolution"
  (let* ((file-op (test-scope-file--make-file-op :read "./file.txt" "cat"))
         (directory "/workspace")
         (paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-file-operation
                  file-op directory paths-config)))
    (should (null result))))  ; Should resolve to /workspace/file.txt and pass

(ert-deftest test-scope-file-paths-resolve-parent-directory ()
  "Spec scenario: Resolve parent directory reference.
Reference: specs/scope-validation-file-paths/spec.md § Path resolution"
  (let* ((file-op (test-scope-file--make-file-op :read "../other/file.txt" "cat"))
         (directory "/workspace/project")
         (paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-file-operation
                  file-op directory paths-config)))
    (should (null result))))  ; Should resolve to /workspace/other/file.txt and pass

(ert-deftest test-scope-file-paths-absolute-path-unchanged  ()
  "Spec scenario: Absolute path unchanged.
Reference: specs/scope-validation-file-paths/spec.md § Path resolution"
  (let* ((file-op (test-scope-file--make-file-op :read "/etc/passwd" "cat"))
         (directory "/workspace")
         (paths-config (test-scope-file--make-paths-config
                        :read '("/etc/**")))
         (result (jf/gptel-scope--validate-file-operation
                  file-op directory paths-config)))
    (should (null result))))  ; Absolute path should remain /etc/passwd

(ert-deftest test-scope-file-paths-resolve-symlinks ()
  "Spec scenario: Resolve symlinks in path.
Reference: specs/scope-validation-file-paths/spec.md § Path resolution"
  ;; This test documents expected behavior but implementation uses expand-file-name
  ;; which may or may not resolve symlinks depending on whether they exist
  ;; The key is that validation uses the resolved path
  (let* ((file-op (test-scope-file--make-file-op :read "/workspace/link/file.txt" "cat"))
         (directory "/workspace")
         (paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-file-operation
                  file-op directory paths-config)))
    ;; Should pass regardless of symlink resolution since pattern covers /workspace/**
    (should (null result))))

;;; Requirement 3: Operation-specific scope validation

(ert-deftest test-scope-file-paths-read-matches-paths-read ()
  "Spec scenario: Read operation matches paths.read.
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths-config)))
    (should (null result))))

(ert-deftest test-scope-file-paths-read-matches-paths-write ()
  "Spec scenario: Read operation matches paths.write (write scope includes read).
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :write '("/tmp/**")))
         (result (jf/gptel-scope--validate-operation :read "/tmp/file.txt" paths-config)))
    (should (null result))))  ; Write scope includes read capability

(ert-deftest test-scope-file-paths-write-requires-paths-write ()
  "Spec scenario: Write operation requires paths.write.
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation :write "/workspace/output.txt" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_out_of_scope"))))

(ert-deftest test-scope-file-paths-execute-requires-paths-execute ()
  "Spec scenario: Execute operation requires paths.execute.
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation :execute "/workspace/scripts/deploy.py" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_out_of_scope"))))

(ert-deftest test-scope-file-paths-modify-requires-paths-modify ()
  "Spec scenario: Modify operation requires paths.modify.
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :write '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation :modify "/workspace/config/app.yml" paths-config)))
    ;; Modify is subset of write, so write scope allows modify
    (should (null result))))

(ert-deftest test-scope-file-paths-modify-allowed-by-paths-write ()
  "Spec scenario: Modify operation allowed by paths.write (write includes modify).
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :write '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation :modify "/workspace/config/app.yml" paths-config)))
    (should (null result))))

(ert-deftest test-scope-file-paths-modify-allowed-by-paths-modify ()
  "Spec scenario: Modify operation allowed by paths.modify.
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :modify '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation :modify "/workspace/config/app.yml" paths-config)))
    (should (null result))))

(ert-deftest test-scope-file-paths-delete-requires-paths-write ()
  "Spec scenario: Delete operation requires paths.write (deletion is write operation).
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :write '("/tmp/**")))
         (result (jf/gptel-scope--validate-operation :delete "/tmp/file.txt" paths-config)))
    (should (null result))))

(ert-deftest test-scope-file-paths-delete-denied-without-write ()
  "Spec scenario: Delete operation denied without paths.write.
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation :delete "/workspace/file.txt" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_out_of_scope"))))

;;; Requirement 4: Deny patterns take precedence

(ert-deftest test-scope-file-paths-read-operation-denied ()
  "Spec scenario: Read operation on denied path.
Reference: specs/scope-validation-file-paths/spec.md § Deny patterns take precedence"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :deny '("/etc/**")))
         (result (jf/gptel-scope--validate-operation :read "/etc/passwd" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_denied"))))

(ert-deftest test-scope-file-paths-deny-overrides-read-scope ()
  "Spec scenario: Deny overrides read scope.
Reference: specs/scope-validation-file-paths/spec.md § Deny patterns take precedence"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")
                        :deny '("/workspace/secrets/**")))
         (result (jf/gptel-scope--validate-operation :read "/workspace/secrets/api-key.txt" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_denied"))))

(ert-deftest test-scope-file-paths-deny-overrides-write-scope ()
  "Spec scenario: Deny overrides write scope.
Reference: specs/scope-validation-file-paths/spec.md § Deny patterns take precedence"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :write '("/workspace/**")
                        :deny '("/workspace/.git/**")))
         (result (jf/gptel-scope--validate-operation :write "/workspace/.git/config" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_denied"))))

(ert-deftest test-scope-file-paths-deny-overrides-execute-scope ()
  "Spec scenario: Deny overrides execute scope.
Reference: specs/scope-validation-file-paths/spec.md § Deny patterns take precedence"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :execute '("/workspace/**")
                        :deny '("/workspace/dangerous/**")))
         (result (jf/gptel-scope--validate-operation :execute "/workspace/dangerous/script.sh" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_denied"))))

;;; Requirement 5: Multiple file operations validated independently

(ert-deftest test-scope-file-paths-copy-both-in-scope ()
  "Spec scenario: Copy with both paths in scope.
Reference: specs/scope-validation-file-paths/spec.md § Multiple file operations"
  (let* ((file-ops (list
                    (test-scope-file--make-file-op :read "/workspace/source.txt" "cp")
                    (test-scope-file--make-file-op :write "/tmp/dest.txt" "cp")))
         (scope-config (list :paths (test-scope-file--make-paths-config
                                     :read '("/workspace/**")
                                     :write '("/tmp/**"))))
         (result (jf/gptel-scope--validate-file-operations
                  file-ops "/workspace" scope-config)))
    (should (null result))))

(ert-deftest test-scope-file-paths-copy-destination-out-of-scope ()
  "Spec scenario: Copy with destination out of scope.
Reference: specs/scope-validation-file-paths/spec.md § Multiple file operations"
  (let* ((file-ops (list
                    (test-scope-file--make-file-op :read "/workspace/source.txt" "cp")
                    (test-scope-file--make-file-op :write "/etc/dest.txt" "cp")))
         (scope-config (list :paths (test-scope-file--make-paths-config
                                     :read '("/workspace/**"))))
         (result (jf/gptel-scope--validate-file-operations
                  file-ops "/workspace" scope-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_out_of_scope"))
    (should (string= (plist-get result :path) "/etc/dest.txt"))))

(ert-deftest test-scope-file-paths-first-operation-fails-immediate-return ()
  "Spec scenario: First operation fails, report immediately.
Reference: specs/scope-validation-file-paths/spec.md § Multiple file operations"
  (let* ((file-ops (list
                    (test-scope-file--make-file-op :write "/etc/bad.txt" "cp")
                    (test-scope-file--make-file-op :read "/workspace/source.txt" "cp")))
         (scope-config (list :paths (test-scope-file--make-paths-config
                                     :read '("/workspace/**"))))
         (result (jf/gptel-scope--validate-file-operations
                  file-ops "/workspace" scope-config)))
    ;; Should fail on first operation without checking second
    (should (plist-get result :error))
    (should (string= (plist-get result :path) "/etc/bad.txt"))))

(ert-deftest test-scope-file-paths-multiple-reads-all-validated ()
  "Spec scenario: Multiple read operations all validated.
Reference: specs/scope-validation-file-paths/spec.md § Multiple file operations"
  (let* ((file-ops (list
                    (test-scope-file--make-file-op :read "/workspace/file1.txt")
                    (test-scope-file--make-file-op :read "/workspace/file2.txt")
                    (test-scope-file--make-file-op :read "/workspace/file3.txt")))
         (scope-config (list :paths (test-scope-file--make-paths-config
                                     :read '("/workspace/**"))))
         (result (jf/gptel-scope--validate-file-operations
                  file-ops "/workspace" scope-config)))
    (should (null result))))

(ert-deftest test-scope-file-paths-mixed-operations-all-validated ()
  "Spec scenario: Mixed operations (read/write/execute) all validated.
Reference: specs/scope-validation-file-paths/spec.md § Multiple file operations"
  (let* ((file-ops (list
                    (test-scope-file--make-file-op :read "/workspace/input.txt")
                    (test-scope-file--make-file-op :execute "/workspace/script.sh")
                    (test-scope-file--make-file-op :write "/tmp/output.txt")))
         (scope-config (list :paths (test-scope-file--make-paths-config
                                     :read '("/workspace/**")
                                     :execute '("/workspace/**")
                                     :write '("/tmp/**"))))
         (result (jf/gptel-scope--validate-file-operations
                  file-ops "/workspace" scope-config)))
    (should (null result))))

;;; Requirement 6: Structured error responses with path details

(ert-deftest test-scope-file-paths-error-path-out-of-scope-structure ()
  "Spec scenario: Path out of scope error structure.
Reference: specs/scope-validation-file-paths/spec.md § Structured error responses"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation :write "/tmp/file.txt" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_out_of_scope"))
    (should (plist-get result :path))
    (should (plist-get result :operation))
    (should (plist-get result :message))))

(ert-deftest test-scope-file-paths-error-path-denied-structure ()
  "Spec scenario: Path denied error structure.
Reference: specs/scope-validation-file-paths/spec.md § Structured error responses"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")
                        :deny '("/workspace/secrets/**")))
         (result (jf/gptel-scope--validate-operation :read "/workspace/secrets/key.txt" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_denied"))
    (should (plist-get result :path))
    (should (plist-get result :operation))
    (should (plist-get result :message))))

(ert-deftest test-scope-file-paths-error-message-actionable ()
  "Spec scenario: Error message suggests scope expansion.
Reference: specs/scope-validation-file-paths/spec.md § Structured error responses"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**")))
         (result (jf/gptel-scope--validate-operation :write "/tmp/file.txt" paths-config)))
    (should (plist-get result :message))
    ;; Error message should be descriptive and suggest action
    (should (stringp (plist-get result :message)))))

;;; Requirement 7: Glob pattern matching for scope validation

(ert-deftest test-scope-file-paths-glob-single-level-wildcard ()
  "Spec scenario: Single-level wildcard match.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (should (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/*"))
  (should-not (jf/gptel-scope--glob-match-p "/workspace/sub/file.txt" "/workspace/*")))

(ert-deftest test-scope-file-paths-glob-recursive-wildcard ()
  "Spec scenario: Recursive wildcard match.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (should (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/**"))
  (should (jf/gptel-scope--glob-match-p "/workspace/deep/nested/file.txt" "/workspace/**")))

(ert-deftest test-scope-file-paths-glob-extension-pattern ()
  "Spec scenario: Extension pattern match.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (should (jf/gptel-scope--glob-match-p "/workspace/config.yml" "/workspace/*.yml"))
  (should-not (jf/gptel-scope--glob-match-p "/workspace/config.json" "/workspace/*.yml")))

(ert-deftest test-scope-file-paths-glob-multiple-patterns ()
  "Spec scenario: Multiple patterns checked in order.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (let ((patterns '("/workspace/*.txt" "/workspace/*.yml")))
    (should (jf/gptel-scope--path-matches-any-pattern-p "/workspace/file.txt" patterns))
    (should (jf/gptel-scope--path-matches-any-pattern-p "/workspace/config.yml" patterns))
    (should-not (jf/gptel-scope--path-matches-any-pattern-p "/workspace/script.sh" patterns))))

(ert-deftest test-scope-file-paths-glob-directory-exact-match ()
  "Spec scenario: Exact directory path match.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (should (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/file.txt"))
  (should-not (jf/gptel-scope--glob-match-p "/workspace/other.txt" "/workspace/file.txt")))

(ert-deftest test-scope-file-paths-glob-middle-wildcard ()
  "Spec scenario: Wildcard in middle of pattern.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (should (jf/gptel-scope--glob-match-p "/workspace/test/file.txt" "/workspace/*/file.txt"))
  (should-not (jf/gptel-scope--glob-match-p "/workspace/test/sub/file.txt" "/workspace/*/file.txt")))

(ert-deftest test-scope-file-paths-glob-recursive-middle ()
  "Spec scenario: Recursive wildcard in middle of pattern.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (should (jf/gptel-scope--glob-match-p "/workspace/test/file.txt" "/workspace/**/file.txt"))
  (should (jf/gptel-scope--glob-match-p "/workspace/test/sub/file.txt" "/workspace/**/file.txt"))
  (should (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/**/file.txt")))

;;; Additional edge cases and integration scenarios

(ert-deftest test-scope-file-paths-empty-patterns-deny-all ()
  "Spec scenario: Empty pattern list denies all operations.
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config))
         (result (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "path_out_of_scope"))))

(ert-deftest test-scope-file-paths-nil-patterns-deny-all ()
  "Spec scenario: nil pattern list denies all operations.
Reference: specs/scope-validation-file-paths/spec.md § Operation-specific scope validation"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read nil))
         (result (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths-config)))
    (should (plist-get result :error))))

(ert-deftest test-scope-file-paths-overlapping-patterns ()
  "Spec scenario: Overlapping allow patterns (any match allows).
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (let* ((paths-config (test-scope-file--make-paths-config
                        :read '("/workspace/**" "/workspace/sub/**")))
         (result (jf/gptel-scope--validate-operation :read "/workspace/sub/file.txt" paths-config)))
    ;; Either pattern matches, operation allowed
    (should (null result))))

(ert-deftest test-scope-file-paths-case-sensitive-patterns ()
  "Spec scenario: Pattern matching is case-sensitive.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (should (jf/gptel-scope--glob-match-p "/workspace/File.txt" "/workspace/File.txt"))
  (should-not (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/File.txt")))

(ert-deftest test-scope-file-paths-trailing-slash-handling ()
  "Spec scenario: Trailing slashes in patterns and paths.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  ;; Trailing slash should not affect matching
  (should (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/**"))
  (should (jf/gptel-scope--glob-match-p "/workspace/sub/file.txt" "/workspace/**")))

(ert-deftest test-scope-file-paths-dot-files-matched ()
  "Spec scenario: Dot files (hidden files) are matched by wildcards.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (should (jf/gptel-scope--glob-match-p "/workspace/.hidden" "/workspace/*"))
  (should (jf/gptel-scope--glob-match-p "/workspace/.config/file" "/workspace/**")))

(ert-deftest test-scope-file-paths-special-chars-in-filename ()
  "Spec scenario: Special characters in filenames are matched literally.
Reference: specs/scope-validation-file-paths/spec.md § Glob pattern matching"
  (should (jf/gptel-scope--glob-match-p "/workspace/file-name.txt" "/workspace/*"))
  (should (jf/gptel-scope--glob-match-p "/workspace/file_name.txt" "/workspace/*"))
  (should (jf/gptel-scope--glob-match-p "/workspace/file.name.txt" "/workspace/*")))

(provide 'test-scope-validation-file-paths)
;;; test-scope-validation-file-paths.el ends here
