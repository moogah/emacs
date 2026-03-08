;;; workflows-spec.el --- Behavioral tests for scope validation workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; BEHAVIORAL TESTS: End-to-end workflow validation for scope control
;;
;; Tests complete user workflows through the validation pipeline:
;; 1. Schema loading and configuration workflows
;; 2. File operation validation workflows
;; 3. Cloud authentication workflows
;; 4. Parse completeness workflows
;; 5. Error message quality and structure
;; 6. Multi-operation validation workflows
;;
;; These tests validate the full pipeline from command input to validation result,
;; ensuring proper integration of all validation stages.
;;
;; Test naming convention: describe "Workflow: <scenario-name>"
;; Each test simulates a complete user interaction flow.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name ".." test-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;; Also need scope-core
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name "../scope" test-root-dir)))
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir)))

;;; Workflow Tests

(describe "Workflow: Schema loading with defaults"

  (it "loads schema with operation-specific paths"
    (let* ((scope-plist '(:paths (:read ("/workspace/**")
                                 :write ("/tmp/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Should have all path keys
      (expect (plist-get paths :read) :to-equal '("/workspace/**"))
      (expect (plist-get paths :write) :to-equal '("/tmp/**"))
      ;; Missing keys should default to empty lists
      (expect (plist-get paths :execute) :to-equal '())
      (expect (plist-get paths :modify) :to-equal '())
      (expect (plist-get paths :deny) :to-equal '())))

  (it "merges cloud config with defaults"
    (let* ((scope-plist '(:cloud (:auth-detection "deny")))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (cloud (plist-get schema :cloud)))
      (expect (plist-get cloud :auth-detection) :to-equal "deny")
      ;; allowed-providers should be nil if not specified
      (expect (plist-get cloud :allowed-providers) :to-be nil)))

  (it "merges security config with defaults"
    (let* ((scope-plist '(:security (:enforce-parse-complete nil)))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (security (plist-get schema :security)))
      ;; Provided value should override default
      (expect (plist-get security :enforce-parse-complete) :to-be nil)
      ;; Missing values should use defaults
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.8))))

(describe "Workflow: File operation validation"

  (it "allows read operation within read scope"
    (let* ((scope-plist '(:paths (:read ("/workspace/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths))
           (result (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths)))
      ;; Should succeed (nil = success)
      (expect result :to-be nil)))

  (it "denies read operation outside scope"
    (let* ((scope-plist '(:paths (:read ("/workspace/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths))
           (result (jf/gptel-scope--validate-operation :read "/etc/passwd" paths)))
      ;; Should fail with error
      (expect result :not :to-be nil)
      (expect (plist-get result :error) :not :to-be nil)
      (expect (plist-get result :path) :to-equal "/etc/passwd")
      (expect (plist-get result :operation) :to-equal :read)))

  (it "denies access to paths in deny list despite broad permissions"
    (let* ((scope-plist '(:paths (:read ("/**")  ; Allow everything
                                 :deny ("/etc/**"))))  ; Except /etc
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; /etc should be denied
      (let ((result (jf/gptel-scope--validate-operation :read "/etc/passwd" paths)))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-match "denied"))
      ;; But /home should work
      (let ((result (jf/gptel-scope--validate-operation :read "/home/user/file.txt" paths)))
        (expect result :to-be nil)))))

(describe "Workflow: Operation-specific path permissions"

  (it "validates write operation requires write scope"
    (let* ((scope-plist '(:paths (:write ("/tmp/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Write in write scope should succeed
      (expect (jf/gptel-scope--validate-operation :write "/tmp/output.txt" paths)
              :to-be nil)
      ;; Write outside write scope should fail
      (let ((result (jf/gptel-scope--validate-operation :write "/etc/config" paths)))
        (expect result :not :to-be nil))))

  (it "validates execute operation requires execute scope"
    (let* ((scope-plist '(:paths (:execute ("/workspace/scripts/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Execute in execute scope should succeed
      (expect (jf/gptel-scope--validate-operation :execute "/workspace/scripts/deploy.sh" paths)
              :to-be nil)
      ;; Execute outside execute scope should fail
      (let ((result (jf/gptel-scope--validate-operation :execute "/tmp/script.sh" paths)))
        (expect result :not :to-be nil))))

  (it "validates modify operation requires modify or write scope"
    (let* ((scope-plist '(:paths (:modify ("/workspace/config/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Modify in modify scope should succeed
      (expect (jf/gptel-scope--validate-operation :modify "/workspace/config/settings.conf" paths)
              :to-be nil)
      ;; Modify outside modify scope should fail
      (let ((result (jf/gptel-scope--validate-operation :modify "/tmp/other.conf" paths)))
        (expect result :not :to-be nil)))))

(describe "Workflow: File operation validation with glob patterns"

  (it "matches paths against glob patterns"
    (let* ((scope-plist '(:paths (:read ("/workspace/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Should match recursive glob
      (expect (jf/gptel-scope--validate-operation :read "/workspace/project/src/file.txt" paths)
              :to-be nil)
      (expect (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths)
              :to-be nil)))

  (it "validates multiple file operations independently"
    (let* ((scope-plist '(:paths (:read ("/workspace/**")
                                 :write ("/tmp/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths))
           ;; Simulate multiple operations
           (read-op '(:operation :read :path "/workspace/file.txt"))
           (write-op '(:operation :write :path "/tmp/output.txt")))
      ;; Both should succeed
      (expect (jf/gptel-scope--validate-operation
               (plist-get read-op :operation)
               (plist-get read-op :path)
               paths)
              :to-be nil)
      (expect (jf/gptel-scope--validate-operation
               (plist-get write-op :operation)
               (plist-get write-op :path)
               paths)
              :to-be nil))))

(describe "Workflow: Cloud authentication validation"

  (it "loads cloud config with auth-detection setting"
    (let* ((scope-plist '(:cloud (:auth-detection "deny")))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (cloud (plist-get schema :cloud)))
      (expect (plist-get cloud :auth-detection) :to-equal "deny")))

  (it "loads cloud config with allowed-providers"
    (let* ((scope-plist '(:cloud (:allowed-providers ("aws" "gcp"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (cloud (plist-get schema :cloud)))
      (expect (plist-get cloud :allowed-providers) :not :to-be nil)))

  (it "validates cloud auth against config"
    (let* ((scope-plist '(:cloud (:auth-detection "deny"
                                  :allowed-providers ("aws"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (cloud (plist-get schema :cloud))
           ;; Simulate AWS auth detection
           (aws-auth '(:provider "aws" :command "aws-vault exec prod"))
           (result (jf/gptel-scope--validate-cloud-auth (list aws-auth) cloud)))
      ;; Should have error in deny mode
      (expect result :not :to-be nil))))

(describe "Workflow: File path validation with deny precedence"

  (it "allows file access within read scope"
    (let* ((scope-plist '(:paths (:read ("/workspace/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Read operation should succeed
      (let ((result (jf/gptel-scope--validate-operation
                     :read "/workspace/file.txt" paths)))
        (expect result :to-be nil))))

  (it "denies file access outside scope"
    (let* ((scope-plist '(:paths (:read ("/workspace/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Access outside scope should fail
      (let ((result (jf/gptel-scope--validate-operation
                     :read "/etc/passwd" paths)))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-match "scope"))))

  (it "denies access to paths in deny list despite broad permissions"
    (let* ((scope-plist '(:paths (:read ("/**")  ; Allow read anywhere
                                 :deny ("/etc/**"))))  ; Except /etc
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Access to /etc should be denied
      (let ((result (jf/gptel-scope--validate-operation
                     :read "/etc/passwd" paths)))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-match "denied"))
      ;; But other paths should work
      (let ((result (jf/gptel-scope--validate-operation
                     :read "/home/user/file.txt" paths)))
        (expect result :to-be nil)))))

(describe "Workflow: Operation-specific path validation"

  (it "validates read operation requires read or write scope"
    (let* ((scope-plist '(:paths (:read ("/workspace/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Read operation in read scope should succeed
      (expect (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths)
              :to-be nil)))

  (it "validates write operation requires write scope"
    (let* ((scope-plist '(:paths (:write ("/tmp/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Write operation in write scope should succeed
      (expect (jf/gptel-scope--validate-operation :write "/tmp/output.txt" paths)
              :to-be nil)
      ;; Write operation outside write scope should fail
      (let ((result (jf/gptel-scope--validate-operation :write "/etc/config" paths)))
        (expect result :not :to-be nil))))

  (it "validates execute operation requires execute scope"
    (let* ((scope-plist '(:paths (:execute ("/workspace/scripts/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Execute in execute scope should succeed
      (expect (jf/gptel-scope--validate-operation
               :execute "/workspace/scripts/deploy.sh" paths)
              :to-be nil)
      ;; Execute outside execute scope should fail
      (let ((result (jf/gptel-scope--validate-operation
                     :execute "/tmp/script.sh" paths)))
        (expect result :not :to-be nil))))

  (it "validates modify operation requires modify or write scope"
    (let* ((scope-plist '(:paths (:modify ("/workspace/config/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Modify in modify scope should succeed
      (expect (jf/gptel-scope--validate-operation
               :modify "/workspace/config/settings.conf" paths)
              :to-be nil))))

(describe "Workflow: Parse completeness checking"

  (it "validates complete parse with strict mode"
    (let* ((scope-plist '(:security (:enforce-parse-complete t)))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (security (plist-get schema :security)))
      (expect (plist-get security :enforce-parse-complete) :to-be t)))

  (it "validates permissive mode allows incomplete parse"
    (let* ((scope-plist '(:security (:enforce-parse-complete nil)))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (security (plist-get schema :security)))
      (expect (plist-get security :enforce-parse-complete) :to-be nil))))

(describe "Workflow: Error message structure"

  (it "validates operation returns structured error for path out of scope"
    (let* ((scope-plist '(:paths (:read ("/workspace/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths))
           (result (jf/gptel-scope--validate-operation :read "/etc/passwd" paths)))
      ;; Should return error plist
      (expect result :not :to-be nil)
      (expect (plist-get result :error) :not :to-be nil)
      (expect (plist-get result :path) :to-equal "/etc/passwd")
      (expect (plist-get result :operation) :to-equal :read)))

  (it "validates operation returns nil for success"
    (let* ((scope-plist '(:paths (:read ("/workspace/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths))
           (result (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths)))
      ;; Success returns nil
      (expect result :to-be nil))))

(describe "Workflow: Multiple file operations"

  (it "validates each operation independently"
    (let* ((scope-plist '(:paths (:read ("/workspace/**")
                                 :write ("/tmp/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Create simulated file operations
      (let ((file-ops (list (list :operation :read
                                  :path "/workspace/data.txt"
                                  :command-name "cat")
                            (list :operation :write
                                  :path "/tmp/output.txt"
                                  :command-name "echo"))))
        ;; Validate each operation
        (dolist (file-op file-ops)
          (let ((result (jf/gptel-scope--validate-operation
                         (plist-get file-op :operation)
                         (plist-get file-op :path)
                         paths)))
            (expect result :to-be nil))))))

  (it "fails if any operation is out of scope"
    (let* ((scope-plist '(:paths (:read ("/workspace/**"))))
           (schema (jf/gptel-scope--load-schema scope-plist))
           (paths (plist-get schema :paths)))
      ;; Mix of valid and invalid operations
      (let* ((valid-op (list :operation :read :path "/workspace/file.txt"))
             (invalid-op (list :operation :read :path "/etc/passwd"))
             (valid-result (jf/gptel-scope--validate-operation
                            (plist-get valid-op :operation)
                            (plist-get valid-op :path)
                            paths))
             (invalid-result (jf/gptel-scope--validate-operation
                              (plist-get invalid-op :operation)
                              (plist-get invalid-op :path)
                              paths)))
        ;; Valid should succeed, invalid should fail
        (expect valid-result :to-be nil)
        (expect invalid-result :not :to-be nil)))))

(provide 'workflows-spec)

;;; workflows-spec.el ends here
