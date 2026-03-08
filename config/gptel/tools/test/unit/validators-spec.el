;;; validators-spec.el --- Unit tests for scope validation validators -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; UNIT TESTS: Permission validators and error formatting
;;
;; Tests 25 isolated validation functions:
;; - Operation validators (validate-operation)
;; - Schema validators (validate-cloud-config, validate-security-config)
;; - File operation validators
;; - Parse completeness checking
;; - Error message formatting
;;
;; Test organization:
;; 1. Operation validation with permission hierarchy (8 tests)
;; 2. Cloud config validation (5 tests)
;; 3. Security config validation (5 tests)
;; 4. Parse completeness validation (4 tests)
;; 5. Error message structure (3 tests)

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name "../.." test-dir))
       (tools-dir test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Helper Functions

(defun test-validators--make-paths-config (&rest args)
  "Create paths configuration plist from keyword arguments."
  (list :read (plist-get args :read)
        :write (plist-get args :write)
        :execute (plist-get args :execute)
        :modify (plist-get args :modify)
        :deny (plist-get args :deny)))

;;; Operation Validation Tests

(describe "jf/gptel-scope--validate-operation"

  (describe "read operation hierarchy"
    (it "allows read if in read patterns"
      (let* ((paths-config (test-validators--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :read "/workspace/file.txt" paths-config)))
        (expect result :to-be nil)))  ; nil = success

    (it "allows read if in write patterns (write includes read)"
      (let* ((paths-config (test-validators--make-paths-config
                            :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :read "/workspace/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "denies read if not in read or write patterns"
      (let* ((paths-config (test-validators--make-paths-config
                            :read '("/tmp/**")))
             (result (jf/gptel-scope--validate-operation
                      :read "/workspace/file.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :operation) :to-equal :read))))

  (describe "write operation"
    (it "allows write if in write patterns"
      (let* ((paths-config (test-validators--make-paths-config
                            :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :write "/workspace/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "denies write if not in write patterns"
      (let* ((paths-config (test-validators--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :write "/workspace/file.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :operation) :to-equal :write))))

  (describe "modify operation hierarchy"
    (it "allows modify if in modify patterns"
      (let* ((paths-config (test-validators--make-paths-config
                            :modify '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :modify "/workspace/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "allows modify if in write patterns (write includes modify)"
      (let* ((paths-config (test-validators--make-paths-config
                            :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :modify "/workspace/file.txt" paths-config)))
        (expect result :to-be nil))))

  (describe "execute operation"
    (it "requires explicit execute permission"
      (let* ((paths-config (test-validators--make-paths-config
                            :execute '("/workspace/scripts/**")))
             (result (jf/gptel-scope--validate-operation
                      :execute "/workspace/scripts/deploy.sh" paths-config)))
        (expect result :to-be nil)))

    (it "denies execute even if in write patterns"
      (let* ((paths-config (test-validators--make-paths-config
                            :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :execute "/workspace/script.sh" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope"))))

  (describe "deny precedence"
    (it "denies access even if in allow patterns"
      (let* ((paths-config (test-validators--make-paths-config
                            :read '("/workspace/**")
                            :deny '("/workspace/secret/**")))
             (result (jf/gptel-scope--validate-operation
                      :read "/workspace/secret/key.pem" paths-config)))
        (expect (plist-get result :error) :to-equal "path_denied")
        (expect (plist-get result :path) :to-equal "/workspace/secret/key.pem")))

    (it "deny overrides write permission"
      (let* ((paths-config (test-validators--make-paths-config
                            :write '("/workspace/**")
                            :deny '("/workspace/.git/**")))
             (result (jf/gptel-scope--validate-operation
                      :write "/workspace/.git/config" paths-config)))
        (expect (plist-get result :error) :to-equal "path_denied")))))

;;; Cloud Config Validation Tests

(describe "jf/gptel-scope--validate-cloud-config"

  (it "accepts valid 'allow' mode"
    (let ((cloud-config '(:auth-detection "allow")))
      (expect (jf/gptel-scope--validate-cloud-config cloud-config) :to-be t)))

  (it "accepts valid 'warn' mode"
    (let ((cloud-config '(:auth-detection "warn")))
      (expect (jf/gptel-scope--validate-cloud-config cloud-config) :to-be t)))

  (it "accepts valid 'deny' mode"
    (let ((cloud-config '(:auth-detection "deny")))
      (expect (jf/gptel-scope--validate-cloud-config cloud-config) :to-be t)))

  (it "rejects invalid mode"
    (let ((cloud-config '(:auth-detection "invalid")))
      (expect (jf/gptel-scope--validate-cloud-config cloud-config)
              :to-throw)))

  (it "handles nil config"
    (expect (jf/gptel-scope--validate-cloud-config nil) :to-be t)))

;;; Security Config Validation Tests

(describe "jf/gptel-scope--validate-security-config"

  (it "accepts valid boolean enforce-parse-complete"
    (let ((security-config '(:enforce-parse-complete t)))
      (expect (jf/gptel-scope--validate-security-config security-config) :to-be t)))

  (it "accepts valid threshold in range"
    (let ((security-config '(:max-coverage-threshold 0.8)))
      (expect (jf/gptel-scope--validate-security-config security-config) :to-be t)))

  (it "rejects threshold below 0.0"
    (let ((security-config '(:max-coverage-threshold -0.1)))
      (expect (jf/gptel-scope--validate-security-config security-config)
              :to-throw)))

  (it "rejects threshold above 1.0"
    (let ((security-config '(:max-coverage-threshold 1.5)))
      (expect (jf/gptel-scope--validate-security-config security-config)
              :to-throw)))

  (it "accepts edge values 0.0 and 1.0"
    (let ((config-zero '(:max-coverage-threshold 0.0))
          (config-one '(:max-coverage-threshold 1.0)))
      (expect (jf/gptel-scope--validate-security-config config-zero) :to-be t)
      (expect (jf/gptel-scope--validate-security-config config-one) :to-be t))))

;;; Parse Completeness Validation Tests

(describe "jf/gptel-scope--validate-parse-completeness"

  (it "returns nil for complete parse with strict mode"
    (let* ((parse-result '(:parse-complete t))
           (security-config '(:enforce-parse-complete t))
           (result (jf/gptel-scope--validate-parse-completeness
                    parse-result security-config)))
      (expect result :to-be nil)))

  (it "returns error for incomplete parse with strict mode"
    (let* ((parse-result '(:parse-complete nil
                           :parse-errors "Unexpected token at line 2"))
           (security-config '(:enforce-parse-complete t))
           (result (jf/gptel-scope--validate-parse-completeness
                    parse-result security-config)))
      (expect (plist-get result :error) :to-equal "parse_incomplete")
      (expect (plist-get result :parse-errors) :to-equal "Unexpected token at line 2")))

  (it "returns nil for incomplete parse with permissive mode"
    (let* ((parse-result '(:parse-complete nil
                           :parse-errors "Some error"))
           (security-config '(:enforce-parse-complete nil))
           (result (jf/gptel-scope--validate-parse-completeness
                    parse-result security-config)))
      (expect result :to-be nil)))

  (it "includes error details in message"
    (let* ((parse-result '(:parse-complete nil
                           :parse-errors "Syntax error: unexpected }"))
           (security-config '(:enforce-parse-complete t))
           (result (jf/gptel-scope--validate-parse-completeness
                    parse-result security-config)))
      (expect (plist-get result :message) :to-match "Syntax error: unexpected }"))))

;;; Error Message Structure Tests

(describe "validation error structure"

  (it "includes all required fields for path_out_of_scope"
    (let* ((paths-config (test-validators--make-paths-config :read '("/workspace/**")))
           (result (jf/gptel-scope--validate-operation
                    :read "/etc/passwd" paths-config)))
      (expect (plist-get result :error) :to-equal "path_out_of_scope")
      (expect (plist-get result :path) :to-equal "/etc/passwd")
      (expect (plist-get result :operation) :to-equal :read)
      (expect (plist-get result :required-scope) :not :to-be nil)
      (expect (plist-get result :message) :not :to-be nil)))

  (it "includes all required fields for path_denied"
    (let* ((paths-config (test-validators--make-paths-config
                          :read '("/workspace/**")
                          :deny '("/workspace/secret/**")))
           (result (jf/gptel-scope--validate-operation
                    :read "/workspace/secret/key.pem" paths-config)))
      (expect (plist-get result :error) :to-equal "path_denied")
      (expect (plist-get result :path) :not :to-be nil)
      (expect (plist-get result :operation) :to-equal :read)
      (expect (plist-get result :message) :not :to-be nil)))

  (it "formats human-readable messages"
    (let* ((paths-config (test-validators--make-paths-config :read '("/workspace/**")))
           (result (jf/gptel-scope--validate-operation
                    :write "/tmp/file.txt" paths-config))
           (message (plist-get result :message)))
      (expect message :to-match "Path not in")
      (expect message :to-match "/tmp/file.txt"))))

(provide 'validators-spec)

;;; validators-spec.el ends here
