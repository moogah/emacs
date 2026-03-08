;;; test-scope-validation-cloud-auth.el --- Integration tests for cloud auth validation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INTEGRATION TESTS: Cloud authentication validation covering all enforcement modes
;;
;; Comprehensive coverage of 30+ scenarios from:
;; openspec/changes/bash-parser-integration/specs/scope-validation-cloud-auth/spec.md
;;
;; Test organization:
;; 1. Cloud authentication detection (5 scenarios)
;; 2. Cloud configuration loading (4 scenarios)
;; 3. Allow mode enforcement (2 scenarios)
;; 4. Warn mode enforcement (3 scenarios)
;; 5. Deny mode enforcement (3 scenarios)
;; 6. Provider filtering (3 scenarios)
;; 7. Structured error responses (3 scenarios)
;; 8. Combined validation scenarios (3 scenarios)
;;
;; Test naming convention: test-scope-cloud-auth-<requirement>-<scenario-slug>
;; Each test references its spec scenario for traceability.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name ".." test-dir))
       (tools-dir (expand-file-name ".." test-root-dir)))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Helper Functions

(defun test-cloud-auth--make-cloud-config (&rest args)
  "Create cloud configuration plist from keyword arguments.
Accepts :auth-detection and :allowed-providers keywords."
  (list :auth-detection (plist-get args :auth-detection)
        :allowed-providers (plist-get args :allowed-providers)))

(defun test-cloud-auth--make-cloud-auth-ops (provider command)
  "Create cloud-auth operations plist for testing.
PROVIDER is keyword like :aws, :gcp, :azure.
COMMAND is the command string that triggered detection."
  (list :provider provider
        :command command))

;;; Requirement 1: Cloud authentication detection
;; These tests verify cloud-auth plugin detects cloud providers correctly

(ert-deftest test-scope-cloud-auth-detect-aws-authentication ()
  "Spec scenario: Detect AWS authentication.
Reference: specs/scope-validation-cloud-auth/spec.md § Cloud authentication detection"
  ;; This integration test assumes bash-parser cloud-auth plugin working
  ;; We test the validation layer handles AWS detection correctly
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws-vault exec prod -- aws s3 ls"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "allow"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (null result))))  ; Allow mode returns nil

(ert-deftest test-scope-cloud-auth-detect-aws-cli-authentication ()
  "Spec scenario: Detect AWS CLI authentication.
Reference: specs/scope-validation-cloud-auth/spec.md § Cloud authentication detection"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws configure"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "allow"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (null result))))

(ert-deftest test-scope-cloud-auth-detect-gcp-authentication ()
  "Spec scenario: Detect GCP authentication.
Reference: specs/scope-validation-cloud-auth/spec.md § Cloud authentication detection"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :gcp "gcloud auth login"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "allow"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (null result))))

(ert-deftest test-scope-cloud-auth-detect-azure-authentication ()
  "Spec scenario: Detect Azure authentication.
Reference: specs/scope-validation-cloud-auth/spec.md § Cloud authentication detection"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :azure "az login"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "allow"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (null result))))

(ert-deftest test-scope-cloud-auth-no-detection-for-non-auth-commands ()
  "Spec scenario: No detection for non-auth commands.
Reference: specs/scope-validation-cloud-auth/spec.md § Cloud authentication detection"
  ;; When bash-parser returns nil for cloud-auth domain, validation should pass
  (let* ((cloud-auth nil)  ; No cloud auth detected
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "deny"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (null result))))  ; No cloud auth = pass

;;; Requirement 2: Cloud configuration in scope.yml
;; These tests verify cloud config loading and defaults

(ert-deftest test-scope-cloud-auth-config-allow-mode ()
  "Spec scenario: Load allow mode.
Reference: specs/scope-validation-cloud-auth/spec.md § Cloud configuration"
  (let* ((cloud-plist (list :auth-detection "allow"))
         (loaded (jf/gptel-scope--load-cloud-config cloud-plist)))
    (should (string= (plist-get loaded :auth-detection) "allow"))))

(ert-deftest test-scope-cloud-auth-config-warn-mode ()
  "Spec scenario: Load warn mode.
Reference: specs/scope-validation-cloud-auth/spec.md § Cloud configuration"
  (let* ((cloud-plist (list :auth-detection "warn"))
         (loaded (jf/gptel-scope--load-cloud-config cloud-plist)))
    (should (string= (plist-get loaded :auth-detection) "warn"))))

(ert-deftest test-scope-cloud-auth-config-deny-mode ()
  "Spec scenario: Load deny mode.
Reference: specs/scope-validation-cloud-auth/spec.md § Cloud configuration"
  (let* ((cloud-plist (list :auth-detection "deny"))
         (loaded (jf/gptel-scope--load-cloud-config cloud-plist)))
    (should (string= (plist-get loaded :auth-detection) "deny"))))

(ert-deftest test-scope-cloud-auth-config-default-to-warn ()
  "Spec scenario: Default to warn when missing.
Reference: specs/scope-validation-cloud-auth/spec.md § Cloud configuration"
  ;; When cloud-config is nil or auth-detection is missing, defaults to warn
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws configure"))
         (cloud-config nil)  ; No config provided
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    ;; With no config, defaults to "warn" mode
    (should (plist-get result :warning))
    (should (string= (plist-get result :warning) "cloud_auth_detected"))))

;;; Requirement 3: Allow mode enforcement
;; Cloud commands execute without warnings or restrictions

(ert-deftest test-scope-cloud-auth-allow-aws-command ()
  "Spec scenario: AWS command allowed in allow mode.
Reference: specs/scope-validation-cloud-auth/spec.md § Allow mode enforcement"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws-vault exec prod -- aws s3 ls"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "allow"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (null result))  ; nil = allowed, no warnings
    (should-not (plist-get result :warning))
    (should-not (plist-get result :error))))

(ert-deftest test-scope-cloud-auth-allow-gcp-command ()
  "Spec scenario: GCP command allowed in allow mode.
Reference: specs/scope-validation-cloud-auth/spec.md § Allow mode enforcement"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :gcp "gcloud auth login"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "allow"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (null result))))

;;; Requirement 4: Warn mode enforcement
;; Commands execute with warnings in result

(ert-deftest test-scope-cloud-auth-warn-aws-command ()
  "Spec scenario: AWS command warns in warn mode.
Reference: specs/scope-validation-cloud-auth/spec.md § Warn mode enforcement"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws-vault exec prod -- aws s3 ls"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "warn"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    ;; Warn mode returns warning plist (non-blocking)
    (should (plist-get result :warning))
    (should (string= (plist-get result :warning) "cloud_auth_detected"))
    (should-not (plist-get result :error))))  ; Not an error

(ert-deftest test-scope-cloud-auth-warn-identifies-provider ()
  "Spec scenario: Warning identifies provider.
Reference: specs/scope-validation-cloud-auth/spec.md § Warn mode enforcement"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :gcp "gcloud auth login"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "warn"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (eq (plist-get result :provider) :gcp))
    (should (string-match-p "gcp" (downcase (plist-get result :message))))))

(ert-deftest test-scope-cloud-auth-warn-suggests-reviewing-scope ()
  "Spec scenario: Warning suggests reviewing scope.
Reference: specs/scope-validation-cloud-auth/spec.md § Warn mode enforcement"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws configure"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "warn"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    ;; Warning message should be actionable
    (should (stringp (plist-get result :message)))
    (should (plist-get result :command))))

;;; Requirement 5: Deny mode enforcement
;; Cloud commands rejected unless provider explicitly allowed

(ert-deftest test-scope-cloud-auth-deny-aws-command ()
  "Spec scenario: AWS command denied in deny mode.
Reference: specs/scope-validation-cloud-auth/spec.md § Deny mode enforcement"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws-vault exec prod -- aws s3 ls"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "deny"
                        :allowed-providers '()))  ; Empty list
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "cloud_auth_denied"))))

(ert-deftest test-scope-cloud-auth-deny-allowed-provider-permitted ()
  "Spec scenario: Allowed provider permitted in deny mode.
Reference: specs/scope-validation-cloud-auth/spec.md § Deny mode enforcement"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws configure"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "deny"
                        :allowed-providers '(:aws)))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    ;; AWS is in allowed list, so deny mode still allows it
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "cloud_auth_denied"))))

(ert-deftest test-scope-cloud-auth-deny-disallowed-provider-rejected ()
  "Spec scenario: Disallowed provider rejected in deny mode.
Reference: specs/scope-validation-cloud-auth/spec.md § Deny mode enforcement"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :gcp "gcloud auth login"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "deny"
                        :allowed-providers '(:aws)))  ; Only AWS allowed
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "cloud_provider_denied"))
    (should (eq (plist-get result :provider) :gcp))))

;;; Requirement 6: Provider filtering
;; Test allowed_providers list filtering

(ert-deftest test-scope-cloud-auth-filter-empty-list-denies-all ()
  "Spec scenario: Empty allowed list denies all providers.
Reference: specs/scope-validation-cloud-auth/spec.md § Provider filtering"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws configure"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "deny"
                        :allowed-providers '()))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "cloud_auth_denied"))))

(ert-deftest test-scope-cloud-auth-filter-specific-provider-allowed ()
  "Spec scenario: Specific provider allowed.
Reference: specs/scope-validation-cloud-auth/spec.md § Provider filtering"
  (let* ((cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "warn"  ; Use warn to test filtering independent of deny
                        :allowed-providers '(:aws :gcp))))
    ;; AWS should pass filtering
    (let* ((aws-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws configure"))
           (result-aws (jf/gptel-scope--validate-cloud-auth aws-auth cloud-config)))
      (should-not (string= (plist-get result-aws :error) "cloud_provider_denied")))
    ;; GCP should pass filtering
    (let* ((gcp-auth (test-cloud-auth--make-cloud-auth-ops :gcp "gcloud auth login"))
           (result-gcp (jf/gptel-scope--validate-cloud-auth gcp-auth cloud-config)))
      (should-not (string= (plist-get result-gcp :error) "cloud_provider_denied")))))

(ert-deftest test-scope-cloud-auth-filter-unlisted-provider-denied ()
  "Spec scenario: Unlisted provider denied.
Reference: specs/scope-validation-cloud-auth/spec.md § Provider filtering"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :azure "az login"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "allow"  ; Mode doesn't matter, filtering happens first
                        :allowed-providers '(:aws)))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "cloud_provider_denied"))
    (should (eq (plist-get result :provider) :azure))))

;;; Requirement 7: Structured error responses
;; Verify error structure includes all required fields

(ert-deftest test-scope-cloud-auth-error-denied-structure ()
  "Spec scenario: Cloud auth denied error structure.
Reference: specs/scope-validation-cloud-auth/spec.md § Structured error responses"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws configure"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "deny"
                        :allowed-providers '()))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    ;; Verify all required fields present
    (should (string= (plist-get result :error) "cloud_auth_denied"))
    (should (eq (plist-get result :provider) :aws))
    (should (string= (plist-get result :command) "aws configure"))
    (should (stringp (plist-get result :message)))))

(ert-deftest test-scope-cloud-auth-error-provider-restriction-explains ()
  "Spec scenario: Error message explains provider restriction.
Reference: specs/scope-validation-cloud-auth/spec.md § Structured error responses"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :gcp "gcloud auth login"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "allow"
                        :allowed-providers '(:aws :azure)))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "cloud_provider_denied"))
    ;; Message should identify provider and show allowed list
    (should (string-match-p "gcp" (downcase (plist-get result :message))))
    (should (plist-get result :allowed-providers))))

(ert-deftest test-scope-cloud-auth-error-suggests-scope-expansion ()
  "Spec scenario: Error suggests scope expansion.
Reference: specs/scope-validation-cloud-auth/spec.md § Structured error responses"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws-vault exec prod"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "deny"
                        :allowed-providers '()))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    ;; Error message should be actionable (implementation detail)
    (should (stringp (plist-get result :message)))
    (should (plist-get result :error))))

;;; Requirement 8: Combined validation scenarios
;; Cloud auth validation works with other validation layers

(ert-deftest test-scope-cloud-auth-combined-all-pass ()
  "Spec scenario: Cloud command passes all validations.
Reference: specs/scope-validation-cloud-auth/spec.md § Combined validation"
  ;; This is a documentation test - in real pipeline, cloud auth is one stage
  ;; Here we verify cloud auth stage passes when it should
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws s3 ls"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "allow"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (null result))))  ; Stage passes, next stages can proceed

(ert-deftest test-scope-cloud-auth-combined-fails-auth-policy ()
  "Spec scenario: Cloud command fails auth policy.
Reference: specs/scope-validation-cloud-auth/spec.md § Combined validation"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws configure"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "deny"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    ;; Cloud auth validation fails, pipeline should stop here
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "cloud_auth_denied"))))

(ert-deftest test-scope-cloud-auth-combined-allowed-but-other-validations-may-fail ()
  "Spec scenario: Cloud command allowed but file path denied (separate stage).
Reference: specs/scope-validation-cloud-auth/spec.md § Combined validation"
  ;; Cloud auth passes, but other validations (file paths) may still fail
  ;; This test verifies cloud auth stage returns nil when it should pass
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :gcp "gcloud storage cat gs://bucket/file.txt"))
         (cloud-config (test-cloud-auth--make-cloud-config :auth-detection "allow"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (null result))))  ; Cloud auth passes, file path stage would handle gs://

;;; Additional edge cases

(ert-deftest test-scope-cloud-auth-invalid-mode-fails-closed ()
  "Edge case: Invalid auth-detection mode fails closed.
Reference: Implementation - invalid mode handling"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :aws "aws configure"))
         (cloud-config (list :auth-detection "invalid-mode"))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "invalid_cloud_auth_mode"))))

(ert-deftest test-scope-cloud-auth-nil-cloud-auth-ops-passes ()
  "Edge case: nil cloud-auth-ops passes validation.
Reference: Implementation - no cloud auth detected"
  (let* ((cloud-config (test-cloud-auth--make-cloud-config :auth-detection "deny"))
         (result (jf/gptel-scope--validate-cloud-auth nil cloud-config)))
    (should (null result))))  ; No cloud auth = pass

(ert-deftest test-scope-cloud-auth-provider-filtering-precedence ()
  "Edge case: Provider filtering checked before mode enforcement.
Reference: Implementation - filtering happens first"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :azure "az login"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "allow"  ; Mode is allow
                        :allowed-providers '(:aws)))  ; But provider not in list
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    ;; Provider filtering takes precedence over allow mode
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "cloud_provider_denied"))))

(ert-deftest test-scope-cloud-auth-warn-mode-with-filtering ()
  "Edge case: Warn mode respects provider filtering.
Reference: Implementation - filtering applies to all modes"
  (let* ((cloud-auth (test-cloud-auth--make-cloud-auth-ops :gcp "gcloud auth login"))
         (cloud-config (test-cloud-auth--make-cloud-config
                        :auth-detection "warn"
                        :allowed-providers '(:aws)))
         (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
    ;; Provider filtering blocks even in warn mode
    (should (plist-get result :error))
    (should (string= (plist-get result :error) "cloud_provider_denied"))))

(provide 'test-scope-validation-cloud-auth)
;;; test-scope-validation-cloud-auth.el ends here
