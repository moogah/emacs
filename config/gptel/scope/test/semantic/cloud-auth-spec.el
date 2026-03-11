;;; cloud-auth-spec.el --- Consolidated cloud authentication validation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CONSOLIDATED CLOUD AUTHENTICATION VALIDATION (Stage 7)
;;
;; Consolidates tests from:
;; - tools/test/integration/test-cloud-auth.el (30 ERT tests)
;; - tools/test/behavioral/run-bash-command/cloud-authentication-spec.el (10 Buttercup)
;;
;; Covers: Cloud authentication detection, policy modes (allow/warn/deny),
;; provider filtering (AWS/GCP/Azure), structured error responses, and
;; combined validation scenarios.
;;
;; Dedup strategy: ERT unit-level tests merged where behavioral tests exercise
;; the same validation function with identical inputs. Provider-specific
;; scenarios preserved from both sources when they test different code paths.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (semantic-dir test-dir)
       (scope-test-dir (expand-file-name ".." semantic-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))

;;; Local helpers (from ERT test-cloud-auth.el)

(defun cloud-auth-spec--make-cloud-config (&rest args)
  "Create cloud configuration plist from keyword arguments.
Accepts :auth-detection and :allowed-providers keywords."
  (list :auth-detection (plist-get args :auth-detection)
        :allowed-providers (plist-get args :allowed-providers)))

(defun cloud-auth-spec--make-cloud-auth-ops (provider command)
  "Create cloud-auth operations plist for testing.
PROVIDER is keyword like :aws, :gcp, :azure.
COMMAND is the command string that triggered detection."
  (list :provider provider
        :command command))

;;; Test Suite

(describe "Cloud authentication validation (stage 7)"

  ;; === Detection (unit-level) ===

  (describe "cloud authentication detection"

    (it "detects AWS authentication in allow mode"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws-vault exec prod -- aws s3 ls"))
             (cloud-config (cloud-auth-spec--make-cloud-config :auth-detection "allow"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect result :to-be nil)))

    (it "detects GCP authentication in allow mode"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :gcp "gcloud auth login"))
             (cloud-config (cloud-auth-spec--make-cloud-config :auth-detection "allow"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect result :to-be nil)))

    (it "detects Azure authentication in allow mode"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :azure "az login"))
             (cloud-config (cloud-auth-spec--make-cloud-config :auth-detection "allow"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect result :to-be nil)))

    (it "passes when no cloud auth detected even in deny mode"
      (let* ((cloud-config (cloud-auth-spec--make-cloud-config :auth-detection "deny"))
             (result (jf/gptel-scope--validate-cloud-auth nil cloud-config)))
        (expect result :to-be nil))))

  ;; === Configuration loading ===

  (describe "cloud configuration loading"

    (it "loads allow mode"
      (let* ((cloud-plist (list :auth-detection "allow"))
             (loaded (jf/gptel-scope--load-cloud-config cloud-plist)))
        (expect (plist-get loaded :auth-detection) :to-equal "allow")))

    (it "loads warn mode"
      (let* ((cloud-plist (list :auth-detection "warn"))
             (loaded (jf/gptel-scope--load-cloud-config cloud-plist)))
        (expect (plist-get loaded :auth-detection) :to-equal "warn")))

    (it "loads deny mode"
      (let* ((cloud-plist (list :auth-detection "deny"))
             (loaded (jf/gptel-scope--load-cloud-config cloud-plist)))
        (expect (plist-get loaded :auth-detection) :to-equal "deny")))

    (it "defaults to warn when missing"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws configure"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth nil)))
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected"))))

  ;; === Allow mode enforcement ===

  (describe "allow mode enforcement"

    (it "allows AWS command silently"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws-vault exec prod -- aws s3 ls"))
             (cloud-config (cloud-auth-spec--make-cloud-config :auth-detection "allow"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect result :to-be nil))))

  ;; === Warn mode enforcement ===

  (describe "warn mode enforcement"

    (it "warns for AWS command"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws-vault exec prod -- aws s3 ls"))
             (cloud-config (cloud-auth-spec--make-cloud-config :auth-detection "warn"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
        (expect (plist-get result :error) :to-be nil)))

    (it "identifies provider in warning"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :gcp "gcloud auth login"))
             (cloud-config (cloud-auth-spec--make-cloud-config :auth-detection "warn"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :provider) :to-equal :gcp)
        (expect (string-match-p "gcp" (downcase (plist-get result :message))) :to-be-truthy)))

    (it "includes actionable message and command"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws configure"))
             (cloud-config (cloud-auth-spec--make-cloud-config :auth-detection "warn"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (stringp (plist-get result :message)) :to-be t)
        (expect (plist-get result :command) :not :to-be nil))))

  ;; === Deny mode enforcement ===

  (describe "deny mode enforcement"

    (it "denies AWS command"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws-vault exec prod -- aws s3 ls"))
             (cloud-config (cloud-auth-spec--make-cloud-config
                            :auth-detection "deny"
                            :allowed-providers '()))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :error) :to-equal "cloud_auth_denied")))

    (it "denies with allowed-providers list (provider in list, still denied by mode)"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws configure"))
             (cloud-config (cloud-auth-spec--make-cloud-config
                            :auth-detection "deny"
                            :allowed-providers '(:aws)))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :error) :to-equal "cloud_auth_denied")))

    (it "denies disallowed provider with provider-specific error"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :gcp "gcloud auth login"))
             (cloud-config (cloud-auth-spec--make-cloud-config
                            :auth-detection "deny"
                            :allowed-providers '(:aws)))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :error) :to-equal "cloud_provider_denied")
        (expect (plist-get result :provider) :to-equal :gcp))))

  ;; === Provider filtering ===

  (describe "provider filtering"

    (it "empty allowed list denies all providers"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws configure"))
             (cloud-config (cloud-auth-spec--make-cloud-config
                            :auth-detection "deny"
                            :allowed-providers '()))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :error) :to-equal "cloud_auth_denied")))

    (it "unlisted provider denied"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :azure "az login"))
             (cloud-config (cloud-auth-spec--make-cloud-config
                            :auth-detection "allow"
                            :allowed-providers '(:aws)))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :error) :to-equal "cloud_provider_denied")
        (expect (plist-get result :provider) :to-equal :azure)))

    (it "provider filtering overrides warn mode"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :gcp "gcloud auth login"))
             (cloud-config (cloud-auth-spec--make-cloud-config
                            :auth-detection "warn"
                            :allowed-providers '(:aws)))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :error) :to-equal "cloud_provider_denied"))))

  ;; === Structured error responses ===

  (describe "structured error responses"

    (it "denied error includes all required fields"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws configure"))
             (cloud-config (cloud-auth-spec--make-cloud-config
                            :auth-detection "deny"
                            :allowed-providers '()))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :error) :to-equal "cloud_auth_denied")
        (expect (plist-get result :provider) :to-equal :aws)
        (expect (plist-get result :command) :to-equal "aws configure")
        (expect (stringp (plist-get result :message)) :to-be t)))

    (it "provider restriction error explains allowed list"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :gcp "gcloud auth login"))
             (cloud-config (cloud-auth-spec--make-cloud-config
                            :auth-detection "allow"
                            :allowed-providers '(:aws :azure)))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :error) :to-equal "cloud_provider_denied")
        (expect (string-match-p "gcp" (downcase (plist-get result :message))) :to-be-truthy)
        (expect (plist-get result :allowed-providers) :not :to-be nil)))

    (it "invalid mode fails closed"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws configure"))
             (cloud-config (list :auth-detection "invalid-mode"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud-config)))
        (expect (plist-get result :error) :to-equal "invalid_cloud_auth_mode"))))

  ;; === Full pipeline behavioral tests (from cloud-authentication-spec.el) ===

  (describe "full pipeline behavioral validation"

    (before-each
      (helpers-spec-setup-session)
      (helpers-spec-setup-bash-mocks))

    (after-each
      (helpers-spec-teardown-bash-mocks)
      (helpers-spec-teardown-session))

    (it "allows cloud auth with warning in warn mode"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny: []

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "aws-vault exec prod -- aws s3 ls"
         '("aws-vault" "aws") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/data.json" :command-name "aws"))
         (list :provider "aws" :command "aws-vault exec prod")
         '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "aws-vault exec prod -- aws s3 ls" "/workspace" scope-config)))
          (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
          (expect (plist-get result :provider) :to-equal "aws"))
        (delete-file scope-yml)))

    (it "allows cloud auth silently in allow mode"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny: []

cloud:
  auth_detection: \"allow\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "aws-vault exec prod -- ls"
         '("aws-vault" "ls") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "ls"))
         (list :provider "aws" :command "aws-vault exec prod")
         '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "aws-vault exec prod -- ls" "/workspace" scope-config)))
          (expect result :to-be nil))
        (delete-file scope-yml)))

    (it "denies cloud auth in deny mode"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny: []

cloud:
  auth_detection: \"deny\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "gcloud config set project foo"
         '("gcloud") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :write "/workspace/config.json" :command-name "gcloud"))
         (list :provider "gcp" :command "gcloud config set project")
         '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "gcloud config set project foo" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "cloud_auth_denied")
          (expect (plist-get result :provider) :to-equal "gcp"))
        (delete-file scope-yml)))

    (it "denies unlisted provider even in warn mode"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny: []

cloud:
  auth_detection: \"warn\"
  allowed_providers:
    - aws

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "az login" '("az") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :write "/workspace/auth.json" :command-name "az"))
         (list :provider "azure" :command "az login")
         '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "az login" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "cloud_provider_denied")
          (expect (plist-get result :provider) :to-equal "azure"))
        (delete-file scope-yml)))

    (it "skips cloud auth validation when no cloud auth detected"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny: []

cloud:
  auth_detection: \"deny\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "ls /workspace" '("ls") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "ls"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "ls /workspace" "/workspace" scope-config)))
          (expect result :to-be nil))
        (delete-file scope-yml)))))

(provide 'cloud-auth-spec)
;;; cloud-auth-spec.el ends here
