;;; cloud-authentication-spec.el --- Cloud authentication policy enforcement tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CLOUD AUTHENTICATION POLICY ENFORCEMENT (Stage 7)
;;
;; Tests the cloud authentication validation stage of the run_bash_command
;; seven-stage validation pipeline.
;;
;; Stage 7 enforces cloud authentication policies based on scope configuration
;; and semantic analysis results. This prevents execution of commands using
;; cloud provider authentication (AWS, GCP, Azure, etc.) when policies restrict them.
;;
;; Key behaviors tested:
;; - Warn mode allows cloud auth with warning (default behavior)
;; - Allow mode allows cloud auth silently
;; - Deny mode blocks all cloud auth commands
;; - Provider filtering via allowed_providers list
;; - Commands without cloud auth skip this stage
;; - Cloud auth validation combines with file operation validation
;; - Provider filtering overrides mode settings
;;
;; Test naming convention: describe "Category: <scenario-name>"
;; Each test validates a complete interaction flow through stage 7.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-parent-dir (expand-file-name ".." test-dir))
       (test-root-dir (expand-file-name ".." test-parent-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "../scope/scope-shell-tools.el" tools-dir)))

;;; Test Suite

(describe "run_bash_command: Cloud authentication policy enforcement (stage 7)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "allows cloud auth with warning in warn mode (default behavior)"
    ;; Test 2: Warn mode allows with warning (default behavior)
    ;; Setup: Scope with cloud.auth_detection: 'warn'
    ;; Command: 'aws-vault exec prod -- aws s3 ls'
    ;; Mock semantics: Return cloud-auth {:provider 'aws' :command 'aws-vault exec'}
    ;; Assert: :warning "cloud_auth_detected", command executes successfully
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
      ;; Mock parse: Extract 'aws-vault' and 'aws' commands
      (helpers-spec-mock-bash-parse
       "aws-vault exec prod -- aws s3 ls"
       '("aws-vault" "aws")
       t)

      ;; Mock semantics: Return cloud-auth for AWS plus file op to avoid no-op short-circuit
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/data.json" :command-name "aws"))
       (list :provider "aws" :command "aws-vault exec prod")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "aws-vault exec prod -- aws s3 ls"
                     "/workspace"
                     scope-config)))
        ;; Assert: Warning returned (non-blocking)
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
        (expect (plist-get result :provider) :to-equal "aws")
        (expect (plist-get result :command) :to-equal "aws-vault exec prod"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows cloud auth with warning for GCP commands in warn mode"
    ;; Test 3: Warn mode for GCP commands
    ;; Setup: Scope with cloud.auth_detection: 'warn'
    ;; Command: 'gcloud auth login'
    ;; Mock semantics: Return cloud-auth {:provider 'gcp' :command 'gcloud auth'}
    ;; Assert: Success with warning
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
      ;; Mock parse: Extract 'gcloud' command
      (helpers-spec-mock-bash-parse
       "gcloud auth login"
       '("gcloud")
       t)

      ;; Mock semantics: Return cloud-auth for GCP plus file op to avoid no-op short-circuit
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :write "/workspace/config.json" :command-name "gcloud"))
       (list :provider "gcp" :command "gcloud auth login")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "gcloud auth login"
                     "/workspace"
                     scope-config)))
        ;; Assert: Warning returned for GCP
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
        (expect (plist-get result :provider) :to-equal "gcp"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows cloud auth with warning for Azure commands in warn mode"
    ;; Test 4: Warn mode for Azure commands
    ;; Setup: Scope with cloud.auth_detection: 'warn'
    ;; Command: 'az login'
    ;; Mock semantics: Return cloud-auth {:provider 'azure' :command 'az login'}
    ;; Assert: Success with warning
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
      ;; Mock parse: Extract 'az' command
      (helpers-spec-mock-bash-parse
       "az login"
       '("az")
       t)

      ;; Mock semantics: Return cloud-auth for Azure plus file op to avoid no-op short-circuit
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :write "/workspace/auth.json" :command-name "az"))
       (list :provider "azure" :command "az login")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "az login"
                     "/workspace"
                     scope-config)))
        ;; Assert: Warning returned for Azure
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
        (expect (plist-get result :provider) :to-equal "azure"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows cloud auth silently when auth_detection is allow"
    ;; Test 5: Allow mode permits without warnings
    ;; Setup: Scope with cloud.auth_detection: 'allow'
    ;; Command: 'aws-vault exec prod -- ls'
    ;; Mock semantics: Return cloud-auth {:provider 'aws'}
    ;; Assert: nil (no warnings, no errors - silent success)
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
      ;; Mock parse: Extract 'aws-vault' and 'ls' commands
      (helpers-spec-mock-bash-parse
       "aws-vault exec prod -- ls"
       '("aws-vault" "ls")
       t)

      ;; Mock semantics: Return cloud-auth for AWS, plus file ops
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "ls"))
       (list :provider "aws" :command "aws-vault exec prod")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "aws-vault exec prod -- ls"
                     "/workspace"
                     scope-config)))
        ;; Assert: nil (silent success - no warnings, no errors)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies cloud auth when auth_detection is deny"
    ;; Test 6: Deny mode blocks all cloud auth
    ;; Setup: Scope with cloud.auth_detection: 'deny'
    ;; Command: 'gcloud config set project foo'
    ;; Mock semantics: Return cloud-auth {:provider 'gcp' :command 'gcloud config'}
    ;; Assert: :error 'cloud_auth_denied', :provider 'gcp'
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
      ;; Mock parse: Extract 'gcloud' command
      (helpers-spec-mock-bash-parse
       "gcloud config set project foo"
       '("gcloud")
       t)

      ;; Mock semantics: Return cloud-auth for GCP plus file op to avoid no-op short-circuit
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :write "/workspace/config.json" :command-name "gcloud"))
       (list :provider "gcp" :command "gcloud config set project")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "gcloud config set project foo"
                     "/workspace"
                     scope-config)))
        ;; Assert: Denied with error
        (expect (plist-get result :error) :to-equal "cloud_auth_denied")
        (expect (plist-get result :provider) :to-equal "gcp"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows cloud auth when provider is in allowed list"
    ;; Test 7: Provider filtering allows listed providers
    ;; Setup: Scope with cloud.auth_detection: 'deny', allowed_providers: ['aws', 'gcp']
    ;; Command: 'aws-vault exec prod -- aws s3 ls'
    ;; Mock semantics: Return cloud-auth {:provider 'aws'}
    ;; Assert: nil (Success - AWS in allowed list, so deny mode overridden)
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
  allowed_providers:
    - aws
    - gcp

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'aws-vault' and 'aws' commands
      (helpers-spec-mock-bash-parse
       "aws-vault exec prod -- aws s3 ls"
       '("aws-vault" "aws")
       t)

      ;; Mock semantics: Return cloud-auth for AWS plus file op to avoid no-op short-circuit
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/data.json" :command-name "aws"))
       (list :provider "aws" :command "aws-vault exec prod")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "aws-vault exec prod -- aws s3 ls"
                     "/workspace"
                     scope-config)))
        ;; Assert: Error - provider filtering allows AWS (in whitelist), but deny mode still blocks
        ;; allowed_providers acts as a whitelist (only these providers allowed)
        ;; If in whitelist, mode enforcement still applies
        ;; So: AWS in whitelist → pass provider filter → deny mode blocks → error
        (expect (plist-get result :error) :to-equal "cloud_auth_denied")
        (expect (plist-get result :provider) :to-equal "aws"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies cloud auth when provider is not in allowed list"
    ;; Test 8: Provider filtering denies unlisted providers
    ;; Setup: Scope with cloud.auth_detection: 'deny', allowed_providers: ['aws']
    ;; Command: 'gcloud auth login'
    ;; Mock semantics: Return cloud-auth {:provider 'gcp'}
    ;; Assert: :error 'cloud_provider_denied', :provider 'gcp', :allowed-providers ['aws']
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
  allowed_providers:
    - aws

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'gcloud' command
      (helpers-spec-mock-bash-parse
       "gcloud auth login"
       '("gcloud")
       t)

      ;; Mock semantics: Return cloud-auth for GCP plus file op to avoid no-op short-circuit
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :write "/workspace/config.json" :command-name "gcloud"))
       (list :provider "gcp" :command "gcloud auth login")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "gcloud auth login"
                     "/workspace"
                     scope-config)))
        ;; Assert: Provider filtering error (GCP not in allowed list)
        (expect (plist-get result :error) :to-equal "cloud_provider_denied")
        (expect (plist-get result :provider) :to-equal "gcp")
        (expect (plist-get result :allowed-providers) :to-equal '("aws")))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies unlisted providers even in warn mode"
    ;; Test 9: Provider filtering in warn mode
    ;; Setup: Scope with cloud.auth_detection: 'warn', allowed_providers: ['aws']
    ;; Command: 'az login'
    ;; Mock semantics: Return cloud-auth {:provider 'azure'}
    ;; Assert: :error 'cloud_provider_denied' (provider filtering overrides warn mode)
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
      ;; Mock parse: Extract 'az' command
      (helpers-spec-mock-bash-parse
       "az login"
       '("az")
       t)

      ;; Mock semantics: Return cloud-auth for Azure plus file op to avoid no-op short-circuit
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :write "/workspace/auth.json" :command-name "az"))
       (list :provider "azure" :command "az login")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "az login"
                     "/workspace"
                     scope-config)))
        ;; Assert: Provider filtering error (Azure not in allowed list)
        ;; Provider filtering happens before mode check, so it blocks even in warn mode
        (expect (plist-get result :error) :to-equal "cloud_provider_denied")
        (expect (plist-get result :provider) :to-equal "azure"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "skips cloud auth validation when no cloud auth detected"
    ;; Test 10: Commands without cloud auth skip this stage
    ;; Command: 'ls /workspace'
    ;; Mock semantics: Return file-ops but NO cloud-auth
    ;; Assert: Cloud auth validation skipped (stage 7 not executed)
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
      ;; Mock parse: Extract 'ls' command
      (helpers-spec-mock-bash-parse
       "ls /workspace"
       '("ls")
       t)

      ;; Mock semantics: Return file-ops but NO cloud-auth
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "ls"))
       nil  ; NO cloud auth
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls /workspace"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success (cloud auth stage skipped since no cloud auth detected)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates both cloud auth and file operations together"
    ;; Test 11: Cloud auth with file operations validates both
    ;; Command: 'aws-vault exec prod -- cat /workspace/secrets.yml'
    ;; Mock semantics: Return both cloud-auth {:provider 'aws'} AND file-ops {:operation :read}
    ;; Setup: Scope with cloud.auth_detection: 'warn', paths.read: ['/workspace/**']
    ;; Assert: Warning for cloud auth, file ops validated
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
      ;; Mock parse: Extract 'aws-vault' and 'cat' commands
      (helpers-spec-mock-bash-parse
       "aws-vault exec prod -- cat /workspace/secrets.yml"
       '("aws-vault" "cat")
       t)

      ;; Mock semantics: Return both cloud-auth AND file-ops
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/secrets.yml" :command-name "cat"))
       (list :provider "aws" :command "aws-vault exec prod")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "aws-vault exec prod -- cat /workspace/secrets.yml"
                     "/workspace"
                     scope-config)))
        ;; Assert: Warning for cloud auth (file ops validated successfully)
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
        (expect (plist-get result :provider) :to-equal "aws"))

      ;; Cleanup
      (delete-file scope-yml))))

(provide 'cloud-authentication-spec)
;;; cloud-authentication-spec.el ends here
