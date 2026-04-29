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
       (validation-dir test-dir)
       (scope-test-dir (expand-file-name ".." validation-dir))
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

  ;; === Configuration loading (drawer reader) ===
  ;;
  ;; The legacy YAML loader and `jf/gptel-scope-yaml--merge-schema-defaults'
  ;; have been deleted (cycle-3 delete-yaml-and-security-residue).  The
  ;; auth-detection round-trip is now exercised by the drawer-fixture loader
  ;; tests via `jf/gptel-test--with-scope-drawer'.

  (describe "cloud configuration loading (drawer reader)"

    (it "loads allow mode from drawer"
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_SCOPE_CLOUD_AUTH . "allow"))
        (let ((config (jf/gptel-scope--load-from-buffer (current-buffer))))
          (expect (plist-get (plist-get config :cloud) :auth-detection)
                  :to-equal "allow"))))

    (it "loads warn mode from drawer"
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_SCOPE_CLOUD_AUTH . "warn"))
        (let ((config (jf/gptel-scope--load-from-buffer (current-buffer))))
          (expect (plist-get (plist-get config :cloud) :auth-detection)
                  :to-equal "warn"))))

    (it "loads deny mode from drawer"
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_SCOPE_CLOUD_AUTH . "deny"))
        (let ((config (jf/gptel-scope--load-from-buffer (current-buffer))))
          (expect (plist-get (plist-get config :cloud) :auth-detection)
                  :to-equal "deny"))))

    (it "defaults to warn when missing"
      (let* ((cloud-auth (cloud-auth-spec--make-cloud-auth-ops :aws "aws configure"))
             (result (jf/gptel-scope--validate-cloud-auth cloud-auth nil)))
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected"))))

  ;; === Loader normalizes provider names to keywords (cycle-4 Ask 2) ===
  ;;
  ;; The drawer carries provider names as bare strings
  ;; (`:GPTEL_SCOPE_CLOUD_PROVIDERS: aws gcp'); the cloud-auth ops detector
  ;; emits keyword providers (`:aws', `:aws-cli', etc.); the validator's
  ;; `(member provider allowed-providers)' test in
  ;; `jf/gptel-scope--validate-cloud-auth' compares the two.  The loader is
  ;; the parse-boundary normalisation point: drawer strings → keywords, so
  ;; the in-memory plist's `:allowed-providers' is a keyword list and the
  ;; validator's check works without further coercion.  See
  ;; `register/shape/scope-config-plist' (cycle-4 narrowing) and
  ;; `register/boundary/scope-config-loader'.
  (describe "loader normalizes provider names to keywords (cycle-4 Ask 2)"

    (it "loads :allowed-providers as a list of keywords"
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_SCOPE_CLOUD_PROVIDERS . ("aws" "gcp")))
        (let* ((config (jf/gptel-scope--load-from-buffer (current-buffer)))
               (cloud (plist-get config :cloud))
               (allowed (plist-get cloud :allowed-providers)))
          (expect allowed :to-equal '(:aws :gcp))
          (expect (seq-every-p #'keywordp allowed) :to-be-truthy))))

    (it "yields nil :allowed-providers when GPTEL_SCOPE_CLOUD_PROVIDERS is absent"
      ;; Empty drawer → no provider key → nil (not a one-element list of an
      ;; empty keyword like `(:)' or '(""))).
      (jf/gptel-test--with-scope-drawer '()
        (let* ((config (jf/gptel-scope--load-from-buffer (current-buffer)))
               (cloud (plist-get config :cloud)))
          (expect (plist-get cloud :allowed-providers) :to-be nil))))

    (it "validator's keyword-vs-keyword comparison succeeds against the loader output"
      ;; End-to-end: a drawer with allowed-providers `aws gcp', a cloud-auth
      ;; ops plist for `:aws', and the validator returns nil (allowed).
      ;; Pre-normalizer this would emit a `cloud_provider_denied' error
      ;; because `(member :aws ("aws" "gcp"))' is nil.
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_SCOPE_CLOUD_AUTH . "allow")
            (:GPTEL_SCOPE_CLOUD_PROVIDERS . ("aws" "gcp")))
        (let* ((config (jf/gptel-scope--load-from-buffer (current-buffer)))
               (cloud (plist-get config :cloud))
               (cloud-auth (cloud-auth-spec--make-cloud-auth-ops
                            :aws "aws s3 ls"))
               (result (jf/gptel-scope--validate-cloud-auth cloud-auth cloud)))
          (expect result :to-be nil)))))

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
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/workspace/**")
                           :write '("/workspace/**")
                           :auth-detection "warn")))
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
          (expect (plist-get result :provider) :to-equal "aws"))))

    (it "allows cloud auth silently in allow mode"
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/workspace/**")
                           :write '("/workspace/**")
                           :auth-detection "allow")))
        (helpers-spec-mock-bash-parse
         "aws-vault exec prod -- ls"
         '("aws-vault" "ls") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "ls"))
         (list :provider "aws" :command "aws-vault exec prod")
         '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "aws-vault exec prod -- ls" "/workspace" scope-config)))
          (expect result :to-be nil))))

    (it "denies cloud auth in deny mode"
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/workspace/**")
                           :write '("/workspace/**")
                           :auth-detection "deny")))
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
          (expect (plist-get result :provider) :to-equal "gcp"))))

    (it "denies unlisted provider even in warn mode"
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/workspace/**")
                           :write '("/workspace/**")
                           :auth-detection "warn"
                           :allowed-providers '(:aws))))
        (helpers-spec-mock-bash-parse
         "az login" '("az") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :write "/workspace/auth.json" :command-name "az"))
         (list :provider :azure :command "az login")
         '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "az login" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "cloud_provider_denied")
          (expect (plist-get result :provider) :to-equal :azure))))

    (it "skips cloud auth validation when no cloud auth detected"
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/workspace/**")
                           :write '("/workspace/**")
                           :auth-detection "deny")))
        (helpers-spec-mock-bash-parse
         "ls /workspace" '("ls") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "ls"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "ls /workspace" "/workspace" scope-config)))
          (expect result :to-be nil))))))

(provide 'cloud-auth-spec)
;;; cloud-auth-spec.el ends here
