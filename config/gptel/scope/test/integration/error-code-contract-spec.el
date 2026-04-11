;;; error-code-contract-spec.el --- Contract tests for validator error codes and build-violation-info -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Contract tests for the producer-consumer relationship between scope
;; validators (producers) and build-violation-info (consumer).
;;
;; ISSUE: build-violation-info maps error codes to :resource extraction
;; logic using a pcase dispatch.  But the error codes it checks don't
;; all match the codes validators actually produce:
;;   - Checks "path_out_of_scope" (from scope-shell-tools validate-operation)
;;   - Checks "path_denied" (from scope-shell-tools validate-operation)
;;   - Checks "command_denied" (from validate-pipeline-commands)
;;   - Checks "cloud_auth_denied" (from validate-cloud-auth)
;;   - Checks "incomplete_parse" — but the validator produces "parse_incomplete"
;;   - Does NOT check "not-in-scope" (from scope-core validate-path-tool)
;;   - Does NOT check "denied-pattern" (from scope-core validate-path-tool)
;;
;; This means path violations from the path validator (scope-core) fall
;; through to the catch-all, which may or may not extract the right resource.
;;
;; Approach: define the error code vocabulary as a shared constant, then
;; test that both producers and consumer agree on it.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (config-dir (expand-file-name ".." gptel-dir))
       (contracts-dir (expand-file-name "test/contracts/" config-dir)))
  ;; Contract infrastructure
  (add-to-list 'load-path contracts-dir)
  (require 'contract-core)
  (require 'contract-bash-parser)
  (contract--register-buttercup-matcher)
  ;; Scope helpers
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  ;; Interface contracts (source of truth for error codes)
  (require 'scope-interfaces (expand-file-name "scope/interfaces.el" gptel-dir))
  ;; Production scope modules
  (require 'jf-gptel-scope-validation (expand-file-name "scope/scope-validation.el" gptel-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope/scope-tool-wrapper.el" gptel-dir))
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))


;;; Producer-side tests: validators only produce known error codes

(describe "error code contract: producer side"

  (describe "validate-path-tool produces only known codes"

    (it "denied-pattern on deny list match"
      (let* ((config '(:paths (:read ("/workspace/**") :write ()
                               :execute () :modify ()
                               :deny ("/workspace/secret/**"))))
             (result (jf/gptel-scope--validate-path-tool
                      "read_file"
                      (list "/workspace/secret/key.pem")
                      '(:validation path :operation read)
                      config nil)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern")
        (expect (member "denied-pattern" scope/interface--error-codes) :to-be-truthy)))

    (it "not-in-scope when path not matched"
      (let* ((config '(:paths (:read ("/workspace/**") :write ()
                               :execute () :modify () :deny ())))
             (result (jf/gptel-scope--validate-path-tool
                      "read_file"
                      (list "/outside/file.txt")
                      '(:validation path :operation read)
                      config nil)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")
        (expect (member "not-in-scope" scope/interface--error-codes) :to-be-truthy))))

  (describe "validate-pipeline-commands produces only known codes"

    (it "command_denied when command in deny list"
      (let ((result (jf/gptel-scope--validate-pipeline-commands
                     '("ls" "rm") '(:deny ("rm" "sudo")))))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (member "command_denied" scope/interface--error-codes) :to-be-truthy))))

  (describe "validate-operation should produce canonical codes"

    ;; RED: validate-operation uses "path_out_of_scope" and "path_denied"
    ;; but the canonical codes are "not-in-scope" and "denied-pattern"

    (it "should use canonical code for out-of-scope paths"
      (let* ((paths-config '(:read ("/workspace/**") :write ()
                             :execute () :modify () :deny ()))
             (result (jf/gptel-scope--validate-operation
                      :read "/outside/file.txt" paths-config)))
        (expect result :not :to-be nil)
        (expect (member (plist-get result :error) scope/interface--error-codes)
                :to-be-truthy)))

    (it "should use canonical code for deny-matched paths"
      (let* ((paths-config '(:read ("/workspace/**") :write ()
                             :execute () :modify ()
                             :deny ("/workspace/.git/**")))
             (result (jf/gptel-scope--validate-operation
                      :read "/workspace/.git/config" paths-config)))
        (expect result :not :to-be nil)
        (expect (member (plist-get result :error) scope/interface--error-codes)
                :to-be-truthy))))

  (describe "validate-cloud-auth produces only known codes"

    (it "cloud_auth_denied when mode is deny"
      (let ((result (jf/gptel-scope--validate-cloud-auth
                     '(:provider "aws" :command "aws s3 ls")
                     '(:auth-detection "deny"))))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-equal "cloud_auth_denied")
        (expect (member "cloud_auth_denied" scope/interface--error-codes) :to-be-truthy)))

    (it "cloud_provider_denied when provider not in allowed list"
      (let ((result (jf/gptel-scope--validate-cloud-auth
                     '(:provider "azure" :command "az login")
                     '(:auth-detection "warn" :allowed-providers ("aws" "gcp")))))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-equal "cloud_provider_denied")
        (expect (member "cloud_provider_denied" scope/interface--error-codes) :to-be-truthy))))

  (describe "validate-parse-completeness produces only known codes"

    (it "parse_incomplete when parse is incomplete and enforce is true"
      (let ((result (jf/gptel-scope--validate-parse-completeness
                     '(:parse-complete nil :parse-errors "Unexpected token")
                     '(:enforce-parse-complete t))))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-equal "parse_incomplete")
        (expect (member "parse_incomplete" scope/interface--error-codes) :to-be-truthy)))))


;;; Consumer-side tests: build-violation-info handles all known codes

(describe "error code contract: consumer side"

  (describe "build-violation-info extracts :resource for every known code"

    ;; For each error code in the vocabulary, build a representative
    ;; validation error and verify build-violation-info extracts a
    ;; non-nil :resource.

    (it "handles denied-pattern (path validator)"
      (let* ((error-plist '(:error "denied-pattern"
                            :resource "/workspace/secret/key.pem"
                            :message "Path denied by scope"))
             (result (jf/gptel-scope--build-violation-info error-plist "read_file")))
        (expect (plist-get result :resource) :not :to-be nil)))

    (it "handles not-in-scope (path validator)"
      (let* ((error-plist '(:error "not-in-scope"
                            :resource "/outside/file.txt"
                            :message "Path not in read scope"))
             (result (jf/gptel-scope--build-violation-info error-plist "read_file")))
        (expect (plist-get result :resource) :not :to-be nil)))

    (it "handles path_out_of_scope (file-ops validator)"
      (let* ((error-plist '(:error "path_out_of_scope"
                            :path "/outside/file.txt"
                            :operation :read
                            :message "Path not in scope"))
             (result (jf/gptel-scope--build-violation-info error-plist "run_bash_command")))
        (expect (plist-get result :resource) :to-equal "/outside/file.txt")))

    (it "handles path_denied (file-ops validator)"
      (let* ((error-plist '(:error "path_denied"
                            :path "/workspace/.git/config"
                            :operation :read
                            :message "Path denied"))
             (result (jf/gptel-scope--build-violation-info error-plist "run_bash_command")))
        (expect (plist-get result :resource) :to-equal "/workspace/.git/config")))

    (it "handles command_denied (pipeline validator)"
      (let* ((error-plist '(:error "command_denied"
                            :command "rm"
                            :position 0
                            :message "Command denied"))
             (result (jf/gptel-scope--build-violation-info error-plist "run_bash_command")))
        (expect (plist-get result :resource) :to-equal "rm")))

    (it "handles cloud_auth_denied"
      (let* ((error-plist '(:error "cloud_auth_denied"
                            :provider "aws"
                            :command "aws s3 ls"
                            :message "Cloud auth denied"))
             (result (jf/gptel-scope--build-violation-info error-plist "run_bash_command")))
        (expect (plist-get result :resource) :to-equal "aws")))

    (it "extracts provider for cloud_provider_denied"
      ;; RED: build-violation-info has no pcase branch for "cloud_provider_denied"
      ;; so :provider is not extracted — falls to catch-all which gets nil
      (let* ((error-plist '(:error "cloud_provider_denied"
                            :provider "azure"
                            :command "az login"
                            :message "Provider not allowed"))
             (result (jf/gptel-scope--build-violation-info error-plist "run_bash_command")))
        (expect (plist-get result :resource) :to-equal "azure")))

    (it "extracts command for parse_incomplete"
      ;; RED: build-violation-info checks "incomplete_parse" but the validator
      ;; produces "parse_incomplete" — the typo means this falls to catch-all
      (let* ((error-plist '(:error "parse_incomplete"
                            :command "echo 'unterminated"
                            :message "Parse incomplete"))
             (result (jf/gptel-scope--build-violation-info error-plist "run_bash_command")))
        (expect (plist-get result :resource) :to-equal "echo 'unterminated")))

    (it "handles command-not-allowed (no bash_tools config)"
      (let* ((error-plist '(:error "command-not-allowed"
                            :resource "which brew"
                            :command "which brew"
                            :message "No bash_tools config"))
             (result (jf/gptel-scope--build-violation-info error-plist "run_bash_command")))
        (expect (plist-get result :resource) :not :to-be nil))))


  (describe "build-violation-info always populates required fields"

    (dolist (code scope/interface--error-codes)
      (it (format "populates :tool, :reason, :validation-type for %s" code)
        (let* ((error-plist (list :error code
                                  :message (format "Test message for %s" code)
                                  :resource "fallback-resource"
                                  :path "/test/path"
                                  :command "test-cmd"
                                  :provider "aws"))
               (result (jf/gptel-scope--build-violation-info
                        error-plist "read_file")))
          (expect (plist-get result :tool) :to-equal "read_file")
          (expect (plist-get result :reason) :not :to-be nil)
          (expect (plist-get result :resource) :not :to-be nil))))))


;;; Cross-cutting: path validators should use the same error codes

(describe "error code consistency: path validators should agree"

  ;; RED PHASE: scope-core's validate-path-tool and scope-shell-tools's
  ;; validate-operation both check "is this path allowed?" but produce
  ;; different error codes.  They should use the same vocabulary.

  (it "out-of-scope: both validators should use the same error code"
    (let* ((config '(:paths (:read ("/workspace/**") :write ()
                             :execute () :modify () :deny ())))
           (path-result (jf/gptel-scope--validate-path-tool
                         "read_file" (list "/outside/file.txt")
                         '(:validation path :operation read) config nil))
           (paths-config (plist-get config :paths))
           (ops-result (jf/gptel-scope--validate-operation
                        :read "/outside/file.txt" paths-config)))
      ;; Both should produce the same error code
      (expect (plist-get path-result :error)
              :to-equal (plist-get ops-result :error))))

  (it "deny match: both validators should use the same error code"
    (let* ((config '(:paths (:read ("/workspace/**") :write ()
                             :execute () :modify ()
                             :deny ("/workspace/secret/**"))))
           (path-result (jf/gptel-scope--validate-path-tool
                         "read_file" (list "/workspace/secret/key.pem")
                         '(:validation path :operation read) config nil))
           (paths-config (plist-get config :paths))
           (ops-result (jf/gptel-scope--validate-operation
                        :read "/workspace/secret/key.pem" paths-config)))
      ;; Both should produce the same error code
      (expect (plist-get path-result :error)
              :to-equal (plist-get ops-result :error)))))


(provide 'error-code-contract-spec)

;;; error-code-contract-spec.el ends here
