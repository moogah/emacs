;;; error-code-contract-spec.el --- Contract tests for validator error codes and build-violation-info -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Contract tests for the producer-consumer relationship between scope
;; validators (producers) and build-violation-info (consumer).
;;
;; The canonical error-code vocabulary lives in
;; `scope/interface--error-codes' (interfaces.el).  Every validator must
;; produce only codes from that list, and build-violation-info must
;; handle every code in the list.
;;
;; This file pins the contract from both sides:
;;   - Producer side: each validator entry point is exercised in a way
;;     that yields a known code, and the result is checked against the
;;     canonical list.
;;   - Consumer side: for every canonical code, build a representative
;;     validation error and verify build-violation-info extracts the
;;     expected :resource.

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


;;; Producer-side tests: validators only produce canonical error codes

(describe "error code contract: producer side"

  (describe "validate-path-operation produces canonical codes"

    (it "denied-pattern on deny list match"
      (let* ((config '(:paths (:read ("/workspace/**") :write ()
                               :execute () :modify ()
                               :deny ("/workspace/secret/**"))))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/secret/key.pem" :read config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern")
        (expect (member "denied-pattern" scope/interface--error-codes) :to-be-truthy)))

    (it "not-in-scope when path matches no allow pattern"
      (let* ((config '(:paths (:read ("/workspace/**") :write ()
                               :execute () :modify () :deny ())))
             (result (jf/gptel-scope--validate-path-operation
                      "/outside/file.txt" :read config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")
        (expect (member "not-in-scope" scope/interface--error-codes) :to-be-truthy))))

  (describe "validate-filesystem-tool produces canonical codes"

    ;; validate-filesystem-tool is the entry point used by the
    ;; gptel-make-scoped-tool macro for filesystem tools.  It wraps
    ;; validate-path-operation and must surface the same error codes.

    (it "denied-pattern on deny list match"
      (let* ((config '(:paths (:read ("/workspace/**") :write ()
                               :execute () :modify ()
                               :deny ("/workspace/secret/**"))))
             (result (jf/gptel-scope--validate-filesystem-tool
                      "read_file_in_scope" 'read
                      (list "/workspace/secret/key.pem")
                      config nil)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern")
        (expect (member "denied-pattern" scope/interface--error-codes) :to-be-truthy)))

    (it "not-in-scope when path matches no allow pattern"
      (let* ((config '(:paths (:read ("/workspace/**") :write ()
                               :execute () :modify () :deny ())))
             (result (jf/gptel-scope--validate-filesystem-tool
                      "read_file_in_scope" 'read
                      (list "/outside/file.txt")
                      config nil)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")
        (expect (member "not-in-scope" scope/interface--error-codes) :to-be-truthy))))

  (describe "validate-cloud-auth produces canonical codes"

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

  (describe "validate-parse-completeness produces canonical codes"

    (it "parse_incomplete when parse fails"
      (let ((result (jf/gptel-scope--validate-parse-completeness
                     '(:parse-complete nil :parse-errors ("Unexpected token")))))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-equal "parse_incomplete")
        (expect (member "parse_incomplete" scope/interface--error-codes) :to-be-truthy)))))


;;; Consumer-side tests: build-violation-info handles every canonical code

(describe "error code contract: consumer side"

  (describe "build-violation-info extracts :resource for every canonical code"

    ;; For each canonical code, build a representative validation error
    ;; and verify build-violation-info extracts the resource identifier
    ;; from the field documented in `scope/interface--error-resource-fields'.

    (it "denied-pattern → :resource (path)"
      (let* ((error-plist '(:error "denied-pattern"
                            :resource "/workspace/secret/key.pem"
                            :operation :read
                            :validation-type path
                            :message "Path denied by scope"))
             (result (jf/gptel-scope--build-violation-info
                      error-plist "read_file_in_scope")))
        (expect (plist-get result :resource) :to-equal "/workspace/secret/key.pem")))

    (it "not-in-scope → :resource (path)"
      (let* ((error-plist '(:error "not-in-scope"
                            :resource "/outside/file.txt"
                            :operation :read
                            :validation-type path
                            :message "Path not in read scope"))
             (result (jf/gptel-scope--build-violation-info
                      error-plist "read_file_in_scope")))
        (expect (plist-get result :resource) :to-equal "/outside/file.txt")))

    (it "parse_incomplete → :command"
      (let* ((error-plist '(:error "parse_incomplete"
                            :command "echo 'unterminated"
                            :validation-type bash
                            :message "Parse incomplete"))
             (result (jf/gptel-scope--build-violation-info
                      error-plist "run_bash_command")))
        (expect (plist-get result :resource) :to-equal "echo 'unterminated")))

    (it "cloud_auth_denied → :provider"
      (let* ((error-plist '(:error "cloud_auth_denied"
                            :provider "aws"
                            :command "aws s3 ls"
                            :validation-type bash
                            :message "Cloud auth denied"))
             (result (jf/gptel-scope--build-violation-info
                      error-plist "run_bash_command")))
        (expect (plist-get result :resource) :to-equal "aws")))

    (it "cloud_provider_denied → :provider"
      (let* ((error-plist '(:error "cloud_provider_denied"
                            :provider "azure"
                            :command "az login"
                            :validation-type bash
                            :message "Provider not allowed"))
             (result (jf/gptel-scope--build-violation-info
                      error-plist "run_bash_command")))
        (expect (plist-get result :resource) :to-equal "azure"))))


  (describe "build-violation-info always populates required fields"

    (dolist (code scope/interface--error-codes)
      (it (format "populates :tool, :reason, :resource for %s" code)
        (let* ((error-plist (list :error code
                                  :message (format "Test message for %s" code)
                                  :resource "fallback-resource"
                                  :command "test-cmd"
                                  :provider "aws"
                                  :validation-type 'path))
               (result (jf/gptel-scope--build-violation-info
                        error-plist "read_file_in_scope")))
          (expect (plist-get result :tool) :to-equal "read_file_in_scope")
          (expect (plist-get result :reason) :not :to-be nil)
          (expect (plist-get result :resource) :not :to-be nil)
          (expect (plist-get result :validation-type) :to-equal 'path)))))

  (describe "build-violation-info never returns nil :resource"

    ;; Regression coverage: when the validation error lacks the
    ;; expected resource field for its error code, build-violation-info
    ;; must still produce a non-nil :resource.  Otherwise the nil
    ;; propagates to the transient formatter and crashes the expansion
    ;; UI with (wrong-type-argument stringp nil) on propertize.
    ;; Triggered in practice when the tree-sitter bash grammar is
    ;; missing and parse_incomplete is emitted without a :command.

    (it "returns non-nil :resource for unknown error code without :resource or :path"
      (let* ((error-plist '(:error "some_future_error"
                            :message "Something went wrong"
                            :validation-type bash))
             (result (jf/gptel-scope--build-violation-info
                      error-plist "run_bash_command")))
        (expect (plist-get result :resource) :not :to-be nil)
        (expect (stringp (plist-get result :resource)) :to-be t)))

    (it "returns non-nil :resource for denied-pattern without :resource key"
      (let* ((error-plist '(:error "denied-pattern"
                            :message "Path denied"
                            :validation-type path))
             (result (jf/gptel-scope--build-violation-info
                      error-plist "read_file_in_scope")))
        (expect (plist-get result :resource) :not :to-be nil)
        (expect (stringp (plist-get result :resource)) :to-be t)))

    (it "returns non-nil :resource for parse_incomplete without :command key"
      (let* ((error-plist '(:error "parse_incomplete"
                            :message "Parse incomplete"
                            :validation-type bash))
             (result (jf/gptel-scope--build-violation-info
                      error-plist "run_bash_command")))
        (expect (plist-get result :resource) :not :to-be nil)
        (expect (stringp (plist-get result :resource)) :to-be t)))

    (it "returns non-nil :resource for cloud_auth_denied without :provider key"
      (let* ((error-plist '(:error "cloud_auth_denied"
                            :message "Cloud auth denied"
                            :validation-type bash))
             (result (jf/gptel-scope--build-violation-info
                      error-plist "run_bash_command")))
        (expect (plist-get result :resource) :not :to-be nil)
        (expect (stringp (plist-get result :resource)) :to-be t)))))


;;; Cross-cutting: filesystem-tool wrapper agrees with shared validator

(describe "error code consistency: filesystem-tool wraps validate-path-operation"

  ;; validate-filesystem-tool is a thin wrapper over validate-path-operation.
  ;; They must produce the same error code for the same path/operation, or
  ;; the wrapper introduces silent semantic drift.

  (it "out-of-scope: wrapper and shared validator produce the same code"
    (let* ((config '(:paths (:read ("/workspace/**") :write ()
                             :execute () :modify () :deny ())))
           (wrapper-result (jf/gptel-scope--validate-filesystem-tool
                            "read_file_in_scope" 'read
                            (list "/outside/file.txt") config nil))
           (shared-result (jf/gptel-scope--validate-path-operation
                           "/outside/file.txt" :read config)))
      (expect (plist-get wrapper-result :error)
              :to-equal (plist-get shared-result :error))))

  (it "deny match: wrapper and shared validator produce the same code"
    (let* ((config '(:paths (:read ("/workspace/**") :write ()
                             :execute () :modify ()
                             :deny ("/workspace/secret/**"))))
           (wrapper-result (jf/gptel-scope--validate-filesystem-tool
                            "read_file_in_scope" 'read
                            (list "/workspace/secret/key.pem") config nil))
           (shared-result (jf/gptel-scope--validate-path-operation
                           "/workspace/secret/key.pem" :read config)))
      (expect (plist-get wrapper-result :error)
              :to-equal (plist-get shared-result :error)))))


(provide 'error-code-contract-spec)

;;; error-code-contract-spec.el ends here
