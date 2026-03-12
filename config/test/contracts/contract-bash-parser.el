;;; contract-bash-parser.el --- Contracts for bash-parser ↔ scope boundary -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: testing, contracts

;;; Commentary:

;; Contract definitions for data flowing across the bash-parser → scope boundary.
;; These are the single source of truth for what bash-parser produces and what
;; scope validation can assume.
;;
;; Source of truth references:
;; - Parse result: config/bash-parser/core/bash-parser-core.el (jf/bash-parse)
;; - Semantics: config/bash-parser/analysis/bash-parser-plugins.el (jf/bash-extract-semantics)
;; - File ops: config/bash-parser/plugins/bash-parser-file-ops.el
;;
;; Contracts:
;; - contract/bash-parse-result--validate  — jf/bash-parse output shape
;; - contract/bash-semantics--validate     — jf/bash-extract-semantics output shape
;; - contract/bash-file-op--validate       — individual file operation plist
;; - contract/bash-cloud-auth-op--validate — individual cloud auth operation plist
;; - contract/bash-domains--validate       — :domains alist structure

;;; Code:

(require 'contract-core)

;;; Valid Value Sets

(defconst contract/bash--valid-operations
  '(:read :write :delete :modify :create :create-or-modify :append
    :match-pattern :read-directory :read-metadata :execute)
  "Valid file operation types from bash-parser.")

(defconst contract/bash--valid-confidence-levels
  '(:high :medium :low)
  "Valid confidence levels from bash-parser.")

(defconst contract/bash--valid-sources
  '(:redirection :positional-arg :flag-arg :exec-block :test-expression
    :loop-glob :command-name)
  "Valid file-op source indicators from bash-parser.")

(defconst contract/bash--valid-cloud-providers
  '(:aws :aws-cli :aws-vault :gcloud :azure)
  "Valid cloud auth provider keywords from bash-parser.
:aws is used by command handlers (aws.el), :aws-cli is a legacy variant.")

(defconst contract/bash--valid-domain-keys
  '(:filesystem :authentication :network)
  "Valid domain keys in the semantics :domains alist.
:network is produced by command handlers (e.g., aws.el) for implicit network access.")

;;; File Operation Contract

(defun contract/bash-file-op--validate (file-op)
  "Validate a single FILE-OP plist from bash-parser.
Expected shape: (:file STRING :operation KEYWORD :confidence KEYWORD
                 :command STRING [:source KEYWORD] ...)
:source is optional — universal plugin includes it, command-specific handlers may not.
Returns nil if valid, error string if invalid."
  (cl-block validate
  (let ((errors nil))
    ;; Must be a list
    (unless (listp file-op)
      (cl-return-from validate
        (format "Expected plist, got %s" (type-of file-op))))

    ;; Required: :file (string path)
    (if (not (plist-member file-op :file))
        (push "Missing required key :file" errors)
      (unless (stringp (plist-get file-op :file))
        (push (format ":file must be string, got %S" (plist-get file-op :file)) errors)))

    ;; Required: :operation (valid keyword)
    (if (not (plist-member file-op :operation))
        (push "Missing required key :operation" errors)
      (unless (memq (plist-get file-op :operation) contract/bash--valid-operations)
        (push (format ":operation %S not in valid set" (plist-get file-op :operation)) errors)))

    ;; Required: :confidence (valid keyword)
    (if (not (plist-member file-op :confidence))
        (push "Missing required key :confidence" errors)
      (unless (memq (plist-get file-op :confidence) contract/bash--valid-confidence-levels)
        (push (format ":confidence %S not in valid set" (plist-get file-op :confidence)) errors)))

    ;; Optional: :source (valid keyword if present)
    ;; Universal plugin includes :source, command-specific handlers (aws.el etc.) may omit it
    (when (plist-member file-op :source)
      (unless (memq (plist-get file-op :source) contract/bash--valid-sources)
        (push (format ":source %S not in valid set" (plist-get file-op :source)) errors)))

    ;; Optional: :command (string if present)
    (when (plist-member file-op :command)
      (unless (stringp (plist-get file-op :command))
        (push (format ":command must be string, got %S" (plist-get file-op :command)) errors)))

    (when errors
      (mapconcat #'identity (nreverse errors) "; ")))))

;;; Cloud Auth Operation Contract

(defun contract/bash-cloud-auth-op--validate (cloud-auth-op)
  "Validate a single CLOUD-AUTH-OP plist from bash-parser.
Expected shape: (:provider KEYWORD :command STRING [:operation :authenticate] [:context PLIST])
:operation is optional — universal plugin includes it, command handlers (aws.el) may omit it.
Returns nil if valid, error string if invalid."
  (cl-block validate
  (let ((errors nil))
    (unless (listp cloud-auth-op)
      (cl-return-from validate
        (format "Expected plist, got %s" (type-of cloud-auth-op))))

    ;; Optional: :operation (must be :authenticate if present)
    ;; Universal plugin includes :operation :authenticate, command handlers may omit it
    (when (plist-member cloud-auth-op :operation)
      (unless (eq (plist-get cloud-auth-op :operation) :authenticate)
        (push (format ":operation must be :authenticate, got %S"
                      (plist-get cloud-auth-op :operation))
              errors)))

    ;; Required: :provider (valid keyword)
    (if (not (plist-member cloud-auth-op :provider))
        (push "Missing required key :provider" errors)
      (unless (memq (plist-get cloud-auth-op :provider) contract/bash--valid-cloud-providers)
        (push (format ":provider %S not in valid set" (plist-get cloud-auth-op :provider)) errors)))

    ;; Optional: :command (string if present)
    (when (plist-member cloud-auth-op :command)
      (unless (stringp (plist-get cloud-auth-op :command))
        (push (format ":command must be string, got %S" (plist-get cloud-auth-op :command)) errors)))

    ;; Optional: :context (plist if present)
    (when (plist-member cloud-auth-op :context)
      (let ((ctx (plist-get cloud-auth-op :context)))
        (when (and ctx (not (listp ctx)))
          (push (format ":context must be plist, got %S" ctx) errors))))

    (when errors
      (mapconcat #'identity (nreverse errors) "; ")))))

;;; Domains Alist Contract

(defun contract/bash-domains--validate (domains)
  "Validate DOMAINS alist from bash-parser semantics.
Expected shape: ((:filesystem . FILE-OPS-LIST) (:authentication . AUTH-OPS-LIST))
This is an ALIST, not a plist. Using plist-get on it will silently return nil.
Returns nil if valid, error string if invalid."
  (contract--validate-alist domains
    contract/bash--valid-domain-keys
    #'listp))

;;; Semantics Result Contract

(defun contract/bash-semantics--validate (semantics)
  "Validate SEMANTICS plist from jf/bash-extract-semantics.
Expected shape: (:domains ALIST :coverage PLIST :parse-complete BOOL)
Returns nil if valid, error string if invalid."
  (cl-block validate
    (let ((errors nil))
      ;; Top-level plist validation
      (when-let ((plist-error
                  (contract--validate-plist semantics
                    '((:domains listp)
                      (:coverage listp)
                      (:parse-complete booleanp))
                    nil)))
        (cl-return-from validate plist-error))

      ;; Validate :domains is an alist (not a plist)
      (let ((domains (plist-get semantics :domains)))
        (when domains
          (when-let ((domain-error (contract/bash-domains--validate domains)))
            (push (format ":domains: %s" domain-error) errors))

          ;; Validate file ops within :filesystem domain
          (when-let ((file-ops (alist-get :filesystem domains)))
            (let ((idx 0))
              (dolist (op file-ops)
                (when-let ((op-error (contract/bash-file-op--validate op)))
                  (push (format ":domains.:filesystem[%d]: %s" idx op-error) errors))
                (cl-incf idx))))

          ;; Validate cloud auth ops within :authentication domain
          (when-let ((auth-ops (alist-get :authentication domains)))
            (let ((idx 0))
              (dolist (op auth-ops)
                (when-let ((op-error (contract/bash-cloud-auth-op--validate op)))
                  (push (format ":domains.:authentication[%d]: %s" idx op-error) errors))
                (cl-incf idx))))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

;;; Parse Result Contract

(defun contract/bash-parse-result--validate (parse-result)
  "Validate PARSE-RESULT plist from jf/bash-parse.
Expected shape: (:success BOOL :all-commands LIST :parse-complete BOOL
                 :tokens LIST :command-count INT ...)
Returns nil if valid, error string if invalid."
  (contract--validate-plist parse-result
    '((:success booleanp)
      (:all-commands listp)
      (:parse-complete booleanp))
    '((:tokens listp)
      (:command-count integerp)
      (:error stringp)
      (:parse-errors listp)
      (:type symbolp)
      (:command-name stringp)
      (:subcommand stringp)
      (:flags listp)
      (:positional-args listp)
      (:args listp)
      (:dangerous-p booleanp))))

(provide 'contract-bash-parser)

;;; contract-bash-parser.el ends here
