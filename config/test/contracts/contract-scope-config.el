;;; contract-scope-config.el --- Contracts for scope.yml configuration data -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: testing, contracts

;;; Commentary:

;; Contract definitions for scope configuration data loaded from scope.yml.
;; These are the single source of truth for what a valid merged scope config
;; looks like after jf/gptel-scope--load-schema processes it.
;;
;; Source of truth references:
;; - Schema defaults: config/gptel/scope/scope-shell-tools.el (jf/gptel-scope-schema-defaults)
;; - Schema loader: config/gptel/scope/scope-shell-tools.el (jf/gptel-scope--load-schema)
;;
;; Contracts:
;; - contract/scope-paths--validate       — :paths section shape
;; - contract/scope-cloud--validate       — :cloud section shape
;; - contract/scope-security--validate    — :security section shape
;; - contract/scope-bash-tools--validate  — :bash-tools section shape
;; - contract/scope-config--validate      — top-level scope config shape

;;; Code:

(require 'contract-core)

;;; Valid Value Sets

(defconst contract/scope-config--valid-auth-detection-values
  '("allow" "warn" "deny")
  "Valid values for cloud.auth-detection field.")

(defconst contract/scope-config--valid-cloud-providers
  '(:aws :gcp :azure :aws-cli :aws-vault :gcloud)
  "Valid cloud provider keywords for cloud.allowed-providers.")

(defconst contract/scope-config--required-path-keys
  '(:read :write :execute :modify :deny)
  "Required keys in the :paths section.")

;;; Paths Contract

(defun contract/scope-paths--validate (paths)
  "Validate PATHS plist from scope config.
Expected shape: (:read LIST :write LIST :execute LIST :modify LIST :deny LIST)
All 5 keys required (filled by schema defaults if missing from YAML).
Each value must be a list (may be empty) of strings (glob patterns).
Returns nil if valid, error string if invalid."
  (cl-block validate
    (let ((errors nil))
      ;; Must be a list (plist)
      (unless (listp paths)
        (cl-return-from validate
          (format "Expected plist, got %s" (type-of paths))))

      ;; All 5 path keys required
      (dolist (key contract/scope-config--required-path-keys)
        (if (not (plist-member paths key))
            (push (format "Missing required key %S" key) errors)
          (let ((value (plist-get paths key)))
            ;; Value must be a list
            (unless (listp value)
              (push (format "%S must be a list, got %S" key value) errors))
            ;; Each entry must be a string (glob pattern)
            (when (listp value)
              (let ((idx 0))
                (dolist (entry value)
                  (unless (stringp entry)
                    (push (format "%S[%d] must be string, got %S" key idx entry) errors))
                  (cl-incf idx)))))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

;;; Cloud Contract

(defun contract/scope-cloud--validate (cloud)
  "Validate CLOUD plist from scope config.
Expected shape: (:auth-detection STRING [:allowed-providers LIST])
:auth-detection required, must be one of \"allow\", \"warn\", \"deny\".
:allowed-providers optional, list of keyword symbols.
Returns nil if valid, error string if invalid."
  (cl-block validate
    (let ((errors nil))
      ;; Must be a list (plist)
      (unless (listp cloud)
        (cl-return-from validate
          (format "Expected plist, got %s" (type-of cloud))))

      ;; Required: :auth-detection (valid string)
      (if (not (plist-member cloud :auth-detection))
          (push "Missing required key :auth-detection" errors)
        (let ((value (plist-get cloud :auth-detection)))
          (unless (and (stringp value)
                       (member value contract/scope-config--valid-auth-detection-values))
            (push (format ":auth-detection %S not in valid set %S"
                          value contract/scope-config--valid-auth-detection-values)
                  errors))))

      ;; Optional: :allowed-providers (list of keywords if present)
      (when (plist-member cloud :allowed-providers)
        (let ((providers (plist-get cloud :allowed-providers)))
          (when providers
            (unless (listp providers)
              (push (format ":allowed-providers must be a list, got %S" providers) errors))
            (when (listp providers)
              (let ((idx 0))
                (dolist (p providers)
                  (unless (memq p contract/scope-config--valid-cloud-providers)
                    (push (format ":allowed-providers[%d] %S not in valid set" idx p) errors))
                  (cl-incf idx)))))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

;;; Security Contract

(defun contract/scope-security--validate (security)
  "Validate SECURITY plist from scope config.
Expected shape: (:enforce-parse-complete BOOLEAN :max-coverage-threshold NUMBER)
Both required (filled by schema defaults).
:enforce-parse-complete must be t or nil (not :true/:false — YAML keyword bug).
:max-coverage-threshold must be a number between 0.0 and 1.0.
Returns nil if valid, error string if invalid."
  (cl-block validate
    (let ((errors nil))
      ;; Must be a list (plist)
      (unless (listp security)
        (cl-return-from validate
          (format "Expected plist, got %s" (type-of security))))

      ;; Required: :enforce-parse-complete (boolean)
      (if (not (plist-member security :enforce-parse-complete))
          (push "Missing required key :enforce-parse-complete" errors)
        (let ((value (plist-get security :enforce-parse-complete)))
          ;; Must be exactly t or nil, not :true/:false (YAML keyword bug)
          (unless (or (eq value t) (eq value nil))
            (push (format ":enforce-parse-complete must be t or nil, got %S (YAML boolean bug?)"
                          value)
                  errors))))

      ;; Required: :max-coverage-threshold (number in 0.0-1.0)
      (if (not (plist-member security :max-coverage-threshold))
          (push "Missing required key :max-coverage-threshold" errors)
        (let ((value (plist-get security :max-coverage-threshold)))
          (unless (numberp value)
            (push (format ":max-coverage-threshold must be a number, got %S" value) errors))
          (when (numberp value)
            (unless (and (>= value 0.0) (<= value 1.0))
              (push (format ":max-coverage-threshold must be between 0.0 and 1.0, got %S" value)
                    errors)))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

;;; Bash Tools Contract

(defun contract/scope-bash-tools--validate (bash-tools)
  "Validate BASH-TOOLS plist from scope config.
Expected shape: (:deny LIST) or nil.
:deny is a list of strings (command names).
Must NOT contain :categories key (migration validation).
Returns nil if valid, error string if invalid."
  (cl-block validate
    ;; nil is valid (section not present)
    (when (null bash-tools)
      (cl-return-from validate nil))

    (let ((errors nil))
      ;; Must be a list (plist)
      (unless (listp bash-tools)
        (cl-return-from validate
          (format "Expected plist or nil, got %s" (type-of bash-tools))))

      ;; Migration validation: :categories must NOT be present
      (when (plist-member bash-tools :categories)
        (push "Contains :categories key — migration required (remove categories, keep only deny list)"
              errors))

      ;; Optional: :deny (list of strings if present)
      (when (plist-member bash-tools :deny)
        (let ((deny-list (plist-get bash-tools :deny)))
          (unless (listp deny-list)
            (push (format ":deny must be a list, got %S" deny-list) errors))
          (when (listp deny-list)
            (let ((idx 0))
              (dolist (cmd deny-list)
                (unless (stringp cmd)
                  (push (format ":deny[%d] must be string, got %S" idx cmd) errors))
                (cl-incf idx))))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

;;; Top-Level Config Contract

(defun contract/scope-config--validate (config)
  "Validate CONFIG plist from jf/gptel-scope--load-schema.
Expected shape: (:paths PLIST :cloud PLIST :security PLIST [:bash-tools PLIST])
Delegates to sub-validators for each section.
:paths required. :cloud and :security always present after load-schema.
:bash-tools optional (may be nil or plist).
Returns nil if valid, error string if invalid."
  (cl-block validate
    (let ((errors nil))
      ;; Must be a list (plist)
      (unless (listp config)
        (cl-return-from validate
          (format "Expected plist, got %s" (type-of config))))

      ;; Required: :paths
      (if (not (plist-member config :paths))
          (push "Missing required key :paths" errors)
        (when-let ((path-error (contract/scope-paths--validate (plist-get config :paths))))
          (push (format ":paths: %s" path-error) errors)))

      ;; Required: :cloud (always present after load-schema)
      (if (not (plist-member config :cloud))
          (push "Missing required key :cloud" errors)
        (when-let ((cloud-error (contract/scope-cloud--validate (plist-get config :cloud))))
          (push (format ":cloud: %s" cloud-error) errors)))

      ;; Required: :security (always present after load-schema)
      (if (not (plist-member config :security))
          (push "Missing required key :security" errors)
        (when-let ((sec-error (contract/scope-security--validate (plist-get config :security))))
          (push (format ":security: %s" sec-error) errors)))

      ;; Optional: :bash-tools (may be nil or plist)
      (when (plist-member config :bash-tools)
        (let ((bash-tools (plist-get config :bash-tools)))
          (when bash-tools
            (when-let ((bt-error (contract/scope-bash-tools--validate bash-tools)))
              (push (format ":bash-tools: %s" bt-error) errors)))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

(provide 'contract-scope-config)

;;; contract-scope-config.el ends here
