;;; contract-test-helpers.el --- Contract validation for bash-parser producer tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared helpers that wire bash-parser producer tests to the contract
;; validation system.  Loading this file makes contract validators
;; available and provides helpers that validate producer output shapes.
;;
;; Every operation returned by a command handler should satisfy the
;; appropriate contract.  These helpers enforce that automatically
;; so existing test assertions don't need to change — they just get
;; contract validation for free.

;;; Code:

(require 'cl-lib)

;; Load contract infrastructure
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       ;; commands/test/ -> commands/ -> bash-parser/ -> config/
       (config-dir (expand-file-name "../../.." test-dir))
       (contracts-dir (expand-file-name "core/contracts/" config-dir)))
  (add-to-list 'load-path contracts-dir))

(require 'contract-core)
(require 'contract-bash-parser)

;; Register Buttercup matcher if available
(when (featurep 'buttercup)
  (contract--register-buttercup-matcher))

(defun contract-test--validate-file-ops (ops context)
  "Validate each file-op in OPS against the contract.
CONTEXT is a string describing what produced the ops (for error messages).
Signals an error if any op violates the contract."
  (when ops
    (let ((idx 0))
      (dolist (op ops)
        (when-let ((error (contract/bash-file-op--validate op)))
          (error "Producer contract violation in %s, op[%d]: %s\nOp: %S"
                 context idx error op))
        (cl-incf idx)))))

(defun contract-test--validate-cloud-auth-ops (ops context)
  "Validate each cloud-auth-op in OPS against the contract.
CONTEXT is a string describing what produced the ops (for error messages).
Signals an error if any op violates the contract."
  (when ops
    (let ((idx 0))
      (dolist (op ops)
        (when-let ((error (contract/bash-cloud-auth-op--validate op)))
          (error "Producer contract violation in %s, auth-op[%d]: %s\nOp: %S"
                 context idx error op))
        (cl-incf idx)))))

(defun contract-test--validate-handler-result (result context)
  "Validate a handler RESULT plist against contracts.
Checks :operations contains valid file-ops or cloud-auth-ops based on :domain.
CONTEXT is a string for error messages.
Returns RESULT unchanged (pass-through for chaining)."
  (when result
    (let ((domain (plist-get result :domain))
          (ops (plist-get result :operations)))
      (pcase domain
        (:filesystem
         (contract-test--validate-file-ops ops context))
        (:authentication
         (contract-test--validate-cloud-auth-ops ops context))
        ;; :network ops don't have a contract yet — they use different fields
        )))
  result)

(defun contract-test--validate-domains (domains context)
  "Validate a DOMAINS alist from the orchestrator against contracts.
Checks the alist structure and validates ops within each domain.
CONTEXT is a string for error messages."
  (when domains
    ;; Validate alist structure
    (when-let ((error (contract/bash-domains--validate domains)))
      (error "Producer contract violation in %s domains: %s\nDomains: %S"
             context error domains))
    ;; Validate ops within each domain
    (when-let ((file-ops (alist-get :filesystem domains)))
      (contract-test--validate-file-ops file-ops context))
    (when-let ((auth-ops (alist-get :authentication domains)))
      (contract-test--validate-cloud-auth-ops auth-ops context))))

(provide 'contract-test-helpers)

;;; contract-test-helpers.el ends here
