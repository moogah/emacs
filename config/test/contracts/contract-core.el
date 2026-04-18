;;; contract-core.el --- Cross-boundary contract validation primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: testing, contracts

;;; Commentary:

;; Provides validation primitives for cross-boundary data shape contracts.
;; Contracts are the single source of truth for what a producer must output
;; and what a consumer can assume.
;;
;; Two validation primitives:
;; - `contract--validate-plist` - validates plist shape (required/optional keys + type predicates)
;; - `contract--validate-alist` - validates alist shape (catches plist-get-on-alist bugs)
;;
;; One Buttercup matcher:
;; - `:to-satisfy-contract` - calls a contract validator and reports errors
;;
;; Validators return nil if valid, descriptive error string if invalid.

;;; Code:

(require 'cl-lib)

(defun contract--validate-plist (data required &optional optional)
  "Validate DATA as a plist against REQUIRED and OPTIONAL key specs.
REQUIRED is a list of (KEY PREDICATE) pairs that must be present and satisfy predicate.
OPTIONAL is a list of (KEY PREDICATE) pairs that may be present.

Returns nil if valid, descriptive error string if invalid.

Example:
  (contract--validate-plist result
    \\='((:success booleanp) (:all-commands listp))
    \\='((:error stringp)))"
  (cl-block validate
    (let ((errors nil))
      ;; Check it's a plist (list)
      (unless (listp data)
        (cl-return-from validate
          (format "Expected plist, got %s" (type-of data))))

      ;; Check required keys
      (dolist (req required)
        (let ((key (car req))
              (pred (cadr req)))
          (if (not (plist-member data key))
              (push (format "Missing required key %S" key) errors)
            (let ((value (plist-get data key)))
              (unless (funcall pred value)
                (push (format "Key %S: value %S does not satisfy %S"
                              key value pred)
                      errors))))))

      ;; Check optional keys (only validate if present)
      (dolist (opt optional)
        (let ((key (car opt))
              (pred (cadr opt)))
          (when (plist-member data key)
            (let ((value (plist-get data key)))
              (when (and value (not (funcall pred value)))
                (push (format "Optional key %S: value %S does not satisfy %S"
                              key value pred)
                      errors))))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

(defun contract--validate-alist (data allowed-keys &optional value-predicate)
  "Validate DATA as an alist.
ALLOWED-KEYS is a list of allowed car values (keywords or symbols).
VALUE-PREDICATE is a predicate that each cdr must satisfy (default: always true).

Returns nil if valid, descriptive error string if invalid.

This specifically catches the plist-get-on-alist class of bug:
if DATA is accessed with `plist-get' instead of `alist-get', it silently returns nil.

Example:
  (contract--validate-alist domains
    \\='(:filesystem :authentication)
    #\\='listp)"
  (cl-block validate
    (let ((value-pred (or value-predicate #'identity))
          (errors nil))

      ;; Check it's a list
      (unless (listp data)
        (cl-return-from validate
          (format "Expected alist, got %s" (type-of data))))

      ;; Check it's actually an alist (list of cons cells), not a plist
      (dolist (entry data)
        (unless (consp entry)
          (push (format "Alist entry is not a cons cell: %S (is this a plist instead of alist?)"
                        entry)
                errors)))

      ;; Check keys are allowed
      (when (and allowed-keys (not errors))
        (dolist (entry data)
          (when (consp entry)
            (let ((key (car entry)))
              (unless (memq key allowed-keys)
                (push (format "Unknown alist key %S (allowed: %S)" key allowed-keys)
                      errors))))))

      ;; Check value predicate
      (when (and value-pred (not errors))
        (dolist (entry data)
          (when (consp entry)
            (let ((value (cdr entry)))
              (when (and value (not (funcall value-pred value)))
                (push (format "Alist key %S: value does not satisfy %S"
                              (car entry) value-pred)
                      errors))))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

(defun contract--validate-plist-in-list (data-list item-validator)
  "Validate that DATA-LIST is a list where each element satisfies ITEM-VALIDATOR.
ITEM-VALIDATOR is a function that returns nil if valid, error string if invalid.
Returns nil if valid, descriptive error string if invalid."
  (cl-block validate
    (unless (listp data-list)
      (cl-return-from validate
        (format "Expected list, got %s" (type-of data-list))))
    (let ((errors nil)
          (idx 0))
      (dolist (item data-list)
        (when-let ((error (funcall item-validator item)))
          (push (format "Item %d: %s" idx error) errors))
        (cl-incf idx))
      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

;; Buttercup matcher - register when buttercup is available
(defun contract--register-buttercup-matcher ()
  "Register :to-satisfy-contract Buttercup matcher."
  (buttercup-define-matcher :to-satisfy-contract (result validator)
    "Match that RESULT satisfies contract VALIDATOR.
VALIDATOR is a function that returns nil if valid, error string if invalid."
    (let* ((actual-result (funcall result))
           (actual-validator (funcall validator))
           (error (funcall actual-validator actual-result)))
      (if (null error)
          t
        (cons nil (format "Contract violation: %s\nData: %S" error actual-result))))))

(when (featurep 'buttercup)
  (contract--register-buttercup-matcher))

(provide 'contract-core)

;;; contract-core.el ends here
