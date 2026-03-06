;;; test-input-validation.el --- Input validation tests for jf/bash-parse -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Tests for input validation in jf/bash-parse entry point.
;; Ensures that invalid inputs are properly rejected with clear error messages.
;;
;; Key requirements:
;; - Type validation: command-string must be a string
;; - Emptiness validation: command-string must not be empty
;; - Error signals: wrong-type-argument for invalid types
;; - Error plists: :success nil with :error message for empty strings

;;; Code:

(require 'test-helper (expand-file-name "test-helper.el"
                                        (file-name-directory load-file-name)))

;;; Type Validation Tests

(ert-deftest test-parse-rejects-nil-input ()
  "Verify jf/bash-parse signals wrong-type-argument for nil input."
  (should-error (jf/bash-parse nil)
                :type 'wrong-type-argument))

(ert-deftest test-parse-rejects-number-input ()
  "Verify jf/bash-parse signals wrong-type-argument for number input."
  (should-error (jf/bash-parse 123)
                :type 'wrong-type-argument))

(ert-deftest test-parse-rejects-list-input ()
  "Verify jf/bash-parse signals wrong-type-argument for list input."
  (should-error (jf/bash-parse '("ls" "-la"))
                :type 'wrong-type-argument))

(ert-deftest test-parse-rejects-symbol-input ()
  "Verify jf/bash-parse signals wrong-type-argument for symbol input."
  (should-error (jf/bash-parse 'command)
                :type 'wrong-type-argument))

(ert-deftest test-parse-rejects-vector-input ()
  "Verify jf/bash-parse signals wrong-type-argument for vector input."
  (should-error (jf/bash-parse ["ls" "-la"])
                :type 'wrong-type-argument))

;;; Emptiness Validation Tests

(ert-deftest test-parse-rejects-empty-string ()
  "Verify jf/bash-parse returns error plist for empty string input."
  (let ((result (jf/bash-parse "")))
    (should (eq (plist-get result :success) nil))
    (should (equal (plist-get result :error) "Empty command string"))
    (should (eq (plist-get result :type) :empty))))

;;; Valid Input Tests (sanity checks)

(ert-deftest test-parse-accepts-valid-command ()
  "Verify jf/bash-parse accepts valid command string.
This is a sanity check to ensure validation doesn't break normal usage."
  (let ((result (jf/bash-parse "ls -la")))
    (should (eq (plist-get result :success) t))
    (should (equal (plist-get result :command-name) "ls"))
    (should-not (plist-get result :error))))

(ert-deftest test-parse-accepts-whitespace-padded-command ()
  "Verify jf/bash-parse accepts command with leading/trailing whitespace.
Whitespace-only strings should be rejected by tree-sitter, but padded
commands should parse successfully."
  (let ((result (jf/bash-parse "  cat file.txt  ")))
    (should (eq (plist-get result :success) t))
    (should (equal (plist-get result :command-name) "cat"))))

(provide 'test-input-validation)
;;; test-input-validation.el ends here
