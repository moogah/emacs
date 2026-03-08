;;; test-helper.el --- Test setup for bash-parser tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Centralized test helper for all bash-parser tests.
;; Provides common setup, dependencies, and utilities.
;;
;; Usage in test files:
;;   (require 'test-helper (expand-file-name "test-helper.el"
;;                                            (file-name-directory load-file-name)))

;;; Code:

(require 'ert)

;; Load bash-parser modules
(let ((bash-parser-dir (expand-file-name ".."
                                         (file-name-directory (or load-file-name
                                                                 buffer-file-name)))))
  (require 'bash-parser (expand-file-name "bash-parser.el" bash-parser-dir)))

;; Common test utilities can be added here in the future
;; For example:
;; - Test data fixtures
;; - Assertion helpers
;; - Mock functions

(provide 'test-helper)
;;; test-helper.el ends here
