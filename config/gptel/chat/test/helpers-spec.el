;;; helpers-spec.el --- Shared fixtures for gptel-chat-mode buttercup tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Lightweight shared fixtures for the gptel-chat-mode test suite.
;;
;; This file is intentionally minimal.  It provides only what is
;; needed across sibling spec files so the test tree does not grow an
;; accidental test framework of its own.  Specifically:
;;
;;   `gptel-chat-test--with-buffer'   — run BODY in a fresh temp
;;                                      buffer populated with CONTENT.
;;
;; The helper is load-safe (no side effects on load beyond `provide')
;; so sibling spec files may `require' it unconditionally.

;;; Code:

(require 'cl-lib)

(defmacro gptel-chat-test--with-buffer (content &rest body)
  "Create a fresh temp buffer with CONTENT and evaluate BODY inside it.

The buffer is plain (no major-mode activation), which is deliberate:
parser tests exercise the block-level state machine directly and do
not depend on org-mode being active.  Mode-activation tests belong in
a separate file owned by the `mode-definition' task.

CONTENT is a string inserted verbatim; it is the caller's
responsibility to supply well-formed text for the scenario under
test."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(provide 'gptel-chat-test-helpers)

;;; helpers-spec.el ends here
