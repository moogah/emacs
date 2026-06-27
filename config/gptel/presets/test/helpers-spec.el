;;; helpers-spec.el --- Test helpers for the presets sub-module -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Shared test infrastructure for `config/gptel/presets/' specs.
;;
;; Provides `presets-test-read-golden', the reader behind the golden-snapshot
;; assertions: render output is compared `:to-equal' the verbatim contents of a
;; committed `golden/<name>.<backend>.txt' file.  Golden files are stored without
;; a trailing newline so they match the renderer output exactly; this reader does
;; no normalization.

;;; Code:

(require 'buttercup)

(defconst presets-test-golden-dir
  (expand-file-name
   "golden"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding golden-snapshot files for presets specs.")

(defun presets-test-read-golden (name)
  "Return the verbatim contents of golden file NAME.
NAME is the basename under `presets-test-golden-dir', e.g.
\"core-sample.claude.txt\".  The file is read with no decoding tricks
and no whitespace normalization so the result can be compared
`:to-equal' against renderer output byte-for-byte."
  (let ((path (expand-file-name name presets-test-golden-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;; NOTE: provide a sub-module-unique feature symbol.  `config/gptel/scope/test/
;; helpers-spec.el' already provides the generic `helpers-spec' feature and is
;; what the scope specs `(require 'helpers-spec ...)' resolve to.  Because the
;; full buttercup batch loads `presets/test/' before `scope/test/' (recursive
;; alphabetical order), providing the generic name here would satisfy that
;; feature first and make the scope specs' `require' a no-op, leaving
;; `helpers-spec-make-scope-config' undefined.  Use a distinct symbol instead.
(provide 'presets-helpers-spec)
;;; helpers-spec.el ends here
