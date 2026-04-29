;;; add-to-scope-section-targeting-spec.el --- add-path-to-scope section targeting by denied operation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Specifies how `jf/gptel-scope--add-path-to-scope' picks which
;; `paths.*' subsection to write to, based on the DENIED-OPERATION
;; keyword passed from the validation pipeline.
;;
;; Historical context: an earlier version of add-path-to-scope chose the
;; target section from a tool-name registry (jf/gptel-scope--tool-categories).
;; That made run_bash_command always target paths.write, even when the
;; denial was a read-metadata failure on a sub-command. The fix: drive
;; the section choice from the actual denied operation in the validation
;; error, not from the tool name. These tests lock that behavior in.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
;; NOTE: scope-yaml.el was deleted as part of the YAML→drawer migration.
;; This spec is owned by migrate-expansion-tests and will be rewritten to
;; use drawer fixtures.  The require for scope-yaml is removed so the file
;; can still load; individual `it' blocks that reference the deleted YAML
;; helpers will fail loudly with `void-function' at runtime, which is the
;; expected "fail loudly" signal documented in delete-yaml-and-security-residue.
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Test Infrastructure

(defvar section--temp-dir nil)
(defvar section--scope-file nil)

(defun section--make-empty-scope-yml ()
  "Create scope.yml with empty read/write/execute/modify paths."
  (let ((yaml "paths:
  read: []
  write: []
  execute: []
  modify: []
  deny: []
cloud:
  auth_detection: \"warn\"
security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
    (with-temp-file section--scope-file
      (insert yaml))))

(defun section--parse-scope-yml ()
  "Parse the test scope.yml into a normalized plist."
  (jf/gptel-scope-yaml--parse-file section--scope-file))

(defun section--paths-for (section)
  "Return the list under `paths.SECTION' in the current test scope file."
  (plist-get (plist-get (section--parse-scope-yml) :paths) section))

(defun section--matches-p (section substring)
  "Return non-nil if any path in SECTION contains SUBSTRING."
  (cl-some (lambda (p) (string-match-p substring p))
           (section--paths-for section)))

;;; Tests

(describe "add-path-to-scope section targeting"

  (before-each
    (setq section--temp-dir (make-temp-file "section-" t))
    (setq section--scope-file (expand-file-name "scope.yml" section--temp-dir))
    (section--make-empty-scope-yml))

  (after-each
    (when (and section--temp-dir (file-exists-p section--temp-dir))
      (delete-directory section--temp-dir t)))

  (describe "without denied-operation"

    (it "defaults to paths.read (the safest default)"
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/outside/file.txt" "run_bash_command")
      (expect (section--matches-p :read "file\\.txt") :to-be-truthy)
      (expect (section--matches-p :write "file\\.txt") :not :to-be-truthy)))

  (describe "with :read-family operations"

    (it ":read targets paths.read"
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/brew" "run_bash_command" :read)
      (expect (section--matches-p :read "brew") :to-be-truthy)
      (expect (section--matches-p :write "brew") :not :to-be-truthy))

    (it ":read-metadata targets paths.read"
      ;; The original bug: bash tools doing metadata lookups (which brew)
      ;; were denied for a :read-metadata op but the path landed in write.
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/brew" "run_bash_command" :read-metadata)
      (expect (section--matches-p :read "brew") :to-be-truthy)
      (expect (section--matches-p :write "brew") :not :to-be-truthy))

    (it ":read-directory targets paths.read"
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/usr/local" "run_bash_command" :read-directory)
      (expect (section--matches-p :read "usr/local") :to-be-truthy))

    (it ":match-pattern targets paths.read"
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/workspace/src" "read_file_in_scope" :match-pattern)
      (expect (section--matches-p :read "workspace/src") :to-be-truthy)))

  (describe "with :write-family operations"

    (it ":write targets paths.write"
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/tmp/output.txt" "run_bash_command" :write)
      (expect (section--matches-p :write "output") :to-be-truthy)
      (expect (section--matches-p :read "output") :not :to-be-truthy))

    (it ":create targets paths.write"
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/tmp/new.txt" "write_file_in_scope" :create)
      (expect (section--matches-p :write "new") :to-be-truthy))

    (it ":delete targets paths.write"
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/tmp/old.txt" "run_bash_command" :delete)
      (expect (section--matches-p :write "old") :to-be-truthy))

    (it ":modify targets paths.write"
      ;; :modify is intentionally folded into paths.write — in-place edits
      ;; are a form of writing, not a separate permission class at scope time.
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/tmp/config.el" "edit_file_in_scope" :modify)
      (expect (section--matches-p :write "config") :to-be-truthy)))

  (describe "with :execute"

    (it ":execute targets paths.execute"
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/workspace/scripts/run.sh" "run_bash_command" :execute)
      (expect (section--matches-p :execute "run\\.sh") :to-be-truthy)
      (expect (section--matches-p :read "run\\.sh") :not :to-be-truthy)
      (expect (section--matches-p :write "run\\.sh") :not :to-be-truthy))))

(provide 'add-to-scope-section-targeting-spec)

;;; add-to-scope-section-targeting-spec.el ends here
