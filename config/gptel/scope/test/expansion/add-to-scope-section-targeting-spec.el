;;; add-to-scope-section-targeting-spec.el --- RED: add-to-scope should target section by denied operation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; RED PHASE UNIT TESTS for add-path-to-scope section targeting.
;;
;; BUG: add-path-to-scope determines the target section (paths.read vs
;; paths.write) by looking up the TOOL's category operation:
;;   (let* ((category (cdr (assoc tool jf/gptel-scope--tool-categories)))
;;          (operation (plist-get category :operation))
;;          (target-section (if (eq operation 'read) :read :write)))
;;
;; For run_bash_command, the tool category says :operation write (because bash
;; commands CAN write). But when the denial was for a :read-metadata operation
;; on "/brew", the path should go to paths.read, not paths.write.
;;
;; The fix: add-path-to-scope (or its caller) needs the DENIED OPERATION from
;; the validation pipeline, not just the tool name. This could come through
;; violation-info :operation.
;;
;; Tests assert CORRECT behavior and fail against current implementation.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope-tool-wrapper.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Test Infrastructure

(defvar section--temp-dir nil)
(defvar section--scope-file nil)

(defun section--make-empty-scope-yml ()
  "Create scope.yml with empty read/write paths."
  (let ((yaml "paths:
  read:
    []
  write:
    []
  execute:
    []
  modify:
    []
  deny:
    []
bash_tools:
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
  (jf/gptel-scope--normalize-plist-keys
   (jf/gptel-scope-yaml--parse-string
    (with-temp-buffer
      (insert-file-contents section--scope-file)
      (buffer-string)))))


;;; Tests

(describe "add-path-to-scope section targeting"

  (before-each
    (setq section--temp-dir (make-temp-file "section-" t))
    (setq section--scope-file (expand-file-name "scope.yml" section--temp-dir))
    (section--make-empty-scope-yml))

  (after-each
    (when (and section--temp-dir (file-exists-p section--temp-dir))
      (delete-directory section--temp-dir t)))

  (describe "run_bash_command tool adds to wrong section"

    (it "run_bash_command is categorized as :operation write"
      ;; Characterization: this is a fact about the tool categories
      (let ((category (cdr (assoc "run_bash_command" jf/gptel-scope--tool-categories))))
        (expect (plist-get category :operation) :to-equal 'write)))

    (it "add-path-to-scope puts run_bash_command paths in :write (even for read denials)"
      ;; Characterization of CURRENT behavior — this test PASSES
      ;; It documents the bug: all bash tool paths go to write
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/brew" "run_bash_command")

      (let* ((scope (section--parse-scope-yml))
             (paths (plist-get scope :paths))
             (write-paths (plist-get paths :write)))
        (expect (cl-some (lambda (p) (string-match-p "brew" p)) write-paths)
                :to-be-truthy))))

  (describe "correct behavior: section should match denied operation"

    (it "read-denied path should go to paths.read, not paths.write"
      ;; When "which brew" is denied because /brew is not in paths.read,
      ;; "Add to scope" should expand paths.read, not paths.write
      ;; Pass denied-operation :read-metadata to target the correct section
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/brew" "run_bash_command" :read-metadata)

      (let* ((scope (section--parse-scope-yml))
             (paths (plist-get scope :paths))
             (read-paths (plist-get paths :read))
             (write-paths (plist-get paths :write)))
        ;; RED: Currently /brew goes to paths.write
        ;; CORRECT: Should go to paths.read
        (expect (cl-some (lambda (p) (string-match-p "brew" p)) read-paths)
                :to-be-truthy)
        (expect (cl-some (lambda (p) (string-match-p "brew" p)) write-paths)
                :to-be nil)))

    (it "write-denied path should go to paths.write"
      ;; When a bash command writes to an out-of-scope path,
      ;; "Add to scope" should expand paths.write
      ;; NOTE: This currently works by accident (run_bash_command is write)
      ;; but should work by design (the denied operation was :write)
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/tmp/output.txt" "run_bash_command")

      (let* ((scope (section--parse-scope-yml))
             (paths (plist-get scope :paths))
             (write-paths (plist-get paths :write)))
        ;; This happens to pass because run_bash_command → write
        ;; But the correct reason should be: denied operation was :write
        (expect (cl-some (lambda (p) (string-match-p "output" p)) write-paths)
                :to-be-truthy))))

  (describe "read_file tool (contrast case)"

    (it "read_file paths correctly go to paths.read"
      ;; read_file has :operation read, so add-path-to-scope gets this right
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/outside/file.txt" "read_file")

      (let* ((scope (section--parse-scope-yml))
             (paths (plist-get scope :paths))
             (read-paths (plist-get paths :read))
             (write-paths (plist-get paths :write)))
        (expect (cl-some (lambda (p) (string-match-p "file\\.txt" p)) read-paths)
                :to-be-truthy)
        (expect (cl-some (lambda (p) (string-match-p "file\\.txt" p)) write-paths)
                :to-be nil)))

    (it "write_file_in_scope paths correctly go to paths.write"
      (jf/gptel-scope--add-path-to-scope
       section--scope-file "/outside/file.txt" "write_file_in_scope")

      (let* ((scope (section--parse-scope-yml))
             (paths (plist-get scope :paths))
             (write-paths (plist-get paths :write)))
        (expect (cl-some (lambda (p) (string-match-p "file\\.txt" p)) write-paths)
                :to-be-truthy)))))

(provide 'add-to-scope-section-targeting-spec)

;;; add-to-scope-section-targeting-spec.el ends here
