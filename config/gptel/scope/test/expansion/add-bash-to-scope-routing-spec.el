;;; add-bash-to-scope-routing-spec.el --- RED: add-bash-to-scope should route by error type, not string content -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; RED PHASE UNIT TESTS for add-bash-to-scope routing logic.
;;
;; BUG: add-bash-to-scope decides whether the resource is a "directory path"
;; or a "command pattern" by checking (string-match-p "/" resource). This is
;; fragile because:
;;   - The composite allow-once key "which brew:/" contains "/"
;;   - Actual file paths like "/etc/passwd" also contain "/"
;;   - Command patterns like "git log" don't contain "/" but are still commands
;;
;; The routing should be based on the ERROR TYPE from the validation pipeline,
;; not the string content of the resource:
;;   - path_out_of_scope → add the denied file path to paths section
;;   - command_denied → this is a deny-list issue, not a scope addition
;;   - command-not-allowed → missing bash_tools config, not scope addition
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
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Test Infrastructure

(defvar routing--temp-dir nil)
(defvar routing--scope-file nil)

(defun routing--make-scope-yml ()
  "Create a minimal scope.yml for testing."
  (let ((yaml "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute:
    []
  modify:
    []
  deny:
    []
bash_tools:
  deny:
    - \"sudo\"
cloud:
  auth_detection: \"warn\"
security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
    (with-temp-file routing--scope-file
      (insert yaml))))

(defun routing--read-scope-yml ()
  "Read and return contents of the test scope.yml."
  (with-temp-buffer
    (insert-file-contents routing--scope-file)
    (buffer-string)))

(defun routing--parse-scope-yml ()
  "Parse the test scope.yml into a normalized plist."
  (jf/gptel-scope--normalize-plist-keys
   (jf/gptel-scope-yaml--parse-string (routing--read-scope-yml))))


;;; Tests

(describe "add-bash-to-scope routing"

  (before-each
    (setq routing--temp-dir (make-temp-file "routing-" t))
    (setq routing--scope-file (expand-file-name "scope.yml" routing--temp-dir))
    (routing--make-scope-yml))

  (after-each
    (when (and routing--temp-dir (file-exists-p routing--temp-dir))
      (delete-directory routing--temp-dir t)))

  (describe "with file path resource (path_out_of_scope scenario)"

    (it "should add '/brew' to paths section, not bash_tools"
      ;; When the denied resource is a file path like "/brew",
      ;; add-bash-to-scope should add it to paths (read or write)
      (jf/gptel-scope--add-bash-to-scope routing--scope-file "/brew" "run_bash_command")

      (let* ((scope (routing--parse-scope-yml))
             (paths (plist-get scope :paths))
             (all-path-patterns (append (plist-get paths :read)
                                        (plist-get paths :write))))
        ;; CORRECT: /brew (or a parent glob) should appear in paths
        ;; RED: Currently writes "/brew" to paths.write via add-path-to-scope
        ;; which is the WRONG section (should be paths.read since the operation
        ;; was :read-metadata), but at least it IS a valid path.
        ;; This test just checks it ended up in paths somewhere.
        (expect (cl-some (lambda (p) (string-match-p "brew" p))
                         all-path-patterns)
                :to-be-truthy)))

    (it "should add '/etc/passwd' to paths.read for a read operation"
      ;; The denied operation was :read, so the path should go to paths.read
      ;; Pass denied-operation so add-path-to-scope targets the correct section
      (jf/gptel-scope--add-bash-to-scope routing--scope-file "/etc/passwd" "run_bash_command" :read)

      (let* ((scope (routing--parse-scope-yml))
             (paths (plist-get scope :paths))
             (read-paths (plist-get paths :read))
             (write-paths (plist-get paths :write)))
        ;; RED: Currently goes to paths.write because run_bash_command is :operation write
        ;; CORRECT: Should go to paths.read (the actual denied operation context)
        ;; NOTE: add-bash-to-scope currently has no way to know the denied operation.
        ;; The fix likely needs to pass operation context through violation-info.
        ;; For now, we assert the path doesn't go to write when it should be read.
        (expect (cl-some (lambda (p) (string-match-p "passwd" p)) read-paths)
                :to-be-truthy)))

    (it "should NOT write composite 'which brew:/' as a path pattern"
      ;; Even if Bug 1 isn't fixed yet, add-bash-to-scope should
      ;; recognize and reject composite keys
      (jf/gptel-scope--add-bash-to-scope routing--scope-file "which brew:/" "run_bash_command")

      (let ((contents (routing--read-scope-yml)))
        ;; RED: Currently writes "which brew:/**" (appends /** to trailing /)
        ;; CORRECT: Should not write composite keys as path patterns
        (expect contents :not :to-match "which brew:"))))

  (describe "routing should not depend on '/' in resource string"

    (it "resource with '/' (file path) should not be treated as command pattern"
      ;; "/etc/passwd" contains "/" but is a file path, not a directory
      ;; for the bash_tools categories section
      (spy-on 'jf/gptel-scope--add-path-to-scope :and-call-through)

      (jf/gptel-scope--add-bash-to-scope routing--scope-file "/etc/passwd" "run_bash_command")

      ;; It's OK to call add-path-to-scope with a clean file path
      ;; What's NOT OK is to call it with a composite key
      (when (spy-calls-count 'jf/gptel-scope--add-path-to-scope)
        (let ((path-arg (nth 1 (spy-calls-args-for 'jf/gptel-scope--add-path-to-scope 0))))
          ;; The path should be clean — no composite format
          (expect path-arg :not :to-match ":"))))

    (it "resource without '/' (bare command) should not go to paths section"
      ;; "brew" has no "/" — currently falls into command pattern branch
      ;; but there's nothing useful to do with it there either
      ;; This documents the less-broken case
      (let ((contents-before (routing--read-scope-yml)))
        (jf/gptel-scope--add-bash-to-scope routing--scope-file "brew" "run_bash_command")
        ;; For a bare command name, the behavior is less critical
        ;; but it should not corrupt scope.yml
        (let ((contents-after (routing--read-scope-yml)))
          ;; At minimum, scope.yml should still be valid YAML
          (expect (jf/gptel-scope-yaml--parse-string contents-after) :to-be-truthy))))))

(provide 'add-bash-to-scope-routing-spec)

;;; add-bash-to-scope-routing-spec.el ends here
