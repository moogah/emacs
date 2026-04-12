;;; expansion-roundtrip-spec.el --- Round-trip tests: scope expansion writes valid configs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Round-trip integration tests for scope expansion.
;;
;; ISSUE: add-bash-to-scope writes invalid configurations:
;;   - For non-path bash denials (command_denied), it writes to
;;     bash_tools.categories (which load-schema rejects)
;;   - The resulting scope.yml fails contract validation
;;
;; These tests verify the invariant:
;;   expand scope → write scope.yml → reload scope.yml → contract valid
;;
;; Every expansion action (add-path, add-bash, add-pattern) must produce
;; a scope.yml that satisfies contract/scope-config--validate after
;; being reloaded through jf/gptel-scope-yaml--load-schema.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (config-dir (expand-file-name ".." gptel-dir))
       (contracts-dir (expand-file-name "test/contracts/" config-dir)))
  ;; Contract infrastructure
  (add-to-list 'load-path contracts-dir)
  (require 'contract-core)
  (require 'contract-scope-config)
  (contract--register-buttercup-matcher)
  ;; Scope helpers
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  ;; Production scope modules
  (require 'jf-gptel-scope-validation (expand-file-name "scope/scope-validation.el" gptel-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope/scope-tool-wrapper.el" gptel-dir))
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  (require 'jf-gptel-scope-expansion
           (expand-file-name "scope/scope-expansion.el" gptel-dir)))


;;; Test infrastructure

(defvar roundtrip--temp-dir nil
  "Temporary directory for test scope.yml files.")

(defvar roundtrip--scope-file nil
  "Path to test scope.yml.")

(defun roundtrip--make-minimal-scope-yml ()
  "Create a minimal valid scope.yml for round-trip testing.
Returns scope-file path."
  (with-temp-file roundtrip--scope-file
    (insert "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny:
    - \"**/.git/**\"
bash_tools:
  deny:
    - \"rm\"
    - \"sudo\"
cloud:
  auth_detection: \"warn\"
security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
  roundtrip--scope-file)

(defun roundtrip--make-empty-scope-yml ()
  "Create scope.yml with empty path lists for denial testing."
  (with-temp-file roundtrip--scope-file
    (insert "paths:
  read: []
  write: []
  execute: []
  modify: []
  deny:
    - \"**/.git/**\"
bash_tools:
  deny:
    - \"sudo\"
cloud:
  auth_detection: \"deny\"
security:
  enforce_parse_complete: true
  max_coverage_threshold: 1
"))
  roundtrip--scope-file)

(defun roundtrip--reload-and-validate ()
  "Reload scope.yml and validate against contract.
Returns the loaded config on success, signals error on contract violation."
  (let ((config (helpers-spec-load-scope-config roundtrip--scope-file)))
    (let ((error (contract/scope-config--validate config)))
      (when error
        (error "Contract violation after round-trip: %s\nScope.yml contents:\n%s"
               error
               (with-temp-buffer
                 (insert-file-contents roundtrip--scope-file)
                 (buffer-string)))))
    config))

(defun roundtrip--read-scope-contents ()
  "Read scope.yml contents as string."
  (with-temp-buffer
    (insert-file-contents roundtrip--scope-file)
    (buffer-string)))


;;; Round-trip: add-path-to-scope

(describe "expansion round-trip: add-path-to-scope"

  (before-each
    (setq roundtrip--temp-dir (make-temp-file "roundtrip-scope-" t))
    (setq roundtrip--scope-file (expand-file-name "scope.yml" roundtrip--temp-dir))
    (roundtrip--make-minimal-scope-yml))

  (after-each
    (when (and roundtrip--temp-dir (file-exists-p roundtrip--temp-dir))
      (delete-directory roundtrip--temp-dir t)))

  (it "adding a read path produces valid config"
    (jf/gptel-scope--add-path-to-scope
     roundtrip--scope-file "/tmp/data.txt" "read_file")
    (let ((config (roundtrip--reload-and-validate)))
      (expect (member "/tmp/data.txt" (plist-get (plist-get config :paths) :read))
              :to-be-truthy)))

  (it "adding a write path produces valid config"
    (jf/gptel-scope--add-path-to-scope
     roundtrip--scope-file "/tmp/output.txt" "write_file_in_scope" :write)
    (let ((config (roundtrip--reload-and-validate)))
      (expect (member "/tmp/output.txt" (plist-get (plist-get config :paths) :write))
              :to-be-truthy)))

  (it "adding a directory path normalizes to glob pattern"
    (jf/gptel-scope--add-path-to-scope
     roundtrip--scope-file "/tmp/data/" "read_file")
    (let ((config (roundtrip--reload-and-validate)))
      (expect (member "/tmp/data/**" (plist-get (plist-get config :paths) :read))
              :to-be-truthy)))

  (it "adding duplicate path is idempotent"
    (jf/gptel-scope--add-path-to-scope
     roundtrip--scope-file "/workspace/new.txt" "read_file")
    (jf/gptel-scope--add-path-to-scope
     roundtrip--scope-file "/workspace/new.txt" "read_file")
    (let* ((config (roundtrip--reload-and-validate))
           (read-paths (plist-get (plist-get config :paths) :read))
           (count (cl-count "/workspace/new.txt" read-paths :test #'equal)))
      (expect count :to-equal 1)))

  (it "adding path with explicit denied-operation targets correct section"
    ;; When run_bash_command is denied for :read-metadata, the path should
    ;; go to paths.read (not paths.write, which is the tool's category)
    (jf/gptel-scope--add-path-to-scope
     roundtrip--scope-file "/brew" "run_bash_command" :read-metadata)
    (let* ((config (roundtrip--reload-and-validate))
           (paths (plist-get config :paths)))
      (expect (member "/brew" (plist-get paths :read)) :to-be-truthy)
      (expect (member "/brew" (plist-get paths :write)) :to-be nil))))


;;; Round-trip: add-bash-to-scope

(describe "expansion round-trip: add-bash-to-scope"

  (before-each
    (setq roundtrip--temp-dir (make-temp-file "roundtrip-scope-" t))
    (setq roundtrip--scope-file (expand-file-name "scope.yml" roundtrip--temp-dir))
    (roundtrip--make-empty-scope-yml))

  (after-each
    (when (and roundtrip--temp-dir (file-exists-p roundtrip--temp-dir))
      (delete-directory roundtrip--temp-dir t)))

  (it "adding a file path resource produces valid config"
    ;; When bash validation fails on path_out_of_scope, resource is a file path
    (jf/gptel-scope--add-bash-to-scope
     roundtrip--scope-file "/brew" "run_bash_command" :read-metadata)
    (let ((config (roundtrip--reload-and-validate)))
      ;; Path should be in read (since denied operation is read-metadata)
      (let ((read-paths (plist-get (plist-get config :paths) :read)))
        (expect (member "/brew" read-paths) :to-be-truthy))))

  (it "adding a file path with :write operation targets paths.write"
    (jf/gptel-scope--add-bash-to-scope
     roundtrip--scope-file "/tmp/output.log" "run_bash_command" :write)
    (let* ((config (roundtrip--reload-and-validate))
           (paths (plist-get config :paths)))
      (expect (member "/tmp/output.log" (plist-get paths :write)) :to-be-truthy)))

  (it "adding a command name resource produces valid config"
    ;; When bash validation fails on command_denied, resource is a command name
    ;; This is the case that currently writes to categories (BUG)
    (jf/gptel-scope--add-bash-to-scope
     roundtrip--scope-file "brew" "run_bash_command")
    ;; The critical assertion: reloading must not error
    (let ((config (roundtrip--reload-and-validate)))
      ;; Config should be valid (no categories section)
      (let ((bash-tools (plist-get config :bash-tools)))
        (expect (plist-member bash-tools :categories) :to-be nil))))

  (it "scope.yml should not contain categories after adding command"
    (jf/gptel-scope--add-bash-to-scope
     roundtrip--scope-file "tree" "run_bash_command")
    (let ((contents (roundtrip--read-scope-contents)))
      (expect contents :not :to-match "categories")))

  (it "adding absolute path resource routes to path expansion"
    (jf/gptel-scope--add-bash-to-scope
     roundtrip--scope-file "/usr/local/bin/brew" "run_bash_command" :execute)
    (let* ((config (roundtrip--reload-and-validate))
           (paths (plist-get config :paths)))
      ;; Should go to paths.execute since denied-operation is :execute
      (expect (member "/usr/local/bin/brew" (plist-get paths :execute))
              :to-be-truthy)))

  (it "adding directory path normalizes to glob and produces valid config"
    (jf/gptel-scope--add-bash-to-scope
     roundtrip--scope-file "/tmp/scripts/" "run_bash_command" :read)
    (let* ((config (roundtrip--reload-and-validate))
           (paths (plist-get config :paths)))
      (expect (member "/tmp/scripts/**" (plist-get paths :read))
              :to-be-truthy))))


;;; Round-trip: preservation of existing config

(describe "expansion round-trip: config preservation"

  (before-each
    (setq roundtrip--temp-dir (make-temp-file "roundtrip-scope-" t))
    (setq roundtrip--scope-file (expand-file-name "scope.yml" roundtrip--temp-dir))
    (roundtrip--make-minimal-scope-yml))

  (after-each
    (when (and roundtrip--temp-dir (file-exists-p roundtrip--temp-dir))
      (delete-directory roundtrip--temp-dir t)))

  (it "adding a path preserves existing deny patterns"
    (let* ((before (helpers-spec-load-scope-config roundtrip--scope-file))
           (deny-before (plist-get (plist-get before :paths) :deny)))
      (jf/gptel-scope--add-path-to-scope
       roundtrip--scope-file "/tmp/new.txt" "read_file")
      (let* ((after (roundtrip--reload-and-validate))
             (deny-after (plist-get (plist-get after :paths) :deny)))
        (expect deny-after :to-equal deny-before))))

  (it "adding a path preserves bash_tools deny list"
    (let* ((before (helpers-spec-load-scope-config roundtrip--scope-file))
           (deny-before (plist-get (plist-get before :bash-tools) :deny)))
      (jf/gptel-scope--add-path-to-scope
       roundtrip--scope-file "/tmp/new.txt" "read_file")
      (let* ((after (roundtrip--reload-and-validate))
             (deny-after (plist-get (plist-get after :bash-tools) :deny)))
        (expect deny-after :to-equal deny-before))))

  (it "adding a path preserves cloud configuration"
    (let* ((before (helpers-spec-load-scope-config roundtrip--scope-file))
           (cloud-before (plist-get before :cloud)))
      (jf/gptel-scope--add-path-to-scope
       roundtrip--scope-file "/tmp/new.txt" "read_file")
      (let* ((after (roundtrip--reload-and-validate))
             (cloud-after (plist-get after :cloud)))
        (expect (plist-get cloud-after :auth-detection)
                :to-equal (plist-get cloud-before :auth-detection)))))

  (it "adding a path preserves security configuration"
    (let* ((before (helpers-spec-load-scope-config roundtrip--scope-file))
           (security-before (plist-get before :security)))
      (jf/gptel-scope--add-path-to-scope
       roundtrip--scope-file "/tmp/new.txt" "read_file")
      (let* ((after (roundtrip--reload-and-validate))
             (security-after (plist-get after :security)))
        (expect (plist-get security-after :enforce-parse-complete)
                :to-equal (plist-get security-before :enforce-parse-complete))
        (expect (plist-get security-after :max-coverage-threshold)
                :to-equal (plist-get security-before :max-coverage-threshold))))))


(provide 'expansion-roundtrip-spec)

;;; expansion-roundtrip-spec.el ends here
