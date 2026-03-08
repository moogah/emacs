;;; config-spec.el --- Unit tests for scope configuration management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; UNIT TESTS: Configuration loading, merging, and state management
;;
;; Tests 10 isolated configuration functions:
;; - Schema loading and defaults
;; - Cloud config loading
;; - Security config loading
;; - Tool categorization
;; - Pipeline command extraction
;;
;; Test organization:
;; 1. Schema loading with defaults (3 tests)
;; 2. Cloud config loading (2 tests)
;; 3. Security config loading (2 tests)
;; 4. Tool categorization (2 tests)
;; 5. Pipeline command extraction (1 test)

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name "../.." test-dir))
       (tools-dir test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;; Also need scope-core for tool categories
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name "../.." test-dir))
       (scope-dir (expand-file-name "../scope" test-root-dir)))
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir)))

;;; Schema Loading Tests

(describe "jf/gptel-scope--load-schema"

  (it "merges provided paths with defaults"
    (let* ((schema-plist '(:paths (:read ("/workspace/**"))))
           (result (jf/gptel-scope--load-schema schema-plist))
           (paths (plist-get result :paths)))
      (expect (plist-get paths :read) :to-equal '("/workspace/**"))
      ;; Other path keys should be empty lists (from defaults)
      (expect (plist-get paths :write) :to-equal '())
      (expect (plist-get paths :deny) :to-equal '())))

  (it "uses defaults when sections missing"
    (let* ((schema-plist '(:paths nil))
           (result (jf/gptel-scope--load-schema schema-plist))
           (cloud (plist-get result :cloud))
           (security (plist-get result :security)))
      ;; Should get defaults
      (expect (plist-get cloud :auth-detection) :to-equal "warn")
      (expect (plist-get security :enforce-parse-complete) :to-be t)
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.8)))

  (it "preserves bash-tools section with normalized keys"
    (let* ((bash-tools '(:categories (:read_only (:commands ("ls" "cat")))
                         :deny ("rm")))
           (schema-plist (list :bash-tools bash-tools))
           (result (jf/gptel-scope--load-schema schema-plist))
           (result-bash-tools (plist-get result :bash-tools)))
      ;; Keys are normalized to kebab-case by normalize-keys
      (expect (plist-get (plist-get result-bash-tools :categories) :read-only)
              :to-equal '(:commands ("ls" "cat")))
      (expect (plist-get result-bash-tools :deny) :to-equal '("rm")))))

;;; Cloud Config Loading Tests

(describe "jf/gptel-scope--load-cloud-config"

  (it "loads auth-detection setting"
    (let* ((cloud-plist '(:auth-detection "deny"))
           (result (jf/gptel-scope--load-cloud-config cloud-plist)))
      (expect (plist-get result :auth-detection) :to-equal "deny")))

  (it "returns nil for nil config"
    (let ((result (jf/gptel-scope--load-cloud-config nil)))
      (expect result :to-be nil))))

;;; Security Config Loading Tests

(describe "jf/gptel-scope--load-security-config"

  (it "loads both security settings"
    (let* ((security-plist '(:enforce-parse-complete nil
                             :max-coverage-threshold 0.9))
           (result (jf/gptel-scope--load-security-config security-plist)))
      (expect (plist-get result :enforce-parse-complete) :to-be nil)
      (expect (plist-get result :max-coverage-threshold) :to-equal 0.9)))

  (it "returns nil for nil config"
    (let ((result (jf/gptel-scope--load-security-config nil)))
      (expect result :to-be nil))))

;;; Tool Categorization Tests

(describe "jf/gptel-scope--infer-validation-type"

  (it "infers path validation for read_file tool"
    (let ((validation-type (jf/gptel-scope--infer-validation-type "read_file")))
      (expect validation-type :to-equal 'path)))

  (it "infers bash validation for run_bash_command tool"
    (let ((validation-type (jf/gptel-scope--infer-validation-type "run_bash_command")))
      (expect validation-type :to-equal 'bash))))

;;; Pipeline Command Extraction Tests

(describe "jf/gptel-scope--extract-pipeline-commands"

  (it "extracts all commands from pipeline"
    (let* ((parsed-command '(:all-commands ((:command-name "ls")
                                            (:command-name "grep")
                                            (:command-name "head"))))
           (result (jf/gptel-scope--extract-pipeline-commands parsed-command)))
      (expect result :to-equal '("ls" "grep" "head"))))

  (it "returns empty list for no commands"
    (let* ((parsed-command '(:all-commands ()))
           (result (jf/gptel-scope--extract-pipeline-commands parsed-command)))
      (expect result :to-equal '())))

  (it "handles missing command-name gracefully"
    (let* ((parsed-command '(:all-commands ((:command-name "ls")
                                            (:other-field "value")
                                            (:command-name "grep"))))
           (result (jf/gptel-scope--extract-pipeline-commands parsed-command)))
      (expect result :to-equal '("ls" "grep")))))

(provide 'config-spec)

;;; config-spec.el ends here
