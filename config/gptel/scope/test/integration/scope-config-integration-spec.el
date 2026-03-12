;;; scope-config-integration-spec.el --- Integration tests for scope config consumers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Integration tests validating that internal scope consumers
;; (validate-operation, validate-cloud-auth, load-schema) work correctly
;; with scope configs parsed from real YAML through the real pipeline.
;;
;; No mocks — every test uses jf/gptel-scope-yaml--parse-string and
;; jf/gptel-scope--load-schema with real YAML input.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       ;; test/integration/ -> test/ -> scope/ -> gptel/ -> config/
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (config-dir (expand-file-name ".." gptel-dir))
       (contracts-dir (expand-file-name "test/contracts/" config-dir)))
  ;; Contract infrastructure
  (add-to-list 'load-path contracts-dir)
  (require 'contract-core)
  (require 'contract-bash-parser)
  (require 'contract-scope-config)
  (contract--register-buttercup-matcher)
  ;; Scope validation (pulls in scope-yaml, scope-shell-tools)
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  ;; Scope expansion (provides write-yaml-plist for round-trip test)
  (require 'jf-gptel-scope-expansion
           (expand-file-name "scope/scope-expansion.el" gptel-dir))
  ;; Helpers for scope config builders
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir)))

;;; Test suites

(describe "Scope config integration"

  ;; -- Scenario 1: Parse round-trip --
  (describe "parse round-trip"
    (it "preserves paths data through parse -> serialize -> parse cycle"
      (let* ((yaml-str "paths:\n  read:\n    - \"/tmp/**\"\n    - \"/home/**\"\n  write:\n    - \"/tmp/**\"\n  execute:\n    []\n  modify:\n    []\n  deny:\n    - \"/etc/**\"\ncloud:\n  auth_detection: \"warn\"\nsecurity:\n  enforce_parse_complete: true\n  max_coverage_threshold: 0.8\n")
             (first-parse (jf/gptel-scope--load-schema
                           (jf/gptel-scope-yaml--parse-string yaml-str)))
             ;; Serialize back to YAML
             (serialized (with-temp-buffer
                           (jf/gptel-scope--write-yaml-plist first-parse)
                           (buffer-string)))
             ;; Parse again
             (second-parse (jf/gptel-scope--load-schema
                            (jf/gptel-scope-yaml--parse-string serialized)))
             (first-paths (plist-get first-parse :paths))
             (second-paths (plist-get second-parse :paths)))
        (expect (plist-get first-paths :read) :to-equal (plist-get second-paths :read))
        (expect (plist-get first-paths :write) :to-equal (plist-get second-paths :write))
        (expect (plist-get first-paths :deny) :to-equal (plist-get second-paths :deny)))))

  ;; -- Scenario 2: Schema merge with defaults --
  (describe "schema merge with defaults"
    (it "fills in all defaults for minimal YAML"
      (let* ((yaml-str "paths:\n  read:\n    - \"/tmp/**\"\n")
             (config (jf/gptel-scope--load-schema
                      (jf/gptel-scope-yaml--parse-string yaml-str))))
        ;; Contract validation passes
        (expect config :to-satisfy-contract #'contract/scope-config--validate)
        ;; Defaults filled in
        (expect (plist-get (plist-get config :paths) :write) :to-equal '())
        (expect (plist-get (plist-get config :paths) :execute) :to-equal '())
        (expect (plist-get (plist-get config :paths) :modify) :to-equal '())
        (expect (plist-get (plist-get config :paths) :deny) :to-equal '())
        (expect (plist-get (plist-get config :cloud) :auth-detection) :to-equal "warn")
        (expect (plist-get (plist-get config :security) :enforce-parse-complete) :to-be t)
        (expect (plist-get (plist-get config :security) :max-coverage-threshold) :to-equal 0.8))))

  ;; -- Scenario 3: Path validator compatibility (permissive) --
  (describe "path validator with permissive config"
    (it "allows all 11 operations with permissive paths"
      (let* ((yml-file (helpers-spec-make-scope-yml
                        (helpers-spec--scope-with-paths
                         '("/**") '("/**") '("/**") '("/**") nil)))
             (config (unwind-protect
                         (helpers-spec-load-scope-config yml-file)
                       (delete-file yml-file)))
             (paths-config (plist-get config :paths)))
        (dolist (op contract/bash--valid-operations)
          (expect (jf/gptel-scope--validate-operation op "/tmp/file.txt" paths-config)
                  :to-be nil)))))

  ;; -- Scenario 4: Path validator compatibility (restrictive) --
  (describe "path validator with restrictive config"
    (it "denies all 11 operations with empty paths"
      (let* ((yml-file (helpers-spec-make-scope-yml
                        (helpers-spec--scope-with-paths
                         nil nil nil nil nil)))
             (config (unwind-protect
                         (helpers-spec-load-scope-config yml-file)
                       (delete-file yml-file)))
             (paths-config (plist-get config :paths)))
        (dolist (op contract/bash--valid-operations)
          (expect (jf/gptel-scope--validate-operation op "/tmp/file.txt" paths-config)
                  :not :to-be nil)))))

  ;; -- Scenario 5: Cloud validator compatibility --
  (describe "cloud validator compatibility"
    (it "cloud auth deny mode blocks auth operations"
      (let* ((yaml-str "paths:\n  read:\n    - \"/**\"\n  write: []\n  execute: []\n  modify: []\n  deny: []\ncloud:\n  auth_detection: \"deny\"\nsecurity:\n  enforce_parse_complete: true\n  max_coverage_threshold: 0.8\n")
             (config (jf/gptel-scope--load-schema
                      (jf/gptel-scope-yaml--parse-string yaml-str)))
             (cloud-config (plist-get config :cloud))
             (cloud-auth-op '(:provider :aws-cli :command "aws s3 ls")))
        (expect (jf/gptel-scope--validate-cloud-auth cloud-auth-op cloud-config)
                :not :to-be nil)))

    (it "cloud auth allow mode permits auth operations"
      (let* ((yaml-str "paths:\n  read: []\n  write: []\n  execute: []\n  modify: []\n  deny: []\ncloud:\n  auth_detection: \"allow\"\nsecurity:\n  enforce_parse_complete: true\n  max_coverage_threshold: 0.8\n")
             (config (jf/gptel-scope--load-schema
                      (jf/gptel-scope-yaml--parse-string yaml-str)))
             (cloud-config (plist-get config :cloud))
             (cloud-auth-op '(:provider :aws-cli :command "aws s3 ls")))
        (expect (jf/gptel-scope--validate-cloud-auth cloud-auth-op cloud-config)
                :to-be nil))))

  ;; -- Scenario 6: Parsed config satisfies contract --
  (describe "parsed configs satisfy scope-config contract"
    (it "parsed config always satisfies scope-config contract"
      (dolist (yaml-str (list
                         ;; Minimal
                         "paths:\n  read:\n    - \"/tmp/**\"\n"
                         ;; Full
                         "paths:\n  read:\n    - \"/**\"\n  write:\n    - \"/**\"\n  execute:\n    - \"/usr/bin/**\"\n  modify:\n    - \"/tmp/**\"\n  deny:\n    - \"/etc/**\"\ncloud:\n  auth_detection: \"allow\"\nsecurity:\n  enforce_parse_complete: false\n  max_coverage_threshold: 0.5\n"))
        (let ((config (jf/gptel-scope--load-schema
                       (jf/gptel-scope-yaml--parse-string yaml-str))))
          (expect config :to-satisfy-contract #'contract/scope-config--validate)))))

  ;; -- Scenario 7: YAML boolean normalization --
  (describe "YAML boolean normalization"
    (it "normalizes YAML boolean keywords to elisp booleans"
      (let* ((yaml-str "paths:\n  read: []\n  write: []\n  execute: []\n  modify: []\n  deny: []\ncloud:\n  auth_detection: \"warn\"\nsecurity:\n  enforce_parse_complete: true\n  max_coverage_threshold: 0.8\n")
             (config (jf/gptel-scope--load-schema
                      (jf/gptel-scope-yaml--parse-string yaml-str))))
        ;; enforce_parse_complete should be t, not :true
        (expect (plist-get (plist-get config :security) :enforce-parse-complete)
                :to-be t)
        ;; Verify it's not :true (YAML keyword)
        (expect (plist-get (plist-get config :security) :enforce-parse-complete)
                :not :to-equal :true)))))

(provide 'scope-config-integration-spec)

;;; scope-config-integration-spec.el ends here
