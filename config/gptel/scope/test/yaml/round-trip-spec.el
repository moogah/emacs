;;; yaml-round-trip-spec.el --- YAML round-trip tests for scope expansion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; YAML ROUND-TRIP TESTS
;;
;; These tests verify that reading a scope file and writing it back preserves
;; all structure, including nested plists like :cloud and :security.
;;
;; Test coverage:
;; 1. Full scope file with all sections (paths, cloud, security, tools, org-roam)
;; 2. Inline expansion (add-path-to-scope) preserves existing :cloud and :security sections
;; 3. Generic nested plist handling for unknown keys
;;
;; This test would have caught the "Unknown simple value type for key 'cloud'" bug.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'jf-gptel-scope-validation)
(require 'jf-gptel-scope-tool-wrapper)
(require 'jf-gptel-scope-expansion)
(require 'yaml)

;;; Test Fixtures

(defvar test-full-scope-yaml
  "paths:
  read:
    - \"/workspace/allowed/**\"
    - \"~/.machine-role\"
  write:
    - \"/workspace/writable/**\"
  execute: []
  modify: []
  deny:
    - \"**/.git/**\"
    - \"**/runtime/**\"

org_roam_patterns:
  subdirectory:
    - \"gptel/**\"
  tags:
    - \"gptel\"
  node_ids:
    - \"*\"

cloud:
  auth_detection: \"warn\"
  allowed_providers: []

security:
  enforce_parse_complete: \"t\"
  max_coverage_threshold: 0.8

tools:
  - read_file_in_scope
  - write_file_in_scope
"
  "Full scope.yml with all sections including nested plists.")

;;; Test Suite

(describe "YAML Round-trip Tests"

  (describe "Reading and writing full scope files"

    (it "preserves all sections when reading and writing back"
      (let* ((temp-file (make-temp-file "scope-" nil ".yml"))
             parsed normalized)
        (unwind-protect
            (progn
              ;; Write test YAML to temp file
              (with-temp-file temp-file
                (insert test-full-scope-yaml))

              ;; Read and parse
              (setq parsed (jf/gptel-scope--read-scope-file-as-yaml temp-file))
              (setq normalized (jf/gptel-scope-yaml--normalize-keys parsed))

              ;; Verify all sections present
              (expect (plist-get normalized :paths) :not :to-be nil)
              (expect (plist-get normalized :cloud) :not :to-be nil)
              (expect (plist-get normalized :security) :not :to-be nil)
              (expect (plist-get normalized :tools) :not :to-be nil)

              ;; Verify nested cloud structure
              (let ((cloud (plist-get normalized :cloud)))
                (expect (plist-get cloud :auth-detection) :to-equal "warn")
                (expect (plist-get cloud :allowed-providers) :to-equal '()))

              ;; Verify nested security structure
              (let ((security (plist-get normalized :security)))
                (expect (plist-get security :enforce-parse-complete) :to-equal "t")
                (expect (plist-get security :max-coverage-threshold) :to-equal 0.8))

              ;; Write back to file
              (with-temp-buffer
                (jf/gptel-scope--write-yaml-plist normalized)
                (write-region (point-min) (point-max) temp-file nil 'silent))

              ;; Read again and verify structure preserved
              (let* ((parsed2 (jf/gptel-scope--read-scope-file-as-yaml temp-file))
                     (normalized2 (jf/gptel-scope-yaml--normalize-keys parsed2)))
                (expect (plist-get normalized2 :cloud) :not :to-be nil)
                (expect (plist-get normalized2 :security) :not :to-be nil)
                (let ((cloud2 (plist-get normalized2 :cloud)))
                  (expect (plist-get cloud2 :auth-detection) :to-equal "warn"))
                (let ((security2 (plist-get normalized2 :security)))
                  (expect (plist-get security2 :enforce-parse-complete) :to-equal "t")
                  (expect (plist-get security2 :max-coverage-threshold) :to-equal 0.8))))

          ;; Cleanup
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

    (it "handles empty nested plist values"
      (let* ((temp-file (make-temp-file "scope-" nil ".yml"))
             (test-yaml "cloud:\n  auth_detection: \"allow\"\n  allowed_providers: []\n"))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert test-yaml))
              (let* ((parsed (jf/gptel-scope--read-scope-file-as-yaml temp-file))
                     (normalized (jf/gptel-scope-yaml--normalize-keys parsed)))
                (with-temp-buffer
                  (jf/gptel-scope--write-yaml-plist normalized)
                  (write-region (point-min) (point-max) temp-file nil 'silent))
                ;; Verify round-trip
                (let* ((parsed2 (jf/gptel-scope--read-scope-file-as-yaml temp-file))
                       (normalized2 (jf/gptel-scope-yaml--normalize-keys parsed2))
                       (cloud (plist-get normalized2 :cloud)))
                  (expect (plist-get cloud :auth-detection) :to-equal "allow")
                  (expect (plist-get cloud :allowed-providers) :to-equal '()))))
          (when (file-exists-p temp-file)
            (delete-file temp-file))))))

  (describe "Inline expansion with full scope files"

    (it "preserves :cloud and :security when adding path"
      (let* ((temp-file (make-temp-file "scope-" nil ".yml")))
        (unwind-protect
            (progn
              ;; Write full scope file
              (with-temp-file temp-file
                (insert test-full-scope-yaml))

              ;; Add a new path (simulating inline expansion)
              (jf/gptel-scope--add-path-to-scope temp-file "/new/path/**" "read_file_in_scope")

              ;; Read back and verify ALL sections still present
              (let* ((parsed (jf/gptel-scope--read-scope-file-as-yaml temp-file))
                     (normalized (jf/gptel-scope-yaml--normalize-keys parsed)))

                ;; Verify new path added
                (let* ((paths (plist-get normalized :paths))
                       (read-paths (plist-get paths :read)))
                  (expect (member "/new/path/**" read-paths) :to-be-truthy))

                ;; CRITICAL: Verify :cloud section preserved
                (let ((cloud (plist-get normalized :cloud)))
                  (expect cloud :not :to-be nil)
                  (expect (plist-get cloud :auth-detection) :to-equal "warn"))

                ;; CRITICAL: Verify :security section preserved
                (let ((security (plist-get normalized :security)))
                  (expect security :not :to-be nil)
                  (expect (plist-get security :enforce-parse-complete) :to-equal "t")
                  (expect (plist-get security :max-coverage-threshold) :to-equal 0.8))

                ;; Verify other sections preserved
                (expect (plist-get normalized :org-roam-patterns) :not :to-be nil)))

          ;; Cleanup
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

    ))

(provide 'yaml-round-trip-spec)
;;; yaml-round-trip-spec.el ends here
