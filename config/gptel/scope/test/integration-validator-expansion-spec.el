;;; integration-validator-expansion-spec.el --- Integration tests for validator → expansion UI flow -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INTEGRATION TESTS: Validator → build-violation-info → Expansion UI
;;
;; These tests verify the interface contract between scope validation
;; components, ensuring proper data flow from validators through to
;; the expansion UI without mismatches in field names or formats.
;;
;; Test coverage:
;; 1. Path validator output → build-violation-info → violation-info format
;; 2. Pattern validator output → build-violation-info → violation-info format
;; 3. Bash validator output → build-violation-info → violation-info format
;; 4. Violation-info → Expansion UI rendering (no crashes on missing fields)
;;
;; This integration test would have caught the :reason vs :message mismatch
;; that caused the "wrong-type-argument stringp nil" crash in production.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'jf-gptel-scope-core)
(require 'jf-gptel-scope-expansion)

;;; Test Fixtures

(defvar test-scope-config
  '(:paths (:read ("/workspace/allowed/**")
            :write ("/workspace/writable/**")
            :execute ()
            :modify ()
            :deny ("/workspace/denied/**"))
    :cloud (:auth-detection "warn")
    :security (:enforce-parse-complete t
               :max-coverage-threshold 0.8))
  "Test scope configuration for validators.")

;;; Helper Functions

(defun test-call-path-validator (filepath operation)
  "Call path validator with FILEPATH and OPERATION.
Returns check-result from validator."
  (let* ((tool-name (if (eq operation 'read) "read_file" "write_file_in_scope"))
         (category (cdr (assoc tool-name jf/gptel-scope--tool-categories)))
         (args (list filepath))
         (metadata nil))  ; Metadata not needed for this test
    (jf/gptel-scope--validate-path-tool tool-name args category test-scope-config metadata)))

(defun test-call-pattern-validator (pattern)
  "Call pattern validator with PATTERN.
Returns check-result from validator."
  (let* ((tool-name "create_roam_node_in_scope")
         (category (cdr (assoc tool-name jf/gptel-scope--tool-categories)))
         (args (list pattern))
         (config '(:roam (:node-patterns ("work/**" "personal/**")))))
    (jf/gptel-scope--validate-pattern-tool tool-name args category config)))

;;; Test Suite

(describe "Validator → build-violation-info → Expansion UI Integration"

  (describe "Path validator integration"

    (it "transforms path validator denial into valid violation-info"
      ;; Call path validator with out-of-scope file
      (let* ((check-result (test-call-path-validator "/tmp/outside.txt" 'read))
             (violation-info (jf/gptel-scope--build-violation-info check-result "read_file")))
        ;; Verify check-result has expected format
        (expect (plist-get check-result :allowed) :to-be nil)
        (expect (plist-get check-result :reason) :to-equal "not-in-scope")
        (expect (plist-get check-result :resource) :to-be-truthy)

        ;; Verify violation-info transformation
        (expect (plist-get violation-info :tool) :to-equal "read_file")
        (expect (plist-get violation-info :resource) :to-be-truthy)
        (expect (plist-get violation-info :reason) :to-be-truthy)  ;; BUG: This is currently nil!
        (expect (plist-get violation-info :reason) :not :to-be nil)
        (expect (plist-get violation-info :validation-type) :to-equal 'path)))

    (it "handles denied-pattern reason from path validator"
      (let* ((check-result '(:allowed nil
                             :reason "denied-pattern"
                             :resource "/workspace/denied/file.txt"
                             :tool "read_file"))
             (violation-info (jf/gptel-scope--build-violation-info check-result "read_file")))
        (expect (plist-get violation-info :reason) :to-equal "denied-pattern"))))

  (describe "Pattern validator integration"

    (it "transforms pattern validator denial into valid violation-info"
      (let* ((check-result (test-call-pattern-validator "invalid/node/path"))
             (violation-info (jf/gptel-scope--build-violation-info check-result "create_roam_node_in_scope")))
        ;; Verify check-result format
        (expect (plist-get check-result :allowed) :to-be nil)
        (expect (plist-get check-result :reason) :to-be-truthy)

        ;; Verify violation-info transformation
        (expect (plist-get violation-info :tool) :to-equal "create_roam_node_in_scope")
        (expect (plist-get violation-info :reason) :not :to-be nil)
        (expect (plist-get violation-info :validation-type) :to-equal 'pattern))))

  (describe "Bash validator integration (legacy :message format)"

    (it "handles legacy validation-error format with :message field"
      ;; This is the format used by bash validator and in existing tests
      (let* ((validation-error '(:error "command_denied"
                                 :command "rm -rf /tmp/file"
                                 :operation write
                                 :message "Command denied"))
             (violation-info (jf/gptel-scope--build-violation-info validation-error "run_bash_command")))
        ;; Should still work (backwards compatibility)
        (expect (plist-get violation-info :tool) :to-equal "run_bash_command")
        (expect (plist-get violation-info :resource) :to-equal "rm -rf /tmp/file")
        (expect (plist-get violation-info :reason) :to-equal "Command denied")
        (expect (plist-get violation-info :validation-type) :to-equal 'bash)))

    (it "handles cloud auth denial with :message field"
      (let* ((validation-error '(:error "cloud_auth_denied"
                                 :provider "aws"
                                 :operation write
                                 :message "Cloud authentication denied"))
             (violation-info (jf/gptel-scope--build-violation-info validation-error "run_bash_command")))
        (expect (plist-get violation-info :reason) :to-equal "Cloud authentication denied"))))

  (describe "Format priority: :reason over :message"

    (it "prioritizes :reason when both fields present"
      ;; Edge case: if both fields exist, :reason should win (current format)
      (let* ((mixed-format '(:allowed nil
                             :reason "current-format-reason"
                             :message "legacy-format-message"
                             :resource "/tmp/file.txt"
                             :tool "read_file"))
             (violation-info (jf/gptel-scope--build-violation-info mixed-format "read_file")))
        (expect (plist-get violation-info :reason) :to-equal "current-format-reason"))))

  (describe "Transformation robustness"

    (it "returns nil for :reason when neither :reason nor :message present"
      ;; Edge case: malformed check-result missing both fields
      (let* ((malformed '(:allowed nil
                         :resource "/tmp/file.txt"
                         :tool "read_file"))
             (violation-info (jf/gptel-scope--build-violation-info malformed "read_file")))
        ;; Should not crash, but reason will be nil
        (expect (plist-get violation-info :reason) :to-be nil)
        (expect (plist-get violation-info :resource) :to-equal "/tmp/file.txt")))))

(provide 'integration-validator-expansion-spec)
;;; integration-validator-expansion-spec.el ends here
