;;; expansion-ui-handlers-spec.el --- Tests for expansion UI handler implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; REAL UI HANDLER TESTS: Expansion UI Implementation
;;
;; These tests verify the ACTUAL expansion UI handler implementation
;; (jf/gptel-scope--deny-expansion, jf/gptel-scope--add-to-scope,
;; jf/gptel-scope--allow-once-action) with mocked file I/O.
;;
;; What this catches that mocked UI tests don't:
;; - Transient scope storage/retrieval bugs
;; - JSON serialization format changes
;; - Error handling in callback invocation
;; - Handler logic regressions
;; - All user choice paths (deny, approve, allow-once)
;;
;; What this mocks:
;; - File I/O (no disk access needed)
;; - Transient state setup (test sets it directly)

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)
(require 'jf-gptel-scope-core)
(require 'jf-gptel-scope-expansion)

;;; Test Helpers

(defvar test-transient-scope-data nil
  "Storage for mocked transient scope data.")

(defun test-setup-transient-scope (violation-info callback patterns tool-name)
  "Set up transient scope for testing handlers.
VIOLATION-INFO is the violation plist.
CALLBACK is the callback function.
PATTERNS is the list of patterns to add.
TOOL-NAME is the tool requesting expansion.

This simulates what jf/gptel-scope-prompt-expansion does
without actually displaying the transient menu."
  (setq test-transient-scope-data
        (list :violation violation-info
              :callback callback
              :patterns patterns
              :tool-name tool-name)))

(defvar test-yaml-content
  "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  deny: []
org_roam_patterns:
  subdirectory: []
  tags: []
  node_ids: []
bash_tools:
  categories:
    read_only:
      commands: []
    safe_write:
      commands: []
    dangerous:
      commands: []
  deny: []
"
  "Test YAML content for mocked file reads.")

;;; Test Suite

(describe "Expansion UI Handlers (Real Implementation)"

  (before-each
    ;; Mock file I/O operations
    (spy-on 'jf/gptel-scope--get-scope-file-path
            :and-return-value "/fake/scope.yml")
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'file-writable-p :and-return-value t)
    (spy-on 'insert-file-contents
            :and-call-fake (lambda (&rest _)
                            (insert test-yaml-content)))
    (spy-on 'write-region :and-return-value nil)
    (spy-on 'transient-quit-one :and-return-value nil)

    ;; Mock transient-scope to return our test data
    (spy-on 'transient-scope
            :and-call-fake (lambda () test-transient-scope-data))

    ;; Clear test state
    (setq test-transient-scope-data nil))

  (describe "Deny action"

    (it "retrieves callback from transient scope and invokes with denial JSON"
      (let* ((callback-invoked nil)
             (callback-result nil)
             (test-callback (lambda (json)
                             (setq callback-invoked t)
                             (setq callback-result json)))
             (violation-info (list :tool "read_file"
                                  :resource "/tmp/outside.txt"
                                  :reason "not-in-scope"
                                  :validation-type 'path)))

        ;; Setup transient scope manually
        (test-setup-transient-scope violation-info test-callback
                                   '("/tmp/outside.txt") "read_file")

        ;; Call REAL handler
        (jf/gptel-scope--deny-expansion)

        ;; Verify callback was invoked
        (expect callback-invoked :to-be t)

        ;; Verify REAL JSON format from REAL handler
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be nil)
          (expect (plist-get parsed :user_denied) :to-be t)
          (expect (plist-get parsed :message) :to-equal "User denied scope expansion request."))))

    (it "handles missing callback gracefully"
      (let ((violation-info (list :tool "test" :resource "test")))
        ;; Setup scope WITHOUT callback
        (test-setup-transient-scope violation-info nil '("test") "test")

        ;; Should not crash, just warn
        (expect (jf/gptel-scope--deny-expansion) :not :to-throw)

        ;; Verify transient-quit-one was still called
        (expect 'transient-quit-one :to-have-been-called)))

    (it "handles callback invocation errors gracefully"
      (let* ((evil-callback (lambda (_) (error "Boom!")))
             (violation-info (list :tool "test" :resource "test")))

        (test-setup-transient-scope violation-info evil-callback '("test") "test")

        ;; Should catch error and not propagate
        (expect (jf/gptel-scope--deny-expansion) :not :to-throw)

        ;; Should still quit transient
        (expect 'transient-quit-one :to-have-been-called))))

  (describe "Add to scope action"

    (it "adds path to scope and invokes callback with success JSON"
      (let* ((callback-result nil)
             (test-callback (lambda (json) (setq callback-result json)))
             (violation-info (list :tool "write_file_in_scope"
                                  :resource "/workspace/newfile.txt"
                                  :reason "not-in-scope"
                                  :validation-type 'path)))

        (test-setup-transient-scope violation-info test-callback
                                   '("/workspace/newfile.txt") "write_file_in_scope")

        ;; Call REAL handler
        (jf/gptel-scope--add-to-scope)

        ;; Verify file operations were called
        (expect 'jf/gptel-scope--get-scope-file-path :to-have-been-called)
        (expect 'file-exists-p :to-have-been-called)
        (expect 'file-writable-p :to-have-been-called)
        (expect 'write-region :to-have-been-called)

        ;; Verify REAL JSON from REAL handler
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (plist-get parsed :patterns_added) :to-be-truthy)
          ;; Critical: verify it's a vector (JSON array), not list
          (expect (vectorp (plist-get parsed :patterns_added)) :to-be t)
          (expect (length (plist-get parsed :patterns_added)) :to-equal 1)
          (expect (plist-get parsed :message) :to-match "Scope expanded"))))

    (it "handles pattern validation type (org-roam)"
      (let* ((callback-result nil)
             (test-callback (lambda (json) (setq callback-result json)))
             (violation-info (list :tool "create_roam_node_in_scope"
                                  :resource "subdirectory:personal/notes"
                                  :reason "not-in-org-roam-patterns"
                                  :validation-type 'pattern)))

        (test-setup-transient-scope violation-info test-callback
                                   '("subdirectory:personal/notes") "create_roam_node_in_scope")

        ;; Call REAL handler
        (jf/gptel-scope--add-to-scope)

        ;; Verify success
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t))))

    (it "handles bash validation type"
      (let* ((callback-result nil)
             (test-callback (lambda (json) (setq callback-result json)))
             (violation-info (list :tool "run_bash_command"
                                  :resource "grep"
                                  :reason "command-not-allowed"
                                  :validation-type 'bash)))

        (test-setup-transient-scope violation-info test-callback
                                   '("grep") "run_bash_command")

        ;; Call REAL handler
        (jf/gptel-scope--add-to-scope)

        ;; Verify success
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t))))

    (it "handles scope file not found error"
      (spy-on 'file-exists-p :and-return-value nil)  ; Override before-each

      (let* ((callback-result nil)
             (test-callback (lambda (json) (setq callback-result json)))
             (violation-info (list :tool "write_file_in_scope"
                                  :resource "/workspace/file.txt"
                                  :validation-type 'path)))

        (test-setup-transient-scope violation-info test-callback
                                   '("/workspace/file.txt") "write_file_in_scope")

        ;; Should raise user-error which propagates (not caught by handler)
        (expect (jf/gptel-scope--add-to-scope) :to-throw 'user-error)))

    (it "handles scope file not writable error"
      (spy-on 'file-writable-p :and-return-value nil)  ; Override before-each

      (let* ((callback-result nil)
             (test-callback (lambda (json) (setq callback-result json)))
             (violation-info (list :tool "write_file_in_scope"
                                  :resource "/workspace/file.txt"
                                  :validation-type 'path)))

        (test-setup-transient-scope violation-info test-callback
                                   '("/workspace/file.txt") "write_file_in_scope")

        ;; Should raise user-error
        (expect (jf/gptel-scope--add-to-scope) :to-throw 'user-error)))

    (it "handles callback invocation errors gracefully"
      (let* ((evil-callback (lambda (_) (error "Callback explosion!")))
             (violation-info (list :tool "write_file_in_scope"
                                  :resource "/workspace/file.txt"
                                  :validation-type 'path)))

        (test-setup-transient-scope violation-info evil-callback
                                   '("/workspace/file.txt") "write_file_in_scope")

        ;; Should not propagate error (handler catches it)
        (expect (jf/gptel-scope--add-to-scope) :not :to-throw))))

  (describe "Allow once action"

    (it "adds to allow-once list and invokes callback with allowed_once flag"
      (let* ((callback-result nil)
             (test-callback (lambda (json) (setq callback-result json)))
             (violation-info (list :tool "read_file"
                                  :resource "/tmp/temp.txt"
                                  :reason "not-in-scope"
                                  :validation-type 'path)))

        ;; Clear allow-once list
        (setq-local jf/gptel-scope--allow-once-list nil)

        (test-setup-transient-scope violation-info test-callback
                                   '("/tmp/temp.txt") "read_file")

        ;; Call REAL handler
        (jf/gptel-scope--allow-once-action)

        ;; Verify allow-once list was updated (real implementation)
        (expect jf/gptel-scope--allow-once-list :not :to-be nil)
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)

        ;; Verify entry format
        (let ((entry (car jf/gptel-scope--allow-once-list)))
          (expect (car entry) :to-equal "read_file")
          (expect (cdr entry) :to-be-truthy))

        ;; Verify JSON format
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (plist-get parsed :allowed_once) :to-be t)
          (expect (plist-get parsed :message) :to-equal "Permission granted for this turn only."))))

    (it "handles callback invocation errors gracefully"
      (let* ((evil-callback (lambda (_) (error "Boom!")))
             (violation-info (list :tool "read_file"
                                  :resource "/tmp/test.txt"
                                  :validation-type 'path)))

        (test-setup-transient-scope violation-info evil-callback
                                   '("/tmp/test.txt") "read_file")

        ;; Should catch error
        (expect (jf/gptel-scope--allow-once-action) :not :to-throw))))

  (describe "Scope setup function"

    (it "creates transient scope with correct structure"
      (let* ((test-callback (lambda (json) json))
             (violation-info (list :tool "read_file"
                                  :resource "/tmp/test.txt"
                                  :reason "not-in-scope"
                                  :validation-type 'path))
             (patterns '("/tmp/test.txt"))
             (tool-name "read_file"))

        ;; Mock transient-setup to capture arguments
        (spy-on 'transient-setup
                :and-call-fake (lambda (prefix _ignored _ignored2 &rest args)
                                (setq transient--prefix args)))

        ;; Call REAL jf/gptel-scope-prompt-expansion
        (jf/gptel-scope-prompt-expansion violation-info test-callback patterns tool-name)

        ;; Verify transient-setup was called with correct prefix
        (expect 'transient-setup :to-have-been-called-with
                'jf/gptel-scope-expansion-menu nil nil
                :scope (list :violation violation-info
                            :callback test-callback
                            :patterns patterns
                            :tool-name tool-name))))))

(provide 'expansion-ui-handlers-spec)
;;; expansion-ui-handlers-spec.el ends here
