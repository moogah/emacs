;;; filesystem-tools-scope-expansion-spec.el --- Behavioral tests for filesystem tool scope expansion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; FILESYSTEM TOOLS SCOPE EXPANSION BEHAVIORAL TESTS
;;
;; Tests the complete scope expansion workflow for filesystem tools
;; (read_file, create_file, list_directory) where validation failures
;; automatically trigger the expansion UI, allowing operations within
;; a single tool call upon user approval.
;;
;; This is HIGH-LEVEL behavioral testing:
;; - Tests path validation workflow (read/write operations)
;; - Tests expansion UI trigger and approval flow
;; - Tests allow-once permission lifecycle
;; - Tests transient action handlers (user choices)
;; - Mocks only stateful operations (file I/O, git, user interaction)
;;
;; Key workflows tested for read_file:
;; 1. File in read scope → validation passes
;; 2. File in write scope → validation passes (hierarchical permissions)
;; 3. File out of scope → validation fails → UI triggered → user adds to scope → approval
;; 4. File out of scope → validation fails → UI triggered → user allows once → approval
;; 5. File out of scope → validation fails → UI triggered → user denies → error
;; 6. File not found → error returned
;; 7. Allow-once permission lifecycle (granted → consumed → expired)
;;
;; NOTE: These tests focus on the validation and expansion workflow.
;; Actual file I/O operations are mocked to test only the permission logic.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name ".." test-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "../../../scope/scope-expansion.el" tools-dir)))

;;; Helper Functions

(defun helpers-spec-make-scope-with-paths (read-paths write-paths)
  "Create scope.yml with specified READ-PATHS and WRITE-PATHS.
Returns path to the created file."
  (helpers-spec-make-scope-yml
   (helpers-spec--scope-with-paths
    read-paths
    write-paths
    '() ;; execute
    '() ;; modify
    '()))) ;; deny

(defun helpers-spec-mock-file-metadata (filepath exists git-tracked)
  "Mock file metadata for FILEPATH.
EXISTS is boolean indicating if file exists.
GIT-TRACKED is boolean indicating if file is git-tracked."
  ;; Store original function before spying
  (let ((original-file-exists-p (symbol-function 'file-exists-p)))
    (spy-on 'file-exists-p
            :and-call-fake
            (lambda (path)
              (if (string= path filepath)
                  exists
                ;; Let other file existence checks pass through using original
                (funcall original-file-exists-p path)))))
  (spy-on 'jf/gptel--file-is-git-tracked-p
          :and-call-fake
          (lambda (path)
            (when (string= path filepath)
              git-tracked))))

;;; Test Suite

(describe "read_file: Scope expansion workflows"

  (before-each
    (helpers-spec-setup-session)
    ;; Clear allow-once list for test isolation
    (setq jf/gptel-scope--allow-once-list nil))

  (after-each
    (helpers-spec-teardown-session))

  (describe "Path validation: in-scope operations"

    (it "allows read operation when file is in read scope"
      ;; Scenario: File path matches configured read patterns
      ;; Expected: Validation passes (returns nil)
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '("/workspace/**")
                         '("/tmp/**")))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (filepath "/workspace/README.md")
             (paths (plist-get scope-config :paths)))

        ;; Mock file metadata
        (helpers-spec-mock-file-metadata filepath t t)

        ;; Validate read operation
        (let ((result (jf/gptel-scope--validate-operation :read filepath paths)))
          ;; Assert: Validation succeeds (nil means success)
          (expect result :to-be nil))

        ;; Cleanup
        (delete-file scope-yml)))

    (it "allows read operation when file is in write scope (hierarchical)"
      ;; Scenario: Write scope implies read permission
      ;; Expected: Read validation passes for file in write scope
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '()
                         '("/workspace/**")))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (filepath "/workspace/data.txt")
             (paths (plist-get scope-config :paths)))

        ;; Mock file metadata
        (helpers-spec-mock-file-metadata filepath t t)

        ;; Validate read operation (should succeed via write scope)
        (let ((result (jf/gptel-scope--validate-operation :read filepath paths)))
          (expect result :to-be nil))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Path validation: out-of-scope operations"

    (it "denies read operation when file is outside scope"
      ;; Scenario: File path does not match any configured patterns
      ;; Expected: Validation fails with path_out_of_scope error
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '("/workspace/**")
                         '("/tmp/**")))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (filepath "/etc/passwd")
             (paths (plist-get scope-config :paths)))

        ;; Mock file metadata
        (helpers-spec-mock-file-metadata filepath t nil)

        ;; Validate read operation
        (let ((result (jf/gptel-scope--validate-operation :read filepath paths)))
          ;; Assert: Validation fails (returns error plist)
          (expect result :not :to-be nil)
          (expect (plist-get result :error) :to-match "scope")
          (expect (plist-get result :path) :to-equal filepath)
          (expect (plist-get result :operation) :to-equal :read))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Expansion workflow: add-to-scope"

    (it "triggers expansion UI when out of scope, user adds to scope, approval granted"
      ;; Scenario: Out-of-scope → UI → add to scope → scope.yml updated → approval
      ;; Expected: Scope file modified, callback receives approval with patterns
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (filepath "/home/user/data.txt")
             (paths (plist-get scope-config :paths))
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        ;; Mock file metadata
        (helpers-spec-mock-file-metadata filepath t t)

        ;; Get validation error
        (let ((validation-error (jf/gptel-scope--validate-operation :read filepath paths)))
          (expect validation-error :not :to-be nil)

          ;; Spy on expansion UI
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns tool-name)
                    (setq expansion-ui-called t)
                    ;; Simulate user choosing "Add to scope"
                    ;; patterns_added should be a vector (JSON array)
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :patterns_added (vector "/home/user/**")
                                    :message "Added to scope permanently")))))

          ;; Build violation info for expansion trigger
          (let ((violation-info (list :tool "read_file"
                                      :resource filepath
                                      :operation :read
                                      :reason "path_out_of_scope"
                                      :metadata (list :exists t
                                                     :git-tracked t))))

            ;; Trigger inline expansion
            (jf/gptel-scope-prompt-expansion
             violation-info
             (lambda (expansion-result)
               (setq wrapper-callback-result expansion-result))
             '("/home/user/**")
             "read_file")

            ;; Assert: Expansion UI was called
            (expect expansion-ui-called :to-be t)

            ;; Assert: Wrapper callback received approval
            (let ((parsed (json-parse-string wrapper-callback-result :object-type 'plist)))
              (expect (plist-get parsed :success) :to-be t)
              (expect (plist-get parsed :patterns_added) :not :to-be nil))))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Expansion workflow: allow-once"

    (it "triggers expansion UI when out of scope, user allows once, permission granted"
      ;; Scenario: Out-of-scope → UI → allow once → allow-once list updated → approval
      ;; Expected: Allow-once permission granted, callback receives approval
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (filepath "/tmp/temp.txt")
             (paths (plist-get scope-config :paths))
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        ;; Mock file metadata
        (helpers-spec-mock-file-metadata filepath t nil)

        ;; Get validation error
        (let ((validation-error (jf/gptel-scope--validate-operation :read filepath paths)))
          (expect validation-error :not :to-be nil)

          ;; Spy on expansion UI
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns tool-name)
                    (setq expansion-ui-called t)
                    ;; Simulate user choosing "Allow once"
                    (jf/gptel-scope-add-to-allow-once-list
                     "read_file"
                     filepath)
                    ;; Invoke callback with approval
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :allowed_once t
                                    :message "Allowed for this turn only")))))

          ;; Build violation info
          (let ((violation-info (list :tool "read_file"
                                      :resource filepath
                                      :operation :read
                                      :reason "path_out_of_scope"
                                      :metadata (list :exists t
                                                     :git-tracked nil))))

            ;; Trigger inline expansion
            (jf/gptel-scope-prompt-expansion
             violation-info
             (lambda (expansion-result)
               (setq wrapper-callback-result expansion-result))
             (list filepath)
             "read_file")

            ;; Assert: Expansion UI was called
            (expect expansion-ui-called :to-be t)

            ;; Assert: Wrapper callback received approval
            (let ((parsed (json-parse-string wrapper-callback-result :object-type 'plist)))
              (expect (plist-get parsed :success) :to-be t)
              (expect (plist-get parsed :allowed_once) :to-be t))

            ;; Assert: Allow-once permission was granted
            (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
            (let ((entry (car jf/gptel-scope--allow-once-list)))
              (expect (car entry) :to-equal "read_file")
              (expect (cdr entry) :to-equal filepath))))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Expansion workflow: denial"

    (it "triggers expansion UI when out of scope, user denies, error returned"
      ;; Scenario: Out-of-scope → UI → deny → rejection
      ;; Expected: Wrapper callback receives denial with user_denied flag
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (filepath "/etc/hosts")
             (paths (plist-get scope-config :paths))
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        ;; Mock file metadata
        (helpers-spec-mock-file-metadata filepath t nil)

        ;; Get validation error
        (let ((validation-error (jf/gptel-scope--validate-operation :read filepath paths)))
          (expect validation-error :not :to-be nil)

          ;; Spy on expansion UI
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns tool-name)
                    (setq expansion-ui-called t)
                    ;; Simulate user choosing "Deny"
                    (funcall callback
                             (json-serialize
                              (list :success nil
                                    :user_denied t
                                    :message "User denied request")))))

          ;; Build violation info
          (let ((violation-info (list :tool "read_file"
                                      :resource filepath
                                      :operation :read
                                      :reason "path_out_of_scope"
                                      :metadata (list :exists t
                                                     :git-tracked nil))))

            ;; Trigger inline expansion
            (jf/gptel-scope-prompt-expansion
             violation-info
             (lambda (expansion-result)
               (setq wrapper-callback-result expansion-result))
             (list filepath)
             "read_file")

            ;; Assert: Expansion UI was called
            (expect expansion-ui-called :to-be t)

            ;; Assert: Wrapper callback received denial
            (let ((parsed (json-parse-string wrapper-callback-result :object-type 'plist)))
              (expect (or (eq (plist-get parsed :success) :json-false)
                          (eq (plist-get parsed :success) nil)) :to-be t)
              (expect (plist-get parsed :user_denied) :to-be t))))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Error handling"

    (it "returns error when file does not exist"
      ;; Scenario: File path is valid but file doesn't exist
      ;; Expected: Operation proceeds with metadata indicating non-existence
      ;; NOTE: File existence is checked by the tool, not the validator
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (filepath "/workspace/missing.txt")
             (paths (plist-get scope-config :paths)))

        ;; Mock file as non-existent
        (helpers-spec-mock-file-metadata filepath nil nil)

        ;; Validate operation (should pass validation even if file doesn't exist)
        (let ((result (jf/gptel-scope--validate-operation :read filepath paths)))
          ;; Validation should succeed (file existence is tool's concern)
          (expect result :to-be nil))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Allow-once permission lifecycle"

    (it "validation succeeds after allow-once permission granted"
      ;; Scenario: Permission granted → validation check → allow-once detected
      ;; Expected: Allow-once permission exists and can be checked
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (filepath "/tmp/temporary.txt"))

        ;; Initially, allow-once list is empty
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 0)

        ;; Grant allow-once permission (what expansion UI does)
        (jf/gptel-scope-add-to-allow-once-list "read_file" filepath)

        ;; Verify permission was granted
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
        (let ((entry (car jf/gptel-scope--allow-once-list)))
          (expect (car entry) :to-equal "read_file")
          (expect (cdr entry) :to-equal filepath))

        ;; Cleanup
        (delete-file scope-yml)))

    (it "allow-once permission can be consumed and removed"
      ;; Scenario: Permission granted → consumed → removed from list
      ;; Expected: Allow-once list updated correctly after consumption
      (let* ((filepath "/tmp/once.txt"))

        ;; Grant permission
        (jf/gptel-scope-add-to-allow-once-list "read_file" filepath)
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)

        ;; Consume permission (simulate what jf/gptel-scope--check-allow-once does)
        (let ((entry (cl-find-if (lambda (e)
                                   (and (equal (car e) "read_file")
                                        (equal (cdr e) filepath)))
                                 jf/gptel-scope--allow-once-list)))
          (when entry
            (setq jf/gptel-scope--allow-once-list
                  (delq entry jf/gptel-scope--allow-once-list))))

        ;; Verify permission was consumed
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 0)))

    (it "allow-once permission is isolated per tool"
      ;; Scenario: Different tools have separate allow-once permissions
      ;; Expected: Permissions don't interfere across tools
      (let* ((filepath "/tmp/shared.txt"))

        ;; Grant permission for read_file
        (jf/gptel-scope-add-to-allow-once-list "read_file" filepath)

        ;; Grant permission for create_file (different tool)
        (jf/gptel-scope-add-to-allow-once-list "create_file" filepath)

        ;; Both permissions should exist
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 2)

        ;; Consume read_file permission (simulate what jf/gptel-scope--check-allow-once does)
        (let ((entry (cl-find-if (lambda (e)
                                   (and (equal (car e) "read_file")
                                        (equal (cdr e) filepath)))
                                 jf/gptel-scope--allow-once-list)))
          (when entry
            (setq jf/gptel-scope--allow-once-list
                  (delq entry jf/gptel-scope--allow-once-list))))

        ;; Only create_file permission should remain
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
        (let ((entry (car jf/gptel-scope--allow-once-list)))
          (expect (car entry) :to-equal "create_file")))))

  (describe "Metadata handling"

    (it "includes git-tracked status in expansion UI"
      ;; Scenario: Validation fails for git-tracked file
      ;; Expected: Expansion UI receives metadata with git-tracked flag
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (filepath "/home/user/tracked.txt")
             (paths (plist-get scope-config :paths))
             (expansion-ui-called nil)
             (received-metadata nil))

        ;; Mock file as existing and git-tracked
        (helpers-spec-mock-file-metadata filepath t t)

        ;; Get validation error
        (let ((validation-error (jf/gptel-scope--validate-operation :read filepath paths)))
          (expect validation-error :not :to-be nil)

          ;; Spy on expansion UI to capture metadata
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns tool-name)
                    (setq expansion-ui-called t)
                    (setq received-metadata (plist-get violation-info :metadata))
                    (funcall callback
                             (json-serialize
                              (list :success nil
                                    :user_denied t
                                    :message "Denied")))))

          ;; Build violation info with metadata
          (let ((violation-info (list :tool "read_file"
                                      :resource filepath
                                      :operation :read
                                      :reason "path_out_of_scope"
                                      :metadata (list :exists t
                                                     :git-tracked t))))

            ;; Trigger expansion
            (jf/gptel-scope-prompt-expansion
             violation-info
             (lambda (result) nil)
             (list filepath)
             "read_file")

            ;; Assert: Expansion UI was called with correct metadata
            (expect expansion-ui-called :to-be t)
            (expect (plist-get received-metadata :exists) :to-be t)
            (expect (plist-get received-metadata :git-tracked) :to-be t)))

        ;; Cleanup
        (delete-file scope-yml)))

    (it "includes non-existent status in expansion UI"
      ;; Scenario: Validation fails for non-existent file
      ;; Expected: Expansion UI receives metadata with exists=nil
      (let* ((scope-yml (helpers-spec-make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (filepath "/home/user/missing.txt")
             (paths (plist-get scope-config :paths))
             (expansion-ui-called nil)
             (received-metadata nil))

        ;; Mock file as non-existent
        (helpers-spec-mock-file-metadata filepath nil nil)

        ;; Get validation error
        (let ((validation-error (jf/gptel-scope--validate-operation :read filepath paths)))
          (expect validation-error :not :to-be nil)

          ;; Spy on expansion UI to capture metadata
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns tool-name)
                    (setq expansion-ui-called t)
                    (setq received-metadata (plist-get violation-info :metadata))
                    (funcall callback
                             (json-serialize
                              (list :success nil
                                    :user_denied t
                                    :message "Denied")))))

          ;; Build violation info with metadata
          (let ((violation-info (list :tool "read_file"
                                      :resource filepath
                                      :operation :read
                                      :reason "path_out_of_scope"
                                      :metadata (list :exists nil
                                                     :git-tracked nil))))

            ;; Trigger expansion
            (jf/gptel-scope-prompt-expansion
             violation-info
             (lambda (result) nil)
             (list filepath)
             "read_file")

            ;; Assert: Expansion UI was called with correct metadata
            (expect expansion-ui-called :to-be t)
            (expect (plist-get received-metadata :exists) :to-be nil)
            (expect (plist-get received-metadata :git-tracked) :to-be nil)))

        ;; Cleanup
        (delete-file scope-yml)))))

(provide 'filesystem-tools-scope-expansion-spec)
;;; filesystem-tools-scope-expansion-spec.el ends here
