;;; expansion-ui-spec.el --- Scope expansion UI behavioral tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; SCOPE EXPANSION UI BEHAVIORAL TESTS
;;
;; Consolidated tests for expansion UI mechanics:
;; - Transient menu triggering and action handlers
;; - Scope YAML file updates (add-path-to-scope, add-bash-to-scope)
;; - Inline expansion trigger and approval flow
;; - Expansion prompt display and user choices
;; - Allow-once permission lifecycle through expansion UI
;;
;; Sources:
;; - tools/test/behavioral/run-bash-command/scope-expansion-spec.el (all 7 tests)
;; - tools/test/behavioral/filesystem-tools-scope-expansion-spec.el (Transient section, tests 28-32)
;;
;; Mocking approach:
;; - Mock: transient menu user choices, call-process
;; - Real: validation functions, expansion triggering logic

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Helper Functions

(defun expansion-ui-spec--make-scope-with-paths (read-paths write-paths)
  "Create scope.yml with specified READ-PATHS and WRITE-PATHS.
Returns path to the created file."
  (helpers-spec-make-scope-yml
   (helpers-spec--scope-with-paths
    read-paths
    write-paths
    '() ;; execute
    '() ;; modify
    '()))) ;; deny

(defun expansion-ui-spec--mock-file-metadata (filepath exists git-tracked)
  "Mock file metadata for FILEPATH.
EXISTS is boolean indicating if file exists.
GIT-TRACKED is boolean indicating if file is git-tracked."
  (let ((original-file-exists-p (symbol-function 'file-exists-p)))
    (spy-on 'file-exists-p
            :and-call-fake
            (lambda (path)
              (if (string= path filepath)
                  exists
                (funcall original-file-exists-p path)))))
  (spy-on 'jf/gptel--file-is-git-tracked-p
          :and-call-fake
          (lambda (path)
            (when (string= path filepath)
              git-tracked))))

;;; ============================================================
;;; Tests from scope-expansion-spec.el (bash command expansion)
;;; ============================================================

(describe "Inline scope expansion: bash command workflows"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (describe "Semantic validation: in-scope commands"

    (it "validates read operation when path is in read scope"
      ;; Scenario: Command reads file within configured read paths
      ;; Expected: Validation passes (returns nil)
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /workspace/README.md")
             (directory "/workspace"))

        ;; Mock bash parse and semantics
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/README.md" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Validate command
        (let ((result (jf/gptel-scope--validate-command-semantics
                       command directory scope-config)))

          ;; Assert: Validation succeeds (nil means success)
          (expect result :to-be nil))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Semantic validation: out-of-scope commands"

    (it "denies read operation when path is out of scope"
      ;; Scenario: Command tries to read file outside configured paths
      ;; Expected: Validation fails with not-in-scope error plist
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /tmp/secret.txt")
             (directory "/workspace"))

        ;; Mock out-of-scope read
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/secret.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Validate command
        (let ((result (jf/gptel-scope--validate-command-semantics
                       command directory scope-config)))

          ;; Assert: Validation fails (returns error plist)
          (expect result :not :to-be nil)
          (expect (plist-get result :error) :to-equal "not-in-scope")
          (expect (plist-get result :resource) :to-equal "/tmp/secret.txt")
          (expect (plist-get result :operation) :to-equal :read))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Inline expansion: trigger and approval flow"

    (it "user approves with add-to-scope: wrapper callback receives approval"
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /tmp/data.txt")
             (directory "/workspace")
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/data.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((validation-error
               (append (jf/gptel-scope--validate-command-semantics
                        command directory scope-config)
                       (list :validation-type 'bash))))

          (expect validation-error :not :to-be nil)

          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (_violation-info callback _patterns _tool-name)
                    (setq expansion-ui-called t)
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :patterns_added (vector "/tmp/data.txt")
                                    :message "Added to scope permanently")))))

          (jf/gptel-scope--trigger-inline-expansion
           validation-error "run_bash_command"
           (lambda (expansion-result)
             (setq wrapper-callback-result expansion-result)))

          (expect expansion-ui-called :to-be t)
          (expect (plist-get wrapper-callback-result :approved) :to-be t))

        (delete-file scope-yml)))

    (it "user approves with allow-once: wrapper callback receives approval"
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /tmp/data.txt")
             (directory "/workspace")
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/data.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((validation-error
               (append (jf/gptel-scope--validate-command-semantics
                        command directory scope-config)
                       (list :validation-type 'bash))))

          (expect validation-error :not :to-be nil)

          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (_violation-info callback _patterns _tool-name)
                    (setq expansion-ui-called t)
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :allowed_once t
                                    :message "Allowed for this invocation")))))

          (jf/gptel-scope--trigger-inline-expansion
           validation-error "run_bash_command"
           (lambda (expansion-result)
             (setq wrapper-callback-result expansion-result)))

          (expect expansion-ui-called :to-be t)
          (expect (plist-get wrapper-callback-result :approved) :to-be t))

        (delete-file scope-yml)))

    (it "user denies: wrapper callback receives denial"
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "rm /etc/hosts")
             (directory "/workspace")
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        (helpers-spec-mock-bash-parse command '("rm") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :write "/etc/hosts" :command-name "rm"))
         nil
         '(:ratio 1.0))

        (let ((validation-error
               (append (jf/gptel-scope--validate-command-semantics
                        command directory scope-config)
                       (list :validation-type 'bash))))

          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (_violation-info callback _patterns _tool-name)
                    (setq expansion-ui-called t)
                    (funcall callback
                             (json-serialize
                              (list :success nil
                                    :user_denied t
                                    :message "User denied request")))))

          (jf/gptel-scope--trigger-inline-expansion
           validation-error "run_bash_command"
           (lambda (expansion-result)
             (setq wrapper-callback-result expansion-result)))

          (expect expansion-ui-called :to-be t)
          (expect (plist-get wrapper-callback-result :approved) :to-be nil)
          (expect (plist-get wrapper-callback-result :reason) :to-equal "user_denied"))

        (delete-file scope-yml))))

  (describe "Transient action handlers (bash)"

    (it "add-to-scope action updates scope file and invokes callback"
      ;; Scenario: User clicks "Add to scope (permanent)" in transient menu
      ;; Expected: Scope file updated, callback invoked with success
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                              (setq callback-invoked t)
                              (setq callback-result result))))

        ;; Mock transient scope with path-based violation
        (spy-on 'transient-scope
                :and-return-value (list :violation (helpers-spec--make-violation-info
                                                      "run_bash_command" "command-not-allowed"
                                                      :command "/tmp/test.txt"
                                                      :operation :read)
                                        :callback mock-callback
                                        :patterns '("/tmp/**")
                                        :tool-name "run_bash_command"))

        ;; Mock scope file operations to avoid actual file writes
        (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
        (spy-on 'jf/gptel-scope--add-path-to-scope :and-return-value t)

        ;; Prevent actual transient quit
        (spy-on 'transient-quit-one)

        ;; Simulate user clicking add-to-scope
        (jf/gptel-scope--add-to-scope)

        ;; Assert: Callback was invoked
        (expect callback-invoked :to-be t)

        ;; Assert: Callback result indicates success
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (plist-get parsed :patterns_added) :not :to-be nil))

        ;; Cleanup
        (delete-file scope-yml)))

    (it "deny action invokes callback with user_denied result"
      ;; Scenario: User clicks "Deny" in transient menu
      ;; Expected: Callback invoked with denial structure
      (let* ((callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                              (setq callback-invoked t)
                              (setq callback-result result))))

        ;; Mock transient scope
        (spy-on 'transient-scope
                :and-return-value (list :violation (helpers-spec--make-violation-info
                                                      "run_bash_command" "command-not-allowed"
                                                      :command "/tmp/file.txt")
                                        :callback mock-callback
                                        :patterns '("/tmp/**")
                                        :tool-name "run_bash_command"))

        ;; Prevent actual transient quit
        (spy-on 'transient-quit-one)

        ;; Simulate user clicking deny
        (jf/gptel-scope--deny-expansion)

        ;; Assert: Callback was invoked
        (expect callback-invoked :to-be t)

        ;; Assert: Result structure indicates denial
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (or (eq (plist-get parsed :success) :json-false)
                      (eq (plist-get parsed :success) nil)) :to-be t)
          (expect (plist-get parsed :user_denied) :to-be t))))

    (it "allow-once action invokes callback with success + allowed_once flag"
      (let* ((callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                              (setq callback-invoked t)
                              (setq callback-result result))))

        (spy-on 'transient-scope
                :and-return-value (list :violation (helpers-spec--make-violation-info
                                                      "run_bash_command" "command-not-allowed"
                                                      :command "cat /tmp/file.txt")
                                        :callback mock-callback
                                        :patterns '("/tmp/file.txt")
                                        :tool-name "run_bash_command"))

        (spy-on 'transient-quit-one)

        (jf/gptel-scope--allow-once-action)

        (expect callback-invoked :to-be t)
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (plist-get parsed :allowed_once) :to-be t))))))

;;; ============================================================
;;; Transient action handler tests from filesystem-tools-scope-expansion-spec.el
;;; ============================================================

(describe "Transient action handlers (filesystem tools)"

  (before-each
    (helpers-spec-setup-session))

  (after-each
    (helpers-spec-teardown-session))

  (describe "Add-to-scope action"

    (it "updates read scope for read operations"
      ;; Scenario: User adds path to scope for read operation
      ;; Expected: Expansion UI receives :read operation type
      (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (filepath "/home/user/data.txt")
             (expansion-ui-called nil))

        ;; Spy on expansion UI to verify correct scope section
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback patterns tool-name)
                  (setq expansion-ui-called t)
                  ;; Verify operation is :read
                  (expect (plist-get violation-info :operation) :to-equal :read)
                  ;; Simulate add-to-scope action (updates paths.read)
                  (funcall callback
                           (json-serialize
                            (list :success t
                                  :patterns_added (vector "/home/user/**")
                                  :message "Added to read scope")))))

        ;; Trigger expansion with read operation
        (let ((violation-info (helpers-spec--make-violation-info
                              "read_file_in_scope" "path_out_of_scope"
                              :path filepath
                              :operation :read
                              :metadata (list :exists t :git-tracked t))))
          (jf/gptel-scope-prompt-expansion
           violation-info
           (lambda (result) nil)
           (list "/home/user/**")
           "read_file_in_scope"))

        ;; Assert: Expansion UI was called with read operation
        (expect expansion-ui-called :to-be t)

        ;; Cleanup
        (delete-file scope-yml)))

    (it "updates write scope for write operations"
      ;; Scenario: User adds path to scope for write operation
      ;; Expected: Expansion UI receives :write operation type
      (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                         '()
                         '("/workspace/**")))
             (filepath "/home/user/output.txt")
             (expansion-ui-called nil))

        ;; Spy on expansion UI to verify correct scope section
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback patterns tool-name)
                  (setq expansion-ui-called t)
                  ;; Verify operation is :write
                  (expect (plist-get violation-info :operation) :to-equal :write)
                  ;; Simulate add-to-scope action (updates paths.write)
                  (funcall callback
                           (json-serialize
                            (list :success t
                                  :patterns_added (vector "/home/user/**")
                                  :message "Added to write scope")))))

        ;; Trigger expansion with write operation
        (let ((violation-info (helpers-spec--make-violation-info
                              "write_file_in_scope" "path_out_of_scope"
                              :path filepath
                              :operation :write
                              :metadata (list :exists nil :git-tracked nil))))
          (jf/gptel-scope-prompt-expansion
           violation-info
           (lambda (result) nil)
           (list "/home/user/**")
           "write_file_in_scope"))

        ;; Assert: Expansion UI was called with write operation
        (expect expansion-ui-called :to-be t)

        ;; Cleanup
        (delete-file scope-yml)))

    (it "updates write scope for edit operations"
      ;; Scenario: User adds path to scope for edit operation
      ;; Expected: Expansion UI receives :write operation type (edit requires write permission)
      (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                         '()
                         '("/workspace/**")))
             (filepath "/home/user/code.el")
             (expansion-ui-called nil))

        ;; Spy on expansion UI to verify correct scope section
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback patterns tool-name)
                  (setq expansion-ui-called t)
                  ;; Verify operation is :write (edit uses write validation)
                  (expect (plist-get violation-info :operation) :to-equal :write)
                  ;; Simulate add-to-scope action (updates paths.write)
                  (funcall callback
                           (json-serialize
                            (list :success t
                                  :patterns_added (vector "/home/user/**")
                                  :message "Added to write scope")))))

        ;; Trigger expansion with write operation (edit_file uses write validation)
        (let ((violation-info (helpers-spec--make-violation-info
                              "edit_file_in_scope" "path_out_of_scope"
                              :path filepath
                              :operation :write
                              :metadata (list :exists t :git-tracked t))))
          (jf/gptel-scope-prompt-expansion
           violation-info
           (lambda (result) nil)
           (list "/home/user/**")
           "edit_file_in_scope"))

        ;; Assert: Expansion UI was called with write operation
        (expect expansion-ui-called :to-be t)

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Deny action"

    (it "returns user_denied when user denies request"
      ;; Scenario: User denies scope expansion
      ;; Expected: Callback receives success=false with user_denied flag
      (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (filepath "/etc/passwd")
             (callback-result nil))

        ;; Spy on expansion UI
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback patterns tool-name)
                  ;; Simulate deny action
                  (funcall callback
                           (json-serialize
                            (list :success nil
                                  :user_denied t
                                  :message "User denied request")))))

        ;; Trigger expansion
        (let ((violation-info (helpers-spec--make-violation-info
                              "read_file_in_scope" "path_out_of_scope"
                              :path filepath
                              :operation :read
                              :metadata (list :exists t :git-tracked nil))))
          (jf/gptel-scope-prompt-expansion
           violation-info
           (lambda (result)
             (setq callback-result result))
           (list filepath)
           "read_file_in_scope"))

        ;; Assert: Callback received denial
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (or (eq (plist-get parsed :success) :json-false)
                      (eq (plist-get parsed :success) nil)) :to-be t)
          (expect (plist-get parsed :user_denied) :to-be t))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Allow-once action"

    (it "callback receives success + allowed_once when user allows once"
      (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                         '("/workspace/**")
                         '()))
             (filepath "/tmp/temp.txt")
             (callback-result nil))

        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (_violation-info callback _patterns _tool-name)
                  (funcall callback
                           (json-serialize
                            (list :success t
                                  :allowed_once t
                                  :message "Allowed for this invocation only")))))

        (let ((violation-info (helpers-spec--make-violation-info
                              "read_file_in_scope" "path_out_of_scope"
                              :path filepath
                              :operation :read
                              :metadata (list :exists t :git-tracked nil))))
          (jf/gptel-scope-prompt-expansion
           violation-info
           (lambda (result) (setq callback-result result))
           (list filepath)
           "read_file_in_scope"))

        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (plist-get parsed :allowed_once) :to-be t))

        (delete-file scope-yml)))))

;;; ============================================================
;;; Regression: read-like bash violations must write to paths.read
;;; ============================================================
;;;
;;; Bug: run_bash_command is categorized as :operation write in tool-categories,
;;; so add-path-to-scope always routes bash violations to paths.write.
;;; But read-like violations (:read-metadata, :read-directory, :read, :match-pattern)
;;; should write to paths.read.

(describe "Scope file routing: read vs write paths"

  (it "routes read-metadata bash violation to paths.read, not paths.write"
    ;; Scenario: 'which python3' generates a :read-metadata violation.
    ;; User approves via wildcard expansion.
    ;; Expected: pattern written to paths.read, not paths.write.
    (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                       '("/workspace/**") '("/workspace/**")))
           (resource "/usr/local/bin/python3")
           (callback-result nil)
           (mock-callback (lambda (result) (setq callback-result result))))

      ;; Simulate the transient scope for a read-metadata bash violation
      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :read-metadata
                                                       :validation-type 'bash
                                                       :reason "path_out_of_scope")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'transient-quit-one)

      ;; Call the real write chain — do NOT stub write-pattern-to-scope
      (jf/gptel-scope--add-wildcard-to-scope)

      ;; Read back the scope.yml and verify routing
      (let* ((parsed (jf/gptel-scope-yaml--parse-file scope-yml))
             (normalized (jf/gptel-scope-yaml--normalize-keys parsed))
             (paths (plist-get normalized :paths))
             (read-paths (plist-get paths :read))
             (write-paths (plist-get paths :write)))

        ;; The wildcard /usr/local/bin/** should be in read, not write
        (expect (member "/usr/local/bin/**" read-paths) :not :to-be nil)
        (expect (member "/usr/local/bin/**" write-paths) :to-be nil))

      (delete-file scope-yml)))

  (it "routes read-directory bash violation to paths.read, not paths.write"
    ;; Scenario: 'find /usr/bin -name python*' generates a :read-directory violation.
    ;; User approves via add-to-scope.
    ;; Expected: resource written to paths.read, not paths.write.
    (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                       '("/workspace/**") '("/workspace/**")))
           (resource "/usr/bin/python3")
           (callback-result nil)
           (mock-callback (lambda (result) (setq callback-result result))))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :read-directory
                                                       :validation-type 'bash
                                                       :reason "path_out_of_scope")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'transient-quit-one)

      (jf/gptel-scope--add-to-scope)

      (let* ((parsed (jf/gptel-scope-yaml--parse-file scope-yml))
             (normalized (jf/gptel-scope-yaml--normalize-keys parsed))
             (paths (plist-get normalized :paths))
             (read-paths (plist-get paths :read))
             (write-paths (plist-get paths :write)))

        (expect (member "/usr/bin/python3" read-paths) :not :to-be nil)
        (expect (member "/usr/bin/python3" write-paths) :to-be nil))

      (delete-file scope-yml)))

  (it "routes read bash violation to paths.read, not paths.write"
    ;; Scenario: 'cat /etc/hosts' generates a :read violation.
    ;; Expected: resource written to paths.read.
    (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                       '("/workspace/**") '("/workspace/**")))
           (resource "/etc/hosts")
           (mock-callback (lambda (result) nil)))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :read
                                                       :validation-type 'bash
                                                       :reason "path_out_of_scope")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'transient-quit-one)

      (jf/gptel-scope--add-to-scope)

      (let* ((parsed (jf/gptel-scope-yaml--parse-file scope-yml))
             (normalized (jf/gptel-scope-yaml--normalize-keys parsed))
             (paths (plist-get normalized :paths))
             (read-paths (plist-get paths :read))
             (write-paths (plist-get paths :write)))

        (expect (member "/etc/hosts" read-paths) :not :to-be nil)
        (expect (member "/etc/hosts" write-paths) :to-be nil))

      (delete-file scope-yml)))

  (it "routes match-pattern bash violation to paths.read, not paths.write"
    ;; Scenario: 'find /usr/share -name "*.txt"' generates a :match-pattern violation
    ;; where the resource is the glob pattern string, not a concrete path.
    ;; Expected: pattern string written to paths.read.
    (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                       '("/workspace/**") '("/workspace/**")))
           (resource "*.txt")
           (mock-callback (lambda (result) nil)))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :match-pattern
                                                       :validation-type 'bash
                                                       :reason "path_out_of_scope")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'transient-quit-one)

      (jf/gptel-scope--add-to-scope)

      (let* ((parsed (jf/gptel-scope-yaml--parse-file scope-yml))
             (normalized (jf/gptel-scope-yaml--normalize-keys parsed))
             (paths (plist-get normalized :paths))
             (read-paths (plist-get paths :read))
             (write-paths (plist-get paths :write)))

        (expect (member "*.txt" read-paths) :not :to-be nil)
        (expect (member "*.txt" write-paths) :to-be nil))

      (delete-file scope-yml)))

  (it "routes write bash violation to paths.write"
    ;; Sanity check: write-like operations still route to paths.write.
    (let* ((scope-yml (expansion-ui-spec--make-scope-with-paths
                       '("/workspace/**") '("/workspace/**")))
           (resource "/tmp/output.txt")
           (mock-callback (lambda (result) nil)))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :write
                                                       :validation-type 'bash
                                                       :reason "path_out_of_scope")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'transient-quit-one)

      (jf/gptel-scope--add-to-scope)

      (let* ((parsed (jf/gptel-scope-yaml--parse-file scope-yml))
             (normalized (jf/gptel-scope-yaml--normalize-keys parsed))
             (paths (plist-get normalized :paths))
             (read-paths (plist-get paths :read))
             (write-paths (plist-get paths :write)))

        (expect (member "/tmp/output.txt" write-paths) :not :to-be nil)
        (expect (member "/tmp/output.txt" read-paths) :to-be nil))

      (delete-file scope-yml))))

;;; ============================================================
;;; Tests for wildcard helper and new expansion suffixes
;;; ============================================================

(describe "jf/gptel-scope--parent-wildcard-for"

  (it "returns parent dir with /** for a file path"
    (expect (jf/gptel-scope--parent-wildcard-for "~/foo/bar.txt")
            :to-equal "~/foo/**"))

  (it "handles deeply nested file paths"
    (expect (jf/gptel-scope--parent-wildcard-for "/a/b/src/init.el")
            :to-equal "/a/b/src/**"))

  (it "normalizes trailing slash in parent dir"
    ;; file-name-directory adds trailing slash; string-remove-suffix strips it
    (expect (jf/gptel-scope--parent-wildcard-for "/home/user/projects/foo.txt")
            :to-equal "/home/user/projects/**")))

(describe "Transient action handlers — wildcard add-to-scope"

  (it "wildcard option available when resource is a file"
    ;; The :if predicate returns t when resource is not a directory
    (spy-on 'transient-scope
            :and-return-value (list :violation (list :resource "/home/user/data.txt")
                                    :callback nil))
    (spy-on 'file-directory-p :and-return-value nil)
    (expect (not (file-directory-p
                  (plist-get (plist-get (transient-scope) :violation) :resource)))
            :to-be t))

  (it "wildcard option not available when resource is a directory"
    ;; The :if predicate returns nil when resource is a directory
    (spy-on 'transient-scope
            :and-return-value (list :violation (list :resource "/home/user/projects/")
                                    :callback nil))
    (spy-on 'file-directory-p :and-return-value t)
    (expect (not (file-directory-p
                  (plist-get (plist-get (transient-scope) :violation) :resource)))
            :to-be nil))

  (it "derives parent directory wildcard from file path"
    ;; jf/gptel-scope--add-wildcard-to-scope writes parent/** not the original resource
    (let* ((scope-yml (helpers-spec-make-minimal-scope))
           (resource "/home/user/data.txt")
           (mock-callback (lambda (result) nil)))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :read
                                                       :validation-type 'bash
                                                       :reason "test")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'jf/gptel-scope--write-pattern-to-scope :and-return-value t)
      (spy-on 'transient-quit-one)

      (jf/gptel-scope--add-wildcard-to-scope)

      ;; Assert: write-pattern-to-scope was called with the wildcard, not the original resource.
      ;; The :read at the end is the denied-operation threaded through for section routing.
      (expect 'jf/gptel-scope--write-pattern-to-scope
              :to-have-been-called-with "/home/user/**" 'bash "run_bash_command" scope-yml :read)

      (delete-file scope-yml)))

  (it "callback receives derived wildcard pattern in patterns_added"
    (let* ((scope-yml (helpers-spec-make-minimal-scope))
           (resource "/home/user/data.txt")
           (callback-result nil)
           (mock-callback (lambda (result) (setq callback-result result))))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :read
                                                       :validation-type 'bash
                                                       :reason "test")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'jf/gptel-scope--write-pattern-to-scope :and-return-value t)
      (spy-on 'transient-quit-one)

      (jf/gptel-scope--add-wildcard-to-scope)

      ;; Assert: callback received wildcard pattern, not original resource
      (let* ((parsed (json-parse-string callback-result :object-type 'plist))
             (patterns-added (plist-get parsed :patterns_added)))
        (expect (plist-get parsed :success) :to-be t)
        (expect (aref patterns-added 0) :to-equal "/home/user/**"))

      (delete-file scope-yml)))

  (it "routes through validation-type for wildcard pattern"
    ;; validation-type 'path routes write-pattern-to-scope to add-path-to-scope
    (let* ((scope-yml (helpers-spec-make-minimal-scope))
           (resource "/home/user/data.txt")
           (mock-callback (lambda (result) nil)))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "read_file_in_scope"
                                                       :resource resource
                                                       :operation :read
                                                       :validation-type 'path
                                                       :reason "test")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "read_file_in_scope"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'jf/gptel-scope--write-pattern-to-scope :and-return-value t)
      (spy-on 'transient-quit-one)

      (jf/gptel-scope--add-wildcard-to-scope)

      ;; Assert: write-pattern-to-scope was called with 'path validation type
      ;; and the denied-operation keyword (so paths.read gets targeted).
      (expect 'jf/gptel-scope--write-pattern-to-scope
              :to-have-been-called-with "/home/user/**" 'path "read_file_in_scope" scope-yml :read)

      (delete-file scope-yml))))

(describe "Transient action handlers — custom pattern add-to-scope"

  (it "pre-populates read-string prompt with denied resource"
    ;; read-string should receive resource as the initial input value
    (let* ((scope-yml (helpers-spec-make-minimal-scope))
           (resource "/home/user/data.txt")
           (read-string-initial nil)
           (mock-callback (lambda (result) nil)))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :read
                                                       :validation-type 'bash
                                                       :reason "test")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'jf/gptel-scope--write-pattern-to-scope :and-return-value t)
      (spy-on 'transient-quit-one)

      ;; Stub read-string: capture initial value and return it
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial &rest args)
                   (setq read-string-initial initial)
                   initial)))
        (jf/gptel-scope--add-custom-to-scope))

      ;; Assert: read-string was called with resource as initial input
      (expect read-string-initial :to-equal resource)

      (delete-file scope-yml)))

  (it "writes edited custom pattern to scope when confirmed"
    ;; When read-string returns an edited value, that value is written to scope
    (let* ((scope-yml (helpers-spec-make-minimal-scope))
           (resource "/home/user/data.txt")
           (custom-pattern "/home/user/**")
           (callback-result nil)
           (mock-callback (lambda (result) (setq callback-result result))))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :read
                                                       :validation-type 'bash
                                                       :reason "test")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'jf/gptel-scope--write-pattern-to-scope :and-return-value t)
      (spy-on 'transient-quit-one)

      ;; Stub read-string to return the edited custom pattern
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial &rest args) custom-pattern)))
        (jf/gptel-scope--add-custom-to-scope))

      ;; Assert: write-pattern-to-scope was called with the custom pattern
      (expect 'jf/gptel-scope--write-pattern-to-scope
              :to-have-been-called-with custom-pattern 'bash "run_bash_command" scope-yml)

      ;; Assert: callback received success with custom pattern in patterns_added
      (let* ((parsed (json-parse-string callback-result :object-type 'plist))
             (patterns-added (plist-get parsed :patterns_added)))
        (expect (plist-get parsed :success) :to-be t)
        (expect (aref patterns-added 0) :to-equal custom-pattern))

      (delete-file scope-yml)))

  (it "invokes callback with user_denied when prompt cancelled"
    ;; When user presses C-g (signals quit), callback gets :user_denied t
    (let* ((scope-yml (helpers-spec-make-minimal-scope))
           (resource "/home/user/data.txt")
           (callback-result nil)
           (mock-callback (lambda (result) (setq callback-result result))))

      (spy-on 'transient-scope
              :and-return-value (list :violation (list :tool "run_bash_command"
                                                       :resource resource
                                                       :operation :read
                                                       :validation-type 'bash
                                                       :reason "test")
                                      :callback mock-callback
                                      :patterns (list resource)
                                      :tool-name "run_bash_command"))
      (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
      (spy-on 'transient-quit-one)

      ;; Stub read-string to signal quit (simulating C-g)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial &rest args)
                   (signal 'quit nil))))
        (jf/gptel-scope--add-custom-to-scope))

      ;; Assert: callback received denial
      (let ((parsed (json-parse-string callback-result :object-type 'plist)))
        (expect (or (eq (plist-get parsed :success) :json-false)
                    (eq (plist-get parsed :success) nil)) :to-be t)
        (expect (plist-get parsed :user_denied) :to-be t))

      (delete-file scope-yml))))

(provide 'expansion-ui-spec)
;;; expansion-ui-spec.el ends here
