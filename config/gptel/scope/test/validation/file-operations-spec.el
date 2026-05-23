;;; file-operations-spec.el --- Consolidated file operation path validation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CONSOLIDATED FILE OPERATION PATH VALIDATION (Stage 6)
;;
;; Consolidates tests from:
;; - tools/test/integration/test-file-paths.el (44 ERT tests)
;; - tools/test/behavioral/run-bash-command/operation-specific-paths-spec.el (13 Buttercup)
;;
;; Covers: Operation-specific path validation against scope patterns,
;; deny patterns, glob matching, permission hierarchy (write includes read/modify),
;; path resolution, multiple file operations, and structured error responses.
;;
;; Dedup strategy: ERT tests that exercise identical code paths as behavioral
;; tests are merged. Unique scenarios from each source are preserved.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (validation-dir test-dir)
       (scope-test-dir (expand-file-name ".." validation-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))

;;; Local helpers (from ERT test-file-paths.el)

(defun file-ops-spec--make-config (&rest args)
  "Create scope config plist with a :paths section from keyword arguments.
Accepts :read, :write, :execute, :modify, :deny keywords."
  (list :paths
        (list :read (plist-get args :read)
              :write (plist-get args :write)
              :execute (plist-get args :execute)
              :modify (plist-get args :modify)
              :deny (plist-get args :deny))))

(defun file-ops-spec--make-file-op (operation path &optional command-name _absolute-path)
  "Create file operation plist for testing in bash-parser format.
OPERATION is keyword like :read, :write, :execute, :modify.
PATH is the file path.
COMMAND-NAME is optional command that performs operation.
Delegates to `helpers-spec--make-file-op' for contract validation."
  (helpers-spec--make-file-op operation path
                              :command (or command-name "test")))

;;; Test Suite

(describe "File operation path validation (stage 6)"

  ;; === Operation-specific scope validation (unit-level) ===

  (describe "operation-specific scope matching"

    (it "read operation matches paths.read"
      (let* ((config (file-ops-spec--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/file.txt" :read config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "read operation matches paths.write (write includes read)"
      (let* ((config (file-ops-spec--make-config
                      :write '("/tmp/**")))
             (result (jf/gptel-scope--validate-path-operation "/tmp/file.txt" :read config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "write operation requires paths.write"
      (let* ((config (file-ops-spec--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/output.txt" :write config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")))

    (it "execute operation requires paths.execute"
      (let* ((config (file-ops-spec--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/scripts/deploy.py" :execute config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")))

    (it "modify operation allowed by paths.modify"
      (let* ((config (file-ops-spec--make-config
                      :modify '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/config/app.yml" :modify config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "modify operation allowed by paths.write (write includes modify)"
      (let* ((config (file-ops-spec--make-config
                      :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/config/app.yml" :modify config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "delete operation requires paths.write"
      (let* ((config (file-ops-spec--make-config
                      :write '("/tmp/**")))
             (result (jf/gptel-scope--validate-path-operation "/tmp/file.txt" :delete config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "delete operation denied without paths.write"
      (let* ((config (file-ops-spec--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/file.txt" :delete config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")))

    (it "empty patterns deny all operations"
      (let* ((config (file-ops-spec--make-config))
             (result (jf/gptel-scope--validate-path-operation "/workspace/file.txt" :read config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")))

    (it "nil patterns deny all operations"
      (let* ((config (file-ops-spec--make-config :read nil))
             (result (jf/gptel-scope--validate-path-operation "/workspace/file.txt" :read config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :not :to-be nil))))

  ;; === Deny patterns take precedence ===

  (describe "deny patterns precedence"

    (it "read operation on denied path is rejected"
      (let* ((config (file-ops-spec--make-config
                      :deny '("/etc/**")))
             (result (jf/gptel-scope--validate-path-operation "/etc/passwd" :read config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern")))

    (it "deny overrides read scope"
      (let* ((config (file-ops-spec--make-config
                      :read '("/workspace/**")
                      :deny '("/workspace/secrets/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/secrets/api-key.txt" :read config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern")))

    (it "deny overrides write scope"
      (let* ((config (file-ops-spec--make-config
                      :write '("/workspace/**")
                      :deny '("/workspace/.git/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/.git/config" :write config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern")))

    (it "deny overrides execute scope"
      (let* ((config (file-ops-spec--make-config
                      :execute '("/workspace/**")
                      :deny '("/workspace/dangerous/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/dangerous/script.sh" :execute config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern"))))

  ;; === Multiple file operations ===

  (describe "multiple file operations validated independently"

    (it "copy with both paths in scope succeeds"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :read "/workspace/source.txt" "cp")
                        (file-ops-spec--make-file-op :write "/tmp/dest.txt" "cp")))
             (scope-config (file-ops-spec--make-config
                            :read '("/workspace/**")
                            :write '("/tmp/**")))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect result :to-be nil)))

    (it "copy with destination out of scope fails"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :read "/workspace/source.txt" "cp")
                        (file-ops-spec--make-file-op :write "/etc/dest.txt" "cp")))
             (scope-config (file-ops-spec--make-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect (plist-get result :error) :to-equal "not-in-scope")
        (expect (plist-get result :resource) :to-equal "/etc/dest.txt")))

    (it "first operation fails reports immediately"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :write "/etc/bad.txt" "cp")
                        (file-ops-spec--make-file-op :read "/workspace/source.txt" "cp")))
             (scope-config (file-ops-spec--make-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect (plist-get result :error) :not :to-be nil)
        (expect (plist-get result :resource) :to-equal "/etc/bad.txt")))

    (it "multiple read operations all validated"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :read "/workspace/file1.txt")
                        (file-ops-spec--make-file-op :read "/workspace/file2.txt")
                        (file-ops-spec--make-file-op :read "/workspace/file3.txt")))
             (scope-config (file-ops-spec--make-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect result :to-be nil)))

    (it "mixed operations (read/write/execute) all validated"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :read "/workspace/input.txt")
                        (file-ops-spec--make-file-op :execute "/workspace/script.sh")
                        (file-ops-spec--make-file-op :write "/tmp/output.txt")))
             (scope-config (file-ops-spec--make-config
                            :read '("/workspace/**")
                            :execute '("/workspace/**")
                            :write '("/tmp/**")))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect result :to-be nil))))

  ;; === Path resolution ===

  (describe "path resolution"

    (it "resolves relative path from directory"
      (let* ((file-op (file-ops-spec--make-file-op :read "./file.txt" "cat"))
             (directory "/workspace")
             (config (file-ops-spec--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-file-operation
                      file-op directory config)))
        (expect result :to-be nil)))

    (it "resolves parent directory reference"
      (let* ((file-op (file-ops-spec--make-file-op :read "../other/file.txt" "cat"))
             (directory "/workspace/project")
             (config (file-ops-spec--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-file-operation
                      file-op directory config)))
        (expect result :to-be nil)))

    (it "absolute path unchanged"
      (let* ((file-op (file-ops-spec--make-file-op :read "/etc/passwd" "cat"))
             (directory "/workspace")
             (config (file-ops-spec--make-config
                      :read '("/etc/**")))
             (result (jf/gptel-scope--validate-file-operation
                      file-op directory config)))
        (expect result :to-be nil))))

  ;; === Structured error responses ===

  (describe "structured error responses"

    (it "not-in-scope error includes required fields"
      (let* ((config (file-ops-spec--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation "/tmp/file.txt" :write config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")
        (expect (plist-get result :resource) :not :to-be nil)
        (expect (plist-get result :operation) :not :to-be nil)
        (expect (plist-get result :message) :not :to-be nil)))

    (it "denied-pattern error includes required fields"
      (let* ((config (file-ops-spec--make-config
                      :read '("/workspace/**")
                      :deny '("/workspace/secrets/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/secrets/key.txt" :read config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern")
        (expect (plist-get result :resource) :not :to-be nil)
        (expect (plist-get result :operation) :not :to-be nil)
        (expect (plist-get result :message) :not :to-be nil)))

    (it "error message is a string"
      (let* ((config (file-ops-spec--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation "/tmp/file.txt" :write config)))
        (expect (plist-get result :message) :not :to-be nil)
        (expect (stringp (plist-get result :message)) :to-be t))))

  ;; === Glob pattern matching ===

  (describe "glob pattern matching"

    (it "single-level wildcard matches"
      (expect (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/*") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/sub/file.txt" "/workspace/*") :to-be nil))

    (it "recursive wildcard matches"
      (expect (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/**") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/deep/nested/file.txt" "/workspace/**") :to-be-truthy))

    (it "extension pattern matches"
      (expect (jf/gptel-scope--glob-match-p "/workspace/config.yml" "/workspace/*.yml") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/config.json" "/workspace/*.yml") :to-be nil))

    (it "multiple patterns checked in order"
      (let ((patterns '("/workspace/*.txt" "/workspace/*.yml")))
        (expect (jf/gptel-scope--path-matches-any-pattern-p "/workspace/file.txt" patterns) :to-be-truthy)
        (expect (jf/gptel-scope--path-matches-any-pattern-p "/workspace/config.yml" patterns) :to-be-truthy)
        (expect (jf/gptel-scope--path-matches-any-pattern-p "/workspace/script.sh" patterns) :to-be nil)))

    (it "exact path match"
      (expect (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/file.txt") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/other.txt" "/workspace/file.txt") :to-be nil))

    (it "wildcard in middle of pattern"
      (expect (jf/gptel-scope--glob-match-p "/workspace/test/file.txt" "/workspace/*/file.txt") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/test/sub/file.txt" "/workspace/*/file.txt") :to-be nil))

    (it "recursive wildcard in middle of pattern"
      (expect (jf/gptel-scope--glob-match-p "/workspace/test/file.txt" "/workspace/**/file.txt") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/test/sub/file.txt" "/workspace/**/file.txt") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/**/file.txt") :to-be-truthy))

    (it "overlapping patterns (any match allows)"
      (let* ((config (file-ops-spec--make-config
                      :read '("/workspace/**" "/workspace/sub/**")))
             (result (jf/gptel-scope--validate-path-operation "/workspace/sub/file.txt" :read config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "case-sensitive matching"
      (expect (jf/gptel-scope--glob-match-p "/workspace/File.txt" "/workspace/File.txt") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/file.txt" "/workspace/File.txt") :to-be nil))

    (it "dot files matched by wildcards"
      (expect (jf/gptel-scope--glob-match-p "/workspace/.hidden" "/workspace/*") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/.config/file" "/workspace/**") :to-be-truthy))

    (it "special characters in filenames matched literally"
      (expect (jf/gptel-scope--glob-match-p "/workspace/file-name.txt" "/workspace/*") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/file_name.txt" "/workspace/*") :to-be-truthy)
      (expect (jf/gptel-scope--glob-match-p "/workspace/file.name.txt" "/workspace/*") :to-be-truthy)))

  ;; === Full pipeline behavioral tests (from operation-specific-paths-spec.el) ===

  (describe "full pipeline behavioral validation"

    (before-each
      (helpers-spec-setup-session)
      (helpers-spec-setup-bash-mocks))

    (after-each
      (helpers-spec-teardown-bash-mocks)
      (helpers-spec-teardown-session))

    (it "allows read operation within read scope via pipeline"
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/workspace/**" "/tmp/**"))))
        (helpers-spec-mock-bash-parse
         "cat /workspace/file.txt" '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "cat"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /workspace/file.txt" "/workspace" scope-config)))
          (expect result :to-be nil))))

    (it "denies read operation outside scope via pipeline"
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/workspace/**"))))
        (helpers-spec-mock-bash-parse
         "cat /etc/passwd" '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/etc/passwd" :command-name "cat"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /etc/passwd" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "not-in-scope")
          (expect (plist-get result :resource) :to-equal "/etc/passwd"))))

    (it "denies write when only read scope exists via pipeline"
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/tmp/**"))))
        (helpers-spec-mock-bash-parse
         "echo test > /tmp/output.txt" '("echo") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :write "/tmp/output.txt" :command-name "echo"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "echo test > /tmp/output.txt" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "not-in-scope")
          (expect (plist-get result :operation) :to-equal :write))))

    (it "denies operations on deny list paths via pipeline"
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/workspace/**")
                           :deny '("**/.ssh/**"))))
        (helpers-spec-mock-bash-parse
         "cat /workspace/.ssh/id_rsa" '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/.ssh/id_rsa" :command-name "cat"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /workspace/.ssh/id_rsa" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "denied-pattern")
          (expect (plist-get result :resource) :to-equal "/workspace/.ssh/id_rsa"))))

    (it "denies when multiple file operations include paths outside scope"
      (let ((scope-config (helpers-spec-make-scope-config
                           :read '("/workspace/**")
                           :write '("/workspace/**"))))
        (helpers-spec-mock-bash-parse
         "cp /workspace/src.txt /tmp/dst.txt" '("cp") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/src.txt" :command-name "cp")
               (helpers-spec--make-file-op :write "/tmp/dst.txt" :command-name "cp"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cp /workspace/src.txt /tmp/dst.txt" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "not-in-scope")
          (expect (plist-get result :resource) :to-equal "/tmp/dst.txt"))))

    (it "allows multiple operations when all within scope"
      (let ((scope-config (helpers-spec-make-scope-config
                           :write '("/workspace/**"))))
        (helpers-spec-mock-bash-parse
         "cp /workspace/src.txt /workspace/dst.txt" '("cp") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/src.txt" :command-name "cp")
               (helpers-spec--make-file-op :write "/workspace/dst.txt" :command-name "cp"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cp /workspace/src.txt /workspace/dst.txt" "/workspace" scope-config)))
          (expect result :to-be nil))))))

(provide 'file-operations-spec)
;;; file-operations-spec.el ends here
