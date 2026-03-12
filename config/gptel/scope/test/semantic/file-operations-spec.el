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
       (semantic-dir test-dir)
       (scope-test-dir (expand-file-name ".." semantic-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))

;;; Local helpers (from ERT test-file-paths.el)

(defun file-ops-spec--make-paths-config (&rest args)
  "Create paths configuration plist from keyword arguments.
Accepts :read, :write, :execute, :modify, :deny keywords."
  (list :read (plist-get args :read)
        :write (plist-get args :write)
        :execute (plist-get args :execute)
        :modify (plist-get args :modify)
        :deny (plist-get args :deny)))

(defun file-ops-spec--make-file-op (operation path &optional command-name _absolute-path)
  "Create file operation plist for testing in bash-parser format.
OPERATION is keyword like :read, :write, :execute, :modify.
PATH is the file path.
COMMAND-NAME is optional command that performs operation."
  (list :file path
        :operation operation
        :confidence :high
        :source :positional-arg
        :command (or command-name "test")))

;;; Test Suite

(describe "File operation path validation (stage 6)"

  ;; === Operation-specific scope validation (unit-level) ===

  (describe "operation-specific scope matching"

    (it "read operation matches paths.read"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "read operation matches paths.write (write includes read)"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :write '("/tmp/**")))
             (result (jf/gptel-scope--validate-operation :read "/tmp/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "write operation requires paths.write"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation :write "/workspace/output.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")))

    (it "execute operation requires paths.execute"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation :execute "/workspace/scripts/deploy.py" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")))

    (it "modify operation allowed by paths.modify"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :modify '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation :modify "/workspace/config/app.yml" paths-config)))
        (expect result :to-be nil)))

    (it "modify operation allowed by paths.write (write includes modify)"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation :modify "/workspace/config/app.yml" paths-config)))
        (expect result :to-be nil)))

    (it "delete operation requires paths.write"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :write '("/tmp/**")))
             (result (jf/gptel-scope--validate-operation :delete "/tmp/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "delete operation denied without paths.write"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation :delete "/workspace/file.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")))

    (it "empty patterns deny all operations"
      (let* ((paths-config (file-ops-spec--make-paths-config))
             (result (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")))

    (it "nil patterns deny all operations"
      (let* ((paths-config (file-ops-spec--make-paths-config :read nil))
             (result (jf/gptel-scope--validate-operation :read "/workspace/file.txt" paths-config)))
        (expect (plist-get result :error) :not :to-be nil))))

  ;; === Deny patterns take precedence ===

  (describe "deny patterns precedence"

    (it "read operation on denied path is rejected"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :deny '("/etc/**")))
             (result (jf/gptel-scope--validate-operation :read "/etc/passwd" paths-config)))
        (expect (plist-get result :error) :to-equal "path_denied")))

    (it "deny overrides read scope"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")
                            :deny '("/workspace/secrets/**")))
             (result (jf/gptel-scope--validate-operation :read "/workspace/secrets/api-key.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_denied")))

    (it "deny overrides write scope"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :write '("/workspace/**")
                            :deny '("/workspace/.git/**")))
             (result (jf/gptel-scope--validate-operation :write "/workspace/.git/config" paths-config)))
        (expect (plist-get result :error) :to-equal "path_denied")))

    (it "deny overrides execute scope"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :execute '("/workspace/**")
                            :deny '("/workspace/dangerous/**")))
             (result (jf/gptel-scope--validate-operation :execute "/workspace/dangerous/script.sh" paths-config)))
        (expect (plist-get result :error) :to-equal "path_denied"))))

  ;; === Multiple file operations ===

  (describe "multiple file operations validated independently"

    (it "copy with both paths in scope succeeds"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :read "/workspace/source.txt" "cp")
                        (file-ops-spec--make-file-op :write "/tmp/dest.txt" "cp")))
             (scope-config (list :paths (file-ops-spec--make-paths-config
                                         :read '("/workspace/**")
                                         :write '("/tmp/**"))))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect result :to-be nil)))

    (it "copy with destination out of scope fails"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :read "/workspace/source.txt" "cp")
                        (file-ops-spec--make-file-op :write "/etc/dest.txt" "cp")))
             (scope-config (list :paths (file-ops-spec--make-paths-config
                                         :read '("/workspace/**"))))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :path) :to-equal "/etc/dest.txt")))

    (it "first operation fails reports immediately"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :write "/etc/bad.txt" "cp")
                        (file-ops-spec--make-file-op :read "/workspace/source.txt" "cp")))
             (scope-config (list :paths (file-ops-spec--make-paths-config
                                         :read '("/workspace/**"))))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect (plist-get result :error) :not :to-be nil)
        (expect (plist-get result :path) :to-equal "/etc/bad.txt")))

    (it "multiple read operations all validated"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :read "/workspace/file1.txt")
                        (file-ops-spec--make-file-op :read "/workspace/file2.txt")
                        (file-ops-spec--make-file-op :read "/workspace/file3.txt")))
             (scope-config (list :paths (file-ops-spec--make-paths-config
                                         :read '("/workspace/**"))))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect result :to-be nil)))

    (it "mixed operations (read/write/execute) all validated"
      (let* ((file-ops (list
                        (file-ops-spec--make-file-op :read "/workspace/input.txt")
                        (file-ops-spec--make-file-op :execute "/workspace/script.sh")
                        (file-ops-spec--make-file-op :write "/tmp/output.txt")))
             (scope-config (list :paths (file-ops-spec--make-paths-config
                                         :read '("/workspace/**")
                                         :execute '("/workspace/**")
                                         :write '("/tmp/**"))))
             (result (jf/gptel-scope--validate-file-operations
                      file-ops "/workspace" scope-config)))
        (expect result :to-be nil))))

  ;; === Path resolution ===

  (describe "path resolution"

    (it "resolves relative path from directory"
      (let* ((file-op (file-ops-spec--make-file-op :read "./file.txt" "cat"))
             (directory "/workspace")
             (paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-file-operation
                      file-op directory paths-config)))
        (expect result :to-be nil)))

    (it "resolves parent directory reference"
      (let* ((file-op (file-ops-spec--make-file-op :read "../other/file.txt" "cat"))
             (directory "/workspace/project")
             (paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-file-operation
                      file-op directory paths-config)))
        (expect result :to-be nil)))

    (it "absolute path unchanged"
      (let* ((file-op (file-ops-spec--make-file-op :read "/etc/passwd" "cat"))
             (directory "/workspace")
             (paths-config (file-ops-spec--make-paths-config
                            :read '("/etc/**")))
             (result (jf/gptel-scope--validate-file-operation
                      file-op directory paths-config)))
        (expect result :to-be nil))))

  ;; === Structured error responses ===

  (describe "structured error responses"

    (it "path out of scope error includes required fields"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation :write "/tmp/file.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :path) :not :to-be nil)
        (expect (plist-get result :operation) :not :to-be nil)
        (expect (plist-get result :message) :not :to-be nil)))

    (it "path denied error includes required fields"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")
                            :deny '("/workspace/secrets/**")))
             (result (jf/gptel-scope--validate-operation :read "/workspace/secrets/key.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_denied")
        (expect (plist-get result :path) :not :to-be nil)
        (expect (plist-get result :operation) :not :to-be nil)
        (expect (plist-get result :message) :not :to-be nil)))

    (it "error message is a string"
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation :write "/tmp/file.txt" paths-config)))
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
      (let* ((paths-config (file-ops-spec--make-paths-config
                            :read '("/workspace/**" "/workspace/sub/**")))
             (result (jf/gptel-scope--validate-operation :read "/workspace/sub/file.txt" paths-config)))
        (expect result :to-be nil)))

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
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '("/workspace/**" "/tmp/**")
                          '() '() '() '())))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "cat /workspace/file.txt" '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "cat"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /workspace/file.txt" "/workspace" scope-config)))
          (expect result :to-be nil))
        (delete-file scope-yml)))

    (it "denies read operation outside scope via pipeline"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '("/workspace/**") '() '() '() '())))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "cat /etc/passwd" '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/etc/passwd" :command-name "cat"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /etc/passwd" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "path_out_of_scope")
          (expect (plist-get result :path) :to-equal "/etc/passwd"))
        (delete-file scope-yml)))

    (it "denies write when only read scope exists via pipeline"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '("/tmp/**") '() '() '() '())))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "echo test > /tmp/output.txt" '("echo") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :write "/tmp/output.txt" :command-name "echo"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "echo test > /tmp/output.txt" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "path_out_of_scope")
          (expect (plist-get result :operation) :to-equal :write))
        (delete-file scope-yml)))

    (it "denies operations on deny list paths via pipeline"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '("/workspace/**") '() '() '() '("**/.ssh/**"))))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "cat /workspace/.ssh/id_rsa" '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/.ssh/id_rsa" :command-name "cat"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /workspace/.ssh/id_rsa" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "path_denied")
          (expect (plist-get result :path) :to-equal "/workspace/.ssh/id_rsa"))
        (delete-file scope-yml)))

    (it "denies when multiple file operations include paths outside scope"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '("/workspace/**") '("/workspace/**") '() '() '())))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "cp /workspace/src.txt /tmp/dst.txt" '("cp") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/src.txt" :command-name "cp")
               (helpers-spec--make-file-op :write "/tmp/dst.txt" :command-name "cp"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cp /workspace/src.txt /tmp/dst.txt" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "path_out_of_scope")
          (expect (plist-get result :path) :to-equal "/tmp/dst.txt"))
        (delete-file scope-yml)))

    (it "allows multiple operations when all within scope"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '() '("/workspace/**") '() '() '())))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "cp /workspace/src.txt /workspace/dst.txt" '("cp") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/src.txt" :command-name "cp")
               (helpers-spec--make-file-op :write "/workspace/dst.txt" :command-name "cp"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cp /workspace/src.txt /workspace/dst.txt" "/workspace" scope-config)))
          (expect result :to-be nil))
        (delete-file scope-yml)))))

(provide 'file-operations-spec)
;;; file-operations-spec.el ends here
