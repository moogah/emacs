;;; operation-specific-paths-spec.el --- Operation-specific path validation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; OPERATION-SPECIFIC PATH VALIDATION (Stage 6)
;;
;; Tests the operation-specific path validation stage of the run_bash_command
;; seven-stage validation pipeline.
;;
;; Stage 6 validates that file operations on specific paths are allowed by
;; the scope configuration. Validates that read/write/execute/modify operations
;; are matched against their respective scope lists, with hierarchical permission
;; inheritance (write scope allows read, write scope allows modify).
;;
;; Key behaviors tested:
;; - Read operations allowed in read scope
;; - Read operations allowed in write scope (hierarchical)
;; - Read operations denied outside scope
;; - Write operations require write scope
;; - Write operations denied with read-only scope
;; - Execute operations require execute scope
;; - Modify operations require modify scope
;; - Modify operations allowed in write scope (hierarchical)
;; - Deny list paths take precedence over allow patterns
;; - Multiple file operations validated independently
;; - Multiple operations succeed when all within scope
;;
;; Test naming convention: describe "Category: <scenario-name>"
;; Each test validates a complete interaction flow through stage 6.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-parent-dir (expand-file-name ".." test-dir))
       (test-root-dir (expand-file-name ".." test-parent-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Test Suite

(describe "run_bash_command: Operation-specific path validation (stage 6)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "allows read operation within read scope"
    ;; Test: Read operation allowed in paths.read scope
    ;; Setup: Scope with paths.read: ['/workspace/**', '/tmp/**']
    ;; Command: 'cat /workspace/file.txt'
    ;; Mock semantics: Return file-op {:operation :read :path '/workspace/file.txt'}
    ;; Assert: Success (read operation matches read scope)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**" "/tmp/**")  ; read
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'cat' command
      (helpers-spec-mock-bash-parse
       "cat /workspace/file.txt"
       '("cat")
       t)

      ;; Mock semantics: Read operation on /workspace/file.txt
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "cat"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /workspace/file.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success (nil = no error)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows read operation within write scope (hierarchical permissions)"
    ;; Test: Read operation allowed in paths.write scope (hierarchical permissions)
    ;; Setup: Scope with paths.write: ['/workspace/**']
    ;; Command: 'cat /workspace/output.txt'
    ;; Mock semantics: Return file-op {:operation :read :path '/workspace/output.txt'}
    ;; Assert: Success (write scope includes read capability)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '()  ; read
                        '("/workspace/**")  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'cat' command
      (helpers-spec-mock-bash-parse
       "cat /workspace/output.txt"
       '("cat")
       t)

      ;; Mock semantics: Read operation on /workspace/output.txt
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/output.txt" :command-name "cat"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /workspace/output.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success (write scope includes read)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies read operation outside scope"
    ;; Test: Read operation denied outside scope
    ;; Setup: Scope with paths.read: ['/workspace/**']
    ;; Command: 'cat /etc/passwd'
    ;; Mock semantics: Return file-op {:operation :read :path '/etc/passwd'}
    ;; Assert: :error 'path_out_of_scope', :path '/etc/passwd', :operation :read
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**")  ; read
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'cat' command
      (helpers-spec-mock-bash-parse
       "cat /etc/passwd"
       '("cat")
       t)

      ;; Mock semantics: Read operation on /etc/passwd
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/etc/passwd" :command-name "cat"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /etc/passwd"
                     "/workspace"
                     scope-config)))
        ;; Assert: path_out_of_scope error
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :path) :to-equal "/etc/passwd")
        (expect (plist-get result :operation) :to-equal :read))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies write operation when only read scope exists"
    ;; Test: Write operation requires paths.write
    ;; Setup: Scope with paths.read: ['/tmp/**'] (read only, no write)
    ;; Command: 'echo test > /tmp/output.txt'
    ;; Mock semantics: Return file-op {:operation :write :path '/tmp/output.txt'}
    ;; Assert: :error 'path_out_of_scope' (write requires write scope)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/tmp/**")  ; read
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'echo' command
      (helpers-spec-mock-bash-parse
       "echo test > /tmp/output.txt"
       '("echo")
       t)

      ;; Mock semantics: Write operation on /tmp/output.txt
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :write "/tmp/output.txt" :command-name "echo"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "echo test > /tmp/output.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: path_out_of_scope error (write not allowed)
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :operation) :to-equal :write))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows write operation within write scope"
    ;; Test: Write operation allowed in paths.write
    ;; Setup: Scope with paths.write: ['/workspace/**']
    ;; Command: 'mkdir /workspace/newdir'
    ;; Mock semantics: Return file-op {:operation :write :path '/workspace/newdir'}
    ;; Assert: Success
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '()  ; read
                        '("/workspace/**")  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'mkdir' command
      (helpers-spec-mock-bash-parse
       "mkdir /workspace/newdir"
       '("mkdir")
       t)

      ;; Mock semantics: Write operation on /workspace/newdir
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :write "/workspace/newdir" :command-name "mkdir"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "mkdir /workspace/newdir"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies execute operation without execute scope"
    ;; Test: Execute operation requires paths.execute
    ;; Setup: Scope with paths.read: ['/workspace/**'], no paths.execute
    ;; Command: 'bash /workspace/scripts/deploy.sh'
    ;; Mock semantics: Return file-op {:operation :execute :path '/workspace/scripts/deploy.sh'}
    ;; Assert: :error 'path_out_of_scope', :operation :execute
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**")  ; read
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'bash' command
      (helpers-spec-mock-bash-parse
       "bash /workspace/scripts/deploy.sh"
       '("bash")
       t)

      ;; Mock semantics: Execute operation on /workspace/scripts/deploy.sh
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :execute "/workspace/scripts/deploy.sh" :command-name "bash"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "bash /workspace/scripts/deploy.sh"
                     "/workspace"
                     scope-config)))
        ;; Assert: path_out_of_scope error (execute not allowed)
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :operation) :to-equal :execute))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows execute operation within execute scope"
    ;; Test: Execute operation allowed in paths.execute scope
    ;; Setup: Scope with paths.execute: ['/workspace/scripts/**']
    ;; Command: 'python3 /workspace/scripts/deploy.py'
    ;; Mock semantics: Return file-op {:operation :execute :path '/workspace/scripts/deploy.py'}
    ;; Assert: Success
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '()  ; read
                        '()  ; write
                        '("/workspace/scripts/**")  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'python3' command
      (helpers-spec-mock-bash-parse
       "python3 /workspace/scripts/deploy.py"
       '("python3")
       t)

      ;; Mock semantics: Execute operation on /workspace/scripts/deploy.py
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :execute "/workspace/scripts/deploy.py" :command-name "python3"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "python3 /workspace/scripts/deploy.py"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies modify operation without modify scope"
    ;; Test: Modify operation requires paths.modify
    ;; Setup: Scope with paths.read: ['/workspace/**'], no paths.modify
    ;; Command: 'sed -i "s/foo/bar/" /workspace/config.yml'
    ;; Mock semantics: Return file-op {:operation :modify :path '/workspace/config.yml'}
    ;; Assert: :error 'path_out_of_scope', :operation :modify
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**")  ; read
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'sed' command
      (helpers-spec-mock-bash-parse
       "sed -i \"s/foo/bar/\" /workspace/config.yml"
       '("sed")
       t)

      ;; Mock semantics: Modify operation on /workspace/config.yml
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :modify "/workspace/config.yml" :command-name "sed"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "sed -i \"s/foo/bar/\" /workspace/config.yml"
                     "/workspace"
                     scope-config)))
        ;; Assert: path_out_of_scope error (modify not allowed)
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :operation) :to-equal :modify))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows modify operation within modify scope"
    ;; Test: Modify operation allowed in paths.modify scope
    ;; Setup: Scope with paths.modify: ['/workspace/config/**']
    ;; Command: 'sed -i "s/foo/bar/" /workspace/config/app.yml'
    ;; Mock semantics: Return file-op {:operation :modify :path '/workspace/config/app.yml'}
    ;; Assert: Success
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '()  ; read
                        '()  ; write
                        '()  ; execute
                        '("/workspace/config/**")  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'sed' command
      (helpers-spec-mock-bash-parse
       "sed -i \"s/foo/bar/\" /workspace/config/app.yml"
       '("sed")
       t)

      ;; Mock semantics: Modify operation on /workspace/config/app.yml
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :modify "/workspace/config/app.yml" :command-name "sed"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "sed -i \"s/foo/bar/\" /workspace/config/app.yml"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows modify operation within write scope (hierarchical permissions)"
    ;; Test: Modify operation allowed in paths.write scope (hierarchical)
    ;; Setup: Scope with paths.write: ['/workspace/**']
    ;; Command: 'sed -i "s/foo/bar/" /workspace/data.txt'
    ;; Mock semantics: Return file-op {:operation :modify :path '/workspace/data.txt'}
    ;; Assert: Success (write scope includes modify capability)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '()  ; read
                        '("/workspace/**")  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'sed' command
      (helpers-spec-mock-bash-parse
       "sed -i \"s/foo/bar/\" /workspace/data.txt"
       '("sed")
       t)

      ;; Mock semantics: Modify operation on /workspace/data.txt
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :modify "/workspace/data.txt" :command-name "sed"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "sed -i \"s/foo/bar/\" /workspace/data.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success (write scope includes modify)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies operations on deny list paths regardless of allow patterns"
    ;; Test: Deny patterns override all allow patterns
    ;; Setup: Scope with paths.read: ['/workspace/**'], paths.deny: ['~/.ssh/**']
    ;; Command: 'cat /workspace/.ssh/id_rsa'
    ;; Mock semantics: Return file-op {:operation :read :path '/workspace/.ssh/id_rsa'}
    ;; Assert: :error 'path_denied' (deny takes precedence over read)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**")  ; read
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '("**/.ssh/**")))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'cat' command
      (helpers-spec-mock-bash-parse
       "cat /workspace/.ssh/id_rsa"
       '("cat")
       t)

      ;; Mock semantics: Read operation on /workspace/.ssh/id_rsa
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/.ssh/id_rsa" :command-name "cat"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /workspace/.ssh/id_rsa"
                     "/workspace"
                     scope-config)))
        ;; Assert: path_denied error (deny overrides read)
        (expect (plist-get result :error) :to-equal "path_denied")
        (expect (plist-get result :path) :to-equal "/workspace/.ssh/id_rsa"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies when multiple file operations include paths outside scope"
    ;; Test: Multiple file operations validated independently
    ;; Command: 'cp /workspace/src.txt /tmp/dst.txt'
    ;; Mock semantics: Return two file-ops:
    ;;   * {:operation :read :path '/workspace/src.txt'}
    ;;   * {:operation :write :path '/tmp/dst.txt'}
    ;; Setup: Scope with paths.read: ['/workspace/**'], paths.write: ['/workspace/**']
    ;; Assert: :error 'path_out_of_scope' for dst.txt (write outside write scope)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**")  ; read
                        '("/workspace/**")  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'cp' command
      (helpers-spec-mock-bash-parse
       "cp /workspace/src.txt /tmp/dst.txt"
       '("cp")
       t)

      ;; Mock semantics: Read src.txt, write dst.txt
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/src.txt" :command-name "cp")
             (helpers-spec--make-file-op :write "/tmp/dst.txt" :command-name "cp"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cp /workspace/src.txt /tmp/dst.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: path_out_of_scope error for /tmp/dst.txt
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :path) :to-equal "/tmp/dst.txt")
        (expect (plist-get result :operation) :to-equal :write))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows multiple operations when all are within scope"
    ;; Test: Multiple operations all in scope succeed
    ;; Command: 'cp /workspace/src.txt /workspace/dst.txt'
    ;; Mock semantics: Return two file-ops (read src, write dst)
    ;; Setup: Scope with paths.write: ['/workspace/**']
    ;; Assert: Success (write scope includes read, both operations allowed)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '()  ; read
                        '("/workspace/**")  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'cp' command
      (helpers-spec-mock-bash-parse
       "cp /workspace/src.txt /workspace/dst.txt"
       '("cp")
       t)

      ;; Mock semantics: Read src.txt, write dst.txt
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/src.txt" :command-name "cp")
             (helpers-spec--make-file-op :write "/workspace/dst.txt" :command-name "cp"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cp /workspace/src.txt /workspace/dst.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success (both operations allowed)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml))))

(provide 'operation-specific-paths-spec)
;;; operation-specific-paths-spec.el ends here
