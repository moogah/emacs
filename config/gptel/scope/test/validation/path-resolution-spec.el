;;; path-resolution-spec.el --- Working directory and path resolution tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; WORKING DIRECTORY AND PATH RESOLUTION (Stage 4)
;;
;; Tests the working directory and path resolution validation stage of the
;; run_bash_command seven-stage validation pipeline.
;;
;; This stage handles resolution of relative paths from the working directory
;; argument, symlink resolution, and path normalization for validation against
;; scope patterns.
;;
;; Key behaviors tested:
;; - Relative paths (./file, ../other/file) resolved from directory argument
;; - Parent directory navigation (..) resolved correctly
;; - Path traversal attacks detected and denied (../../etc/passwd)
;; - Current directory (.) resolved to directory argument
;; - Denied paths block operations even when accessed via current directory
;; - Symlink directories resolved to real paths for validation
;; - Absolute paths bypass directory context but still validated
;; - Multiple paths in same command resolved independently
;; - Working directory passed to command execution
;;
;; Test naming convention: describe "Category: <scenario-name>"
;; Each test validates a complete interaction flow through stage 4.

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

;;; Test Suite

(describe "run_bash_command: Working directory and path resolution"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "relative paths resolved from directory argument"
    ;; Test: Relative paths resolved from directory argument
    ;; Command: 'cat ./README.md'
    ;; Directory: '/workspace'
    ;; Mock semantics: Return file-op {:operation :read :path './README.md'}
    ;; Expected resolution: /workspace/README.md
    ;; Setup: Scope with paths.read: ['/workspace/**']
    ;; Assert: Success (relative path resolved correctly)
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**"))))
      (helpers-spec-mock-bash-parse
       "cat ./README.md" '("cat") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "./README.md" :command "cat"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat ./README.md" "/workspace" scope-config)))
        (expect (plist-get result :error) :to-be nil))))

  (it "dot-dot paths resolved from directory"
    ;; Test: Dot-dot paths resolved from directory
    ;; Setup: paths.read: ['/workspace/**']; resolves /workspace/project/../other/file.txt
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**"))))
      (helpers-spec-mock-bash-parse
       "cat ../other/file.txt" '("cat") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "../other/file.txt" :command "cat"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat ../other/file.txt" "/workspace/project" scope-config)))
        (expect (plist-get result :error) :to-be nil))))

  (it "path traversal detected and denied"
    ;; Setup: paths.read: ['/workspace/**'], deny: ['/etc/**']
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :deny '("/etc/**"))))
      (helpers-spec-mock-bash-parse
       "cat ../../etc/passwd" '("cat") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "../../etc/passwd" :command "cat"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat ../../etc/passwd" "/workspace/project" scope-config)))
        (expect (plist-get result :error) :to-equal "denied-pattern"))))

  (it "current directory (.) resolved"
    ;; Setup: paths.read: ['/workspace', '/workspace/**']
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace" "/workspace/**"))))
      (helpers-spec-mock-bash-parse
       "ls ." '("ls") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "." :command "ls"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls ." "/workspace" scope-config)))
        (expect (plist-get result :error) :to-be nil))))

  (it "directory itself in deny list blocks operation"
    ;; Setup: paths.read: ['/**'], deny: ['/etc', '/etc/**']
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/**")
                         :deny '("/etc" "/etc/**"))))
      (helpers-spec-mock-bash-parse
       "ls ." '("ls") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "." :command "ls"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls ." "/etc" scope-config)))
        (expect (plist-get result :error) :to-equal "denied-pattern"))))

  (it "symlink directory resolved (mock file-truename)"
    ;; Setup: paths.read: ['/workspace/**', '/home/user/projects/**']
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**" "/home/user/projects/**")))
          (original-file-truename (symbol-function 'file-truename)))
      (unwind-protect
          (progn
            (fset 'file-truename
                  (lambda (path)
                    (if (string-prefix-p "/workspace" path)
                        (replace-regexp-in-string "^/workspace" "/home/user/projects/app" path)
                      (funcall original-file-truename path))))
            (helpers-spec-mock-bash-parse
             "cat file.txt" '("cat") t)
            (helpers-spec-mock-bash-semantics
             (list (helpers-spec--make-file-op :read "file.txt" :command "cat"))
             nil
             '(:ratio 1.0))
            (let ((result (jf/gptel-scope--validate-command-semantics
                           "cat file.txt" "/workspace" scope-config)))
              (expect (plist-get result :error) :to-be nil)))
        (fset 'file-truename original-file-truename))))

  (it "absolute path in command bypasses directory context"
    ;; Setup: paths.read: ['/workspace/**'], deny: ['/etc/**']
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :deny '("/etc/**"))))
      (helpers-spec-mock-bash-parse
       "cat /etc/passwd" '("cat") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/etc/passwd" :command "cat"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /etc/passwd" "/workspace" scope-config)))
        (expect (plist-get result :error) :to-equal "denied-pattern"))))

  (it "multiple relative paths resolved independently"
    ;; Setup: paths.read: ['/workspace/src/**'], write: ['/workspace/dst/**']
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/src/**")
                         :write '("/workspace/dst/**"))))
      (helpers-spec-mock-bash-parse
       "cp ./src/file.txt ./dst/file.txt" '("cp") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "./src/file.txt" :command "cp")
             (helpers-spec--make-file-op :write "./dst/file.txt" :command "cp"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cp ./src/file.txt ./dst/file.txt" "/workspace" scope-config)))
        (expect (plist-get result :error) :to-be nil))))

  (it "working directory provided to command execution"
    ;; Test: Working directory provided to command execution
    ;; Command: 'pwd'
    ;; Directory: '/tmp/test-dir'
    ;; Mock parse: No deny list issues
    ;; Mock semantics: No file ops (no-op)
    ;; Spy on call-process to capture default-directory
    ;; Assert: call-process invoked with default-directory='/tmp/test-dir/' (or resolved symlink)
    (let ((captured-directory nil))
      ;; Spy on call-process to capture default-directory
      (spy-on 'call-process
              :and-call-fake
              (lambda (&rest args)
                (setq captured-directory default-directory)
                0)) ; Return success exit code

      ;; Execute command directly (no scope validation needed for this test)
      (jf/gptel-bash--execute-command "pwd" "/tmp/test-dir")

      ;; Assert: default-directory was set correctly
      ;; Note: On macOS, /tmp -> /private/tmp, so check both possibilities
      ;; The implementation uses file-truename which resolves symlinks
      (let ((expected-dirs (list "/tmp/test-dir/"
                                 "/private/tmp/test-dir/"
                                 "/tmp/test-dir"
                                 "/private/tmp/test-dir")))
        (expect (member captured-directory expected-dirs) :not :to-be nil)))))

(provide 'path-resolution-spec)
;;; path-resolution-spec.el ends here
