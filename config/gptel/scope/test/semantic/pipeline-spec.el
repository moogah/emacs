;;; pipeline-spec.el --- Consolidated pipeline validation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CONSOLIDATED PIPELINE VALIDATION (Full 7-stage)
;;
;; Consolidates tests from:
;; - tools/test/integration/test-pipelines.el (36 ERT tests)
;; - tools/test/behavioral/run-bash-command/integration-scenarios-spec.el (10 Buttercup)
;;
;; Covers: Pipeline command extraction, deny list validation across pipelines,
;; security bypass prevention, structured errors with position, chained command
;; validation, multi-stage integration scenarios.
;;
;; Dedup strategy: ERT pipeline-specific tests merged with behavioral tests
;; that exercise the same multi-command validation paths. Unique security
;; scenarios and integration scenarios preserved.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'bash-parser-core)  ; For jf/bash-parse

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (semantic-dir test-dir)
       (scope-test-dir (expand-file-name ".." semantic-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))

;;; Local helpers (from ERT test-pipelines.el)

(defun pipeline-spec--make-categories ()
  "Create test bash_tools structure for pipeline validation tests.
Deny-list-only validation: deny: rm, mv, chmod, sh, bash, eval."
  (list :deny '("rm" "mv" "chmod" "sh" "bash" "eval")))

(defun pipeline-spec--extract-commands (command-string)
  "Helper: Parse COMMAND-STRING and extract pipeline commands.
Returns list of command names or nil for empty/invalid input."
  (when (and command-string (not (string-blank-p command-string)))
    (let ((parsed (jf/bash-parse command-string)))
      (jf/gptel-scope--extract-pipeline-commands parsed))))

;;; Test Suite

(describe "Pipeline validation"

  ;; === Command extraction ===

  (describe "pipeline command extraction"

    (it "extracts commands from pipe"
      (let ((commands (pipeline-spec--extract-commands "ls -la | grep foo")))
        (expect commands :to-equal '("ls" "grep"))))

    (it "extracts commands from multi-stage pipeline"
      (let ((commands (pipeline-spec--extract-commands "cat file.txt | grep pattern | head -10")))
        (expect commands :to-equal '("cat" "grep" "head"))))

    (it "extracts commands from semicolon chain"
      (let ((commands (pipeline-spec--extract-commands "cd /tmp; ls -la")))
        (expect commands :to-equal '("cd" "ls"))))

    (it "extracts commands from AND chain"
      (let ((commands (pipeline-spec--extract-commands "mkdir foo && cd foo")))
        (expect commands :to-equal '("mkdir" "cd"))))

    (it "extracts commands from OR chain"
      (let ((commands (pipeline-spec--extract-commands "test -f file.txt || touch file.txt")))
        (expect commands :to-equal '("test" "touch"))))

    (it "extracts command with arguments (ignores args)"
      (let ((commands (pipeline-spec--extract-commands "grep -rn 'pattern' .")))
        (expect commands :to-equal '("grep"))))

    (it "extracts git as base command"
      (let ((commands (pipeline-spec--extract-commands "git log --oneline")))
        (expect commands :to-equal '("git"))))

    (it "returns nil for empty string"
      (expect (pipeline-spec--extract-commands "") :to-be nil)
      (expect (pipeline-spec--extract-commands "   ") :to-be nil))

    (it "single command (no pipeline)"
      (let ((commands (pipeline-spec--extract-commands "ls -la")))
        (expect commands :to-equal '("ls")))))

  ;; === Deny list validation ===

  (describe "pipeline deny list validation"

    (it "all commands allowed passes"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("ls" "grep") categories)))
        (expect result :to-be nil)))

    (it "second command denied"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("ls" "xargs" "rm") categories)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 2)))

    (it "middle command denied"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("cat" "sh" "head") categories)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "sh")
        (expect (plist-get result :position) :to-equal 1)))

    (it "first command denied reports position 0"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("rm" "ls") categories)))
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 0)))

    (it "empty command list passes"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '() categories)))
        (expect result :to-be nil)))

    (it "four-stage pipeline all allowed"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("cat" "grep" "head" "wc") categories)))
        (expect result :to-be nil)))

    (it "command not in deny list passes"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("unknown-command") categories)))
        (expect result :to-be nil)))

    (it "empty deny list allows everything"
      (let* ((categories (list :deny '()))
             (result (jf/gptel-scope--validate-pipeline-commands '("rm" "sudo" "chmod") categories)))
        (expect result :to-be nil)))

    (it "fails at first denied command in mixed pipeline"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("ls" "grep" "rm" "head") categories)))
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 2)))

    (it "multiple denied commands fails at first"
      (let* ((categories (list :deny '("rm" "sudo" "chmod")))
             (result (jf/gptel-scope--validate-pipeline-commands '("ls" "rm" "sudo" "chmod") categories)))
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 1))))

  ;; === Security bypass prevention ===

  (describe "security bypass prevention"

    (it "prevents xargs rm bypass"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("find" "xargs" "rm") categories)))
        (expect result :not :to-be nil)
        (expect (member (plist-get result :command) '("xargs" "rm")) :not :to-be nil)))

    (it "prevents sh execution bypass"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("grep" "sh") categories)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "sh")))

    (it "prevents chmod bypass"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("find" "xargs" "chmod") categories)))
        (expect result :not :to-be nil)))

    (it "prevents curl pipe sh"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("curl" "sh") categories)))
        (expect result :not :to-be nil)))

    (it "prevents cat eval"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("cat" "eval") categories)))
        (expect (plist-get result :command) :to-equal "eval")))

    (it "allows legitimate data pipeline"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("cat" "grep" "head" "wc") categories)))
        (expect result :to-be nil))))

  ;; === Structured errors ===

  (describe "structured error responses"

    (it "error identifies command and position"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("ls" "rm") categories)))
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 1)))

    (it "error includes message"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("ls" "rm") categories)))
        (expect (stringp (plist-get result :message)) :to-be t)
        (expect (string-empty-p (plist-get result :message)) :to-be nil)))

    (it "third command failure reports position 2"
      (let* ((categories (pipeline-spec--make-categories))
             (result (jf/gptel-scope--validate-pipeline-commands '("ls" "grep" "rm") categories)))
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 2))))

  ;; === Full pipeline behavioral tests (from integration-scenarios-spec.el) ===

  (describe "full pipeline behavioral validation"

    (before-each
      (helpers-spec-setup-session)
      (helpers-spec-setup-bash-mocks))

    (after-each
      (helpers-spec-teardown-bash-mocks)
      (helpers-spec-teardown-session))

    (it "pipeline with mixed operations succeeds"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '() '("/workspace/**") '() '() '())))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "cat /workspace/input.txt | grep pattern | head -10 > /workspace/output.txt"
         '("cat" "grep" "head" "bash-redirection") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/input.txt" :command-name "cat")
               (helpers-spec--make-file-op :write "/workspace/output.txt" :command-name "bash-redirection"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /workspace/input.txt | grep pattern | head -10 > /workspace/output.txt"
                       "/workspace" scope-config)))
          (expect result :to-be nil))
        (delete-file scope-yml)))

    (it "pipeline with denied command fails at stage 3"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny:
    - rm

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "find /workspace -name \"*.tmp\" | xargs rm | wc -l"
         '("find" "xargs" "rm" "wc") t)
        (helpers-spec-mock-bash-semantics '() nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "find /workspace -name \"*.tmp\" | xargs rm | wc -l"
                       "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "command_denied")
          (expect (plist-get result :command) :to-equal "rm")
          (expect (plist-get result :position) :to-equal 2))
        (delete-file scope-yml)))

    (it "command substitution with execute operation denied"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '("/workspace/**") '() '() '() '())))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "cat $(find /workspace/docs -name README.md) | python3 /workspace/scripts/process.py"
         '("find" "cat" "python3") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/docs" :command-name "find")
               (helpers-spec--make-file-op :read "/workspace/README.md" :command-name "cat")
               (helpers-spec--make-file-op :execute "/workspace/scripts/process.py" :command-name "python3"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat $(find /workspace/docs -name README.md) | python3 /workspace/scripts/process.py"
                       "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "path_out_of_scope")
          (expect (plist-get result :operation) :to-equal :execute))
        (delete-file scope-yml)))

    (it "no-op in deny list fails before no-op check"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny:
    - sudo

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse "sudo --version" '("sudo") t)
        (helpers-spec-mock-bash-semantics '() nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "sudo --version" "/workspace" scope-config)))
          (expect (plist-get result :error) :to-equal "command_denied")
          (expect (plist-get result :command) :to-equal "sudo"))
        (delete-file scope-yml)))

    (it "modify operation allowed via write scope (hierarchical)"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '() '("/workspace/**") '() '() '())))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "sed -i \"s/foo/bar/\" /workspace/config.yml"
         '("sed") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :modify "/workspace/config.yml" :command-name "sed"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "sed -i \"s/foo/bar/\" /workspace/config.yml"
                       "/workspace" scope-config)))
          (expect result :to-be nil))
        (delete-file scope-yml)))

    (it "deny pattern overrides broad permissions"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '("/**") '() '() '()
                          '("~/.ssh/**" "**/.ssh/**"))))
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

    (it "complete successful execution (validation phase)"
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         (helpers-spec--scope-with-paths
                          '("/workspace/**") '() '() '() '())))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        (helpers-spec-mock-bash-parse
         "ls -la /workspace/src" '("ls") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/src" :command-name "ls"))
         nil '(:ratio 1.0))
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "ls -la /workspace/src" "/workspace" scope-config)))
          (expect result :to-be nil))
        (delete-file scope-yml)))))

(provide 'pipeline-spec)
;;; pipeline-spec.el ends here
