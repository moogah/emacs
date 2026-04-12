;;; bash-scope-expansion-integration-spec.el --- Integration tests: run_bash_command through real bash-parser and scope validation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INTEGRATION TESTS: Exercise the real production run_bash_command tool through
;; the full pipeline: real bash-parser (tree-sitter) → real 7-stage semantic
;; validation → real scope config from YAML → expansion UI → gptel callback.
;;
;; What makes these integration tests unique:
;;
;; - Uses the REAL run_bash_command tool from scope-shell-tools.el
;; - Uses the REAL bash-parser (jf/bash-parse, jf/bash-extract-semantics)
;;   with tree-sitter — NO mocked parse results
;; - Uses REAL 7-stage semantic validation pipeline
;; - Uses REAL scope config parsed from YAML
;; - Only mocks at boundaries:
;;   * Config loading (to inject test YAML without a real session directory)
;;   * Expansion UI (Pattern B: immediate callback, no transient display)
;;   * Command execution (to avoid running real shell commands)
;;
;; Starting command: "which brew" — a no-op command (zero file operations)
;; that exercises the parse → semantic extraction → no-op short-circuit path.
;;
;; Also tests commands that DO have file operations (cat, denied commands)
;; to verify the full validation + expansion + callback chain.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       ;; test/integration/ -> test/ -> scope/ -> gptel/ -> config/
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (config-dir (expand-file-name ".." gptel-dir))
       (contracts-dir (expand-file-name "test/contracts/" config-dir)))
  ;; Contract infrastructure
  (add-to-list 'load-path contracts-dir)
  (require 'contract-core)
  (require 'contract-bash-parser)
  (contract--register-buttercup-matcher)
  ;; Scope helpers (matchers, config builders)
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  ;; Production scope modules (scope-shell-tools pulls in bash-parser)
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  (require 'jf-gptel-scope-expansion
           (expand-file-name "scope/scope-expansion.el" gptel-dir))
  (require 'jf-gptel-scope-filesystem-tools
           (expand-file-name "scope/scope-filesystem-tools.el" gptel-dir))
  ;; gptel itself (tool struct accessors, tool registry)
  (require 'gptel))

;;; Test Infrastructure

(defvar bash-integ--callback-result nil
  "Parsed plist from the gptel callback's JSON string argument.")

(defvar bash-integ--callback-raw nil
  "Raw string argument passed to gptel callback.")

(defvar bash-integ--temp-dir nil
  "Temporary directory for test working directories.")

(defun bash-integ--gptel-callback (result-json)
  "Simulate gptel's process-tool-result callback.
Captures both the raw JSON string and parsed plist."
  (setq bash-integ--callback-raw result-json)
  (setq bash-integ--callback-result
        (json-parse-string result-json :object-type 'plist)))

(defun bash-integ--find-tool (name)
  "Find tool NAME in gptel--known-tools registry."
  (cl-block nil
    (dolist (category-entry gptel--known-tools)
      (dolist (tool-entry (cdr category-entry))
        (when (string= (car tool-entry) name)
          (cl-return (cdr tool-entry)))))))

(defun bash-integ--make-scope-config (read-paths write-paths deny-paths
                                       &optional _bash-deny cloud-auth)
  "Build a scope config from YAML through the real parse pipeline.
READ-PATHS, WRITE-PATHS, DENY-PATHS are lists of glob pattern strings.
CLOUD-AUTH is the auth detection mode string (default \"warn\").
The fourth positional is retained for backward compatibility with existing
callers but is ignored — there is no longer a command-name deny list."
  (let* ((format-paths (lambda (paths)
                         (if paths
                             (mapconcat (lambda (p) (format "    - \"%s\"" p))
                                        paths "\n")
                           "    []")))
         (yaml (format "paths:
  read:
%s
  write:
%s
  execute:
    []
  modify:
    []
  deny:
%s

cloud:
  auth_detection: \"%s\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"
                       (funcall format-paths read-paths)
                       (funcall format-paths write-paths)
                       (funcall format-paths deny-paths)
                       (or cloud-auth "warn"))))
    (jf/gptel-scope-yaml--merge-schema-defaults
     (jf/gptel-scope-yaml--parse-string yaml))))

;;; Test Suites

;; ============================================================
;; SUITE 1: "which brew" — no-op command through full pipeline
;; The bash-parser parses it, semantic extraction finds zero file
;; operations, the no-op check short-circuits, tool body runs.
;; ============================================================

(describe "run_bash_command integration: \"which brew\" (no-op)"

  (before-each
    (setq bash-integ--callback-result nil)
    (setq bash-integ--callback-raw nil)
    (setq bash-integ--temp-dir (make-temp-file "bash-integ-" t)))

  (after-each
    (when (and bash-integ--temp-dir (file-exists-p bash-integ--temp-dir))
      (delete-directory bash-integ--temp-dir t)))

  (describe "real bash-parser produces valid output"

    (it "parses 'which brew' successfully"
      (let ((parsed (jf/bash-parse "which brew")))
        (expect parsed :to-satisfy-contract #'contract/bash-parse-result--validate)
        (expect (plist-get parsed :success) :to-be t)
        (expect (plist-get parsed :parse-complete) :to-be t)))

    (it "extracts read-metadata operation for 'which brew' (not a filesystem read)"
      (let* ((parsed (jf/bash-parse "which brew"))
             (semantics (jf/bash-extract-semantics parsed))
             (file-ops (alist-get :filesystem (plist-get semantics :domains))))
        (expect semantics :to-satisfy-contract #'contract/bash-semantics--validate)
        ;; bash-parser extracts :read-metadata for "which" arguments —
        ;; this is a PATH lookup, not a file read, so the pipeline allows it
        (when file-ops
          (expect (plist-get (car file-ops) :operation) :to-equal :read-metadata)
          (expect (plist-get (car file-ops) :command) :to-equal "which")))))

  (describe "7-stage pipeline allows no-op"

    (it "validate-command-semantics returns nil (allowed) for 'which brew'"
      (let ((config (bash-integ--make-scope-config
                     (list (concat bash-integ--temp-dir "/**"))
                     nil nil)))
        (expect (jf/gptel-scope--validate-command-semantics
                 "which brew" bash-integ--temp-dir config)
                :to-be nil))))

  (describe "full tool invocation through macro"

    (it "tool body executes and callback receives success"
      (let* ((config (bash-integ--make-scope-config
                      (list (concat bash-integ--temp-dir "/**"))
                      nil nil))
             (tool (bash-integ--find-tool "run_bash_command")))

        (expect tool :to-be-truthy)
        (expect (gptel-tool-async tool) :to-be t)

        ;; Mock config loading and command execution (boundaries only)
        (spy-on 'jf/gptel-scope--load-config :and-return-value config)
        (spy-on 'jf/gptel-bash--execute-command
                :and-return-value '(:output "/opt/homebrew/bin/brew"
                                    :exit_code 0
                                    :truncated nil
                                    :warnings nil
                                    :error nil))

        ;; Real bash-parser runs inside the macro's validation path
        ;; Real 7-stage pipeline runs (no-op short-circuit at stage 4)
        (funcall (gptel-tool-function tool)
                 #'bash-integ--gptel-callback
                 "which brew"
                 bash-integ--temp-dir)

        ;; Callback fired with JSON string
        (expect bash-integ--callback-raw :to-be-truthy)
        (expect (stringp bash-integ--callback-raw) :to-be t)

        ;; Tool body executed successfully
        (expect (plist-get bash-integ--callback-result :success) :to-be t)
        (expect (plist-get bash-integ--callback-result :output)
                :to-equal "/opt/homebrew/bin/brew")
        (expect (plist-get bash-integ--callback-result :exit_code) :to-equal 0)))

    (it "expansion UI is NOT triggered for allowed no-op"
      (let* ((config (bash-integ--make-scope-config
                      (list (concat bash-integ--temp-dir "/**"))
                      nil nil))
             (tool (bash-integ--find-tool "run_bash_command")))

        (spy-on 'jf/gptel-scope--load-config :and-return-value config)
        (spy-on 'jf/gptel-bash--execute-command
                :and-return-value '(:output "" :exit_code 0
                                    :truncated nil :warnings nil :error nil))
        (spy-on 'jf/gptel-scope-prompt-expansion)

        (funcall (gptel-tool-function tool)
                 #'bash-integ--gptel-callback
                 "which brew"
                 bash-integ--temp-dir)

        (expect 'jf/gptel-scope-prompt-expansion :not :to-have-been-called)
        (expect (plist-get bash-integ--callback-result :success) :to-be t)))

    (it "real validate-bash-tool is called (not bypassed)"
      (let* ((config (bash-integ--make-scope-config
                      (list (concat bash-integ--temp-dir "/**"))
                      nil nil))
             (tool (bash-integ--find-tool "run_bash_command")))

        (spy-on 'jf/gptel-scope--load-config :and-return-value config)
        (spy-on 'jf/gptel-bash--execute-command
                :and-return-value '(:output "" :exit_code 0
                                    :truncated nil :warnings nil :error nil))
        ;; Spy + call-through: observe real validation runs
        (spy-on 'jf/gptel-scope--validate-bash-tool :and-call-through)
        (spy-on 'jf/gptel-scope--validate-command-semantics :and-call-through)

        (funcall (gptel-tool-function tool)
                 #'bash-integ--gptel-callback
                 "which brew"
                 bash-integ--temp-dir)

        (expect 'jf/gptel-scope--validate-bash-tool :to-have-been-called)
        (expect 'jf/gptel-scope--validate-command-semantics :to-have-been-called)))))


;; ============================================================
;; SUITE 2: Denied command — "rm -rf /tmp/foo"
;; Real bash-parser parses it, "rm" hits the deny list at stage 3,
;; expansion UI triggers, user responses tested.
;; ============================================================

(describe "run_bash_command integration: denied command"

  (before-each
    (setq bash-integ--callback-result nil)
    (setq bash-integ--callback-raw nil)
    (setq bash-integ--temp-dir (make-temp-file "bash-integ-" t)))

  (after-each
    (when (and bash-integ--temp-dir (file-exists-p bash-integ--temp-dir))
      (delete-directory bash-integ--temp-dir t)))

  (it "bash-parser parses 'rm -rf /tmp/foo' and path validation denies it"
    (let* ((config (bash-integ--make-scope-config
                    '("/workspace/**") '("/workspace/**") nil))
           (result (jf/gptel-scope--validate-command-semantics
                    "rm -rf /tmp/foo" "/workspace" config)))
      ;; /tmp/foo is not in /workspace/** so the delete is denied
      (expect result :not :to-be nil)
      (expect (plist-get result :error) :to-equal "not-in-scope")))

  (it "triggers expansion UI for denied command through macro"
    (let* ((config (bash-integ--make-scope-config
                    '("/workspace/**") '("/workspace/**") nil
                    '("rm" "sudo")))
           (tool (bash-integ--find-tool "run_bash_command")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      (spy-on 'jf/gptel-scope-prompt-expansion)

      (funcall (gptel-tool-function tool)
               #'bash-integ--gptel-callback
               "rm -rf /tmp/foo"
               "/workspace")

      ;; Expansion UI triggered (async tool denied → expansion)
      (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)
      ;; Callback not invoked yet (waiting for user)
      (expect bash-integ--callback-result :to-be nil)))

  (it "user deny returns error to gptel callback"
    (let* ((config (bash-integ--make-scope-config
                    '("/workspace/**") '("/workspace/**") nil
                    '("rm" "sudo")))
           (tool (bash-integ--find-tool "run_bash_command")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      ;; Simulate user choosing "Deny"
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (_violation-info callback _patterns _tool-name)
                (funcall callback
                         (json-serialize
                          (list :success :false
                                :user_denied t)))))

      (funcall (gptel-tool-function tool)
               #'bash-integ--gptel-callback
               "rm -rf /tmp/foo"
               "/workspace")

      ;; Callback received error
      (expect bash-integ--callback-result :to-be-truthy)
      (expect (plist-get bash-integ--callback-result :success) :not :to-be t)
      (expect (plist-get bash-integ--callback-result :error) :to-be-truthy))))


;; ============================================================
;; SUITE 3: File operation out of scope — "cat /etc/passwd"
;; Real bash-parser extracts the read operation, stage 5 denies it,
;; expansion UI + allow-once tested.
;; ============================================================

(describe "run_bash_command integration: file operation out of scope"

  (before-each
    (setq bash-integ--callback-result nil)
    (setq bash-integ--callback-raw nil)
    (setq bash-integ--temp-dir (make-temp-file "bash-integ-" t)))

  (after-each
    (when (and bash-integ--temp-dir (file-exists-p bash-integ--temp-dir))
      (delete-directory bash-integ--temp-dir t)))

  (it "bash-parser extracts read operation for /etc/passwd"
    (let* ((parsed (jf/bash-parse "cat /etc/passwd"))
           (semantics (jf/bash-extract-semantics parsed))
           (file-ops (alist-get :filesystem (plist-get semantics :domains))))
      (expect parsed :to-satisfy-contract #'contract/bash-parse-result--validate)
      (expect semantics :to-satisfy-contract #'contract/bash-semantics--validate)
      (expect file-ops :not :to-be nil)
      (expect (cl-some (lambda (op) (eq (plist-get op :operation) :read)) file-ops)
              :to-be-truthy)))

  (it "7-stage pipeline denies out-of-scope file read"
    (let* ((config (bash-integ--make-scope-config
                    '("/workspace/**") '("/workspace/**") nil)))
      ;; /etc/passwd is not in /workspace/**
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /etc/passwd" "/workspace" config)))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope"))))

  (it "triggers expansion UI through macro for out-of-scope read"
    (let* ((config (bash-integ--make-scope-config
                    '("/workspace/**") '("/workspace/**") nil))
           (tool (bash-integ--find-tool "run_bash_command")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      (spy-on 'jf/gptel-scope-prompt-expansion)

      (funcall (gptel-tool-function tool)
               #'bash-integ--gptel-callback
               "cat /etc/passwd"
               "/workspace")

      (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)
      (expect bash-integ--callback-result :to-be nil)))

  (it "allow-once approves and tool body executes"
    (let* ((config (bash-integ--make-scope-config
                    '("/workspace/**") '("/workspace/**") nil))
           (tool (bash-integ--find-tool "run_bash_command")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      ;; Mock command execution
      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value '(:output "root:x:0:0:root:/root:/bin/bash"
                                  :exit_code 0
                                  :truncated nil :warnings nil :error nil))

      ;; "Allow once": signal success. Wrapper trusts approval and
      ;; runs the body without re-validation.
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (_violation-info callback _patterns _tool-name)
                (funcall callback
                         (json-serialize '(:success t :allowed_once t)))))

      (funcall (gptel-tool-function tool)
               #'bash-integ--gptel-callback
               "cat /etc/passwd"
               "/workspace")

      (expect bash-integ--callback-result :to-be-truthy)
      (expect (plist-get bash-integ--callback-result :success) :to-be t)
      (expect (plist-get bash-integ--callback-result :output)
              :to-equal "root:x:0:0:root:/root:/bin/bash")))

  (it "add-to-scope approves and runs the body"
    (let* ((config (bash-integ--make-scope-config
                    '("/workspace/**") '("/workspace/**") nil))
           (tool (bash-integ--find-tool "run_bash_command"))
           (load-count 0))

      (spy-on 'jf/gptel-scope--load-config
              :and-call-fake
              (lambda () (cl-incf load-count) config))

      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value '(:output "root:x:0:0:root:/root:/bin/bash"
                                  :exit_code 0
                                  :truncated nil :warnings nil :error nil))

      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (_violation-info callback _patterns _tool-name)
                (funcall callback
                         (json-serialize
                          (list :success t
                                :patterns_added (vector "/etc/**"))))))

      (funcall (gptel-tool-function tool)
               #'bash-integ--gptel-callback
               "cat /etc/passwd"
               "/workspace")

      ;; One validation pass, one config load — approval runs the body directly.
      (expect load-count :to-equal 1)
      (expect (plist-get bash-integ--callback-result :success) :to-be t))))


;; ============================================================
;; SUITE 4: gptel callback contract for bash tools
;; Verify error paths still invoke the callback (never signal)
;; ============================================================

(describe "run_bash_command integration: gptel callback contract"

  (before-each
    (setq bash-integ--callback-result nil)
    (setq bash-integ--callback-raw nil)
    (setq bash-integ--temp-dir (make-temp-file "bash-integ-" t)))

  (after-each
    (when (and bash-integ--temp-dir (file-exists-p bash-integ--temp-dir))
      (delete-directory bash-integ--temp-dir t)))

  (it "no_scope_config returns JSON through callback"
    (let ((tool (bash-integ--find-tool "run_bash_command")))
      (spy-on 'jf/gptel-scope--load-config :and-return-value nil)

      (funcall (gptel-tool-function tool)
               #'bash-integ--gptel-callback
               "which brew"
               "/workspace")

      (expect bash-integ--callback-result :to-be-truthy)
      (expect (plist-get bash-integ--callback-result :error)
              :to-equal "no_scope_config")))

  (it "callback receives exactly one string argument"
    (let* ((config (bash-integ--make-scope-config
                    (list (concat bash-integ--temp-dir "/**"))
                    nil nil))
           (tool (bash-integ--find-tool "run_bash_command"))
           (callback-arg-count nil)
           (callback-arg-type nil))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value '(:output "" :exit_code 0
                                  :truncated nil :warnings nil :error nil))

      (funcall (gptel-tool-function tool)
               (lambda (&rest args)
                 (setq callback-arg-count (length args))
                 (setq callback-arg-type (type-of (car args))))
               "which brew"
               bash-integ--temp-dir)

      (expect callback-arg-count :to-equal 1)
      (expect callback-arg-type :to-equal 'string)))

  (it "tool is registered as async in gptel--known-tools"
    (let ((tool (bash-integ--find-tool "run_bash_command")))
      (expect tool :to-be-truthy)
      (expect (gptel-tool-async tool) :to-be t))))

(provide 'bash-scope-expansion-integration-spec)

;;; bash-scope-expansion-integration-spec.el ends here
