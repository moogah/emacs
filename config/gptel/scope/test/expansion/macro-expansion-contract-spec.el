;;; macro-expansion-contract-spec.el --- Contract tests for gptel-make-scoped-tool macro -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CONTRACT TESTS: Verify the gptel-make-scoped-tool macro correctly connects
;; validation routing, expansion triggering, and tool body execution.
;;
;; These tests use MINIMAL test tools created via the macro — tool bodies are
;; one-liners whose only purpose is proving the macro let them execute. This
;; isolates macro behavior from tool-specific complexity.
;;
;; Three test layers:
;;   Layer 1: Routing — correct validator dispatched for each category
;;   Layer 2: Expansion — macro triggers expansion on denial, handles approval/denial
;;   Layer 3: Validator — validate-bash-tool returns :allowed nil (the core bug)
;;
;; RED PHASE: These tests expose the current broken behavior:
;;   - validate-bash-tool returns :allowed t (should return :allowed nil)
;;   - Expansion UI never triggers for bash tool denials
;;   - Tool body runs validation internally (should be macro-only)

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)
(require 'gptel)

;; Load scope modules
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (expansion-dir test-dir)
       (scope-test-dir (expand-file-name ".." expansion-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope-tool-wrapper.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir)))

;;; Test State

(defvar macro-contract--callback-result nil
  "Result captured from gptel callback.")

(defvar macro-contract--body-executed nil
  "Flag: was the tool body executed?")

(defvar macro-contract--test-config nil
  "Scope config for tests.")

;;; Helpers

(defun macro-contract--gptel-callback (result-json)
  "Capture RESULT-JSON from tool callback."
  (setq macro-contract--callback-result
        (json-parse-string result-json :object-type 'plist)))

(defun macro-contract--find-tool (name)
  "Find tool NAME in gptel--known-tools registry.
gptel-make-tool stores tools in gptel--known-tools (alist keyed by category)."
  (cl-block nil
    (dolist (category-entry gptel--known-tools)
      (dolist (tool-entry (cdr category-entry))
        (when (string= (car tool-entry) name)
          (cl-return (cdr tool-entry)))))))

;; ============================================================
;; LAYER 1: Routing Contract
;; Verify the macro dispatches to the correct validator
;; ============================================================

(describe "Layer 1: Routing contract"

  (before-each
    (setq macro-contract--callback-result nil)
    (setq macro-contract--body-executed nil)
    ;; Provide a config so validation proceeds
    (setq macro-contract--test-config
          '(:paths (:read ("/workspace/**") :write ("/workspace/**")
                   :execute () :modify () :deny ())
            :bash-tools (:deny ())
            :cloud (:auth-detection "warn")
            :security (:enforce-parse-complete t :max-coverage-threshold 0.8)))
    (spy-on 'jf/gptel-scope--load-config
            :and-return-value macro-contract--test-config)

    ;; Register test tool names in categories so routing dispatches correctly.
    ;; This tests that check-tool-permission routes based on the category entry.
    (push '("test_bash_routing" . (:validation bash :operation write))
          jf/gptel-scope--tool-categories)
    (push '("test_path_routing" . (:validation path :operation read))
          jf/gptel-scope--tool-categories)
    (push '("test_meta_routing" . (:validation meta :operation meta))
          jf/gptel-scope--tool-categories))

  (after-each
    ;; Clean up test entries from categories
    (setq jf/gptel-scope--tool-categories
          (cl-remove-if (lambda (entry)
                          (string-prefix-p "test_" (car entry)))
                        jf/gptel-scope--tool-categories)))

  (describe "bash-category tool"

    (it "dispatches to validate-bash-tool (not validate-path-tool)"
      ;; Create minimal bash-category tool
      (eval
       '(gptel-make-scoped-tool
         "test_bash_routing"
         "Test bash routing"
         (list '(:name "command" :type string :description "Command")
               '(:name "directory" :type string :description "Dir"))
         :operation write
         :async
         (progn
           (setq macro-contract--body-executed t)
           (list :success t))))

      (let ((tool (macro-contract--find-tool "test_bash_routing")))
        (expect tool :to-be-truthy)

        ;; Spy on validators
        (spy-on 'jf/gptel-scope--validate-bash-tool :and-call-through)
        (spy-on 'jf/gptel-scope--validate-path-tool :and-call-through)

        ;; Mock bash-parser so validate-command-semantics doesn't fail
        (helpers-spec-setup-bash-mocks)
        (helpers-spec-mock-bash-parse "ls" '("ls") t)
        (helpers-spec-mock-bash-semantics '() nil '(:ratio 1.0))

        ;; Invoke tool through macro
        (funcall (gptel-tool-function tool)
                 #'macro-contract--gptel-callback "ls" "/workspace")

        ;; Assert routing
        (expect 'jf/gptel-scope--validate-bash-tool :to-have-been-called)
        (expect 'jf/gptel-scope--validate-path-tool :not :to-have-been-called)

        (helpers-spec-teardown-bash-mocks))))

  (describe "path-category tool"

    (it "dispatches to validate-path-tool (not validate-bash-tool)"
      ;; Create minimal path-category tool
      (eval
       '(gptel-make-scoped-tool
         "test_path_routing"
         "Test path routing"
         (list '(:name "filepath" :type string :description "Path"))
         :operation read
         :async
         (progn
           (setq macro-contract--body-executed t)
           (list :success t :content "file content"))))

      (let ((tool (macro-contract--find-tool "test_path_routing")))
        (expect tool :to-be-truthy)

        ;; Spy on validators
        (spy-on 'jf/gptel-scope--validate-bash-tool :and-call-through)
        (spy-on 'jf/gptel-scope--validate-path-tool :and-call-through)

        ;; Invoke with an in-scope path
        (funcall (gptel-tool-function tool)
                 #'macro-contract--gptel-callback "/workspace/file.txt")

        ;; Assert routing
        (expect 'jf/gptel-scope--validate-path-tool :to-have-been-called)
        (expect 'jf/gptel-scope--validate-bash-tool :not :to-have-been-called))))

  (describe "meta-category tool"

    (it "bypasses all validation and executes body directly"
      (eval
       '(gptel-make-scoped-tool
         "test_meta_routing"
         "Test meta routing"
         (list '(:name "arg" :type string :description "Arg"))
         :operation read
         :async
         (progn
           (setq macro-contract--body-executed t)
           (list :success t))))

      (let ((tool (macro-contract--find-tool "test_meta_routing")))
        (expect tool :to-be-truthy)

        ;; Spy on validators
        (spy-on 'jf/gptel-scope--validate-bash-tool :and-call-through)
        (spy-on 'jf/gptel-scope--validate-path-tool :and-call-through)
        (spy-on 'jf/gptel-scope--validate-pattern-tool :and-call-through)

        ;; Invoke
        (funcall (gptel-tool-function tool)
                 #'macro-contract--gptel-callback "test-arg")

        ;; Assert no validators called, body executed
        (expect 'jf/gptel-scope--validate-bash-tool :not :to-have-been-called)
        (expect 'jf/gptel-scope--validate-path-tool :not :to-have-been-called)
        (expect 'jf/gptel-scope--validate-pattern-tool :not :to-have-been-called)
        (expect macro-contract--body-executed :to-be t)))))

;; ============================================================
;; LAYER 2: Expansion Contract
;; Verify the macro triggers expansion on denial and handles
;; approval/denial correctly
;; ============================================================

(describe "Layer 2: Expansion contract"

  (before-each
    (setq macro-contract--callback-result nil)
    (setq macro-contract--body-executed nil)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil))

    ;; Register test tool names in categories
    (push '("test_expansion_trigger" . (:validation bash :operation write))
          jf/gptel-scope--tool-categories)
    (push '("test_expansion_approve" . (:validation bash :operation write))
          jf/gptel-scope--tool-categories)
    (push '("test_expansion_deny" . (:validation bash :operation write))
          jf/gptel-scope--tool-categories)
    (push '("test_sync_no_expansion" . (:validation path :operation read))
          jf/gptel-scope--tool-categories))

  (after-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil))
    (setq jf/gptel-scope--tool-categories
          (cl-remove-if (lambda (entry)
                          (string-prefix-p "test_" (car entry)))
                        jf/gptel-scope--tool-categories)))

  (describe "async tool with validation denial"

    (it "triggers expansion UI when check-tool-permission returns :allowed nil"
      ;; Create a minimal async tool with 2 args (bash tools need command + directory)
      (eval
       '(gptel-make-scoped-tool
         "test_expansion_trigger"
         "Test expansion trigger"
         (list '(:name "command" :type string :description "Command")
               '(:name "directory" :type string :description "Dir"))
         :operation write
         :async
         (progn
           (setq macro-contract--body-executed t)
           (list :success t))))

      (let ((tool (macro-contract--find-tool "test_expansion_trigger")))
        (expect tool :to-be-truthy)

        ;; Mock config
        (spy-on 'jf/gptel-scope--load-config
                :and-return-value '(:bash-tools (:deny ())))

        ;; Mock check-tool-permission to deny
        (spy-on 'jf/gptel-scope--check-tool-permission
                :and-return-value '(:allowed nil
                                    :error "path_out_of_scope"
                                    :message "Path not in scope: /etc/passwd"
                                    :path "/etc/passwd"))

        ;; Spy on expansion UI (don't invoke callback — just verify it's called)
        (spy-on 'jf/gptel-scope-prompt-expansion)

        ;; Invoke tool with command + directory
        (funcall (gptel-tool-function tool)
                 #'macro-contract--gptel-callback "ls /etc" "/workspace")

        ;; THE KEY ASSERTION: expansion UI should be triggered
        (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)

        ;; Tool body should NOT have executed
        (expect macro-contract--body-executed :to-be nil)))

    (it "executes tool body after expansion approval with allow-once"
      (eval
       '(gptel-make-scoped-tool
         "test_expansion_approve"
         "Test expansion approve"
         (list '(:name "command" :type string :description "Command")
               '(:name "directory" :type string :description "Dir"))
         :operation write
         :async
         (progn
           (setq macro-contract--body-executed t)
           (list :success t :value "tool-result"))))

      (let ((tool (macro-contract--find-tool "test_expansion_approve")))
        (expect tool :to-be-truthy)

        ;; Mock config
        (spy-on 'jf/gptel-scope--load-config
                :and-return-value '(:bash-tools (:deny ())))

        ;; First call denies, retry allows (after allow-once added)
        (let ((call-count 0))
          (spy-on 'jf/gptel-scope--check-tool-permission
                  :and-call-fake
                  (lambda (_config _tool-name _args _metadata)
                    (setq call-count (1+ call-count))
                    (if (= call-count 1)
                        ;; First call: deny
                        '(:allowed nil
                          :error "path_out_of_scope"
                          :message "Path not in scope"
                          :path "/etc/foo")
                      ;; Retry after approval: allow
                      '(:allowed t)))))

        ;; Mock expansion UI to auto-approve (simulate allow-once)
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback _patterns _tool-name)
                  (funcall callback
                           (json-serialize '(:success t :allowed_once t)))))

        ;; Invoke tool with command + directory
        (funcall (gptel-tool-function tool)
                 #'macro-contract--gptel-callback "ls /etc" "/workspace")

        ;; Expansion was triggered
        (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)

        ;; Tool body executed after approval
        (expect macro-contract--body-executed :to-be t)

        ;; Callback received success result
        (expect (plist-get macro-contract--callback-result :success) :to-be t)))

    (it "does NOT execute tool body when expansion denied"
      (eval
       '(gptel-make-scoped-tool
         "test_expansion_deny"
         "Test expansion deny"
         (list '(:name "command" :type string :description "Command")
               '(:name "directory" :type string :description "Dir"))
         :operation write
         :async
         (progn
           (setq macro-contract--body-executed t)
           (list :success t))))

      (let ((tool (macro-contract--find-tool "test_expansion_deny")))
        (expect tool :to-be-truthy)

        ;; Mock config
        (spy-on 'jf/gptel-scope--load-config
                :and-return-value '(:bash-tools (:deny ())))

        ;; Mock validation to deny
        (spy-on 'jf/gptel-scope--check-tool-permission
                :and-return-value '(:allowed nil
                                    :error "path_out_of_scope"
                                    :message "Not in scope"
                                    :path "/etc/foo"))

        ;; Mock expansion UI to deny (user rejects)
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback _patterns _tool-name)
                  (funcall callback
                           (json-serialize '(:success :false :user_denied t)))))

        ;; Invoke tool with command + directory
        (funcall (gptel-tool-function tool)
                 #'macro-contract--gptel-callback "ls /etc" "/workspace")

        ;; Expansion was triggered
        (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)

        ;; Tool body should NOT have executed
        (expect macro-contract--body-executed :to-be nil)

        ;; Callback received error
        (expect (plist-get macro-contract--callback-result :success) :not :to-be t))))

  (describe "sync tool with validation denial"

    (it "returns error immediately without triggering expansion UI"
      (eval
       '(gptel-make-scoped-tool
         "test_sync_no_expansion"
         "Test sync no expansion"
         (list '(:name "filepath" :type string :description "Path"))
         :operation read
         ;; No :async — sync tool
         (progn
           (setq macro-contract--body-executed t)
           (list :success t))))

      (let ((tool (macro-contract--find-tool "test_sync_no_expansion")))
        (expect tool :to-be-truthy)

        ;; Mock config
        (spy-on 'jf/gptel-scope--load-config
                :and-return-value '(:paths (:read () :write () :deny ())))

        ;; Spy on expansion UI
        (spy-on 'jf/gptel-scope-prompt-expansion)

        ;; Invoke sync tool (no callback first arg)
        (let ((result (funcall (gptel-tool-function tool) "/etc/passwd")))
          ;; Expansion UI should NOT be triggered for sync tools
          (expect 'jf/gptel-scope-prompt-expansion :not :to-have-been-called)

          ;; Tool body should NOT have executed
          (expect macro-contract--body-executed :to-be nil)

          ;; Should return error plist
          (expect (plist-get result :success) :to-be nil))))))

;; ============================================================
;; LAYER 3: Validator Unit Tests
;; Verify validate-bash-tool returns :allowed nil on denial
;; (This is the CORE BUG — currently returns :allowed t always)
;; ============================================================

(describe "Layer 3: validate-bash-tool returns correct :allowed result"

  (before-each
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks))

  (it "returns :allowed nil when file path is out of scope"
    ;; Mock: command reads /etc/passwd which is not in paths.read
    (helpers-spec-mock-bash-parse "cat /etc/passwd" '("cat") t)
    (helpers-spec-mock-bash-semantics
     (list (helpers-spec--make-file-op :read "/etc/passwd" :command "cat"))
     nil
     '(:ratio 1.0))

    (let* ((config '(:paths (:read ("/workspace/**") :write ()
                             :execute () :modify () :deny ())
                    :bash-tools (:deny ())
                    :cloud (:auth-detection "warn")
                    :security (:enforce-parse-complete t
                               :max-coverage-threshold 0.8)))
           (result (jf/gptel-scope--validate-bash-tool
                    "run_bash_command"
                    '("cat /etc/passwd" "/workspace")
                    config
                    nil)))
      ;; CORE BUG: Currently returns (:allowed t :validation semantic)
      ;; Should return (:allowed nil :error "path_out_of_scope" ...)
      (expect (plist-get result :allowed) :to-be nil)
      (expect (plist-get result :error) :to-equal "path_out_of_scope")))

  (it "returns :allowed t when command has no file operations (no-op)"
    ;; Mock: version check has zero file operations
    (helpers-spec-mock-bash-parse "git --version" '("git") t)
    (helpers-spec-mock-bash-semantics '() nil '(:ratio 1.0))

    (let* ((config '(:paths (:read () :write () :execute () :modify () :deny ())
                    :bash-tools (:deny ())
                    :cloud (:auth-detection "warn")
                    :security (:enforce-parse-complete t
                               :max-coverage-threshold 0.8)))
           (result (jf/gptel-scope--validate-bash-tool
                    "run_bash_command"
                    '("git --version" "/workspace")
                    config
                    nil)))
      (expect (plist-get result :allowed) :to-be t)))

  (it "returns :allowed nil when command is in deny list"
    ;; Mock: rm is in deny list
    (helpers-spec-mock-bash-parse "rm -rf /tmp/foo" '("rm") t)
    (helpers-spec-mock-bash-semantics '() nil '(:ratio 1.0))

    (let* ((config '(:paths (:read ("/workspace/**") :write ("/workspace/**")
                             :execute () :modify () :deny ())
                    :bash-tools (:deny ("rm" "sudo"))
                    :cloud (:auth-detection "warn")
                    :security (:enforce-parse-complete t
                               :max-coverage-threshold 0.8)))
           (result (jf/gptel-scope--validate-bash-tool
                    "run_bash_command"
                    '("rm -rf /tmp/foo" "/workspace")
                    config
                    nil)))
      ;; Should deny because rm is in deny list
      (expect (plist-get result :allowed) :to-be nil)
      (expect (plist-get result :error) :to-equal "command_denied")))

  (it "returns :allowed nil when bash_tools config is missing"
    (let* ((config '(:paths (:read ("/workspace/**") :write ()
                             :execute () :modify () :deny ())
                    :cloud (:auth-detection "warn")
                    :security (:enforce-parse-complete t
                               :max-coverage-threshold 0.8)))
           (result (jf/gptel-scope--validate-bash-tool
                    "run_bash_command"
                    '("ls" "/workspace")
                    config
                    nil)))
      ;; This one should already pass — existing guard clause
      (expect (plist-get result :allowed) :to-be nil)
      (expect (plist-get result :error) :to-equal "command-not-allowed")))

  (it "propagates error fields for build-violation-info consumption"
    ;; Mock: path out of scope — error should include :path, :message, etc.
    (helpers-spec-mock-bash-parse "cat /etc/passwd" '("cat") t)
    (helpers-spec-mock-bash-semantics
     (list (helpers-spec--make-file-op :read "/etc/passwd" :command "cat"))
     nil
     '(:ratio 1.0))

    (let* ((config '(:paths (:read ("/workspace/**") :write ()
                             :execute () :modify () :deny ())
                    :bash-tools (:deny ())
                    :cloud (:auth-detection "warn")
                    :security (:enforce-parse-complete t
                               :max-coverage-threshold 0.8)))
           (result (jf/gptel-scope--validate-bash-tool
                    "run_bash_command"
                    '("cat /etc/passwd" "/workspace")
                    config
                    nil)))
      ;; Should have error fields that build-violation-info can consume
      (expect (plist-get result :allowed) :to-be nil)
      (expect (plist-get result :error) :to-be-truthy)
      (expect (plist-get result :message) :to-be-truthy))))

;; ============================================================
;; LAYER 2+3 COMBINED: End-to-End
;; Bash tool through macro → real validation → expansion triggers
;; This is the exact scenario that was broken in production
;; ============================================================

(describe "End-to-end: bash tool denial triggers expansion through macro"

  (before-each
    (setq macro-contract--callback-result nil)
    (setq macro-contract--body-executed nil)
    (helpers-spec-setup-bash-mocks)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil))

    ;; Register test tool name in categories
    (push '("test_e2e_bash_expansion" . (:validation bash :operation write))
          jf/gptel-scope--tool-categories))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil))
    (setq jf/gptel-scope--tool-categories
          (cl-remove-if (lambda (entry)
                          (string-prefix-p "test_" (car entry)))
                        jf/gptel-scope--tool-categories)))

  (it "triggers expansion UI when bash tool reads out-of-scope path"
    ;; Create bash tool via macro
    (eval
     '(gptel-make-scoped-tool
       "test_e2e_bash_expansion"
       "Test e2e bash expansion"
       (list '(:name "command" :type string :description "Command")
             '(:name "directory" :type string :description "Dir"))
       :operation write
       :async
       (progn
         (setq macro-contract--body-executed t)
         (list :success t :output "command output"))))

    (let ((tool (macro-contract--find-tool "test_e2e_bash_expansion")))
      (expect tool :to-be-truthy)

      ;; Config: /workspace readable, /etc not
      (spy-on 'jf/gptel-scope--load-config
              :and-return-value
              '(:paths (:read ("/workspace/**") :write ()
                        :execute () :modify () :deny ())
                :bash-tools (:deny ())
                :cloud (:auth-detection "warn")
                :security (:enforce-parse-complete t
                           :max-coverage-threshold 0.8)))

      ;; Mock bash-parser: cat /etc/passwd reads /etc/passwd
      (helpers-spec-mock-bash-parse "cat /etc/passwd" '("cat") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/etc/passwd" :command "cat"))
       nil
       '(:ratio 1.0))

      ;; Spy on expansion UI
      (spy-on 'jf/gptel-scope-prompt-expansion)

      ;; Invoke tool through macro — this is the REAL flow
      (funcall (gptel-tool-function tool)
               #'macro-contract--gptel-callback
               "cat /etc/passwd" "/workspace")

      ;; THE REGRESSION TEST:
      ;; In production this fails — expansion UI is never triggered because
      ;; validate-bash-tool returns :allowed t and the tool body handles
      ;; validation internally, returning error JSON without expansion.
      (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)

      ;; Tool body should NOT have run (validation failed, waiting for expansion)
      (expect macro-contract--body-executed :to-be nil))))

;; ============================================================
;; ALLOW-ONCE ROUND-TRIP: Resource Format Consistency
;; Verify that the resource stored by expansion UI "allow once"
;; matches what check-allow-once looks up on retry.
;; This caught a format mismatch where the UI stored a file path
;; but check-allow-once expected "command:directory" composite format.
;; ============================================================

(describe "Allow-once round-trip: resource format consistency"

  (before-each
    (setq macro-contract--callback-result nil)
    (setq macro-contract--body-executed nil)
    (helpers-spec-setup-bash-mocks)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil))

    ;; Register test tool names
    (push '("test_allowonce_bash" . (:validation bash :operation write))
          jf/gptel-scope--tool-categories)
    (push '("test_allowonce_path" . (:validation path :operation read))
          jf/gptel-scope--tool-categories))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil))
    (setq jf/gptel-scope--tool-categories
          (cl-remove-if (lambda (entry)
                          (string-prefix-p "test_" (car entry)))
                        jf/gptel-scope--tool-categories)))

  (it "bash tool: allow-once stores resource in format check-allow-once expects"
    ;; The resource format stored by the expansion UI must match
    ;; what check-allow-once constructs from args.
    ;; For bash: "command:directory" composite format.
    (let* ((validation-error '(:allowed nil
                               :error "path_out_of_scope"
                               :message "Path not in scope: /etc/passwd"
                               :path "/etc/passwd"
                               :command "cat /etc/passwd"))
           (tool-args '("cat /etc/passwd" "/workspace")))

      ;; Mock expansion UI to simulate "allow once" action
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (violation-info callback _patterns _tool-name)
                ;; This is what the real allow-once action does:
                ;; stores (tool-name . allow-once-resource) from violation-info
                (jf/gptel-scope-add-to-allow-once-list
                 (plist-get violation-info :tool)
                 (or (plist-get violation-info :allow-once-resource)
                     (plist-get violation-info :resource)))
                (funcall callback
                         (json-serialize '(:success t :allowed_once t)))))

      ;; Call trigger-inline-expansion with real function
      ;; (not spied — we want to test the resource format it produces)
      (jf/gptel-scope--trigger-inline-expansion
       validation-error
       "test_allowonce_bash"
       tool-args
       (lambda (_result) nil))

      ;; Now verify the stored resource matches what check-allow-once expects
      (expect jf/gptel-scope--allow-once-list :not :to-be nil)

      ;; check-allow-once constructs: (format "%s:%s" (car args) (expand-file-name (cadr args)))
      (let* ((expected-resource (format "%s:%s" (car tool-args)
                                       (expand-file-name (cadr tool-args))))
             (stored-entry (car jf/gptel-scope--allow-once-list)))
        (expect (car stored-entry) :to-equal "test_allowonce_bash")
        (expect (cdr stored-entry) :to-equal expected-resource))

      ;; Verify check-allow-once actually finds it
      (let ((found (jf/gptel-scope--check-allow-once
                    "test_allowonce_bash" tool-args nil)))
        (expect found :to-be t))))

  (it "path tool: allow-once stores resource in format check-allow-once expects"
    (let* ((validation-error '(:allowed nil
                               :error "not-in-scope"
                               :message "Path not in read scope"
                               :path "/outside/scope/file.txt"))
           (tool-args '("/outside/scope/file.txt")))

      ;; Mock expansion UI to simulate "allow once"
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (violation-info callback _patterns _tool-name)
                (jf/gptel-scope-add-to-allow-once-list
                 (plist-get violation-info :tool)
                 (plist-get violation-info :resource))
                (funcall callback
                         (json-serialize '(:success t :allowed_once t)))))

      ;; Call trigger-inline-expansion with real function
      (jf/gptel-scope--trigger-inline-expansion
       validation-error
       "test_allowonce_path"
       tool-args
       (lambda (_result) nil))

      ;; check-allow-once for path tools: (expand-file-name (car args))
      (let* ((expected-resource (expand-file-name (car tool-args)))
             (stored-entry (car jf/gptel-scope--allow-once-list)))
        (expect (car stored-entry) :to-equal "test_allowonce_path")
        (expect (cdr stored-entry) :to-equal expected-resource))

      ;; Verify check-allow-once actually finds it
      (let ((found (jf/gptel-scope--check-allow-once
                    "test_allowonce_path" tool-args nil)))
        (expect found :to-be t))))

  (it "bash tool: full macro round-trip with allow-once approval executes body"
    ;; End-to-end: tool invoked → denied → expansion → allow-once → retry → body runs
    (eval
     '(gptel-make-scoped-tool
       "test_allowonce_bash"
       "Test allow-once round-trip"
       (list '(:name "command" :type string :description "Command")
             '(:name "directory" :type string :description "Dir"))
       :operation write
       :async
       (progn
         (setq macro-contract--body-executed t)
         (list :success t :output "command output"))))

    (let ((tool (macro-contract--find-tool "test_allowonce_bash")))
      (expect tool :to-be-truthy)

      ;; Config: /workspace readable, nothing else
      (spy-on 'jf/gptel-scope--load-config
              :and-return-value
              '(:paths (:read ("/workspace/**") :write ()
                        :execute () :modify () :deny ())
                :bash-tools (:deny ())
                :cloud (:auth-detection "warn")
                :security (:enforce-parse-complete t
                           :max-coverage-threshold 0.8)))

      ;; Mock bash-parser: ls /etc reads /etc (out of scope)
      (helpers-spec-mock-bash-parse "ls /etc" '("ls") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/etc" :command "ls"))
       nil
       '(:ratio 1.0))

      ;; Mock expansion UI: simulate "allow once" by adding to allow-once list
      ;; using the resource from violation-info (which trigger-inline-expansion
      ;; has already formatted to match check-allow-once)
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (violation-info callback _patterns _tool-name)
                (jf/gptel-scope-add-to-allow-once-list
                 (plist-get violation-info :tool)
                 (or (plist-get violation-info :allow-once-resource)
                     (plist-get violation-info :resource)))
                (funcall callback
                         (json-serialize '(:success t :allowed_once t)))))

      ;; Mock command execution (avoid real shell)
      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value '(:exit_code 0 :output "etc contents"
                                  :truncated nil :warnings nil))

      ;; Invoke tool through macro
      (funcall (gptel-tool-function tool)
               #'macro-contract--gptel-callback
               "ls /etc" "/workspace")

      ;; Expansion UI was triggered
      (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)

      ;; Tool body executed after allow-once approval and retry
      (expect macro-contract--body-executed :to-be t)

      ;; Callback received success
      (expect (plist-get macro-contract--callback-result :success) :to-be t))))

(provide 'macro-expansion-contract-spec)

;;; macro-expansion-contract-spec.el ends here
