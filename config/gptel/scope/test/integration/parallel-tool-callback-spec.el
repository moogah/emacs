;;; parallel-tool-callback-spec.el --- RED: parallel async tool calls must not lose callbacks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; RED PHASE TESTS: Reproduce the session stuck bug observed in
;; /Users/jefffarr/emacs-activities/test-2-2026-03-26/
;;
;; ROOT CAUSE: gptel's `gptel--handle-tool-use` processes ALL tool calls from
;; one LLM response via `mapc` (gptel-request.el:1695). For async tools, it
;; fires them all immediately without waiting:
;;
;;   (mapc (lambda (tool-call)
;;           ...
;;           (if (gptel-tool-async tool-spec)
;;               (apply (gptel-tool-function tool-spec)
;;                      process-tool-result arg-values)
;;             ...))
;;         tool-use)
;;
;; When two async scoped tools both trigger the expansion UI, both call
;; `jf/gptel-scope-prompt-expansion` → `transient-setup`. Since transient is
;; single-instance, the second call REPLACES the first transient's scope
;; (including its :callback). When the user responds, only the LAST callback
;; fires. The FIRST tool's `process-tool-result` is never called.
;;
;; gptel's FSM waits until tool-idx >= ntools (2), but only reaches 1.
;; The FSM never advances. The session shows "Calling tool..." forever.
;;
;; FROM THE SESSION LOG:
;;   The LLM returned two tool calls:
;;     1. run_bash_command("which brew", "/")
;;     2. run_bash_command("ls -la /usr/local/bin/brew ...", "/")
;;   Both denied (empty paths.read). Both trigger expansion UI.
;;   No follow-up request was ever sent → session stuck.
;;
;; CORRECT BEHAVIOR: When multiple async tools trigger the expansion UI,
;; all callbacks must eventually fire. This could be achieved by:
;;   - Queuing expansion prompts (show one at a time)
;;   - Batching denials into a single prompt
;;   - Auto-denying subsequent tools while one is pending
;;   - Using gptel's :confirm mechanism to serialize tool calls
;;
;; Tests simulate gptel's exact `mapc` invocation pattern and verify
;; both callbacks fire.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
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
  ;; Scope helpers
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  ;; Production scope modules
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  (require 'jf-gptel-scope-expansion
           (expand-file-name "scope/scope-expansion.el" gptel-dir))
  (require 'gptel))

;;; Test Infrastructure

(defvar parallel--tool-results nil
  "Alist of (tool-id . parsed-result) for each completed tool.")

(defvar parallel--callback-count 0
  "Number of times any process-tool-result callback was invoked.")

(defun parallel--find-tool (name)
  "Find tool NAME in gptel--known-tools registry."
  (cl-block nil
    (dolist (category-entry gptel--known-tools)
      (dolist (tool-entry (cdr category-entry))
        (when (string= (car tool-entry) name)
          (cl-return (cdr tool-entry)))))))

(defun parallel--make-process-tool-result (tool-id)
  "Create a process-tool-result callback like gptel does.
TOOL-ID is a label for tracking which callback fired.
Increments the shared counter and records the result."
  (lambda (result)
    (cl-incf parallel--callback-count)
    (let ((parsed (if (stringp result)
                      (json-parse-string result :object-type 'plist)
                    result)))
      (push (cons tool-id parsed) parallel--tool-results))))

(defun parallel--make-empty-scope-config ()
  "Build scope config with empty read paths (causes denial for all commands)."
  (jf/gptel-scope-yaml--merge-schema-defaults
   (jf/gptel-scope-yaml--parse-string
    "paths:
  read:
    []
  write:
    []
  execute:
    []
  modify:
    []
  deny:
    []
bash_tools:
  deny:
    - \"sudo\"
cloud:
  auth_detection: \"deny\"
security:
  enforce_parse_complete: true
  max_coverage_threshold: 1
")))


;;; Test Suites

;; ============================================================
;; SUITE 1: Characterization — reproduce gptel's mapc pattern
;; Verify the exact scenario from the session log
;; ============================================================

(describe "Parallel tool callback: session stuck reproduction"

  (before-each
    (setq parallel--tool-results nil)
    (setq parallel--callback-count 0)
    )

  (after-each
    )

  (describe "single tool call works (baseline)"

    (it "one denied tool fires callback when user denies expansion"
      (let* ((config (parallel--make-empty-scope-config))
             (tool (parallel--find-tool "run_bash_command")))

        (spy-on 'jf/gptel-scope--load-config :and-return-value config)
        ;; User denies
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (_violation-info callback _patterns _tool-name)
                  (funcall callback
                           (json-serialize (list :success :false :user_denied t)))))

        (funcall (gptel-tool-function tool)
                 (parallel--make-process-tool-result :tool-1)
                 "which brew" "/")

        (expect parallel--callback-count :to-equal 1))))

  (describe "two tools fired via mapc (the production scenario)"

    (it "both callbacks fire when expansion UI auto-denies both"
      ;; This simulates gptel's mapc: fire both tools immediately
      (let* ((config (parallel--make-empty-scope-config))
             (tool (parallel--find-tool "run_bash_command"))
             (ntools 2)
             (prompt-call-count 0))

        (spy-on 'jf/gptel-scope--load-config :and-return-value config)

        ;; Mock expansion UI to immediately deny both
        ;; In production, transient-setup clobbers the first callback
        ;; Here we simulate CORRECT behavior: both callbacks fire
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (_violation-info callback _patterns _tool-name)
                  (cl-incf prompt-call-count)
                  (funcall callback
                           (json-serialize (list :success :false :user_denied t)))))

        ;; === Simulate gptel's mapc loop ===
        ;; Fire both tools immediately (no waiting between them)
        (funcall (gptel-tool-function tool)
                 (parallel--make-process-tool-result :tool-1)
                 "which brew" "/")

        (funcall (gptel-tool-function tool)
                 (parallel--make-process-tool-result :tool-2)
                 "ls -la /usr/local/bin/brew" "/")

        ;; Expansion UI should have been called twice
        (expect prompt-call-count :to-equal 2)

        ;; CORRECT: Both callbacks must fire for FSM to advance
        (expect parallel--callback-count :to-equal ntools)

        ;; Both tools should have results
        (expect (assoc :tool-1 parallel--tool-results) :to-be-truthy)
        (expect (assoc :tool-2 parallel--tool-results) :to-be-truthy)))))


;; ============================================================
;; SUITE 2: RED — Simulate transient collision
;; The REAL prompt-expansion calls transient-setup, which is
;; single-instance. The second call replaces the first's scope.
;; When the user responds, only the last callback fires.
;; ============================================================

(describe "Parallel tool callback: transient collision"

  (before-each
    (setq parallel--tool-results nil)
    (setq parallel--callback-count 0)
    )

  (after-each
    )

  (it "when second transient-setup clobbers first, only last callback fires (characterizes bug)"
    ;; Simulate what transient actually does: only the LAST registered
    ;; callback survives. This is the production behavior.
    (let* ((config (parallel--make-empty-scope-config))
           (tool (parallel--find-tool "run_bash_command"))
           (last-callback nil))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      ;; Simulate transient collision: each call overwrites last-callback.
      ;; Only the final value of last-callback is invoked (simulating
      ;; transient-setup replacing the scope and user responding once).
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (_violation-info callback _patterns _tool-name)
                ;; Transient-setup REPLACES the scope, so only the last
                ;; callback is accessible when the user makes a choice
                (setq last-callback callback)))

      ;; Fire both tools (simulating mapc)
      (funcall (gptel-tool-function tool)
               (parallel--make-process-tool-result :tool-1)
               "which brew" "/")

      (funcall (gptel-tool-function tool)
               (parallel--make-process-tool-result :tool-2)
               "ls -la /usr/local/bin/brew" "/")

      ;; User responds to the transient menu (only last callback available)
      (when last-callback
        (funcall last-callback
                 (json-serialize (list :success :false :user_denied t))))

      ;; BUG: Only 1 callback fires (the last one)
      ;; tool-1's callback was lost when transient-setup was called the second time
      (expect parallel--callback-count :to-equal 1)

      ;; tool-2 got its result (last callback survived)
      (expect (assoc :tool-2 parallel--tool-results) :to-be-truthy)

      ;; tool-1's result is LOST
      (expect (assoc :tool-1 parallel--tool-results) :to-be nil)))

  (it "CORRECT: both callbacks fire via queue when user responds to each prompt"
    ;; Uses the REAL queue-aware prompt-expansion. Mocks transient-setup
    ;; to capture scopes, then simulates user responding to each sequentially.
    (let* ((config (parallel--make-empty-scope-config))
           (tool (parallel--find-tool "run_bash_command"))
           (ntools 2)
           (transient-scopes nil))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      ;; Let prompt-expansion run (queue-aware), but capture transient-setup calls
      (spy-on 'transient-setup
              :and-call-fake
              (lambda (_prefix &rest args)
                (let ((scope (plist-get (cddr args) :scope)))
                  (push scope transient-scopes))))

      ;; Reset queue state
      (setq jf/gptel-scope--expansion-active nil)
      (setq jf/gptel-scope--expansion-queue nil)

      ;; Fire both tools (simulating mapc)
      (funcall (gptel-tool-function tool)
               (parallel--make-process-tool-result :tool-1)
               "which brew" "/")

      (funcall (gptel-tool-function tool)
               (parallel--make-process-tool-result :tool-2)
               "ls -la /usr/local/bin/brew" "/")

      ;; First transient shown, second queued
      (expect (length transient-scopes) :to-equal 1)
      (expect (length jf/gptel-scope--expansion-queue) :to-equal 1)

      ;; User responds to first prompt (deny)
      (let ((cb-1 (plist-get (car transient-scopes) :callback)))
        (funcall cb-1 (json-serialize (list :success :false :user_denied t))))
      ;; Process queue — shows second prompt
      (jf/gptel-scope--process-expansion-queue)

      ;; Second transient now shown
      (expect (length transient-scopes) :to-equal 2)

      ;; User responds to second prompt (deny)
      (let ((cb-2 (plist-get (car transient-scopes) :callback)))
        (funcall cb-2 (json-serialize (list :success :false :user_denied t))))
      (jf/gptel-scope--process-expansion-queue)

      ;; Both callbacks fired
      (expect parallel--callback-count :to-equal ntools)
      (expect (assoc :tool-1 parallel--tool-results) :to-be-truthy)
      (expect (assoc :tool-2 parallel--tool-results) :to-be-truthy)))


  (it "CORRECT: FSM counter reaches ntools via queue for session to advance"
    ;; Simulates the exact FSM counter logic from gptel-request.el:1710-1718
    (let* ((config (parallel--make-empty-scope-config))
           (tool (parallel--find-tool "run_bash_command"))
           (ntools 2)
           (tool-idx 0)
           (fsm-advanced nil)
           (transient-scopes nil))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      (spy-on 'transient-setup
              :and-call-fake
              (lambda (_prefix &rest args)
                (let ((scope (plist-get (cddr args) :scope)))
                  (push scope transient-scopes))))

      (setq jf/gptel-scope--expansion-active nil)
      (setq jf/gptel-scope--expansion-queue nil)

      ;; Create process-tool-result callbacks like gptel does
      (let ((make-callback
             (lambda (tool-id)
               (lambda (result)
                 (push (cons tool-id result) parallel--tool-results)
                 (cl-incf tool-idx)
                 (when (>= tool-idx ntools)
                   (setq fsm-advanced t))))))

        ;; Fire both tools
        (funcall (gptel-tool-function tool)
                 (funcall make-callback :tool-1)
                 "which brew" "/")

        (funcall (gptel-tool-function tool)
                 (funcall make-callback :tool-2)
                 "ls -la /usr/local/bin/brew" "/")

        ;; Respond to both prompts sequentially via queue
        (let ((cb-1 (plist-get (car transient-scopes) :callback)))
          (funcall cb-1 (json-serialize (list :success :false :user_denied t))))
        (jf/gptel-scope--process-expansion-queue)

        (let ((cb-2 (plist-get (car transient-scopes) :callback)))
          (funcall cb-2 (json-serialize (list :success :false :user_denied t))))
        (jf/gptel-scope--process-expansion-queue)

        ;; FSM advances — both callbacks fired
        (expect fsm-advanced :to-be t)))))


;; ============================================================
;; SUITE 3: Mixed scenarios (one allowed, one denied)
;; ============================================================

(describe "Parallel tool callback: mixed allowed/denied"

  (before-each
    (setq parallel--tool-results nil)
    (setq parallel--callback-count 0)
    )

  (after-each
    )

  (it "both callbacks fire when one tool passes and one triggers expansion"
    ;; Scenario: tool-1 passes validation (e.g., no-op), tool-2 fails
    ;; The passing tool fires its callback immediately, the failing one
    ;; triggers expansion UI. With transient collision there's only one
    ;; expansion, so this should work if the first tool completes synchronously.
    (let* ((tool (parallel--find-tool "run_bash_command"))
           (ntools 2)
           ;; Config that allows /workspace but not /etc
           (config (jf/gptel-scope-yaml--merge-schema-defaults
                    (jf/gptel-scope-yaml--parse-string
                     "paths:
  read:
    - \"/workspace/**\"
  write:
    []
  deny:
    []
cloud:
  auth_detection: \"warn\"
security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      ;; Mock command execution for the tool that passes
      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value '(:output "output" :exit_code 0
                                  :truncated nil :warnings nil :error nil))
      ;; Mock expansion UI for the tool that fails
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (_violation-info callback _patterns _tool-name)
                (funcall callback
                         (json-serialize (list :success :false :user_denied t)))))

      ;; tool-1: "git --version" in /workspace → no-op, passes
      (funcall (gptel-tool-function tool)
               (parallel--make-process-tool-result :tool-1)
               "git --version" "/workspace")

      ;; tool-2: "cat /etc/passwd" in /workspace → out of scope, denied
      (funcall (gptel-tool-function tool)
               (parallel--make-process-tool-result :tool-2)
               "cat /etc/passwd" "/workspace")

      ;; CORRECT: Both callbacks should fire
      (expect parallel--callback-count :to-equal ntools)

      ;; tool-1 succeeded (no-op → tool body ran)
      (let ((result-1 (cdr (assoc :tool-1 parallel--tool-results))))
        (expect result-1 :to-be-truthy)
        (expect (plist-get result-1 :success) :to-be t))

      ;; tool-2 was denied
      (let ((result-2 (cdr (assoc :tool-2 parallel--tool-results))))
        (expect result-2 :to-be-truthy)
        (expect (plist-get result-2 :success) :not :to-be t)))))

(provide 'parallel-tool-callback-spec)

;;; parallel-tool-callback-spec.el ends here
