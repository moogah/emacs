;;; gptel-transient-async.el --- Experiments for gptel-transient async integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr & Claude Sonnet 4.5
;; Created: 2026-01-23

;;; Commentary:

;; This file contains incremental experiments to isolate and understand
;; the incompatibility between gptel's FSM-based async tool callbacks
;; and transient's command-driven menu system.

;; Usage:
;; 1. Load this file: M-x load-file RET experiments/gptel-transient-async.el RET
;; 2. Run experiments: M-x gptel-exp-run-experiment-1 RET
;; 3. Check results in *gptel-exp-log* buffer

;;; Code:

(require 'cl-lib)

;;; Logging utilities

(defvar gptel-exp-log nil
  "Log of experiment events.")

(defun gptel-exp-clear-log ()
  "Clear the experiment log."
  (setq gptel-exp-log nil))

(defun gptel-exp-log (event &rest args)
  "Log an EVENT with ARGS."
  (let ((entry (cons event args)))
    (push entry gptel-exp-log)
    (message "[LOG] %s: %s" event args)))

(defun gptel-exp-show-log ()
  "Display the experiment log in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*gptel-exp-log*")
    (erase-buffer)
    (insert "# Experiment Log\n\n")
    (dolist (entry (reverse gptel-exp-log))
      (insert (format "%s: %s\n" (car entry) (cdr entry))))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; Mock gptel FSM

(cl-defstruct gptel-exp-fsm
  "Mock gptel FSM structure."
  state        ; Current state: READY, WAITING_FOR_TOOL, WAITING_FOR_RESPONSE
  tools        ; Available tools
  pending      ; Pending tool results
  callback)    ; Continuation callback

(defun gptel-exp-create-fsm ()
  "Create a mock gptel FSM."
  (make-gptel-exp-fsm
   :state 'READY
   :tools '()
   :pending '()
   :callback nil))

(defun gptel-exp-fsm-transition (fsm new-state)
  "Transition FSM to NEW-STATE."
  (gptel-exp-log 'fsm-transition
                 (format "%s -> %s" (gptel-exp-fsm-state fsm) new-state))
  (setf (gptel-exp-fsm-state fsm) new-state))

(defun gptel-exp-create-tool-callback (fsm tool-name)
  "Create a tool callback closure for FSM and TOOL-NAME.
This simulates gptel's process-tool-result closure."
  (lambda (result)
    (gptel-exp-log 'callback-invoked
                   (format "tool=%s result=%s" tool-name result))
    ;; Store result
    (push (cons tool-name result) (gptel-exp-fsm-pending fsm))
    ;; Transition FSM
    (gptel-exp-fsm-transition fsm 'PROCESSING_RESULT)
    ;; Simulate processing complete
    (run-with-timer 0.1 nil
                    (lambda ()
                      (gptel-exp-log 'processing-complete "FSM ready for next request")
                      (gptel-exp-fsm-transition fsm 'READY)))))

;;; Mock Transient

(defvar gptel-exp-transient-active nil
  "Whether transient is active.")

(defvar gptel-exp-transient-scope nil
  "Transient scope data.")

(defun gptel-exp-transient-setup (prefix)
  "Setup transient with PREFIX."
  (gptel-exp-log 'transient-setup prefix)
  (setq gptel-exp-transient-active t)
  (setq gptel-exp-transient-scope (make-hash-table)))

(defun gptel-exp-transient-quit ()
  "Quit transient."
  (gptel-exp-log 'transient-quit "Cleaning up")
  (setq gptel-exp-transient-active nil)
  (setq gptel-exp-transient-scope nil))

(defun gptel-exp-transient-suffix (name fn)
  "Define a transient suffix NAME that calls FN."
  (lambda ()
    (interactive)
    (gptel-exp-log 'suffix-invoke name)
    (funcall fn)
    ;; Simulate transient staying active (transient suffix)
    ;; or quitting (non-transient suffix)
    (when (string-suffix-p "-quit" (symbol-name name))
      (gptel-exp-transient-quit))))

;;; Experiment 1: Baseline Async Pattern

(defun gptel-exp-run-experiment-1 ()
  "Test that gptel's closure-based async coordination works without transient.

Expected: FSM transitions correctly via callback."
  (interactive)
  (gptel-exp-clear-log)
  (gptel-exp-log 'experiment "Starting Experiment 1: Baseline Async Pattern")

  ;; Create FSM
  (let ((exp1-fsm (gptel-exp-create-fsm)))

    ;; Transition to waiting for tool
    (gptel-exp-fsm-transition exp1-fsm 'WAITING_FOR_TOOL)

    ;; Create callback
    (let ((exp1-callback (gptel-exp-create-tool-callback exp1-fsm "test-tool")))

      ;; Simulate tool execution
      (gptel-exp-log 'tool-execute "test-tool starting")

      ;; Invoke callback immediately (simulating tool completion)
      (funcall exp1-callback "tool-result-data")

      ;; Wait for async processing
      (sleep-for 0.2)

      ;; Check final state
      (gptel-exp-log 'final-state (gptel-exp-fsm-state exp1-fsm))
      (gptel-exp-log 'pending-results (gptel-exp-fsm-pending exp1-fsm))

      ;; Show log
      (gptel-exp-show-log)

      ;; Return success/failure
      (if (eq (gptel-exp-fsm-state exp1-fsm) 'READY)
          (message "✓ Experiment 1 PASSED: FSM transitioned correctly")
        (message "✗ Experiment 1 FAILED: FSM in state %s (expected READY)"
                 (gptel-exp-fsm-state exp1-fsm))))))

;;; Experiment 2: Transient Basics

(defun gptel-exp-run-experiment-2 ()
  "Understand transient's execution model and lifecycle timing.

Expected: Understand when commands return and hooks run."
  (interactive)
  (gptel-exp-clear-log)
  (gptel-exp-log 'experiment "Starting Experiment 2: Transient Basics")

  ;; Setup transient
  (gptel-exp-transient-setup 'test-prefix)

  ;; Test immediate execution
  (let ((exp2-test-value nil))

    (gptel-exp-log 'test "immediate execution")
    (setq exp2-test-value "immediate-result")
    (gptel-exp-log 'result (format "value=%s" exp2-test-value))

    ;; Test post-command hook timing
    (gptel-exp-log 'test "post-command hook")
    (add-hook 'post-command-hook
              (lambda ()
                (gptel-exp-log 'post-command-hook "running")
                (setq exp2-test-value "post-command-result")
                (remove-hook 'post-command-hook (lambda () nil)))
              nil t)
    (run-hooks 'post-command-hook)
    (gptel-exp-log 'result (format "value=%s" exp2-test-value))

    ;; Test timer-based deferral
    (gptel-exp-log 'test "timer-based deferral")
    (run-with-timer 0.05 nil
                    (lambda ()
                      (gptel-exp-log 'timer-callback "running")
                      (setq exp2-test-value "timer-result")))
    (gptel-exp-log 'result (format "value=%s (before timer)" exp2-test-value))
    (sleep-for 0.1)
    (gptel-exp-log 'result (format "value=%s (after timer)" exp2-test-value))

    ;; Cleanup
    (gptel-exp-transient-quit)

    ;; Show log
    (gptel-exp-show-log)
    (message "✓ Experiment 2 complete - check log for timing details")))

;;; Experiment 3: Naive Integration

(defun gptel-exp-run-experiment-3 ()
  "Attempt straightforward integration of callback with transient suffix.

Expected: May reproduce the original error."
  (interactive)
  (gptel-exp-clear-log)
  (gptel-exp-log 'experiment "Starting Experiment 3: Naive Integration")

  (let ((exp3-fsm (gptel-exp-create-fsm))
        (exp3-callback nil)
        (success-count 0))

    ;; Test 1: Immediate invocation
    (gptel-exp-log 'test "immediate invocation")
    (gptel-exp-fsm-transition exp3-fsm 'WAITING_FOR_TOOL)
    (setq exp3-callback (gptel-exp-create-tool-callback exp3-fsm "transient-tool-1"))
    (gptel-exp-transient-setup 'exp3-prefix)

    (condition-case err
        (progn
          (funcall exp3-callback "immediate-result")
          (gptel-exp-log 'callback-success "immediate")
          (setq success-count (1+ success-count)))
      (error
       (gptel-exp-log 'callback-error (format "immediate: %s" err))))
    (gptel-exp-transient-quit)
    (sleep-for 0.2)

    ;; Test 2: Post-command invocation
    (gptel-exp-log 'test "post-command invocation")
    (gptel-exp-fsm-transition exp3-fsm 'WAITING_FOR_TOOL)
    (setq exp3-callback (gptel-exp-create-tool-callback exp3-fsm "transient-tool-2"))
    (gptel-exp-transient-setup 'exp3-prefix)

    (let ((callback exp3-callback))
      (add-hook 'post-command-hook
                (lambda ()
                  (remove-hook 'post-command-hook (lambda () nil))
                  (gptel-exp-log 'post-command-hook "invoking callback")
                  (condition-case err
                      (progn
                        (funcall callback "post-command-result")
                        (gptel-exp-log 'callback-success "post-command")
                        (setq success-count (1+ success-count)))
                    (error
                     (gptel-exp-log 'callback-error (format "post-command: %s" err)))))
                nil t)
      (run-hooks 'post-command-hook))
    (gptel-exp-transient-quit)
    (sleep-for 0.2)

    ;; Test 3: Timer invocation
    (gptel-exp-log 'test "timer invocation")
    (gptel-exp-fsm-transition exp3-fsm 'WAITING_FOR_TOOL)
    (setq exp3-callback (gptel-exp-create-tool-callback exp3-fsm "transient-tool-3"))
    (gptel-exp-transient-setup 'exp3-prefix)

    (let ((callback exp3-callback))
      (run-with-timer 0.05 nil
                      (lambda ()
                        (gptel-exp-log 'timer-callback "invoking callback")
                        (condition-case err
                            (progn
                              (funcall callback "timer-result")
                              (gptel-exp-log 'callback-success "timer")
                              (setq success-count (1+ success-count)))
                          (error
                           (gptel-exp-log 'callback-error (format "timer: %s" err)))))))
    (gptel-exp-transient-quit)
    (sleep-for 0.2)

    ;; Check final state
    (gptel-exp-log 'final-state (gptel-exp-fsm-state exp3-fsm))
    (gptel-exp-log 'pending-results (gptel-exp-fsm-pending exp3-fsm))
    (gptel-exp-log 'summary (format "%d/3 tests succeeded" success-count))

    ;; Show log
    (gptel-exp-show-log)

    (message "Experiment 3 complete: %d/3 tests succeeded" success-count)))

;;; Experiment 4: Transient Scope Isolation

(defun gptel-exp-run-experiment-4 ()
  "Test if transient's scope interferes with callback execution.

Expected: Callbacks work regardless of transient state."
  (interactive)
  (gptel-exp-clear-log)
  (gptel-exp-log 'experiment "Starting Experiment 4: Transient Scope Isolation")

  ;; Setup transient with complex data in scope
  (gptel-exp-transient-setup 'exp4-prefix)

  ;; Create callback with complex closure data
  (let* ((exp4-fsm (gptel-exp-create-fsm))
         (exp4-complex-data (make-hash-table :test 'equal))
         (exp4-callback nil))

    (puthash "key1" "value1" exp4-complex-data)
    (puthash "key2" '(a b c) exp4-complex-data)

    (setq exp4-callback
          (let ((data exp4-complex-data)
                (fsm exp4-fsm))
            (lambda (result)
              (gptel-exp-log 'callback-start (format "result=%s" result))
              ;; Access closure data
              (gptel-exp-log 'closure-data-access
                             (format "key1=%s key2=%s"
                                     (gethash "key1" data)
                                     (gethash "key2" data)))
              ;; Modify FSM
              (gptel-exp-fsm-transition fsm 'PROCESSING_RESULT)
              (gptel-exp-log 'callback-end "success"))))

    ;; Test 1: Invoke with transient active
    (gptel-exp-log 'test "callback with transient active")
    (condition-case err
        (funcall exp4-callback "test-result-1")
      (error
       (gptel-exp-log 'error (format "with-transient: %s" err))))

    ;; Test 2: Invoke after transient quit
    (gptel-exp-transient-quit)
    (gptel-exp-log 'test "callback after transient quit")
    (condition-case err
        (funcall exp4-callback "test-result-2")
      (error
       (gptel-exp-log 'error (format "after-quit: %s" err))))

    ;; Show log
    (gptel-exp-show-log)
    (message "✓ Experiment 4 complete - check log for scope interference")))

;;; Experiment 5: Callback Execution Context

(defun gptel-exp-run-experiment-5 ()
  "Test if buffer context affects callback execution.

Expected: Closures work across buffers (lexical scoping)."
  (interactive)
  (gptel-exp-clear-log)
  (gptel-exp-log 'experiment "Starting Experiment 5: Callback Execution Context")

  ;; Create FSM in buffer A
  (with-temp-buffer
    (rename-buffer "*exp5-buffer-a*" t)
    (gptel-exp-log 'setup "Creating FSM in buffer A")

    (let ((exp5-fsm (gptel-exp-create-fsm))
          (exp5-buffer-a (current-buffer))
          (exp5-callback (gptel-exp-create-tool-callback
                          (gptel-exp-create-fsm) "test-tool")))

      ;; Test 1: Invoke in same buffer
      (gptel-exp-log 'test "invoke in buffer A (same buffer)")
      (condition-case err
          (progn
            (funcall exp5-callback "result-in-buffer-a")
            (gptel-exp-log 'success "buffer A"))
        (error
         (gptel-exp-log 'error (format "buffer A: %s" err))))

      ;; Test 2: Invoke in different buffer
      (with-temp-buffer
        (rename-buffer "*exp5-buffer-b*" t)
        (gptel-exp-log 'test "invoke in buffer B (different buffer)")
        (condition-case err
            (progn
              (funcall exp5-callback "result-in-buffer-b")
              (gptel-exp-log 'success "buffer B"))
          (error
           (gptel-exp-log 'error (format "buffer B: %s" err)))))

      ;; Test 3: Invoke with explicit buffer context
      (with-temp-buffer
        (rename-buffer "*exp5-buffer-c*" t)
        (gptel-exp-log 'test "invoke in buffer C with-current-buffer wrapper")
        (condition-case err
            (with-current-buffer exp5-buffer-a
              (funcall exp5-callback "result-with-context")
              (gptel-exp-log 'success "buffer C with context"))
          (error
           (gptel-exp-log 'error (format "buffer C: %s" err)))))

      ;; Wait for async processing
      (sleep-for 0.2)))

  ;; Show log
  (gptel-exp-show-log)
  (message "✓ Experiment 5 complete - check log for buffer context issues"))

;;; Experiment 6: FSM State Access Pattern

(defun gptel-exp-run-experiment-6 ()
  "Simulate gptel's actual FSM transition mechanism more closely.

Expected: Callbacks can modify buffer-local state."
  (interactive)
  (gptel-exp-clear-log)
  (gptel-exp-log 'experiment "Starting Experiment 6: FSM State Access Pattern")

  ;; More realistic FSM with buffer-local state
  (let ((exp6-test-buffer (get-buffer-create "*exp6-test*")))

    (with-current-buffer exp6-test-buffer
      (setq-local exp6-fsm-state 'WAITING_FOR_TOOL)
      (setq-local exp6-fsm-pending nil))

    ;; Create callback that modifies buffer-local state
    (let ((exp6-callback
           (let ((buffer exp6-test-buffer))
             (lambda (result tool-name)
               (gptel-exp-log 'callback-invoke
                              (format "tool=%s buffer=%s" tool-name buffer))
               (with-current-buffer buffer
                 ;; Access buffer-local variables
                 (gptel-exp-log 'state-access
                                (format "current-state=%s" exp6-fsm-state))
                 ;; Modify state
                 (setq exp6-fsm-state 'PROCESSING_RESULT)
                 (push (cons tool-name result) exp6-fsm-pending)
                 (gptel-exp-log 'state-modified
                                (format "new-state=%s pending=%s"
                                        exp6-fsm-state
                                        exp6-fsm-pending)))))))

      ;; Test: Invoke from different buffer (like transient would)
      (with-temp-buffer
        (gptel-exp-log 'test "invoke from different buffer")
        (condition-case err
            (progn
              (funcall exp6-callback "test-result" "test-tool")
              (gptel-exp-log 'success "callback executed"))
          (error
           (gptel-exp-log 'error (format "%s" err)))))

      ;; Check state in test buffer
      (with-current-buffer exp6-test-buffer
        (gptel-exp-log 'final-state
                       (format "state=%s pending=%s"
                               exp6-fsm-state
                               exp6-fsm-pending)))

      ;; Cleanup
      (kill-buffer exp6-test-buffer)))

  ;; Show log
  (gptel-exp-show-log)
  (message "✓ Experiment 6 complete - check log for buffer-local state handling"))

;;; Interactive test menu

(defun gptel-exp-run-all ()
  "Run all experiments in sequence."
  (interactive)
  (message "Running all experiments...")
  (gptel-exp-run-experiment-1)
  (sit-for 1)
  (gptel-exp-run-experiment-2)
  (sit-for 1)
  (gptel-exp-run-experiment-3)
  (sit-for 1)
  (gptel-exp-run-experiment-4)
  (sit-for 1)
  (gptel-exp-run-experiment-5)
  (sit-for 1)
  (gptel-exp-run-experiment-6)
  (message "All experiments complete - check *gptel-exp-log* buffer"))

(provide 'gptel-transient-async)
;;; gptel-transient-async.el ends here
