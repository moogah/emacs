;;; test-transient-async.el --- Test gptel callback with real transient -*- lexical-binding: t; -*-

;; Test script for gptel async callback integration with transient.
;; Uses REAL transient code, only simulates gptel FSM.
;; Can be run in batch mode without user interaction.

;;; Usage:
;;
;; Interactive:
;;   emacs -Q -l experiments/test-transient-async.el -f test-run-experiment-1
;;
;; Batch mode:
;;   emacs --batch -l experiments/test-transient-async.el -f test-run-experiment-1

;;; Code:

(require 'cl-lib)

;; Load transient if not already loaded
(unless (featurep 'transient)
  (add-to-list 'load-path (expand-file-name "runtime/straight/build/cond-let" default-directory))
  (add-to-list 'load-path (expand-file-name "runtime/straight/build/compat" default-directory))
  (add-to-list 'load-path (expand-file-name "runtime/straight/build/transient" default-directory))
  (require 'transient))

;;; Mock gptel FSM (minimal simulation)

(defvar-local test-gptel--fsm nil
  "Mock gptel FSM for testing. Mimics real gptel--fsm structure.")

(defun test-gptel-init-fsm ()
  "Initialize a mock gptel FSM."
  (setq test-gptel--fsm
        (list :state 'TOOL
              :pending-tool-results '())))

(defun test-gptel-fsm-transition (new-state)
  "Transition FSM to NEW-STATE."
  (message "[FSM] Transition: %s -> %s"
           (plist-get test-gptel--fsm :state)
           new-state)
  (setq test-gptel--fsm
        (plist-put test-gptel--fsm :state new-state)))

(defun test-gptel-create-tool-callback (buffer)
  "Create a mock gptel tool callback for BUFFER.
This simulates the closure created by gptel's process-tool-result."
  (lambda (result)
    (message "[CALLBACK] Invoked with result: %s" result)
    (message "[CALLBACK] Current buffer: %s" (current-buffer))
    (message "[CALLBACK] Target buffer: %s" buffer)
    (message "[CALLBACK] Switching to target buffer...")

    (with-current-buffer buffer
      (message "[CALLBACK] In target buffer: %s" (current-buffer))
      (message "[CALLBACK] FSM bound: %s" (boundp 'test-gptel--fsm))

      (when (boundp 'test-gptel--fsm)
        (message "[CALLBACK] FSM state before: %s" (plist-get test-gptel--fsm :state))

        ;; Store result
        (let ((results (plist-get test-gptel--fsm :pending-tool-results)))
          (setq test-gptel--fsm
                (plist-put test-gptel--fsm :pending-tool-results
                          (cons result results))))

        ;; Transition FSM (this is what's failing in real code)
        (test-gptel-fsm-transition 'WAIT)

        (message "[CALLBACK] FSM state after: %s" (plist-get test-gptel--fsm :state))
        (message "[CALLBACK] Pending results: %s"
                 (plist-get test-gptel--fsm :pending-tool-results))))))

;;; Test transient menu (matches question-tools.el structure)

(defvar-local test-callback nil
  "Buffer-local callback storage (matches question-tools.el pattern).")

(defvar-local test-origin-buffer nil
  "Buffer where tool was invoked (matches question-tools.el pattern).")

(defun test-get-answer (question-id)
  "Get answer for QUESTION-ID from transient scope."
  (let* ((scope (transient-scope))
         (answers (plist-get scope :answers)))
    (gethash question-id answers)))

(defun test-set-answer (question-id answer)
  "Set ANSWER for QUESTION-ID in transient scope."
  (let* ((scope (transient-scope))
         (answers (plist-get scope :answers)))
    (puthash question-id answer answers)))

(defun test-build-result ()
  "Build JSON result from current answers (simplified)."
  (require 'json)
  (let* ((scope (transient-scope))
         (questions (plist-get scope :questions))
         (result '()))
    (dolist (q questions)
      (let* ((id (plist-get q :id))
             (answer (test-get-answer id)))
        (push `((id . ,id)
                (answer . ,(or answer "unanswered")))
              result)))
    (json-encode (nreverse result))))

(defun test-submit ()
  "Submit answers and invoke callback (matches question-tools.el pattern)."
  (interactive)
  (message "[SUBMIT] Building result...")
  (let* ((result-json (test-build-result))
         (callback test-callback)
         (origin-buffer test-origin-buffer))

    (message "[SUBMIT] Result: %s" result-json)
    (message "[SUBMIT] Origin buffer: %s" origin-buffer)
    (message "[SUBMIT] Quitting transient...")

    ;; Close transient FIRST (matches question-tools.el)
    (transient-quit-one)

    (message "[SUBMIT] Scheduling callback via timer...")

    ;; Invoke callback via timer (matches question-tools.el)
    (run-at-time
     0.05 nil
     (lambda ()
       (message "[TIMER] Timer fired")
       (condition-case err
           (progn
             (message "[TIMER] Calling callback...")
             (funcall callback result-json)
             (message "[TIMER] Callback completed successfully"))
         (error
          (message "[TIMER] ERROR: %s" (error-message-string err))
          (message "[TIMER] Error details: %S" err)))))))

(transient-define-prefix test-questions-menu ()
  "Test questions menu (simplified from question-tools.el)."
  [:description "Test Questions"
   [""
    ("RET" "Submit" test-submit)
    ("q" "Quit" transient-quit-one)]])

;;; Test experiments

(defun test-run-experiment-1 ()
  "Experiment 1: Baseline test with real transient, no user input.

Test flow:
1. Create gptel buffer with FSM
2. Create callback closure
3. Set up transient with test data
4. Programmatically fill in answers
5. Call submit directly (simulates user pressing RET)
6. Verify FSM transitions correctly"
  (interactive)
  (message "\n========== EXPERIMENT 1: Baseline Transient + Callback ==========\n")

  ;; Create a test buffer (simulates gptel chat buffer)
  (with-current-buffer (get-buffer-create "*test-gptel-chat*")
    (erase-buffer)
    (insert ";; Mock gptel chat buffer\n")

    ;; Initialize FSM
    (test-gptel-init-fsm)
    (message "[SETUP] Created gptel buffer: %s" (current-buffer))
    (message "[SETUP] FSM initial state: %s" (plist-get test-gptel--fsm :state))

    ;; Create callback
    (let* ((origin-buffer (current-buffer))
           (callback (test-gptel-create-tool-callback origin-buffer)))

      (message "[SETUP] Created callback for buffer: %s" origin-buffer)

      ;; Store callback and origin buffer (matches question-tools.el pattern)
      (setq-local test-callback callback)
      (setq-local test-origin-buffer origin-buffer)

      ;; Set up transient with test questions
      (let* ((questions (list (list :id "q1"
                                   :type "multiple-choice"
                                   :prompt "Test question 1?"
                                   :choices '("A" "B" "C"))
                             (list :id "q2"
                                   :type "text"
                                   :prompt "Test question 2?")))
             (answers (make-hash-table :test 'equal)))

        (message "[SETUP] Initializing transient with %d questions" (length questions))

        ;; Initialize answer hash
        (puthash "q1" nil answers)
        (puthash "q2" nil answers)

        ;; Set up transient
        (transient-setup
         'test-questions-menu
         nil nil
         :scope (list :questions questions
                     :answers answers))

        (message "[SETUP] Transient initialized")
        (message "[SETUP] Current buffer after setup: %s" (current-buffer))

        ;; Programmatically fill in answers (simulates user input)
        (message "[TEST] Filling in answers...")
        (test-set-answer "q1" "B")
        (test-set-answer "q2" "test answer")
        (message "[TEST] Answer q1: %s" (test-get-answer "q1"))
        (message "[TEST] Answer q2: %s" (test-get-answer "q2"))

        ;; Call submit directly (simulates user pressing RET)
        (message "[TEST] Calling submit...")
        (test-submit)

        ;; Wait for timer to fire
        (message "[TEST] Waiting for async callback...")
        (sleep-for 0.2)

        ;; Check results
        (with-current-buffer origin-buffer
          (message "\n[RESULTS] Final FSM state: %s"
                   (plist-get test-gptel--fsm :state))
          (message "[RESULTS] Pending results: %s"
                   (plist-get test-gptel--fsm :pending-tool-results))

          (if (eq (plist-get test-gptel--fsm :state) 'WAIT)
              (message "\n✓ EXPERIMENT 1 PASSED: FSM transitioned correctly")
            (message "\n✗ EXPERIMENT 1 FAILED: FSM in state %s (expected WAIT)"
                     (plist-get test-gptel--fsm :state))))))))

(defun test-run-experiment-2 ()
  "Experiment 2: Test without transient-quit-one.

Tests if calling callback BEFORE quitting transient causes issues."
  (interactive)
  (message "\n========== EXPERIMENT 2: Callback Before Quit ==========\n")

  (with-current-buffer (get-buffer-create "*test-gptel-chat-2*")
    (erase-buffer)
    (test-gptel-init-fsm)
    (message "[SETUP] FSM initial state: %s" (plist-get test-gptel--fsm :state))

    (let* ((origin-buffer (current-buffer))
           (callback (test-gptel-create-tool-callback origin-buffer)))

      (setq-local test-callback callback)
      (setq-local test-origin-buffer origin-buffer)

      (let* ((questions (list (list :id "q1" :prompt "Test?")))
             (answers (make-hash-table :test 'equal)))

        (puthash "q1" nil answers)

        (transient-setup
         'test-questions-menu
         nil nil
         :scope (list :questions questions
                     :answers answers))

        (test-set-answer "q1" "answer")

        ;; Try calling callback BEFORE quitting (different order)
        (message "[TEST] Calling callback BEFORE quit...")
        (let ((result-json (test-build-result)))
          (condition-case err
              (progn
                (funcall callback result-json)
                (message "[TEST] Callback succeeded before quit")
                (transient-quit-one))
            (error
             (message "[TEST] ERROR calling callback before quit: %s"
                      (error-message-string err))
             (transient-quit-one))))

        (sleep-for 0.2)

        (with-current-buffer origin-buffer
          (message "\n[RESULTS] Final FSM state: %s"
                   (plist-get test-gptel--fsm :state))

          (if (eq (plist-get test-gptel--fsm :state) 'WAIT)
              (message "\n✓ EXPERIMENT 2 PASSED")
            (message "\n✗ EXPERIMENT 2 FAILED: FSM in state %s"
                     (plist-get test-gptel--fsm :state))))))))

(defun test-run-experiment-3 ()
  "Experiment 3: Test with immediate callback (no timer).

Tests if timer is necessary or if immediate callback works."
  (interactive)
  (message "\n========== EXPERIMENT 3: Immediate Callback (No Timer) ==========\n")

  (with-current-buffer (get-buffer-create "*test-gptel-chat-3*")
    (erase-buffer)
    (test-gptel-init-fsm)
    (message "[SETUP] FSM initial state: %s" (plist-get test-gptel--fsm :state))

    (let* ((origin-buffer (current-buffer))
           (callback (test-gptel-create-tool-callback origin-buffer)))

      (setq-local test-callback callback)
      (setq-local test-origin-buffer origin-buffer)

      (let* ((questions (list (list :id "q1" :prompt "Test?")))
             (answers (make-hash-table :test 'equal)))

        (puthash "q1" nil answers)

        (transient-setup
         'test-questions-menu
         nil nil
         :scope (list :questions questions
                     :answers answers))

        (test-set-answer "q1" "answer")

        ;; Quit transient first
        (message "[TEST] Quitting transient...")
        (transient-quit-one)

        ;; Call callback IMMEDIATELY (no timer)
        (message "[TEST] Calling callback immediately (no timer)...")
        (let ((result-json (test-build-result)))
          (condition-case err
              (progn
                (funcall callback result-json)
                (message "[TEST] Immediate callback succeeded"))
            (error
             (message "[TEST] ERROR with immediate callback: %s"
                      (error-message-string err)))))

        (with-current-buffer origin-buffer
          (message "\n[RESULTS] Final FSM state: %s"
                   (plist-get test-gptel--fsm :state))

          (if (eq (plist-get test-gptel--fsm :state) 'WAIT)
              (message "\n✓ EXPERIMENT 3 PASSED")
            (message "\n✗ EXPERIMENT 3 FAILED: FSM in state %s"
                     (plist-get test-gptel--fsm :state))))))))

(defun test-run-all ()
  "Run all experiments in sequence."
  (interactive)
  (test-run-experiment-1)
  (sit-for 1)
  (test-run-experiment-2)
  (sit-for 1)
  (test-run-experiment-3)
  (message "\n========== ALL EXPERIMENTS COMPLETE ==========\n"))

(provide 'test-transient-async)
;;; test-transient-async.el ends here
