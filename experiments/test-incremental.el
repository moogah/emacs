;;; test-incremental.el --- Incremental tests matching real implementation -*- lexical-binding: t; -*-

;; Incrementally add complexity to match question-tools.el implementation
;; Run tests after each increment to identify exactly what breaks

;;; Usage:
;;
;; Load in real Emacs session (not batch mode for now):
;;   M-x load-file RET experiments/test-incremental.el RET
;;
;; Run tests in order:
;;   M-x test-inc-1-baseline
;;   M-x test-inc-2-json-result
;;   M-x test-inc-3-complex-choices
;;   M-x test-inc-4-real-tool
;;   M-x test-inc-5-real-gptel-callback

;;; Code:

(require 'cl-lib)
(require 'json)

;; Load transient
(unless (featurep 'transient)
  (add-to-list 'load-path (expand-file-name "runtime/straight/build/cond-let" default-directory))
  (add-to-list 'load-path (expand-file-name "runtime/straight/build/compat" default-directory))
  (add-to-list 'load-path (expand-file-name "runtime/straight/build/transient" default-directory))
  (require 'transient))

;; Load gptel if available
(when (file-exists-p (expand-file-name "runtime/straight/build/gptel/gptel.el" default-directory))
  (add-to-list 'load-path (expand-file-name "runtime/straight/build/gptel" default-directory))
  (require 'gptel nil t))

;;; Test Infrastructure

(defvar test-inc-log nil "Test execution log.")

(defun test-inc-log (msg &rest args)
  "Log MSG with ARGS."
  (let ((formatted (apply #'format msg args)))
    (push formatted test-inc-log)
    (message "[TEST] %s" formatted)))

(defun test-inc-clear-log ()
  "Clear test log."
  (setq test-inc-log nil))

(defun test-inc-show-log ()
  "Display test log."
  (with-current-buffer (get-buffer-create "*test-incremental-log*")
    (erase-buffer)
    (insert "# Incremental Test Log\n\n")
    (dolist (entry (reverse test-inc-log))
      (insert entry "\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; Increment 1: Baseline (from test-transient-async.el)

(defvar-local test-inc-callback nil)
(defvar-local test-inc-origin-buffer nil)

(defun test-inc-simple-callback (buffer)
  "Simple callback that just logs."
  (lambda (result)
    (test-inc-log "Callback invoked: %s" result)
    (test-inc-log "Callback succeeded")))

(transient-define-prefix test-inc-menu-1 ()
  "Baseline test menu."
  ["Test"
   ("RET" "Submit" test-inc-submit-1)])

(defun test-inc-submit-1 ()
  "Submit baseline test."
  (interactive)
  (test-inc-log "Submit called")
  (let ((callback test-inc-callback)
        (origin test-inc-origin-buffer))
    (transient-quit-one)
    (run-at-time 0.05 nil
                 (lambda ()
                   (condition-case err
                       (funcall callback "simple-result")
                     (error
                      (test-inc-log "ERROR: %s" (error-message-string err))))))))

(defun test-inc-1-baseline ()
  "Increment 1: Baseline test with simple callback."
  (interactive)
  (test-inc-clear-log)
  (test-inc-log "=== INCREMENT 1: Baseline ===")

  (with-current-buffer (get-buffer-create "*test-inc-buffer*")
    (let ((callback (test-inc-simple-callback (current-buffer))))
      (setq-local test-inc-callback callback)
      (setq-local test-inc-origin-buffer (current-buffer))

      (transient-setup 'test-inc-menu-1)
      (test-inc-submit-1)
      (sleep-for 0.2)

      (test-inc-log "Increment 1: %s"
                    (if (member "Callback succeeded" test-inc-log) "PASS" "FAIL"))
      (test-inc-show-log))))

;;; Increment 2: JSON Result (matches question-tools.el format)

(defun test-inc-json-callback (buffer)
  "Callback expecting JSON string."
  (lambda (result-json)
    (test-inc-log "Callback invoked with JSON: %s" result-json)
    (condition-case err
        (let ((parsed (json-read-from-string result-json)))
          (test-inc-log "Parsed JSON: %S" parsed)
          (test-inc-log "Callback succeeded"))
      (error
       (test-inc-log "JSON parse error: %s" (error-message-string err))))))

(defun test-inc-submit-2 ()
  "Submit with JSON result."
  (interactive)
  (test-inc-log "Submit called")
  (let* ((callback test-inc-callback)
         (origin test-inc-origin-buffer)
         ;; Build JSON like question-tools.el does
         (answer-json (json-encode
                       (list `((id . "q1")
                              (answer . "test-answer")
                              (comment . "")
                              (skipped . :json-false))))))
    (transient-quit-one)
    (run-at-time 0.05 nil
                 (lambda ()
                   (condition-case err
                       (funcall callback answer-json)
                     (error
                      (test-inc-log "ERROR: %s" (error-message-string err))))))))

(transient-define-prefix test-inc-menu-2 ()
  "JSON result test menu."
  ["Test"
   ("RET" "Submit" test-inc-submit-2)])

(defun test-inc-2-json-result ()
  "Increment 2: Test with JSON-encoded result."
  (interactive)
  (test-inc-clear-log)
  (test-inc-log "=== INCREMENT 2: JSON Result ===")

  (with-current-buffer (get-buffer-create "*test-inc-buffer*")
    (let ((callback (test-inc-json-callback (current-buffer))))
      (setq-local test-inc-callback callback)
      (setq-local test-inc-origin-buffer (current-buffer))

      (transient-setup 'test-inc-menu-2)
      (test-inc-submit-2)
      (sleep-for 0.2)

      (test-inc-log "Increment 2: %s"
                    (if (member "Callback succeeded" test-inc-log) "PASS" "FAIL"))
      (test-inc-show-log))))

;;; Increment 3: Complex Choices (like real questions)

(defvar test-inc-question-with-choices
  '(:id "favorite_season"
    :type "multiple-choice"
    :prompt "What is your favorite season?"
    :choices ["Spring - Fresh blooms and mild weather"
             "Summer - Warm days and beach time"
             "Fall - Colorful leaves and cozy vibes"
             "Winter - Snow and holiday cheer"]
    :required t)
  "Test question with long choice strings that cause the error.")

(defun test-inc-complex-callback (buffer question)
  "Callback that has access to question definition (like real gptel).
This simulates the closure capturing the original tool call."
  (lambda (result-json)
    (test-inc-log "Callback invoked with JSON: %s" result-json)
    (test-inc-log "Callback has access to question: %S" question)
    (test-inc-log "Question choices: %S" (plist-get question :choices))

    ;; Try to do something with the choices (simulate what gptel might do)
    (condition-case err
        (let* ((parsed (json-read-from-string result-json))
               (choices (plist-get question :choices)))
          (test-inc-log "Parsed result: %S" parsed)
          (test-inc-log "Choices type: %s" (type-of choices))

          ;; This might be where the error happens - iterating over choices
          (when (vectorp choices)
            (dotimes (i (length choices))
              (let ((choice (aref choices i)))
                (test-inc-log "Choice %d: %s" i choice))))

          (test-inc-log "Callback succeeded"))
      (error
       (test-inc-log "ERROR: %s" (error-message-string err))
       (test-inc-log "Error type: %s" (car err))
       (test-inc-log "Error data: %S" (cdr err))))))

(defun test-inc-submit-3 ()
  "Submit with complex question."
  (interactive)
  (test-inc-log "Submit called")
  (let* ((callback test-inc-callback)
         (origin test-inc-origin-buffer)
         (answer-json (json-encode
                       (list `((id . "favorite_season")
                              (answer . "Winter - Snow and holiday cheer")
                              (comment . "")
                              (skipped . :json-false))))))
    (test-inc-log "Submitting answer: %s" answer-json)
    (transient-quit-one)
    (run-at-time 0.05 nil
                 (lambda ()
                   (condition-case err
                       (with-current-buffer origin
                         (test-inc-log "In origin buffer: %s" (current-buffer))
                         (funcall callback answer-json))
                     (error
                      (test-inc-log "ERROR: %s" (error-message-string err))
                      (test-inc-log "Error type: %s" (car err))
                      (test-inc-log "Error data: %S" (cdr err))))))))

(transient-define-prefix test-inc-menu-3 ()
  "Complex choices test menu."
  ["Test"
   ("RET" "Submit" test-inc-submit-3)])

(defun test-inc-3-complex-choices ()
  "Increment 3: Test with complex choice strings in callback closure."
  (interactive)
  (test-inc-clear-log)
  (test-inc-log "=== INCREMENT 3: Complex Choices ===")

  (with-current-buffer (get-buffer-create "*test-inc-buffer*")
    (let* ((question test-inc-question-with-choices)
           (callback (test-inc-complex-callback (current-buffer) question)))
      (setq-local test-inc-callback callback)
      (setq-local test-inc-origin-buffer (current-buffer))

      (test-inc-log "Question: %S" question)
      (transient-setup 'test-inc-menu-3)
      (test-inc-submit-3)
      (sleep-for 0.2)

      (test-inc-log "Increment 3: %s"
                    (if (member "Callback succeeded" test-inc-log) "PASS" "FAIL"))
      (test-inc-show-log))))

;;; Increment 4: Real Tool Registration

(defun test-inc-4-real-tool ()
  "Increment 4: Register real gptel tool and invoke it."
  (interactive)

  (unless (featurep 'gptel)
    (test-inc-log "ERROR: gptel not loaded - skipping test 4")
    (test-inc-show-log)
    (error "gptel not loaded"))

  (test-inc-clear-log)
  (test-inc-log "=== INCREMENT 4: Real Tool Registration ===")

  ;; Define a minimal test tool
  (test-inc-log "Registering test tool...")

  (gptel-make-tool
   :name "test_simple_question"
   :function (lambda (callback question-text)
               (test-inc-log "Tool invoked: %s" question-text)
               (run-at-time 0.1 nil
                            (lambda ()
                              (condition-case err
                                  (progn
                                    (test-inc-log "Calling callback...")
                                    (funcall callback "test answer"))
                                (error
                                 (test-inc-log "ERROR in callback: %s"
                                              (error-message-string err)))))))
   :description "Test tool with simple string argument"
   :args '((:name "question_text"
            :type "string"
            :description "Question to ask"))
   :async t)

  (test-inc-log "Tool registered successfully")
  (test-inc-log "Increment 4: PASS (if no errors)")
  (test-inc-show-log))

;;; Increment 5: Real Tool with Complex Schema

(defun test-inc-5-complex-schema ()
  "Increment 5: Register tool with complex nested schema like question-tools.el."
  (interactive)

  (unless (featurep 'gptel)
    (test-inc-log "ERROR: gptel not loaded - skipping test 5")
    (test-inc-show-log)
    (error "gptel not loaded"))

  (test-inc-clear-log)
  (test-inc-log "=== INCREMENT 5: Complex Schema ===")

  ;; Define tool with nested array schema
  (test-inc-log "Registering tool with complex schema...")

  (condition-case err
      (progn
        (gptel-make-tool
         :name "test_question_with_choices"
         :function (lambda (callback question)
                     (test-inc-log "Tool invoked with question: %S" question)
                     (test-inc-log "Question choices: %S" (plist-get question :choices))
                     (run-at-time 0.1 nil
                                  (lambda ()
                                    (condition-case err
                                        (progn
                                          (test-inc-log "Calling callback...")
                                          (funcall callback
                                                   (json-encode
                                                    `((id . ,(plist-get question :id))
                                                      (answer . "Winter")))))
                                      (error
                                       (test-inc-log "ERROR in callback: %s"
                                                    (error-message-string err)))))))
         :description "Test tool with multiple-choice question"
         :args '((:name "question"
                  :type "object"
                  :properties (:id (:type "string")
                              :type (:type "string")
                              :prompt (:type "string")
                              :choices (:type "array"
                                       :items (:type "string"))
                              :required (:type "boolean"))))
         :async t)

        (test-inc-log "Tool registered successfully")
        (test-inc-log "Increment 5: PASS"))
    (error
     (test-inc-log "ERROR during tool registration: %s" (error-message-string err))
     (test-inc-log "Increment 5: FAIL")))

  (test-inc-show-log))

;;; Test Runner

(defun test-inc-run-all ()
  "Run all incremental tests in order."
  (interactive)
  (message "\n========== Running Incremental Tests ==========\n")

  (test-inc-1-baseline)
  (sit-for 1)

  (test-inc-2-json-result)
  (sit-for 1)

  (test-inc-3-complex-choices)
  (sit-for 1)

  (when (featurep 'gptel)
    (test-inc-4-real-tool)
    (sit-for 1)

    (test-inc-5-complex-schema)
    (sit-for 1))

  (message "\n========== All Tests Complete ==========\n"))

(provide 'test-incremental)
;;; test-incremental.el ends here
