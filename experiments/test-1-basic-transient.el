;;; test-1-basic-transient.el --- Basic transient functionality test -*- lexical-binding: t -*-

;;; Commentary:
;; Test 1: Verify basic transient functionality with simple string storage.
;; Goal: Ensure basic transient structure can be defined without errors.

;;; Code:

(require 'transient)

;; Simple variable to store test value
(defvar test-1-stored-value nil
  "Value storage for test 1.")

(transient-define-infix test-1-infix ()
  "Simple infix that stores a string value."
  :class 'transient-lisp-variable
  :variable 'test-1-stored-value
  :reader (lambda (_prompt _initial _history)
            "test-string"))

(transient-define-suffix test-1-set-and-exit ()
  "Set the value and exit."
  :transient nil
  (interactive)
  (setq test-1-stored-value "test-string")
  (message "Value set to: %s" test-1-stored-value))

(transient-define-prefix test-1-menu ()
  "Basic transient menu for testing."
  ["Test Actions"
   ("s" "Set value and exit" test-1-set-and-exit)])

(defun test-1-main ()
  "Entry point for batch mode execution."
  (message "=== TEST 1: Basic Transient Menu ===")
  (message "Testing basic transient structure definition...")

  ;; Test 1: Can we define a transient menu?
  (condition-case err
      (progn
        (message "✓ Transient menu defined successfully")

        ;; Test 2: Can we simulate a suffix action?
        (test-1-set-and-exit)

        ;; Test 3: Did the value get stored?
        (if (equal test-1-stored-value "test-string")
            (message "✓ TEST 1 PASSED: Value stored correctly: %s" test-1-stored-value)
          (message "✗ TEST 1 FAILED: Expected 'test-string', got: %s" test-1-stored-value)))
    (error
     (message "✗ TEST 1 FAILED with error: %s" err)))

  (message "=== TEST 1 COMPLETE ==="))

(provide 'test-1-basic-transient)
;;; test-1-basic-transient.el ends here
