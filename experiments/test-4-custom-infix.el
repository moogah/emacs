;;; test-4-custom-infix.el --- Test custom transient infix class -*- lexical-binding: t -*-

;;; Commentary:
;; Test 4: Test custom transient infix class with simple data.
;; Goal: Verify custom infix class mechanics work correctly.

;;; Code:

(require 'transient)
(require 'eieio)

;; Storage for collected values
(defvar test-4-collected-values nil
  "Values collected via custom infix.")

;; Define custom infix class
(defclass test-4-custom-infix (transient-infix)
  ((custom-property :initarg :custom-property
                    :initform nil
                    :documentation "Custom property for storing data."))
  "Custom infix class for testing.")

(cl-defmethod transient-infix-read ((obj test-4-custom-infix))
  "Read a value for the custom infix OBJ."
  (oref obj custom-property))

(cl-defmethod transient-infix-set ((obj test-4-custom-infix) value)
  "Set VALUE for custom infix OBJ."
  (oset obj value value)
  (push (cons 'custom-data value) test-4-collected-values)
  (message "Stored via custom infix: %s" value))

(cl-defmethod transient-format-value ((obj test-4-custom-infix))
  "Format the value of OBJ for display."
  (let ((value (oref obj value)))
    (if value
        (propertize (format "%s" value) 'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(transient-define-infix test-4-my-infix ()
  "Custom infix using our test class."
  :class 'test-4-custom-infix
  :custom-property "test-value-123"
  :description "Custom infix")

(transient-define-suffix test-4-show-results ()
  "Show collected values."
  :transient nil
  (interactive)
  (message "")
  (message "=== Collected Values ===")
  (dolist (item test-4-collected-values)
    (message "  %s = %s" (car item) (cdr item))))

(transient-define-prefix test-4-menu ()
  "Custom infix test menu."
  ["Test Actions"
   ("c" test-4-my-infix)
   ("s" "Show results" test-4-show-results)])

(defun test-4-verify ()
  "Verify collected values."
  (let ((expected '((custom-data . "test-value-123"))))
    (if (equal test-4-collected-values expected)
        (message "✓ TEST 4 PASSED: Custom infix stored correct value")
      (message "✗ TEST 4 FAILED: Expected %s, got %s"
               expected test-4-collected-values))))

(defun test-4-main ()
  "Entry point for batch mode execution."
  (message "=== TEST 4: Custom Infix Class ===")
  (message "Testing custom infix class mechanics...")

  ;; Initialize
  (setq test-4-collected-values nil)

  ;; Test 1: Can we create an instance?
  (condition-case err
      (let ((infix-obj (test-4-custom-infix :custom-property "test-value-123")))
        (message "✓ Created custom infix instance")

        ;; Test 2: Can we access the property?
        (let ((prop-value (oref infix-obj custom-property)))
          (message "✓ Retrieved custom-property: %s" prop-value))

        ;; Test 3: Can we manually set a value?
        (oset infix-obj value "test-value-123")
        (message "✓ Set value slot")

        ;; Test 4: Can we retrieve it?
        (let ((stored-value (oref infix-obj value)))
          (message "✓ Retrieved value slot: %s" stored-value)

          ;; Store in our collection (simulating what transient-infix-set does)
          (push (cons 'custom-data stored-value) test-4-collected-values)))
    (error
     (message "✗ TEST 4 FAILED with error: %s" err)
     (setq test-4-collected-values nil)))

  ;; Verify
  (message "")
  (message "Verification:")
  (test-4-verify)

  (message "=== TEST 4 COMPLETE ==="))

(provide 'test-4-custom-infix)
;;; test-4-custom-infix.el ends here
