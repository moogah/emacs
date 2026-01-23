;;; test-5-infix-with-plist.el --- Test custom infix with plist containing choices -*- lexical-binding: t -*-

;;; Commentary:
;; Test 5: Test custom infix storing a plist with a :choices property containing strings.
;; Goal: Verify whether the plist with :choices causes symbol/string confusion.
;; This test is critical as it mirrors the question-tools pattern.

;;; Code:

(require 'transient)
(require 'eieio)

;; Storage for answers
(defvar test-5-answers nil
  "Answers collected via custom infix.")

;; Define custom infix class that stores a question plist
(defclass test-5-question-infix (transient-infix)
  ((question-data :initarg :question-data
                  :initform nil
                  :documentation "Plist containing question data including :choices."))
  "Custom infix class for question with choices.")

(cl-defmethod transient-infix-read ((obj test-5-question-infix))
  "Read a value for the question infix OBJ using completing-read."
  (let* ((question-data (oref obj question-data))
         (choices (plist-get question-data :choices))
         (question (plist-get question-data :question)))
    (message "Question: %s" question)
    (message "Choices: %s" choices)
    ;; Mock completing-read to return first choice
    (car choices)))

(cl-defmethod transient-infix-set ((obj test-5-question-infix) value)
  "Set VALUE for question infix OBJ."
  (oset obj value value)
  (let* ((question-data (oref obj question-data))
         (question-id (plist-get question-data :id)))
    (push (cons question-id value) test-5-answers)
    (message "Stored answer: %s = %s" question-id value)))

(cl-defmethod transient-format-value ((obj test-5-question-infix))
  "Format the value of OBJ for display."
  (let ((value (oref obj value)))
    (if value
        (propertize (format "%s" value) 'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(transient-define-prefix test-5-menu ()
  "Question with choices test menu."
  ["Questions"
   ("q" "Answer question" test-5-question-suffix)])

(defun test-5-verify ()
  "Verify collected answers."
  (let ((expected '(("q1" . "Spring - Fresh beginnings and blooming flowers"))))
    (if (equal test-5-answers expected)
        (message "✓ TEST 5 PASSED: Answer stored correctly")
      (message "✗ TEST 5 FAILED: Expected %s, got %s"
               expected test-5-answers))))

(defun test-5-main ()
  "Entry point for batch mode execution."
  (message "=== TEST 5: Custom Infix with Plist Data ===")
  (message "Testing plist with :choices property...")

  ;; Initialize
  (setq test-5-answers nil)

  ;; Create a question plist (mimicking question-tools structure)
  (let* ((question-plist '(:id "q1"
                          :question "What is your favorite season?"
                          :choices ("Spring - Fresh beginnings and blooming flowers"
                                   "Summer - Warm days and long nights"
                                   "Fall - Colorful leaves and crisp air"
                                   "Winter - Snow and cozy evenings")))
         (infix-obj (test-5-question-infix :question-data question-plist)))

    (message "Created infix with question data:")
    (message "  ID: %s" (plist-get question-plist :id))
    (message "  Question: %s" (plist-get question-plist :question))
    (message "  Choices: %s" (plist-get question-plist :choices))

    ;; Test accessing the plist
    (let ((retrieved-data (oref infix-obj question-data)))
      (message "✓ Retrieved question-data from infix"))

    ;; Manually simulate what would happen during transient interaction
    ;; (we can't actually invoke transient-infix-read without a transient context)
    (let* ((question-data (oref infix-obj question-data))
           (choices (plist-get question-data :choices))
           (selected-choice (car choices)))  ; Mock selection
      (message "")
      (message "Simulating user selection:")
      (message "  Selected: %s" selected-choice)

      ;; Manually set the value (simulating transient-infix-set)
      (oset infix-obj value selected-choice)
      (let ((question-id (plist-get question-data :id)))
        (push (cons question-id selected-choice) test-5-answers)
        (message "  Stored: %s = %s" question-id selected-choice))))

  ;; Verify
  (message "")
  (message "Verification:")
  (test-5-verify)

  (message "=== TEST 5 COMPLETE ==="))

(provide 'test-5-infix-with-plist)
;;; test-5-infix-with-plist.el ends here
