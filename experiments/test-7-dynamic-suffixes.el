;;; test-7-dynamic-suffixes.el --- Test dynamic suffix generation with string arguments -*- lexical-binding: t -*-

;;; Commentary:
;; Test 7: Test dynamic suffix generation using :setup-children.
;; Goal: Reproduce the exact pattern from question-tools where question IDs
;; (strings) are used as the "argument" parameter in suffix definitions.
;; This might be where the symbolp error originates.

;;; Code:

(require 'transient)
(require 'eieio)

;; Storage
(defvar test-7-answers nil
  "Hash table for storing answers.")

;; Custom infix class
(defclass test-7-question-infix (transient-infix)
  ((question-data :initarg :question-data))
  "Infix class for questions with choices.")

(cl-defmethod transient-infix-read ((obj test-7-question-infix))
  "Read answer for question OBJ."
  (let* ((q (oref obj question-data))
         (choices (plist-get q :choices))
         (prompt (plist-get q :prompt)))
    ;; Mock completing-read (would be interactive in real usage)
    (message "Would prompt: %s" prompt)
    (message "Choices: %S" choices)
    ;; Return first choice
    (car choices)))

(cl-defmethod transient-infix-set ((obj test-7-question-infix) value)
  "Set VALUE for question OBJ."
  (let* ((q (oref obj question-data))
         (q-id (plist-get q :id)))
    (oset obj value value)
    (puthash q-id value test-7-answers)
    (message "Stored: %s = %s" q-id value)))

(cl-defmethod transient-format ((obj test-7-question-infix))
  "Format question for display."
  (let* ((q (oref obj question-data))
         (prompt (plist-get q :prompt))
         (value (oref obj value)))
    (format "  %s %s"
            prompt
            (if value
                (propertize (format "→ %s" value) 'face 'success)
              ""))))

(defun test-7-setup-children (_)
  "Generate transient suffixes dynamically.
This mirrors jf/gptel-questions--setup-question-suffixes exactly."
  (let* ((scope (transient-scope))
         (questions (plist-get scope :questions))
         (keys '("a" "b" "c")))

    (transient-parse-suffixes
     'test-7-menu
     (cl-loop for q in questions
              for key in keys
              collect
              (list key
                    ""  ; Description computed by transient-format
                    (plist-get q :id)  ; Use question ID as the argument (STRING!)
                    :class 'test-7-question-infix
                    :question-data q)))))

(defun test-7-show-results ()
  "Show collected answers."
  (interactive)
  (message "")
  (message "=== Results ===")
  (maphash (lambda (k v)
             (message "  %s: %s" k v))
           test-7-answers)
  (transient-quit-one))

(transient-define-prefix test-7-menu ()
  "Dynamic suffix test menu."
  :refresh-suffixes t

  ["Questions"
   ("RET" "Show results" test-7-show-results)]

  [[:class transient-column
    :setup-children test-7-setup-children]])

(defun test-7-main ()
  "Entry point for batch mode execution."
  (message "=== TEST 7: Dynamic Suffix Generation ===")
  (message "Testing :setup-children with string arguments...")

  ;; Initialize
  (setq test-7-answers (make-hash-table :test 'equal))

  ;; Create test questions (mimicking question-tools structure)
  (let ((questions '((:id "q1"
                      :type "multiple-choice"
                      :prompt "Favorite season?"
                      :choices ("Spring - Fresh beginnings and blooming flowers"
                               "Summer - Warm days and long nights"))
                     (:id "q2"
                      :type "multiple-choice"
                      :prompt "Preferred time?"
                      :choices ("Morning - Fresh start"
                               "Evening - Peaceful end")))))

    (message "Questions prepared: %d" (length questions))
    (message "")
    (message "Testing suffix generation...")

    ;; Test suffix generation with string arguments
    (condition-case err
        (let ((suffixes (transient-parse-suffixes
                        'test-7-menu
                        (cl-loop for q in questions
                                 for key in '("a" "b")
                                 collect
                                 (list key
                                       ""
                                       (plist-get q :id)  ; STRING as argument!
                                       :class 'test-7-question-infix
                                       :question-data q)))))
          (message "✓ Generated %d suffixes" (length suffixes))

          ;; Examine the first suffix
          (when (and suffixes (car suffixes))
            (let* ((first-entry (car suffixes))
                   (first-spec (cdr first-entry))
                   (class-name (car first-spec))
                   (plist (cadr first-spec)))
              (message "")
              (message "First suffix entry:")
              (message "  Class: %s" class-name)
              (message "  Plist: %S" plist)

              ;; Extract the argument from the plist
              (let ((arg (plist-get plist :argument)))
                (message "")
                (message "Argument from plist:")
                (message "  Value: %S" arg)
                (message "  Type: %s" (type-of arg))

                ;; This is the KEY test: Is the argument a string?
                (if (stringp arg)
                    (message "✓ Argument is a string (as expected)")
                  (message "✗ Argument is NOT a string: %s" (type-of arg))))))

          (message "")
          (message "✓ TEST 7 PASSED: Suffix generation completed"))
      (error
       (message "✗ TEST 7 FAILED: %s" (error-message-string err)))))

  (message "=== TEST 7 COMPLETE ==="))

(provide 'test-7-dynamic-suffixes)
;;; test-7-dynamic-suffixes.el ends here
