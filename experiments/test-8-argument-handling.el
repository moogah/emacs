;;; test-8-argument-handling.el --- Test string arguments in transient infixes -*- lexical-binding: t -*-

;;; Commentary:
;; Test 8: Verify that using plain strings (not CLI-style "--arg") as
;; transient infix arguments doesn't cause symbol/string confusion.
;;
;; In question-tools, we use question IDs like "q1" as the :argument slot.
;; This test checks if that pattern causes issues.

;;; Code:

(require 'transient)
(require 'eieio)

;; Custom infix that uses a plain string (not "--flag") as argument
(defclass test-8-plain-arg-infix (transient-infix)
  ((data :initarg :data))
  "Infix with plain string argument.")

(cl-defmethod transient-infix-read ((obj test-8-plain-arg-infix))
  "Read value for OBJ."
  (let ((data (oref obj data)))
    (message "Reading value for: %s" data)
    (completing-read "Choose: " '("Option A" "Option B" "Option C") nil t)))

(cl-defmethod transient-infix-set ((obj test-8-plain-arg-infix) value)
  "Set VALUE for OBJ."
  (message "Setting value: %s" value)
  (oset obj value value))

(cl-defmethod transient-format ((obj test-8-plain-arg-infix))
  "Format OBJ for display."
  (let ((value (oref obj value))
        (arg (oref obj argument)))  ;; Try to access argument slot
    (format "  %s → %s (arg: %s)"
            (oref obj data)
            (or value "unset")
            (or arg "none"))))

(defun test-8-show ()
  "Show results and quit."
  (interactive)
  (message "Test complete")
  (transient-quit-one))

(transient-define-prefix test-8-menu ()
  "Test plain string arguments."
  ["Test Infixes with Plain String Arguments"
   ("a" "First"  "plain-id-1"  ;; Plain string, not "--flag"
    :class test-8-plain-arg-infix
    :data "First Question")
   ("b" "Second" "plain-id-2"  ;; Another plain string
    :class test-8-plain-arg-infix
    :data "Second Question")]
  ["Actions"
   ("RET" "Show" test-8-show)])

(defun test-8-main ()
  "Entry point for batch mode execution."
  (message "=== TEST 8: Argument Handling ===")
  (message "Testing plain strings (not CLI-style) as infix arguments...")

  ;; Test: Create infix with plain string argument
  (condition-case err
      (let ((infix (test-8-plain-arg-infix
                   :argument "plain-id-1"  ;; Not "--verbose", just "plain-id-1"
                   :data "Test Data")))
        (message "✓ Created infix with plain string argument")

        ;; Check if argument slot contains the string
        (let ((arg (oref infix argument)))
          (message "Argument slot value: %S" arg)
          (message "Argument type: %s" (type-of arg))

          (if (and (stringp arg) (equal arg "plain-id-1"))
              (message "✓ Argument stored correctly as string")
            (message "✗ Argument not stored as expected: %S" arg)))

        ;; Test if transient-parse-suffixes works with plain string args
        (message "")
        (message "Testing suffix parsing with plain string arguments...")
        (let ((suffixes (transient-parse-suffixes
                        'test-8-menu
                        '(("a" "First" "plain-id-1"
                           :class test-8-plain-arg-infix
                           :data "First Question")))))
          (message "✓ Parsed %d suffix(es)" (length suffixes))

          ;; Check the parsed suffix
          (when suffixes
            (let* ((first-entry (car suffixes))
                   (spec (cdr first-entry))
                   (class-name (car spec))
                   (plist (cadr spec))
                   (arg (plist-get plist :argument)))
              (message "")
              (message "Parsed suffix argument:")
              (message "  Value: %S" arg)
              (message "  Type: %s" (type-of arg))

              (if (and (stringp arg) (equal arg "plain-id-1"))
                  (message "✓ TEST 8 PASSED: Plain string arguments work correctly")
                (message "✗ TEST 8 FAILED: Argument value unexpected: %S" arg))))))
    (error
     (message "✗ TEST 8 FAILED: %s" (error-message-string err))
     (message "Error details: %S" err)))

  (message "=== TEST 8 COMPLETE ==="))

(provide 'test-8-argument-handling)
;;; test-8-argument-handling.el ends here
