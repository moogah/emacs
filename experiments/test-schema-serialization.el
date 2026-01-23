;;; test-schema-serialization.el --- Test gptel tool schema serialization -*- lexical-binding: t; -*-

;; Test to prove hypothesis: The error occurs during schema serialization,
;; not during transient/callback execution.
;;
;; Hypothesis:
;; - Error: "Wrong type argument: symbolp, <string>"
;; - Occurs when gptel serializes tool schema with :choices array
;; - Happens during tool registration or API message construction
;; - NOT related to transient, callbacks, or FSM transitions
;;
;; This test will:
;; 1. Register tools with progressively complex schemas
;; 2. Try to trigger schema serialization
;; 3. Isolate the exact schema structure that causes the error
;; 4. Prove it's unrelated to transient

;;; Usage:
;;
;; Interactive:
;;   emacs -Q -l experiments/test-schema-serialization.el
;;   M-x test-schema-run-all
;;
;; Batch mode:
;;   emacs --batch -l experiments/test-schema-serialization.el \
;;         -f test-schema-run-all

;;; Code:

(require 'cl-lib)

;; Try to load gptel
(unless (featurep 'gptel)
  (message "WARNING: gptel not loaded. Loading from runtime/straight/build/...")
  (add-to-list 'load-path (expand-file-name "runtime/straight/build/gptel" default-directory))
  (require 'gptel nil t))

(unless (featurep 'gptel)
  (error "Cannot load gptel - tests require gptel"))

;;; Test Utilities

(defvar test-schema-results '()
  "Test results: list of (test-name . result) where result is 'pass, 'fail, or error message.")

(defun test-schema-record (name result)
  "Record test NAME with RESULT."
  (push (cons name result) test-schema-results)
  (message "[%s] %s" name result))

(defun test-schema-show-results ()
  "Display all test results."
  (message "\n========== TEST RESULTS ==========\n")
  (dolist (result (reverse test-schema-results))
    (let ((name (car result))
          (status (cdr result)))
      (message "%s: %s" name status)))
  (let ((passed (cl-count-if (lambda (r) (eq (cdr r) 'pass)) test-schema-results))
        (total (length test-schema-results)))
    (message "\n%d/%d tests passed" passed total)))

;;; Test 1: Minimal Tool (Baseline)

(defun test-schema-1-minimal ()
  "Test 1: Minimal tool with single string argument.
This should always work."
  (message "\n=== Test 1: Minimal Tool ===")
  (condition-case err
      (progn
        (gptel-make-tool
         :name "test_minimal"
         :function (lambda (callback name)
                     (message "Tool invoked with: %s" name)
                     (funcall callback (format "Hello, %s!" name)))
         :description "Minimal test tool with single string arg"
         :args '((:name "name"
                  :type "string"
                  :description "User's name"))
         :async t)

        (message "✓ Tool registered successfully")
        (test-schema-record 'test-1-minimal 'pass)
        'pass)
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-1-minimal (error-message-string err))
     'fail)))

;;; Test 2: Simple Array

(defun test-schema-2-simple-array ()
  "Test 2: Tool with simple string array argument.
Tests if basic array serialization works."
  (message "\n=== Test 2: Simple Array ===")
  (condition-case err
      (progn
        (gptel-make-tool
         :name "test_simple_array"
         :function (lambda (callback tags)
                     (message "Tool invoked with: %s" tags)
                     (funcall callback "OK"))
         :description "Test tool with string array"
         :args '((:name "tags"
                  :type "array"
                  :items (:type "string")
                  :description "List of tags"))
         :async t)

        (message "✓ Tool registered successfully")
        (test-schema-record 'test-2-simple-array 'pass)
        'pass)
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-2-simple-array (error-message-string err))
     'fail)))

;;; Test 3: Object with String Properties

(defun test-schema-3-object-with-strings ()
  "Test 3: Tool with object argument containing string properties.
Tests nested object serialization."
  (message "\n=== Test 3: Object with String Properties ===")
  (condition-case err
      (progn
        (gptel-make-tool
         :name "test_object_strings"
         :function (lambda (callback question)
                     (message "Tool invoked with: %s" question)
                     (funcall callback "OK"))
         :description "Test tool with object containing strings"
         :args '((:name "question"
                  :type "object"
                  :properties (:id (:type "string")
                              :prompt (:type "string")
                              :type (:type "string"))
                  :description "Question object"))
         :async t)

        (message "✓ Tool registered successfully")
        (test-schema-record 'test-3-object-strings 'pass)
        'pass)
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-3-object-strings (error-message-string err))
     'fail)))

;;; Test 4: Object with Enum

(defun test-schema-4-object-with-enum ()
  "Test 4: Tool with object containing enum field.
Tests if enum serialization works."
  (message "\n=== Test 4: Object with Enum ===")
  (condition-case err
      (progn
        (gptel-make-tool
         :name "test_object_enum"
         :function (lambda (callback question)
                     (message "Tool invoked with: %s" question)
                     (funcall callback "OK"))
         :description "Test tool with object containing enum"
         :args '((:name "question"
                  :type "object"
                  :properties (:type (:type "string"
                                     :enum ["multiple-choice" "yes-no" "text"])
                              :prompt (:type "string"))
                  :description "Question with type enum"))
         :async t)

        (message "✓ Tool registered successfully")
        (test-schema-record 'test-4-object-enum 'pass)
        'pass)
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-4-object-enum (error-message-string err))
     'fail)))

;;; Test 5: Object with Nested Array (The Suspect)

(defun test-schema-5-object-with-array ()
  "Test 5: Object with nested string array property.
This matches the :choices structure from question-tools.
HYPOTHESIS: This is where the error occurs."
  (message "\n=== Test 5: Object with Nested Array (SUSPECT) ===")
  (condition-case err
      (progn
        (gptel-make-tool
         :name "test_object_array"
         :function (lambda (callback question)
                     (message "Tool invoked with: %s" question)
                     (funcall callback "OK"))
         :description "Test tool with object containing array property"
         :args '((:name "question"
                  :type "object"
                  :properties (:prompt (:type "string")
                              :choices (:type "array"
                                       :items (:type "string")
                                       :description "List of choice strings"))
                  :description "Question with choices"))
         :async t)

        (message "✓ Tool registered successfully")
        (test-schema-record 'test-5-object-array 'pass)
        'pass)
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-5-object-array (error-message-string err))
     'fail)))

;;; Test 6: Array of Objects (Full question-tools structure)

(defun test-schema-6-array-of-objects ()
  "Test 6: Array of objects with nested properties.
This is the EXACT structure from question-tools.el.
HYPOTHESIS: This should reproduce the error."
  (message "\n=== Test 6: Array of Objects (FULL STRUCTURE) ===")
  (condition-case err
      (progn
        (gptel-make-tool
         :name "test_array_objects"
         :function (lambda (callback questions)
                     (message "Tool invoked with: %s" questions)
                     (funcall callback "OK"))
         :description "Test tool matching question-tools structure"
         :args '((:name "questions"
                  :type "array"
                  :items (:type "object"
                          :properties (:id (:type "string")
                                      :type (:type "string"
                                            :enum ["multiple-choice" "yes-no" "text"])
                                      :prompt (:type "string")
                                      :choices (:type "array"
                                               :items (:type "string")
                                               :description "For multiple-choice")))
                  :description "Array of question objects"))
         :async t)

        (message "✓ Tool registered successfully")
        (test-schema-record 'test-6-array-objects 'pass)
        'pass)
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-6-array-objects (error-message-string err))
     'fail)))

;;; Test 7: Schema Serialization (Direct Test)

(defun test-schema-7-serialize-directly ()
  "Test 7: Directly test gptel's schema serialization.
Try to call gptel's internal serialization functions."
  (message "\n=== Test 7: Direct Schema Serialization ===")
  (condition-case err
      (let ((test-schema
             '(:name "questions"
               :type "array"
               :items (:type "object"
                       :properties (:choices (:type "array"
                                             :items (:type "string")))))))

        (message "Test schema: %S" test-schema)

        ;; Try to serialize using json-encode
        (require 'json)
        (let ((json (json-encode test-schema)))
          (message "JSON encoded: %s" json))

        ;; Check if gptel has a tool serialization function
        (when (fboundp 'gptel--tools-to-spec)
          (message "Found gptel--tools-to-spec function")
          (let ((spec (gptel--tools-to-spec '("test_array_objects"))))
            (message "Tool spec: %S" spec)))

        (test-schema-record 'test-7-serialize 'pass)
        'pass)
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-7-serialize (error-message-string err))
     'fail)))

;;; Test 8: Actual Tool Invocation (If Possible)

(defun test-schema-8-invoke-tool ()
  "Test 8: Try to invoke a registered tool programmatically.
This tests if error occurs during invocation vs registration."
  (message "\n=== Test 8: Tool Invocation ===")
  (condition-case err
      (progn
        ;; First ensure tool is registered
        (test-schema-5-object-with-array)

        ;; Try to find the tool
        (if (boundp 'gptel--known-tools)
            (progn
              (message "Known tools: %S" (mapcar #'car gptel--known-tools))

              ;; Try to get tool spec
              (let ((tool (assoc "test_object_array" gptel--known-tools)))
                (if tool
                    (progn
                      (message "Tool found: %S" tool)

                      ;; Try to call the function
                      (let ((fn (plist-get (cdr tool) :function)))
                        (if fn
                            (progn
                              (message "Calling tool function...")
                              (funcall fn
                                       (lambda (result)
                                         (message "Tool callback invoked with: %s" result))
                                       '(:prompt "Test?" :choices ["A" "B" "C"]))
                              (message "✓ Tool invoked successfully"))
                          (message "Tool has no :function"))))
                  (message "Tool not found in gptel--known-tools"))))
          (message "gptel--known-tools not bound"))

        (test-schema-record 'test-8-invoke 'pass)
        'pass)
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-8-invoke (error-message-string err))
     'fail)))

;;; Test 9: Inspect Registered Tools

(defun test-schema-9-inspect-tools ()
  "Test 9: Inspect what gptel stores for registered tools.
See if we can find where serialization might fail."
  (message "\n=== Test 9: Inspect Registered Tools ===")
  (condition-case err
      (progn
        (if (boundp 'gptel--known-tools)
            (progn
              (message "gptel--known-tools is bound")
              (message "Number of tools: %d" (length gptel--known-tools))

              (dolist (tool gptel--known-tools)
                (let ((name (car tool))
                      (props (cdr tool)))
                  (message "\nTool: %s" name)
                  (message "  Properties: %S" (mapcar #'car (seq-partition props 2))))))
          (message "gptel--known-tools is NOT bound"))

        (test-schema-record 'test-9-inspect 'pass)
        'pass)
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-9-inspect (error-message-string err))
     'fail)))

;;; Test 10: Message Construction Simulation

(defun test-schema-10-message-construction ()
  "Test 10: Simulate constructing an API message with tool use.
This is where the error likely occurs in real usage."
  (message "\n=== Test 10: Message Construction ===")
  (condition-case err
      (progn
        ;; Register tool first
        (test-schema-6-array-of-objects)

        ;; Try to construct a message that includes tool use
        ;; This simulates what gptel does when sending to API
        (require 'json)

        (let* ((tool-call '((type . "tool_use")
                           (id . "test-123")
                           (name . "test_array_objects")
                           (input . ((questions . [((id . "q1")
                                                    (type . "multiple-choice")
                                                    (prompt . "Pick one")
                                                    (choices . ["Spring - Fresh beginnings"
                                                               "Summer - Warm weather"]))])))))
               (json-str (json-encode tool-call)))

          (message "Tool call JSON: %s" json-str)
          (message "✓ Message construction succeeded")

          (test-schema-record 'test-10-message 'pass)
          'pass))
    (error
     (message "✗ FAILED: %s" (error-message-string err))
     (test-schema-record 'test-10-message (error-message-string err))
     'fail)))

;;; Run All Tests

(defun test-schema-run-all ()
  "Run all schema serialization tests."
  (interactive)
  (message "\n==========================================")
  (message "GPTEL SCHEMA SERIALIZATION TESTS")
  (message "==========================================")
  (message "Hypothesis: Error occurs during schema serialization,")
  (message "not during transient/callback execution.")
  (message "==========================================\n")

  (setq test-schema-results '())

  (test-schema-1-minimal)
  (sit-for 0.1)

  (test-schema-2-simple-array)
  (sit-for 0.1)

  (test-schema-3-object-with-strings)
  (sit-for 0.1)

  (test-schema-4-object-with-enum)
  (sit-for 0.1)

  (test-schema-5-object-with-array)
  (sit-for 0.1)

  (test-schema-6-array-of-objects)
  (sit-for 0.1)

  (test-schema-7-serialize-directly)
  (sit-for 0.1)

  (test-schema-8-invoke-tool)
  (sit-for 0.1)

  (test-schema-9-inspect-tools)
  (sit-for 0.1)

  (test-schema-10-message-construction)
  (sit-for 0.1)

  (test-schema-show-results)

  (message "\n==========================================")
  (message "TESTS COMPLETE")
  (message "=========================================="))

(provide 'test-schema-serialization)
;;; test-schema-serialization.el ends here
