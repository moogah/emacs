;; -*- lexical-binding: t; -*-

(defun jf/gptel-transient-choice--handle-selection (choice)
  "Handle user selection from transient menu.
CHOICE is the string value to return to the LLM.
Retrieves callback from transient scope and invokes it."
  (let* ((scope (transient-scope))
         (callback (plist-get scope :callback)))
    (when callback
      (funcall callback choice))))

(transient-define-suffix jf/gptel-transient-choice--select-option-a ()
  "Select option A."
  :transient nil  ; Exit menu after execution
  (interactive)
  (jf/gptel-transient-choice--handle-selection "Option A"))

(transient-define-suffix jf/gptel-transient-choice--select-option-b ()
  "Select option B."
  :transient nil
  (interactive)
  (jf/gptel-transient-choice--handle-selection "Option B"))

(transient-define-suffix jf/gptel-transient-choice--cancel ()
  "Cancel the selection."
  :transient nil
  (interactive)
  (jf/gptel-transient-choice--handle-selection "Cancelled"))

(transient-define-prefix jf/gptel-transient-choice-menu ()
  "Simple choice menu for gptel tool demonstration."
  ["Choose an option"
   ("a" "Option A" jf/gptel-transient-choice--select-option-a)
   ("b" "Option B" jf/gptel-transient-choice--select-option-b)
   ("q" "Cancel" jf/gptel-transient-choice--cancel)])

(defun jf/gptel-transient-choice-tool (callback)
  "Async tool that presents a transient menu for user selection.
CALLBACK is called with the user's choice when they select an option.

This is an async tool - it returns immediately after opening the menu.
The callback will be invoked later when the user makes a selection."
  ;; Pass callback via transient scope (supports multiple concurrent invocations)
  (transient-setup 'jf/gptel-transient-choice-menu nil nil
                   :scope (list :callback callback)))

(gptel-make-tool
 :name "transient_choice"
 :function #'jf/gptel-transient-choice-tool
 :description "Present a simple menu to the user with two options (A or B). This is an interactive tool that requires user input. Returns the user's selection as a string: 'Option A', 'Option B', or 'Cancelled' if they quit."
 :args nil  ; No arguments for this minimal example
 :category "demo"
 :async t
 :confirm nil)

(defun jf/gptel-validate-questions-json (json-string)
  "Validate JSON string for question format.
Returns cons cell: (success-p . error-message-or-data)
On success: (t . parsed-plist)
On failure: (nil . \"Error: description\")"
  (condition-case err
      ;; Tier 1: JSON parsing
      (let ((data (json-parse-string json-string :object-type 'plist :array-type 'list)))
        ;; Tier 2: Schema validation
        (let ((schema-result (jf/gptel-validate-questions-schema data)))
          (if (car schema-result)
              ;; Tier 3: Semantic validation
              (let ((semantic-result (jf/gptel-validate-questions-semantics data)))
                (if (car semantic-result)
                    (cons t data)  ; All validation passed
                  semantic-result))
            schema-result)))
    (json-parse-error
     (cons nil (format "Invalid JSON syntax: %s at position %d"
                       (error-message-string err)
                       (if (> (length err) 2) (nth 2 err) 0))))
    (error
     (cons nil (format "Validation error: %s" (error-message-string err))))))

(defun jf/gptel-validate-questions-schema (data)
  "Validate schema structure and types.
Returns (success-p . error-message)."
  (cond
   ;; Check root structure
   ((not (plistp data))
    (cons nil "Root must be an object"))

   ;; Check questions field exists
   ((not (plist-member data :questions))
    (cons nil "Missing required field: questions"))

   ;; Check questions is an array
   ((not (listp (plist-get data :questions)))
    (cons nil "Field 'questions' must be an array"))

   ;; Check questions count (1-4)
   ((let ((count (length (plist-get data :questions))))
      (or (< count 1) (> count 4)))
    (cons nil (format "Must have 1-4 questions, got %d"
                      (length (plist-get data :questions)))))

   ;; Validate each question
   (t
    (let ((questions (plist-get data :questions))
          (idx 0))
      (catch 'validation-error
        (dolist (q questions)
          (let ((result (jf/gptel-validate-question-schema q idx)))
            (unless (car result)
              (throw 'validation-error result)))
          (setq idx (1+ idx)))
        (cons t "Schema valid"))))))

(defun jf/gptel-validate-question-schema (question idx)
  "Validate a single question's schema.
QUESTION is the question plist, IDX is its index in the array.
Returns (success-p . error-message)."
  (let ((prefix (format "Question[%d]: " idx)))
    (cond
     ;; Check question is object
     ((not (plistp question))
      (cons nil (concat prefix "Must be an object")))

     ;; Check required fields
     ((not (plist-member question :id))
      (cons nil (concat prefix "Missing required field: id")))
     ((not (plist-member question :header))
      (cons nil (concat prefix "Missing required field: header")))
     ((not (plist-member question :question))
      (cons nil (concat prefix "Missing required field: question")))
     ((not (plist-member question :type))
      (cons nil (concat prefix "Missing required field: type")))

     ;; Check field types
     ((not (stringp (plist-get question :id)))
      (cons nil (concat prefix "Field 'id' must be a string")))
     ((not (stringp (plist-get question :header)))
      (cons nil (concat prefix "Field 'header' must be a string")))
     ((not (stringp (plist-get question :question)))
      (cons nil (concat prefix "Field 'question' must be a string")))
     ((not (stringp (plist-get question :type)))
      (cons nil (concat prefix "Field 'type' must be a string")))

     ;; Check type value
     ((not (member (plist-get question :type) '("choice" "text")))
      (cons nil (concat prefix (format "Field 'type' must be 'choice' or 'text', got '%s'"
                                       (plist-get question :type)))))

     ;; Validate choice-specific fields
     ((string= (plist-get question :type) "choice")
      (jf/gptel-validate-choice-question question prefix))

     ;; Validate text-specific fields
     ((string= (plist-get question :type) "text")
      (jf/gptel-validate-text-question question prefix))

     (t (cons t "Valid")))))

(defun jf/gptel-validate-choice-question (question prefix)
  "Validate choice question specific fields.
Returns (success-p . error-message)."
  (cond
   ;; Check options field exists
   ((not (plist-member question :options))
    (cons nil (concat prefix "Choice question missing required field: options")))

   ;; Check options is array
   ((not (listp (plist-get question :options)))
    (cons nil (concat prefix "Field 'options' must be an array")))

   ;; Check options count (2-4)
   ((let ((count (length (plist-get question :options))))
      (or (< count 2) (> count 4)))
    (cons nil (concat prefix (format "Choice question must have 2-4 options, got %d"
                                     (length (plist-get question :options))))))

   ;; Validate each option
   (t
    (let ((options (plist-get question :options))
          (opt-idx 0))
      (catch 'option-error
        (dolist (opt options)
          (let ((result (jf/gptel-validate-option-schema opt prefix opt-idx)))
            (unless (car result)
              (throw 'option-error result)))
          (setq opt-idx (1+ opt-idx)))
        (cons t "Valid"))))))

(defun jf/gptel-validate-option-schema (option prefix idx)
  "Validate a single option's schema.
Returns (success-p . error-message)."
  (let ((opt-prefix (concat prefix (format "Option[%d]: " idx))))
    (cond
     ;; Check option is object
     ((not (plistp option))
      (cons nil (concat opt-prefix "Must be an object")))

     ;; Check required fields
     ((not (plist-member option :key))
      (cons nil (concat opt-prefix "Missing required field: key")))
     ((not (plist-member option :label))
      (cons nil (concat opt-prefix "Missing required field: label")))
     ((not (plist-member option :description))
      (cons nil (concat opt-prefix "Missing required field: description")))

     ;; Check field types
     ((not (stringp (plist-get option :key)))
      (cons nil (concat opt-prefix "Field 'key' must be a string")))
     ((not (stringp (plist-get option :label)))
      (cons nil (concat opt-prefix "Field 'label' must be a string")))
     ((not (stringp (plist-get option :description)))
      (cons nil (concat opt-prefix "Field 'description' must be a string")))

     (t (cons t "Valid")))))

(defun jf/gptel-validate-text-question (question prefix)
  "Validate text question specific fields.
Returns (success-p . error-message)."
  (cond
   ;; Check prompt field (optional but must be string if present)
   ((and (plist-member question :prompt)
         (not (stringp (plist-get question :prompt))))
    (cons nil (concat prefix "Field 'prompt' must be a string if present")))

   ;; Check default field (optional but must be string if present)
   ((and (plist-member question :default)
         (not (stringp (plist-get question :default))))
    (cons nil (concat prefix "Field 'default' must be a string if present")))

   (t (cons t "Valid"))))

(defun jf/gptel-validate-questions-semantics (data)
  "Validate semantic constraints.
Returns (success-p . error-message)."
  (let ((questions (plist-get data :questions))
        (seen-ids '()))
    (catch 'semantic-error
      ;; Check for duplicate question IDs
      (let ((idx 0))
        (dolist (q questions)
          (let ((id (plist-get q :id)))
            (when (member id seen-ids)
              (throw 'semantic-error
                     (cons nil (format "Question[%d]: Duplicate question ID '%s'" idx id))))
            (push id seen-ids))

          ;; Check header length (max 12 chars)
          (let ((header (plist-get q :header)))
            (when (> (length header) 12)
              (throw 'semantic-error
                     (cons nil (format "Question[%d]: Header too long (max 12 chars): '%s'"
                                       idx header)))))

          ;; Validate choice question keys
          (when (string= (plist-get q :type) "choice")
            (let ((result (jf/gptel-validate-choice-keys q idx)))
              (unless (car result)
                (throw 'semantic-error result))))

          (setq idx (1+ idx))))

      (cons t "Semantics valid"))))

(defun jf/gptel-validate-choice-keys (question idx)
  "Validate option keys in a choice question.
Returns (success-p . error-message)."
  (let ((options (plist-get question :options))
        (seen-keys '())
        (opt-idx 0)
        (prefix (format "Question[%d]: " idx)))
    (catch 'key-error
      (dolist (opt options)
        (let ((key (plist-get opt :key)))
          ;; Check key is single character
          (unless (= (length key) 1)
            (throw 'key-error
                   (cons nil (concat prefix
                                     (format "Option[%d]: Key must be single character, got '%s'"
                                             opt-idx key)))))

          ;; Check for duplicate keys
          (when (member key seen-keys)
            (throw 'key-error
                   (cons nil (concat prefix
                                     (format "Option[%d]: Duplicate key '%s'"
                                             opt-idx key)))))

          (push key seen-keys))
        (setq opt-idx (1+ opt-idx)))

      (cons t "Keys valid"))))

(defvar-local jf/gptel-ask--answers nil
  "Alist of answers during question flow.
Each entry is (question-id question-text answer key).")

(defun jf/gptel-ask--record-answer (question-id question-text answer key)
  "Record user's answer for QUESTION-ID.
QUESTION-TEXT is the full question, ANSWER is the user's response,
KEY is the option key (for choice questions, nil for text)."
  (setq jf/gptel-ask--answers
        (cons (list question-id question-text answer key)
              (assoc-delete-all question-id jf/gptel-ask--answers))))

(defun jf/gptel-ask--all-answered-p (questions)
  "Check if all QUESTIONS have been answered."
  (= (length jf/gptel-ask--answers)
     (length questions)))

(defun jf/gptel-ask--clear-answers ()
  "Clear all recorded answers."
  (setq jf/gptel-ask--answers nil))

(defun jf/gptel-ask--build-suffixes (questions callback)
  "Build suffix list for QUESTIONS.
CALLBACK is the async callback to invoke when complete.
For Phase 2, we only handle the first question (single choice)."
  (let ((question (car questions)))  ; Phase 2: single question only
    (if (string= (plist-get question :type) "choice")
        (jf/gptel-ask--build-choice-suffixes question callback)
      (error "Text questions not yet implemented (Phase 4)"))))

(defun jf/gptel-ask--build-choice-suffixes (question callback)
  "Build suffix specs for a choice QUESTION.
Returns list of transient suffix specifications."
  (let ((options (plist-get question :options))
        (question-id (plist-get question :id))
        (question-text (plist-get question :question)))
    (mapcar
     (lambda (option)
       (let ((key (plist-get option :key))
             (label (plist-get option :label))
             (description (plist-get option :description)))
         ;; Build suffix spec: (key label command :description desc)
         (list key
               label
               (jf/gptel-ask--make-choice-handler
                question-id question-text option callback)
               :description description)))
     options)))

(defun jf/gptel-ask--make-choice-handler (question-id question-text option callback)
  "Create suffix command for a choice option.
QUESTION-ID identifies the question, QUESTION-TEXT is the full question,
OPTION is the option plist, CALLBACK is the async callback."
  (lambda ()
    (interactive)
    (let ((answer (plist-get option :label))
          (key (plist-get option :key)))
      ;; Record answer
      (jf/gptel-ask--record-answer question-id question-text answer key)

      ;; Phase 2: Single question, so we're done
      ;; Get questions from scope to check completion
      (let* ((scope (transient-scope))
             (questions (plist-get scope :questions)))
        (if (jf/gptel-ask--all-answered-p questions)
            (jf/gptel-ask--finish questions callback)
          ;; Phase 3 will handle multi-question flow here
          (error "Multiple questions not yet implemented"))))))

(defun jf/gptel-ask--finish (questions callback)
  "Build result JSON and invoke CALLBACK.
QUESTIONS is the list of question plists."
  (let ((result (jf/gptel-ask--build-result-json questions jf/gptel-ask--answers)))
    ;; Clean up
    (jf/gptel-ask--clear-answers)
    ;; Exit transient
    (transient-quit-one)
    ;; Invoke callback with JSON result
    (funcall callback result)))

(defun jf/gptel-ask--build-result-json (questions answers)
  "Build JSON result from ANSWERS.
QUESTIONS is the list of question plists for context.
Returns JSON string."
  (let ((answer-list
         (mapcar
          (lambda (q)
            (let* ((q-id (plist-get q :id))
                   (answer-data (assoc q-id answers))
                   (q-text (nth 1 answer-data))
                   (answer (nth 2 answer-data))
                   (key (nth 3 answer-data)))
              (if key
                  ;; Choice question (has key)
                  (list :id q-id
                        :question q-text
                        :answer answer
                        :key key)
                ;; Text question (no key)
                (list :id q-id
                      :question q-text
                      :answer answer))))
          questions)))
    (json-serialize
     (list :status "completed"
           :answers (vconcat answer-list)))))

(transient-define-prefix jf/gptel-ask-menu ()
  "Dynamic question menu for gptel tool."
  [:description
   (lambda ()
     (let* ((scope (transient-scope))
            (questions (plist-get scope :questions))
            (question (car questions)))  ; Phase 2: single question
       (plist-get question :question)))
   [:class transient-column
    :setup-children
    (lambda (_)
      (let* ((scope (transient-scope))
             (questions (plist-get scope :questions))
             (callback (plist-get scope :callback)))
        (transient-parse-suffixes
         'jf/gptel-ask-menu
         (jf/gptel-ask--build-suffixes questions callback))))]])

(defun jf/gptel-ask-user-questions (callback questions-json)
  "Async tool to ask user structured questions via transient menu.
CALLBACK is the async callback (called with JSON result).
QUESTIONS-JSON is the JSON string describing the questions."
  ;; Validate JSON
  (let ((validation (jf/gptel-validate-questions-json questions-json)))
    (if (car validation)
        ;; Validation passed - open menu
        (let ((data (cdr validation))
              (questions (plist-get (cdr validation) :questions)))
          ;; Clear any previous answers
          (jf/gptel-ask--clear-answers)
          ;; Open transient with questions and callback in scope
          (transient-setup 'jf/gptel-ask-menu nil nil
                           :scope (list :questions questions
                                        :callback callback)))
      ;; Validation failed - return error
      (funcall callback (cdr validation)))))

(gptel-make-tool
 :name "ask_user_questions"
 :function #'jf/gptel-ask-user-questions
 :description "Ask the user structured questions via an interactive menu.

This tool presents questions to the user through a visual menu interface.
The user can select from predefined options or provide text input.

Input: JSON string with the following structure:
{
  \"questions\": [
    {
      \"id\": \"unique_id\",
      \"header\": \"Short Label\",
      \"question\": \"Full question text?\",
      \"type\": \"choice\" | \"text\",
      \"options\": [{\"key\": \"a\", \"label\": \"Option A\", \"description\": \"What this means\"}],
      \"prompt\": \"Text prompt: \" (for text type),
      \"default\": \"default value\" (for text type)
    }
  ]
}

Returns: JSON string with answers:
{
  \"status\": \"completed\" | \"cancelled\",
  \"answers\": [
    {\"id\": \"q1\", \"question\": \"...\", \"answer\": \"...\", \"key\": \"a\"}
  ]
}

Constraints:
- 1-4 questions max (Phase 2 supports 1 question only)
- 2-4 options per choice question
- Single character keys
- Headers max 12 characters

Phase 2 status: Single choice question only. Text questions and multiple
questions coming in later phases."
 :args (list '(:name "questions_json"
               :type string
               :description "JSON string containing the questions to ask"))
 :category "interaction"
 :async t
 :confirm nil)

(provide 'transient-tools)
;;; transient-tools.el ends here
