;; Example question plist:
;; (:id "question-1"                    ; Unique identifier (required)
;;  :type "multiple-choice"             ; Question type (required)
;;  :prompt "What is your goal?"        ; Question text (required)
;;  :description "Optional context"     ; Additional detail (optional)
;;  :required t                         ; Whether answer required (default: t)
;;  :default "value"                    ; Default value (optional)
;;  :choices ("Option A" "Option B")    ; For multiple-choice (required if type=multiple-choice)
;;  :min 0                              ; For numeric (optional)
;;  :max 100)                           ; For numeric (optional)

;; Example answer plist returned to LLM:
;; (:id "question-1"                    ; Question identifier
;;  :answer "Option A"                  ; User's answer value
;;  :comment "My reasoning here"        ; Optional user comment
;;  :skipped nil)                       ; Whether skipped (if not required)

;; Scope structure stored in transient:
;; (:questions (list ...)               ; Original questions from LLM
;;  :answers (hash-table)               ; ID → answer value mapping
;;  :comments (hash-table)              ; ID → comment string mapping
;;  :callback (function)                ; Async callback
;;  :current-question "question-1")    ; Currently selected (for future nav features)

(defun jf/gptel-questions--answer-question (question-id answer)
  "Store ANSWER for QUESTION-ID in current scope."
  (let* ((scope (transient-scope))
         (answers (plist-get scope :answers)))
    (puthash question-id answer answers)))

(defun jf/gptel-questions--set-comment (question-id comment)
  "Store COMMENT for QUESTION-ID in current scope."
  (let* ((scope (transient-scope))
         (comments (plist-get scope :comments)))
    (puthash question-id comment comments)))

(defun jf/gptel-questions--get-answer (question-id)
  "Get current answer for QUESTION-ID from scope."
  (let* ((scope (transient-scope))
         (answers (plist-get scope :answers)))
    (gethash question-id answers)))

(defun jf/gptel-questions--get-comment (question-id)
  "Get current comment for QUESTION-ID from scope."
  (let* ((scope (transient-scope))
         (comments (plist-get scope :comments)))
    (gethash question-id comments)))

(defun jf/gptel-questions--is-answered-p (question-id)
  "Check if QUESTION-ID has been answered."
  (let ((answer (jf/gptel-questions--get-answer question-id)))
    (not (null answer))))

(defun jf/gptel-questions--validate-answers ()
  "Check if all required questions are answered.
Returns (valid-p . error-message)."
  (let* ((scope (transient-scope))
         (questions (plist-get scope :questions))
         (unanswered-required '()))

    (dolist (q questions)
      (let ((id (plist-get q :id))
            (required (if (plist-member q :required)
                          (plist-get q :required)
                        t)))  ; default to required
        (when (and required
                   (not (jf/gptel-questions--is-answered-p id)))
          (push (plist-get q :prompt) unanswered-required))))

    (if unanswered-required
        (cons nil (format "Required questions unanswered: %s"
                         (string-join (nreverse unanswered-required) ", ")))
      (cons t nil))))

(defun jf/gptel-questions--build-answer-list ()
  "Build answer list from current state for callback."
  (let* ((scope (transient-scope))
         (questions (plist-get scope :questions))
         (answers-hash (plist-get scope :answers))
         (comments-hash (plist-get scope :comments))
         (answer-list '()))

    (dolist (q questions)
      (let* ((id (plist-get q :id))
             (answer (gethash id answers-hash))
             (comment (gethash id comments-hash))
             (skipped (null answer)))
        (push (list :id id
                   :answer (or answer "")
                   :comment (or comment "")
                   :skipped skipped)
              answer-list)))

    (nreverse answer-list)))

(defun jf/gptel-questions--add-or-edit-comment ()
  "Interactively select a question and add/edit its comment."
  (interactive)
  (let* ((scope (transient-scope))
         (questions (plist-get scope :questions))
         (choices (mapcar (lambda (q)
                           (let* ((id (plist-get q :id))
                                  (prompt (plist-get q :prompt))
                                  (has-comment (not (string-empty-p
                                                    (jf/gptel-questions--get-comment id))))
                                  (indicator (if has-comment "💬 " "")))
                             (cons (format "%s%s" indicator prompt) id)))
                         questions))
         (selection (completing-read "Add/edit comment for: " choices nil t))
         (question-id (cdr (assoc selection choices))))

    (when question-id
      (let* ((current-comment (jf/gptel-questions--get-comment question-id))
             (new-comment (read-string "Comment: " current-comment)))
        (jf/gptel-questions--set-comment question-id new-comment)
        (message "Comment saved for: %s" selection)))))

(defun jf/gptel-questions--format-question (question key)
  "Format QUESTION for display in transient menu with KEY binding."
  (let* ((prompt (plist-get question :prompt))
         (required (if (plist-member question :required)
                       (plist-get question :required)
                     t))
         (id (plist-get question :id))
         (answered (jf/gptel-questions--is-answered-p id))
         (answer (jf/gptel-questions--get-answer id))
         (comment (jf/gptel-questions--get-comment id))
         (has-comment (and comment (not (string-empty-p comment))))
         (indicator (cond
                     (answered (propertize "✓" 'face 'success))
                     (required (propertize "●" 'face 'error))
                     (t (propertize "○" 'face 'transient-inactive-value))))
         (comment-indicator (if has-comment
                                (propertize " 💬" 'face 'transient-value)
                              ""))
         (key-str (propertize (format "[%s]" key) 'face 'transient-key)))

    (format "%s %s %s%s%s"
            key-str
            indicator
            prompt
            (if answered
                (propertize (format " → %s" answer) 'face 'transient-value)
              "")
            comment-indicator)))

(defun jf/gptel-questions--progress-info ()
  "Generate progress indicator string."
  (let* ((scope (transient-scope))
         (questions (plist-get scope :questions))
         (total (length questions))
         (answered (cl-count-if
                    (lambda (q)
                      (jf/gptel-questions--is-answered-p (plist-get q :id)))
                    questions))
         (face (if (= answered total) 'success 'transient-value)))
    (propertize (format "Progress: %d/%d answered" answered total)
                'face face)))

(defun jf/gptel-questions--submit ()
  "Validate and submit answers."
  (interactive)
  (let ((validation (jf/gptel-questions--validate-answers)))
    (if (car validation)
        (let* ((scope (transient-scope))
               (callback (plist-get scope :callback))
               (answer-list (jf/gptel-questions--build-answer-list)))

          ;; Invoke callback
          (funcall callback answer-list)

          ;; Close transient
          (transient-quit-one))

      ;; Validation failed
      (message "%s" (cdr validation)))))

(defun jf/gptel-questions--cancel ()
  "Cancel and return partial answers to LLM."
  (interactive)
  (let* ((scope (transient-scope))
         (callback (plist-get scope :callback))
         (answer-list (jf/gptel-questions--build-answer-list)))

    ;; Invoke callback with partial answers
    (funcall callback answer-list)

    ;; Close transient
    (transient-quit-one)))

(defclass jf/gptel-question-infix (transient-infix)
  ((question :initarg :question))
  "Infix class for question answering.")

(cl-defmethod transient-infix-read ((obj jf/gptel-question-infix))
  "Read answer for question OBJ based on question type."
  (let* ((q (oref obj question))
         (q-type (plist-get q :type))
         (q-id (plist-get q :id))
         (prompt (plist-get q :prompt))
         (default (plist-get q :default))
         (current (jf/gptel-questions--get-answer q-id)))

    (pcase q-type
      ("multiple-choice"
       (let ((choices (plist-get q :choices)))
         (completing-read (format "%s: " prompt)
                         choices nil t (or current default))))

      ("yes-no"
       (if (y-or-n-p (format "%s " prompt)) "yes" "no"))

      ("text"
       (read-string (format "%s: " prompt) (or current default)))

      ("numeric"
       (let* ((min (plist-get q :min))
              (max (plist-get q :max))
              (default-val (or (and current (string-to-number current))
                              default
                              0))
              (input (read-number (format "%s: " prompt) default-val)))
         (when (and min (< input min))
           (error "Value must be at least %d" min))
         (when (and max (> input max))
           (error "Value must be at most %d" max))
         (number-to-string input)))

      (_ (error "Unknown question type: %s" q-type)))))

(cl-defmethod transient-infix-set ((obj jf/gptel-question-infix) value)
  "Set VALUE as answer for question OBJ."
  (let ((q-id (plist-get (oref obj question) :id)))
    (jf/gptel-questions--answer-question q-id value)
    (oset obj value value)))

(cl-defmethod transient-format ((obj jf/gptel-question-infix))
  "Format question and answer for display."
  (let* ((q (oref obj question))
         (key (oref obj key))
         (formatted (jf/gptel-questions--format-question q key)))
    (format "  %s" formatted)))

(defun jf/gptel-questions--generate-key-pool (count)
  "Generate COUNT key bindings for questions.
Uses single letters (a-z) for first 26, then two-letter combinations (aa-zz)."
  (let ((keys '())
        (letters "abcdefghijklmnopqrstuvwxyz"))
    ;; Single letters: a-z (26 keys)
    (dotimes (i (min count 26))
      (push (substring letters i (1+ i)) keys))

    ;; Two letters: aa-zz (676 more keys if needed)
    (when (> count 26)
      (dotimes (i (min (- count 26) 676))
        (let ((first (/ i 26))
              (second (mod i 26)))
          (push (format "%c%c"
                        (aref letters first)
                        (aref letters second))
                keys))))

    (nreverse keys)))

(defun jf/gptel-questions--setup-question-suffixes (_)
  "Generate transient suffixes for each question."
  (let* ((scope (transient-scope))
         (questions (plist-get scope :questions))
         (key-pool (jf/gptel-questions--generate-key-pool (length questions))))

    (transient-parse-suffixes
     'jf/gptel-questions-menu
     (cl-loop for q in questions
              for key in key-pool
              collect
              (list key
                    ""  ; Description computed dynamically by transient-format
                    (plist-get q :id)  ; Use question ID as the argument
                    :class 'jf/gptel-question-infix
                    :question q)))))

(transient-define-prefix jf/gptel-questions-menu ()
  "Answer questions from LLM agent."
  :refresh-suffixes t

  ;; Top section: Info and actions
  [:description "Answer Questions"
   [""
    (:info (lambda () (jf/gptel-questions--progress-info)))]
   [""
    ("RET" "Submit answers" jf/gptel-questions--submit)
    ("C" "Add/edit comment" jf/gptel-questions--add-or-edit-comment
     :transient t)
    ("q" "Cancel (send partial)" jf/gptel-questions--cancel)]]

  ;; Questions section: dynamically generated
  [[:class transient-column
    :setup-children jf/gptel-questions--setup-question-suffixes]])

(defun jf/gptel-questions--ask (callback questions)
  "Async tool function. Display questions UI and invoke CALLBACK with answers.
QUESTIONS is array of question plists from LLM."
  (let ((answers (make-hash-table :test 'equal))
        (comments (make-hash-table :test 'equal)))
    ;; Initialize hash tables
    (dolist (q questions)
      (puthash (plist-get q :id) nil answers)
      (puthash (plist-get q :id) "" comments))

    ;; Launch transient with scope
    (transient-setup
     'jf/gptel-questions-menu
     nil nil
     :scope (list :questions questions
                  :answers answers
                  :comments comments
                  :callback callback
                  :current-question (plist-get (car questions) :id)))))

(gptel-make-tool
 :name "ask_questions"
 :function #'jf/gptel-questions--ask

 :description "Ask user structured questions and collect answers interactively.

Present multiple questions in an interactive menu where user can:
- Answer in any order
- Add optional comments/notes to answers
- See progress (answered vs total)
- Review and change answers before submission
- Submit when ready or cancel to return partial answers

Question types supported:
- multiple-choice: User selects from predefined choices list
- yes-no: Simple yes/no question
- text: Free-form text input
- numeric: Numeric input with optional min/max validation

Use this when you need user input to refine your plan or make decisions.
For example, ask about goals, constraints, preferences, or technical choices."

 :args '((:name "questions"
          :type array
          :items (:type object
                  :properties (:id (:type string
                                   :description "Unique question identifier")
                              :type (:type string
                                    :enum ["multiple-choice" "yes-no" "text" "numeric"]
                                    :description "Question type")
                              :prompt (:type string
                                      :description "Question text to display")
                              :description (:type string
                                           :description "Optional additional context")
                              :required (:type boolean
                                        :description "Whether answer is required (default: true)")
                              :default (:type string
                                       :description "Default answer value")
                              :choices (:type array
                                       :items (:type string)
                                       :description "For multiple-choice: list of options")
                              :min (:type number
                                   :description "For numeric: minimum value")
                              :max (:type number
                                   :description "For numeric: maximum value")))
          :description "Array of question objects to ask user"))

 :category "interaction"
 :async t
 :confirm nil)

(defun jf/gptel-questions-test-basic ()
  "Test basic functionality with text, yes-no, and multiple-choice."
  (interactive)
  (jf/gptel-questions--ask
   (lambda (answers) (message "Answers: %S" answers))
   '((:id "test1" :type "text" :prompt "Your name?" :required t)
     (:id "test2" :type "yes-no" :prompt "Proceed?")
     (:id "test3" :type "multiple-choice"
      :prompt "Choose color"
      :choices ("Red" "Blue" "Green")))))

(defun jf/gptel-questions-test-numeric ()
  "Test numeric validation with min/max."
  (interactive)
  (jf/gptel-questions--ask
   (lambda (answers) (message "Answers: %S" answers))
   '((:id "age" :type "numeric" :prompt "Age?" :min 0 :max 120 :required t))))

(defun jf/gptel-questions-test-optional ()
  "Test optional fields with defaults."
  (interactive)
  (jf/gptel-questions--ask
   (lambda (answers) (message "Answers: %S" answers))
   '((:id "opt1" :type "text" :prompt "Optional field"
      :required nil :default "Default value"))))

(defun jf/gptel-questions-test-large ()
  "Test with 15 questions to verify scrolling."
  (interactive)
  (jf/gptel-questions--ask
   (lambda (answers) (message "Got %d answers" (length answers)))
   (cl-loop for i from 1 to 15
            collect (list :id (format "q%d" i)
                         :type "text"
                         :prompt (format "Question %d?" i)
                         :required nil))))

(defun jf/gptel-questions-test-realistic ()
  "Test with realistic planning scenario questions."
  (interactive)
  (jf/gptel-questions--ask
   (lambda (answers)
     (message "Planner received %d answers" (length answers))
     (dolist (a answers)
       (message "  %s: %s%s"
                (plist-get a :id)
                (plist-get a :answer)
                (if (string-empty-p (plist-get a :comment))
                    ""
                  (format " [%s]" (plist-get a :comment))))))
   '((:id "primary_goal"
      :type "multiple-choice"
      :prompt "What is your primary goal for this feature?"
      :description "This helps me prioritize design trade-offs"
      :choices ("Performance" "Readability" "Maintainability" "Simplicity")
      :required t)
     (:id "refactor_existing"
      :type "yes-no"
      :prompt "Should I refactor existing code or leave it as-is?"
      :required t)
     (:id "deadline"
      :type "text"
      :prompt "When do you need this completed?"
      :required nil
      :default "No specific deadline")
     (:id "complexity_tolerance"
      :type "numeric"
      :prompt "Complexity tolerance (1=simple, 10=complex)"
      :description "Higher values mean more sophisticated solutions"
      :min 1
      :max 10
      :default "5"
      :required nil))))

(provide 'jf/gptel-questions)
