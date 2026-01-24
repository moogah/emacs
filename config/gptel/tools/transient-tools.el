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

(provide 'transient-tools)
;;; transient-tools.el ends here
