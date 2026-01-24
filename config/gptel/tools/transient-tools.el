;; -*- lexical-binding: t; -*-

(defvar jf/gptel-transient-choice-callback nil
  "Callback for the transient choice tool.
Set when tool is invoked, cleared after selection is made.")

(defun jf/gptel-transient-choice--handle-selection (choice)
  "Handle user selection from transient menu.
CHOICE is the string value to return to the LLM.
Invokes the stored callback and clears it to prevent double-invocation."
  (when jf/gptel-transient-choice-callback
    (funcall jf/gptel-transient-choice-callback choice)
    (setq jf/gptel-transient-choice-callback nil)))

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
  ;; Store callback for suffix commands to access
  (setq jf/gptel-transient-choice-callback callback)
  ;; Open transient menu (returns immediately, menu stays open)
  (jf/gptel-transient-choice-menu))

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
