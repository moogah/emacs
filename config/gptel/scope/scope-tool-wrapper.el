;;; scope-tool-wrapper.el --- Thin macro for scope-aware tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Provides gptel-make-scoped-tool macro.  The macro's only job is to
;; bind arguments, hand them to the scope authorization dispatcher,
;; and run the body on allow or deliver the denial response through
;; the async callback.  No validation logic, no config loading, no
;; metadata gathering, no expansion-UI handling, no error formatting
;; lives here — the dispatcher (`jf/gptel-scope-authorize-tool-call`
;; in scope-validation) owns all of that.
;;
;; Scoped tools are always async because the expansion UI resolves on
;; a later turn.  Synchronous scoped tools would be unable to escalate
;; denials to the user, so they are not supported; if a tool doesn't
;; need scope control, register it with `gptel-make-tool' directly.

;;; Code:

;; Dependencies


;; [[file:scope-tool-wrapper.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'gptel-request)
(require 'jf-gptel-scope-validation)
;; Dependencies:1 ends here

;; Scoped Tool Macro

;; The single keyword option =:operation SYM= declares the filesystem
;; operation (=read=, =write=, =modify=, =execute=). Omit for tools whose
;; operations must be extracted from input (e.g. bash, whose file ops are
;; parsed from the command).

;; The macro does no validation reasoning of its own. It delegates to
;; =jf/gptel-scope-authorize-tool-call= and supplies an on-allow thunk
;; that runs the body plus an on-deny thunk that delivers the denial
;; response through the gptel async callback.


;; [[file:scope-tool-wrapper.org::*Scoped Tool Macro][Scoped Tool Macro:1]]
(defmacro gptel-make-scoped-tool (name description args &rest rest)
  "Create an async scope-aware gptel tool with automatic validation.

NAME is the tool name string.
DESCRIPTION is the tool description for the LLM.
ARGS is the list of argument specs.

REST is parsed as an optional :operation keyword followed by BODY:
  [:operation SYM]  declared operation (read/write/modify/execute).
                    Omit for tools whose operations are extracted from
                    input (e.g. bash).

Scoped tools are always async so the expansion UI can resolve on a
later turn."
  (let (operation)
    (while (keywordp (car rest))
      (pcase (car rest)
        (:operation (setq operation (cadr rest)
                          rest (cddr rest)))
        (kw (error "gptel-make-scoped-tool: unknown keyword %S" kw))))
    (let* ((body rest)
           (arg-names (mapcar (lambda (arg-spec)
                                (intern (plist-get arg-spec :name)))
                              (eval args))))
      `(gptel-make-tool
        :name ,name
        :description ,description
        :args ,args
        :category "scope"
        :async t
        :function
        (lambda (callback ,@arg-names)
          (condition-case err
              (jf/gptel-scope-authorize-tool-call
               ,name ',operation (list ,@arg-names)
               (lambda ()
                 (funcall callback (json-serialize (progn ,@body))))
               (lambda (deny-response)
                 (funcall callback (json-serialize deny-response))))
            (error
             (funcall callback
                      (json-serialize
                       (list :success nil
                             :error "tool_exception"
                             :message (format "Tool error: %s" (error-message-string err))))))))))))
;; Scoped Tool Macro:1 ends here

;; Provide Feature


;; [[file:scope-tool-wrapper.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-tool-wrapper)
;;; scope-tool-wrapper.el ends here
;; Provide Feature:1 ends here
