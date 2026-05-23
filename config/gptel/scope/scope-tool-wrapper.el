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

;; JSON-Serialization Sanitizer

;; =json-serialize= signals =wrong-type-argument json-value-p= when a
;; string leaf contains raw bytes that are not valid UTF-8 — e.g.,
;; subprocess output captured by =run_bash_command= when the underlying
;; command emits non-UTF-8 bytes (binary file content, broken locale,
;; encoding mismatch).  Without intervention the wrapper's outer
;; =condition-case= would catch that error and return a
;; =tool_exception= payload to the model, killing the assistant turn
;; with an opaque diagnostic.

;; Sanitization decodes any offending bytes as Latin-1 — a coding system
;; that maps every byte 0..255 to a valid Unicode code point, so the
;; re-encoded result is always JSON-serializable.  Multibyte UTF-8
;; content is preserved (the sanitizer only descends into a string when
;; =json-serialize= rejected it).


;; [[file:scope-tool-wrapper.org::*JSON-Serialization Sanitizer][JSON-Serialization Sanitizer:1]]
(defun jf/gptel-scope--sanitize-for-json (value)
  "Return VALUE with embedded strings made safe for `json-serialize'.
Recursively descends into plists, lists, and vectors.  String leaves
that `json-serialize' rejects (typically because they contain raw
bytes that are not valid UTF-8) are decoded as Latin-1 so every byte
maps to a valid Unicode code point.  Other leaves pass through."
  (cond
   ((stringp value)
    (condition-case nil
        (progn (json-serialize value) value)
      (wrong-type-argument
       (decode-coding-string
        (if (multibyte-string-p value)
            (encode-coding-string value 'utf-8 t)
          value)
        'latin-1))))
   ((vectorp value)
    (vconcat (mapcar #'jf/gptel-scope--sanitize-for-json
                     (append value nil))))
   ((and (consp value) (keywordp (car value)))
    ;; plist — walk key/value pairs, preserve keys verbatim
    (cl-loop for (k v) on value by #'cddr
             append (list k (jf/gptel-scope--sanitize-for-json v))))
   ((consp value)
    (mapcar #'jf/gptel-scope--sanitize-for-json value))
   (t value)))

(defun jf/gptel-scope--serialize-tool-result (result)
  "Serialize RESULT for the model, returning a multibyte JSON string.
Calls `json-serialize'; on `wrong-type-argument' (typically a string
leaf with raw bytes) sanitizes RESULT via
`jf/gptel-scope--sanitize-for-json' and retries.

Critically, the value `json-serialize' returns is a UNIBYTE string
of UTF-8 bytes — and Emacs's `json-serialize' REJECTS unibyte
strings whose bytes are >= #x80 when they appear as values in a
later serialization pass.  Upstream gptel feeds our tool result
back through `json-serialize' as the `:content' of an Anthropic
`tool_result' message on the next request round; if we hand back
the raw unibyte output, that re-encoding fails with
`wrong-type-argument json-value-p \"<the JSON we returned>\"' and
the conversation halts.  Decoding back to a multibyte string with
proper Unicode codepoints sidesteps that check."
  (let ((unibyte
         (condition-case _err
             (json-serialize result)
           (wrong-type-argument
            (json-serialize
             (jf/gptel-scope--sanitize-for-json result))))))
    ;; Convert the unibyte UTF-8 bytes back to a multibyte string so
    ;; the next `json-serialize' pass (upstream's request builder)
    ;; doesn't reject the value via the unibyte-with-high-bytes check.
    (decode-coding-string unibyte 'utf-8)))
;; JSON-Serialization Sanitizer:1 ends here

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
                 (funcall callback
                          (jf/gptel-scope--serialize-tool-result
                           (progn ,@body))))
               (lambda (deny-response)
                 (funcall callback
                          (jf/gptel-scope--serialize-tool-result deny-response))))
            (error
             (funcall callback
                      (jf/gptel-scope--serialize-tool-result
                       (list :success nil
                             :error "tool_exception"
                             :message (format "Tool error: %s"
                                              (error-message-string err))))))))))))
;; Scoped Tool Macro:1 ends here

;; Provide Feature


;; [[file:scope-tool-wrapper.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-tool-wrapper)
;;; scope-tool-wrapper.el ends here
;; Provide Feature:1 ends here
