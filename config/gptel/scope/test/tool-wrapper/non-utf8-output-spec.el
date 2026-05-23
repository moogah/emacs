;;; non-utf8-output-spec.el --- Scoped tool tolerates non-UTF-8 byte output -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Red-phase regression for a real-world failure captured in a live
;; session (May 2026): `run_bash_command' produced a tool_exception
;; with the message
;;
;;   "Tool error: Wrong type argument: json-value-p, \"<bytes...>\""
;;
;; Root cause: `gptel-make-scoped-tool' (config/gptel/scope/scope-tool-wrapper.el)
;; runs the body and immediately hands the result to `json-serialize'
;; via the on-allow lambda.  Emacs's built-in `json-serialize' signals
;; `wrong-type-argument json-value-p' when the input contains a string
;; with raw bytes that are not valid UTF-8.  Subprocess output captured
;; into the bash tool's `:output' slot can land as such a byte string
;; when the process emits non-UTF-8 bytes (binary file content, broken
;; locale, encoding mismatch).
;;
;; Today the wrapper still produces a tool_exception in that case
;; because the on-allow lambda's failure is caught by the OUTER
;; condition-case in `gptel-make-scoped-tool', which then encodes a
;; canned error plist.  That kills the assistant's turn and confuses
;; the LLM (it sees an opaque "Wrong type argument" message rather
;; than a normal command result).
;;
;; The expected behavior (to be enforced by the eventual fix) is that
;; the wrapper either coerces non-UTF-8 bytes into a printable form
;; before serialization, or the bash tool sanitizes its output before
;; returning.  Either way, the LLM should receive a normal-looking
;; tool result rather than a `tool_exception'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'gptel)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-dir (expand-file-name "../.." test-dir)))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope-tool-wrapper.el" scope-dir)))

(defun non-utf8--find-tool-in-registry (tool-name)
  "Find TOOL-NAME in `gptel--known-tools' (alist-of-alists)."
  (catch 'found
    (dolist (category-entry gptel--known-tools)
      (let ((tools-alist (cdr category-entry)))
        (dolist (tool-entry tools-alist)
          (when (string= (car tool-entry) tool-name)
            (throw 'found (cdr tool-entry))))))
    nil))

(defmacro non-utf8--with-allow-stub (&rest body)
  "Run BODY with `jf/gptel-scope-authorize-tool-call' forced to call ON-ALLOW.
The real dispatcher would consult the scope drawer, prompt for
expansion, etc. — none of which is relevant here.  The bug under
test lives in the on-allow path of the scope-tool-wrapper macro,
specifically the `json-serialize' invocation.  Stubbing the
dispatcher isolates that path from the rest of the validation
plumbing."
  `(cl-letf (((symbol-function 'jf/gptel-scope-authorize-tool-call)
              (lambda (_name _op _args on-allow _on-deny)
                (funcall on-allow))))
     ,@body))

(describe "gptel-make-scoped-tool — non-UTF-8 byte output in body result"

  (before-each
    ;; A scoped tool whose body returns a plist shaped like
    ;; run_bash_command's result.  The body is parameterized via a
    ;; dynamic variable so each `it' can plug in its own `:output'
    ;; payload — that lets one fixture cover the UTF-8-safe baseline
    ;; and the non-UTF-8 regression in parallel.
    (defvar non-utf8--injected-output nil
      "Per-test `:output' payload returned by the bash-shaped scoped tool.")
    (eval
     '(gptel-make-scoped-tool
       "non_utf8_bash_shape"
       "Test tool: mirrors run_bash_command's body result plist shape."
       (list '(:name "command" :type string :description "Ignored"))
       (list :success t
             :output non-utf8--injected-output
             :exit_code 0
             :truncated nil
             :warnings nil))))

  (it "baseline: UTF-8 output is delivered as a normal tool result"
    ;; Pre-flight check: with a well-formed UTF-8 `:output', the
    ;; wrapper produces a normal JSON result and the callback never
    ;; sees `tool_exception'.  Pins the happy path so the regression
    ;; case below is unambiguous.
    (let ((non-utf8--injected-output "line1\nline2\n")
          (captured nil))
      (non-utf8--with-allow-stub
       (let* ((tool (non-utf8--find-tool-in-registry "non_utf8_bash_shape"))
              (fn (gptel-tool-function tool)))
         (funcall fn (lambda (result) (setq captured result)) "ignored")))
      (expect (stringp captured) :to-be-truthy)
      (expect captured :not :to-match "tool_exception")
      (expect captured :to-match "\"success\":true")))

  (it "returns a multibyte string so the next request build does not reject it"
    ;; The captured halting bug: `json-serialize' returns a UNIBYTE
    ;; string of UTF-8 bytes.  Upstream gptel feeds that string back
    ;; through `json-serialize' on the NEXT request as the `:content'
    ;; of an Anthropic `tool_result' message.  `json-value-p' rejects
    ;; unibyte strings with bytes >= #x80, so multibyte content in
    ;; the original output (e.g. `→' RIGHTWARDS ARROW from a
    ;; file containing → arrows) caused the next-round serialization
    ;; to fail with `wrong-type-argument json-value-p \"<our JSON>\"',
    ;; the error propagated back through our wrapper's outer
    ;; condition-case as a `tool_exception', and the conversation
    ;; halted.
    ;;
    ;; The wrapper MUST hand back a multibyte string for the bytes
    ;; >= #x80 to be treated as proper Unicode code points rather
    ;; than raw bytes on re-serialization.
    (let ((non-utf8--injected-output "foo → bar ✓")
          (captured nil))
      (non-utf8--with-allow-stub
       (let* ((tool (non-utf8--find-tool-in-registry "non_utf8_bash_shape"))
              (fn (gptel-tool-function tool)))
         (funcall fn (lambda (result) (setq captured result)) "ignored")))
      (expect (stringp captured) :to-be-truthy)
      (expect (multibyte-string-p captured) :to-be-truthy)
      ;; Round-trip: pass the captured string through another
      ;; `json-serialize' the way the Anthropic backend does for the
      ;; next request's `tool_result.content'.  This is what failed
      ;; in the live session.
      (let ((req-plist (list :content captured)))
        (expect (lambda () (json-serialize req-plist)) :not :to-throw))))

  (it "raw-byte output does NOT surface as a tool_exception to the model"
    ;; The captured failure: the bash tool's `:output' contains raw
    ;; bytes that are not valid UTF-8 (e.g., subprocess emitting a
    ;; binary file's content, or a broken locale decode).  Today the
    ;; wrapper's `(json-serialize result-plist)' signals
    ;; `wrong-type-argument json-value-p' and the outer condition-case
    ;; converts it into a canned `tool_exception' payload.  The model
    ;; then sees an opaque error rather than a useful tool result.
    ;;
    ;; Expected behavior: the wrapper (or the body) must coerce or
    ;; strip the offending bytes so a normal `:success' / `:output'
    ;; JSON object reaches the model.  The exact coercion (escape,
    ;; replace, truncate, error-string-fallback) is up to the fix;
    ;; this test pins ONLY the user-visible contract: no
    ;; tool_exception for this input.
    (let ((non-utf8--injected-output (unibyte-string 255 254 253))
          (captured nil))
      (non-utf8--with-allow-stub
       (let* ((tool (non-utf8--find-tool-in-registry "non_utf8_bash_shape"))
              (fn (gptel-tool-function tool)))
         (funcall fn (lambda (result) (setq captured result)) "ignored")))
      (expect (stringp captured) :to-be-truthy)
      ;; The bug shape: the callback receives a JSON object whose
      ;; `error' field is `tool_exception' and whose `message' begins
      ;; with the json-value-p text.  Both checks must be falsy for
      ;; the test to pass.
      (expect captured :not :to-match "\"error\":\"tool_exception\"")
      (expect captured :not :to-match "json-value-p"))))

(provide 'non-utf8-output-spec)

;;; non-utf8-output-spec.el ends here
