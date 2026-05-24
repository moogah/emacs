;;; request-scope-expansion-operation-spec.el --- request_scope_expansion takes operation, not tool_name -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Regression spec for scope-rearch-followups Bug 1: request_scope_expansion's
;; primary LLM-facing argument is `operation' (closed enum read/write/modify/
;; execute/bash), not `tool_name'. The :validation-type passed to
;; jf/gptel-scope-prompt-expansion derives directly from operation —
;; matching how every other consumer of :validation-type recovers it
;; (see scope-validation.el:779-785).
;;
;; Pinned scenarios (from openspec/changes/scope-rearch-followups/specs/gptel/
;; scope-expansion.md, Requirement "request_scope_expansion tool"):
;;
;;   1. Filesystem operations (read/write/modify/execute) derive
;;      :validation-type 'path and :operation as the matching symbol.
;;   2. operation "bash" derives :validation-type 'bash.
;;   3. Out-of-enum operation values (including stale tool_name strings
;;      from pre-migration prompts) short-circuit with :success false
;;      and never reach jf/gptel-scope-prompt-expansion.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)
(require 'gptel)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir)))

(defun rse-op-spec--find-tool-function (tool-name)
  "Return the :function for TOOL-NAME from `gptel--known-tools', or nil."
  (catch 'found
    (dolist (category-entry gptel--known-tools)
      (dolist (tool-entry (cdr category-entry))
        (when (string= (car tool-entry) tool-name)
          (throw 'found (gptel-tool-function (cdr tool-entry))))))
    nil))

(defun rse-op-spec--invoke (operation patterns justification)
  "Call request_scope_expansion's :function with OPERATION, PATTERNS, JUSTIFICATION.
Returns a plist with :callback-payload and :prompt-expansion-args (the args
passed to jf/gptel-scope-prompt-expansion, or nil if it was never called)."
  (let ((tool-fn (rse-op-spec--find-tool-function "request_scope_expansion"))
        (callback-payload nil)
        (prompt-expansion-args nil))
    (expect tool-fn :to-be-truthy)
    (spy-on 'jf/gptel-scope-prompt-expansion
            :and-call-fake
            (lambda (violation-info _cb patterns tool-name)
              (setq prompt-expansion-args
                    (list :violation-info violation-info
                          :patterns patterns
                          :tool-name tool-name))))
    (funcall tool-fn
             (lambda (payload) (setq callback-payload payload))
             operation patterns justification)
    (list :callback-payload callback-payload
          :prompt-expansion-args prompt-expansion-args)))

(describe "request_scope_expansion: operation-derived validation-type"

  (dolist (op '("read" "write" "modify" "execute"))
    (it (format "derives :validation-type 'path from filesystem operation %S" op)
      (let* ((result (rse-op-spec--invoke op (vector "/tmp/foo") "test"))
             (args (plist-get result :prompt-expansion-args))
             (vi (plist-get args :violation-info)))
        (expect (spy-context-args
                 (car (spy-calls-all 'jf/gptel-scope-prompt-expansion)))
                :not :to-be nil)
        (expect vi :not :to-be nil)
        (expect (plist-get vi :validation-type) :to-be 'path)
        (expect (plist-get vi :operation) :to-be (intern op))
        (expect (plist-get vi :patterns) :to-equal '("/tmp/foo")))))

  (it "derives :validation-type 'bash from operation \"bash\""
    (let* ((result (rse-op-spec--invoke "bash" (vector "tar -xf x.tar") "test"))
           (args (plist-get result :prompt-expansion-args))
           (vi (plist-get args :violation-info)))
      (expect vi :not :to-be nil)
      (expect (plist-get vi :validation-type) :to-be 'bash)
      (expect (plist-get vi :operation) :to-be 'bash)))

  (it "rejects out-of-enum operation values without invoking the expansion UI"
    (let* ((result (rse-op-spec--invoke "made_up_verb" (vector "/tmp/foo") "test"))
           (payload (plist-get result :callback-payload))
           (args (plist-get result :prompt-expansion-args))
           (parsed (and payload (json-parse-string payload :object-type 'plist))))
      (expect args :to-be nil)
      (expect payload :not :to-be nil)
      (expect (plist-get parsed :success) :to-be :false)
      (expect (plist-get parsed :error) :to-equal "unknown_operation")
      (expect (plist-get parsed :message) :to-match "made_up_verb")))

  (it "rejects a stale tool_name string (pre-migration prompt) as out-of-enum"
    (let* ((result (rse-op-spec--invoke "read_file_in_scope" (vector "/tmp/foo") "test"))
           (payload (plist-get result :callback-payload))
           (args (plist-get result :prompt-expansion-args))
           (parsed (and payload (json-parse-string payload :object-type 'plist))))
      (expect args :to-be nil)
      (expect payload :not :to-be nil)
      (expect (plist-get parsed :success) :to-be :false)
      (expect (plist-get parsed :error) :to-equal "unknown_operation"))))

(provide 'request-scope-expansion-operation-spec)

;;; request-scope-expansion-operation-spec.el ends here
