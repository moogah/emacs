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
;;      :validation-type 'path and :operation as the matching KEYWORD
;;      (`:read', `:write', `:modify', `:execute'). Keyword form is
;;      required for composition with --map-operation-to-drawer-key
;;      (scope-expansion.el:91-134), which matches against keywords.
;;   2. operation "bash" derives :validation-type 'bash and :operation
;;      nil — bash routing has no canonical drawer key, so Stage 1 of
;;      --add-to-scope's dispatcher handles the no-operation case.
;;   3. Out-of-enum operation values (including stale tool_name strings
;;      from pre-migration prompts) short-circuit with :success false
;;      and never reach jf/gptel-scope-prompt-expansion.
;;   4. End-to-end: the violation-info from a filesystem operation
;;      composes with the action-handler stack — clicking Add to Scope
;;      writes to the matching drawer key. (This is the regression that
;;      a bare-symbol :operation would silently break; the earlier
;;      version of this spec stubbed --prompt-expansion and missed the
;;      composition gap.)

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)
(require 'gptel)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
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

  (dolist (pair '(("read" . :read)
                  ("write" . :write)
                  ("modify" . :modify)
                  ("execute" . :execute)))
    (let ((op (car pair))
          (op-keyword (cdr pair)))
      (it (format "derives :validation-type 'path from filesystem operation %S" op)
        (let* ((result (rse-op-spec--invoke op (vector "/tmp/foo") "test"))
               (args (plist-get result :prompt-expansion-args))
               (vi (plist-get args :violation-info)))
          (expect vi :not :to-be nil)
          (expect (plist-get vi :validation-type) :to-be 'path)
          ;; Keyword form, NOT bare symbol — composes with
          ;; --map-operation-to-drawer-key downstream.
          (expect (plist-get vi :operation) :to-be op-keyword)
          (expect (plist-get vi :patterns) :to-equal '("/tmp/foo"))))))

  (it "derives :validation-type 'bash from operation \"bash\" with nil :operation"
    (let* ((result (rse-op-spec--invoke "bash" (vector "tar -xf x.tar") "test"))
           (args (plist-get result :prompt-expansion-args))
           (vi (plist-get args :violation-info)))
      (expect vi :not :to-be nil)
      (expect (plist-get vi :validation-type) :to-be 'bash)
      ;; Bash has no canonical drawer-key; :operation nil routes the
      ;; transient's Add-to-Scope click through Stage 1 of the action
      ;; handler (`--handle-nil-operation') rather than the writer.
      (expect (plist-get vi :operation) :to-be nil)))

  (it "passes a non-nil tool-name to --prompt-expansion so success messages don't say 'to nil'"
    (let* ((result (rse-op-spec--invoke "read" (vector "/tmp/foo") "test"))
           (args (plist-get result :prompt-expansion-args)))
      (expect (plist-get args :tool-name) :to-be-truthy)
      (expect (stringp (plist-get args :tool-name)) :to-be t)))

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

(describe "request_scope_expansion: end-to-end with the action handler"
  ;; This block does NOT stub --prompt-expansion. It exercises the full
  ;; action-handler chain (handle-action → write-pattern-to-scope →
  ;; add-path-to-scope → write-pattern-to-drawer → map-operation-to-
  ;; drawer-key) for a filesystem operation. A bare-symbol :operation
  ;; (e.g. `read' instead of `:read') would fall into the unmapped-:op
  ;; error arm of --map-operation-to-drawer-key and the writer would
  ;; signal; this spec pins that the composition is intact.

  (it "writes the pattern to GPTEL_SCOPE_READ when LLM requests operation \"read\" and user clicks Add to Scope"
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil))
              ((symbol-function 'transient-quit-one) (lambda (&rest _) nil))
              ((symbol-function 'jf/gptel-scope--process-expansion-queue)
               (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer '()
        (let* ((callback-payloads nil)
               (callback (lambda (payload) (push payload callback-payloads)))
               (tool-fn (rse-op-spec--find-tool-function "request_scope_expansion"))
               (captured-scope nil))
          ;; Capture the transient scope that --prompt-expansion would
          ;; construct and route us into --add-to-scope directly.
          (cl-letf (((symbol-function 'jf/gptel-scope-prompt-expansion)
                     (lambda (violation-info cb patterns tool-name)
                       (setq captured-scope
                             (list :violation violation-info
                                   :callback cb
                                   :patterns patterns
                                   :tool-name tool-name
                                   :chat-buffer (current-buffer)))))
                    ((symbol-function 'transient-scope)
                     (lambda (&rest _) captured-scope)))
            (funcall tool-fn callback "read" (vector "/tmp/foo") "test")
            (jf/gptel-scope--add-to-scope))
          ;; Pattern landed in the read drawer key, and the callback
          ;; received structured success — neither possible if :operation
          ;; were a bare symbol falling through map-operation-to-drawer-key's
          ;; error arm.
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal '("/tmp/foo"))
          (expect (length callback-payloads) :to-equal 1)
          (let ((parsed (json-parse-string (car callback-payloads)
                                           :object-type 'plist)))
            (expect (plist-get parsed :success) :to-be t)))))))

(provide 'request-scope-expansion-operation-spec)

;;; request-scope-expansion-operation-spec.el ends here
