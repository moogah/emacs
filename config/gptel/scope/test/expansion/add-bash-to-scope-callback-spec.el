;;; add-bash-to-scope-callback-spec.el --- add-bash-to-scope callback contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Regression spec for scope-rearch-followups Bug 2.
;;
;; Contract pinned: every branch of the Add-to-scope action handler MUST
;; deliver a structured payload to the wrapper's async callback exactly
;; once. The bare-command-name refusal branch of
;; `jf/gptel-scope--add-bash-to-scope' previously returned nil, which
;; bubbled up to `--add-to-scope--emit-callback' and was indistinguishable
;; from a dedup short-circuit — the LLM received `:success t
;; :patterns_added [] :message "Pattern already in scope (no-op)."',
;; interpreted it as "added; retry", retried the same command, hit the
;; same denial, and looped.
;;
;; Post-fix:
;;   - `--add-bash-to-scope' returns the sentinel `:bare-command-refusal'
;;     for the bare-command-name path (distinguishing it from dedup).
;;   - `--add-to-scope--emit-callback' branches on the sentinel and emits
;;     a Deny-shaped payload (`:success nil :error
;;     "command_name_not_expandable" :user_denied t').
;;
;; Spec scenarios (from openspec/changes/scope-rearch-followups/specs/
;; gptel/scope-expansion.md, Requirement "Section-targeted writes"):
;;   - "Bash file-op denials route to path sections" — path-shaped resource
;;     writes to the drawer and emits :success t.
;;   - "Bare command name refusal invokes the callback" — bare command
;;     name does NOT write; callback emits :success nil with
;;     "command_name_not_expandable" error.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope-tool-wrapper.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

(defun add-bash-callback-spec--make-scope (violation callback patterns)
  "Construct a transient-scope plist for Add-to-scope action handler."
  (list :violation violation
        :callback callback
        :patterns patterns
        :tool-name "run_bash_command"
        :chat-buffer (current-buffer)))

(defmacro add-bash-callback-spec--with-stub-scope (scope-form &rest body)
  "Run BODY with `transient-scope' stubbed to return SCOPE-FORM."
  (declare (indent 1))
  `(let ((scope ,scope-form))
     (cl-letf (((symbol-function 'transient-scope)
                (lambda (&rest _) scope))
               ((symbol-function 'transient-quit-one)
                (lambda (&rest _) nil))
               ((symbol-function 'jf/gptel-scope--process-expansion-queue)
                (lambda (&rest _) nil))
               ((symbol-function 'save-buffer)
                (lambda (&rest _) nil)))
       ,@body)))

(describe "jf/gptel-scope--add-bash-to-scope callback contract"

  (describe "writer return contract"

    (it "returns the :bare-command-refusal sentinel for a bare command name"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (let ((result (jf/gptel-scope--add-bash-to-scope
                         "brew" "run_bash_command" :execute)))
            (expect result :to-be :bare-command-refusal)
            ;; And it must NOT have written anything.
            (expect (org-entry-get-multivalued-property
                     (point-min) "GPTEL_SCOPE_READ") :to-equal nil)
            (expect (org-entry-get-multivalued-property
                     (point-min) "GPTEL_SCOPE_WRITE") :to-equal nil)
            (expect (org-entry-get-multivalued-property
                     (point-min) "GPTEL_SCOPE_EXECUTE") :to-equal nil)))))

    (it "returns the pattern string for a path-shaped resource"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (let ((result (jf/gptel-scope--add-bash-to-scope
                         "/etc/passwd" "run_bash_command" :read)))
            (expect (stringp result) :to-be t))))))

  (describe "callback payload via --add-to-scope action handler"

    (it "invokes callback with :success nil and command_name_not_expandable for bare command"
      (let ((callback-payloads nil))
        (jf/gptel-test--with-scope-drawer '()
          (let* ((callback (lambda (payload) (push payload callback-payloads)))
                 (violation (list :resource "brew"
                                  :reason "Need brew for dep"
                                  :validation-type 'bash
                                  :operation :execute
                                  :patterns '("brew")))
                 (scope (add-bash-callback-spec--make-scope
                         violation callback '("brew"))))
            (add-bash-callback-spec--with-stub-scope scope
              (jf/gptel-scope--add-to-scope))))
        (expect (length callback-payloads) :to-equal 1)
        (let* ((payload (car callback-payloads))
               (parsed (json-parse-string payload :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be :false)
          (expect (plist-get parsed :error) :to-equal "command_name_not_expandable")
          (expect (plist-get parsed :message) :to-match "brew")
          (expect (plist-get parsed :user_denied) :to-be t))))

    (it "invokes callback with :success t for path-shaped bash resource"
      (let ((callback-payloads nil))
        (jf/gptel-test--with-scope-drawer '()
          (let* ((callback (lambda (payload) (push payload callback-payloads)))
                 (violation (list :resource "/etc/passwd"
                                  :reason "Need passwd"
                                  :validation-type 'bash
                                  :operation :read
                                  :patterns '("/etc/passwd")))
                 (scope (add-bash-callback-spec--make-scope
                         violation callback '("/etc/passwd"))))
            (add-bash-callback-spec--with-stub-scope scope
              (jf/gptel-scope--add-to-scope))))
        (expect (length callback-payloads) :to-equal 1)
        (let* ((payload (car callback-payloads))
               (parsed (json-parse-string payload :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t))))))

(provide 'add-bash-to-scope-callback-spec)

;;; add-bash-to-scope-callback-spec.el ends here
