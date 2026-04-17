;;; authorize-tool-call-spec.el --- Dispatcher tests for scope authorization -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; DISPATCHER TESTS: jf/gptel-scope-authorize-tool-call
;;
;; The dispatcher owns the full authorization flow for scope-aware tools:
;; config load, validation, expansion UI triggering, and the allow-once /
;; add-to-scope / deny branches.  The scoped-tool macro is a thin adapter
;; that supplies on-allow / on-deny thunks; the tool bodies themselves
;; have no authorization responsibility.  These tests drive the dispatcher
;; directly with mocked collaborators to cover every branch.
;;
;; Mocked collaborators (boundary with the rest of the scope subsystem):
;; - jf/gptel-scope--load-config
;; - jf/gptel-scope--validate-tool-call
;; - jf/gptel-scope--trigger-inline-expansion
;;
;; Branches covered:
;; 1. Missing config short-circuits to on-deny with :error no_scope_config
;; 2. Validation :allowed t invokes on-allow and skips expansion
;; 3. Validation failure invokes trigger-inline-expansion with
;;    (check-result, tool-name, callback)
;; 4. Expansion result :allowed-once t invokes on-allow without re-validating
;; 5. Expansion result :approved nil invokes on-deny with a formatted response
;; 6. Expansion result :approved t (add-to-scope) re-invokes validation and,
;;    once validation passes, invokes on-allow

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation
           (expand-file-name "scope-validation.el" scope-dir)))

;;; Test Helpers

(defun authorize-spec--denied-check-result (&optional error-type resource)
  "Return a validation plist representing a denied call."
  (list :allowed nil
        :error (or error-type "path_out_of_scope")
        :message "Path not in scope"
        :resource (or resource "/denied/path")
        :validation-type 'path))

(defun authorize-spec--allowed-check-result ()
  "Return a validation plist representing an allowed call."
  (list :allowed t :validation-type 'path))

;;; Test Suite

(describe "jf/gptel-scope-authorize-tool-call"

  (describe "no scope configuration"

    (before-each
      (spy-on 'jf/gptel-scope--load-config :and-return-value nil)
      (spy-on 'jf/gptel-scope--validate-tool-call)
      (spy-on 'jf/gptel-scope--trigger-inline-expansion))

    (it "invokes on-deny with no_scope_config and never validates"
      (let (allow-called deny-response)
        (jf/gptel-scope-authorize-tool-call
         "read_file" 'read (list "/workspace/foo")
         (lambda () (setq allow-called t))
         (lambda (response) (setq deny-response response)))
        (expect allow-called :to-be nil)
        (expect (plist-get deny-response :error) :to-equal "no_scope_config")
        (expect (plist-get deny-response :success) :to-be nil)
        (expect 'jf/gptel-scope--validate-tool-call :not :to-have-been-called)
        (expect 'jf/gptel-scope--trigger-inline-expansion
                :not :to-have-been-called))))

  (describe "validation passes"

    (before-each
      (spy-on 'jf/gptel-scope--load-config :and-return-value '(:config t))
      (spy-on 'jf/gptel-scope--validate-tool-call
              :and-return-value (authorize-spec--allowed-check-result))
      (spy-on 'jf/gptel-scope--trigger-inline-expansion))

    (it "invokes on-allow and skips expansion"
      (let (allow-called deny-called)
        (jf/gptel-scope-authorize-tool-call
         "read_file" 'read (list "/workspace/foo")
         (lambda () (setq allow-called t))
         (lambda (_response) (setq deny-called t)))
        (expect allow-called :to-be t)
        (expect deny-called :to-be nil)
        (expect 'jf/gptel-scope--trigger-inline-expansion
                :not :to-have-been-called)))

    (it "passes tool-name, operation, args, and config to the validator"
      (jf/gptel-scope-authorize-tool-call
       "read_file" 'read (list "/workspace/foo")
       (lambda () nil)
       (lambda (_response) nil))
      (let ((call-args (spy-calls-args-for 'jf/gptel-scope--validate-tool-call 0)))
        (expect (nth 0 call-args) :to-equal "read_file")
        (expect (nth 1 call-args) :to-equal 'read)
        (expect (nth 2 call-args) :to-equal (list "/workspace/foo"))
        (expect (nth 3 call-args) :to-equal '(:config t)))))

  (describe "validation fails"

    (before-each
      (spy-on 'jf/gptel-scope--load-config :and-return-value '(:config t))
      (spy-on 'jf/gptel-scope--validate-tool-call
              :and-return-value (authorize-spec--denied-check-result))
      ;; Intercept trigger so the expansion callback never fires — we
      ;; just want to observe that the dispatcher handed control off.
      (spy-on 'jf/gptel-scope--trigger-inline-expansion))

    (it "hands check-result, tool-name, and a callback to trigger-inline-expansion"
      (jf/gptel-scope-authorize-tool-call
       "read_file" 'read (list "/etc/passwd")
       (lambda () nil)
       (lambda (_response) nil))
      (let ((call-args (spy-calls-args-for
                        'jf/gptel-scope--trigger-inline-expansion 0)))
        (expect (plist-get (nth 0 call-args) :error)
                :to-equal "path_out_of_scope")
        (expect (nth 1 call-args) :to-equal "read_file")
        (expect (functionp (nth 2 call-args)) :to-be t)))

    (it "does not invoke on-allow or on-deny before the user decides"
      (let (allow-called deny-called)
        (jf/gptel-scope-authorize-tool-call
         "read_file" 'read (list "/etc/passwd")
         (lambda () (setq allow-called t))
         (lambda (_response) (setq deny-called t)))
        (expect allow-called :to-be nil)
        (expect deny-called :to-be nil))))

  (describe "user chooses allow-once"

    (before-each
      (spy-on 'jf/gptel-scope--load-config :and-return-value '(:config t))
      (spy-on 'jf/gptel-scope--validate-tool-call
              :and-return-value (authorize-spec--denied-check-result))
      (spy-on 'jf/gptel-scope--trigger-inline-expansion
              :and-call-fake
              (lambda (_check-result _tool-name callback)
                (funcall callback (list :approved t :allowed-once t)))))

    (it "invokes on-allow without re-validating"
      (let (allow-called deny-called)
        (jf/gptel-scope-authorize-tool-call
         "read_file" 'read (list "/etc/passwd")
         (lambda () (setq allow-called t))
         (lambda (_response) (setq deny-called t)))
        (expect allow-called :to-be t)
        (expect deny-called :to-be nil)
        ;; Single validation pass — no re-check after allow-once.
        (expect (spy-calls-count 'jf/gptel-scope--validate-tool-call)
                :to-equal 1))))

  (describe "user denies"

    (before-each
      (spy-on 'jf/gptel-scope--load-config :and-return-value '(:config t))
      (spy-on 'jf/gptel-scope--validate-tool-call
              :and-return-value (authorize-spec--denied-check-result
                                 "path_out_of_scope" "/etc/shadow"))
      (spy-on 'jf/gptel-scope--trigger-inline-expansion
              :and-call-fake
              (lambda (_check-result _tool-name callback)
                (funcall callback (list :approved nil :reason "user_denied")))))

    (it "invokes on-deny with a formatted response and never on-allow"
      (let (allow-called deny-response)
        (jf/gptel-scope-authorize-tool-call
         "read_file" 'read (list "/etc/shadow")
         (lambda () (setq allow-called t))
         (lambda (response) (setq deny-response response)))
        (expect allow-called :to-be nil)
        (expect (plist-get deny-response :success) :to-be nil)
        (expect (plist-get deny-response :error) :to-equal "path_out_of_scope")
        (expect (plist-get deny-response :tool) :to-equal "read_file")
        (expect (plist-get deny-response :resource) :to-equal "/etc/shadow"))))

  (describe "user chooses add-to-scope"

    (before-each
      (spy-on 'jf/gptel-scope--load-config :and-return-value '(:config t))
      ;; First validation call denies; second (after add-to-scope) passes.
      (let ((call-count 0))
        (spy-on 'jf/gptel-scope--validate-tool-call
                :and-call-fake
                (lambda (&rest _)
                  (cl-incf call-count)
                  (if (= call-count 1)
                      (authorize-spec--denied-check-result)
                    (authorize-spec--allowed-check-result)))))
      ;; Expansion fires once; the add-to-scope branch recurses into
      ;; authorize-tool-call, which re-enters validate-tool-call (now
      ;; passing) rather than re-triggering expansion.
      (spy-on 'jf/gptel-scope--trigger-inline-expansion
              :and-call-fake
              (lambda (_check-result _tool-name callback)
                (funcall callback (list :approved t)))))

    (it "re-validates and invokes on-allow when the retry passes"
      (let (allow-called deny-called)
        (jf/gptel-scope-authorize-tool-call
         "read_file" 'read (list "/workspace/newly-added")
         (lambda () (setq allow-called t))
         (lambda (_response) (setq deny-called t)))
        (expect allow-called :to-be t)
        (expect deny-called :to-be nil)
        (expect (spy-calls-count 'jf/gptel-scope--validate-tool-call)
                :to-equal 2)
        (expect (spy-calls-count 'jf/gptel-scope--trigger-inline-expansion)
                :to-equal 1)))))

(provide 'authorize-tool-call-spec)

;;; authorize-tool-call-spec.el ends here
