;;; work-root-worked-example-spec.el --- Headless reproduction of the original cwd-bug table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; HEADLESS END-TO-END REPRODUCTION of the worked example that surfaced the bug.
;;
;; The work-root change was born from a live PersistentAgent test
;; (.tasks/design-working-directory-scoping-for-sessions-and-agents.md).  That
;; run used relative paths that never left the agent's bookkeeping sandbox, so
;; the scope boundary was never crossed: an out-of-scope RELATIVE write silently
;; succeeded into the sandbox, and out-of-scope reads returned `file_not_found'
;; (body ran) instead of a scope denial.  The original table:
;;
;;   | Step | Tool call (model-emitted)        | Was (buggy)              | Must be now      |
;;   |------|----------------------------------|-------------------------|------------------|
;;   | 1    | read  config/.../minimal.md (rel)| file_not_found (sandbox)| ALLOWED          |
;;   | 2    | read  <abs>/init.el (out of scope)| file_not_found (sandbox)| scope denial     |
;;   | 3    | write <abs>/ok.txt (in scope)    | OK                      | ALLOWED          |
;;   | 4    | write ./x.txt (rel, out of scope)| silently written (BUG)  | scope denial,    |
;;   |      |                                  |                         | NOT written      |
;;
;; This spec drives the REAL filesystem tools through the REAL scope wrapper
;; (mocking only config loading and the expansion UI, as the sibling
;; integration spec does) with `default-directory' bound to a hermetic temp
;; work root, reproducing all four steps for BOTH relative and absolute paths.
;; It is the permanent regression form of the manual repro the design doc's
;; "How to reproduce / verify" section described.
;;
;; Read scope  = <work-root>/**            (so relative paths resolve in-scope to READ)
;; Write scope = <scratch>/**              (work root is readable, NOT writable, by D6)
;; => a RELATIVE write resolves into the work root, which is OUT of write scope:
;;    step 4 must deny AND leave no file behind.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope/scope-validation.el" gptel-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope/scope-tool-wrapper.el" gptel-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope/scope-expansion.el" gptel-dir))
  (require 'jf-gptel-scope-filesystem-tools (expand-file-name "scope/scope-filesystem-tools.el" gptel-dir))
  (require 'gptel))

;;; Infrastructure

(defvar wex--result nil "Parsed callback plist.")
(defvar wex--work-root nil "Hermetic project / work root (default-directory).")
(defvar wex--scratch nil "Hermetic write-allowed scratch dir (stands in for /tmp/**).")
(defvar wex--outside nil "Hermetic dir OUTSIDE read and write scope.")

(defun wex--callback (json) (setq wex--result (json-parse-string json :object-type 'plist)))

(defun wex--find-tool (name)
  (cl-block nil
    (dolist (cat gptel--known-tools)
      (dolist (entry (cdr cat))
        (when (string= (car entry) name) (cl-return (cdr entry)))))))

(defun wex--config ()
  "Reproduce the worked example's scope: read = work root, write = scratch only."
  (helpers-spec-make-scope-config
   :read  (list (concat wex--work-root "**"))
   :write (list (concat wex--scratch "**"))))

(defun wex--stub-expansion ()
  "Stub the expansion UI as a no-op that records the call but does not respond.

A denied (out-of-scope) call is ROUTED to the expansion prompt and the tool
body does not run until the user answers.  Stubbing the prompt as a no-op
(the proven pattern from `filesystem-scope-integration-spec.el's \"triggers
expansion UI when path is outside read scope\") proves the body did NOT run —
which is exactly the silent-write regression point — without invoking the
deny-continuation dispatcher (noisy in batch mode)."
  (spy-on 'jf/gptel-scope-prompt-expansion))

(describe "Worked-example reproduction: the original cwd-bug table, with the fix in place"

  (before-each
    (setq wex--result nil)
    (setq wex--work-root (file-name-as-directory (make-temp-file "wex-proj-" t)))
    (setq wex--scratch   (file-name-as-directory (make-temp-file "wex-scratch-" t)))
    (setq wex--outside   (file-name-as-directory (make-temp-file "wex-outside-" t))))

  (after-each
    (dolist (d (list wex--work-root wex--scratch wex--outside))
      (when (and d (file-exists-p d)) (delete-directory d t))))

  (it "STEP 1: relative in-scope read is ALLOWED (resolves into the work root)"
    ;; Was: file_not_found because cwd was the sandbox.  Now: resolves to
    ;; <work-root>/config/minimal.md, in read scope, content returned.
    (let* ((default-directory wex--work-root)
           (dir (expand-file-name "config" wex--work-root)))
      (make-directory dir t)
      (with-temp-file (expand-file-name "minimal.md" dir) (insert "# minimal"))
      (spy-on 'jf/gptel-scope--load-config :and-return-value (wex--config))

      (funcall (gptel-tool-function (wex--find-tool "read_file_in_scope"))
               #'wex--callback "config/minimal.md")

      (expect (plist-get wex--result :success) :to-be t)
      (expect (plist-get wex--result :content) :to-equal "# minimal")))

  (it "STEP 2: absolute out-of-scope read is DENIED (scope boundary is now crossed)"
    ;; Was: file_not_found because the body ran in the sandbox.  Now: the
    ;; absolute path is outside read scope, so the call is routed to the
    ;; expansion prompt and the body does NOT run (no content returned) until
    ;; the user answers.  This is the boundary the original run never crossed.
    (let* ((default-directory wex--work-root)
           (abs (expand-file-name "init.el" wex--outside)))
      (with-temp-file abs (insert "(secret)"))
      (spy-on 'jf/gptel-scope--load-config :and-return-value (wex--config))
      (wex--stub-expansion)

      (funcall (gptel-tool-function (wex--find-tool "read_file_in_scope"))
               #'wex--callback abs)

      ;; Routed to expansion (denied), and the body never ran.
      (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)
      (expect wex--result :to-be nil)))

  (it "STEP 3: absolute in-scope write is ALLOWED (scratch space)"
    (let* ((default-directory wex--work-root)
           (abs (expand-file-name "ok.txt" wex--scratch)))
      (spy-on 'jf/gptel-scope--load-config :and-return-value (wex--config))

      (funcall (gptel-tool-function (wex--find-tool "write_file_in_scope"))
               #'wex--callback abs "ok")

      (expect (plist-get wex--result :success) :to-be t)
      (expect (file-exists-p abs) :to-be-truthy)))

  (it "STEP 4: relative out-of-scope write is DENIED and leaves NO file (the headline fix)"
    ;; THE bug: previously this silently wrote into the sandbox.  Now the
    ;; relative path resolves to <work-root>/x.txt, which is in READ scope but
    ;; NOT in WRITE scope, so the write is denied and nothing is created.
    (let* ((default-directory wex--work-root)
           (would-be (expand-file-name "x.txt" wex--work-root)))
      (spy-on 'jf/gptel-scope--load-config :and-return-value (wex--config))
      (wex--stub-expansion)

      (funcall (gptel-tool-function (wex--find-tool "write_file_in_scope"))
               #'wex--callback "x.txt" "should not be written")

      ;; Routed to expansion (denied) rather than silently written.
      (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)
      (expect wex--result :to-be nil)
      ;; The decisive regression assertion: the file was NOT silently created.
      (expect (file-exists-p would-be) :to-be nil)))

  (it "STEP 4b: granting expansion lets the retry succeed (expansion efficacy)"
    ;; The design doc flagged expansion efficacy as UNCONFIRMED (the original
    ;; live retries never changed the outcome because the files were in the
    ;; sandbox).  Here, simulating an allow-once grant runs the body and the
    ;; file IS written at the work-root-resolved path.
    (let* ((default-directory wex--work-root)
           (would-be (expand-file-name "x.txt" wex--work-root)))
      (spy-on 'jf/gptel-scope--load-config :and-return-value (wex--config))
      ;; Allow-once: the wrapper trusts success as authorization and runs body.
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (_violation callback _patterns _tool-name)
                (funcall callback (json-serialize '(:success t :allowed_once t)))))

      (funcall (gptel-tool-function (wex--find-tool "write_file_in_scope"))
               #'wex--callback "x.txt" "granted")

      (expect (plist-get wex--result :success) :to-be t)
      (expect (file-exists-p would-be) :to-be-truthy)
      (expect (with-temp-buffer (insert-file-contents would-be) (buffer-string))
              :to-equal "granted"))))

(provide 'work-root-worked-example-spec)
;;; work-root-worked-example-spec.el ends here
