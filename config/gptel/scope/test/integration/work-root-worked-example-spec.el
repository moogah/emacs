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

(defvar wex--shown nil
  "Transient scopes shown via `transient-setup', newest first.")

(defun wex--install-real-deny ()
  "Let the REAL expansion queue run, capturing transients so a test can deny.

REPLACES the former no-op stub.  Stubbing `jf/gptel-scope-prompt-expansion'
as a complete no-op proved the body did not run, but it NEVER drove the deny
continuation — which is exactly where the parallel-deny serialization crash
lived (.tasks/scope-deny-symbolp-crash-parallel-tools.md).  Capturing the
transient and driving the real `jf/gptel-scope--deny-expansion' exercises the
full deny path under a non-empty scope, so this spec can no longer hide that
class of bug."
  (spy-on 'transient-setup :and-call-fake
          (lambda (_prefix &rest args)
            (push (plist-get (cddr args) :scope) wex--shown)))
  (spy-on 'transient-quit-one)
  (setq jf/gptel-scope--expansion-active nil
        jf/gptel-scope--expansion-queue nil
        wex--shown nil))

(defun wex--deny-current ()
  "Run the REAL `jf/gptel-scope--deny-expansion' against the shown transient."
  (let ((scope (car wex--shown)))
    (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope)))
      (jf/gptel-scope--deny-expansion))))

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

  (it "STEP 2: absolute out-of-scope read is DENIED and delivers a structured denial"
    ;; Was: file_not_found because the body ran in the sandbox.  Now: the
    ;; absolute path is outside read scope, so the call is routed to the
    ;; expansion prompt and the body does NOT run.  Driving the REAL deny suffix
    ;; (not a no-op stub) proves the denial serializes and reaches the callback
    ;; under a NON-EMPTY scope — the case that used to crash and hang.
    (let* ((default-directory wex--work-root)
           (abs (expand-file-name "init.el" wex--outside)))
      (with-temp-file abs (insert "(secret)"))
      (spy-on 'jf/gptel-scope--load-config :and-return-value (wex--config))
      (wex--install-real-deny)

      (funcall (gptel-tool-function (wex--find-tool "read_file_in_scope"))
               #'wex--callback abs)

      ;; Routed to expansion (transient shown), body has not run yet.
      (expect (length wex--shown) :to-equal 1)
      (expect wex--result :to-be nil)
      ;; Drive the REAL deny: callback receives a structured denial (no crash,
      ;; no hang), the body still never ran (no :content), and the denial
      ;; reports allowed_patterns as a JSON array.
      (wex--deny-current)
      (expect (plist-get wex--result :success) :not :to-be t)
      (expect (plist-get wex--result :content) :to-be nil)
      (expect (plist-get wex--result :allowed-patterns) :to-be-truthy)))

  (it "STEP 3: absolute in-scope write is ALLOWED (scratch space)"
    (let* ((default-directory wex--work-root)
           (abs (expand-file-name "ok.txt" wex--scratch)))
      (spy-on 'jf/gptel-scope--load-config :and-return-value (wex--config))

      (funcall (gptel-tool-function (wex--find-tool "write_file_in_scope"))
               #'wex--callback abs "ok")

      (expect (plist-get wex--result :success) :to-be t)
      (expect (file-exists-p abs) :to-be-truthy)))

  (it "STEP 4: relative out-of-scope write is DENIED, delivers a denial, and leaves NO file (the headline fix)"
    ;; THE bug: previously this silently wrote into the sandbox.  Now the
    ;; relative path resolves to <work-root>/x.txt, which is in READ scope but
    ;; NOT in WRITE scope, so the write is denied and nothing is created.  The
    ;; REAL deny suffix runs (under non-empty scope) and must deliver cleanly.
    (let* ((default-directory wex--work-root)
           (would-be (expand-file-name "x.txt" wex--work-root)))
      (spy-on 'jf/gptel-scope--load-config :and-return-value (wex--config))
      (wex--install-real-deny)

      (funcall (gptel-tool-function (wex--find-tool "write_file_in_scope"))
               #'wex--callback "x.txt" "should not be written")

      ;; Routed to expansion (transient shown) rather than silently written.
      (expect (length wex--shown) :to-equal 1)
      (wex--deny-current)
      ;; The denial must actually REACH the callback (non-nil) — this is the
      ;; assertion that fails when the deny payload crashes serialization; a
      ;; bare `success not t' would pass vacuously on a nil (never-delivered)
      ;; result and mask the bug.
      (expect wex--result :to-be-truthy)
      (expect (plist-get wex--result :success) :not :to-be t)
      (expect (plist-get wex--result :allowed-patterns) :to-be-truthy)
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
