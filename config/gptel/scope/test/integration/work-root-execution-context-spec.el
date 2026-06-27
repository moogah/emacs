;;; work-root-execution-context-spec.el --- Tool BODY executes in the work-root context -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; EXECUTION CONTEXT: the tool BODY runs against the work-root default-directory.
;;
;; Coverage gap this closes (Phase 1 of the work-root testing roadmap):
;; `relative-resolution-spec.el' proves the VALIDATOR resolves a relative
;; path against `default-directory'.  It passes no directory and asserts a
;; verdict — it never runs a tool body, so it cannot prove the SIDE EFFECT
;; (the byte written, the byte read) also lands in the work root.  That
;; `with-current-buffer info:buffer' wiring (design.md Context fact #2) is the
;; exact mechanism the original PersistentAgent bug exposed: a relative write
;; silently landed in the agent's bookkeeping dir because the body's bare
;; `(expand-file-name filepath)' resolved against the wrong `default-directory'.
;;
;; These specs drive the REAL `write_file_in_scope' / `read_file_in_scope'
;; tools through the REAL scope wrapper (mocking only config loading, exactly
;; as `filesystem-scope-integration-spec.el' does), with `default-directory'
;; bound to a real temp work root and a RELATIVE path argument.  They assert
;; the file is created / read AT `<work-root>/<relative>' and that the result's
;; `:full_path' equals that — i.e. execution followed the bound work root.
;;
;; The decisive negative-control spec rebinds `default-directory' to a DIFFERENT
;; work root and asserts the SAME relative path lands in the OTHER dir (and not
;; in the first), so the body cannot be resolving against a baked-in root.
;;
;; Maps to scope/spec.md scenario "Tool executes in the work-root context" and
;; "A relative write inside the work root is in scope by construction".

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies via path resolution (mirrors filesystem-scope-integration-spec.el).
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       ;; test/integration/ -> test/ -> scope/ -> gptel/ -> config/
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope/scope-validation.el" gptel-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope/scope-tool-wrapper.el" gptel-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope/scope-expansion.el" gptel-dir))
  (require 'jf-gptel-scope-filesystem-tools (expand-file-name "scope/scope-filesystem-tools.el" gptel-dir))
  (require 'gptel))

;;; Test Infrastructure

(defvar wrexec--result nil
  "Parsed plist from the gptel callback's JSON string argument.")

(defvar wrexec--work-root nil
  "Temp directory standing in for the session's work root (default-directory).")

(defvar wrexec--other-root nil
  "A second temp directory used as a DIFFERENT work root (negative control).")

(defun wrexec--callback (result-json)
  "Capture the scoped tool's JSON RESULT-JSON as a parsed plist."
  (setq wrexec--result (json-parse-string result-json :object-type 'plist)))

(defun wrexec--find-tool (name)
  "Find tool NAME in `gptel--known-tools'."
  (cl-block nil
    (dolist (category-entry gptel--known-tools)
      (dolist (tool-entry (cdr category-entry))
        (when (string= (car tool-entry) name)
          (cl-return (cdr tool-entry)))))))

(describe "Work-root execution context: tool BODY resolves relative paths against default-directory"

  (before-each
    (setq wrexec--result nil)
    (setq wrexec--work-root (file-name-as-directory (make-temp-file "wrexec-root-" t)))
    (setq wrexec--other-root (file-name-as-directory (make-temp-file "wrexec-other-" t))))

  (after-each
    (when (and wrexec--work-root (file-exists-p wrexec--work-root))
      (delete-directory wrexec--work-root t))
    (when (and wrexec--other-root (file-exists-p wrexec--other-root))
      (delete-directory wrexec--other-root t)))

  (it "writes a RELATIVE path into the work root, not the bookkeeping dir"
    ;; The headline-bug mechanism, inverted into a passing assertion: with
    ;; default-directory bound to the work root and the work root in write
    ;; scope, a relative `out.txt' must be created AT <work-root>/out.txt.
    (let* ((default-directory wrexec--work-root)
           (config (helpers-spec-make-scope-config
                    :read  (list (concat wrexec--work-root "**"))
                    :write (list (concat wrexec--work-root "**"))))
           (tool (wrexec--find-tool "write_file_in_scope"))
           (expected (expand-file-name "out.txt" wrexec--work-root)))
      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      ;; Async write signature: (callback filepath content)
      (funcall (gptel-tool-function tool) #'wrexec--callback "out.txt" "payload")

      (expect (plist-get wrexec--result :success) :to-be t)
      ;; The body resolved the relative path against default-directory.
      (expect (plist-get wrexec--result :full_path) :to-equal expected)
      ;; And the side effect genuinely landed there.
      (expect (file-exists-p expected) :to-be-truthy)
      (expect (with-temp-buffer (insert-file-contents expected) (buffer-string))
              :to-equal "payload")))

  (it "reads a RELATIVE path from the work root"
    ;; Place a file under the work root, then read it via a relative path with
    ;; default-directory bound there — the body must find it.
    (let* ((default-directory wrexec--work-root)
           (config (helpers-spec-make-scope-config
                    :read (list (concat wrexec--work-root "**"))))
           (tool (wrexec--find-tool "read_file_in_scope"))
           (ondisk (expand-file-name "data.txt" wrexec--work-root)))
      (with-temp-file ondisk (insert "from work root"))
      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      (funcall (gptel-tool-function tool) #'wrexec--callback "data.txt")

      (expect (plist-get wrexec--result :success) :to-be t)
      (expect (plist-get wrexec--result :content) :to-equal "from work root")
      (expect (plist-get wrexec--result :full_path) :to-equal ondisk)))

  (it "the SAME relative path follows a DIFFERENT work root (no baked-in root)"
    ;; Negative control: identical relative arg, but default-directory and
    ;; scope point at OTHER-ROOT.  The file must land under OTHER-ROOT and NOT
    ;; under the first work root — proving the body tracks default-directory.
    (let* ((default-directory wrexec--other-root)
           (config (helpers-spec-make-scope-config
                    :read  (list (concat wrexec--other-root "**"))
                    :write (list (concat wrexec--other-root "**"))))
           (tool (wrexec--find-tool "write_file_in_scope"))
           (expected-here (expand-file-name "out.txt" wrexec--other-root))
           (not-expected  (expand-file-name "out.txt" wrexec--work-root)))
      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      (funcall (gptel-tool-function tool) #'wrexec--callback "out.txt" "payload")

      (expect (plist-get wrexec--result :success) :to-be t)
      (expect (plist-get wrexec--result :full_path) :to-equal expected-here)
      (expect (file-exists-p expected-here) :to-be-truthy)
      (expect (file-exists-p not-expected) :to-be nil))))

(provide 'work-root-execution-context-spec)
;;; work-root-execution-context-spec.el ends here
