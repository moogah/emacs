;;; bash-multi-violation-expansion-spec.el --- RED PHASE: multi-violation bash commands leak through add-to-scope -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; RED PHASE TESTS: When a bash command has multiple denied file operations,
;; Stage 3 of the pipeline (`validate-file-operations') returns on the first
;; violation (`catch 'error-found'), the expansion UI fires for that single
;; resource, and `scope-authorize-tool-call' runs `on-allow' on approval
;; without re-validating — so every subsequent denied operation silently
;; bypasses scope.
;;
;; Observed in session:
;;   /Users/jefffarr/emacs-activities/test-scope-1-2026-04-16/session/branches/main
;;
;; The tool call
;;
;;     ls -la /usr/local/bin/brew 2>/dev/null || ls -la /opt/homebrew/bin/brew 2>/dev/null
;;
;; was run with an empty scope. Only `/dev/null' (the first extracted
;; operation, because redirections are extracted before positional args in
;; `bash-parser-file-ops.el:400-406') ended up in `scope.yml' after the user
;; clicked "Add to scope" — the `ls' reads on `/usr/local/bin/brew' and
;; `/opt/homebrew/bin/brew' never reached the expansion UI.
;;
;; CORRECT behavior:
;;   After the user approves "Add to scope" for a multi-violation command,
;;   every denied path should end up in the appropriate `scope.yml' section.
;;   The approval is semantically "authorize this command," not "authorize
;;   the first denied op and silently run the rest."
;;
;; The tests here are fix-agnostic: they pass if the pipeline surfaces all
;; violations at once (Option A), or if approval triggers re-validation
;; (Options B/C). They fail today because the approval short-circuits at the
;; first denial.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       ;; test/integration/ -> test/ -> scope/ -> gptel/ -> config/
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (config-dir (expand-file-name ".." gptel-dir))
       (contracts-dir (expand-file-name "test/contracts/" config-dir)))
  (add-to-list 'load-path contracts-dir)
  (require 'contract-core)
  (require 'contract-bash-parser)
  (contract--register-buttercup-matcher)
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  (require 'jf-gptel-scope-expansion
           (expand-file-name "scope/scope-expansion.el" gptel-dir))
  (require 'jf-gptel-scope-filesystem-tools
           (expand-file-name "scope/scope-filesystem-tools.el" gptel-dir))
  (require 'gptel))

;;; Test Infrastructure

(defvar multi--callback-result nil)
(defvar multi--callback-raw nil)
(defvar multi--callback-invoked nil)

(defun multi--gptel-callback (result-json)
  (setq multi--callback-invoked t)
  (setq multi--callback-raw result-json)
  (setq multi--callback-result
        (json-parse-string result-json :object-type 'plist)))

(defun multi--find-tool (name)
  (cl-block nil
    (dolist (category-entry gptel--known-tools)
      (dolist (tool-entry (cdr category-entry))
        (when (string= (car tool-entry) name)
          (cl-return (cdr tool-entry)))))))

(defun multi--drawer-paths (operation)
  "Return the patterns in the current buffer's scope drawer for OPERATION.
OPERATION is a keyword like `:read', `:write' that maps to a
`:GPTEL_SCOPE_*' drawer key via
`jf/gptel-scope--map-operation-to-drawer-key'."
  (let ((key (jf/gptel-scope--map-operation-to-drawer-key operation)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (org-entry-get-multivalued-property (point) key))))

(defun multi--install-add-to-scope-spy ()
  "Spy on expansion UI that mirrors the real \"Add to scope\" action:
take a single violation, call the real `add-bash-to-scope' updater, then
resolve the callback. This is faithful to what the production transient
does today (one violation in, one path added)."
  (spy-on 'jf/gptel-scope-prompt-expansion
          :and-call-fake
          (lambda (violation-info callback patterns _tool-name)
            (let ((validation-type (plist-get violation-info :validation-type))
                  (resource (plist-get violation-info :resource))
                  (tool (plist-get violation-info :tool))
                  (denied-operation (plist-get violation-info :operation)))
              (pcase validation-type
                ('bash
                 (jf/gptel-scope--add-bash-to-scope
                  resource tool denied-operation))
                ('path
                 (jf/gptel-scope--add-path-to-scope
                  resource tool denied-operation)))
              (funcall callback
                       (json-serialize
                        (list :success t
                              :patterns_added (vconcat patterns))))))))

;;; Characterization — document current behavior (these PASS)

(describe "Characterization: multi-violation parser + pipeline"

  (it "parser extracts both redirection and positional file ops"
    (let* ((parsed (jf/bash-parse "ls /foo 2>/dev/null"))
           (semantics (jf/bash-extract-semantics parsed))
           (file-ops (alist-get :filesystem (plist-get semantics :domains)))
           (by-file (mapcar (lambda (op)
                              (cons (plist-get op :file)
                                    (plist-get op :operation)))
                            file-ops)))
      ;; Both ops are extracted — the bug is downstream in validation/expansion
      (expect (assoc "/dev/null" by-file) :to-equal '("/dev/null" . :write))
      (expect (assoc "/foo" by-file) :to-equal '("/foo" . :read-directory))))

  (it "redirection ops precede positional ops in extraction order"
    ;; Extraction order drives which violation the pipeline surfaces.
    ;; See bash-parser-file-ops.el:400-406 — redirections are appended first.
    (let* ((parsed (jf/bash-parse "ls /foo 2>/dev/null"))
           (semantics (jf/bash-extract-semantics parsed))
           (file-ops (alist-get :filesystem (plist-get semantics :domains))))
      (expect (plist-get (nth 0 file-ops) :file) :to-equal "/dev/null")
      (expect (plist-get (nth 1 file-ops) :file) :to-equal "/foo")))

  (it "validate-command-semantics returns only the first denial"
    (let* ((config (helpers-spec-make-scope-config
                    :read '() :write '() :execute '() :modify '() :deny '()
                    :auth-detection "warn"))
           (result (jf/gptel-scope--validate-command-semantics
                    "ls /foo 2>/dev/null" "/workspace" config)))
      ;; Bug driver: the pipeline exits on the first denied op (/dev/null),
      ;; never reporting /foo. The expansion UI only ever sees this one.
      (expect (plist-get result :error) :to-equal "not-in-scope")
      (expect (plist-get result :resource) :to-equal "/dev/null")
      (expect (plist-get result :operation) :to-equal :write))))

;;; RED — end-to-end bug (these FAIL today)

(describe "Bug: multi-violation add-to-scope leaks subsequent denials"

  (before-each
    (setq multi--callback-result nil)
    (setq multi--callback-raw nil)
    (setq multi--callback-invoked nil)
    ;; Reset expansion-machinery flags; the RED bug paths in these tests
    ;; can leave them set if the spy chain errors out mid-flow.
    (setq jf/gptel-scope--expansion-active nil)
    (setq jf/gptel-scope--expansion-queue nil))

  (after-each
    (setq jf/gptel-scope--expansion-active nil)
    (setq jf/gptel-scope--expansion-queue nil))

  (it "drawer contains every denied path after user approves Add to scope"
    (let ((tool (multi--find-tool "run_bash_command")))
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (multi--install-add-to-scope-spy)
          (spy-on 'jf/gptel-bash--execute-command
                  :and-return-value '(:output ""
                                      :exit_code 0
                                      :truncated nil
                                      :warnings nil
                                      :error nil))

          (let ((default-directory "/workspace"))
            (funcall (gptel-tool-function tool)
                     #'multi--gptel-callback
                     "ls /foo 2>/dev/null"))

          (let ((read-paths (multi--drawer-paths :read))
                (write-paths (multi--drawer-paths :write)))
            ;; Current behavior: only /dev/null is added to write paths.
            ;; Correct behavior: /foo should also be in read paths —
            ;; the user's "Add to scope" means "authorize this command,"
            ;; not "authorize the first denial and silently run the rest."
            (expect write-paths :to-contain "/dev/null")
            (expect read-paths :to-contain "/foo"))))))

  (it "real-world session case: ls ABSPATH 2>/dev/null adds both paths"
    ;; The exact shape that broke in the session observed on 2026-04-16.
    (let ((tool (multi--find-tool "run_bash_command")))
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (multi--install-add-to-scope-spy)
          (spy-on 'jf/gptel-bash--execute-command
                  :and-return-value '(:output "-rwxr-xr-x 1 root 0 /opt/homebrew/bin/brew"
                                      :exit_code 0
                                      :truncated nil
                                      :warnings nil
                                      :error nil))

          (let ((default-directory "/workspace"))
            (funcall (gptel-tool-function tool)
                     #'multi--gptel-callback
                     "ls -la /usr/local/bin/brew 2>/dev/null"))

          (let ((read-paths (multi--drawer-paths :read))
                (write-paths (multi--drawer-paths :write)))
            (expect write-paths :to-contain "/dev/null")
            (expect read-paths :to-contain "/usr/local/bin/brew")))))))

(provide 'bash-multi-violation-expansion-spec)

;;; bash-multi-violation-expansion-spec.el ends here
