;;; bash-add-to-scope-bug-spec.el --- Bug reproduction: "which brew" add-to-scope writes garbage to scope.yml -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; BUG REPRODUCTION: When user runs "which brew" and the expansion UI shows
;; (because "brew" resolves to "/brew" which is out of scope), choosing
;; "Add to scope" writes "which brew:/**" into paths.write in scope.yml.
;;
;; Observed in session: /Users/jefffarr/emacs-activities/test-2-2026-03-26/
;; The session also appears stuck after the expansion UI closes.
;;
;; Root cause chain (3 bugs):
;;
;; BUG 1: trigger-inline-expansion overwrites violation-info :resource with
;;   the allow-once composite format ("command:directory") for ALL use cases.
;;   add-to-scope receives "which brew:/" instead of the actual denied path "/brew".
;;
;; BUG 2: add-bash-to-scope checks (string-match-p "/" resource) to decide if
;;   the resource is a directory path vs a command pattern. The composite format
;;   "which brew:/" contains "/", so it falls into the directory-path branch and
;;   delegates to add-path-to-scope, which appends "/**" and writes to paths.write.
;;
;; BUG 3: add-path-to-scope uses run_bash_command's tool category (:operation write)
;;   to decide the target section. Even though the actual denied operation was
;;   :read-metadata, the resource gets written to paths.write because run_bash_command
;;   is categorized as a write tool.
;;
;; Session stuck (BUG 4 — separate): After add-to-scope modifies scope.yml, the
;;   macro retries validation. The retry may fail (garbage path doesn't help) or
;;   succeed (if the write pattern accidentally matches). Either way, the gptel
;;   callback should fire. This test checks whether the callback actually fires.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (config-dir (expand-file-name ".." gptel-dir))
       (contracts-dir (expand-file-name "test/contracts/" config-dir)))
  ;; Contract infrastructure
  (add-to-list 'load-path contracts-dir)
  (require 'contract-core)
  (require 'contract-bash-parser)
  (contract--register-buttercup-matcher)
  ;; Scope helpers
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  ;; Production scope modules
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  (require 'jf-gptel-scope-expansion
           (expand-file-name "scope/scope-expansion.el" gptel-dir))
  (require 'jf-gptel-scope-filesystem-tools
           (expand-file-name "scope/scope-filesystem-tools.el" gptel-dir))
  (require 'gptel))

;;; Test Infrastructure

(defvar bug--callback-result nil
  "Parsed plist from gptel callback.")

(defvar bug--callback-raw nil
  "Raw JSON string passed to gptel callback.")

(defvar bug--callback-invoked nil
  "Whether the gptel callback was invoked at all.")

(defvar bug--temp-dir nil
  "Temporary directory for test scope.yml files.")

(defvar bug--scope-file nil
  "Path to test scope.yml.")

(defun bug--gptel-callback (result-json)
  "Capture gptel callback invocation."
  (setq bug--callback-invoked t)
  (setq bug--callback-raw result-json)
  (setq bug--callback-result
        (json-parse-string result-json :object-type 'plist)))

(defun bug--find-tool (name)
  "Find tool NAME in gptel--known-tools registry."
  (cl-block nil
    (dolist (category-entry gptel--known-tools)
      (dolist (tool-entry (cdr category-entry))
        (when (string= (car tool-entry) name)
          (cl-return (cdr tool-entry)))))))

(defun bug--make-session-scope-yml ()
  "Create scope.yml matching the real session that triggered the bug.
Empty read paths, no write paths — this is the minimal config that
causes 'which brew' to be denied."
  (let ((yaml "paths:
  read:
    []
  write:
    []
  execute:
    []
  modify:
    []
  deny:
    - \"**/.git/**\"
    - \"**/runtime/**\"
bash_tools:
  deny:
    - \"sudo\"
cloud:
  auth_detection: \"deny\"
security:
  enforce_parse_complete: true
  max_coverage_threshold: 1
"))
    (with-temp-file bug--scope-file
      (insert yaml))))

(defun bug--read-scope-yml ()
  "Read and return the contents of the test scope.yml."
  (with-temp-buffer
    (insert-file-contents bug--scope-file)
    (buffer-string)))


;;; Test Suites

;; ============================================================
;; SUITE 1: Reproduce the exact denial path
;; "which brew" with empty read scope → denied at stage 5
;; ============================================================

(describe "Bug reproduction: 'which brew' denial path"

  (before-each
    (setq bug--temp-dir (make-temp-file "bug-scope-" t))
    (setq bug--scope-file (expand-file-name "scope.yml" bug--temp-dir))
    (setq bug--callback-result nil)
    (setq bug--callback-raw nil)
    (setq bug--callback-invoked nil)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (when (and bug--temp-dir (file-exists-p bug--temp-dir))
      (delete-directory bug--temp-dir t))
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (it "bash-parser extracts :read-metadata on 'brew' (bare name, not path)"
    (let* ((parsed (jf/bash-parse "which brew"))
           (semantics (jf/bash-extract-semantics parsed))
           (file-ops (alist-get :filesystem (plist-get semantics :domains))))
      ;; "which" handler extracts read-metadata for its argument
      (expect file-ops :not :to-be nil)
      (let ((op (car file-ops)))
        (expect (plist-get op :operation) :to-equal :read-metadata)
        (expect (plist-get op :file) :to-equal "brew")
        (expect (plist-get op :command) :to-equal "which"))))

  (it "'brew' resolves to '/brew' when directory is '/'"
    ;; This is why the command gets denied — "brew" relative to "/" = "/brew"
    (expect (expand-file-name "brew" "/") :to-equal "/brew"))

  (it "7-stage pipeline denies 'which brew' with empty read scope"
    (bug--make-session-scope-yml)
    (let ((config (helpers-spec-load-scope-config bug--scope-file)))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "which brew" "/" config)))
        ;; Stage 5 denies: /brew not in empty read paths
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        ;; The denied path is /brew (brew resolved relative to /)
        (expect (plist-get result :path) :to-equal "/brew")))))


;; ============================================================
;; SUITE 2: BUG 1 — trigger-inline-expansion overwrites :resource
;; with allow-once composite format
;; ============================================================

(describe "Bug 1: trigger-inline-expansion overwrites :resource with allow-once format"

  (before-each
    (setq bug--temp-dir (make-temp-file "bug-scope-" t))
    (setq bug--scope-file (expand-file-name "scope.yml" bug--temp-dir))
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (when (and bug--temp-dir (file-exists-p bug--temp-dir))
      (delete-directory bug--temp-dir t))
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (it "expansion UI receives composite 'command:directory' as :resource instead of denied path"
    (let* ((captured-violation nil)
           ;; Validation error from the pipeline — note :path is "/brew"
           (validation-error (list :allowed nil
                                   :error "path_out_of_scope"
                                   :path "/brew"
                                   :operation :read-metadata
                                   :message "Path not in read-metadata scope: /brew"
                                   :tool "run_bash_command"
                                   :resource "which brew"
                                   :command "which brew"))
           (tool-args '("which brew" "/")))

      ;; Capture what the expansion UI receives
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (violation-info _callback _patterns _tool-name)
                (setq captured-violation violation-info)))

      (jf/gptel-scope--trigger-inline-expansion
       validation-error "run_bash_command" tool-args
       (lambda (_result) nil))

      ;; BUG: :resource should be "/brew" (the denied path) but instead
      ;; it's the allow-once composite "which brew:/"
      (expect captured-violation :to-be-truthy)
      (let ((resource (plist-get captured-violation :resource)))
        ;; This documents the CURRENT (broken) behavior:
        ;; The resource is the allow-once composite, NOT the denied path
        (expect resource :to-equal "which brew:/")
        ;; What it SHOULD be (for add-to-scope to work correctly):
        ;; (expect resource :to-equal "/brew")
        ))))


;; ============================================================
;; SUITE 3: BUG 2 — add-bash-to-scope misroutes composite resource
;; to add-path-to-scope because it contains "/"
;; ============================================================

(describe "Bug 2: add-bash-to-scope misroutes composite resource as directory path"

  (before-each
    (setq bug--temp-dir (make-temp-file "bug-scope-" t))
    (setq bug--scope-file (expand-file-name "scope.yml" bug--temp-dir))
    (bug--make-session-scope-yml))

  (after-each
    (when (and bug--temp-dir (file-exists-p bug--temp-dir))
      (delete-directory bug--temp-dir t)))

  (it "composite resource 'which brew:/' contains '/' so falls into path branch"
    ;; This is the routing check in add-bash-to-scope
    (let ((resource "which brew:/"))
      ;; The function checks: (string-match-p "/" resource)
      ;; "which brew:/" obviously contains "/"
      (expect (string-match-p "/" resource) :to-be-truthy)))

  (it "add-bash-to-scope delegates to add-path-to-scope for composite resource"
    (spy-on 'jf/gptel-scope--add-path-to-scope)

    ;; Call with the composite resource (what trigger-inline-expansion produces)
    (jf/gptel-scope--add-bash-to-scope bug--scope-file "which brew:/" "run_bash_command")

    ;; BUG: Falls into path branch instead of command branch
    (expect 'jf/gptel-scope--add-path-to-scope :to-have-been-called)

    ;; Verify what was passed to add-path-to-scope
    (let ((call-args (spy-calls-args-for 'jf/gptel-scope--add-path-to-scope 0)))
      ;; First arg: scope-file, second: the composite resource, third: tool
      (expect (nth 1 call-args) :to-equal "which brew:/"))))


;; ============================================================
;; SUITE 4: BUG 3 — add-path-to-scope writes garbage to paths.write
;; ============================================================

(describe "Bug 3: add-path-to-scope writes 'which brew:/**' to paths.write"

  (before-each
    (setq bug--temp-dir (make-temp-file "bug-scope-" t))
    (setq bug--scope-file (expand-file-name "scope.yml" bug--temp-dir))
    (bug--make-session-scope-yml))

  (after-each
    (when (and bug--temp-dir (file-exists-p bug--temp-dir))
      (delete-directory bug--temp-dir t)))

  (it "writes composite resource to paths.write (run_bash_command is write tool)"
    ;; run_bash_command has :operation write in tool categories
    (let ((category (cdr (assoc "run_bash_command" jf/gptel-scope--tool-categories))))
      (expect (plist-get category :operation) :to-equal 'write))

    ;; Call add-path-to-scope as add-bash-to-scope would
    (jf/gptel-scope--add-path-to-scope bug--scope-file "which brew:/" "run_bash_command")

    ;; Read scope.yml to see what was written
    (let ((contents (bug--read-scope-yml)))
      ;; BUG: "which brew:/**" appears in paths.write
      ;; (the trailing "/" gets "/**" appended by add-path-to-scope)
      (expect contents :to-match "which brew:")
      ;; Parse the YAML to verify it's specifically in paths.write
      (let* ((parsed (jf/gptel-scope-yaml--parse-string contents))
             (normalized (jf/gptel-scope--normalize-plist-keys parsed))
             (paths (plist-get normalized :paths))
             (write-paths (plist-get paths :write)))
        (expect (cl-some (lambda (p) (string-match-p "which brew:" p))
                         write-paths)
                :to-be-truthy))))

  (it "the written pattern 'which brew:/**' is not a valid glob path"
    ;; This pattern will never match any real file path
    ;; It would need to be a real path like "/brew" or "/usr/local/bin/brew"
    (let ((garbage-pattern "which brew:/**"))
      ;; No real path starts with "which brew:"
      (expect (jf/gptel-scope--matches-pattern "/brew" garbage-pattern) :to-be nil)
      (expect (jf/gptel-scope--matches-pattern "/usr/local/bin/brew" garbage-pattern) :to-be nil))))


;; ============================================================
;; SUITE 5: Session stuck — does the gptel callback fire?
;; Full end-to-end: tool invoked → denied → add-to-scope →
;; scope.yml updated with garbage → macro retries → retry fails →
;; callback should still fire
;; ============================================================

(describe "Bug 4: session stuck — gptel callback after add-to-scope"

  (before-each
    (setq bug--temp-dir (make-temp-file "bug-scope-" t))
    (setq bug--scope-file (expand-file-name "scope.yml" bug--temp-dir))
    (setq bug--callback-result nil)
    (setq bug--callback-raw nil)
    (setq bug--callback-invoked nil)
    (bug--make-session-scope-yml)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (when (and bug--temp-dir (file-exists-p bug--temp-dir))
      (delete-directory bug--temp-dir t))
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (it "gptel callback fires after add-to-scope with garbage path (retry fails)"
    (let* ((config (helpers-spec-load-scope-config bug--scope-file))
           (tool (bug--find-tool "run_bash_command"))
           (load-count 0))

      ;; Mock config loading: returns same config each time
      ;; (In real life, the second load would include the garbage pattern,
      ;; but it won't help validate "/brew" anyway)
      (spy-on 'jf/gptel-scope--load-config
              :and-call-fake
              (lambda ()
                (cl-incf load-count)
                config))

      ;; Simulate "Add to scope" — the expansion callback fires but
      ;; we skip actually writing scope.yml (would need writable scope-file
      ;; context which requires buffer-local jf/gptel--branch-dir)
      ;; Instead, directly invoke callback with success JSON
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (_violation-info callback _patterns _tool-name)
                (funcall callback
                         (json-serialize
                          (list :success t
                                :patterns_added (vector "which brew:/**"))))))

      ;; Invoke the real tool through the macro
      (funcall (gptel-tool-function tool)
               #'bug--gptel-callback
               "which brew"
               "/")

      ;; KEY CHECK: did the gptel callback fire at all?
      (expect bug--callback-invoked :to-be t)

      ;; Config was loaded twice: initial + retry after expansion
      (expect load-count :to-equal 2)

      ;; The retry should FAIL because the garbage pattern doesn't help
      ;; So the callback should receive an error (not success)
      (expect bug--callback-result :to-be-truthy)
      (expect (plist-get bug--callback-result :success) :not :to-be t)))

  (it "gptel callback fires after add-to-scope when scope.yml is actually updated"
    ;; This test writes to the real scope.yml and verifies the full round-trip
    ;; including reading the updated (garbage) config on retry
    (let* ((tool (bug--find-tool "run_bash_command")))

      ;; Use the real config loading but point it at our test scope.yml
      ;; by setting the buffer-local branch-dir
      (with-temp-buffer
        (setq-local jf/gptel--branch-dir bug--temp-dir)

        ;; Simulate "Add to scope" using Pattern A: direct suffix invocation
        ;; This calls the REAL add-to-scope code path
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback patterns tool-name)
                  ;; Simulate what the real transient suffix does
                  (let* ((validation-type (plist-get violation-info :validation-type))
                         (resource (plist-get violation-info :resource))
                         (tool (plist-get violation-info :tool)))
                    ;; Call the real add-to-scope updater
                    (pcase validation-type
                      ('bash
                       (jf/gptel-scope--add-bash-to-scope
                        bug--scope-file resource tool))
                      ('path
                       (jf/gptel-scope--add-path-to-scope
                        bug--scope-file resource tool)))
                    ;; Then invoke callback (as the real suffix does)
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :patterns_added (vconcat patterns)))))))

        ;; Invoke the real tool through the macro
        (funcall (gptel-tool-function tool)
                 #'bug--gptel-callback
                 "which brew"
                 "/")

        ;; Callback MUST fire for gptel FSM to advance
        (expect bug--callback-invoked :to-be t)

        ;; Verify scope.yml was actually modified with the garbage
        (let ((contents (bug--read-scope-yml)))
          (expect contents :to-match "which brew:"))

        ;; The callback result — should be an error since retry fails
        ;; OR success if the garbage pattern accidentally matches
        ;; Either way, the callback MUST have fired
        (expect bug--callback-result :to-be-truthy)))))

(provide 'bash-add-to-scope-bug-spec)

;;; bash-add-to-scope-bug-spec.el ends here
