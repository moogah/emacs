;;; bash-add-to-scope-bug-spec.el --- RED PHASE: "which brew" add-to-scope should write correct path to scope.yml -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; RED PHASE TESTS: Assert CORRECT behavior for the "which brew" add-to-scope
;; flow. These tests FAIL against the current implementation, documenting the
;; bugs that need to be fixed.
;;
;; Observed in session: /Users/jefffarr/emacs-activities/test-2-2026-03-26/
;; The session also appears stuck after the expansion UI closes.
;;
;; CORRECT behavior:
;; 1. "which brew" with empty read scope is denied because "/brew" is not in
;;    paths.read. The expansion UI should show "/brew" as the denied resource.
;; 2. "Add to scope" should add the denied path to paths.read (not paths.write),
;;    since the denied operation was :read-metadata.
;; 3. After scope expansion, retry should succeed and tool body should execute.
;; 4. The gptel callback should receive a success result.
;;
;; Root cause chain:
;;
;; BUG 1: trigger-inline-expansion overwrites violation-info :resource with
;;   the allow-once composite format ("which brew:/") for ALL downstream
;;   consumers. The expansion UI and add-to-scope should see the denied path
;;   "/brew", not the allow-once key.
;;
;; BUG 2: add-bash-to-scope checks (string-match-p "/" resource) to decide if
;;   the resource is a directory path vs a command pattern. The composite format
;;   contains "/", causing misrouting to add-path-to-scope.
;;
;; BUG 3: For bash tool path_out_of_scope denials, add-to-scope should add the
;;   denied FILE PATH to the section matching the denied OPERATION (read for
;;   :read-metadata), not the tool's category operation (write for run_bash_command).
;;
;; BUG 4: After add-to-scope writes garbage, the retry fails and the gptel
;;   callback receives an error. With correct behavior, the retry should succeed
;;   and the callback should receive success.

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

(defun bug--parse-scope-yml ()
  "Parse the test scope.yml into a normalized plist."
  (let* ((parsed (jf/gptel-scope-yaml--parse-string (bug--read-scope-yml))))
    (jf/gptel-scope-yaml--normalize-keys parsed)))


;;; Test Suites

;; ============================================================
;; SUITE 1: Characterization — confirm the denial path
;; These tests PASS (they characterize existing correct behavior
;; in the parser and pipeline, not the bugs).
;; ============================================================

(describe "Characterization: 'which brew' denial path"

  (before-each
    (setq bug--temp-dir (make-temp-file "bug-scope-" t))
    (setq bug--scope-file (expand-file-name "scope.yml" bug--temp-dir))
    (setq bug--callback-result nil)
    (setq bug--callback-raw nil)
    (setq bug--callback-invoked nil)
    )

  (after-each
    (when (and bug--temp-dir (file-exists-p bug--temp-dir))
      (delete-directory bug--temp-dir t))
    )

  (it "bash-parser extracts :read-metadata on 'brew' (bare name, not path)"
    (let* ((parsed (jf/bash-parse "which brew"))
           (semantics (jf/bash-extract-semantics parsed))
           (file-ops (alist-get :filesystem (plist-get semantics :domains))))
      (expect file-ops :not :to-be nil)
      (let ((op (car file-ops)))
        (expect (plist-get op :operation) :to-equal :read-metadata)
        (expect (plist-get op :file) :to-equal "brew")
        (expect (plist-get op :command) :to-equal "which"))))

  (it "'brew' resolves to '/brew' when directory is '/'"
    (expect (expand-file-name "brew" "/") :to-equal "/brew"))

  (it "7-stage pipeline denies 'which brew' with empty read scope"
    (bug--make-session-scope-yml)
    (let ((config (helpers-spec-load-scope-config bug--scope-file)))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "which brew" "/" config)))
        (expect result :not :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")
        (expect (plist-get result :resource) :to-equal "/brew")))))


;; ============================================================
;; SUITE 2: BUG 1 — expansion UI should receive denied path as :resource
;; RED: FAILS because trigger-inline-expansion overwrites :resource
;;      with the allow-once composite "which brew:/"
;; ============================================================

(describe "Bug 1: expansion UI should receive denied path as :resource"

  (before-each
    )

  (after-each
    )

  (it "expansion UI :resource is the denied path '/brew'"
    (let* ((captured-violation nil)
           (validation-error (list :allowed nil
                                   :error "not-in-scope"
                                   :resource "/brew"
                                   :operation :read-metadata
                                   :validation-type 'bash
                                   :message "Path not in :read-metadata scope: /brew")))

      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (violation-info _callback _patterns _tool-name)
                (setq captured-violation violation-info)))

      (jf/gptel-scope--trigger-inline-expansion
       validation-error "run_bash_command"
       (lambda (_result) nil))

      (expect captured-violation :to-be-truthy)
      (expect (plist-get captured-violation :resource) :to-equal "/brew"))))


;; ============================================================
;; SUITE 3: BUG 2 — add-bash-to-scope should not misroute
;; RED: FAILS because composite resource "which brew:/" contains "/"
;;      and falls into the directory-path branch
;; ============================================================

(describe "Bug 2: add-bash-to-scope should add denied path to correct section"

  (before-each
    (setq bug--temp-dir (make-temp-file "bug-scope-" t))
    (setq bug--scope-file (expand-file-name "scope.yml" bug--temp-dir))
    (bug--make-session-scope-yml))

  (after-each
    (when (and bug--temp-dir (file-exists-p bug--temp-dir))
      (delete-directory bug--temp-dir t)))

  (it "should NOT delegate to add-path-to-scope with a composite resource"
    (spy-on 'jf/gptel-scope--add-path-to-scope :and-call-through)

    ;; Call with the composite resource (what trigger-inline-expansion produces)
    (jf/gptel-scope--add-bash-to-scope bug--scope-file "which brew:/" "run_bash_command")

    ;; CORRECT: should NOT have called add-path-to-scope with the raw composite
    ;; (It may call add-path-to-scope with a CLEAN path, but not with "which brew:/")
    (if (spy-calls-count 'jf/gptel-scope--add-path-to-scope)
        (let ((call-args (spy-calls-args-for 'jf/gptel-scope--add-path-to-scope 0)))
          ;; If add-path-to-scope was called, it should NOT receive the composite
          (expect (nth 1 call-args) :not :to-equal "which brew:/"))
      ;; Or it wasn't called at all (also acceptable)
      t)))


;; ============================================================
;; SUITE 4: BUG 3 — scope.yml should not contain garbage patterns
;; RED: FAILS because "which brew:/**" is written to paths.write
;; ============================================================

(describe "Bug 3: scope.yml should not contain garbage after add-to-scope"

  (before-each
    (setq bug--temp-dir (make-temp-file "bug-scope-" t))
    (setq bug--scope-file (expand-file-name "scope.yml" bug--temp-dir))
    (bug--make-session-scope-yml))

  (after-each
    (when (and bug--temp-dir (file-exists-p bug--temp-dir))
      (delete-directory bug--temp-dir t)))

  (it "scope.yml should not contain 'which brew:' after add-to-scope"
    ;; Simulate what happens: add-bash-to-scope with the composite resource
    (jf/gptel-scope--add-bash-to-scope bug--scope-file "which brew:/" "run_bash_command")

    ;; CORRECT: scope.yml should NOT have the composite key as a path pattern
    (let ((contents (bug--read-scope-yml)))
      (expect contents :not :to-match "which brew:")))

  (it "for path_out_of_scope on :read-metadata, denied path should go to paths.read"
    ;; When bash validation fails because a path is out of read scope,
    ;; "Add to scope" should add that path to paths.read
    ;; (The denied operation was :read-metadata → read-like)
    ;; Pass denied-operation :read-metadata so add-path-to-scope targets paths.read
    (jf/gptel-scope--add-bash-to-scope bug--scope-file "/brew" "run_bash_command" :read-metadata)

    (let* ((scope (bug--parse-scope-yml))
           (paths (plist-get scope :paths))
           (read-paths (plist-get paths :read))
           (write-paths (plist-get paths :write)))
      ;; CORRECT: the path should appear in paths.read, not paths.write
      ;; (The denied operation was :read-metadata, not write)
      (expect (cl-some (lambda (p) (string-match-p "brew" p)) read-paths)
              :to-be-truthy)
      (expect (cl-some (lambda (p) (string-match-p "brew" p)) write-paths)
              :to-be nil))))


;; ============================================================
;; SUITE 5: BUG 4 — end-to-end: add-to-scope should enable retry success
;; RED: FAILS because garbage path written, retry fails, callback
;;      receives error instead of success
;; ============================================================

(describe "Bug 4: end-to-end add-to-scope should enable retry success"

  (before-each
    (setq bug--temp-dir (make-temp-file "bug-scope-" t))
    (setq bug--scope-file (expand-file-name "scope.yml" bug--temp-dir))
    (setq bug--callback-result nil)
    (setq bug--callback-raw nil)
    (setq bug--callback-invoked nil)
    (bug--make-session-scope-yml)
    )

  (after-each
    (when (and bug--temp-dir (file-exists-p bug--temp-dir))
      (delete-directory bug--temp-dir t))
    )

  (it "gptel callback receives success after add-to-scope correctly expands scope"
    (let* ((tool (bug--find-tool "run_bash_command")))

      ;; Use real config loading pointed at our test scope.yml
      (with-temp-buffer
        (setq-local jf/gptel--branch-dir bug--temp-dir)

        ;; Simulate "Add to scope" using the real add-to-scope code path
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback patterns tool-name)
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
                    ;; Then invoke callback
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :patterns_added (vconcat patterns)))))))

        ;; Mock command execution (tool body)
        (spy-on 'jf/gptel-bash--execute-command
                :and-return-value '(:output "/opt/homebrew/bin/brew"
                                    :exit_code 0
                                    :truncated nil :warnings nil :error nil))

        ;; Invoke the real tool through the macro
        (funcall (gptel-tool-function tool)
                 #'bug--gptel-callback
                 "which brew"
                 "/")

        ;; Callback MUST fire
        (expect bug--callback-invoked :to-be t)
        (expect bug--callback-result :to-be-truthy)

        ;; CORRECT: After proper scope expansion, retry should succeed
        ;; and the tool body should execute, returning a success result
        (expect (plist-get bug--callback-result :success) :to-be t)
        (expect (plist-get bug--callback-result :output)
                :to-equal "/opt/homebrew/bin/brew"))))

  (it "scope.yml contains valid path pattern after add-to-scope (not garbage)"
    (let* ((tool (bug--find-tool "run_bash_command")))

      (with-temp-buffer
        (setq-local jf/gptel--branch-dir bug--temp-dir)

        ;; Simulate "Add to scope" with real updater
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback patterns tool-name)
                  (let* ((validation-type (plist-get violation-info :validation-type))
                         (resource (plist-get violation-info :resource))
                         (tool (plist-get violation-info :tool)))
                    (pcase validation-type
                      ('bash
                       (jf/gptel-scope--add-bash-to-scope
                        bug--scope-file resource tool))
                      ('path
                       (jf/gptel-scope--add-path-to-scope
                        bug--scope-file resource tool)))
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :patterns_added (vconcat patterns)))))))

        (spy-on 'jf/gptel-bash--execute-command
                :and-return-value '(:output "" :exit_code 0
                                    :truncated nil :warnings nil :error nil))

        (funcall (gptel-tool-function tool)
                 #'bug--gptel-callback
                 "which brew"
                 "/")

        ;; CORRECT: scope.yml should not contain the composite key
        (let ((contents (bug--read-scope-yml)))
          (expect contents :not :to-match "which brew:"))

        ;; CORRECT: scope.yml should contain a valid path pattern
        ;; that includes "/brew" or a parent glob like "/**"
        (let* ((scope (bug--parse-scope-yml))
               (paths (plist-get scope :paths))
               (all-patterns (append (plist-get paths :read)
                                     (plist-get paths :write))))
          ;; At least one pattern should match /brew
          (expect (cl-some (lambda (p)
                             (jf/gptel-scope--glob-match-p "/brew" p))
                           all-patterns)
                  :to-be-truthy))))))

(provide 'bash-add-to-scope-bug-spec)

;;; bash-add-to-scope-bug-spec.el ends here
