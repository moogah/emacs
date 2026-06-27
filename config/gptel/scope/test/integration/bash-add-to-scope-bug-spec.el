;;; bash-add-to-scope-bug-spec.el --- RED PHASE: "which brew" add-to-scope should write correct path to scope drawer -*- lexical-binding: t; -*-

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
;;
;; Cycle-3 migration: tests now read/write the chat buffer's :PROPERTIES:
;; drawer via `jf/gptel-test--with-scope-drawer' instead of round-tripping
;; through an on-disk scope.yml.  The YAML loader was removed; the production
;; writer (`jf/gptel-scope--write-pattern-to-drawer') is drawer-based.

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

(defvar bug--callback-result nil)
(defvar bug--callback-raw nil)
(defvar bug--callback-invoked nil)

(defun bug--gptel-callback (result-json)
  (setq bug--callback-invoked t)
  (setq bug--callback-raw result-json)
  (setq bug--callback-result
        (json-parse-string result-json :object-type 'plist)))

(defun bug--find-tool (name)
  (cl-block nil
    (dolist (category-entry gptel--known-tools)
      (dolist (tool-entry (cdr category-entry))
        (when (string= (car tool-entry) name)
          (cl-return (cdr tool-entry)))))))

(defun bug--drawer-paths (operation)
  "Return the patterns in the current buffer's scope drawer for OPERATION.
OPERATION is a keyword like `:read', `:write'."
  (let ((key (jf/gptel-scope--map-operation-to-drawer-key operation)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (org-entry-get-multivalued-property (point) key))))

(defun bug--drawer-as-string ()
  "Return current buffer's text for substring-matching the rendered drawer."
  (buffer-substring-no-properties (point-min) (point-max)))


;;; Test Suites

;; ============================================================
;; SUITE 1: Characterization — confirm the denial path
;; These tests PASS (they characterize existing correct behavior
;; in the parser and pipeline, not the bugs).
;; ============================================================

(describe "Characterization: 'which brew' denial path"

  (before-each
    (setq bug--callback-result nil)
    (setq bug--callback-raw nil)
    (setq bug--callback-invoked nil))

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
    (let* ((config (helpers-spec-make-scope-config
                    :read '() :write '() :execute '() :modify '()
                    :deny '("**/.git/**" "**/runtime/**")
                    :auth-detection "deny"))
           (result (jf/gptel-scope--validate-command-semantics
                    "which brew" "/" config)))
      (expect result :not :to-be nil)
      (expect (plist-get result :error) :to-equal "not-in-scope")
      (expect (plist-get result :resource) :to-equal "/brew"))))


;; ============================================================
;; SUITE 2: BUG 1 — expansion UI should receive denied path as :resource
;; RED: FAILS because trigger-inline-expansion overwrites :resource
;;      with the allow-once composite "which brew:/"
;; ============================================================

(describe "Bug 1: expansion UI should receive denied path as :resource"

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

  (it "should NOT delegate to add-path-to-scope with a composite resource"
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer '()
        (spy-on 'jf/gptel-scope--add-path-to-scope :and-call-through)

        (jf/gptel-scope--add-bash-to-scope "which brew:/" "run_bash_command")

        ;; CORRECT: should NOT have called add-path-to-scope with the raw composite
        (if (spy-calls-count 'jf/gptel-scope--add-path-to-scope)
            (let ((call-args (spy-calls-args-for 'jf/gptel-scope--add-path-to-scope 0)))
              (expect (nth 0 call-args) :not :to-equal "which brew:/"))
          t)))))


;; ============================================================
;; SUITE 4: BUG 3 — drawer should not contain garbage patterns
;; RED: FAILS because "which brew:/**" is written to the write key
;; ============================================================

(describe "Bug 3: scope drawer should not contain garbage after add-to-scope"

  (it "drawer should not contain 'which brew:' after add-to-scope"
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer '()
        (jf/gptel-scope--add-bash-to-scope "which brew:/" "run_bash_command")

        (expect (bug--drawer-as-string) :not :to-match "which brew:"))))

  (it "for path_out_of_scope on :read-metadata, denied path should go to paths.read"
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer '()
        (jf/gptel-scope--add-bash-to-scope "/brew" "run_bash_command" :read-metadata)

        (let ((read-paths (bug--drawer-paths :read-metadata))
              (write-paths (bug--drawer-paths :write)))
          (expect (cl-some (lambda (p) (string-match-p "brew" p)) read-paths)
                  :to-be-truthy)
          (expect (cl-some (lambda (p) (string-match-p "brew" p)) write-paths)
                  :to-be nil))))))


;; ============================================================
;; SUITE 5: BUG 4 — end-to-end: add-to-scope should enable retry success
;; RED: FAILS because garbage path written, retry fails, callback
;;      receives error instead of success
;; ============================================================

(describe "Bug 4: end-to-end add-to-scope should enable retry success"

  (before-each
    (setq bug--callback-result nil)
    (setq bug--callback-raw nil)
    (setq bug--callback-invoked nil)
    ;; Reset expansion-machinery flags; the RED bug paths in these tests
    ;; can leave them set if the spy chain errors out mid-flow.
    (setq jf/gptel-scope--expansion-active nil)
    (setq jf/gptel-scope--expansion-queue nil))

  (after-each
    (setq jf/gptel-scope--expansion-active nil)
    (setq jf/gptel-scope--expansion-queue nil))

  (it "gptel callback receives success after add-to-scope correctly expands scope"
    (let ((tool (bug--find-tool "run_bash_command")))
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns _tool-name)
                    (let* ((validation-type (plist-get violation-info :validation-type))
                           (resource (plist-get violation-info :resource))
                           (spied-tool (plist-get violation-info :tool))
                           (denied-operation (plist-get violation-info :operation)))
                      (pcase validation-type
                        ('bash
                         (jf/gptel-scope--add-bash-to-scope
                          resource spied-tool denied-operation))
                        ('path
                         (jf/gptel-scope--add-path-to-scope
                          resource spied-tool denied-operation)))
                      (funcall callback
                               (json-serialize
                                (list :success t
                                      :patterns_added (vconcat patterns)))))))

          (spy-on 'jf/gptel-bash--execute-command
                  :and-return-value '(:output "/opt/homebrew/bin/brew"
                                      :exit_code 0
                                      :truncated nil :warnings nil :error nil))

          (let ((default-directory "/"))
            (funcall (gptel-tool-function tool)
                     #'bug--gptel-callback
                     "which brew"))

          (expect bug--callback-invoked :to-be t)
          (expect bug--callback-result :to-be-truthy)

          (expect (plist-get bug--callback-result :success) :to-be t)
          (expect (plist-get bug--callback-result :output)
                  :to-equal "/opt/homebrew/bin/brew")))))

  (it "scope drawer contains valid path pattern after add-to-scope (not garbage)"
    (let ((tool (bug--find-tool "run_bash_command")))
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns _tool-name)
                    (let* ((validation-type (plist-get violation-info :validation-type))
                           (resource (plist-get violation-info :resource))
                           (spied-tool (plist-get violation-info :tool))
                           (denied-operation (plist-get violation-info :operation)))
                      (pcase validation-type
                        ('bash
                         (jf/gptel-scope--add-bash-to-scope
                          resource spied-tool denied-operation))
                        ('path
                         (jf/gptel-scope--add-path-to-scope
                          resource spied-tool denied-operation)))
                      (funcall callback
                               (json-serialize
                                (list :success t
                                      :patterns_added (vconcat patterns)))))))

          (spy-on 'jf/gptel-bash--execute-command
                  :and-return-value '(:output "" :exit_code 0
                                      :truncated nil :warnings nil :error nil))

          (let ((default-directory "/"))
            (funcall (gptel-tool-function tool)
                     #'bug--gptel-callback
                     "which brew"))

          (expect (bug--drawer-as-string) :not :to-match "which brew:")

          (let ((all-patterns (append (bug--drawer-paths :read)
                                      (bug--drawer-paths :read-metadata)
                                      (bug--drawer-paths :write))))
            (expect (cl-some (lambda (p)
                               (jf/gptel-scope--glob-match-p "/brew" p))
                             all-patterns)
                    :to-be-truthy)))))))

(provide 'bash-add-to-scope-bug-spec)

;;; bash-add-to-scope-bug-spec.el ends here
