;;; trigger-inline-expansion-resource-spec.el --- RED: :resource should preserve denied path for add-to-scope -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; RED PHASE UNIT TESTS for trigger-inline-expansion :resource handling.
;;
;; BUG: trigger-inline-expansion overwrites violation-info :resource with the
;; allow-once composite format ("command:directory") for ALL downstream consumers.
;; The expansion UI — specifically the add-to-scope action — needs the DENIED PATH
;; to know what to add to scope.yml.
;;
;; The allow-once action also needs the composite key, but it currently gets it
;; from violation-info :resource too. The fix must preserve both:
;;   - denied path (for add-to-scope)
;;   - allow-once composite key (for allow-once)
;;
;; Tests assert CORRECT behavior and fail against current implementation.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope-tool-wrapper.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Tests

(describe "trigger-inline-expansion :resource for add-to-scope"

  (before-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (describe "bash tool with path_out_of_scope error"

    (it "expansion UI :resource should be the denied path, not the allow-once composite"
      ;; Validation error from 7-stage pipeline: /brew is out of scope
      (let* ((captured-violation nil)
             (validation-error (list :allowed nil
                                     :error "path_out_of_scope"
                                     :path "/brew"
                                     :operation :read-metadata
                                     :message "Path not in read-metadata scope: /brew"
                                     :tool "run_bash_command"
                                     :resource "which brew"
                                     :command "which brew"))
             (tool-args '("which brew" "/")))

        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info _callback _patterns _tool-name)
                  (setq captured-violation violation-info)))

        (jf/gptel-scope--trigger-inline-expansion
         validation-error "run_bash_command" tool-args 'bash
         (lambda (_result) nil))

        (expect captured-violation :to-be-truthy)
        ;; RED: Currently receives "which brew:/" (allow-once composite)
        ;; CORRECT: Should receive "/brew" (the denied path)
        (expect (plist-get captured-violation :resource) :to-equal "/brew")))

    (it "expansion UI :resource for 'cat /etc/passwd' should be '/etc/passwd'"
      (let* ((captured-violation nil)
             (validation-error (list :allowed nil
                                     :error "path_out_of_scope"
                                     :path "/etc/passwd"
                                     :operation :read
                                     :message "Path not in read scope: /etc/passwd"
                                     :tool "run_bash_command"
                                     :resource "cat /etc/passwd"
                                     :command "cat /etc/passwd"))
             (tool-args '("cat /etc/passwd" "/workspace")))

        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info _callback _patterns _tool-name)
                  (setq captured-violation violation-info)))

        (jf/gptel-scope--trigger-inline-expansion
         validation-error "run_bash_command" tool-args 'bash
         (lambda (_result) nil))

        (expect captured-violation :to-be-truthy)
        ;; RED: Currently receives "cat /etc/passwd:/workspace" (composite)
        ;; CORRECT: Should receive "/etc/passwd" (the denied path)
        (expect (plist-get captured-violation :resource) :to-equal "/etc/passwd")))

    (it "expansion UI :resource for command_denied should be the command name"
      (let* ((captured-violation nil)
             (validation-error (list :allowed nil
                                     :error "command_denied"
                                     :command "rm"
                                     :message "Command 'rm' is in deny list"
                                     :tool "run_bash_command"
                                     :resource "rm -rf /tmp/foo"))
             (tool-args '("rm -rf /tmp/foo" "/workspace")))

        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info _callback _patterns _tool-name)
                  (setq captured-violation violation-info)))

        (jf/gptel-scope--trigger-inline-expansion
         validation-error "run_bash_command" tool-args 'bash
         (lambda (_result) nil))

        (expect captured-violation :to-be-truthy)
        ;; For command_denied, resource should be the command, not a file path
        ;; build-violation-info maps command_denied → :command field
        ;; RED: Currently receives "rm -rf /tmp/foo:/workspace" (composite)
        ;; CORRECT: Should receive "rm" (the denied command)
        (expect (plist-get captured-violation :resource) :to-equal "rm"))))

  (describe "path tool with not-in-scope error"

    (it "expansion UI :resource for read_file should be the denied path"
      (let* ((captured-violation nil)
             (validation-error (list :allowed nil
                                     :error "not-in-scope"
                                     :resource "/outside/scope/file.txt"
                                     :tool "read_file"
                                     :message "Path not in read scope"))
             (tool-args '("/outside/scope/file.txt")))

        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info _callback _patterns _tool-name)
                  (setq captured-violation violation-info)))

        (jf/gptel-scope--trigger-inline-expansion
         validation-error "read_file" tool-args 'path
         (lambda (_result) nil))

        (expect captured-violation :to-be-truthy)
        ;; For path tools, resource should be the expanded file path
        (let ((resource (plist-get captured-violation :resource)))
          (expect resource :to-match "/outside/scope/file\\.txt")))))

  (describe "allow-once still works"
    ;; The fix must not break allow-once — it needs the composite key
    ;; to match what check-allow-once constructs from args

    (it "allow-once action stores a key that check-allow-once can find (bash tool)"
      (let* ((validation-error (list :allowed nil
                                     :error "path_out_of_scope"
                                     :path "/brew"
                                     :tool "run_bash_command"
                                     :resource "which brew"
                                     :command "which brew"))
             (tool-args '("which brew" "/")))

        ;; Simulate allow-once: the expansion UI needs a resource value that,
        ;; when stored via add-to-allow-once-list, can be found by check-allow-once
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback _patterns _tool-name)
                  ;; Store allow-once using the allow-once-resource key
                  (jf/gptel-scope-add-to-allow-once-list
                   (plist-get violation-info :tool)
                   (or (plist-get violation-info :allow-once-resource)
                       (plist-get violation-info :resource)))
                  (funcall callback
                           (json-serialize '(:success t :allowed_once t)))))

        (jf/gptel-scope--trigger-inline-expansion
         validation-error "run_bash_command" tool-args 'bash
         (lambda (_result) nil))

        ;; compute-allow-once-resource for bash produces "command:directory".
        ;; The allow-once action must store something that matches what the
        ;; production code will look up on the retry.
        (let* ((expected-resource
                (format "%s:%s" "which brew" (expand-file-name "/")))
               (found (jf/gptel-scope--check-allow-once
                       "run_bash_command" expected-resource)))
          (expect found :to-be t))))))

(provide 'trigger-inline-expansion-resource-spec)

;;; trigger-inline-expansion-resource-spec.el ends here
