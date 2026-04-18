;;; trigger-inline-expansion-resource-spec.el --- :resource preserves denied path -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; The expansion UI's "Add to scope" action needs the denied path in
;; violation-info :resource to know what to add to scope.yml. These
;; tests inject canonical validation errors (denied-pattern /
;; not-in-scope with :resource and :validation-type) into
;; trigger-inline-expansion and verify the resulting violation-info
;; preserves the denied path.

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

(describe "trigger-inline-expansion :resource for add-to-scope"

  (describe "bash tool with not-in-scope error"

    (it "preserves denied path for read-metadata operation"
      (let* ((captured-violation nil)
             (validation-error (list :allowed nil
                                     :error "not-in-scope"
                                     :resource "/brew"
                                     :operation :read-metadata
                                     :validation-type 'bash
                                     :message "Path not in read-metadata scope: /brew")))
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info _callback _patterns _tool-name)
                  (setq captured-violation violation-info)))

        (jf/gptel-scope--trigger-inline-expansion
         validation-error "run_bash_command"
         (lambda (_result) nil))

        (expect captured-violation :to-be-truthy)
        (expect (plist-get captured-violation :resource) :to-equal "/brew")))

    (it "preserves denied path for 'cat /etc/passwd'"
      (let* ((captured-violation nil)
             (validation-error (list :allowed nil
                                     :error "not-in-scope"
                                     :resource "/etc/passwd"
                                     :operation :read
                                     :validation-type 'bash
                                     :message "Path not in read scope: /etc/passwd")))
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info _callback _patterns _tool-name)
                  (setq captured-violation violation-info)))

        (jf/gptel-scope--trigger-inline-expansion
         validation-error "run_bash_command"
         (lambda (_result) nil))

        (expect captured-violation :to-be-truthy)
        (expect (plist-get captured-violation :resource) :to-equal "/etc/passwd")))

    (it "denied-pattern carries the matched path through expansion UI"
      (let* ((captured-violation nil)
             (validation-error (list :allowed nil
                                     :error "denied-pattern"
                                     :resource "/etc/passwd"
                                     :operation :read
                                     :validation-type 'bash
                                     :message "Path denied by scope: /etc/passwd")))
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info _callback _patterns _tool-name)
                  (setq captured-violation violation-info)))

        (jf/gptel-scope--trigger-inline-expansion
         validation-error "run_bash_command"
         (lambda (_result) nil))

        (expect captured-violation :to-be-truthy)
        (expect (plist-get captured-violation :resource) :to-equal "/etc/passwd"))))

  (describe "path tool with not-in-scope error"

    (it "preserves the denied path for read_file_in_scope"
      (let* ((captured-violation nil)
             (validation-error (list :allowed nil
                                     :error "not-in-scope"
                                     :resource "/outside/scope/file.txt"
                                     :tool "read_file_in_scope"
                                     :validation-type 'path
                                     :message "Path not in read scope")))
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info _callback _patterns _tool-name)
                  (setq captured-violation violation-info)))

        (jf/gptel-scope--trigger-inline-expansion
         validation-error "read_file_in_scope"
         (lambda (_result) nil))

        (expect captured-violation :to-be-truthy)
        (let ((resource (plist-get captured-violation :resource)))
          (expect resource :to-match "/outside/scope/file\\.txt"))))))

(provide 'trigger-inline-expansion-resource-spec)

;;; trigger-inline-expansion-resource-spec.el ends here
