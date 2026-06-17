;;; relative-resolution-spec.el --- Relative paths resolve against work-root default-directory -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; RELATIVE PATH RESOLUTION AGAINST THE WORK ROOT (default-directory)
;;
;; Read-side end-to-end proof for the two CONFIRMED seams behind the
;; cwd<->scope-agreement invariant (register/invariant/cwd-scope-agreement)
;; and the work-root activation seam (register/boundary/work-root-activation-seam):
;;
;;   1. Filesystem tools resolve a relative filepath argument with a BARE
;;      `(expand-file-name filepath)' (scope-filesystem-tools.org:81,124,172
;;      for the tool bodies; scope-validation.org:244 for the scope check in
;;      `jf/gptel-scope--validate-filesystem-tool').  Bare `expand-file-name'
;;      resolves against the ambient `default-directory'.
;;
;;   2. The bash validator reads `default-directory' itself
;;      (`jf/gptel-scope--validate-bash-tool', scope-validation.org:299) and
;;      threads it through `jf/gptel-scope--validate-file-operation', which
;;      resolves relative op paths with `(expand-file-name path directory)'
;;      (scope-validation.org:420-421).
;;
;; The work-root activation seam binds a session buffer's `default-directory'
;; to the work root exactly once (the gptel-chat-mode-hook binder); because
;; `gptel--handle-tool-use' runs tool funcalls inside
;; `(with-current-buffer (plist-get info :buffer) ...)', that single
;; buffer-local set reaches every tool for BOTH validation and execution.
;; These specs pin that the validators actually consult `default-directory'.
;;
;; What this file proves (and the sibling path-resolution-spec.el does NOT):
;; path-resolution-spec.el passes an EXPLICIT `directory' argument to
;; `jf/gptel-scope--validate-command-semantics'.  This file instead binds the
;; AMBIENT `default-directory' and calls the entry points that READ it
;; implicitly (`jf/gptel-scope--validate-filesystem-tool', which takes no
;; directory argument, and `jf/gptel-scope--validate-bash-tool', which reads
;; `default-directory' from dynamic scope).  The SAME relative path lands in
;; or out of scope depending solely on which work root `default-directory'
;; points at.
;;
;; These specs FAIL if a tool ever hardcodes a resolution root or otherwise
;; ignores `default-directory': the "different work root" cases assert the
;; resolved absolute path FOLLOWS the bound directory.
;;
;; This file deliberately does NOT duplicate the absolute-path / fail-closed
;; coverage already green in path-validation-spec.el / path-resolution-spec.el.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies (mirrors sibling validation specs).
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (validation-dir test-dir)
       (scope-test-dir (expand-file-name ".." validation-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  ;; Requiring shell-tools transitively requires jf-gptel-scope-validation,
  ;; which is where both validator entry points live.
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir)))

;;; Filesystem read-side: bare expand-file-name resolves against default-directory

(describe "Filesystem tool: relative path resolves against the work-root default-directory"

  (it "ALLOWS a relative path that resolves under the work root in scope"
    ;; default-directory = /Users/x/proj/, paths.read = /Users/x/proj/**
    ;; A relative "config/x.el" must resolve to /Users/x/proj/config/x.el
    ;; (bare expand-file-name against the ambient default-directory) and be
    ;; allowed.  No explicit directory is passed to the validator.
    (let* ((default-directory "/Users/x/proj/")
           (scope-config (helpers-spec-make-scope-config
                          :read '("/Users/x/proj/**")))
           (result (jf/gptel-scope--validate-filesystem-tool
                    "read_file_in_scope" 'read '("config/x.el")
                    scope-config nil)))
      (expect (plist-get result :allowed) :to-be t)))

  (it "the SAME relative path resolves under a DIFFERENT work root and is OUT of scope"
    ;; Identical scope-config (paths.read = /Users/x/proj/**) and identical
    ;; relative arg "config/x.el", but default-directory = /Users/x/other/.
    ;; Resolution must FOLLOW the work root: /Users/x/other/config/x.el, which
    ;; is NOT under /Users/x/proj/** -> not-in-scope.  This is the case that
    ;; fails if a tool hardcodes a resolution root.
    (let* ((default-directory "/Users/x/other/")
           (scope-config (helpers-spec-make-scope-config
                          :read '("/Users/x/proj/**")))
           (result (jf/gptel-scope--validate-filesystem-tool
                    "read_file_in_scope" 'read '("config/x.el")
                    scope-config nil)))
      (expect (plist-get result :allowed) :to-be nil)
      (expect (plist-get result :error) :to-equal "not-in-scope")
      ;; The resolved resource the validator judged is the work-root-relative
      ;; absolute, proving resolution followed default-directory.
      (expect (plist-get result :resource)
              :to-equal "/Users/x/other/config/x.el")))

  (it "the SAME relative path lands in scope when the work root's scope covers it"
    ;; The dual of the previous case: bind default-directory to /Users/x/other/
    ;; and grant /Users/x/other/** -> the same relative arg is allowed.  Proves
    ;; the resolution genuinely tracks the bound work root rather than a fixed
    ;; "/Users/x/proj" baked anywhere.
    (let* ((default-directory "/Users/x/other/")
           (scope-config (helpers-spec-make-scope-config
                          :read '("/Users/x/other/**")))
           (result (jf/gptel-scope--validate-filesystem-tool
                    "read_file_in_scope" 'read '("config/x.el")
                    scope-config nil)))
      (expect (plist-get result :allowed) :to-be t))))

;;; Bash execution-side: validator reads default-directory implicitly

(describe "run_bash_command: relative op path resolves against the work-root default-directory"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "ALLOWS a relative-path command whose op resolves under the work root in scope"
    ;; jf/gptel-scope--validate-bash-tool reads `directory default-directory'
    ;; (it takes NO directory argument).  A relative read op "config/x.el"
    ;; must resolve to /Users/x/proj/config/x.el under paths.read.
    (let* ((default-directory "/Users/x/proj/")
           (scope-config (helpers-spec-make-scope-config
                          :read '("/Users/x/proj/**"))))
      (helpers-spec-mock-bash-parse "cat config/x.el" '("cat") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "config/x.el" :command "cat"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-bash-tool
                     "run_bash_command" '("cat config/x.el") scope-config)))
        (expect (plist-get result :allowed) :to-be t))))

  (it "the SAME relative-path command resolves under a DIFFERENT work root and is OUT of scope"
    ;; Identical scope-config and command, but default-directory = /Users/x/other/.
    ;; The extracted op resolves to /Users/x/other/config/x.el, outside
    ;; /Users/x/proj/** -> denied.  Confirms execution-path resolution tracks
    ;; the work root the buffer established, not a fixed root.
    (let* ((default-directory "/Users/x/other/")
           (scope-config (helpers-spec-make-scope-config
                          :read '("/Users/x/proj/**"))))
      (helpers-spec-mock-bash-parse "cat config/x.el" '("cat") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "config/x.el" :command "cat"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-bash-tool
                     "run_bash_command" '("cat config/x.el") scope-config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")
        ;; The resolved resource judged is the work-root-relative absolute.
        (expect (plist-get result :resource)
                :to-equal "/Users/x/other/config/x.el"))))

  (it "the SAME relative-path command lands in scope when the work root's scope covers it"
    ;; Dual case: default-directory = /Users/x/other/ with /Users/x/other/**
    ;; granted -> the same command is allowed.  Proves the work-root binding,
    ;; not a fixed path, drives the verdict.
    (let* ((default-directory "/Users/x/other/")
           (scope-config (helpers-spec-make-scope-config
                          :read '("/Users/x/other/**"))))
      (helpers-spec-mock-bash-parse "cat config/x.el" '("cat") t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "config/x.el" :command "cat"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-bash-tool
                     "run_bash_command" '("cat config/x.el") scope-config)))
        (expect (plist-get result :allowed) :to-be t)))))

(provide 'relative-resolution-spec)
;;; relative-resolution-spec.el ends here
