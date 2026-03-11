;;; session-creation-spec.el --- Behavioral tests for session creation (write-side) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests verifying the write-side of session creation via
;; `jf/gptel--create-session-core'.
;;
;; Uses `with-captured-io' macro from persistence-test-helpers to intercept
;; all filesystem writes.  All custom persistence code runs for real;
;; only Emacs primitives (write-region, make-directory, make-symbolic-link,
;; etc.) are mocked.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load test helpers — use jf/emacs-dir (set by init.el, always available in test runner)
(load (expand-file-name "config/gptel/test/persistence-test-helpers.el" jf/emacs-dir) nil t)

;; Load production modules
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-metadata)
(require 'gptel-scope-profiles)
(require 'gptel-session-commands)

(describe "Session creation (write-side)"

  (describe "Basic session creation"

    (it "creates branches/main directory structure"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (expect (captured-dir-p captured-dirs "/sessions/test-session-123/branches")
                :to-be-truthy)
        (expect (captured-dir-p captured-dirs "/sessions/test-session-123/branches/main")
                :to-be-truthy)))

    (it "writes metadata.yml with session_id and preset name"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/metadata.yml")))
          (expect content :to-be-truthy)
          (expect content :to-match "session_id: \"test-session-123\"")
          (expect content :to-match "preset: \"executor\""))))

    (it "writes metadata.yml with created and updated timestamps"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/metadata.yml")))
          ;; ISO8601 timestamp pattern
          (expect content :to-match "created: \"[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T")
          (expect content :to-match "updated: \"[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"))))

    (it "writes session.md with default content"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.md")))
          (expect content :to-be-truthy)
          (expect content :to-equal "###\n"))))

    (it "writes session.md with custom initial content"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor
         "# My Session\n\n")
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.md")))
          (expect content :to-equal "# My Session\n\n"))))

    (it "creates current symlink pointing to branches/main"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (let ((target (captured-symlink-target
                       captured-symlinks
                       "/sessions/test-session-123/current")))
          (expect target :to-equal "branches/main")))))

  (describe "Return value"

    (it "returns plist with session info"
      (with-captured-io
        (let ((result (jf/gptel--create-session-core
                       "test-session-123" "/sessions/test-session-123" 'executor)))
          (expect (plist-get result :session-id) :to-equal "test-session-123")
          (expect (plist-get result :session-dir) :to-equal "/sessions/test-session-123")
          (expect (plist-get result :branch-name) :to-equal "main")
          (expect (plist-get result :branch-dir) :to-be-truthy)
          (expect (plist-get result :session-file) :to-be-truthy)))))

  (describe "Preset metadata recording"

    (it "records preset name as string in metadata.yml"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'researcher)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/metadata.yml")))
          (expect content :to-match "preset: \"researcher\"")))))

  (describe "Scope profile integration"

    (it "writes scope.yml via scope profile system"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        ;; scope.yml should always be written (even if minimal)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/scope.yml")))
          (expect content :to-be-truthy))))

    (it "writes scope.yml with preset scope configuration when available"
      ;; Set up a preset with inline scope defaults
      (let ((jf/gptel-preset--scope-defaults
             `((test-preset . (:paths (:read ("/home/user/project")))))))
        (with-captured-io
          (jf/gptel--create-session-core
           "test-session-123" "/sessions/test-session-123" 'test-preset)
          (let ((content (captured-file-content
                          captured-files
                          "/sessions/test-session-123/branches/main/scope.yml")))
            (expect content :to-be-truthy)
            (expect content :to-match "/home/user/project")))))

    (it "expands ${project_root} in scope.yml paths when project-root provided"
      (let ((jf/gptel-preset--scope-defaults
             `((test-preset . (:paths (:read ("${project_root}/src")))))))
        (with-captured-io
          (jf/gptel--create-session-core
           "test-session-123" "/sessions/test-session-123" 'test-preset
           nil nil "/home/user/my-project")
          (let ((content (captured-file-content
                          captured-files
                          "/sessions/test-session-123/branches/main/scope.yml")))
            (expect content :to-be-truthy)
            (expect content :to-match "/home/user/my-project/src")
            (expect content :not :to-match "\\${project_root}")))))

    (it "deep-merges worktree-paths with preset scope when both provided"
      (let ((jf/gptel-preset--scope-defaults
             `((test-preset . (:paths (:read ("/base/path")))))))
        (with-captured-io
          (jf/gptel--create-session-core
           "test-session-123" "/sessions/test-session-123" 'test-preset
           nil
           '(:paths (:read ("/extra/path")))
           nil)
          (let ((content (captured-file-content
                          captured-files
                          "/sessions/test-session-123/branches/main/scope.yml")))
            (expect content :to-be-truthy)
            ;; Both paths should be present after deep merge
            (expect content :to-match "/base/path")
            (expect content :to-match "/extra/path")))))

    (it "writes minimal scope.yml when preset has no scope configuration"
      (let ((jf/gptel-preset--scope-defaults nil))
        (with-captured-io
          (jf/gptel--create-session-core
           "test-session-123" "/sessions/test-session-123" 'no-scope-preset)
          (let ((content (captured-file-content
                          captured-files
                          "/sessions/test-session-123/branches/main/scope.yml")))
            ;; Should still write a scope.yml, even if minimal
            (expect content :to-be-truthy)))))))

(provide 'session-creation-spec)
;;; session-creation-spec.el ends here
