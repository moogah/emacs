;;; session-creation-spec.el --- Behavioral tests for session creation (write-side) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests verifying the write-side of session creation via
;; `jf/gptel--create-session-core'.
;;
;; Invariants verified (post-`session-creation-drawer-prepopulate'):
;;
;; 1. `session.org' is pre-populated with a `:PROPERTIES:' drawer
;;    containing `GPTEL_PRESET' (and `GPTEL_PARENT_SESSION_ID' for
;;    agent sessions) followed by the chat-mode empty-user-block
;;    template.  The drawer is the authoritative session-level
;;    configuration source (design Decision 4 / Decision 6).
;;
;; 2. No `metadata.yml' sidecar is written during session creation.
;;    The previous `metadata.yml'-backed path has been removed.
;;
;; 3. `scope.yml' is still written from the preset's scope profile
;;    (unchanged by this task).
;;
;; Uses `with-captured-io' macro from persistence-test-helpers to
;; intercept all filesystem writes.  All custom persistence code runs
;; for real; only Emacs primitives (write-region, make-directory,
;; make-symbolic-link, etc.) are mocked.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Ensure test directory is on load-path for require
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'persistence-test-helpers)

;; Load production modules
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
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

    (it "does not write metadata.yml during session creation"
      ;; Post-`session-creation-drawer-prepopulate': the drawer
      ;; embedded in `session.org' replaces the sidecar.  Capture
      ;; every write and assert no path ending in `metadata.yml'
      ;; landed in the hash.
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (let ((paths (hash-table-keys captured-files)))
          (expect (cl-some (lambda (p) (string-suffix-p "/metadata.yml" p)) paths)
                  :to-be nil))))

    (it "writes session.org with pre-populated PROPERTIES drawer by default"
      ;; Per design Decision 4, a fresh `session.org' begins with a
      ;; `:PROPERTIES:' drawer containing `GPTEL_PRESET' so the
      ;; chat-mode restore path applies the preset on first open.
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
          (expect content :to-be-truthy)
          (expect content :to-equal
                  (concat ":PROPERTIES:\n"
                          ":GPTEL_PRESET: executor\n"
                          ":END:\n"
                          "#+begin_user\n"
                          "\n"
                          "#+end_user\n")))))

    (it "writes session.org with custom initial content when provided"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor
         "# My Session\n\n")
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
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

  (describe "Preset is recorded in session.org drawer"

    (it "encodes preset name into GPTEL_PRESET line"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'researcher)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
          (expect content :to-match ":GPTEL_PRESET: researcher")))))

  (describe "Parent-session-id threading for agent sessions"

    (it "omits GPTEL_PARENT_SESSION_ID when parent-session-id is nil"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor
         nil nil nil nil)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
          (expect content :not :to-match "GPTEL_PARENT_SESSION_ID"))))

    (it "omits GPTEL_PARENT_SESSION_ID when parent-session-id is empty string"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor
         nil nil nil "")
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
          (expect content :not :to-match "GPTEL_PARENT_SESSION_ID"))))

    (it "writes GPTEL_PARENT_SESSION_ID when parent-session-id is a non-empty string"
      (with-captured-io
        (jf/gptel--create-session-core
         "agent-abc-20260421120000"
         "/sessions/agent-abc-20260421120000"
         'executor
         nil nil nil
         "parent-xyz-20260101000000")
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/agent-abc-20260421120000/branches/main/session.org")))
          (expect content :to-match ":GPTEL_PARENT_SESSION_ID: parent-xyz-20260101000000")))))

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
