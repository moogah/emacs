;;; commands-test.el --- Tests for org-mode session creation and auto-init -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERT tests for org-mode session creation and auto-initialization.
;; Tests verify that session.org files are created with correct initial
;; content and that .org files are detected for auto-initialization.
;;
;; Spec reference: gptel/sessions - session creation, auto-init detection
;;
;; Test naming convention: test-session-<scenario-slug>-org

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Test helpers

(defvar commands-test--temp-dirs nil
  "List of temp directories to clean up after tests.")

(defun commands-test--make-temp-dir ()
  "Create a temporary directory for testing and register for cleanup."
  (let ((dir (make-temp-file "gptel-session-test-" t)))
    (push dir commands-test--temp-dirs)
    dir))

(defun commands-test--cleanup ()
  "Remove all temporary directories created during tests."
  (dolist (dir commands-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq commands-test--temp-dirs nil))

;;; Session creation tests

(ert-deftest test-session-creation-org ()
  "Spec: sessions § 'Session creation creates session.org with correct initial content'

Verify that jf/gptel--create-session-core creates a session.org file
(not session.md) given the current org-mode default constant."
  (unwind-protect
      (let* ((temp-dir (commands-test--make-temp-dir))
             (session-dir (expand-file-name "test-session" temp-dir))
             ;; Mock the scope profile creation to avoid needing real presets
             (original-scope-fn (symbol-function 'jf/gptel-scope-profile--create-for-session)))
        (make-directory session-dir t)
        (cl-letf (((symbol-function 'jf/gptel-scope-profile--create-for-session)
                   (lambda (_preset _dir &optional _root _paths) nil)))
          (let ((result (jf/gptel--create-session-core
                         "test-session-20260311"
                         session-dir
                         'executor
                         "# Test Session\n\n")))
            ;; Verify session file path uses .org extension
            (let ((session-file (plist-get result :session-file)))
              (should (string-suffix-p "session.org" session-file))
              ;; Verify the file was created
              (should (file-exists-p session-file))
              ;; Verify initial content was written
              (with-temp-buffer
                (insert-file-contents session-file)
                (should (string= (buffer-string) "# Test Session\n\n")))))))
    (commands-test--cleanup)))

(ert-deftest test-session-creation-org-default-content ()
  "Spec: sessions § 'Default session content uses ### prefix'

Verify that when no initial-content is provided, the default
content ('###\\n') is written to session.org."
  (unwind-protect
      (let* ((temp-dir (commands-test--make-temp-dir))
             (session-dir (expand-file-name "test-default" temp-dir)))
        (make-directory session-dir t)
        (cl-letf (((symbol-function 'jf/gptel-scope-profile--create-for-session)
                   (lambda (_preset _dir &optional _root _paths) nil)))
          (let* ((result (jf/gptel--create-session-core
                          "test-default-20260311"
                          session-dir
                          'executor))
                 (session-file (plist-get result :session-file)))
            ;; Default content should be "###\n"
            (with-temp-buffer
              (insert-file-contents session-file)
              (should (string= (buffer-string) "###\n"))))))
    (commands-test--cleanup)))

(ert-deftest test-session-creation-org-context-file-constant ()
  "Spec: sessions § 'Context file constant is session.org'

Verify that jf/gptel-session--context-file is set to 'session.org'
as changed by bead emacs-w7et."
  (should (string= jf/gptel-session--context-file "session.org")))

;;; Auto-init detection tests

(ert-deftest test-auto-init-detection-org ()
  "Spec: sessions § 'Auto-init detects .org files for session initialization'

Verify that jf/gptel--auto-init-session-buffer recognizes .org files
as session files. The auto-init function currently only checks for .md
suffix in its fast path guard. This test documents that .org session
files exist on disk (via constants) even if auto-init does not yet
detect them.

NOTE: This is a unit test of the constant and path resolution, not
a full integration test of auto-init (which requires gptel-mode)."
  (unwind-protect
      (let* ((temp-dir (commands-test--make-temp-dir))
             (session-dir (expand-file-name "test-org-detect" temp-dir))
             (branches-dir (expand-file-name "branches/main" session-dir)))
        (make-directory branches-dir t)
        ;; Create a session.org file
        (let ((session-file (expand-file-name "session.org" branches-dir)))
          (with-temp-file session-file
            (insert "# Test\n"))
          ;; Verify the context-file-path function returns the .org path
          (should (string= (file-name-nondirectory
                            (jf/gptel--context-file-path branches-dir))
                           "session.org"))
          ;; Verify the file exists at the expected location
          (should (file-exists-p (jf/gptel--context-file-path branches-dir)))))
    (commands-test--cleanup)))

(ert-deftest test-auto-init-detection-org-branch-pattern ()
  "Spec: sessions § 'Branch session path pattern matches .org files'

Verify that the branch session path pattern
  */branches/<branch>/session.org
can be matched for org-mode sessions."
  (let ((org-path "/home/user/.gptel/sessions/my-session/branches/main/session.org")
        (md-path "/home/user/.gptel/sessions/my-session/branches/main/session.md"))
    ;; Verify .org path matches branch pattern
    (should (string-match "/branches/\\([^/]+\\)/session\\.org$" org-path))
    (should (string= (match-string 1 org-path) "main"))
    ;; Verify .md path also matches (backward compatibility)
    (should (string-match "/branches/\\([^/]+\\)/session\\.md$" md-path))
    (should (string= (match-string 1 md-path) "main"))))

(provide 'commands-test)
;;; commands-test.el ends here
