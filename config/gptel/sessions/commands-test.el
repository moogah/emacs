;;; commands-test.el --- Tests for session commands: org-mode and backward compatibility -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; ERT tests for org-mode session creation, auto-initialization, and
;; backward compatibility with markdown sessions.
;;
;; Spec reference: gptel/sessions - session creation, auto-init detection
;;
;; Test naming convention:
;;   test-session-<scenario>-org  - org-mode specific tests (bead emacs-g8gn)
;;   test-<operation>-md          - markdown compatibility tests (bead emacs-f6kc)
;;   test-dual-format-*           - both formats tested together

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load session modules from this directory
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (sessions-dir test-dir))
  (add-to-list 'load-path sessions-dir)
  (unless (boundp 'jf/emacs-dir)
    (defvar jf/emacs-dir (expand-file-name "../../.." sessions-dir)))
  (require 'gptel-session-constants)
  (require 'gptel-session-logging)
  (require 'gptel-session-filesystem))

;;; Helpers

(defmacro with-temp-session-dir (&rest body)
  "Execute BODY with a temporary session directory structure.
Binds `session-dir' and cleans up afterward."
  (declare (indent 0))
  `(let ((session-dir (make-temp-file "gptel-test-session-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory session-dir t))))

;;; ==========================================================
;;; Org-mode session creation tests (bead emacs-g8gn)
;;; ==========================================================

(ert-deftest test-session-creation-org ()
  "Spec: sessions § 'Session creation creates session.org with correct initial content'

Verify that jf/gptel--create-session-core creates a session.org file
(not session.md) given the current org-mode default constant."
  (with-temp-session-dir
    (let* ((sub-dir (expand-file-name "test-session" session-dir)))
      (make-directory sub-dir t)
      (cl-letf (((symbol-function 'jf/gptel-scope-profile--create-for-session)
                 (lambda (_preset _dir &optional _root _paths) nil)))
        (let ((result (jf/gptel--create-session-core
                       "test-session-20260311"
                       sub-dir
                       'executor
                       "# Test Session\n\n")))
          (let ((session-file (plist-get result :session-file)))
            (should (string-suffix-p "session.org" session-file))
            (should (file-exists-p session-file))
            (with-temp-buffer
              (insert-file-contents session-file)
              (should (string= (buffer-string) "# Test Session\n\n")))))))))

(ert-deftest test-session-creation-org-default-content ()
  "Spec: sessions § 'Default session content uses ### prefix'

Verify that when no initial-content is provided, the default
content is written to session.org."
  (with-temp-session-dir
    (let* ((sub-dir (expand-file-name "test-default" session-dir)))
      (make-directory sub-dir t)
      (cl-letf (((symbol-function 'jf/gptel-scope-profile--create-for-session)
                 (lambda (_preset _dir &optional _root _paths) nil)))
        (let* ((result (jf/gptel--create-session-core
                        "test-default-20260311"
                        sub-dir
                        'executor))
               (session-file (plist-get result :session-file)))
          (with-temp-buffer
            (insert-file-contents session-file)
            (should (string= (buffer-string) "###\n"))))))))

(ert-deftest test-session-creation-org-context-file-constant ()
  "Spec: sessions § 'Context file constant is session.org'

Verify that jf/gptel-session--context-file is set to 'session.org'
as changed by bead emacs-w7et."
  (should (string= jf/gptel-session--context-file "session.org")))

;;; ==========================================================
;;; Auto-init detection tests (bead emacs-g8gn)
;;; ==========================================================

(ert-deftest test-auto-init-detection-org ()
  "Spec: sessions § 'Auto-init detects .org files for session initialization'

Verify that context-file-path returns .org path and the file exists."
  (with-temp-session-dir
    (let* ((branches-dir (expand-file-name "branches/main" session-dir)))
      (make-directory branches-dir t)
      (let ((session-file (expand-file-name "session.org" branches-dir)))
        (with-temp-file session-file
          (insert "# Test\n"))
        (should (string= (file-name-nondirectory
                          (jf/gptel--context-file-path branches-dir))
                         "session.org"))
        (should (file-exists-p (jf/gptel--context-file-path branches-dir)))))))

(ert-deftest test-auto-init-detection-org-branch-pattern ()
  "Spec: sessions § 'Branch session path pattern matches .org files'

Verify that the branch session path pattern matches both .org and .md."
  (let ((org-path "/home/user/.gptel/sessions/my-session/branches/main/session.org")
        (md-path "/home/user/.gptel/sessions/my-session/branches/main/session.md"))
    (should (string-match "/branches/\\([^/]+\\)/session\\.org$" org-path))
    (should (string= (match-string 1 org-path) "main"))
    (should (string-match "/branches/\\([^/]+\\)/session\\.md$" md-path))
    (should (string= (match-string 1 md-path) "main"))))

;;; ==========================================================
;;; Backward compatibility tests (bead emacs-f6kc)
;;; ==========================================================

(ert-deftest test-session-creation-default-format ()
  "Verify that the default context file is now session.org.
The constant `jf/gptel-session--context-file' should be \"session.org\"
after the org-mode migration (bead emacs-w7et)."
  (should (equal jf/gptel-session--context-file "session.org")))

(ert-deftest test-session-creation-md ()
  "Verify markdown session files can be created and recognized.
Even though the default changed to org, writing a session.md file
must still produce a valid file that the filesystem layer can find."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (md-file (expand-file-name "session.md" branch-dir)))
      (with-temp-file md-file
        (insert "# Test Session\n\nHello world\n"))
      (should (file-exists-p md-file))
      (with-temp-buffer
        (insert-file-contents md-file)
        (should (string-match-p "# Test Session" (buffer-string)))))))

(ert-deftest test-auto-init-detection-md ()
  "Verify auto-init path detection works for .md session files."
  (let ((md-path "/home/user/.gptel/sessions/test-123/branches/main/session.md")
        (org-path "/home/user/.gptel/sessions/test-123/branches/main/session.org"))
    (should (string-suffix-p ".md" md-path))
    (should-not (string-suffix-p ".md" org-path))
    (should (string-match "/branches/\\([^/]+\\)/session\\.md$" md-path))
    (should (equal (match-string 1 md-path) "main"))))

(ert-deftest test-dual-format-detection ()
  "Verify both .md and .org files are recognized as session files."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main")))
      (let ((org-file (expand-file-name "session.org" branch-dir))
            (md-file (expand-file-name "session.md" branch-dir)))
        (with-temp-file org-file
          (insert "* Test Session\n"))
        (with-temp-file md-file
          (insert "# Test Session\n"))
        (should (file-exists-p org-file))
        (should (file-exists-p md-file))
        (should (string-suffix-p "session.org"
                                 (jf/gptel--context-file-path branch-dir)))))))

(ert-deftest test-html-comment-local-variables ()
  "Verify HTML-comment-style Local Variables can be read from markdown files."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (md-file (expand-file-name "session.md" branch-dir)))
      (with-temp-file md-file
        (insert "# Test Session\n\nSome content here.\n\n")
        (insert "<!-- Local Variables: -->\n")
        (insert "<!-- gptel-model: \"gpt-4\" -->\n")
        (insert "<!-- End: -->\n"))
      (with-temp-buffer
        (insert-file-contents md-file)
        (setq buffer-file-name md-file)
        (let ((enable-local-variables t)
              (hack-local-variables-hook nil))
          (hack-local-variables))
        (should (local-variable-p 'gptel-model))
        (should (equal (buffer-local-value 'gptel-model (current-buffer)) "gpt-4"))))))

(ert-deftest test-org-comment-local-variables ()
  "Verify org-comment-style Local Variables can be read from org files."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (org-file (expand-file-name "session.org" branch-dir)))
      (with-temp-file org-file
        (insert "* Test Session\n\nSome content here.\n\n")
        (insert "# Local Variables:\n")
        (insert "# gptel-model: \"gpt-4\"\n")
        (insert "# End:\n"))
      (with-temp-buffer
        (insert-file-contents org-file)
        (setq buffer-file-name org-file)
        (let ((enable-local-variables t)
              (hack-local-variables-hook nil))
          (hack-local-variables))
        (should (local-variable-p 'gptel-model))
        (should (equal (buffer-local-value 'gptel-model (current-buffer)) "gpt-4"))))))

(provide 'commands-test)
;;; commands-test.el ends here
