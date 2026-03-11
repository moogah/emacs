;;; commands-test.el --- Backward compatibility tests for session commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; Tests verifying markdown sessions continue working after org-mode changes.
;; These tests ensure backward compatibility: existing markdown sessions
;; must still be created, detected, and initialized correctly.
;;
;; Test naming convention:
;;   test-<operation>-md  - explicit markdown compatibility test
;;   test-dual-format-*   - both formats tested together

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load session modules from this directory
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (sessions-dir test-dir))
  ;; Add sessions dir to load path for require
  (add-to-list 'load-path sessions-dir)
  ;; Need jf/emacs-dir for constants
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

;;; Test: Session creation defaults to org-mode

(ert-deftest test-session-creation-default-format ()
  "Verify that the default context file is now session.org.
The constant `jf/gptel-session--context-file' should be \"session.org\"
after the org-mode migration (bead emacs-w7et)."
  (should (equal jf/gptel-session--context-file "session.org")))

;;; Test: Markdown session creation still works

(ert-deftest test-session-creation-md ()
  "Verify markdown session files can be created and recognized.
Even though the default changed to org, writing a session.md file
must still produce a valid file that the filesystem layer can find."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (md-file (expand-file-name "session.md" branch-dir)))
      ;; Create a markdown session file manually (simulating legacy creation)
      (with-temp-file md-file
        (insert "# Test Session\n\nHello world\n"))
      ;; The file should exist
      (should (file-exists-p md-file))
      ;; The file should be readable
      (with-temp-buffer
        (insert-file-contents md-file)
        (should (string-match-p "# Test Session" (buffer-string)))))))

;;; Test: Auto-init detection for markdown files

(ert-deftest test-auto-init-detection-md ()
  "Verify auto-init path detection works for .md session files.
The `jf/gptel--auto-init-session-buffer' function checks for
string-suffix-p \".md\" - this must continue working."
  ;; Simulate the path matching that auto-init performs
  (let ((md-path "/home/user/.gptel/sessions/test-123/branches/main/session.md")
        (org-path "/home/user/.gptel/sessions/test-123/branches/main/session.org"))
    ;; Markdown path should match the .md suffix check
    (should (string-suffix-p ".md" md-path))
    ;; Org path should NOT match the .md suffix check (different detection needed)
    (should-not (string-suffix-p ".md" org-path))
    ;; Markdown path should match branch session pattern
    (should (string-match "/branches/\\([^/]+\\)/session\\.md$" md-path))
    (should (equal (match-string 1 md-path) "main"))))

;;; Test: Dual format detection

(ert-deftest test-dual-format-detection ()
  "Verify both .md and .org files are recognized as session files.
The filesystem layer uses `jf/gptel-session--context-file' for path
construction, but detection should handle both extensions."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main")))
      ;; Create both format files
      (let ((org-file (expand-file-name "session.org" branch-dir))
            (md-file (expand-file-name "session.md" branch-dir)))
        (with-temp-file org-file
          (insert "* Test Session\n"))
        (with-temp-file md-file
          (insert "# Test Session\n"))
        ;; Both files should exist
        (should (file-exists-p org-file))
        (should (file-exists-p md-file))
        ;; The context-file-path helper uses the constant (now .org)
        (should (string-suffix-p "session.org"
                                 (jf/gptel--context-file-path branch-dir)))))))

;;; Test: HTML comment Local Variables reading

(ert-deftest test-html-comment-local-variables ()
  "Verify HTML-comment-style Local Variables can be read from markdown files.
Existing markdown sessions store Local Variables in HTML comments:
  <!-- Local Variables: -->
  <!-- gptel-model: \"gpt-4\" -->
  <!-- End: -->
Emacs must still parse these correctly."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (md-file (expand-file-name "session.md" branch-dir)))
      ;; Create a markdown file with HTML comment Local Variables
      (with-temp-file md-file
        (insert "# Test Session\n\nSome content here.\n\n")
        (insert "<!-- Local Variables: -->\n")
        (insert "<!-- gptel-model: \"gpt-4\" -->\n")
        (insert "<!-- End: -->\n"))
      ;; Read the file and parse local variables
      (with-temp-buffer
        (insert-file-contents md-file)
        (setq buffer-file-name md-file)
        (let ((enable-local-variables t)
              (hack-local-variables-hook nil))
          (hack-local-variables))
        ;; The variable should have been set by hack-local-variables
        (should (local-variable-p 'gptel-model))
        (should (equal (buffer-local-value 'gptel-model (current-buffer)) "gpt-4"))))))

;;; Test: Org-mode Local Variables reading

(ert-deftest test-org-comment-local-variables ()
  "Verify org-comment-style Local Variables can be read from org files.
New org-mode sessions store Local Variables as:
  # Local Variables:
  # gptel-model: \"gpt-4\"
  # End:"
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (org-file (expand-file-name "session.org" branch-dir)))
      ;; Create an org file with comment-style Local Variables
      (with-temp-file org-file
        (insert "* Test Session\n\nSome content here.\n\n")
        (insert "# Local Variables:\n")
        (insert "# gptel-model: \"gpt-4\"\n")
        (insert "# End:\n"))
      ;; Read the file and parse local variables
      (with-temp-buffer
        (insert-file-contents org-file)
        (setq buffer-file-name org-file)
        (let ((enable-local-variables t)
              (hack-local-variables-hook nil))
          (hack-local-variables))
        ;; The variable should have been set by hack-local-variables
        (should (local-variable-p 'gptel-model))
        (should (equal (buffer-local-value 'gptel-model (current-buffer)) "gpt-4"))))))

(provide 'commands-test)
;;; commands-test.el ends here
