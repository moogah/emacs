;;; branching-test.el --- Tests for org-mode session branching -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERT tests for org-mode session branching, format detection, format
;; inheritance, and Local Variables writing.
;;
;; Spec reference: gptel/sessions - branching, format detection, Local Variables
;;
;; Tests cover:
;; - Format detection for .org and .md files (bead emacs-0295)
;; - Branch format inheritance from parent (bead emacs-p8wp)
;; - Local Variables writing in org-mode format (bead emacs-qigj)
;;
;; Test naming convention: test-branch-<scenario-slug>-org

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Test helpers

(defvar branching-test--temp-dirs nil
  "List of temp directories to clean up after tests.")

(defun branching-test--make-temp-dir ()
  "Create a temporary directory for testing and register for cleanup."
  (let ((dir (make-temp-file "gptel-branch-test-" t)))
    (push dir branching-test--temp-dirs)
    dir))

(defun branching-test--cleanup ()
  "Remove all temporary directories created during tests."
  (dolist (dir branching-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq branching-test--temp-dirs nil))

;;; Format detection tests (bead emacs-0295)

(ert-deftest test-branch-format-detection-org-path ()
  "Spec: sessions § 'Format detection identifies .org files'

Verify jf/gptel--session-file-format returns :org for .org file paths.
Added by bead emacs-0295."
  (should (eq (jf/gptel--session-file-format "/path/to/session.org") :org)))

(ert-deftest test-branch-format-detection-md-path ()
  "Spec: sessions § 'Format detection identifies .md files'

Verify jf/gptel--session-file-format returns :md for .md file paths.
Added by bead emacs-0295."
  (should (eq (jf/gptel--session-file-format "/path/to/session.md") :md)))

(ert-deftest test-branch-format-detection-unknown ()
  "Spec: sessions § 'Format detection returns nil for unknown extensions'

Verify jf/gptel--session-file-format returns nil for unrecognized files."
  (should (null (jf/gptel--session-file-format "/path/to/session.txt")))
  (should (null (jf/gptel--session-file-format "/path/to/session"))))

(ert-deftest test-branch-format-detection-buffer ()
  "Spec: sessions § 'Format detection works with buffer objects'

Verify jf/gptel--session-file-format accepts buffer objects
and detects format from buffer-file-name."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.org")
    (should (eq (jf/gptel--session-file-format (current-buffer)) :org)))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.md")
    (should (eq (jf/gptel--session-file-format (current-buffer)) :md))))

;;; Extension extraction tests (bead emacs-0295)

(ert-deftest test-branch-get-extension-org ()
  "Spec: sessions § 'Extension extraction returns org for org-mode sessions'

Verify jf/gptel--get-session-file-extension returns \"org\" string.
Added by bead emacs-0295."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.org")
    (should (string= (jf/gptel--get-session-file-extension (current-buffer)) "org"))))

(ert-deftest test-branch-get-extension-md ()
  "Spec: sessions § 'Extension extraction returns md for markdown sessions'

Verify jf/gptel--get-session-file-extension returns \"md\" string."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.md")
    (should (string= (jf/gptel--get-session-file-extension (current-buffer)) "md"))))

(ert-deftest test-branch-get-extension-nil ()
  "Spec: sessions § 'Extension extraction returns nil for unknown format'

Verify jf/gptel--get-session-file-extension returns nil for unknown."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.txt")
    (should (null (jf/gptel--get-session-file-extension (current-buffer))))))

;;; Format inheritance tests (bead emacs-p8wp)

(ert-deftest test-branch-format-inheritance-org ()
  "Spec: sessions § 'Branch inherits org format from parent session'

Verify that branching from an org-mode session produces an org-mode
branch. The extension extraction function is the mechanism by which
format inheritance works in jf/gptel-branch-session."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/sessions/test/branches/main/session.org")
    (let ((ext (jf/gptel--get-session-file-extension (current-buffer))))
      ;; The branch session file should use the same extension
      (should (string= ext "org"))
      ;; Constructing the branch filename preserves format
      (should (string= (concat "session." ext) "session.org")))))

(ert-deftest test-branch-format-inheritance-md-to-md ()
  "Spec: sessions § 'Branch inherits md format from parent session'

Verify that branching from a markdown session produces a markdown branch.
Ensures backward compatibility."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/sessions/test/branches/main/session.md")
    (let ((ext (jf/gptel--get-session-file-extension (current-buffer))))
      (should (string= ext "md"))
      (should (string= (concat "session." ext) "session.md")))))

;;; Local Variables writing tests (bead emacs-qigj)

(ert-deftest test-local-variables-org-format ()
  "Spec: sessions § 'Local Variables written in org-mode comment syntax'

Verify jf/gptel--write-local-variables produces correct org-mode
Local Variables block with # comment prefix.
Added by bead emacs-qigj."
  (with-temp-buffer
    (jf/gptel--write-local-variables
     :org
     '((gptel-model . "gpt-4")
       (gptel--backend-name . "OpenAI")))
    (let ((content (buffer-string)))
      ;; Should use # comment prefix (org-mode style)
      (should (string-match-p "^# Local Variables:$" content))
      (should (string-match-p "^# End:$" content))
      ;; Should contain the variable assignments
      (should (string-match-p "^# gptel-model: \"gpt-4\"$" content))
      (should (string-match-p "^# gptel--backend-name: \"OpenAI\"$" content))
      ;; Should NOT use HTML comment syntax
      (should-not (string-match-p "<!--" content)))))

(ert-deftest test-local-variables-md-format ()
  "Spec: sessions § 'Local Variables written in markdown comment syntax'

Verify jf/gptel--write-local-variables produces correct markdown
Local Variables block with <!-- --> comment syntax."
  (with-temp-buffer
    (jf/gptel--write-local-variables
     :md
     '((gptel-model . "gpt-4")
       (gptel--backend-name . "OpenAI")))
    (let ((content (buffer-string)))
      ;; Should use HTML comment syntax (markdown style)
      (should (string-match-p "^<!-- Local Variables: -->$" content))
      (should (string-match-p "^<!-- End: -->$" content))
      ;; Should contain the variable assignments
      (should (string-match-p "^<!-- gptel-model: \"gpt-4\" -->$" content))
      (should (string-match-p "^<!-- gptel--backend-name: \"OpenAI\" -->$" content))
      ;; Should NOT use # comment syntax
      (should-not (string-match-p "^# " content)))))

(ert-deftest test-local-variables-org-complex-values ()
  "Spec: sessions § 'Local Variables serializes complex data structures'

Verify that complex values (lists, alists) are correctly serialized
with prin1-to-string in org-mode format."
  (with-temp-buffer
    (jf/gptel--write-local-variables
     :org
     `((gptel--bounds . ((response (100 200) (300 400))
                         (context (50 90))))))
    (let ((content (buffer-string)))
      ;; Should start with org-mode Local Variables header
      (should (string-match-p "^# Local Variables:$" content))
      ;; The bounds value should be serialized as a readable s-expression
      (should (string-match-p "^# gptel--bounds:" content))
      ;; Should end with org-mode End marker
      (should (string-match-p "^# End:$" content)))))

(ert-deftest test-local-variables-unknown-format-error ()
  "Spec: sessions § 'Local Variables writer rejects unknown formats'

Verify jf/gptel--write-local-variables signals error for unknown format."
  (with-temp-buffer
    (should-error
     (jf/gptel--write-local-variables :txt '((foo . "bar")))
     :type 'error)))

(provide 'branching-test)
;;; branching-test.el ends here
