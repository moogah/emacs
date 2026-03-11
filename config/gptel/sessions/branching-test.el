;;; branching-test.el --- Mixed-format and branching backward compatibility tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; Tests for format detection helpers and mixed-format branching.
;; Validates that format-aware branching works correctly for both
;; org-mode and markdown session files.
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
  (add-to-list 'load-path sessions-dir)
  (unless (boundp 'jf/emacs-dir)
    (defvar jf/emacs-dir (expand-file-name "../../.." sessions-dir)))
  (require 'gptel-session-constants)
  (require 'gptel-session-logging)
  (require 'gptel-session-filesystem)
  (require 'gptel-session-branching))

;;; Helpers

(defmacro with-temp-session-dir (&rest body)
  "Execute BODY with a temporary session directory structure.
Binds `session-dir' and cleans up afterward."
  (declare (indent 0))
  `(let ((session-dir (make-temp-file "gptel-test-branch-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory session-dir t))))

;;; ============================================================
;;; Format detection: jf/gptel--session-file-format
;;; ============================================================

(ert-deftest test-format-detection-org-file ()
  "Verify format detection returns :org for .org file paths."
  (should (eq :org (jf/gptel--session-file-format "/path/to/session.org"))))

(ert-deftest test-format-detection-md-file ()
  "Verify format detection returns :md for .md file paths."
  (should (eq :md (jf/gptel--session-file-format "/path/to/session.md"))))

(ert-deftest test-format-detection-unknown-file ()
  "Verify format detection returns nil for unknown extensions."
  (should (null (jf/gptel--session-file-format "/path/to/session.txt"))))

(ert-deftest test-format-detection-nil-input ()
  "Verify format detection handles nil gracefully."
  (should (null (jf/gptel--session-file-format nil))))

(ert-deftest test-format-detection-buffer ()
  "Verify format detection works with buffer objects."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.org")
    (should (eq :org (jf/gptel--session-file-format (current-buffer)))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.md")
    (should (eq :md (jf/gptel--session-file-format (current-buffer))))))

;;; ============================================================
;;; Extension helper: jf/gptel--get-session-file-extension
;;; ============================================================

(ert-deftest test-extension-helper-org ()
  "Verify extension helper returns \"org\" for org buffers."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.org")
    (should (equal "org" (jf/gptel--get-session-file-extension (current-buffer))))))

(ert-deftest test-extension-helper-md ()
  "Verify extension helper returns \"md\" for markdown buffers."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.md")
    (should (equal "md" (jf/gptel--get-session-file-extension (current-buffer))))))

(ert-deftest test-extension-helper-unknown ()
  "Verify extension helper returns nil for unknown format buffers."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.txt")
    (should (null (jf/gptel--get-session-file-extension (current-buffer))))))

;;; ============================================================
;;; Local Variables writer: jf/gptel--write-local-variables
;;; ============================================================

(ert-deftest test-write-local-variables-org ()
  "Verify Local Variables writer produces correct org-mode format.
Org format uses # as comment prefix."
  (with-temp-buffer
    (jf/gptel--write-local-variables
     :org
     '((gptel-model . "gpt-4")
       (gptel--backend-name . "OpenAI")))
    (let ((output (buffer-string)))
      (should (string-match-p "^# Local Variables:" output))
      (should (string-match-p "^# gptel-model: \"gpt-4\"" output))
      (should (string-match-p "^# gptel--backend-name: \"OpenAI\"" output))
      (should (string-match-p "^# End:" output)))))

(ert-deftest test-write-local-variables-md ()
  "Verify Local Variables writer produces correct markdown format.
Markdown format uses HTML comments."
  (with-temp-buffer
    (jf/gptel--write-local-variables
     :md
     '((gptel-model . "gpt-4")
       (gptel--backend-name . "OpenAI")))
    (let ((output (buffer-string)))
      (should (string-match-p "^<!-- Local Variables: -->" output))
      (should (string-match-p "^<!-- gptel-model: \"gpt-4\" -->" output))
      (should (string-match-p "^<!-- gptel--backend-name: \"OpenAI\" -->" output))
      (should (string-match-p "^<!-- End: -->" output)))))

(ert-deftest test-write-local-variables-unknown-format-errors ()
  "Verify Local Variables writer signals error for unknown format."
  (should-error
   (with-temp-buffer
     (jf/gptel--write-local-variables :txt '((gptel-model . "gpt-4"))))
   :type 'error))

(ert-deftest test-write-local-variables-complex-values ()
  "Verify Local Variables writer serializes complex values with prin1.
Bounds values are nested alists that must survive round-trip serialization."
  (with-temp-buffer
    (let ((bounds '((response (10 50) (60 120)))))
      (jf/gptel--write-local-variables :org (list (cons 'gptel--bounds bounds)))
      (let ((output (buffer-string)))
        ;; Should contain the prin1 serialization
        (should (string-match-p "gptel--bounds:" output))
        (should (string-match-p "response" output))))))

;;; ============================================================
;;; Local Variables round-trip: write then read
;;; ============================================================

(ert-deftest test-local-variables-roundtrip-org ()
  "Verify org-mode Local Variables survive write-then-read round-trip."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (org-file (expand-file-name "session.org" branch-dir)))
      ;; Write a file with org-format Local Variables
      (with-temp-file org-file
        (insert "* Test\n\n")
        (jf/gptel--write-local-variables
         :org
         '((gptel-model . "claude-opus-4")
           (gptel--backend-name . "Anthropic"))))
      ;; Read back and verify
      (with-temp-buffer
        (insert-file-contents org-file)
        (setq buffer-file-name org-file)
        (let ((enable-local-variables t)
              (hack-local-variables-hook nil))
          (hack-local-variables))
        (should (equal (buffer-local-value 'gptel-model (current-buffer)) "claude-opus-4"))
        (should (equal (buffer-local-value 'gptel--backend-name (current-buffer)) "Anthropic"))))))

(ert-deftest test-local-variables-roundtrip-md ()
  "Verify markdown Local Variables survive write-then-read round-trip."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (md-file (expand-file-name "session.md" branch-dir)))
      ;; Write a file with markdown-format Local Variables
      (with-temp-file md-file
        (insert "# Test\n\n")
        (jf/gptel--write-local-variables
         :md
         '((gptel-model . "claude-opus-4")
           (gptel--backend-name . "Anthropic"))))
      ;; Read back and verify
      (with-temp-buffer
        (insert-file-contents md-file)
        (setq buffer-file-name md-file)
        (let ((enable-local-variables t)
              (hack-local-variables-hook nil))
          (hack-local-variables))
        (should (equal (buffer-local-value 'gptel-model (current-buffer)) "claude-opus-4"))
        (should (equal (buffer-local-value 'gptel--backend-name (current-buffer)) "Anthropic"))))))

;;; ============================================================
;;; Mixed-format branch scenario
;;; ============================================================

(ert-deftest test-mixed-format-branch ()
  "Verify format detection works for mixed-format edge case.
Scenario: markdown parent session, org child branch.
Format detection must correctly identify each file's format independently."
  (with-temp-session-dir
    (let* ((parent-dir (jf/gptel--create-branch-directory session-dir "main"))
           (child-dir (jf/gptel--create-branch-directory session-dir "20260311-child"))
           (parent-file (expand-file-name "session.md" parent-dir))
           (child-file (expand-file-name "session.org" child-dir)))
      ;; Create parent as markdown
      (with-temp-file parent-file
        (insert "# Parent Session\n"))
      ;; Create child as org
      (with-temp-file child-file
        (insert "* Child Session\n"))
      ;; Format detection should work independently per file
      (should (eq :md (jf/gptel--session-file-format parent-file)))
      (should (eq :org (jf/gptel--session-file-format child-file)))
      ;; Extension helper should also work
      (with-temp-buffer
        (setq buffer-file-name parent-file)
        (should (equal "md" (jf/gptel--get-session-file-extension (current-buffer)))))
      (with-temp-buffer
        (setq buffer-file-name child-file)
        (should (equal "org" (jf/gptel--get-session-file-extension (current-buffer))))))))

(ert-deftest test-dual-format-branch-directories ()
  "Verify both format session files can coexist in separate branches.
Different branches within the same session can have different formats."
  (with-temp-session-dir
    (let* ((md-branch (jf/gptel--create-branch-directory session-dir "md-branch"))
           (org-branch (jf/gptel--create-branch-directory session-dir "org-branch"))
           (md-file (expand-file-name "session.md" md-branch))
           (org-file (expand-file-name "session.org" org-branch)))
      ;; Create files in each branch
      (with-temp-file md-file (insert "# Markdown branch\n"))
      (with-temp-file org-file (insert "* Org branch\n"))
      ;; Both branches should exist
      (let ((branches (jf/gptel--list-branches session-dir)))
        (should (member "md-branch" branches))
        (should (member "org-branch" branches)))
      ;; Format detection per branch
      (should (eq :md (jf/gptel--session-file-format md-file)))
      (should (eq :org (jf/gptel--session-file-format org-file))))))

;;; ============================================================
;;; Bounds filtering (used during branching)
;;; ============================================================

(ert-deftest test-filter-bounds-preserves-complete-regions ()
  "Verify bounds filtering keeps regions entirely before branch point."
  (let* ((bounds '((response (10 50) (60 120) (200 300))))
         (filtered (jf/gptel--filter-bounds-before-position bounds 150)))
    ;; Should keep regions starting before 150
    (should (equal filtered '((response (10 50) (60 120)))))))

(ert-deftest test-filter-bounds-excludes-after ()
  "Verify bounds filtering excludes regions starting at or after branch point."
  (let* ((bounds '((response (10 50) (200 300))))
         (filtered (jf/gptel--filter-bounds-before-position bounds 200)))
    ;; Only region (10 50) starts before 200
    (should (equal filtered '((response (10 50)))))))

(ert-deftest test-validate-bounds-valid ()
  "Verify bounds validation accepts well-formed bounds."
  (should (jf/gptel--validate-bounds '((response (10 50) (60 120))))))

(ert-deftest test-validate-bounds-with-id ()
  "Verify bounds validation accepts regions with ID (BEG END ID)."
  (should (jf/gptel--validate-bounds '((response (10 50 "id1") (60 120 "id2"))))))

(ert-deftest test-validate-bounds-invalid-order ()
  "Verify bounds validation rejects region where start >= end."
  (should-error (jf/gptel--validate-bounds '((response (50 10))))))

(provide 'branching-test)
;;; branching-test.el ends here
