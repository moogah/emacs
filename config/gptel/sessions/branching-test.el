;;; branching-test.el --- Tests for session branching, format detection, and backward compatibility -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; ERT tests for org-mode session branching, format detection, format
;; inheritance, Local Variables writing, and backward compatibility.
;;
;; Spec reference: gptel/sessions - branching, format detection, Local Variables
;;
;; Tests cover:
;; - Format detection for .org and .md files (bead emacs-0295)
;; - Branch format inheritance from parent (bead emacs-p8wp)
;; - Local Variables writing in org-mode format (bead emacs-qigj)
;; - Backward compatibility with markdown (bead emacs-f6kc)
;;
;; Test naming convention:
;;   test-branch-<scenario>-org  - org-mode specific tests (bead emacs-g8gn)
;;   test-<operation>-md         - markdown compatibility tests (bead emacs-f6kc)
;;   test-dual-format-*          - both formats tested together

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

;;; ==========================================================
;;; Format detection: jf/gptel--session-file-format
;;; ==========================================================

(ert-deftest test-branch-format-detection-org-path ()
  "Spec: sessions § 'Format detection identifies .org files'

Verify jf/gptel--session-file-format returns :org for .org file paths."
  (should (eq (jf/gptel--session-file-format "/path/to/session.org") :org)))

(ert-deftest test-branch-format-detection-md-path ()
  "Spec: sessions § 'Format detection identifies .md files'

Verify jf/gptel--session-file-format returns :md for .md file paths."
  (should (eq (jf/gptel--session-file-format "/path/to/session.md") :md)))

(ert-deftest test-branch-format-detection-unknown ()
  "Spec: sessions § 'Format detection returns nil for unknown extensions'

Verify jf/gptel--session-file-format returns nil for unrecognized files."
  (should (null (jf/gptel--session-file-format "/path/to/session.txt")))
  (should (null (jf/gptel--session-file-format "/path/to/session"))))

(ert-deftest test-format-detection-nil-input ()
  "Verify format detection handles nil gracefully."
  (should (null (jf/gptel--session-file-format nil))))

(ert-deftest test-branch-format-detection-buffer ()
  "Spec: sessions § 'Format detection works with buffer objects'

Verify jf/gptel--session-file-format accepts buffer objects."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.org")
    (should (eq (jf/gptel--session-file-format (current-buffer)) :org)))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.md")
    (should (eq (jf/gptel--session-file-format (current-buffer)) :md))))

;;; ==========================================================
;;; Extension extraction: jf/gptel--get-session-file-extension
;;; ==========================================================

(ert-deftest test-branch-get-extension-org ()
  "Spec: sessions § 'Extension extraction returns org for org-mode sessions'"
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.org")
    (should (string= (jf/gptel--get-session-file-extension (current-buffer)) "org"))))

(ert-deftest test-branch-get-extension-md ()
  "Spec: sessions § 'Extension extraction returns md for markdown sessions'"
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.md")
    (should (string= (jf/gptel--get-session-file-extension (current-buffer)) "md"))))

(ert-deftest test-branch-get-extension-nil ()
  "Spec: sessions § 'Extension extraction returns nil for unknown format'"
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-session.txt")
    (should (null (jf/gptel--get-session-file-extension (current-buffer))))))

;;; ==========================================================
;;; Format inheritance (bead emacs-p8wp / emacs-g8gn)
;;; ==========================================================

(ert-deftest test-branch-format-inheritance-org ()
  "Spec: sessions § 'Branch inherits org format from parent session'

Verify that branching from an org-mode session produces an org-mode branch."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/sessions/test/branches/main/session.org")
    (let ((ext (jf/gptel--get-session-file-extension (current-buffer))))
      (should (string= ext "org"))
      (should (string= (concat "session." ext) "session.org")))))

(ert-deftest test-branch-format-inheritance-md-to-md ()
  "Spec: sessions § 'Branch inherits md format from parent session'

Verify that branching from a markdown session produces a markdown branch."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/sessions/test/branches/main/session.md")
    (let ((ext (jf/gptel--get-session-file-extension (current-buffer))))
      (should (string= ext "md"))
      (should (string= (concat "session." ext) "session.md")))))

;;; ==========================================================
;;; Local Variables writer: jf/gptel--write-local-variables
;;; ==========================================================

(ert-deftest test-local-variables-org-format ()
  "Spec: sessions § 'Local Variables written in org-mode comment syntax'

Verify jf/gptel--write-local-variables produces correct org-mode
Local Variables block with # comment prefix."
  (with-temp-buffer
    (jf/gptel--write-local-variables
     :org
     '((gptel-model . "gpt-4")
       (gptel--backend-name . "OpenAI")))
    (let ((content (buffer-string)))
      (should (string-match-p "^# Local Variables:$" content))
      (should (string-match-p "^# End:$" content))
      (should (string-match-p "^# gptel-model: \"gpt-4\"$" content))
      (should (string-match-p "^# gptel--backend-name: \"OpenAI\"$" content))
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
      (should (string-match-p "^<!-- Local Variables: -->$" content))
      (should (string-match-p "^<!-- End: -->$" content))
      (should (string-match-p "^<!-- gptel-model: \"gpt-4\" -->$" content))
      (should (string-match-p "^<!-- gptel--backend-name: \"OpenAI\" -->$" content))
      (should-not (string-match-p "^# " content)))))

(ert-deftest test-local-variables-org-complex-values ()
  "Spec: sessions § 'Local Variables serializes complex data structures'

Verify that complex values (lists, alists) are correctly serialized."
  (with-temp-buffer
    (jf/gptel--write-local-variables
     :org
     `((gptel--bounds . ((response (100 200) (300 400))
                         (context (50 90))))))
    (let ((content (buffer-string)))
      (should (string-match-p "^# Local Variables:$" content))
      (should (string-match-p "^# gptel--bounds:" content))
      (should (string-match-p "^# End:$" content)))))

(ert-deftest test-local-variables-unknown-format-error ()
  "Spec: sessions § 'Local Variables writer rejects unknown formats'"
  (with-temp-buffer
    (should-error
     (jf/gptel--write-local-variables :txt '((foo . "bar")))
     :type 'error)))

;;; ==========================================================
;;; Local Variables round-trip: write then read
;;; ==========================================================

(ert-deftest test-local-variables-roundtrip-org ()
  "Verify org-mode Local Variables survive write-then-read round-trip."
  (with-temp-session-dir
    (let* ((branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (org-file (expand-file-name "session.org" branch-dir)))
      (with-temp-file org-file
        (insert "* Test\n\n")
        (jf/gptel--write-local-variables
         :org
         '((gptel-model . "claude-opus-4")
           (gptel--backend-name . "Anthropic"))))
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
      (with-temp-file md-file
        (insert "# Test\n\n")
        (jf/gptel--write-local-variables
         :md
         '((gptel-model . "claude-opus-4")
           (gptel--backend-name . "Anthropic"))))
      (with-temp-buffer
        (insert-file-contents md-file)
        (setq buffer-file-name md-file)
        (let ((enable-local-variables t)
              (hack-local-variables-hook nil))
          (hack-local-variables))
        (should (equal (buffer-local-value 'gptel-model (current-buffer)) "claude-opus-4"))
        (should (equal (buffer-local-value 'gptel--backend-name (current-buffer)) "Anthropic"))))))

;;; ==========================================================
;;; Mixed-format branch scenarios (bead emacs-f6kc)
;;; ==========================================================

(ert-deftest test-mixed-format-branch ()
  "Verify format detection works for mixed-format edge case.
Scenario: markdown parent session, org child branch."
  (with-temp-session-dir
    (let* ((parent-dir (jf/gptel--create-branch-directory session-dir "main"))
           (child-dir (jf/gptel--create-branch-directory session-dir "20260311-child"))
           (parent-file (expand-file-name "session.md" parent-dir))
           (child-file (expand-file-name "session.org" child-dir)))
      (with-temp-file parent-file
        (insert "# Parent Session\n"))
      (with-temp-file child-file
        (insert "* Child Session\n"))
      (should (eq :md (jf/gptel--session-file-format parent-file)))
      (should (eq :org (jf/gptel--session-file-format child-file)))
      (with-temp-buffer
        (setq buffer-file-name parent-file)
        (should (equal "md" (jf/gptel--get-session-file-extension (current-buffer)))))
      (with-temp-buffer
        (setq buffer-file-name child-file)
        (should (equal "org" (jf/gptel--get-session-file-extension (current-buffer))))))))

(ert-deftest test-dual-format-branch-directories ()
  "Verify both format session files can coexist in separate branches."
  (with-temp-session-dir
    (let* ((md-branch (jf/gptel--create-branch-directory session-dir "md-branch"))
           (org-branch (jf/gptel--create-branch-directory session-dir "org-branch"))
           (md-file (expand-file-name "session.md" md-branch))
           (org-file (expand-file-name "session.org" org-branch)))
      (with-temp-file md-file (insert "# Markdown branch\n"))
      (with-temp-file org-file (insert "* Org branch\n"))
      (let ((branches (jf/gptel--list-branches session-dir)))
        (should (member "md-branch" branches))
        (should (member "org-branch" branches)))
      (should (eq :md (jf/gptel--session-file-format md-file)))
      (should (eq :org (jf/gptel--session-file-format org-file))))))

;;; ==========================================================
;;; Bounds filtering (used during branching)
;;; ==========================================================

(ert-deftest test-filter-bounds-preserves-complete-regions ()
  "Verify bounds filtering keeps regions entirely before branch point."
  (let* ((bounds '((response (10 50) (60 120) (200 300))))
         (filtered (jf/gptel--filter-bounds-before-position bounds 150)))
    (should (equal filtered '((response (10 50) (60 120)))))))

(ert-deftest test-filter-bounds-excludes-after ()
  "Verify bounds filtering excludes regions starting at or after branch point."
  (let* ((bounds '((response (10 50) (200 300))))
         (filtered (jf/gptel--filter-bounds-before-position bounds 200)))
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
