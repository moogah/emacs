;;; filesystem-test.el --- Tests for org-mode session filesystem structure -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERT tests for org-mode session filesystem directory structure.
;; Tests verify that session directories are created correctly and
;; that context file paths resolve to .org extension.
;;
;; Spec reference: gptel/sessions - filesystem, directory creation
;;
;; Test naming convention: test-directory-<scenario-slug>-org

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Test helpers

(defvar filesystem-test--temp-dirs nil
  "List of temp directories to clean up after tests.")

(defun filesystem-test--make-temp-dir ()
  "Create a temporary directory for testing and register for cleanup."
  (let ((dir (make-temp-file "gptel-fs-test-" t)))
    (push dir filesystem-test--temp-dirs)
    dir))

(defun filesystem-test--cleanup ()
  "Remove all temporary directories created during tests."
  (dolist (dir filesystem-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq filesystem-test--temp-dirs nil))

;;; Directory creation tests

(ert-deftest test-directory-creation-org-session-structure ()
  "Spec: sessions § 'Session directory structure created with branches/main/'

Verify that jf/gptel--create-session-core creates the full directory
hierarchy: session-dir/branches/main/ with session.org inside."
  (unwind-protect
      (let* ((temp-dir (filesystem-test--make-temp-dir))
             (session-dir (expand-file-name "test-session" temp-dir)))
        (make-directory session-dir t)
        (cl-letf (((symbol-function 'jf/gptel-scope-profile--create-for-session)
                   (lambda (_preset _dir &optional _root _paths) nil)))
          (let ((result (jf/gptel--create-session-core
                         "test-session-20260311"
                         session-dir
                         'executor)))
            ;; Verify branches/main/ directory exists
            (let ((branch-dir (plist-get result :branch-dir)))
              (should (file-directory-p branch-dir))
              (should (string-match-p "/branches/main/$" (file-name-as-directory branch-dir)))
              ;; Verify session.org exists inside branch dir
              (should (file-exists-p (expand-file-name "session.org" branch-dir)))
              ;; Verify metadata.yml exists
              (should (file-exists-p (expand-file-name "metadata.yml" branch-dir)))))))
    (filesystem-test--cleanup)))

(ert-deftest test-directory-creation-org-branch-directory ()
  "Spec: sessions § 'Branch directory creation produces correct path structure'

Verify jf/gptel--create-branch-directory creates the directory and
returns the correct absolute path."
  (unwind-protect
      (let* ((temp-dir (filesystem-test--make-temp-dir))
             (session-dir (expand-file-name "test-session" temp-dir))
             (branch-dir (jf/gptel--create-branch-directory session-dir "test-branch")))
        ;; Verify branch directory was created
        (should (file-directory-p branch-dir))
        ;; Verify path structure
        (should (string-match-p "/branches/test-branch$" branch-dir))
        ;; Verify branches parent dir exists
        (should (file-directory-p (jf/gptel--branches-dir-path session-dir))))
    (filesystem-test--cleanup)))

(ert-deftest test-directory-creation-org-context-file-path ()
  "Spec: sessions § 'Context file path resolves to session.org'

Verify jf/gptel--context-file-path returns path ending in session.org
for any branch directory."
  (let* ((branch-dir "/tmp/test-session/branches/main")
         (context-path (jf/gptel--context-file-path branch-dir)))
    ;; Should end with session.org (based on constant)
    (should (string-suffix-p "session.org" context-path))
    ;; Should be inside the branch directory
    (should (string-prefix-p branch-dir context-path))))

(ert-deftest test-directory-creation-org-symlink ()
  "Spec: sessions § 'Current symlink points to active branch'

Verify jf/gptel--update-current-symlink creates a working symlink
to the current branch."
  (unwind-protect
      (let* ((temp-dir (filesystem-test--make-temp-dir))
             (session-dir (expand-file-name "test-session" temp-dir)))
        ;; Create branch directory first
        (jf/gptel--create-branch-directory session-dir "main")
        ;; Create symlink
        (jf/gptel--update-current-symlink session-dir "main")
        ;; Verify symlink exists
        (let ((symlink (jf/gptel--current-symlink-path session-dir)))
          (should (file-symlink-p symlink))
          ;; Verify current branch name resolves correctly
          (should (string= (jf/gptel--get-current-branch-name session-dir) "main"))))
    (filesystem-test--cleanup)))

(ert-deftest test-directory-creation-org-valid-session-p ()
  "Spec: sessions § 'Valid session directory has branches subdirectory'

Verify jf/gptel--valid-session-directory-p returns t for directories
with a branches/ subdirectory."
  (unwind-protect
      (let* ((temp-dir (filesystem-test--make-temp-dir))
             (session-dir (expand-file-name "test-session" temp-dir)))
        ;; Before creating branches, should be invalid
        (make-directory session-dir t)
        (should-not (jf/gptel--valid-session-directory-p session-dir))
        ;; After creating branch, should be valid
        (jf/gptel--create-branch-directory session-dir "main")
        (should (jf/gptel--valid-session-directory-p session-dir)))
    (filesystem-test--cleanup)))

(ert-deftest test-directory-creation-org-valid-branch-p ()
  "Spec: sessions § 'Valid branch directory contains session context file'

Verify jf/gptel--valid-branch-directory-p returns t only when
the branch directory contains a session.org file."
  (unwind-protect
      (let* ((temp-dir (filesystem-test--make-temp-dir))
             (session-dir (expand-file-name "test-session" temp-dir))
             (branch-dir (jf/gptel--create-branch-directory session-dir "main")))
        ;; Without session.org, should be invalid
        (should-not (jf/gptel--valid-branch-directory-p branch-dir))
        ;; Create session.org
        (with-temp-file (jf/gptel--context-file-path branch-dir)
          (insert "# Test\n"))
        ;; Now should be valid
        (should (jf/gptel--valid-branch-directory-p branch-dir)))
    (filesystem-test--cleanup)))

(ert-deftest test-directory-creation-org-list-branches ()
  "Spec: sessions § 'List branches returns all branch names'

Verify jf/gptel--list-branches discovers all branch directories
within a session."
  (unwind-protect
      (let* ((temp-dir (filesystem-test--make-temp-dir))
             (session-dir (expand-file-name "test-session" temp-dir)))
        ;; Create multiple branches
        (jf/gptel--create-branch-directory session-dir "main")
        (jf/gptel--create-branch-directory session-dir "20260311-feature")
        (jf/gptel--create-branch-directory session-dir "20260311-experiment")
        ;; List branches
        (let ((branches (jf/gptel--list-branches session-dir)))
          (should (= (length branches) 3))
          (should (member "main" branches))
          (should (member "20260311-feature" branches))
          (should (member "20260311-experiment" branches))))
    (filesystem-test--cleanup)))

(ert-deftest test-directory-creation-org-agents-dir ()
  "Spec: sessions § 'Agents directory path resolves correctly within branch'

Verify jf/gptel--agents-dir-path returns correct path for agent
subdirectories within a branch."
  (let ((branch-dir "/tmp/test-session/branches/main"))
    (should (string= (jf/gptel--agents-dir-path branch-dir)
                     "/tmp/test-session/branches/main/agents"))))

(provide 'filesystem-test)
;;; filesystem-test.el ends here
