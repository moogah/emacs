;;; metadata-spec.el --- Unit tests for scope metadata gathering -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; UNIT TESTS: File metadata gathering for scope validation
;;
;; Tests jf/gptel-scope--gather-file-metadata function in isolation:
;; - Plist structure and keys
;; - Path expansion and resolution
;; - File existence detection
;; - Git repository detection
;; - Git tracking status
;; - File type detection (file/directory/other)
;; - Graceful degradation (missing git, non-existent files)
;;
;; Test organization:
;; 1. Plist structure (3 tests)
;; 2. Path handling (4 tests)
;; 3. File existence (3 tests)
;; 4. Git repository detection (3 tests)
;; 5. Git tracking status (4 tests)
;; 6. File type detection (4 tests)
;; 7. Graceful degradation (3 tests)

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-dir (expand-file-name ".." test-dir)))
  (require 'jf-gptel-scope-metadata (expand-file-name "scope-metadata.el" scope-dir)))

;;; Plist Structure Tests

(describe "jf/gptel-scope--gather-file-metadata - plist structure"

  (it "returns a plist"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt")))
      (expect (and (listp metadata)
                   (cl-evenp (length metadata)))
              :to-be t)))

  (it "contains all required keys"
    (let* ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt"))
           (keys (cl-loop for key in metadata by #'cddr collect key)))
      (expect keys :to-contain :path)
      (expect keys :to-contain :real-path)
      (expect keys :to-contain :exists)
      (expect keys :to-contain :git-tracked)
      (expect keys :to-contain :git-repo)
      (expect keys :to-contain :type)))

  (it "has exactly 6 key-value pairs"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt")))
      (expect (length metadata) :to-equal 12))))  ; 6 keys * 2 (key + value)

;;; Path Handling Tests

(describe "jf/gptel-scope--gather-file-metadata - path handling"

  (it "expands relative paths to absolute"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "Makefile")))
      (expect (plist-get metadata :path) :to-match "^/")))

  (it "preserves absolute paths"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt")))
      (expect (plist-get metadata :path) :to-equal "/tmp/test.txt")))

  (it "expands ~ to home directory"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "~/test.txt")))
      (expect (plist-get metadata :path) :to-match "^/")
      (expect (plist-get metadata :path) :not :to-match "^~")))

  (it "resolves symlinks for :real-path"
    ;; Test with actual file (not symlink) - real-path should equal path
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp")))
      (expect (plist-get metadata :real-path) :to-be-truthy))))

;;; File Existence Tests

(describe "jf/gptel-scope--gather-file-metadata - file existence"

  (it "detects existing files"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "Makefile")))
      (expect (plist-get metadata :exists) :to-be t)))

  (it "detects existing directories"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp")))
      (expect (plist-get metadata :exists) :to-be t)))

  (it "detects non-existent files"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/definitely-does-not-exist-xyz123.txt")))
      (expect (plist-get metadata :exists) :to-be nil))))

;;; Git Repository Detection Tests

(describe "jf/gptel-scope--gather-file-metadata - git repository detection"

  (it "detects git repository for files in repo"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "Makefile")))
      (expect (plist-get metadata :git-repo) :to-be-truthy)))

  (it "returns nil for files outside git repo"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt")))
      (expect (plist-get metadata :git-repo) :to-be nil)))

  (it "git-repo path ends with repository root directory"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "Makefile")))
      (when-let ((git-repo (plist-get metadata :git-repo)))
        (expect git-repo :to-match "emacs/$")))))

;;; Git Tracking Status Tests

(describe "jf/gptel-scope--gather-file-metadata - git tracking status"

  (it "detects git-tracked files"
    ;; Makefile is definitely tracked in the repo
    (let ((metadata (jf/gptel-scope--gather-file-metadata "Makefile")))
      (expect (plist-get metadata :git-tracked) :to-be t)))

  (it "returns nil for untracked files in git repo"
    ;; Runtime directory is gitignored
    (let ((metadata (jf/gptel-scope--gather-file-metadata "runtime/test.txt")))
      (expect (plist-get metadata :git-tracked) :to-be nil)))

  (it "returns nil for files outside git repo"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt")))
      (expect (plist-get metadata :git-tracked) :to-be nil)))

  (it "git-tracked is nil when git-repo is nil"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt")))
      (expect (plist-get metadata :git-repo) :to-be nil)
      (expect (plist-get metadata :git-tracked) :to-be nil))))

;;; File Type Detection Tests

(describe "jf/gptel-scope--gather-file-metadata - file type detection"

  (it "detects regular files"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "Makefile")))
      (expect (plist-get metadata :type) :to-equal 'file)))

  (it "detects directories"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp")))
      (expect (plist-get metadata :type) :to-equal 'directory)))

  (it "returns 'other for non-existent files"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/nonexistent-xyz123.txt")))
      (expect (plist-get metadata :type) :to-equal 'other)))

  (it "detects config directory as directory"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "config")))
      (expect (plist-get metadata :type) :to-equal 'directory))))

;;; Graceful Degradation Tests

(describe "jf/gptel-scope--gather-file-metadata - graceful degradation"

  (it "handles non-existent files without errors"
    (expect (jf/gptel-scope--gather-file-metadata "/tmp/does-not-exist.txt")
            :not :to-throw))

  (it "returns complete plist even for non-existent files"
    (let* ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/nonexistent.txt"))
           (keys (cl-loop for key in metadata by #'cddr collect key)))
      (expect (length keys) :to-equal 6)))

  (it "handles paths with special characters"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test-file_2024 (copy).txt")))
      (expect (plist-get metadata :path) :to-equal "/tmp/test-file_2024 (copy).txt"))))

(provide 'metadata-spec)
;;; metadata-spec.el ends here
