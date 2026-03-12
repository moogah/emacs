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
;;
;; All tests use temporary files/directories and mocking.
;; No dependencies on repository file structure.

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

  (let (temp-file)
    (before-each
      (setq temp-file (make-temp-file "metadata-spec-")))
    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (it "expands relative paths to absolute"
      ;; Use the temp file's basename in its parent directory
      (let* ((dir (file-name-directory temp-file))
             (name (file-name-nondirectory temp-file))
             (default-directory dir)
             (metadata (jf/gptel-scope--gather-file-metadata name)))
        (expect (plist-get metadata :path) :to-match "^/"))))

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

  (let (temp-file)
    (before-each
      (setq temp-file (make-temp-file "metadata-spec-")))
    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (it "detects existing files"
      (let ((metadata (jf/gptel-scope--gather-file-metadata temp-file)))
        (expect (plist-get metadata :exists) :to-be t))))

  (it "detects existing directories"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp")))
      (expect (plist-get metadata :exists) :to-be t)))

  (it "detects non-existent files"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/definitely-does-not-exist-xyz123.txt")))
      (expect (plist-get metadata :exists) :to-be nil))))

;;; Git Repository Detection Tests

(describe "jf/gptel-scope--gather-file-metadata - git repository detection"

  (let (temp-file)
    (before-each
      (setq temp-file (make-temp-file "metadata-spec-")))
    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (it "detects git repository for files in repo"
      ;; Mock locate-dominating-file to simulate being in a git repo
      (spy-on 'locate-dominating-file :and-return-value "/fake/repo/")
      (spy-on 'jf/gptel-scope--file-is-git-tracked-p :and-return-value nil)
      (let ((metadata (jf/gptel-scope--gather-file-metadata temp-file)))
        (expect (plist-get metadata :git-repo) :to-be-truthy))))

  (it "returns nil for files outside git repo"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt")))
      (expect (plist-get metadata :git-repo) :to-be nil)))

  (it "git-repo path ends with repository root directory"
    ;; Mock to return a known repo path and verify it's returned as-is
    (spy-on 'locate-dominating-file :and-return-value "/fake/my-repo/")
    (spy-on 'jf/gptel-scope--file-is-git-tracked-p :and-return-value nil)
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/fake/my-repo/somefile.txt")))
      (when-let ((git-repo (plist-get metadata :git-repo)))
        ;; Verify it ends with a directory name (trailing slash)
        (expect git-repo :to-match "/$")
        ;; Verify the directory name is extractable
        (expect (file-name-nondirectory (directory-file-name git-repo))
                :to-equal "my-repo")))))

;;; Git Tracking Status Tests

(describe "jf/gptel-scope--gather-file-metadata - git tracking status"

  (let (temp-file)
    (before-each
      (setq temp-file (make-temp-file "metadata-spec-")))
    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file)))

    (it "detects git-tracked files"
      ;; Mock git to report the file as tracked
      (spy-on 'locate-dominating-file :and-return-value "/fake/repo/")
      (spy-on 'jf/gptel-scope--file-is-git-tracked-p :and-return-value t)
      (let ((metadata (jf/gptel-scope--gather-file-metadata temp-file)))
        (expect (plist-get metadata :git-tracked) :to-be t)))

    (it "returns nil for untracked files in git repo"
      ;; Mock git to report file is in a repo but not tracked
      (spy-on 'locate-dominating-file :and-return-value "/fake/repo/")
      (spy-on 'jf/gptel-scope--file-is-git-tracked-p :and-return-value nil)
      (let ((metadata (jf/gptel-scope--gather-file-metadata temp-file)))
        (expect (plist-get metadata :git-tracked) :to-be nil))))

  (it "returns nil for files outside git repo"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt")))
      (expect (plist-get metadata :git-tracked) :to-be nil)))

  (it "git-tracked is nil when git-repo is nil"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/test.txt")))
      (expect (plist-get metadata :git-repo) :to-be nil)
      (expect (plist-get metadata :git-tracked) :to-be nil))))

;;; File Type Detection Tests

(describe "jf/gptel-scope--gather-file-metadata - file type detection"

  (let (temp-file temp-dir)
    (before-each
      (setq temp-file (make-temp-file "metadata-spec-"))
      (setq temp-dir (make-temp-file "metadata-spec-dir-" t)))
    (after-each
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir)))

    (it "detects regular files"
      (let ((metadata (jf/gptel-scope--gather-file-metadata temp-file)))
        (expect (plist-get metadata :type) :to-equal 'file)))

    (it "detects directories"
      (let ((metadata (jf/gptel-scope--gather-file-metadata temp-dir)))
        (expect (plist-get metadata :type) :to-equal 'directory))))

  (it "returns 'other for non-existent files"
    (let ((metadata (jf/gptel-scope--gather-file-metadata "/tmp/nonexistent-xyz123.txt")))
      (expect (plist-get metadata :type) :to-equal 'other)))

  (let (temp-dir)
    (before-each
      (setq temp-dir (make-temp-file "metadata-spec-dir-" t)))
    (after-each
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir)))

    (it "detects config directory as directory"
      (let ((metadata (jf/gptel-scope--gather-file-metadata temp-dir)))
        (expect (plist-get metadata :type) :to-equal 'directory)))))

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
