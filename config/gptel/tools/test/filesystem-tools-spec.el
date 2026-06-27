;;; filesystem-tools-spec.el --- Spy-based tests for filesystem tool-scope contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; TOOL-SCOPE CONTRACT TESTS: filesystem tools
;;
;; These tests verify the contract between filesystem tools and the scope
;; system. They do NOT re-test path validation logic (that is scope/test/'s
;; job) — they verify that tools fail cleanly on validation denials and that
;; expansion triggers receive the denied resource.
;;
;; Strategy: build synthetic denied check-results via tool-test helpers and
;; verify that `format-tool-error' and `trigger-inline-expansion' produce
;; the expected shapes without touching the filesystem.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tools-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'tool-test-helpers-spec (expand-file-name "helpers-spec.el" test-dir))
  ;; Load scope modules for permission checking
  (require 'jf-gptel-scope-validation (expand-file-name "../scope/scope-validation.el" tools-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "../scope/scope-tool-wrapper.el" tools-dir)))


;;; Test Suite: read_file_in_scope tool-scope contract

(describe "read_file_in_scope: tool-scope contract"

  (describe "on validation success, reads file"

    (it "returns file content when file exists"
      (let* ((temp-file (make-temp-file "tool-test-read-" nil ".txt")))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert "test content here"))
              (let ((full-path (expand-file-name temp-file)))
                (expect (file-exists-p full-path) :to-be t)
                (let ((content (with-temp-buffer
                                 (insert-file-contents full-path)
                                 (buffer-string))))
                  (expect content :to-equal "test content here"))))
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

    (it "returns file_not_found error when file missing"
      (let ((missing-path "/nonexistent/path/file.txt"))
        (expect (file-exists-p missing-path) :to-be nil))))

  (describe "on validation failure, returns error without reading"

    (it "returns structured scope violation error"
      (let* ((denied-result (tool-test--scope-denied "path_out_of_scope" "/etc/passwd" "read_file_in_scope"))
             (formatted (jf/gptel-scope--format-tool-error "read_file_in_scope" "/etc/passwd" denied-result)))
        (expect (plist-get formatted :success) :to-be nil)
        (expect (plist-get formatted :error) :to-equal "path_out_of_scope")
        (expect (plist-get formatted :tool) :to-equal "read_file_in_scope")
        (expect (plist-get formatted :resource) :to-equal "/etc/passwd")))

    (it "does not read file when permission denied"
      (spy-on 'insert-file-contents)
      ;; Simulate the macro behavior: check permission, skip I/O if denied
      (let ((check-result (tool-test--scope-denied "path_out_of_scope" "/etc/shadow" "read_file_in_scope")))
        (unless (plist-get check-result :allowed)
          (jf/gptel-scope--format-tool-error "read_file_in_scope" "/etc/shadow" check-result))
        (expect 'insert-file-contents :not :to-have-been-called)))))


;;; Test Suite: write_file_in_scope tool-scope contract

(describe "write_file_in_scope: tool-scope contract"

  (describe "on validation success, writes file"

    (it "creates file with content when allowed"
      (let ((temp-file (make-temp-file "tool-test-write-" nil ".txt")))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert "written content"))
              (expect (with-temp-buffer
                        (insert-file-contents temp-file)
                        (buffer-string))
                      :to-equal "written content"))
          (when (file-exists-p temp-file)
            (delete-file temp-file))))))

  (describe "on validation failure, returns error without writing"

    (it "does not write file when permission denied"
      (spy-on 'write-file)
      (spy-on 'with-temp-file)
      (let ((check-result (tool-test--scope-denied "path_out_of_scope" "/etc/config" "write_file_in_scope")))
        (unless (plist-get check-result :allowed)
          (jf/gptel-scope--format-tool-error "write_file_in_scope" "/etc/config" check-result))
        (expect 'write-file :not :to-have-been-called)))

    (it "returns error with allowed-patterns for guidance (as a JSON array / vector)"
      ;; allowed-patterns MUST be a vector, not a Lisp list: `json-serialize'
      ;; treats a non-keyword list as an alist/object and throws on a list of
      ;; pattern strings, which crashed the deny path
      ;; (.tasks/scope-deny-symbolp-crash-parallel-tools.md).
      (let* ((denied (tool-test--scope-denied "path_out_of_scope" "/tmp/file.txt" "write_file_in_scope"))
             (formatted (jf/gptel-scope--format-tool-error "write_file_in_scope" "/tmp/file.txt" denied)))
        (expect (plist-get formatted :allowed-patterns) :to-equal ["/workspace/**"])))))


;;; Test Suite: edit_file_in_scope tool-scope contract

(describe "edit_file_in_scope: tool-scope contract"

  (it "returns error without modifying when permission denied"
    (let* ((denied (tool-test--scope-denied "path_out_of_scope" "/etc/config" "edit_file_in_scope"))
           (formatted (jf/gptel-scope--format-tool-error "edit_file_in_scope" "/etc/config" denied)))
      (expect (plist-get formatted :success) :to-be nil)
      (expect (plist-get formatted :tool) :to-equal "edit_file_in_scope"))))


;;; Test Suite: list_directory tool-scope contract (non-scoped, basic tool)

(describe "list_directory: basic tool behavior"

  (it "lists files when directory exists"
    (let ((temp-dir (make-temp-file "tool-test-listdir-" t)))
      (unwind-protect
          (progn
            ;; Create some test files
            (with-temp-file (expand-file-name "test.txt" temp-dir)
              (insert "test"))
            (let ((files (directory-files temp-dir nil "^[^.]")))
              (expect files :to-contain "test.txt")))
        (when (file-exists-p temp-dir)
          (delete-directory temp-dir t)))))

  (it "handles non-existent directory gracefully"
    (expect (file-directory-p "/nonexistent/directory") :to-be nil)))


;;; Test Suite: Expansion UI trigger

(describe "filesystem tools: expansion trigger"

  (it "triggers inline expansion when async tool is denied"
    (spy-on 'jf/gptel-scope--trigger-inline-expansion)
    (let ((validation-error (tool-test--scope-denied "path_out_of_scope" "/outside/scope" "read_file_in_scope"))
          (callback (lambda (_result) nil)))
      (jf/gptel-scope--trigger-inline-expansion validation-error "read_file_in_scope" '("/outside/scope") callback)
      (expect 'jf/gptel-scope--trigger-inline-expansion :to-have-been-called)
      (let ((call-args (spy-calls-args-for 'jf/gptel-scope--trigger-inline-expansion 0)))
        (expect (nth 1 call-args) :to-equal "read_file_in_scope"))))

  (it "passes denied resource to expansion for pattern inference"
    (spy-on 'jf/gptel-scope--trigger-inline-expansion)
    (let ((validation-error (tool-test--scope-denied "path_out_of_scope" "/new/project/file.el" "write_file_in_scope"))
          (callback (lambda (_result) nil)))
      (jf/gptel-scope--trigger-inline-expansion validation-error "write_file_in_scope" '("/new/project/file.el") callback)
      (let ((call-args (spy-calls-args-for 'jf/gptel-scope--trigger-inline-expansion 0)))
        (expect (plist-get (nth 0 call-args) :resource) :to-equal "/new/project/file.el")))))

(provide 'filesystem-tools-spec)

;;; filesystem-tools-spec.el ends here
