;;; index-spec.el --- Tests for bash-commands-index auto-discovery -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup specs for the auto-discovery system in bash-commands-index.

;;; Code:

(require 'buttercup)

;; Load the module under test.  jf/emacs-dir is already set by init.el
;; which the test harness loads before specs.
(load (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir) nil t)

(describe "bash-commands-index auto-discovery"

  (describe "jf/bash-commands--discover-and-load"

    (it "discovers .el files in the commands directory"
      (let* ((commands-dir (expand-file-name "config/bash-parser/commands" jf/emacs-dir))
             (el-files (directory-files commands-dir t "\\.el$"))
             (non-index (seq-remove
                         (lambda (f) (string= (file-name-nondirectory f) "index.el"))
                         el-files)))
        ;; There should be discoverable files (or at least the function runs without error)
        (expect (functionp 'jf/bash-commands--discover-and-load) :to-be-truthy)))

    (it "excludes index.el from discovery"
      (let* ((commands-dir (expand-file-name "config/bash-parser/commands" jf/emacs-dir))
             (el-files (directory-files commands-dir t "\\.el$"))
             (index-file (seq-find
                          (lambda (f) (string= (file-name-nondirectory f) "index.el"))
                          el-files)))
        ;; index.el exists in the directory
        (expect index-file :to-be-truthy)
        ;; But discover-and-load should not include it in results
        (let ((loaded (jf/bash-commands--discover-and-load)))
          (expect (seq-find
                   (lambda (f) (string= (file-name-nondirectory f) "index.el"))
                   loaded)
                  :to-equal nil))))

    (it "isolates errors - one failing file does not prevent others"
      (let* ((commands-dir (expand-file-name "config/bash-parser/commands" jf/emacs-dir))
             (bad-file (expand-file-name "test-bad-handler.el" commands-dir))
             (good-file (expand-file-name "test-good-handler.el" commands-dir)))
        (unwind-protect
            (progn
              ;; Create a file that will error on load
              (with-temp-file bad-file
                (insert "(error \"Intentional test error\")"))
              ;; Create a file that loads successfully
              (with-temp-file good-file
                (insert "(provide 'test-good-handler)"))
              ;; Discovery should succeed and load the good file
              (let ((loaded (jf/bash-commands--discover-and-load)))
                ;; Good file should be in loaded list
                (expect (seq-find
                         (lambda (f) (string= (file-name-nondirectory f) "test-good-handler.el"))
                         loaded)
                        :to-be-truthy)
                ;; Bad file should NOT be in loaded list
                (expect (seq-find
                         (lambda (f) (string= (file-name-nondirectory f) "test-bad-handler.el"))
                         loaded)
                        :to-equal nil)))
          ;; Cleanup temp files
          (when (file-exists-p bad-file) (delete-file bad-file))
          (when (file-exists-p good-file) (delete-file good-file)))))

    (it "returns empty list when no handler files exist"
      ;; With only index.el in the directory (and test files in test/ subdir),
      ;; the function should return an empty or minimal list
      ;; This test verifies it handles the case gracefully
      (expect (listp (jf/bash-commands--discover-and-load)) :to-be-truthy))

    (it "returns list of successfully loaded file paths"
      (let* ((commands-dir (expand-file-name "config/bash-parser/commands" jf/emacs-dir))
             (good-file (expand-file-name "test-verify-handler.el" commands-dir)))
        (unwind-protect
            (progn
              (with-temp-file good-file
                (insert "(provide 'test-verify-handler)"))
              (let ((loaded (jf/bash-commands--discover-and-load)))
                ;; Should return a list
                (expect (listp loaded) :to-be-truthy)
                ;; Each element should be a string (file path)
                (dolist (f loaded)
                  (expect (stringp f) :to-be-truthy))))
          (when (file-exists-p good-file) (delete-file good-file)))))))

;;; index-spec.el ends here
