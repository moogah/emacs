;;; index-spec.el --- Tests for bash-commands-index auto-discovery -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup specs for the auto-discovery system in bash-commands-index.
;; All tests use temp directories so results don't depend on repo state.

;;; Code:

(require 'buttercup)

;; Load the module under test.  jf/emacs-dir is already set by init.el
;; which the test harness loads before specs.
(load (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir) nil t)

(describe "bash-commands-index auto-discovery"

  (describe "jf/bash-commands--discover-and-load"

    ;; Each test creates a temp directory tree matching the expected
    ;; layout: <root>/config/bash-parser/commands/*.el, then let-binds
    ;; jf/emacs-dir to <root> so discover-and-load finds the temp files.

    (it "discovers .el files in the commands directory"
      (let* ((temp-root (make-temp-file "bash-idx-discover-" t))
             (commands-dir (expand-file-name "config/bash-parser/commands" temp-root)))
        (unwind-protect
            (progn
              (make-directory commands-dir t)
              (with-temp-file (expand-file-name "alpha.el" commands-dir)
                (insert "(provide 'alpha)"))
              (with-temp-file (expand-file-name "beta.el" commands-dir)
                (insert "(provide 'beta)"))
              (with-temp-file (expand-file-name "index.el" commands-dir)
                (insert "(provide 'fake-index)"))
              (let* ((jf/emacs-dir temp-root)
                     (loaded (jf/bash-commands--discover-and-load))
                     (names (mapcar #'file-name-nondirectory loaded)))
                ;; Should discover the two handler files
                (expect (length loaded) :to-equal 2)
                (expect (member "alpha.el" names) :to-be-truthy)
                (expect (member "beta.el" names) :to-be-truthy)
                ;; index.el must be excluded
                (expect (member "index.el" names) :not :to-be-truthy)))
          (delete-directory temp-root t))))

    (it "excludes index.el from discovery"
      (let* ((temp-root (make-temp-file "bash-idx-exclude-" t))
             (commands-dir (expand-file-name "config/bash-parser/commands" temp-root)))
        (unwind-protect
            (progn
              (make-directory commands-dir t)
              (with-temp-file (expand-file-name "index.el" commands-dir)
                (insert "(provide 'fake-index)"))
              (with-temp-file (expand-file-name "handler.el" commands-dir)
                (insert "(provide 'handler)"))
              (let* ((jf/emacs-dir temp-root)
                     (loaded (jf/bash-commands--discover-and-load)))
                (expect (seq-find
                         (lambda (f) (string= (file-name-nondirectory f) "index.el"))
                         loaded)
                        :to-equal nil)
                (expect (seq-find
                         (lambda (f) (string= (file-name-nondirectory f) "handler.el"))
                         loaded)
                        :to-be-truthy)))
          (delete-directory temp-root t))))

    (it "isolates errors - one failing file does not prevent others"
      (let* ((temp-root (make-temp-file "bash-idx-isolate-" t))
             (commands-dir (expand-file-name "config/bash-parser/commands" temp-root)))
        (unwind-protect
            (progn
              (make-directory commands-dir t)
              (with-temp-file (expand-file-name "bad-handler.el" commands-dir)
                (insert "(error \"Intentional test error\")"))
              (with-temp-file (expand-file-name "good-handler.el" commands-dir)
                (insert "(provide 'good-handler)"))
              (let* ((jf/emacs-dir temp-root)
                     (loaded (jf/bash-commands--discover-and-load))
                     (names (mapcar #'file-name-nondirectory loaded)))
                ;; Good file should be loaded
                (expect (member "good-handler.el" names) :to-be-truthy)
                ;; Bad file should NOT be in loaded list
                (expect (member "bad-handler.el" names) :not :to-be-truthy)))
          (delete-directory temp-root t))))

    (it "returns empty list when no handler files exist"
      (let* ((temp-root (make-temp-file "bash-idx-empty-" t))
             (commands-dir (expand-file-name "config/bash-parser/commands" temp-root)))
        (unwind-protect
            (progn
              (make-directory commands-dir t)
              ;; Only index.el — should be skipped
              (with-temp-file (expand-file-name "index.el" commands-dir)
                (insert "(provide 'fake-index)"))
              (let* ((jf/emacs-dir temp-root)
                     (loaded (jf/bash-commands--discover-and-load)))
                (expect loaded :to-equal nil)))
          (delete-directory temp-root t))))

    (it "returns list of successfully loaded file paths"
      (let* ((temp-root (make-temp-file "bash-idx-paths-" t))
             (commands-dir (expand-file-name "config/bash-parser/commands" temp-root)))
        (unwind-protect
            (progn
              (make-directory commands-dir t)
              (with-temp-file (expand-file-name "one.el" commands-dir)
                (insert "(provide 'one)"))
              (with-temp-file (expand-file-name "two.el" commands-dir)
                (insert "(provide 'two)"))
              (let* ((jf/emacs-dir temp-root)
                     (loaded (jf/bash-commands--discover-and-load)))
                ;; Should return a list
                (expect (listp loaded) :to-be-truthy)
                ;; Each element should be a full file path string
                (dolist (f loaded)
                  (expect (stringp f) :to-be-truthy)
                  (expect (file-name-absolute-p f) :to-be-truthy))
                ;; Should contain exactly our two files
                (expect (length loaded) :to-equal 2)))
          (delete-directory temp-root t))))))

;;; index-spec.el ends here
