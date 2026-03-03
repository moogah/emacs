(defun jf/test-find-test-files ()
  "Find all test files in config directory.
Returns list of absolute paths to test-*.el or *-test.el files."
  (let* ((config-dir (expand-file-name "config" jf/emacs-dir))
         (test-files '()))
    (dolist (file (directory-files-recursively config-dir "\\.el$"))
      (when (or (string-match-p "-test\\.el$" file)
                (string-match-p "/test-[^/]+\\.el$" file))
        (push file test-files)))
    (nreverse test-files)))

(defun jf/test-load-all-test-files ()
  "Load all test files in config directory.
Fails immediately if any file has syntax errors."
  (interactive)
  (let ((test-files (jf/test-find-test-files))
        (loaded-count 0))
    (dolist (file test-files)
      (message "Loading test file: %s" (file-relative-name file jf/emacs-dir))
      (load-file file)
      (setq loaded-count (1+ loaded-count)))
    (message "Loaded %d test files" loaded-count)
    loaded-count))

(defun jf/test-load-module-under-test (test-file)
  "Load the module corresponding to TEST-FILE.
Supports both test-module.el and module-test.el naming conventions.
Looks for the module in parent directory for test/ subdirectories."
  (cond
   ;; Handle module-test.el pattern
   ((string-match "\\(.+\\)-test\\.el$" test-file)
    (let ((module-file (concat (match-string 1 test-file) ".el")))
      (when (file-exists-p module-file)
        (message "Loading module: %s" (file-relative-name module-file jf/emacs-dir))
        (load-file module-file))))
   ;; Handle test-module.el pattern in test/ subdirectory
   ((string-match "/test/test-\\(.+\\)\\.el$" test-file)
    (let* ((module-name (match-string 1 test-file))
           (test-dir (file-name-directory test-file))
           (parent-dir (file-name-directory (directory-file-name test-dir)))
           (module-file (expand-file-name (concat module-name ".el") parent-dir)))
      (when (file-exists-p module-file)
        (message "Loading module: %s" (file-relative-name module-file jf/emacs-dir))
        (load-file module-file))))))

(defun jf/test-run-all ()
  "Run all tests in the project.
Loads all test files and runs all tests interactively."
  (interactive)
  (jf/test-load-all-test-files)
  (ert t))

(defun jf/test-run-current-file ()
  "Run all tests in the current file.
Works with both test files and module files.
Supports test-*.el and *-test.el naming conventions."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (is-test-file (or (string-match-p "-test\\.el$" current-file)
                           (string-match-p "/test-[^/]+\\.el$" current-file)))
         (test-file (if is-test-file
                        current-file
                      (concat (file-name-sans-extension current-file) "-test.el"))))
    (unless (file-exists-p test-file)
      (user-error "No test file found: %s" test-file))

    ;; Save and load current file
    (when (buffer-modified-p) (save-buffer))

    ;; Load module under test if we're in a test file
    (when is-test-file
      (jf/test-load-module-under-test current-file))

    ;; Load test file
    (load-file test-file)

    ;; Run tests matching the file's prefix
    ;; For test-glob-matching.el -> pattern is "^test-glob-"
    ;; For module-test.el -> pattern is "^test-module-"
    (let* ((test-file-base (file-name-base test-file))
           (prefix (cond
                    ((string-match "^test-\\(.+\\)$" test-file-base)
                     (match-string 1 test-file-base))
                    ((string-match "^\\(.+\\)-test$" test-file-base)
                     (match-string 1 test-file-base))
                    (t test-file-base)))
           (pattern (format "^test-%s-" prefix)))
      (message "Running tests matching: %s" pattern)
      (ert pattern))))

(defun jf/test-run-at-point ()
  "Run the test at point.
Uses which-function to identify the test name."
  (interactive)
  (let ((test-name (which-function)))
    (if test-name
        (progn
          (message "Running test: %s" test-name)
          (ert test-name))
      (user-error "No test found at point"))))

(defun jf/test-rerun-failed ()
  "Re-run only tests that failed in the last run."
  (interactive)
  (ert :failed))

(defun jf/test-run-pattern (pattern)
  "Run tests matching PATTERN.
PATTERN is a regexp matched against test names."
  (interactive "sTest pattern (regexp): ")
  (jf/test-load-all-test-files)
  (ert pattern))

(defun jf/test-run-module (module-name)
  "Run all tests for MODULE-NAME.
MODULE-NAME should match the test prefix (e.g., 'glob' for 'test-glob-*')."
  (interactive
   (list (completing-read "Module: "
                          '("glob" "extraction" "variable" "security" "command")
                          nil nil)))
  (jf/test-load-all-test-files)
  (let ((pattern (format "^test-%s-" module-name)))
    (message "Running tests matching: %s" pattern)
    (ert pattern)))

(defun jf/test-run-all-batch ()
  "Run all tests in batch mode (non-interactive).
Exits with code 0 if all tests pass, 1 otherwise.
For use in scripts and CI."
  (let ((test-files (jf/test-find-test-files)))
    (dolist (file test-files)
      (message "Loading: %s" file)
      (load-file file))
    (ert-run-tests-batch-and-exit)))

(defun jf/test-run-pattern-batch (pattern)
  "Run tests matching PATTERN in batch mode.
PATTERN is a regexp matched against test names."
  (jf/test-load-all-test-files)
  (ert-run-tests-batch-and-exit pattern))

(with-eval-after-load 'transient
  (defun jf/test--bash-parser-file-operations ()
  "Run bash parser file operations tests."
  (interactive)
  (jf/test-load-all-test-files)
  (ert "^test-extraction-"))

(defun jf/test--bash-parser-glob ()
  "Run bash parser glob matching tests."
  (interactive)
  (jf/test-load-all-test-files)
  (ert "^test-glob-"))

(defun jf/test--bash-parser-security ()
  "Run bash parser security tests."
  (interactive)
  (jf/test-load-all-test-files)
  (ert "^test-security-"))

(defun jf/test--bash-parser-variable ()
  "Run bash parser variable resolution tests."
  (interactive)
  (jf/test-load-all-test-files)
  (ert "^test-variable-"))

(defun jf/test--bash-parser-command ()
  "Run bash parser command semantics tests."
  (interactive)
  (jf/test-load-all-test-files)
  (ert "^test-command-"))

(transient-define-prefix jf/test-menu ()
  "Test runner menu for Emacs configuration."
  ["Run Tests"
   [("a" "All tests" jf/test-run-all)
    ("f" "Current file" jf/test-run-current-file)
    ("t" "Test at point" jf/test-run-at-point)]
   [("r" "Re-run failed" jf/test-rerun-failed)
    ("p" "Pattern..." jf/test-run-pattern)
    ("m" "Module..." jf/test-run-module)]]
  ["Bash Parser Tests"
   [("bx" "File operations" jf/test--bash-parser-file-operations)
    ("bg" "Glob matching" jf/test--bash-parser-glob)]
   [("bs" "Security" jf/test--bash-parser-security)
    ("bv" "Variable" jf/test--bash-parser-variable)
    ("bc" "Command" jf/test--bash-parser-command)]]
  ["Navigation"
   [("j" "Jump to *ert* buffer"
     (lambda () (interactive)
       (if (get-buffer "*ert*")
           (switch-to-buffer "*ert*")
         (message "No ERT results buffer"))))
    ("h" "ERT help"
     (lambda () (interactive)
       (describe-function 'ert)))]])

  ;; Keybinding for test menu
  (global-set-key (kbd "C-c t") 'jf/test-menu))

(provide 'jf-testing)
;;; testing.el ends here
