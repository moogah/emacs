;;; explore-corpus.el --- Explore parser behavior on complex commands -*- lexical-binding: t; -*-

;; Script to test parser behavior on exploratory test cases

(require 'bash-parser (expand-file-name "bash-parser.el"
                                        (file-name-directory load-file-name)))
(require 'test-corpus (expand-file-name "test-corpus.el"
                                        (file-name-directory load-file-name)))

(defun jf/bash-parser-explore-corpus ()
  "Run all corpus tests and report results, including exploratory cases."
  (interactive)
  (let ((results '())
        (success-count 0)
        (fail-count 0)
        (exploratory-count 0))

    (dolist (test-case jf/bash-parser-test-corpus)
      (let* ((test-id (plist-get test-case :id))
             (command (plist-get test-case :command))
             (expected (plist-get test-case :expect))
             (note (plist-get test-case :note))
             (result (jf/bash-parse command))
             (is-exploratory (null expected)))

        (when is-exploratory
          (setq exploratory-count (1+ exploratory-count)))

        (push (list :id test-id
                    :command command
                    :note note
                    :expected expected
                    :result result
                    :exploratory is-exploratory)
              results)))

    (setq results (nreverse results))

    ;; Display results
    (with-current-buffer (get-buffer-create "*bash-parser-exploration*")
      (erase-buffer)
      (insert "# Bash Parser Corpus Exploration\n\n")
      (insert (format "Total tests: %d\n" (length results)))
      (insert (format "Exploratory tests: %d\n\n" exploratory-count))
      (insert "="(make-string 70 ?=) "\n\n")

      (dolist (test results)
        (let* ((test-id (plist-get test :id))
               (command (plist-get test :command))
               (note (plist-get test :note))
               (result (plist-get test :result))
               (is-exploratory (plist-get test :exploratory))
               (success (plist-get result :success)))

          (insert (format "## %s%s\n"
                         test-id
                         (if is-exploratory " [EXPLORATORY]" "")))
          (insert (format "Command: %s\n" command))
          (when note
            (insert (format "Note: %s\n" note)))
          (insert "\n")

          (if success
              (progn
                (insert "✓ Parse succeeded\n\n")
                (insert (format "  Command name:    %s\n"
                               (plist-get result :command-name)))
                (insert (format "  Subcommand:      %s\n"
                               (or (plist-get result :subcommand) "nil")))
                (insert (format "  Flags:           %s\n"
                               (prin1-to-string (plist-get result :flags))))
                (insert (format "  Positional args: %s\n"
                               (prin1-to-string (plist-get result :positional-args))))
                (insert (format "  Dangerous:       %s\n"
                               (if (plist-get result :dangerous-p) "YES" "no"))))
            (insert (format "✗ Parse failed: %s\n"
                           (plist-get result :error))))

          (insert "\n" (make-string 70 ?-) "\n\n")))

      (goto-char (point-min))
      (org-mode)
      (display-buffer (current-buffer)))

    (message "Exploration complete: %d tests run, %d exploratory"
             (length results) exploratory-count)))

(provide 'explore-corpus)
;;; explore-corpus.el ends here
