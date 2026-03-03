;;; llm-scenario-runner.el --- Test runner for LLM scenarios -*- lexical-binding: t; -*-

;; Interactive test runner for exploring how the bash parser handles
;; complex LLM-generated commands

(require 'bash-parser)
(require 'corpus-llm-scenarios)

(defun jf/bash-parser-test-llm-scenario (test-case)
  "Test a single LLM scenario TEST-CASE and return results.
TEST-CASE is a plist from jf/bash-parser-test-corpus-llm."
  (let* ((id (plist-get test-case :id))
         (command (plist-get test-case :command))
         (note (plist-get test-case :note))
         (expected (plist-get test-case :expect))
         (result (jf/bash-parse command)))
    (list :id id
          :command command
          :note note
          :expected expected
          :actual result
          :parse-success (plist-get result :success)
          :has-expectation (not (null expected)))))

(defun jf/bash-parser-test-all-llm-scenarios ()
  "Run all LLM scenario tests and return summary."
  (let ((results (mapcar #'jf/bash-parser-test-llm-scenario
                         jf/bash-parser-test-corpus-llm))
        (stats (list :total 0
                     :with-expectations 0
                     :without-expectations 0
                     :parse-success 0
                     :parse-failure 0)))

    (dolist (result results)
      (cl-incf (plist-get stats :total))

      (if (plist-get result :has-expectation)
          (cl-incf (plist-get stats :with-expectations))
        (cl-incf (plist-get stats :without-expectations)))

      (if (plist-get result :parse-success)
          (cl-incf (plist-get stats :parse-success))
        (cl-incf (plist-get stats :parse-failure))))

    (list :results results :stats stats)))

(defun jf/bash-parser-llm-scenarios-report ()
  "Generate a detailed report of LLM scenario test results."
  (interactive)
  (let* ((test-run (jf/bash-parser-test-all-llm-scenarios))
         (results (plist-get test-run :results))
         (stats (plist-get test-run :stats))
         (buffer (get-buffer-create "*LLM Scenario Test Results*")))

    (with-current-buffer buffer
      (erase-buffer)
      (insert "# LLM Bash Command Parsing - Test Results\n\n")

      ;; Summary statistics
      (insert "## Summary\n\n")
      (insert (format "Total test cases: %d\n" (plist-get stats :total)))
      (insert (format "- With expectations: %d\n" (plist-get stats :with-expectations)))
      (insert (format "- Exploratory (no expectations): %d\n" (plist-get stats :without-expectations)))
      (insert (format "- Parse success: %d\n" (plist-get stats :parse-success)))
      (insert (format "- Parse failure: %d\n\n" (plist-get stats :parse-failure)))

      ;; Categorize results
      (let ((exploratory-success nil)
            (exploratory-failure nil)
            (expected-success nil))

        (dolist (result results)
          (cond
           ;; Exploratory tests (no expectations)
           ((not (plist-get result :has-expectation))
            (if (plist-get result :parse-success)
                (push result exploratory-success)
              (push result exploratory-failure)))
           ;; Tests with expectations
           (t
            (push result expected-success))))

        ;; Report exploratory successes
        (insert "## Exploratory Tests - Parse Succeeded\n\n")
        (insert "These commands parsed successfully. Review actual output to verify correctness.\n\n")
        (dolist (result (reverse exploratory-success))
          (insert (format "### %s\n\n" (plist-get result :id)))
          (insert (format "**Command:** `%s`\n\n" (plist-get result :command)))
          (insert (format "**Note:** %s\n\n" (plist-get result :note)))
          (insert "**Parse Result:**\n```elisp\n")
          (insert (pp-to-string (plist-get result :actual)))
          (insert "```\n\n"))

        ;; Report exploratory failures
        (insert "## Exploratory Tests - Parse Failed\n\n")
        (insert "These commands failed to parse. May need special handling.\n\n")
        (dolist (result (reverse exploratory-failure))
          (insert (format "### %s\n\n" (plist-get result :id)))
          (insert (format "**Command:** `%s`\n\n" (plist-get result :command)))
          (insert (format "**Note:** %s\n\n" (plist-get result :note)))
          (insert "**Error:**\n```\n")
          (insert (format "%s\n" (plist-get (plist-get result :actual) :error)))
          (insert "```\n\n"))

        ;; Report expected tests
        (insert "## Tests With Expectations\n\n")
        (insert "These tests have defined expectations. Manual verification needed.\n\n")
        (dolist (result (reverse expected-success))
          (insert (format "### %s\n\n" (plist-get result :id)))
          (insert (format "**Command:** `%s`\n\n" (plist-get result :command)))
          (insert (format "**Note:** %s\n\n" (plist-get result :note)))
          (insert "**Expected:**\n```elisp\n")
          (insert (pp-to-string (plist-get result :expected)))
          (insert "```\n\n")
          (insert "**Actual:**\n```elisp\n")
          (insert (pp-to-string (plist-get result :actual)))
          (insert "```\n\n"))))

      (markdown-mode)
      (goto-char (point-min)))

    (pop-to-buffer buffer)))

(defun jf/bash-parser-test-single-llm-scenario (id)
  "Test a single LLM scenario by ID interactively."
  (interactive
   (list (completing-read "Test ID: "
                         (mapcar (lambda (tc) (plist-get tc :id))
                                jf/bash-parser-test-corpus-llm))))
  (let* ((test-case (cl-find-if (lambda (tc) (string= (plist-get tc :id) id))
                                jf/bash-parser-test-corpus-llm))
         (result (jf/bash-parser-test-llm-scenario test-case)))
    (with-current-buffer (get-buffer-create "*LLM Scenario Test*")
      (erase-buffer)
      (insert (format "# Test: %s\n\n" (plist-get result :id)))
      (insert (format "**Command:**\n```bash\n%s\n```\n\n" (plist-get result :command)))
      (insert (format "**Note:** %s\n\n" (plist-get result :note)))

      (when (plist-get result :expected)
        (insert "**Expected:**\n```elisp\n")
        (insert (pp-to-string (plist-get result :expected)))
        (insert "```\n\n"))

      (insert "**Actual Result:**\n```elisp\n")
      (insert (pp-to-string (plist-get result :actual)))
      (insert "```\n")

      (markdown-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun jf/bash-parser-llm-scenarios-by-category ()
  "Show test results grouped by category."
  (interactive)
  (let* ((test-run (jf/bash-parser-test-all-llm-scenarios))
         (results (plist-get test-run :results))
         (buffer (get-buffer-create "*LLM Scenarios By Category*")))

    (with-current-buffer buffer
      (erase-buffer)
      (insert "# LLM Bash Commands - Organized by Category\n\n")

      ;; Extract categories from test IDs (e.g., "llm-flags-001" -> "flags")
      (let ((categories (make-hash-table :test 'equal)))
        (dolist (result results)
          (let* ((id (plist-get result :id))
                 (category (when (string-match "llm-\\([^-]+\\)-" id)
                            (match-string 1 id))))
            (when category
              (push result (gethash category categories)))))

        ;; Print results by category
        (maphash
         (lambda (category tests)
           (insert (format "## %s (%d tests)\n\n"
                          (upcase category)
                          (length tests)))
           (dolist (result (reverse tests))
             (insert (format "- **%s**: %s\n"
                           (plist-get result :id)
                           (plist-get result :note)))
             (insert (format "  - Command: `%s`\n"
                           (plist-get result :command)))
             (insert (format "  - Parse: %s\n"
                           (if (plist-get result :parse-success)
                               "✓ Success"
                             "✗ Failed")))
             (when (not (plist-get result :parse-success))
               (insert (format "  - Error: %s\n"
                             (plist-get (plist-get result :actual) :error))))
             (insert "\n"))
           (insert "\n"))
         categories))

      (markdown-mode)
      (goto-char (point-min)))

    (pop-to-buffer buffer)))

(provide 'llm-scenario-runner)
;;; llm-scenario-runner.el ends here
