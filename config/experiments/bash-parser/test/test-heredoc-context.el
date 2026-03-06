;;; test-heredoc-context.el --- Heredoc context disambiguation tests -*- lexical-binding: t; -*-

;; Test heredoc context detection to distinguish:
;; - File creation (heredoc with redirect)
;; - Command input (heredoc to stdin)
;; - Commit messages (heredoc in substitution)
;; - Pipe input (heredoc piped to command)

(require 'test-helper (expand-file-name "test-helper.el"
                                        (file-name-directory load-file-name)))

(ert-deftest test-heredoc-file-creation ()
  "Test heredoc with redirect creates file."
  (let* ((parsed (jf/bash-parse "cat <<EOF > config.yml\nkey: value\nEOF"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have write operation for config.yml marked with heredoc-content
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "config.yml")
                            (eq (plist-get op :operation) :write)
                            (plist-get op :heredoc-content)))
                     ops))))

(ert-deftest test-heredoc-git-commit ()
  "Test heredoc in git commit message is not file operation."
  (let* ((parsed (jf/bash-parse "git commit -m \"$(cat <<'EOF'\nUpdate feature\nEOF\n)\""))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have NO file operations from heredoc
    ;; Heredoc is just commit message content
    (should-not (seq-find (lambda (op)
                           (plist-get op :heredoc-content))
                         ops))))

(ert-deftest test-heredoc-embedded-substitution ()
  "Test command substitution inside heredoc."
  (let* ((parsed (jf/bash-parse "cat <<EOF > log.txt\nCurrent date: $(date)\nUser: $(whoami)\nEOF"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have write operation for log.txt
    (should (seq-find (lambda (op)
                       (string= (plist-get op :file) "log.txt"))
                     ops))
    ;; date and whoami don't produce file operations
    ;; but they are embedded substitutions
    ))

(ert-deftest test-heredoc-to-command ()
  "Test heredoc providing input to command (not creating file)."
  (let* ((parsed (jf/bash-parse "python3 <<EOF\nprint('hello')\nEOF"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have NO file operations
    ;; Heredoc is input to python, not file creation
    (should (= 0 (length ops)))))

(ert-deftest test-heredoc-piped ()
  "Test heredoc piped to command."
  (let* ((parsed (jf/bash-parse "cat <<EOF | grep pattern\nline1\npattern\nEOF"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have NO file operations
    ;; Heredoc is piped to grep (stdin, not file)
    (should (= 0 (length ops)))))

(ert-deftest test-heredoc-multi-line-with-redirect ()
  "Test multi-line heredoc with file redirect."
  (let* ((parsed (jf/bash-parse "cat <<EOF > output.txt\nLine 1\nLine 2\nLine 3\nEOF"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have write operation for output.txt marked with heredoc-content
    (should (>= (length ops) 1))
    (let ((op (seq-find (lambda (o) (string= (plist-get o :file) "output.txt")) ops)))
      (should op)
      (should (eq (plist-get op :operation) :write))
      (should (plist-get op :heredoc-content)))))

(ert-deftest test-heredoc-mysql-input ()
  "Test heredoc feeding SQL to mysql command."
  (let* ((parsed (jf/bash-parse "mysql -u root <<SQL\nSELECT * FROM users;\nSQL"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have NO file operations
    ;; Heredoc is SQL input to mysql, not file creation
    (should (= 0 (length ops)))))

(ert-deftest test-heredoc-append-redirect ()
  "Test heredoc with append redirect."
  (let* ((parsed (jf/bash-parse "cat <<EOF >> log.txt\nNew entry\nEOF"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have append operation for log.txt marked with heredoc-content
    (should (>= (length ops) 1))
    (let ((op (seq-find (lambda (o) (string= (plist-get o :file) "log.txt")) ops)))
      (should op)
      (should (eq (plist-get op :operation) :append))
      (should (plist-get op :heredoc-content)))))

(ert-deftest test-heredoc-stderr-redirect ()
  "Test heredoc with stderr redirect."
  (let* ((parsed (jf/bash-parse "cat <<EOF > output.txt 2>&1\nContent\nEOF"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have write operation for output.txt
    ;; Note: 2>&1 is descriptor redirection, not file operation
    (should (= 1 (length ops)))
    (should (string= (plist-get (car ops) :file) "output.txt"))
    (should (eq (plist-get (car ops) :operation) :write))))

(ert-deftest test-heredoc-git-commit-multi-line ()
  "Test multi-line git commit message with heredoc."
  (let* ((parsed (jf/bash-parse "git commit -m \"$(cat <<'EOF'\nImplement feature X\n\nDetailed description here.\n\nCo-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>\nEOF\n)\""))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have NO file operations from heredoc
    ;; Heredoc is commit message content in command substitution
    (should (= 0 (length ops)))))

(provide 'test-heredoc-context)
;;; test-heredoc-context.el ends here
