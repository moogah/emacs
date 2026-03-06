;;; test-command-injection.el --- Tests for command injection detection -*- lexical-binding: t; -*-

;; ERT tests for bash-parser command injection pattern detection
;; Tests cover: bash -c, sh -c, env -S patterns with nested command parsing

(require 'test-helper (expand-file-name "test-helper.el"
                                        (file-name-directory load-file-name)))

;;; Test Suite

;; ============================================================
;; COMMAND INJECTION DETECTION
;; ============================================================

(ert-deftest test-jf/bash-parse-detects-bash-c-injection ()
  "Test detection of bash -c command injection pattern.
Scenario: bash -c 'echo hello'
Expected: Detects injection, parses nested command."
  (let* ((parsed (jf/bash-parse "bash -c 'echo hello'"))
         (command (car (plist-get parsed :all-commands))))

    ;; Should detect injection
    (should (plist-get command :command-injection))
    (should (equal (plist-get command :injection-type) "-c"))

    ;; Should have nested command
    (should (plist-get command :nested-command))
    (let* ((nested (plist-get command :nested-command))
           (nested-cmd (car (plist-get nested :all-commands))))
      (should (equal (plist-get nested-cmd :command-name) "echo"))
      (should (equal (plist-get nested-cmd :positional-args) '("hello"))))))

(ert-deftest test-jf/bash-parse-detects-sh-c-injection ()
  "Test detection of sh -c command injection pattern.
Scenario: sh -c 'cat file.txt'
Expected: Detects injection, parses nested command."
  (let* ((parsed (jf/bash-parse "sh -c 'cat file.txt'"))
         (command (car (plist-get parsed :all-commands))))

    ;; Should detect injection
    (should (plist-get command :command-injection))
    (should (equal (plist-get command :injection-type) "-c"))

    ;; Should have nested command
    (should (plist-get command :nested-command))
    (let* ((nested (plist-get command :nested-command))
           (nested-cmd (car (plist-get nested :all-commands))))
      (should (equal (plist-get nested-cmd :command-name) "cat"))
      (should (equal (plist-get nested-cmd :positional-args) '("file.txt"))))))

(ert-deftest test-jf/bash-parse-detects-env-S-injection ()
  "Test that env -S is handled as wrapper command (not injection).
Scenario: env -S 'bash -c echo test'
Expected: Parsed as wrapper command with bash command as positional arg.
Note: env is a wrapper command, so -S is handled by wrapper logic, not injection logic."
  (let* ((parsed (jf/bash-parse "env -S 'bash -c echo test'"))
         (command (car (plist-get parsed :all-commands))))

    ;; env should be parsed as wrapper command, not injection
    (should (equal (plist-get command :command-name) "env"))
    (should (member "-S" (plist-get command :flags)))
    (should (member "bash -c echo test" (plist-get command :positional-args)))))

(ert-deftest test-jf/bash-parse-strips-quotes-from-injection ()
  "Test that outer quotes are stripped from injection argument.
Scenario: bash -c \"echo hello\"
Expected: Parses nested command correctly without quotes."
  (let* ((parsed (jf/bash-parse "bash -c \"echo hello\""))
         (command (car (plist-get parsed :all-commands))))

    ;; Should detect injection
    (should (plist-get command :command-injection))

    ;; Should have nested command without outer quotes
    (should (plist-get command :nested-command))
    (let* ((nested (plist-get command :nested-command))
           (nested-cmd (car (plist-get nested :all-commands))))
      (should (equal (plist-get nested-cmd :command-name) "echo"))
      (should (equal (plist-get nested-cmd :positional-args) '("hello"))))))

(ert-deftest test-jf/bash-parse-handles-flags-before-injection ()
  "Test injection detection with flags before injection flag.
Scenario: bash -x -e -c 'echo test'
Expected: Detects injection, preserves other flags."
  (let* ((parsed (jf/bash-parse "bash -x -e -c 'echo test'"))
         (command (car (plist-get parsed :all-commands))))

    ;; Should detect injection
    (should (plist-get command :command-injection))
    (should (equal (plist-get command :injection-type) "-c"))

    ;; Should preserve other flags
    (should (member "-x" (plist-get command :flags)))
    (should (member "-e" (plist-get command :flags)))

    ;; Should have nested command
    (should (plist-get command :nested-command))
    (let* ((nested (plist-get command :nested-command))
           (nested-cmd (car (plist-get nested :all-commands))))
      (should (equal (plist-get nested-cmd :command-name) "echo"))
      (should (equal (plist-get nested-cmd :positional-args) '("test"))))))

(ert-deftest test-jf/bash-parse-nested-injection-with-pipeline ()
  "Test injection detection with pipeline in nested command.
Scenario: bash -c 'cat file.txt | grep pattern'
Expected: Parses nested pipeline correctly."
  (let* ((parsed (jf/bash-parse "bash -c 'cat file.txt | grep pattern'"))
         (command (car (plist-get parsed :all-commands))))

    ;; Should detect injection
    (should (plist-get command :command-injection))

    ;; Should have nested pipeline
    (should (plist-get command :nested-command))
    (let* ((nested (plist-get command :nested-command))
           (nested-cmds (plist-get nested :all-commands)))
      (should (= (length nested-cmds) 2))
      (should (equal (plist-get (car nested-cmds) :command-name) "cat"))
      (should (equal (plist-get (cadr nested-cmds) :command-name) "grep")))))

(ert-deftest test-jf/bash-parse-no-injection-without-flag ()
  "Test that commands without injection flags don't trigger detection.
Scenario: bash script.sh
Expected: No injection detected."
  (let* ((parsed (jf/bash-parse "bash script.sh"))
         (command (car (plist-get parsed :all-commands))))

    ;; Should NOT detect injection
    (should-not (plist-get command :command-injection))
    (should-not (plist-get command :nested-command))

    ;; Should parse as regular command
    (should (equal (plist-get command :command-name) "bash"))
    (should (equal (plist-get command :positional-args) '("script.sh")))))

(provide 'test-command-injection)
;;; test-command-injection.el ends here
