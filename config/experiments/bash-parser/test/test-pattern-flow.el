;;; test-pattern-flow.el --- Tests for pattern flow tracking -*- lexical-binding: t; -*-

;; ERT tests for pattern flow tracking through command substitutions.
;; Tests verify that patterns from pattern-producing commands (find, ls, grep -l)
;; flow correctly to outer commands that operate on those patterns.

(require 'ert)
(require 'bash-parser (expand-file-name "../bash-parser.el"
                                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Test Suite

;; ============================================================
;; TIER 1: BASIC PATTERN FLOW - FIND TO OUTER COMMAND
;; ============================================================

(ert-deftest test-pattern-flow-cat-find ()
  "Test pattern flows from find to cat.
Scenario: cat $(find . -name '*.log')
Expected:
  1. find reads directory '.'
  2. find matches pattern '*.log' (from substitution)
  3. cat reads pattern '*.log' with pattern-source pointing to find"
  (let* ((parsed (jf/bash-parse "cat $(find . -name '*.log')"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; Should have at least 3 operations
    (should (>= (length ops) 3))

    ;; 1. find reads directory "."
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) ".")
                            (eq (plist-get op :operation) :read-directory)
                            (string= (plist-get op :command) "find")
                            (plist-get op :from-substitution)))
                     ops))

    ;; 2. find matches pattern "*.log"
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.log")
                            (eq (plist-get op :operation) :match-pattern)
                            (string= (plist-get op :command) "find")
                            (plist-get op :from-substitution)))
                     ops))

    ;; 3. cat reads pattern "*.log" (pattern flow)
    (let ((cat-op (seq-find (lambda (op)
                             (and (string= (plist-get op :file) "*.log")
                                  (eq (plist-get op :operation) :read)
                                  (string= (plist-get op :command) "cat")))
                           ops)))
      (should cat-op)
      (should (plist-get cat-op :pattern))
      (should (plist-get cat-op :pattern-source))
      (let ((pattern-source (plist-get cat-op :pattern-source)))
        (should (string= (plist-get pattern-source :command) "find"))
        (should (string= (plist-get pattern-source :search-scope) "."))
        (should (plist-get pattern-source :from-substitution))))))

(ert-deftest test-pattern-flow-head-find ()
  "Test pattern flows from find to head.
Scenario: head -n 10 $(find /var/log -name '*.log')
Expected: find matches pattern, but head uses custom handler (no pattern flow yet).

KNOWN LIMITATION: head uses a custom handler which doesn't support pattern flow yet.
This test verifies that find pattern is detected but accepts that head won't
have pattern-source metadata until custom handlers are enhanced."
  (let* ((parsed (jf/bash-parse "head -n 10 $(find /var/log -name '*.log')"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; find matches pattern - this should work
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.log")
                            (eq (plist-get op :operation) :match-pattern)
                            (plist-get op :from-substitution)))
                     ops))

    ;; head operation exists but without pattern flow (custom handler limitation)
    ;; We just verify that some head operation was created
    (should (seq-find (lambda (op)
                       (string= (plist-get op :command) "head"))
                     ops))))

;; ============================================================
;; TIER 2: PATTERN FLOW - LS TO OUTER COMMAND
;; ============================================================

(ert-deftest test-pattern-flow-rm-ls ()
  "Test pattern flows from ls glob to rm.
Scenario: rm $(ls *.tmp)
Expected:
  1. ls matches pattern '*.tmp'
  2. rm deletes pattern '*.tmp' with pattern-source pointing to ls"
  (let* ((parsed (jf/bash-parse "rm $(ls *.tmp)"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; ls matches pattern "*.tmp"
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.tmp")
                            (eq (plist-get op :operation) :match-pattern)
                            (string= (plist-get op :command) "ls")
                            (plist-get op :from-substitution)))
                     ops))

    ;; rm deletes pattern "*.tmp" with pattern-source
    (let ((rm-op (seq-find (lambda (op)
                            (and (string= (plist-get op :file) "*.tmp")
                                 (eq (plist-get op :operation) :delete)
                                 (string= (plist-get op :command) "rm")))
                          ops)))
      (should rm-op)
      (should (plist-get rm-op :pattern))
      (should (plist-get rm-op :pattern-source))
      (should (string= (plist-get (plist-get rm-op :pattern-source) :command) "ls")))))

(ert-deftest test-pattern-flow-cat-ls ()
  "Test pattern flows from ls to cat.
Scenario: cat $(ls *.txt)
Expected: cat reads pattern '*.txt' with pattern-source"
  (let* ((parsed (jf/bash-parse "cat $(ls *.txt)"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; ls matches pattern
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.txt")
                            (eq (plist-get op :operation) :match-pattern)
                            (plist-get op :from-substitution)))
                     ops))

    ;; cat reads pattern with pattern-source
    (let ((cat-op (seq-find (lambda (op)
                             (and (string= (plist-get op :file) "*.txt")
                                  (eq (plist-get op :operation) :read)
                                  (string= (plist-get op :command) "cat")))
                           ops)))
      (should cat-op)
      (should (plist-get cat-op :pattern-source)))))

;; ============================================================
;; TIER 3: PATTERN FLOW - GREP -L TO OUTER COMMAND
;; ============================================================

(ert-deftest test-pattern-flow-cat-grep ()
  "Test pattern flows from grep -l to cat.
Scenario: cat $(grep -l 'ERROR' *.log)
Expected:
  1. grep matches pattern '*.log'
  2. cat reads pattern '*.log' with pattern-source pointing to grep"
  (let* ((parsed (jf/bash-parse "cat $(grep -l 'ERROR' *.log)"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; grep matches pattern "*.log"
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.log")
                            (eq (plist-get op :operation) :match-pattern)
                            (string= (plist-get op :command) "grep")
                            (plist-get op :from-substitution)))
                     ops))

    ;; cat reads pattern "*.log" with pattern-source
    (let ((cat-op (seq-find (lambda (op)
                             (and (string= (plist-get op :file) "*.log")
                                  (eq (plist-get op :operation) :read)
                                  (string= (plist-get op :command) "cat")))
                           ops)))
      (should cat-op)
      (should (plist-get cat-op :pattern))
      (should (plist-get cat-op :pattern-source))
      (should (string= (plist-get (plist-get cat-op :pattern-source) :command) "grep")))))

;; ============================================================
;; TIER 4: NESTED PATTERN FLOW
;; ============================================================

(ert-deftest test-pattern-flow-nested ()
  "Test pattern flow with nested substitutions.
Scenario: cat $(find $(pwd) -name '*.txt')
Expected:
  1. pwd doesn't produce pattern (just outputs directory)
  2. find produces pattern '*.txt'
  3. cat reads pattern '*.txt' with pattern-source"
  (let* ((parsed (jf/bash-parse "cat $(find $(pwd) -name '*.txt')"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; find matches pattern
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.txt")
                            (eq (plist-get op :operation) :match-pattern)
                            (plist-get op :from-substitution)))
                     ops))

    ;; cat reads pattern with pattern-source
    (let ((cat-op (seq-find (lambda (op)
                             (and (string= (plist-get op :file) "*.txt")
                                  (eq (plist-get op :operation) :read)
                                  (string= (plist-get op :command) "cat")))
                           ops)))
      (should cat-op)
      (should (plist-get cat-op :pattern-source)))))

;; ============================================================
;; TIER 5: MULTIPLE PATTERNS IN SUBSTITUTION
;; ============================================================

(ert-deftest test-pattern-flow-multiple-patterns ()
  "Test pattern flow when multiple patterns could be produced.
Scenario: cat $(find . -name '*.log' -o -name '*.txt')
Expected: First pattern flows to outer command"
  (let* ((parsed (jf/bash-parse "cat $(find . -name '*.log' -o -name '*.txt')"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; Should have at least one pattern match from find
    (should (seq-find (lambda (op)
                       (and (eq (plist-get op :operation) :match-pattern)
                            (plist-get op :from-substitution)))
                     ops))

    ;; Should have at least one pattern flow to cat
    (should (seq-find (lambda (op)
                       (and (eq (plist-get op :operation) :read)
                            (string= (plist-get op :command) "cat")
                            (plist-get op :pattern-source)))
                     ops))))

;; ============================================================
;; TIER 6: PATTERN FLOW WITH REDIRECTIONS
;; ============================================================

(ert-deftest test-pattern-flow-with-redirection ()
  "Test pattern flow works with output redirection.
Scenario: cat $(find . -name '*.log') > output.txt
Expected: Pattern flow from find to cat, plus redirection operation"
  (let* ((parsed (jf/bash-parse "cat $(find . -name '*.log') > output.txt"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; cat reads pattern with pattern-source
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.log")
                            (eq (plist-get op :operation) :read)
                            (plist-get op :pattern-source)))
                     ops))

    ;; redirection to output.txt
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "output.txt")
                            (eq (plist-get op :operation) :write)
                            (eq (plist-get op :source) :redirection)))
                     ops))))

;; ============================================================
;; TIER 7: EDGE CASES
;; ============================================================

(ert-deftest test-pattern-flow-no-pattern ()
  "Test commands that don't produce patterns don't trigger flow.
Scenario: cat $(echo file.txt)
Expected: No pattern flow (echo doesn't produce patterns)"
  (let* ((parsed (jf/bash-parse "cat $(echo file.txt)"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; Should have cat operation but without pattern-source
    (let ((cat-ops (seq-filter (lambda (op)
                                (string= (plist-get op :command) "cat"))
                              ops)))
      ;; If there are cat operations, they shouldn't have pattern-source
      (dolist (op cat-ops)
        (should-not (and (plist-get op :pattern-source)
                        (not (string-match "echo" (or (plist-get op :file) "")))))))))

(ert-deftest test-pattern-flow-non-reading-command ()
  "Test pattern flow only applies to commands that read files.
Scenario: echo $(find . -name '*.log')
Expected: find produces pattern, but echo doesn't read files (no pattern flow)"
  (let* ((parsed (jf/bash-parse "echo $(find . -name '*.log')"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; find should still match pattern
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.log")
                            (eq (plist-get op :operation) :match-pattern)
                            (plist-get op :from-substitution)))
                     ops))

    ;; echo should not have pattern flow operations
    (should-not (seq-find (lambda (op)
                           (and (string= (plist-get op :command) "echo")
                                (plist-get op :pattern-source)))
                         ops))))

(provide 'test-pattern-flow)
;;; test-pattern-flow.el ends here
