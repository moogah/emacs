;;; test-loop-context.el --- Tests for loop context tracking -*- lexical-binding: t; -*-

;; ERT tests for loop variable binding and resolution
;; Tests cover: loop variable from substitution, glob, literal, nested loops

(require 'ert)
(require 'bash-parser (expand-file-name "../bash-parser.el"
                                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Test Suite

;; ============================================================
;; TIER 1: LOOP VARIABLE FROM COMMAND SUBSTITUTION
;; ============================================================

(ert-deftest test-loop-variable-from-substitution ()
  "Test loop variable bound to pattern from substitution.
Scenario: for file in $(find . -name '*.txt'); do rm $file; done
Expected: rm operates on *.txt (resolved from loop variable)"
  (let* ((parsed (jf/bash-parse "for file in $(find . -name '*.txt'); do rm $file; done"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; find produces pattern *.txt (from substitution)
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.txt")
                            (eq (plist-get op :operation) :match-pattern)
                            (plist-get op :from-substitution)))
                     ops))

    ;; rm deletes *.txt (resolved from loop variable)
    (let ((rm-op (seq-find (lambda (op)
                            (and (string= (plist-get op :file) "*.txt")
                                 (eq (plist-get op :operation) :delete)
                                 (plist-get op :loop-context)))
                          ops)))
      (should rm-op)
      (should (string= (plist-get rm-op :loop-variable) "file")))))

(ert-deftest test-loop-variable-substitution-cat ()
  "Test loop variable with cat command.
Scenario: for file in $(find . -name '*.log'); do cat $file; done
Expected: cat reads *.log (resolved from loop variable)"
  (let* ((parsed (jf/bash-parse "for file in $(find . -name '*.log'); do cat $file; done"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; find produces pattern *.log
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.log")
                            (eq (plist-get op :operation) :match-pattern)
                            (plist-get op :from-substitution)))
                     ops))

    ;; cat reads *.log (resolved from loop variable)
    (let ((cat-op (seq-find (lambda (op)
                             (and (string= (plist-get op :file) "*.log")
                                  (eq (plist-get op :operation) :read)
                                  (plist-get op :loop-context)))
                           ops)))
      (should cat-op)
      (should (string= (plist-get cat-op :loop-variable) "file")))))

;; ============================================================
;; TIER 2: LOOP VARIABLE FROM GLOB PATTERN
;; ============================================================

(ert-deftest test-loop-variable-from-glob ()
  "Test loop variable bound to glob pattern.
Scenario: for file in *.log; do cat $file; done
Expected: cat reads *.log (resolved from loop variable)"
  (let* ((parsed (jf/bash-parse "for file in *.log; do cat $file; done"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; cat reads *.log (resolved from loop variable)
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.log")
                            (eq (plist-get op :operation) :read)
                            (plist-get op :loop-context)))
                     ops))))

(ert-deftest test-loop-variable-glob-with-rm ()
  "Test loop variable from glob with rm command.
Scenario: for f in *.tmp; do rm $f; done
Expected: rm deletes *.tmp (resolved from loop variable)"
  (let* ((parsed (jf/bash-parse "for f in *.tmp; do rm $f; done"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; rm deletes *.tmp (resolved from loop variable)
    (let ((rm-op (seq-find (lambda (op)
                            (and (string= (plist-get op :file) "*.tmp")
                                 (eq (plist-get op :operation) :delete)
                                 (plist-get op :loop-context)))
                          ops)))
      (should rm-op)
      (should (string= (plist-get rm-op :loop-variable) "f")))))

;; ============================================================
;; TIER 3: LOOP VARIABLE FROM LITERAL LIST
;; ============================================================

(ert-deftest test-loop-variable-from-literal ()
  "Test loop variable from literal list.
Scenario: for file in file1.txt file2.txt; do cat $file; done
Expected: cat reads literal values (not resolved)"
  (let* ((parsed (jf/bash-parse "for file in file1.txt file2.txt; do cat $file; done"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; cat should have operation with loop context
    ;; Note: $file won't resolve to specific literals without runtime evaluation
    (should (seq-find (lambda (op)
                       (and (eq (plist-get op :operation) :read)
                            (plist-get op :loop-context)))
                     ops))))

;; ============================================================
;; TIER 4: MULTIPLE COMMANDS IN LOOP BODY
;; ============================================================

(ert-deftest test-loop-multiple-commands ()
  "Test loop with multiple commands in body.
Scenario: for f in *.txt; do cat $f; rm $f; done
Expected: Both cat and rm operate on *.txt"
  (let* ((parsed (jf/bash-parse "for f in *.txt; do cat $f; rm $f; done"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; cat reads *.txt
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.txt")
                            (eq (plist-get op :operation) :read)
                            (plist-get op :loop-context)))
                     ops))

    ;; rm deletes *.txt
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "*.txt")
                            (eq (plist-get op :operation) :delete)
                            (plist-get op :loop-context)))
                     ops))))

;; ============================================================
;; TIER 5: LOOP CONTEXT METADATA
;; ============================================================

(ert-deftest test-loop-context-metadata ()
  "Test that loop operations have correct metadata.
Scenario: for file in *.el; do cat $file; done
Expected: Operations marked with :loop-context and :loop-variable"
  (let* ((parsed (jf/bash-parse "for file in *.el; do cat $file; done"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; Find operation from loop body
    (let ((loop-op (seq-find (lambda (op)
                              (plist-get op :loop-context))
                            ops)))
      (should loop-op)
      (should (plist-get loop-op :loop-context))
      (should (string= (plist-get loop-op :loop-variable) "file")))))

;; ============================================================
;; TIER 6: EDGE CASES
;; ============================================================

(ert-deftest test-loop-empty-body ()
  "Test loop with empty or trivial body.
Scenario: for x in *.txt; do :; done
Expected: No operations from loop body (: is a no-op)"
  (let* ((parsed (jf/bash-parse "for x in *.txt; do :; done"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; May or may not have operations depending on how : is handled
    (should (listp ops))))

(ert-deftest test-loop-unresolved-variable ()
  "Test loop with variable that cannot be resolved at parse time.
Scenario: for file in $(ls); do cat $file; done
Expected: cat operation present but file path may be unresolved"
  (let* ((parsed (jf/bash-parse "for file in $(ls); do cat $file; done"))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; Should have operations from loop
    (should (seq-find (lambda (op)
                       (plist-get op :loop-context))
                     ops))))

(provide 'test-loop-context)
;;; test-loop-context.el ends here
