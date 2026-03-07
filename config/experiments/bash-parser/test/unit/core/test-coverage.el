;;; test-coverage.el --- Tests for coverage calculation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERT tests for bash-parser coverage calculation and visualization.
;; Tests coverage metrics, edge cases, and output formatting.
;;
;; Test naming convention: test-coverage-<scenario-slug>

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory load-file-name)))
(require 'bash-parser-coverage (expand-file-name "../../../bash-parser-coverage.el"
                                                 (file-name-directory load-file-name)))

;;; Coverage Calculation Tests

(ert-deftest test-coverage-full-coverage ()
  "Test full coverage - all tokens claimed."
  (let* ((tokens '((:id 0 :type :command-name :value "ls" :start 0 :end 2)
                   (:id 1 :type :flag :value "-la" :start 3 :end 6)
                   (:id 2 :type :positional-arg :value "/tmp" :start 7 :end 11)))
         (claimed-token-ids '(0 1 2))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids)))
    (should (= (plist-get coverage :total-tokens) 3))
    (should (= (plist-get coverage :claimed-tokens) 3))
    (should (= (plist-get coverage :coverage-ratio) 1.0))
    (should (null (plist-get coverage :unclaimed-tokens)))))

(ert-deftest test-coverage-partial-coverage ()
  "Test partial coverage - 7 out of 10 tokens claimed."
  (let* ((tokens (cl-loop for i from 0 to 9
                         collect (list :id i :type :positional-arg
                                      :value (format "arg%d" i)
                                      :start (* i 5) :end (+ (* i 5) 4))))
         (claimed-token-ids '(0 1 2 3 4 5 6))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids)))
    (should (= (plist-get coverage :total-tokens) 10))
    (should (= (plist-get coverage :claimed-tokens) 7))
    (should (= (plist-get coverage :coverage-ratio) 0.7))
    (should (= (length (plist-get coverage :unclaimed-tokens)) 3))))

(ert-deftest test-coverage-no-coverage ()
  "Test no coverage - no tokens claimed."
  (let* ((tokens '((:id 0 :type :command-name :value "echo" :start 0 :end 4)
                   (:id 1 :type :positional-arg :value "hello" :start 5 :end 10)))
         (claimed-token-ids '())
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids)))
    (should (= (plist-get coverage :total-tokens) 2))
    (should (= (plist-get coverage :claimed-tokens) 0))
    (should (= (plist-get coverage :coverage-ratio) 0.0))
    (should (= (length (plist-get coverage :unclaimed-tokens)) 2))))

(ert-deftest test-coverage-zero-tokens ()
  "Test zero tokens - returns 1.0 ratio (perfect coverage of nothing)."
  (let* ((tokens '())
         (claimed-token-ids '())
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids)))
    (should (= (plist-get coverage :total-tokens) 0))
    (should (= (plist-get coverage :claimed-tokens) 0))
    (should (= (plist-get coverage :coverage-ratio) 1.0))
    (should (null (plist-get coverage :unclaimed-tokens)))))

(ert-deftest test-coverage-shared-claiming ()
  "Test shared claiming - same token claimed by multiple plugins doesn't exceed 1.0."
  (let* ((tokens '((:id 0 :type :command-name :value "cat" :start 0 :end 3)
                   (:id 1 :type :positional-arg :value "file.txt" :start 4 :end 12)))
         ;; Token 0 claimed twice (by two different plugins)
         (claimed-token-ids '(0 0 1))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids)))
    (should (= (plist-get coverage :total-tokens) 2))
    (should (= (plist-get coverage :claimed-tokens) 2))
    (should (= (plist-get coverage :coverage-ratio) 1.0))
    (should (null (plist-get coverage :unclaimed-tokens)))))

(ert-deftest test-coverage-by-type-single-type ()
  "Test coverage by type with single token type."
  (let* ((tokens '((:id 0 :type :flag :value "-v" :start 0 :end 2)
                   (:id 1 :type :flag :value "-l" :start 3 :end 5)
                   (:id 2 :type :flag :value "-a" :start 6 :end 8)))
         (claimed-token-ids '(0 1))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (coverage-by-type (plist-get coverage :coverage-by-type)))
    (should (= (length coverage-by-type) 1))
    (let ((flag-coverage (alist-get :flag coverage-by-type)))
      (should flag-coverage)
      (should (= flag-coverage (/ 2.0 3.0))))))

(ert-deftest test-coverage-by-type-multiple-types ()
  "Test coverage by type with multiple token types."
  (let* ((tokens '((:id 0 :type :command-name :value "grep" :start 0 :end 4)
                   (:id 1 :type :flag :value "-r" :start 5 :end 7)
                   (:id 2 :type :flag :value "-n" :start 8 :end 10)
                   (:id 3 :type :positional-arg :value "pattern" :start 11 :end 18)
                   (:id 4 :type :positional-arg :value "file" :start 19 :end 23)))
         (claimed-token-ids '(0 1 3 4))  ; Missing one flag
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (coverage-by-type (plist-get coverage :coverage-by-type)))
    (should (= (length coverage-by-type) 3))
    (should (= (alist-get :command-name coverage-by-type) 1.0))
    (should (= (alist-get :flag coverage-by-type) 0.5))
    (should (= (alist-get :positional-arg coverage-by-type) 1.0))))

(ert-deftest test-coverage-unclaimed-tokens-list ()
  "Test unclaimed tokens list includes full token details."
  (let* ((tokens '((:id 0 :type :command-name :value "echo" :start 0 :end 4)
                   (:id 1 :type :positional-arg :value "hello" :start 5 :end 10)
                   (:id 2 :type :positional-arg :value "world" :start 11 :end 16)))
         (claimed-token-ids '(0))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (unclaimed (plist-get coverage :unclaimed-tokens)))
    (should (= (length unclaimed) 2))
    ;; Check first unclaimed token
    (let ((first-unclaimed (nth 0 unclaimed)))
      (should (= (plist-get first-unclaimed :id) 1))
      (should (eq (plist-get first-unclaimed :type) :positional-arg))
      (should (string= (plist-get first-unclaimed :value) "hello"))
      (should (= (plist-get first-unclaimed :start) 5))
      (should (= (plist-get first-unclaimed :end) 10)))
    ;; Check second unclaimed token
    (let ((second-unclaimed (nth 1 unclaimed)))
      (should (= (plist-get second-unclaimed :id) 2))
      (should (eq (plist-get second-unclaimed :type) :positional-arg))
      (should (string= (plist-get second-unclaimed :value) "world"))
      (should (= (plist-get second-unclaimed :start) 11))
      (should (= (plist-get second-unclaimed :end) 16)))))

;;; Visualization Tests

(ert-deftest test-visualization-full-coverage ()
  "Test visualization of full coverage."
  (let* ((tokens '((:id 0 :type :command-name :value "ls" :start 0 :end 2)
                   (:id 1 :type :flag :value "-l" :start 3 :end 5)))
         (claimed-token-ids '(0 1))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (output (jf/bash-visualize-coverage coverage t)))
    (should (string-match-p "Parse Complete: ✓" output))
    (should (string-match-p "Overall Coverage: 100\\.0%" output))
    (should (string-match-p "All tokens claimed!" output))
    (should-not (string-match-p "Unclaimed Tokens" output))))

(ert-deftest test-visualization-partial-coverage ()
  "Test visualization of partial coverage."
  (let* ((tokens '((:id 0 :type :command-name :value "grep" :start 0 :end 4)
                   (:id 1 :type :flag :value "-r" :start 5 :end 7)
                   (:id 2 :type :positional-arg :value "pattern" :start 8 :end 15)))
         (claimed-token-ids '(0 1))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (output (jf/bash-visualize-coverage coverage t)))
    (should (string-match-p "Parse Complete: ✓" output))
    (should (string-match-p "Overall Coverage: 66\\.7%" output))
    (should (string-match-p "Unclaimed Tokens (1):" output))
    (should (string-match-p "\\[:positional-arg\\] \"pattern\" (pos 8-15)" output))))

(ert-deftest test-visualization-parse-incomplete ()
  "Test visualization shows parse incomplete status."
  (let* ((tokens '((:id 0 :type :command-name :value "ls" :start 0 :end 2)))
         (claimed-token-ids '(0))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (output (jf/bash-visualize-coverage coverage nil)))
    (should (string-match-p "Parse Complete: ✗" output))))

(ert-deftest test-visualization-coverage-by-type ()
  "Test visualization includes coverage by type."
  (let* ((tokens '((:id 0 :type :command-name :value "find" :start 0 :end 4)
                   (:id 1 :type :flag :value "-name" :start 5 :end 10)
                   (:id 2 :type :flag :value "-type" :start 11 :end 16)
                   (:id 3 :type :positional-arg :value "*.txt" :start 17 :end 22)))
         (claimed-token-ids '(0 1 2))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (output (jf/bash-visualize-coverage coverage t)))
    (should (string-match-p "Coverage by Type:" output))
    (should (string-match-p ":command-name: 100\\.0%" output))
    (should (string-match-p ":flag: 100\\.0%" output))
    (should (string-match-p ":positional-arg: 0\\.0%" output))))

(ert-deftest test-visualization-progress-bar ()
  "Test visualization includes progress bars."
  (let* ((tokens '((:id 0 :type :command-name :value "cat" :start 0 :end 3)))
         (claimed-token-ids '(0))
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (output (jf/bash-visualize-coverage coverage t)))
    ;; Should contain progress bars with █ and ░ characters
    (should (string-match-p "\\[█" output))))

(ert-deftest test-visualization-no-coverage ()
  "Test visualization of zero coverage."
  (let* ((tokens '((:id 0 :type :command-name :value "echo" :start 0 :end 4)
                   (:id 1 :type :positional-arg :value "hi" :start 5 :end 7)))
         (claimed-token-ids '())
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (output (jf/bash-visualize-coverage coverage t)))
    (should (string-match-p "Overall Coverage: 0\\.0%" output))
    (should (string-match-p "Unclaimed Tokens (2):" output))
    (should (string-match-p "\\[:command-name\\] \"echo\"" output))
    (should (string-match-p "\\[:positional-arg\\] \"hi\"" output))))

(ert-deftest test-visualization-zero-tokens ()
  "Test visualization with zero tokens."
  (let* ((tokens '())
         (claimed-token-ids '())
         (coverage (jf/bash-calculate-coverage tokens claimed-token-ids))
         (output (jf/bash-visualize-coverage coverage t)))
    (should (string-match-p "Overall Coverage: 100\\.0%" output))
    (should (string-match-p "All tokens claimed!" output))))

;;; Progress Bar Helper Tests

(ert-deftest test-progress-bar-full ()
  "Test progress bar at 100% filled."
  (let ((bar (jf/bash--coverage-bar 1.0 10)))
    (should (string= bar "[██████████]"))))

(ert-deftest test-progress-bar-empty ()
  "Test progress bar at 0% filled."
  (let ((bar (jf/bash--coverage-bar 0.0 10)))
    (should (string= bar "[░░░░░░░░░░]"))))

(ert-deftest test-progress-bar-half ()
  "Test progress bar at 50% filled."
  (let ((bar (jf/bash--coverage-bar 0.5 10)))
    (should (string= bar "[█████░░░░░]"))))

(ert-deftest test-progress-bar-custom-width ()
  "Test progress bar with custom width."
  (let ((bar (jf/bash--coverage-bar 0.75 20)))
    (should (= (length bar) 22))  ; 20 chars + 2 brackets
    (should (string-match-p "\\[█\\{15\\}░\\{5\\}\\]" bar))))

(provide 'test-coverage)
;;; test-coverage.el ends here
