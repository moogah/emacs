;;; testing-spec.el --- Buttercup example tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Simple Buttercup test suite demonstrating BDD-style testing.
;; This validates the Buttercup installation and serves as a reference.
;;
;; Test file naming: *-spec.el (Buttercup) vs *-test.el (ERT)

;;; Code:

(require 'buttercup)

;;; Basic Assertions

(describe "Buttercup basic tests"
  (it "validates simple equality"
    (expect (+ 1 1) :to-equal 2))

  (it "validates string equality"
    (expect "hello" :to-equal "hello"))

  (it "validates list equality"
    (expect '(1 2 3) :to-equal '(1 2 3))))

;;; Setup and Teardown

(describe "Setup and teardown"
  :var (test-value)

  (before-each
    (setq test-value 42))

  (it "uses setup values"
    (expect test-value :to-equal 42))

  (it "has independent test runs"
    (setq test-value 100)
    (expect test-value :to-equal 100)))

;;; Nested Contexts

(describe "Nested contexts"
  :var (outer-value)

  (before-each
    (setq outer-value "outer"))

  (describe "inner context"
    :var (inner-value)

    (before-each
      (setq inner-value "inner"))

    (it "has access to both contexts"
      (expect outer-value :to-equal "outer")
      (expect inner-value :to-equal "inner"))))

;;; String Matchers

(describe "String matching"
  (it "matches regexp patterns"
    (expect "hello world" :to-match "hello")
    (expect "test-123" :to-match "[0-9]+")))

;;; List Operations

(describe "List operations"
  (it "validates list membership"
    (expect '(1 2 3) :to-contain 2))

  (it "validates list equality ignoring order"
    (expect '(1 2 3) :to-have-same-items-as '(3 2 1))))

;;; Spy System

(describe "Spy system"
  :var (test-function)

  (before-each
    (defun test-function (x)
      (* x 2))
    (spy-on 'test-function))

  (it "tracks function calls"
    (test-function 5)
    (expect 'test-function :to-have-been-called))

  (it "tracks call arguments"
    (test-function 42)
    (expect 'test-function :to-have-been-called-with 42)))

;;; Spy Stubbing

(describe "Spy stubbing"
  :var (stubbed-function)

  (before-each
    (defun stubbed-function (x)
      (error "Should not be called"))
    (spy-on 'stubbed-function :and-return-value 100))

  (it "returns stubbed value"
    (expect (stubbed-function 42) :to-equal 100))

  (it "does not execute original"
    (expect (stubbed-function "anything") :to-equal 100)))

(provide 'testing-spec)
;;; testing-spec.el ends here
