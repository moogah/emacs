;;; test-tool-invocation-spec.el --- Tests for gptel tool invocation mechanics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; Behavioral tests for the BUG: gptel async tools fail with wrong-number-of-arguments
;;
;; OBSERVED BUG (from session.md):
;;   Tool call: read_file(:filepath "~/.machine-role")
;;   Error: wrong-number-of-arguments #[(callback filepath) ...] 1
;;
;; ROOT CAUSE HYPOTHESIS:
;;   gptel-make-scoped-tool creates async tools with signature: (callback arg1 arg2 ...)
;;   But gptel internally may be extracting args from the :args plist incorrectly,
;;   resulting in the entire plist being passed as a single argument.
;;
;; This test captures the minimal reproduction so we can verify and fix it.

;;; Code:

(require 'buttercup)
(require 'jf-gptel-scope-core)

(describe "gptel async scoped tool invocation"

  (describe "minimal bug reproduction"

    (it "should fail when tool is called with plist instead of extracted values"
      ;; Create a simple async scoped tool
      (let ((test-fn nil))
        ;; Note: gptel-make-scoped-tool defines the function directly
        (eval
         '(gptel-make-scoped-tool
           "bug_repro"
           "Test tool"
           (list '(:name "filepath" :type string :description "Path"))
           "test"
           :async
           (list :success t :content filepath)))

        ;; The macro should have created a function with (callback filepath) signature
        ;; If we call it with only 1 arg (a plist), it should fail
        (setq test-fn (symbol-function 'bug_repro))

        (expect
         (funcall test-fn '(:filepath "~/file"))
         :to-throw 'wrong-number-of-arguments)))

    (it "should work when called correctly with callback and extracted args"
      (let ((captured-path nil))
        (eval
         '(gptel-make-scoped-tool
           "correct_call"
           "Test tool"
           (list '(:name "filepath" :type string :description "Path"))
           "test"
           :async
           (progn
             (setq captured-path filepath)
             (funcall callback (json-serialize (list :success t))))))

        ;; Call with callback as first arg, filepath as second arg
        (let ((callback-result nil))
          (funcall (symbol-function 'correct_call)
                   (lambda (result) (setq callback-result result))
                   "~/test-file")

          (expect captured-path :to-equal "~/test-file")
          (expect callback-result :to-be-truthy))))))

(provide 'test-tool-invocation-spec)
;;; test-tool-invocation-spec.el ends here
