;;; test-async-flag-spec.el --- Test that async scoped tools are marked async -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; Tests for the BUG FIX: gptel-make-scoped-tool now passes :async t to gptel-make-tool
;;
;; ORIGINAL BUG:
;;   gptel-make-scoped-tool created callback-first lambda functions but didn't
;;   tell gptel-make-tool that the tool was async. This caused gptel to invoke
;;   the tool without the callback, resulting in wrong-number-of-arguments errors.
;;
;; THE FIX:
;;   Added `,@(when is-async '(:async t))` to the gptel-make-tool expansion
;;   in gptel-make-scoped-tool macro.
;;
;; This test verifies that async scoped tools are properly registered as async.

;;; Code:

(require 'buttercup)
(require 'gptel)
(require 'jf-gptel-scope-core)

(defun test-find-tool-in-registry (tool-name)
  "Find TOOL-NAME in gptel--known-tools registry (alist structure)."
  (catch 'found
    (dolist (category-entry gptel--known-tools)
      (let ((tools-alist (cdr category-entry)))
        (dolist (tool-entry tools-alist)
          (when (string= (car tool-entry) tool-name)
            (throw 'found (cdr tool-entry))))))
    nil))

(describe "gptel-make-scoped-tool async flag propagation"

  (describe "async scoped tools"

    (it "marks async tools as async in the registry"
      ;; Create an async scoped tool
      (eval
       '(gptel-make-scoped-tool
         "test_async_flag"
         "Test async flag propagation"
         (list '(:name "arg1" :type string :description "Arg"))
         "test"
         :async
         (list :success t :value arg1)))

      ;; Check that it's registered as async
      (let ((tool-found (test-find-tool-in-registry "test_async_flag")))
        (expect tool-found :to-be-truthy)
        (expect (gptel-tool-async tool-found) :to-be t)))

    (it "creates callback-first function signature for async tools"
      ;; The function should have (callback arg1) signature
      (eval
       '(gptel-make-scoped-tool
         "test_callback_sig"
         "Test callback signature"
         (list '(:name "filepath" :type string :description "Path"))
         "test"
         :async
         (list :success t :content filepath)))

      (let ((tool-found (test-find-tool-in-registry "test_callback_sig")))
        (expect tool-found :to-be-truthy)
        (let ((fn (gptel-tool-function tool-found)))
          (expect (functionp fn) :to-be t)
          ;; Check that first parameter is 'callback
          (let ((args (help-function-arglist fn t)))
            (expect (car args) :to-be 'callback))))))

  (describe "sync scoped tools"

    (it "does not mark sync tools as async"
      (eval
       '(gptel-make-scoped-tool
         "test_sync_flag"
         "Test sync flag"
         (list '(:name "arg1" :type string :description "Arg"))
         "test"
         ; No :async keyword
         (list :success t :value arg1)))

      (let ((tool-found (test-find-tool-in-registry "test_sync_flag")))
        (expect tool-found :to-be-truthy)
        (expect (gptel-tool-async tool-found) :to-be nil)))))

(describe "regression test for read_file tool"

  (it "read_file should be marked as async"
    ;; read_file is defined with :async in scope-filesystem-tools.el
    (let ((tool-found (test-find-tool-in-registry "read_file")))
      (expect tool-found :to-be-truthy)
      (expect (gptel-tool-async tool-found) :to-be t)))

  (it "read_file function should have callback-first signature"
    (let ((tool-found (test-find-tool-in-registry "read_file")))
      (expect tool-found :to-be-truthy)
      (let* ((fn (gptel-tool-function tool-found))
             (args (help-function-arglist fn t)))
        (expect (car args) :to-be 'callback)
        (expect (length args) :to-be 2)))))

(provide 'test-async-flag-spec)
;;; test-async-flag-spec.el ends here

