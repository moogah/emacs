;;; async-flag-spec.el --- Scoped tool registration is always async -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verifies that every scoped tool registered via gptel-make-scoped-tool
;; reaches the gptel registry as async with a callback-first function
;; signature. The expansion UI resolves asynchronously, so there is no
;; supported sync path.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'gptel)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-dir (expand-file-name "../.." test-dir)))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope-tool-wrapper.el" scope-dir)))

(defun async-flag--find-tool-in-registry (tool-name)
  "Find TOOL-NAME in `gptel--known-tools' (alist-of-alists)."
  (catch 'found
    (dolist (category-entry gptel--known-tools)
      (let ((tools-alist (cdr category-entry)))
        (dolist (tool-entry tools-alist)
          (when (string= (car tool-entry) tool-name)
            (throw 'found (cdr tool-entry))))))
    nil))

(describe "gptel-make-scoped-tool registration"

  (it "marks scoped tools as async in the registry"
    (eval
     '(gptel-make-scoped-tool
       "test_async_flag"
       "Test async flag propagation"
       (list '(:name "arg1" :type string :description "Arg"))
       :operation read
       (list :success t :value arg1)))
    (let ((tool-found (async-flag--find-tool-in-registry "test_async_flag")))
      (expect tool-found :to-be-truthy)
      (expect (gptel-tool-async tool-found) :to-be t)))

  (it "creates a callback-first function signature"
    (eval
     '(gptel-make-scoped-tool
       "test_callback_sig"
       "Test callback signature"
       (list '(:name "filepath" :type string :description "Path"))
       :operation read
       (list :success t :content filepath)))
    (let ((tool-found (async-flag--find-tool-in-registry "test_callback_sig")))
      (expect tool-found :to-be-truthy)
      (let ((fn (gptel-tool-function tool-found)))
        (expect (functionp fn) :to-be t)
        (let ((args (help-function-arglist fn t)))
          (expect (car args) :to-be 'callback))))))

(describe "read_file_in_scope tool registration"

  (it "read_file_in_scope is marked as async"
    (let ((tool-found (async-flag--find-tool-in-registry "read_file_in_scope")))
      (expect tool-found :to-be-truthy)
      (expect (gptel-tool-async tool-found) :to-be t)))

  (it "read_file_in_scope has callback-first signature"
    (let ((tool-found (async-flag--find-tool-in-registry "read_file_in_scope")))
      (expect tool-found :to-be-truthy)
      (let* ((fn (gptel-tool-function tool-found))
             (args (help-function-arglist fn t)))
        (expect (car args) :to-be 'callback)
        (expect (length args) :to-be 2)))))

(provide 'async-flag-spec)

;;; async-flag-spec.el ends here
