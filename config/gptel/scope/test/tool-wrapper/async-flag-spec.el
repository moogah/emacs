;;; async-flag-spec.el --- Async flag propagation through gptel-make-scoped-tool -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verifies that the :async keyword on gptel-make-scoped-tool reaches
;; the registered gptel tool struct, producing a callback-first
;; function signature, and that omitting :async produces a sync
;; signature.

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

(describe "gptel-make-scoped-tool async flag propagation"

  (describe "async scoped tools"

    (it "marks async tools as async in the registry"
      (eval
       '(gptel-make-scoped-tool
         "test_async_flag"
         "Test async flag propagation"
         (list '(:name "arg1" :type string :description "Arg"))
         :operation read
         :async
         (list :success t :value arg1)))
      (let ((tool-found (async-flag--find-tool-in-registry "test_async_flag")))
        (expect tool-found :to-be-truthy)
        (expect (gptel-tool-async tool-found) :to-be t)))

    (it "creates callback-first function signature for async tools"
      (eval
       '(gptel-make-scoped-tool
         "test_callback_sig"
         "Test callback signature"
         (list '(:name "filepath" :type string :description "Path"))
         :operation read
         :async
         (list :success t :content filepath)))
      (let ((tool-found (async-flag--find-tool-in-registry "test_callback_sig")))
        (expect tool-found :to-be-truthy)
        (let ((fn (gptel-tool-function tool-found)))
          (expect (functionp fn) :to-be t)
          (let ((args (help-function-arglist fn t)))
            (expect (car args) :to-be 'callback))))))

  (describe "sync scoped tools"

    (it "does not mark sync tools as async"
      (eval
       '(gptel-make-scoped-tool
         "test_sync_flag"
         "Test sync flag"
         (list '(:name "arg1" :type string :description "Arg"))
         :operation read
         (list :success t :value arg1)))
      (let ((tool-found (async-flag--find-tool-in-registry "test_sync_flag")))
        (expect tool-found :to-be-truthy)
        (expect (gptel-tool-async tool-found) :to-be nil)))))

(describe "read_file tool registration"

  (it "read_file is marked as async"
    (let ((tool-found (async-flag--find-tool-in-registry "read_file")))
      (expect tool-found :to-be-truthy)
      (expect (gptel-tool-async tool-found) :to-be t)))

  (it "read_file has callback-first signature"
    (let ((tool-found (async-flag--find-tool-in-registry "read_file")))
      (expect tool-found :to-be-truthy)
      (let* ((fn (gptel-tool-function tool-found))
             (args (help-function-arglist fn t)))
        (expect (car args) :to-be 'callback)
        (expect (length args) :to-be 2)))))

(provide 'async-flag-spec)

;;; async-flag-spec.el ends here
