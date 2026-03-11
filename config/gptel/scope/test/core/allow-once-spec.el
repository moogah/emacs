;;; allow-once-spec.el --- Tests for allow-once lifecycle and async flag propagation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CONSOLIDATED TESTS: Allow-once permission lifecycle and async tool flag propagation
;;
;; Absorbs:
;; - scope/test/test-async-flag-spec.el (5 Buttercup tests on async flag through validation)
;; - New tests for allow-once add/check/clear/session-scoping
;;
;; Test organization:
;; 1. Allow-once list management (add, check, clear)
;; 2. Allow-once consumption semantics
;; 3. Session scoping (buffer-local behavior)
;; 4. Async flag propagation (from test-async-flag-spec.el)

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'gptel)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-dir (expand-file-name "../.." test-dir)))
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir)))

;;; Allow-Once List Management Tests

(describe "allow-once list management"

  (before-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (describe "jf/gptel-scope-add-to-allow-once-list"

    (it "adds tool-name and resource to the list"
      (jf/gptel-scope-add-to-allow-once-list "read_file" "/workspace/file.txt")
      (expect jf/gptel-scope--allow-once-list :not :to-be nil)
      (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
      (expect (car (car jf/gptel-scope--allow-once-list)) :to-equal "read_file")
      (expect (cdr (car jf/gptel-scope--allow-once-list)) :to-equal "/workspace/file.txt"))

    (it "supports multiple entries"
      (jf/gptel-scope-add-to-allow-once-list "read_file" "/workspace/file1.txt")
      (jf/gptel-scope-add-to-allow-once-list "write_file_in_scope" "/workspace/file2.txt")
      (expect (length jf/gptel-scope--allow-once-list) :to-equal 2)))

  (describe "jf/gptel-scope--clear-allow-once"

    (it "clears the allow-once list"
      (jf/gptel-scope-add-to-allow-once-list "read_file" "/workspace/file.txt")
      (expect jf/gptel-scope--allow-once-list :not :to-be nil)
      (jf/gptel-scope--clear-allow-once)
      (expect jf/gptel-scope--allow-once-list :to-be nil))

    (it "handles clearing an already empty list"
      (jf/gptel-scope--clear-allow-once)
      (expect jf/gptel-scope--allow-once-list :to-be nil)))

  (describe "jf/gptel-scope--check-allow-once"

    (it "returns t and consumes permission when matching entry exists"
      ;; Add permission for read_file on a specific path
      ;; The resource format for path tools is the expanded file path
      (let ((expanded-path (expand-file-name "/workspace/file.txt")))
        (jf/gptel-scope-add-to-allow-once-list "read_file" expanded-path)
        (let ((config '(:paths (:read () :write () :execute () :modify () :deny ()))))
          (expect (jf/gptel-scope--check-allow-once
                   "read_file" (list "/workspace/file.txt") config)
                  :to-be t)
          ;; Permission consumed
          (expect jf/gptel-scope--allow-once-list :to-be nil))))

    (it "returns nil when no matching entry exists"
      (let ((config '(:paths (:read () :write () :execute () :modify () :deny ()))))
        (expect (jf/gptel-scope--check-allow-once
                 "read_file" (list "/workspace/file.txt") config)
                :to-be nil)))

    (it "returns nil when list is empty"
      (let ((config '(:paths (:read () :write () :execute () :modify () :deny ()))))
        (expect (jf/gptel-scope--check-allow-once
                 "read_file" (list "/workspace/file.txt") config)
                :to-be nil)))))

;;; Async Flag Propagation Tests (from test-async-flag-spec.el)

(defun test-allow-once--find-tool-in-registry (tool-name)
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
      (eval
       '(gptel-make-scoped-tool
         "test_async_flag"
         "Test async flag propagation"
         (list '(:name "arg1" :type string :description "Arg"))
         "test"
         :async
         (list :success t :value arg1)))
      (let ((tool-found (test-allow-once--find-tool-in-registry "test_async_flag")))
        (expect tool-found :to-be-truthy)
        (expect (gptel-tool-async tool-found) :to-be t)))

    (it "creates callback-first function signature for async tools"
      (eval
       '(gptel-make-scoped-tool
         "test_callback_sig"
         "Test callback signature"
         (list '(:name "filepath" :type string :description "Path"))
         "test"
         :async
         (list :success t :content filepath)))
      (let ((tool-found (test-allow-once--find-tool-in-registry "test_callback_sig")))
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
         "test"
         ;; No :async keyword
         (list :success t :value arg1)))
      (let ((tool-found (test-allow-once--find-tool-in-registry "test_sync_flag")))
        (expect tool-found :to-be-truthy)
        (expect (gptel-tool-async tool-found) :to-be nil)))))

(describe "regression test for read_file tool"

  (it "read_file should be marked as async"
    (let ((tool-found (test-allow-once--find-tool-in-registry "read_file")))
      (expect tool-found :to-be-truthy)
      (expect (gptel-tool-async tool-found) :to-be t)))

  (it "read_file function should have callback-first signature"
    (let ((tool-found (test-allow-once--find-tool-in-registry "read_file")))
      (expect tool-found :to-be-truthy)
      (let* ((fn (gptel-tool-function tool-found))
             (args (help-function-arglist fn t)))
        (expect (car args) :to-be 'callback)
        (expect (length args) :to-be 2)))))

(provide 'allow-once-spec)

;;; allow-once-spec.el ends here
