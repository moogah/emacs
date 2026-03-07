;;; test-plugins.el --- Tests for plugin infrastructure -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERT tests for bash-parser plugin infrastructure.
;; Tests plugin registration, result structure, and priority ordering.
;;
;; Test naming convention: test-plugin-<scenario-slug>

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory load-file-name)))
(require 'bash-parser-plugins (expand-file-name "../../../bash-parser-plugins.el"
                                                (file-name-directory load-file-name)))

;;; Plugin Result Struct Tests

(ert-deftest test-plugin-result-struct-creation ()
  "Test creating a plugin result struct with all fields."
  (let ((result (make-jf/bash-plugin-result
                 :domain :file-operations
                 :operations '((:type read :path "/foo"))
                 :claimed-token-ids '(1 2 3)
                 :metadata '(:confidence high))))
    (should (jf/bash-plugin-result-p result))
    (should (eq (jf/bash-plugin-result-domain result) :file-operations))
    (should (equal (jf/bash-plugin-result-operations result)
                   '((:type read :path "/foo"))))
    (should (equal (jf/bash-plugin-result-claimed-token-ids result)
                   '(1 2 3)))
    (should (equal (jf/bash-plugin-result-metadata result)
                   '(:confidence high)))))

(ert-deftest test-plugin-result-struct-accessors ()
  "Test that struct accessors work correctly."
  (let ((result (make-jf/bash-plugin-result
                 :domain :variables
                 :operations nil
                 :claimed-token-ids nil
                 :metadata nil)))
    (should (eq (jf/bash-plugin-result-domain result) :variables))
    (should (null (jf/bash-plugin-result-operations result)))
    (should (null (jf/bash-plugin-result-claimed-token-ids result)))
    (should (null (jf/bash-plugin-result-metadata result)))))

(ert-deftest test-plugin-result-struct-type-predicate ()
  "Test that type predicate distinguishes plugin results from other values."
  (let ((result (make-jf/bash-plugin-result :domain :test))
        (not-result '(:domain :test)))
    (should (jf/bash-plugin-result-p result))
    (should-not (jf/bash-plugin-result-p not-result))
    (should-not (jf/bash-plugin-result-p nil))
    (should-not (jf/bash-plugin-result-p "string"))))

;;; Plugin Registration Tests

(ert-deftest test-plugin-registration-basic ()
  "Test registering a plugin and verifying it appears in registry."
  ;; Clear registry
  (setq jf/bash-semantic-plugins '())

  ;; Register a plugin
  (jf/bash-register-plugin
   :name 'test-plugin
   :priority 10
   :extractor (lambda (_tree) nil)
   :predicates '())

  ;; Verify it's in the registry
  (should (= (length jf/bash-semantic-plugins) 1))
  (let ((plugin (car jf/bash-semantic-plugins)))
    (should (eq (plist-get plugin :name) 'test-plugin))
    (should (= (plist-get plugin :priority) 10))
    (should (functionp (plist-get plugin :extractor)))
    (should (listp (plist-get plugin :predicates)))))

(ert-deftest test-plugin-registration-returns-name ()
  "Test that registration function returns the plugin name."
  (setq jf/bash-semantic-plugins '())
  (let ((result (jf/bash-register-plugin
                 :name 'my-plugin
                 :priority 5
                 :extractor (lambda (_tree) nil)
                 :predicates '())))
    (should (eq result 'my-plugin))))

(ert-deftest test-plugin-priority-sorting-higher-first ()
  "Test that plugins are sorted by priority with higher values first."
  (setq jf/bash-semantic-plugins '())

  ;; Register plugins in reverse priority order
  (jf/bash-register-plugin
   :name 'low-priority
   :priority 5
   :extractor (lambda (_tree) nil)
   :predicates '())

  (jf/bash-register-plugin
   :name 'high-priority
   :priority 20
   :extractor (lambda (_tree) nil)
   :predicates '())

  (jf/bash-register-plugin
   :name 'medium-priority
   :priority 10
   :extractor (lambda (_tree) nil)
   :predicates '())

  ;; Verify they're sorted correctly (highest priority first)
  (should (= (length jf/bash-semantic-plugins) 3))
  (should (eq (plist-get (nth 0 jf/bash-semantic-plugins) :name) 'high-priority))
  (should (eq (plist-get (nth 1 jf/bash-semantic-plugins) :name) 'medium-priority))
  (should (eq (plist-get (nth 2 jf/bash-semantic-plugins) :name) 'low-priority)))

(ert-deftest test-plugin-same-priority-maintains-registration-order ()
  "Test that plugins with same priority maintain registration order."
  (setq jf/bash-semantic-plugins '())

  ;; Register three plugins with same priority
  (jf/bash-register-plugin
   :name 'plugin-a
   :priority 10
   :extractor (lambda (_tree) nil)
   :predicates '())

  (jf/bash-register-plugin
   :name 'plugin-b
   :priority 10
   :extractor (lambda (_tree) nil)
   :predicates '())

  (jf/bash-register-plugin
   :name 'plugin-c
   :priority 10
   :extractor (lambda (_tree) nil)
   :predicates '())

  ;; Verify registration order is maintained
  (should (= (length jf/bash-semantic-plugins) 3))
  (should (eq (plist-get (nth 0 jf/bash-semantic-plugins) :name) 'plugin-a))
  (should (eq (plist-get (nth 1 jf/bash-semantic-plugins) :name) 'plugin-b))
  (should (eq (plist-get (nth 2 jf/bash-semantic-plugins) :name) 'plugin-c)))

(ert-deftest test-plugin-re-registration-updates-entry ()
  "Test that re-registering a plugin updates its entry."
  (setq jf/bash-semantic-plugins '())

  ;; Register a plugin
  (jf/bash-register-plugin
   :name 'test-plugin
   :priority 10
   :extractor (lambda (_tree) 'original)
   :predicates '())

  ;; Re-register with different priority
  (jf/bash-register-plugin
   :name 'test-plugin
   :priority 20
   :extractor (lambda (_tree) 'updated)
   :predicates '())

  ;; Verify only one entry exists with updated values
  (should (= (length jf/bash-semantic-plugins) 1))
  (let ((plugin (car jf/bash-semantic-plugins)))
    (should (eq (plist-get plugin :name) 'test-plugin))
    (should (= (plist-get plugin :priority) 20))
    (should (eq (funcall (plist-get plugin :extractor) nil) 'updated))))

;;; Plugin Registration Validation Tests

(ert-deftest test-plugin-registration-requires-name ()
  "Test that registration fails without :name."
  (setq jf/bash-semantic-plugins '())
  (should-error
   (jf/bash-register-plugin
    :priority 10
    :extractor (lambda (_tree) nil)
    :predicates '())
   :type 'error))

(ert-deftest test-plugin-registration-requires-numeric-priority ()
  "Test that registration fails without numeric :priority."
  (setq jf/bash-semantic-plugins '())
  (should-error
   (jf/bash-register-plugin
    :name 'test-plugin
    :priority "high"
    :extractor (lambda (_tree) nil)
    :predicates '())
   :type 'error))

(ert-deftest test-plugin-registration-requires-extractor-function ()
  "Test that registration fails without function :extractor."
  (setq jf/bash-semantic-plugins '())
  (should-error
   (jf/bash-register-plugin
    :name 'test-plugin
    :priority 10
    :extractor 'not-a-function
    :predicates '())
   :type 'error))

(ert-deftest test-plugin-registration-requires-predicates-list ()
  "Test that registration fails if :predicates is not a list."
  (setq jf/bash-semantic-plugins '())
  (should-error
   (jf/bash-register-plugin
    :name 'test-plugin
    :priority 10
    :extractor (lambda (_tree) nil)
    :predicates 'not-a-list)
   :type 'error))

(provide 'test-plugins)
;;; test-plugins.el ends here
