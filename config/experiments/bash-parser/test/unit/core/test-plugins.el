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

;;; Semantic Extraction Orchestrator Tests

(ert-deftest test-orchestrator-runs-all-applicable-plugins ()
  "Test that orchestrator executes all plugins without predicates."
  (setq jf/bash-semantic-plugins '())

  ;; Register two plugins without predicates
  (jf/bash-register-plugin
   :name 'plugin-a
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :domain-a
                 :operations '((:op a))
                 :claimed-token-ids '(1)
                 :metadata nil))
   :predicates '())

  (jf/bash-register-plugin
   :name 'plugin-b
   :priority 5
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :domain-b
                 :operations '((:op b))
                 :claimed-token-ids '(2)
                 :metadata nil))
   :predicates '())

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "foo")
                                    (:id 2 :type word :value "bar"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (domains (plist-get result :domains))
         (plugin-results (plist-get result :plugin-results)))

    ;; Both plugins should run
    (should (= (length plugin-results) 2))

    ;; Both domains should be present
    (should (assq :domain-a domains))
    (should (assq :domain-b domains))

    ;; Verify operations
    (should (equal (cdr (assq :domain-a domains)) '((:op a))))
    (should (equal (cdr (assq :domain-b domains)) '((:op b))))))

(ert-deftest test-orchestrator-predicate-filtering ()
  "Test that predicates filter plugin applicability."
  (setq jf/bash-semantic-plugins '())

  ;; Register plugin with predicate that returns nil
  (jf/bash-register-plugin
   :name 'filtered-plugin
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :filtered
                 :operations '((:should-not-run t))
                 :claimed-token-ids '(1)
                 :metadata nil))
   :predicates (list (lambda (_parsed) nil)))

  ;; Register plugin with predicate that returns t
  (jf/bash-register-plugin
   :name 'applicable-plugin
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :applicable
                 :operations '((:did-run t))
                 :claimed-token-ids '(2)
                 :metadata nil))
   :predicates (list (lambda (_parsed) t)))

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "foo")
                                    (:id 2 :type word :value "bar"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (domains (plist-get result :domains))
         (plugin-results (plist-get result :plugin-results)))

    ;; Only applicable plugin should run
    (should (= (length plugin-results) 1))

    ;; Only applicable domain should be present
    (should (assq :applicable domains))
    (should-not (assq :filtered domains))))

(ert-deftest test-orchestrator-empty-predicates-always-runs ()
  "Test that empty predicate list means plugin always runs."
  (setq jf/bash-semantic-plugins '())

  ;; Register plugin with empty predicates
  (jf/bash-register-plugin
   :name 'universal-plugin
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :universal
                 :operations '((:runs always))
                 :claimed-token-ids '(1)
                 :metadata nil))
   :predicates '())

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "foo"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (domains (plist-get result :domains)))

    ;; Plugin should run
    (should (assq :universal domains))
    (should (equal (cdr (assq :universal domains)) '((:runs always))))))

(ert-deftest test-orchestrator-multiple-predicates-all-must-pass ()
  "Test that all predicates must return true for plugin to run."
  (setq jf/bash-semantic-plugins '())

  ;; Register plugin with multiple predicates - one returns nil
  (jf/bash-register-plugin
   :name 'multi-predicate-plugin
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :multi-pred
                 :operations '((:should-not-run t))
                 :claimed-token-ids '(1)
                 :metadata nil))
   :predicates (list (lambda (_parsed) t)
                     (lambda (_parsed) nil)
                     (lambda (_parsed) t)))

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "foo"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (domains (plist-get result :domains)))

    ;; Plugin should not run because one predicate failed
    (should-not (assq :multi-pred domains))))

(ert-deftest test-orchestrator-error-isolation ()
  "Test that plugin errors don't break extraction."
  (setq jf/bash-semantic-plugins '())

  ;; Register failing plugin
  (jf/bash-register-plugin
   :name 'failing-plugin
   :priority 10
   :extractor (lambda (_parsed)
                (error "Plugin failure"))
   :predicates '())

  ;; Register successful plugin
  (jf/bash-register-plugin
   :name 'working-plugin
   :priority 5
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :working
                 :operations '((:op success))
                 :claimed-token-ids '(1)
                 :metadata nil))
   :predicates '())

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "foo"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (domains (plist-get result :domains))
         (plugin-results (plist-get result :plugin-results)))

    ;; Working plugin should still run
    (should (= (length plugin-results) 1))
    (should (assq :working domains))
    (should (equal (cdr (assq :working domains)) '((:op success))))))

(ert-deftest test-orchestrator-partial-results-on-error ()
  "Test that partial results are returned when some plugins fail."
  (setq jf/bash-semantic-plugins '())

  ;; Register three plugins: success, fail, success
  (jf/bash-register-plugin
   :name 'plugin-1
   :priority 30
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :domain-1
                 :operations '((:op 1))
                 :claimed-token-ids '(1)
                 :metadata nil))
   :predicates '())

  (jf/bash-register-plugin
   :name 'plugin-2
   :priority 20
   :extractor (lambda (_parsed)
                (error "Failure"))
   :predicates '())

  (jf/bash-register-plugin
   :name 'plugin-3
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :domain-3
                 :operations '((:op 3))
                 :claimed-token-ids '(3)
                 :metadata nil))
   :predicates '())

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "a")
                                    (:id 2 :type word :value "b")
                                    (:id 3 :type word :value "c"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (domains (plist-get result :domains))
         (plugin-results (plist-get result :plugin-results)))

    ;; Two successful plugins
    (should (= (length plugin-results) 2))

    ;; Both successful domains present
    (should (assq :domain-1 domains))
    (should (assq :domain-3 domains))

    ;; Failed plugin's domain not present
    (should-not (assq :domain-2 domains))))

(ert-deftest test-orchestrator-shared-token-claiming ()
  "Test that multiple plugins can claim the same token."
  (setq jf/bash-semantic-plugins '())

  ;; Register two plugins that claim the same token
  (jf/bash-register-plugin
   :name 'plugin-a
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :domain-a
                 :operations '((:op a))
                 :claimed-token-ids '(1 2)
                 :metadata nil))
   :predicates '())

  (jf/bash-register-plugin
   :name 'plugin-b
   :priority 5
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :domain-b
                 :operations '((:op b))
                 :claimed-token-ids '(2 3)
                 :metadata nil))
   :predicates '())

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "a")
                                    (:id 2 :type word :value "b")
                                    (:id 3 :type word :value "c"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (coverage (plist-get result :coverage))
         (claimed-tokens (plist-get coverage :claimed-tokens)))

    ;; All three tokens should be claimed (token 2 is claimed by both)
    (should (= claimed-tokens 3))))

(ert-deftest test-orchestrator-result-aggregation ()
  "Test that results are properly aggregated by domain."
  (setq jf/bash-semantic-plugins '())

  ;; Register two plugins with same domain
  (jf/bash-register-plugin
   :name 'plugin-a
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :files
                 :operations '((:op read :file "a.txt"))
                 :claimed-token-ids '(1)
                 :metadata nil))
   :predicates '())

  (jf/bash-register-plugin
   :name 'plugin-b
   :priority 5
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :files
                 :operations '((:op write :file "b.txt"))
                 :claimed-token-ids '(2)
                 :metadata nil))
   :predicates '())

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "foo")
                                    (:id 2 :type word :value "bar"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (domains (plist-get result :domains))
         (files-ops (cdr (assq :files domains))))

    ;; Both operations should be in the same domain
    (should (= (length files-ops) 2))
    (should (member '(:op read :file "a.txt") files-ops))
    (should (member '(:op write :file "b.txt") files-ops))))

(ert-deftest test-orchestrator-coverage-calculation ()
  "Test that coverage is calculated correctly."
  (setq jf/bash-semantic-plugins '())

  ;; Register plugin that claims 2 out of 3 tokens
  (jf/bash-register-plugin
   :name 'partial-coverage-plugin
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :test
                 :operations '((:op test))
                 :claimed-token-ids '(1 2)
                 :metadata nil))
   :predicates '())

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "a")
                                    (:id 2 :type word :value "b")
                                    (:id 3 :type word :value "c"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (coverage (plist-get result :coverage))
         (coverage-ratio (plist-get coverage :coverage-ratio)))

    ;; Coverage should be 2/3
    (should (equal coverage-ratio (/ 2.0 3.0)))))

(ert-deftest test-orchestrator-parse-complete-passthrough ()
  "Test that :parse-complete is passed through from input."
  (setq jf/bash-semantic-plugins '())

  ;; Test with parse-complete true
  (let* ((parsed-command-complete '(:tokens () :parse-complete t))
         (result-complete (jf/bash-extract-semantics parsed-command-complete)))
    (should (eq (plist-get result-complete :parse-complete) t)))

  ;; Test with parse-complete false
  (let* ((parsed-command-incomplete '(:tokens () :parse-complete nil))
         (result-incomplete (jf/bash-extract-semantics parsed-command-incomplete)))
    (should (eq (plist-get result-incomplete :parse-complete) nil))))

(ert-deftest test-orchestrator-plugin-results-included ()
  "Test that raw plugin results are included for debugging."
  (setq jf/bash-semantic-plugins '())

  (jf/bash-register-plugin
   :name 'test-plugin
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :test
                 :operations '((:op test))
                 :claimed-token-ids '(1)
                 :metadata '(:debug-info "test-data")))
   :predicates '())

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "foo"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (plugin-results (plist-get result :plugin-results)))

    ;; Plugin results should be included
    (should (= (length plugin-results) 1))
    (let ((plugin-result (car plugin-results)))
      (should (jf/bash-plugin-result-p plugin-result))
      (should (eq (jf/bash-plugin-result-domain plugin-result) :test))
      (should (equal (jf/bash-plugin-result-metadata plugin-result)
                     '(:debug-info "test-data"))))))

(ert-deftest test-orchestrator-predicate-error-handling ()
  "Test that predicate errors are caught and plugin is skipped."
  (setq jf/bash-semantic-plugins '())

  ;; Register plugin with failing predicate
  (jf/bash-register-plugin
   :name 'failing-predicate-plugin
   :priority 10
   :extractor (lambda (_parsed)
                (make-jf/bash-plugin-result
                 :domain :test
                 :operations '((:should-not-run t))
                 :claimed-token-ids '(1)
                 :metadata nil))
   :predicates (list (lambda (_parsed) (error "Predicate error"))))

  ;; Execute orchestrator
  (let* ((parsed-command '(:tokens ((:id 1 :type word :value "foo"))
                           :parse-complete t))
         (result (jf/bash-extract-semantics parsed-command))
         (domains (plist-get result :domains)))

    ;; Plugin should not run due to predicate error
    (should-not (assq :test domains))))

(provide 'test-plugins)
;;; test-plugins.el ends here
