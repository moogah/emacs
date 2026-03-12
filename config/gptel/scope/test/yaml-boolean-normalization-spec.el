;;; yaml-boolean-normalization-spec.el --- Tests for YAML boolean normalization -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file tests the normalization of YAML boolean keywords (:true, :false, :null)
;; to elisp booleans (t, nil) throughout the scope configuration pipeline.
;;
;; Context: YAML parsers return keyword symbols for boolean literals, which must be
;; normalized to elisp booleans for proper validation and round-trip consistency.
;;
;; Test coverage:
;; 1. Normalization of individual boolean values
;; 2. Recursive normalization in nested plists
;; 3. YAML writing with keyword booleans (defensive)
;; 4. Validation with keyword booleans (defensive)
;; 5. Profile loading round-trip (YAML → plist → YAML)

;;; Code:

(require 'buttercup)
(require 'yaml)

;; Load dependencies with explicit paths to avoid circular dependency issues
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-dir (expand-file-name ".." test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  ;; Load in dependency order
  (require 'jf-gptel-scope-expansion (expand-file-name "scope/scope-expansion.el" gptel-dir))
  (require 'jf-gptel-scope-core (expand-file-name "scope/scope-core.el" gptel-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  (require 'gptel-scope-profiles (expand-file-name "scope-profiles.el" gptel-dir)))

;;; Test Suite

(describe "YAML Boolean Normalization"

  (describe "jf/gptel-scope-profile--normalize-boolean"

    (it "converts :true to t"
      (expect (jf/gptel-scope-profile--normalize-boolean :true) :to-be t))

    (it "converts :false to nil"
      (expect (jf/gptel-scope-profile--normalize-boolean :false) :to-be nil))

    (it "converts :null to nil"
      (expect (jf/gptel-scope-profile--normalize-boolean :null) :to-be nil))

    (it "passes through t unchanged"
      (expect (jf/gptel-scope-profile--normalize-boolean t) :to-be t))

    (it "passes through nil unchanged"
      (expect (jf/gptel-scope-profile--normalize-boolean nil) :to-be nil))

    (it "passes through strings unchanged"
      (expect (jf/gptel-scope-profile--normalize-boolean "hello") :to-equal "hello"))

    (it "passes through numbers unchanged"
      (expect (jf/gptel-scope-profile--normalize-boolean 42) :to-equal 42))

    (it "passes through other keywords unchanged"
      (expect (jf/gptel-scope-profile--normalize-boolean :foo) :to-equal :foo)))

  (describe "jf/gptel-scope-profile--normalize-keys with booleans"

    (it "normalizes :true in top-level plist"
      (let* ((input '(:enforce-parse-complete :true))
             (result (jf/gptel-scope-profile--normalize-keys input)))
        (expect (plist-get result :enforce-parse-complete) :to-be t)))

    (it "normalizes :false in top-level plist"
      (let* ((input '(:enforce-parse-complete :false))
             (result (jf/gptel-scope-profile--normalize-keys input)))
        (expect (plist-get result :enforce-parse-complete) :to-be nil)))

    (it "normalizes :null in top-level plist"
      (let* ((input '(:enforce-parse-complete :null))
             (result (jf/gptel-scope-profile--normalize-keys input)))
        (expect (plist-get result :enforce-parse-complete) :to-be nil)))

    (it "normalizes booleans in nested plists"
      (let* ((input '(:security (:enforce-parse-complete :false
                                 :max-coverage-threshold 0.8)))
             (result (jf/gptel-scope-profile--normalize-keys input))
             (security (plist-get result :security)))
        (expect (plist-get security :enforce-parse-complete) :to-be nil)
        (expect (plist-get security :max-coverage-threshold) :to-equal 0.8)))

    (it "normalizes multiple boolean keywords in same plist"
      (let* ((input '(:enforce-parse-complete :true
                      :auth-enabled :false
                      :max-threshold 0.9))
             (result (jf/gptel-scope-profile--normalize-keys input)))
        (expect (plist-get result :enforce-parse-complete) :to-be t)
        (expect (plist-get result :auth-enabled) :to-be nil)
        (expect (plist-get result :max-threshold) :to-equal 0.9)))

    (it "normalizes booleans while converting snake_case to kebab-case"
      (let* ((input '(:enforce_parse_complete :true))
             (result (jf/gptel-scope-profile--normalize-keys input)))
        (expect (plist-get result :enforce-parse-complete) :to-be t)))

    (it "preserves already-normalized booleans"
      (let* ((input '(:enforce-parse-complete t
                      :auth-enabled nil))
             (result (jf/gptel-scope-profile--normalize-keys input)))
        (expect (plist-get result :enforce-parse-complete) :to-be t)
        (expect (plist-get result :auth-enabled) :to-be nil))))

  (describe "jf/gptel-scope-profile--plist-to-yaml with keyword booleans"

    (it "writes :true as 'true' in YAML"
      (let* ((plist '(:enforce-parse-complete :true))
             (yaml (jf/gptel-scope-profile--plist-to-yaml plist 0)))
        (expect yaml :to-match "enforce_parse_complete: true")))

    (it "writes :false as 'false' in YAML"
      (let* ((plist '(:enforce-parse-complete :false))
             (yaml (jf/gptel-scope-profile--plist-to-yaml plist 0)))
        (expect yaml :to-match "enforce_parse_complete: false")))

    (it "writes :null as 'false' in YAML"
      (let* ((plist '(:enforce-parse-complete :null))
             (yaml (jf/gptel-scope-profile--plist-to-yaml plist 0)))
        (expect yaml :to-match "enforce_parse_complete: false")))

    (it "writes t as 'true' in YAML"
      (let* ((plist '(:enforce-parse-complete t))
             (yaml (jf/gptel-scope-profile--plist-to-yaml plist 0)))
        (expect yaml :to-match "enforce_parse_complete: true")))

    (it "writes nil boolean fields as 'false' in YAML"
      (let* ((plist '(:enforce-parse-complete nil))
             (yaml (jf/gptel-scope-profile--plist-to-yaml plist 0)))
        (expect yaml :to-match "enforce_parse_complete: false")))

    (it "handles nested plists with keyword booleans"
      (let* ((plist '(:security (:enforce-parse-complete :false
                                 :max-coverage-threshold 0.8)))
             (yaml (jf/gptel-scope-profile--plist-to-yaml plist 0)))
        (expect yaml :to-match "security:")
        (expect yaml :to-match "enforce_parse_complete: false")
        (expect yaml :to-match "max_coverage_threshold: 0.8"))))

  (describe "jf/gptel-scope--validate-security-config with keyword booleans"

    (it "accepts :true and normalizes to t"
      (let ((security-plist '(:enforce-parse-complete :true)))
        (expect (jf/gptel-scope--validate-security-config security-plist) :to-be t)))

    (it "accepts :false and normalizes to nil"
      (let ((security-plist '(:enforce-parse-complete :false)))
        (expect (jf/gptel-scope--validate-security-config security-plist) :to-be t)))

    (it "accepts :null and normalizes to nil"
      (let ((security-plist '(:enforce-parse-complete :null)))
        (expect (jf/gptel-scope--validate-security-config security-plist) :to-be t)))

    (it "still accepts already-normalized t"
      (let ((security-plist '(:enforce-parse-complete t)))
        (expect (jf/gptel-scope--validate-security-config security-plist) :to-be t)))

    (it "still accepts already-normalized nil"
      (let ((security-plist '(:enforce-parse-complete nil)))
        (expect (jf/gptel-scope--validate-security-config security-plist) :to-be t)))

    (it "rejects invalid keyword values"
      (let ((security-plist '(:enforce-parse-complete :invalid)))
        (expect (jf/gptel-scope--validate-security-config security-plist) :to-throw)))

    (it "rejects non-boolean types"
      (let ((security-plist '(:enforce-parse-complete "true")))
        (expect (jf/gptel-scope--validate-security-config security-plist) :to-throw))))

  (describe "YAML round-trip with boolean values"

    (it "preserves YAML true through parse → normalize → write"
      (let* ((yaml-content "security:\n  enforce_parse_complete: true")
             (parsed (yaml-parse-string yaml-content :object-type 'plist))
             (normalized (jf/gptel-scope-profile--normalize-keys parsed))
             (security (plist-get normalized :security))
             (value (plist-get security :enforce-parse-complete))
             (written-yaml (jf/gptel-scope-profile--plist-to-yaml normalized 0)))
        ;; Value should be normalized to t
        (expect value :to-be t)
        ;; Written YAML should have 'true'
        (expect written-yaml :to-match "enforce_parse_complete: true")))

    (it "preserves YAML false through parse → normalize → write"
      (let* ((yaml-content "security:\n  enforce_parse_complete: false")
             (parsed (yaml-parse-string yaml-content :object-type 'plist))
             (normalized (jf/gptel-scope-profile--normalize-keys parsed))
             (security (plist-get normalized :security))
             (value (plist-get security :enforce-parse-complete))
             (written-yaml (jf/gptel-scope-profile--plist-to-yaml normalized 0)))
        ;; Value should be normalized to nil
        (expect value :to-be nil)
        ;; Written YAML should have 'false'
        (expect written-yaml :to-match "enforce_parse_complete: false")))

    (it "handles complete security section round-trip"
      (let* ((yaml-content "security:\n  enforce_parse_complete: false\n  max_coverage_threshold: 0.9")
             (parsed (yaml-parse-string yaml-content :object-type 'plist))
             (normalized (jf/gptel-scope-profile--normalize-keys parsed))
             (security (plist-get normalized :security)))
        ;; Validate the normalized values
        (expect (plist-get security :enforce-parse-complete) :to-be nil)
        (expect (plist-get security :max-coverage-threshold) :to-equal 0.9)
        ;; Validate passes
        (expect (jf/gptel-scope--validate-security-config security) :to-be t)))))

(provide 'yaml-boolean-normalization-spec)
;;; yaml-boolean-normalization-spec.el ends here
