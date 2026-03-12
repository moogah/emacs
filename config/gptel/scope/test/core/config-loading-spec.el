;;; config-loading-spec.el --- Tests for scope.yml schema loading and config management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CONSOLIDATED TESTS: Scope schema loading, parsing, and configuration management
;;
;; Consolidates tests from:
;; - tools/test/integration/test-schema.el (62 ERT tests -> Buttercup)
;; - tools/test/unit/config-spec.el (12 Buttercup tests)
;;
;; Test organization:
;; 1. Schema loading with defaults (load-schema, load-cloud-config, load-security-config)
;; 2. Operation-specific path sections
;; 3. Write scope includes read capability
;; 4. Cloud authentication configuration
;; 5. Security configuration
;; 6. Bash tools section
;; 7. YAML parsing and normalization
;; 8. Backward compatibility
;; 9. Validation on schema load
;; 10. Default merging behavior
;; 11. Edge cases and rejection tests
;; 12. Pipeline command extraction

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'yaml)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-dir (expand-file-name "../.." test-dir)))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir))
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir)))

;;; Helper Functions

(defun test-config--normalize-vectors (obj)
  "Recursively convert vectors to lists in OBJ."
  (cond
   ((vectorp obj)
    (mapcar #'test-config--normalize-vectors (append obj nil)))
   ((and (listp obj) (keywordp (car-safe obj)))
    (let ((result nil))
      (cl-loop for (key val) on obj by #'cddr
               do (setq result (plist-put result key
                                         (test-config--normalize-vectors val))))
      result))
   ((listp obj)
    (mapcar #'test-config--normalize-vectors obj))
   (t obj)))

(defun test-config--parse-yml (yml-content)
  "Parse YML-CONTENT string using production code path.
Returns normalized plist with vectors converted to lists."
  (let* ((parsed (yaml-parse-string yml-content :object-type 'plist))
         (loaded (jf/gptel-scope--load-schema parsed))
         (vectors-fixed (test-config--normalize-vectors loaded)))
    vectors-fixed))

;;; Schema Loading Unit Tests (from config-spec.el)

(describe "jf/gptel-scope--load-schema"

  (it "merges provided paths with defaults"
    (let* ((schema-plist '(:paths (:read ("/workspace/**"))))
           (result (jf/gptel-scope--load-schema schema-plist))
           (paths (plist-get result :paths)))
      (expect (plist-get paths :read) :to-equal '("/workspace/**"))
      (expect (plist-get paths :write) :to-equal '())
      (expect (plist-get paths :deny) :to-equal '())))

  (it "uses defaults when sections missing"
    (let* ((schema-plist '(:paths nil))
           (result (jf/gptel-scope--load-schema schema-plist))
           (cloud (plist-get result :cloud))
           (security (plist-get result :security)))
      (expect (plist-get cloud :auth-detection) :to-equal "warn")
      (expect (plist-get security :enforce-parse-complete) :to-be t)
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.8)))

  (it "preserves bash-tools deny list with normalized keys"
    (let* ((bash-tools '(:deny ("rm" "sudo")))
           (schema-plist (list :bash-tools bash-tools))
           (result (jf/gptel-scope--load-schema schema-plist))
           (result-bash-tools (plist-get result :bash-tools)))
      (expect (plist-get result-bash-tools :deny) :to-equal '("rm" "sudo")))))

;;; Cloud Config Loading Tests (from config-spec.el)

(describe "jf/gptel-scope--load-cloud-config"

  (it "loads auth-detection setting"
    (let* ((cloud-plist '(:auth-detection "deny"))
           (result (jf/gptel-scope--load-cloud-config cloud-plist)))
      (expect (plist-get result :auth-detection) :to-equal "deny")))

  (it "returns nil for nil config"
    (let ((result (jf/gptel-scope--load-cloud-config nil)))
      (expect result :to-be nil))))

;;; Security Config Loading Tests (from config-spec.el)

(describe "jf/gptel-scope--load-security-config"

  (it "loads both security settings"
    (let* ((security-plist '(:enforce-parse-complete nil
                             :max-coverage-threshold 0.9))
           (result (jf/gptel-scope--load-security-config security-plist)))
      (expect (plist-get result :enforce-parse-complete) :to-be nil)
      (expect (plist-get result :max-coverage-threshold) :to-equal 0.9)))

  (it "returns nil for nil config"
    (let ((result (jf/gptel-scope--load-security-config nil)))
      (expect result :to-be nil))))

;;; Operation-Specific Path Sections (from test-schema.el)

(describe "scope schema path sections"

  (it "loads paths.read section"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\", \"/home/**\"]"))
           (read-paths (plist-get (plist-get config :paths) :read)))
      (expect (length read-paths) :to-equal 2)
      (expect read-paths :to-contain "/workspace/**")
      (expect read-paths :to-contain "/home/**")))

  (it "loads paths.write section"
    (let* ((config (test-config--parse-yml "paths:\n  write: [\"/workspace/project/**\"]"))
           (write-paths (plist-get (plist-get config :paths) :write)))
      (expect (length write-paths) :to-equal 1)
      (expect (car write-paths) :to-equal "/workspace/project/**")))

  (it "loads paths.execute section"
    (let* ((config (test-config--parse-yml "paths:\n  execute: [\"/workspace/scripts/**\", \"/usr/local/bin/**\"]"))
           (execute-paths (plist-get (plist-get config :paths) :execute)))
      (expect (length execute-paths) :to-equal 2)
      (expect execute-paths :to-contain "/workspace/scripts/**")))

  (it "loads paths.modify section"
    (let* ((config (test-config--parse-yml "paths:\n  modify: [\"/workspace/config/**\"]"))
           (modify-paths (plist-get (plist-get config :paths) :modify)))
      (expect (length modify-paths) :to-equal 1)
      (expect (car modify-paths) :to-equal "/workspace/config/**")))

  (it "loads paths.deny section"
    (let* ((config (test-config--parse-yml "paths:\n  deny: [\"**/.git/**\", \"**/runtime/**\", \"**/.env\"]"))
           (deny-paths (plist-get (plist-get config :paths) :deny)))
      (expect (length deny-paths) :to-equal 3)
      (expect deny-paths :to-contain "**/.git/**")
      (expect deny-paths :to-contain "**/.env"))))

;;; Write Scope Includes Read Capability (from test-schema.el)

(describe "scope schema write/read hierarchy"

  (it "write path section loads separately from read"
    (let* ((config (test-config--parse-yml "paths:\n  write: [\"/workspace/**\"]"))
           (paths (plist-get config :paths)))
      (expect (plist-get paths :write) :not :to-be nil)))

  (it "read path does not imply write"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]"))
           (paths (plist-get config :paths)))
      (expect (plist-get paths :read) :not :to-be nil)
      (expect (plist-get paths :write) :to-equal nil))))

;;; Cloud Authentication Configuration (from test-schema.el)

(describe "scope schema cloud config"

  (it "loads auth_detection field"
    (let* ((config (test-config--parse-yml "cloud:\n  auth_detection: \"warn\""))
           (cloud (plist-get config :cloud)))
      (expect (plist-get cloud :auth-detection) :to-equal "warn")))

  (it "loads allowed_providers field"
    (let* ((config (test-config--parse-yml "cloud:\n  allowed_providers: [\"aws\", \"gcp\", \"azure\"]"))
           (cloud (plist-get config :cloud))
           (providers (plist-get cloud :allowed-providers)))
      (expect (length providers) :to-equal 3)
      (expect providers :to-contain "aws")
      (expect providers :to-contain "gcp")
      (expect providers :to-contain "azure")))

  (it "adds defaults when cloud section missing"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]"))
           (cloud (plist-get config :cloud)))
      (expect cloud :not :to-be nil)
      (expect (plist-get cloud :auth-detection) :to-equal "warn"))))

;;; Security Configuration (from test-schema.el)

(describe "scope schema security config"

  (it "loads enforce_parse_complete field"
    (let* ((config (test-config--parse-yml "security:\n  enforce_parse_complete: true"))
           (security (plist-get config :security)))
      (expect (plist-get security :enforce-parse-complete) :to-be t)))

  (it "loads max_coverage_threshold field"
    (let* ((config (test-config--parse-yml "security:\n  max_coverage_threshold: 0.8"))
           (threshold (plist-get (plist-get config :security) :max-coverage-threshold)))
      (expect threshold :to-equal 0.8)))

  (it "adds defaults when security section missing"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]"))
           (security (plist-get config :security)))
      (expect security :not :to-be nil)
      (expect (plist-get security :enforce-parse-complete) :to-be t)
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.8))))

;;; Bash Tools Section (from test-schema.el)

(describe "scope schema bash_tools"

  (it "rejects categories section"
    (let ((yml "bash_tools:\n  categories:\n    read_only:\n      commands: [\"ls\", \"cat\", \"grep\"]\n    safe_write:\n      commands: [\"mkdir\", \"touch\"]\n    dangerous:\n      commands: []"))
      (expect (test-config--parse-yml yml) :to-throw)))

  (it "loads deny list"
    (let* ((config (test-config--parse-yml "bash_tools:\n  deny: [\"rm\", \"sudo\", \"chmod\"]"))
           (deny (plist-get (plist-get config :bash-tools) :deny)))
      (expect (length deny) :to-equal 3)
      (expect deny :to-contain "rm")
      (expect deny :to-contain "sudo"))))

;;; Full Document Structure (from test-schema.el)

(describe "scope schema full document"

  (it "loads all sections from complete document"
    (let* ((yml "paths:\n  read: [\"/**\"]\n  write: [\"/workspace/**\"]\n  execute: [\"/workspace/scripts/**\"]\n  modify: [\"/workspace/config/**\"]\n  deny: [\"**/.git/**\", \"**/.env\"]\n\nbash_tools:\n  deny: [\"rm\", \"sudo\"]\n\ncloud:\n  auth_detection: \"warn\"\n  allowed_providers: [\"aws\"]\n\nsecurity:\n  enforce_parse_complete: true\n  max_coverage_threshold: 0.8")
           (config (test-config--parse-yml yml)))
      (expect (plist-get config :paths) :not :to-be nil)
      (expect (plist-get config :bash-tools) :not :to-be nil)
      (expect (plist-get config :cloud) :not :to-be nil)
      (expect (plist-get config :security) :not :to-be nil)
      ;; Verify paths subsections
      (let ((paths (plist-get config :paths)))
        (expect (plist-get paths :read) :not :to-be nil)
        (expect (plist-get paths :write) :not :to-be nil)
        (expect (plist-get paths :execute) :not :to-be nil)
        (expect (plist-get paths :modify) :not :to-be nil)
        (expect (plist-get paths :deny) :not :to-be nil))
      ;; Verify bash_tools has no categories
      (expect (plist-get (plist-get config :bash-tools) :categories) :to-be nil))))

;;; YAML Parsing and Normalization (from test-schema.el)

(describe "scope schema YAML normalization"

  (it "handles snake_case keys"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]\ncloud:\n  auth_detection: \"warn\""))
           (cloud (plist-get config :cloud)))
      (expect (plist-get cloud :auth-detection) :to-equal "warn")))

  (it "normalizes all keys to kebab-case"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]\n  write: [\"/tmp/**\"]\n  execute: [\"/usr/bin/**\"]\n  modify: [\"/etc/**\"]"))
           (paths (plist-get config :paths)))
      (expect (plist-get paths :read) :not :to-be nil)
      (expect (plist-get paths :write) :not :to-be nil)
      (expect (plist-get paths :execute) :not :to-be nil)
      (expect (plist-get paths :modify) :not :to-be nil)))

  (it "normalizes nested structures"
    (let* ((config (test-config--parse-yml "cloud:\n  auth_detection: \"warn\"\n  allowed_providers: [\"aws\"]"))
           (cloud (plist-get config :cloud)))
      (expect (plist-get cloud :auth-detection) :not :to-be nil)
      (expect (plist-get cloud :allowed-providers) :not :to-be nil))))

;;; Backward Compatibility (from test-schema.el)

(describe "scope schema backward compatibility"

  (it "legacy document with only read/write does not auto-add execute/modify"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]\n  write: [\"/tmp/**\"]"))
           (paths (plist-get config :paths)))
      (expect (plist-get paths :read) :not :to-be nil)
      (expect (plist-get paths :write) :not :to-be nil)
      (expect (plist-get paths :execute) :to-be nil)
      (expect (plist-get paths :modify) :to-be nil)))

  (it "no automatic migration from legacy schemas"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]\n  write: [\"/tmp/**\"]\nbash_tools:\n  deny: [\"rm\"]"))
           (paths (plist-get config :paths)))
      (expect (plist-get paths :execute) :to-be nil)
      (expect (plist-get paths :modify) :to-be nil)
      ;; But defaults for cloud/security are added
      (expect (plist-get config :cloud) :not :to-be nil)
      (expect (plist-get config :security) :not :to-be nil))))

;;; Validation on Schema Load (from test-schema.el)

(describe "scope schema validation"

  (it "rejects invalid auth_detection value"
    (expect (test-config--parse-yml "cloud:\n  auth_detection: \"invalid-value\"") :to-throw))

  (it "rejects invalid coverage threshold > 1.0"
    (expect (test-config--parse-yml "security:\n  max_coverage_threshold: 1.5") :to-throw))

  (it "loads invalid path structure as-is"
    (let* ((config (test-config--parse-yml "paths:\n  read: \"not-an-array\""))
           (read-paths (plist-get (plist-get config :paths) :read)))
      (expect (stringp read-paths) :to-be t)))

  (it "loads valid document successfully"
    (let* ((yml "paths:\n  read: [\"/**\"]\n  write: [\"/workspace/**\"]\n  execute: [\"/workspace/scripts/**\"]\n  modify: [\"/workspace/config/**\"]\n  deny: [\"**/.git/**\"]\n\nbash_tools:\n  deny: [\"rm\", \"sudo\"]\n\ncloud:\n  auth_detection: \"warn\"\n  allowed_providers: [\"aws\", \"gcp\"]\n\nsecurity:\n  enforce_parse_complete: true\n  max_coverage_threshold: 0.8")
           (config (test-config--parse-yml yml)))
      (expect config :not :to-be nil)
      (let ((paths (plist-get config :paths)))
        (expect (listp (plist-get paths :read)) :to-be t)
        (expect (listp (plist-get paths :deny)) :to-be t)))))

;;; Edge Cases (from test-schema.el)

(describe "scope schema edge cases"

  (it "loads empty arrays correctly"
    (let* ((config (test-config--parse-yml "paths:\n  read: []\n  write: []\n  execute: []\n  modify: []"))
           (paths (plist-get config :paths)))
      (expect (plist-get paths :read) :to-equal nil)
      (expect (plist-get paths :write) :to-equal nil)))

  (it "handles mixed YAML quote styles"
    (let* ((config (test-config--parse-yml "cloud:\n  auth_detection: 'warn'\n  allowed_providers: [\"aws\", 'gcp', azure]"))
           (cloud (plist-get config :cloud)))
      (expect (plist-get cloud :auth-detection) :to-equal "warn")
      (expect (length (plist-get cloud :allowed-providers)) :to-equal 3)))

  (it "normalizes YAML boolean false to elisp nil"
    (let* ((config (test-config--parse-yml "security:\n  enforce_parse_complete: false"))
           (security (plist-get config :security)))
      (expect (plist-get security :enforce-parse-complete) :to-be nil)))

  (it "handles numeric threshold variations"
    (let* ((config (test-config--parse-yml "security:\n  max_coverage_threshold: 0.85"))
           (threshold (plist-get (plist-get config :security) :max-coverage-threshold)))
      (expect (numberp threshold) :to-be t)
      (expect threshold :to-equal 0.85)))

  (it "handles null cloud section with defaults"
    (let* ((config (test-config--parse-yml "cloud: null\npaths:\n  read: [\"/workspace/**\"]"))
           (cloud (plist-get config :cloud)))
      (expect cloud :not :to-be nil)
      (expect (plist-get cloud :auth-detection) :not :to-be nil)))

  (it "loads duplicate path patterns as-is"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\", \"/workspace/**\", \"/tmp/**\"]"))
           (read-paths (plist-get (plist-get config :paths) :read)))
      (expect (length read-paths) :to-equal 3)))

  (it "ignores unknown top-level keys"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]\nunknown_section:\n  foo: bar\ncustom_field: 123")))
      (expect (plist-get config :paths) :not :to-be nil)
      (expect (plist-get config :unknown-section) :to-be nil)
      (expect (plist-get config :custom-field) :to-be nil))))

;;; Validation Rejection Tests (from test-schema.el)

(describe "scope schema rejection"

  (it "rejects negative coverage threshold"
    (expect (test-config--parse-yml "security:\n  max_coverage_threshold: -0.5") :to-throw))

  (it "rejects coverage threshold above 1.0"
    (expect (test-config--parse-yml "security:\n  max_coverage_threshold: 2.0") :to-throw))

  (it "rejects non-numeric coverage threshold"
    (expect (test-config--parse-yml "security:\n  max_coverage_threshold: \"high\"") :to-throw))

  (it "rejects misspelled auth_detection"
    (expect (test-config--parse-yml "cloud:\n  auth_detection: \"warning\"") :to-throw))

  (it "rejects empty auth_detection"
    (expect (test-config--parse-yml "cloud:\n  auth_detection: \"\"") :to-throw))

  (it "rejects numeric auth_detection"
    (expect (test-config--parse-yml "cloud:\n  auth_detection: 123") :to-throw))

  (it "rejects non-boolean enforce_parse_complete"
    (expect (test-config--parse-yml "security:\n  enforce_parse_complete: \"yes\"") :to-throw))

  (it "rejects numeric enforce_parse_complete"
    (expect (test-config--parse-yml "security:\n  enforce_parse_complete: 1") :to-throw))

  (it "rejects extremely large threshold"
    (expect (test-config--parse-yml "security:\n  max_coverage_threshold: 999999") :to-throw))

  (it "handles malformed YAML syntax"
    (expect (yaml-parse-string "paths:\n  read: [unclosed bracket" :object-type 'plist) :to-throw))

  (it "loads malformed path object structure"
    (let* ((config (test-config--parse-yml "paths:\n  write: {\"foo\": \"bar\"}"))
           (write-paths (plist-get (plist-get config :paths) :write)))
      (expect write-paths :not :to-be nil)
      ;; Not a simple string list
      (expect (and (listp write-paths) (stringp (car-safe write-paths))) :not :to-be t)))

  (it "loads numeric path value"
    (let* ((config (test-config--parse-yml "paths:\n  execute: 123"))
           (execute-paths (plist-get (plist-get config :paths) :execute)))
      (expect (numberp execute-paths) :to-be t)))

  (it "loads mixed types in path array"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\", 123, \"/tmp/**\"]"))
           (read-paths (plist-get (plist-get config :paths) :read)))
      (expect (length read-paths) :to-equal 3)
      (expect (numberp (nth 1 read-paths)) :to-be t)))

  (it "loads invalid bash_tools structure"
    (let* ((config (test-config--parse-yml "bash_tools:\n  deny: \"rm\""))
           (deny (plist-get (plist-get config :bash-tools) :deny)))
      (expect (stringp deny) :to-be t)))

  (it "accepts boundary threshold values"
    (let* ((config-zero (test-config--parse-yml "security:\n  max_coverage_threshold: 0.0"))
           (config-one (test-config--parse-yml "security:\n  max_coverage_threshold: 1.0")))
      (expect (plist-get (plist-get config-zero :security) :max-coverage-threshold) :to-equal 0.0)
      (expect (plist-get (plist-get config-one :security) :max-coverage-threshold) :to-equal 1.0)))

  (it "accepts empty allowed_providers array"
    (let* ((config (test-config--parse-yml "cloud:\n  auth_detection: \"warn\"\n  allowed_providers: []"))
           (providers (plist-get (plist-get config :cloud) :allowed-providers)))
      (expect (or (null providers) (and (listp providers) (= 0 (length providers)))) :to-be t)))

  (it "handles scientific notation threshold"
    (let* ((config (test-config--parse-yml "security:\n  max_coverage_threshold: 1e-5"))
           (threshold (plist-get (plist-get config :security) :max-coverage-threshold)))
      (expect (numberp threshold) :to-be t)
      (expect (and (>= threshold 0.0) (<= threshold 1.0)) :to-be t))))

;;; Default Merging Tests (from test-schema.el)

(describe "scope schema default merging"

  (it "empty schema gets all defaults"
    (let* ((config (test-config--parse-yml "paths:\n  read: []"))
           (paths (plist-get config :paths))
           (cloud (plist-get config :cloud))
           (security (plist-get config :security)))
      (expect (plist-get paths :read) :to-equal nil)
      (expect (plist-get cloud :auth-detection) :to-equal "warn")
      (expect (plist-get security :enforce-parse-complete) :to-be t)
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.8)))

  (it "partial paths section merges with defaults"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]\n  write: [\"/output/**\"]"))
           (paths (plist-get config :paths)))
      (expect (plist-get paths :read) :to-equal '("/workspace/**"))
      (expect (plist-get paths :write) :to-equal '("/output/**"))
      (expect (plist-get paths :execute) :to-equal nil)
      (expect (plist-get paths :modify) :to-equal nil)))

  (it "partial cloud section merges with defaults"
    (let* ((config (test-config--parse-yml "cloud:\n  allowed_providers: [\"aws\", \"gcp\"]"))
           (cloud (plist-get config :cloud)))
      (expect (plist-get cloud :allowed-providers) :to-equal '("aws" "gcp"))
      (expect (plist-get cloud :auth-detection) :to-equal "warn")))

  (it "partial security section merges with defaults"
    (let* ((config (test-config--parse-yml "security:\n  max_coverage_threshold: 0.9"))
           (security (plist-get config :security)))
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.9)
      (expect (plist-get security :enforce-parse-complete) :to-be t)))

  (it "user-specified cloud auth overrides default"
    (let* ((config (test-config--parse-yml "cloud:\n  auth_detection: \"deny\""))
           (cloud (plist-get config :cloud)))
      (expect (plist-get cloud :auth-detection) :to-equal "deny")))

  (it "user-specified security threshold overrides default"
    (let* ((config (test-config--parse-yml "security:\n  max_coverage_threshold: 0.5"))
           (security (plist-get config :security)))
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.5)
      (expect (plist-get security :enforce-parse-complete) :to-be t)))

  (it "mixed present/absent sections merge correctly"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/workspace/**\"]\n  write: [\"/output/**\"]\n\nsecurity:\n  enforce_parse_complete: true"))
           (paths (plist-get config :paths))
           (cloud (plist-get config :cloud))
           (security (plist-get config :security)))
      (expect (plist-get paths :read) :to-equal '("/workspace/**"))
      (expect (plist-get cloud :auth-detection) :to-equal "warn")
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.8)))

  (it "all sections can be overridden"
    (let* ((config (test-config--parse-yml "paths:\n  read: [\"/custom/**\"]\n  write: [\"/custom/**\"]\n  execute: [\"/scripts/**\"]\n  modify: [\"/config/**\"]\n  deny: [\"**/.git/**\"]\n\ncloud:\n  auth_detection: \"allow\"\n  allowed_providers: [\"azure\"]\n\nsecurity:\n  enforce_parse_complete: true\n  max_coverage_threshold: 0.95"))
           (paths (plist-get config :paths))
           (cloud (plist-get config :cloud))
           (security (plist-get config :security)))
      (expect (plist-get paths :read) :to-equal '("/custom/**"))
      (expect (plist-get cloud :auth-detection) :to-equal "allow")
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.95)))

  (it "bash_tools not modified by default merging"
    (let* ((config (test-config--parse-yml "bash_tools:\n  deny: [\"rm\"]"))
           (bash-tools (plist-get config :bash-tools)))
      (expect (plist-get bash-tools :deny) :to-equal '("rm"))
      (expect (plist-get bash-tools :categories) :to-be nil)))

  (it "minimal plus bash_tools gets other defaults"
    (let* ((config (test-config--parse-yml "bash_tools:\n  deny: []")))
      (expect (plist-get config :paths) :not :to-be nil)
      (expect (plist-get config :cloud) :not :to-be nil)
      (expect (plist-get config :security) :not :to-be nil)))

  (it "zero threshold is preserved (not replaced by default)"
    (let* ((config (test-config--parse-yml "security:\n  max_coverage_threshold: 0.0"))
           (security (plist-get config :security)))
      (expect (plist-get security :max-coverage-threshold) :to-equal 0.0)
      (expect (plist-get security :enforce-parse-complete) :to-be t)))

  (it "1.0 threshold is preserved"
    (let* ((config (test-config--parse-yml "security:\n  max_coverage_threshold: 1.0"))
           (security (plist-get config :security)))
      (expect (plist-get security :max-coverage-threshold) :to-equal 1.0))))

;;; Pipeline Command Extraction (from config-spec.el)

(describe "jf/gptel-scope--extract-pipeline-commands"

  (it "extracts all commands from pipeline"
    (let* ((parsed-command '(:all-commands ((:command-name "ls")
                                            (:command-name "grep")
                                            (:command-name "head"))))
           (result (jf/gptel-scope--extract-pipeline-commands parsed-command)))
      (expect result :to-equal '("ls" "grep" "head"))))

  (it "returns empty list for no commands"
    (let* ((parsed-command '(:all-commands ()))
           (result (jf/gptel-scope--extract-pipeline-commands parsed-command)))
      (expect result :to-equal '())))

  (it "handles missing command-name gracefully"
    (let* ((parsed-command '(:all-commands ((:command-name "ls")
                                            (:other-field "value")
                                            (:command-name "grep"))))
           (result (jf/gptel-scope--extract-pipeline-commands parsed-command)))
      (expect result :to-equal '("ls" "grep")))))

(provide 'config-loading-spec)

;;; config-loading-spec.el ends here
