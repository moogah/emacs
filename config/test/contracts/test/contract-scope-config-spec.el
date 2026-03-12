;;; contract-scope-config-spec.el --- Tests for scope-config contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests that scope-config contract validators correctly accept valid data
;; and reject invalid data.

;;; Code:

(require 'buttercup)

;; Add contracts directory to load-path
(let ((contracts-dir (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path contracts-dir))

(require 'contract-core)
(require 'contract-scope-config)

;; Register matcher after buttercup is loaded
(contract--register-buttercup-matcher)

(describe "contract/scope-paths--validate"
  (it "accepts valid complete paths"
    (expect (contract/scope-paths--validate
             '(:read ("/home/user/src/**") :write ("/home/user/out/**")
               :execute () :modify () :deny ("/etc/**")))
            :to-be nil))

  (it "accepts paths with all empty lists (schema defaults)"
    (expect (contract/scope-paths--validate
             '(:read () :write () :execute () :modify () :deny ()))
            :to-be nil))

  (it "accepts paths with multiple entries per key"
    (expect (contract/scope-paths--validate
             '(:read ("/tmp/**" "/var/log/**") :write ("/tmp/**")
               :execute ("/usr/bin/**") :modify ("/home/**") :deny ("/etc/**" "/root/**")))
            :to-be nil))

  (it "rejects missing :read key"
    (expect (contract/scope-paths--validate
             '(:write () :execute () :modify () :deny ()))
            :to-match "Missing required key :read"))

  (it "rejects missing :write key"
    (expect (contract/scope-paths--validate
             '(:read () :execute () :modify () :deny ()))
            :to-match "Missing required key :write"))

  (it "rejects missing :execute key"
    (expect (contract/scope-paths--validate
             '(:read () :write () :modify () :deny ()))
            :to-match "Missing required key :execute"))

  (it "rejects missing :modify key"
    (expect (contract/scope-paths--validate
             '(:read () :write () :execute () :deny ()))
            :to-match "Missing required key :modify"))

  (it "rejects missing :deny key"
    (expect (contract/scope-paths--validate
             '(:read () :write () :execute () :modify ()))
            :to-match "Missing required key :deny"))

  (it "rejects non-list value for a path key"
    (expect (contract/scope-paths--validate
             '(:read "/tmp/**" :write () :execute () :modify () :deny ()))
            :to-match ":read must be a list"))

  (it "rejects non-string entry in a path list"
    (expect (contract/scope-paths--validate
             '(:read (:keyword) :write () :execute () :modify () :deny ()))
            :to-match ":read\\[0\\] must be string")))

(describe "contract/scope-cloud--validate"
  (it "accepts valid cloud with auth-detection allow"
    (expect (contract/scope-cloud--validate
             '(:auth-detection "allow"))
            :to-be nil))

  (it "accepts valid cloud with auth-detection warn"
    (expect (contract/scope-cloud--validate
             '(:auth-detection "warn"))
            :to-be nil))

  (it "accepts valid cloud with auth-detection deny"
    (expect (contract/scope-cloud--validate
             '(:auth-detection "deny"))
            :to-be nil))

  (it "accepts valid cloud with allowed-providers"
    (expect (contract/scope-cloud--validate
             '(:auth-detection "warn" :allowed-providers (:aws-cli :gcloud)))
            :to-be nil))

  (it "accepts cloud with nil allowed-providers"
    (expect (contract/scope-cloud--validate
             '(:auth-detection "warn" :allowed-providers nil))
            :to-be nil))

  (it "rejects missing :auth-detection"
    (expect (contract/scope-cloud--validate
             '(:allowed-providers (:aws-cli)))
            :to-match "Missing required key :auth-detection"))

  (it "rejects invalid auth-detection value"
    (expect (contract/scope-cloud--validate
             '(:auth-detection "block"))
            :to-match ":auth-detection.*not in valid set"))

  (it "rejects non-string auth-detection (keyword instead of string)"
    (expect (contract/scope-cloud--validate
             '(:auth-detection :warn))
            :to-match ":auth-detection.*not in valid set"))

  (it "rejects invalid provider in allowed-providers"
    (expect (contract/scope-cloud--validate
             '(:auth-detection "warn" :allowed-providers (:heroku)))
            :to-match ":allowed-providers\\[0\\].*not in valid set")))

(describe "contract/scope-security--validate"
  (it "accepts valid complete security config"
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete t :max-coverage-threshold 0.8))
            :to-be nil))

  (it "accepts security with enforce-parse-complete nil"
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete nil :max-coverage-threshold 0.5))
            :to-be nil))

  (it "accepts security with threshold at boundaries"
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete t :max-coverage-threshold 0.0))
            :to-be nil)
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete t :max-coverage-threshold 1.0))
            :to-be nil))

  (it "rejects :true instead of t (YAML boolean bug)"
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete :true :max-coverage-threshold 0.8))
            :to-match ":enforce-parse-complete must be t or nil.*YAML boolean bug"))

  (it "rejects :false instead of nil (YAML boolean bug)"
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete :false :max-coverage-threshold 0.8))
            :to-match ":enforce-parse-complete must be t or nil.*YAML boolean bug"))

  (it "rejects missing :enforce-parse-complete"
    (expect (contract/scope-security--validate
             '(:max-coverage-threshold 0.8))
            :to-match "Missing required key :enforce-parse-complete"))

  (it "rejects missing :max-coverage-threshold"
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete t))
            :to-match "Missing required key :max-coverage-threshold"))

  (it "rejects threshold above 1.0"
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete t :max-coverage-threshold 1.5))
            :to-match ":max-coverage-threshold must be between 0.0 and 1.0"))

  (it "rejects threshold below 0.0"
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete t :max-coverage-threshold -0.1))
            :to-match ":max-coverage-threshold must be between 0.0 and 1.0"))

  (it "rejects non-number threshold"
    (expect (contract/scope-security--validate
             '(:enforce-parse-complete t :max-coverage-threshold "high"))
            :to-match ":max-coverage-threshold must be a number")))

(describe "contract/scope-bash-tools--validate"
  (it "accepts valid deny list"
    (expect (contract/scope-bash-tools--validate
             '(:deny ("rm" "dd" "mkfs")))
            :to-be nil))

  (it "accepts empty deny list"
    (expect (contract/scope-bash-tools--validate
             '(:deny ()))
            :to-be nil))

  (it "accepts nil (section not present)"
    (expect (contract/scope-bash-tools--validate nil)
            :to-be nil))

  (it "rejects :categories key (migration error)"
    (expect (contract/scope-bash-tools--validate
             '(:categories ("destructive") :deny ("rm")))
            :to-match "Contains :categories key.*migration required"))

  (it "rejects non-string entry in deny list"
    (expect (contract/scope-bash-tools--validate
             '(:deny (:rm "dd")))
            :to-match ":deny\\[0\\] must be string"))

  (it "rejects non-list deny value"
    (expect (contract/scope-bash-tools--validate
             '(:deny "rm"))
            :to-match ":deny must be a list")))

(describe "contract/scope-config--validate"
  (it "accepts valid complete config"
    (expect (contract/scope-config--validate
             '(:paths (:read () :write () :execute () :modify () :deny ())
               :cloud (:auth-detection "warn")
               :security (:enforce-parse-complete t :max-coverage-threshold 0.8)
               :bash-tools (:deny ("rm"))))
            :to-be nil))

  (it "accepts config without bash-tools"
    (expect (contract/scope-config--validate
             '(:paths (:read () :write () :execute () :modify () :deny ())
               :cloud (:auth-detection "warn")
               :security (:enforce-parse-complete t :max-coverage-threshold 0.8)))
            :to-be nil))

  (it "accepts config with nil bash-tools"
    (expect (contract/scope-config--validate
             '(:paths (:read () :write () :execute () :modify () :deny ())
               :cloud (:auth-detection "warn")
               :security (:enforce-parse-complete t :max-coverage-threshold 0.8)
               :bash-tools nil))
            :to-be nil))

  (it "rejects missing :paths"
    (expect (contract/scope-config--validate
             '(:cloud (:auth-detection "warn")
               :security (:enforce-parse-complete t :max-coverage-threshold 0.8)))
            :to-match "Missing required key :paths"))

  (it "rejects missing :cloud"
    (expect (contract/scope-config--validate
             '(:paths (:read () :write () :execute () :modify () :deny ())
               :security (:enforce-parse-complete t :max-coverage-threshold 0.8)))
            :to-match "Missing required key :cloud"))

  (it "rejects missing :security"
    (expect (contract/scope-config--validate
             '(:paths (:read () :write () :execute () :modify () :deny ())
               :cloud (:auth-detection "warn")))
            :to-match "Missing required key :security"))

  (it "surfaces :paths sub-validator errors"
    (expect (contract/scope-config--validate
             '(:paths (:read () :write () :execute () :deny ())
               :cloud (:auth-detection "warn")
               :security (:enforce-parse-complete t :max-coverage-threshold 0.8)))
            :to-match ":paths:.*Missing required key :modify"))

  (it "surfaces :cloud sub-validator errors"
    (expect (contract/scope-config--validate
             '(:paths (:read () :write () :execute () :modify () :deny ())
               :cloud (:auth-detection "block")
               :security (:enforce-parse-complete t :max-coverage-threshold 0.8)))
            :to-match ":cloud:.*:auth-detection.*not in valid set"))

  (it "surfaces :security sub-validator errors"
    (expect (contract/scope-config--validate
             '(:paths (:read () :write () :execute () :modify () :deny ())
               :cloud (:auth-detection "warn")
               :security (:enforce-parse-complete :true :max-coverage-threshold 0.8)))
            :to-match ":security:.*:enforce-parse-complete must be t or nil"))

  (it "surfaces :bash-tools sub-validator errors"
    (expect (contract/scope-config--validate
             '(:paths (:read () :write () :execute () :modify () :deny ())
               :cloud (:auth-detection "warn")
               :security (:enforce-parse-complete t :max-coverage-threshold 0.8)
               :bash-tools (:categories ("destructive"))))
            :to-match ":bash-tools:.*:categories.*migration required")))

(describe ":to-satisfy-contract with scope-config contracts"
  (it "works with paths contract"
    (let ((paths '(:read () :write () :execute () :modify () :deny ())))
      (expect paths :to-satisfy-contract #'contract/scope-paths--validate)))

  (it "works with cloud contract"
    (let ((cloud '(:auth-detection "warn" :allowed-providers (:aws-cli))))
      (expect cloud :to-satisfy-contract #'contract/scope-cloud--validate)))

  (it "works with security contract"
    (let ((security '(:enforce-parse-complete t :max-coverage-threshold 0.8)))
      (expect security :to-satisfy-contract #'contract/scope-security--validate)))

  (it "works with top-level config contract"
    (let ((config '(:paths (:read () :write () :execute () :modify () :deny ())
                    :cloud (:auth-detection "warn")
                    :security (:enforce-parse-complete t :max-coverage-threshold 0.8))))
      (expect config :to-satisfy-contract #'contract/scope-config--validate))))

(provide 'contract-scope-config-spec)

;;; contract-scope-config-spec.el ends here
