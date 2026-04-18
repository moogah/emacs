;;; contract-bash-parser-spec.el --- Tests for bash-parser contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests that bash-parser contract validators correctly accept valid data
;; and reject invalid data. Also validates contracts against real bash-parser
;; output to confirm they match actual producer shapes.

;;; Code:

(require 'buttercup)

;; Add contracts directory to load-path
(let ((contracts-dir (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path contracts-dir))

(require 'contract-core)
(require 'contract-bash-parser)

;; Register matcher after buttercup is loaded
(contract--register-buttercup-matcher)

(describe "contract/bash-file-op--validate"
  (it "accepts valid file operation"
    (expect (contract/bash-file-op--validate
             '(:file "/tmp/test.txt" :operation :read :confidence :high
               :source :positional-arg :command "cat"))
            :to-be nil))

  (it "accepts file-op with optional context flags"
    (expect (contract/bash-file-op--validate
             '(:file "*.txt" :operation :read :confidence :medium
               :source :loop-glob :command "ls"
               :pattern t :loop-context t))
            :to-be nil))

  (it "rejects missing :file key"
    (expect (contract/bash-file-op--validate
             '(:operation :read :confidence :high :source :positional-arg))
            :to-match "Missing required key :file"))

  (it "rejects missing :operation key"
    (expect (contract/bash-file-op--validate
             '(:file "/tmp/test.txt" :confidence :high :source :positional-arg))
            :to-match "Missing required key :operation"))

  (it "rejects missing :confidence key"
    (expect (contract/bash-file-op--validate
             '(:file "/tmp/test.txt" :operation :read :source :positional-arg))
            :to-match "Missing required key :confidence"))

  (it "accepts file-op without :source (optional — command handlers may omit it)"
    (expect (contract/bash-file-op--validate
             '(:file "/tmp/test.txt" :operation :read :confidence :high))
            :to-be nil))

  (it "rejects invalid :source value when present"
    (expect (contract/bash-file-op--validate
             '(:file "/tmp/test.txt" :operation :read :confidence :high
               :source :invented))
            :to-match ":source.*not in valid set"))

  (it "rejects invalid operation type"
    (expect (contract/bash-file-op--validate
             '(:file "/tmp/test.txt" :operation :frobnicate :confidence :high
               :source :positional-arg))
            :to-match ":operation.*not in valid set"))

  (it "rejects invalid confidence level"
    (expect (contract/bash-file-op--validate
             '(:file "/tmp/test.txt" :operation :read :confidence :absolute
               :source :positional-arg))
            :to-match ":confidence.*not in valid set"))

  (it "rejects :path key (old mock format - should be :file)"
    (expect (contract/bash-file-op--validate
             '(:path "/tmp/test.txt" :operation :read :confidence :high
               :source :positional-arg))
            :to-match "Missing required key :file")))

(describe "contract/bash-cloud-auth-op--validate"
  (it "accepts valid cloud auth operation"
    (expect (contract/bash-cloud-auth-op--validate
             '(:operation :authenticate :provider :aws-cli
               :command "aws s3 ls"
               :context (:profile "default" :region "us-east-1")))
            :to-be nil))

  (it "accepts minimal cloud auth (no context)"
    (expect (contract/bash-cloud-auth-op--validate
             '(:operation :authenticate :provider :gcloud :command "gcloud compute list"))
            :to-be nil))

  (it "rejects missing :provider"
    (expect (contract/bash-cloud-auth-op--validate
             '(:operation :authenticate :command "aws s3 ls"))
            :to-match "Missing required key :provider"))

  (it "rejects invalid provider"
    (expect (contract/bash-cloud-auth-op--validate
             '(:operation :authenticate :provider :heroku :command "heroku ps"))
            :to-match ":provider.*not in valid set"))

  (it "rejects wrong :operation"
    (expect (contract/bash-cloud-auth-op--validate
             '(:operation :read :provider :aws-cli))
            :to-match ":operation must be :authenticate"))

  (it "accepts command-handler format (no :operation, :aws provider)"
    (expect (contract/bash-cloud-auth-op--validate
             '(:provider :aws :command "aws"))
            :to-be nil)))

(describe "contract/bash-domains--validate"
  (it "accepts valid domains alist"
    (expect (contract/bash-domains--validate
             '((:filesystem . ((:file "f.txt" :operation :read
                                :confidence :high :source :positional-arg)))))
            :to-be nil))

  (it "accepts empty domains"
    (expect (contract/bash-domains--validate nil)
            :to-be nil))

  (it "catches plist format (the original bug class)"
    ;; This is the exact bug: (:filesystem ops) is a plist, not an alist
    (expect (contract/bash-domains--validate
             '(:filesystem ((:file "f.txt" :operation :read))))
            :to-match "not a cons cell"))

  (it "rejects :cloud-auth domain key (should be :authentication)"
    (expect (contract/bash-domains--validate
             '((:filesystem . nil) (:cloud-auth . nil)))
            :to-match "Unknown alist key :cloud-auth"))

  (it "accepts :authentication domain"
    (expect (contract/bash-domains--validate
             '((:authentication . ((:operation :authenticate
                                    :provider :aws-cli)))))
            :to-be nil)))

(describe "contract/bash-semantics--validate"
  (it "accepts valid semantics with file ops"
    (let ((semantics
           (list :domains (list (cons :filesystem
                                     '((:file "/tmp/f.txt" :operation :read
                                        :confidence :high :source :positional-arg
                                        :command "cat"))))
                 :coverage '(:total-tokens 3 :claimed-tokens 3 :coverage-percent 100)
                 :parse-complete t)))
      (expect (contract/bash-semantics--validate semantics)
              :to-be nil)))

  (it "accepts valid semantics with cloud auth"
    (let ((semantics
           (list :domains (list (cons :authentication
                                     '((:operation :authenticate
                                        :provider :aws-cli
                                        :command "aws s3 ls"))))
                 :coverage '(:total-tokens 3 :claimed-tokens 2 :coverage-percent 67)
                 :parse-complete t)))
      (expect (contract/bash-semantics--validate semantics)
              :to-be nil)))

  (it "accepts semantics with empty domains"
    (let ((semantics (list :domains nil
                           :coverage '(:total-tokens 0 :claimed-tokens 0)
                           :parse-complete t)))
      (expect (contract/bash-semantics--validate semantics)
              :to-be nil)))

  (it "rejects missing :domains"
    (expect (contract/bash-semantics--validate
             '(:coverage (:ratio 1.0) :parse-complete t))
            :to-match "Missing required key :domains"))

  (it "rejects plist domains (catches the alist vs plist bug)"
    (let ((semantics (list :domains '(:filesystem ((:file "f.txt")))
                           :coverage '(:ratio 1.0)
                           :parse-complete t)))
      (expect (contract/bash-semantics--validate semantics)
              :to-match "not a cons cell")))

  (it "rejects :cloud-auth domain key (should be :authentication)"
    (let ((semantics
           (list :domains (list (cons :cloud-auth '((:provider :aws-cli))))
                 :coverage '(:ratio 1.0)
                 :parse-complete t)))
      (expect (contract/bash-semantics--validate semantics)
              :to-match "Unknown alist key :cloud-auth")))

  (it "surfaces invalid file-op within domains"
    (let ((semantics
           (list :domains (list (cons :filesystem
                                     '((:path "/tmp/f.txt" :operation :read))))
                 :coverage '(:ratio 1.0)
                 :parse-complete t)))
      (expect (contract/bash-semantics--validate semantics)
              :to-match "Missing required key :file"))))

(describe "contract/bash-parse-result--validate"
  (it "accepts valid simple parse result"
    (expect (contract/bash-parse-result--validate
             '(:success t :all-commands ((:command-name "cat")) :parse-complete t
               :tokens () :command-count 1 :command-name "cat"))
            :to-be nil))

  (it "accepts minimal parse result"
    (expect (contract/bash-parse-result--validate
             '(:success t :all-commands () :parse-complete t))
            :to-be nil))

  (it "accepts failed parse result"
    (expect (contract/bash-parse-result--validate
             '(:success nil :all-commands () :parse-complete nil
               :error "Unexpected token"))
            :to-be nil))

  (it "rejects missing :success"
    (expect (contract/bash-parse-result--validate
             '(:all-commands () :parse-complete t))
            :to-match "Missing required key :success"))

  (it "rejects missing :all-commands"
    (expect (contract/bash-parse-result--validate
             '(:success t :parse-complete t))
            :to-match "Missing required key :all-commands"))

  (it "rejects missing :parse-complete"
    (expect (contract/bash-parse-result--validate
             '(:success t :all-commands ()))
            :to-match "Missing required key :parse-complete")))

;; Buttercup matcher integration
(describe ":to-satisfy-contract with bash-parser contracts"
  (it "works with file-op contract"
    (let ((op '(:file "/tmp/f.txt" :operation :read :confidence :high
                :source :positional-arg :command "cat")))
      (expect op :to-satisfy-contract #'contract/bash-file-op--validate)))

  (it "works with semantics contract"
    (let ((semantics (list :domains (list (cons :filesystem
                                               '((:file "/tmp/f.txt" :operation :read
                                                  :confidence :high :source :positional-arg))))
                           :coverage '(:total-tokens 2 :claimed-tokens 2 :coverage-percent 100)
                           :parse-complete t)))
      (expect semantics :to-satisfy-contract #'contract/bash-semantics--validate))))

(provide 'contract-bash-parser-spec)

;;; contract-bash-parser-spec.el ends here
