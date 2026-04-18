;;; cloud-auth-spec.el --- Tests for cloud authentication command handlers -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

;; Load contract validation helpers
(require 'contract-test-helpers
         (expand-file-name "contract-test-helpers.el"
                           (file-name-directory (or load-file-name buffer-file-name))))

;;; Helper to reset registry between tests

(defun cloud-auth-test--reset-registry ()
  "Reset the command handler registry and re-register cloud auth handlers."
  (setq jf/bash-command-handlers (make-hash-table :test 'equal))
  (load (expand-file-name "config/bash-parser/commands/gcloud.el" jf/emacs-dir) nil t)
  (load (expand-file-name "config/bash-parser/commands/az.el" jf/emacs-dir) nil t)
  (load (expand-file-name "config/bash-parser/commands/aws-vault.el" jf/emacs-dir) nil t))

(defun cloud-auth-test--validate-result (result context)
  "Validate handler RESULT against contracts. Returns RESULT."
  (contract-test--validate-handler-result result context)
  result)

;;; Tests

(describe "Cloud Authentication Command Handlers"

  (before-each
    (cloud-auth-test--reset-registry))

  (describe "gcloud CLI"

    (describe "authentication handler"

      (it "extracts --project value"
        (let ((result (jf/bash-command-gcloud--auth-handler
                       '(:command-name "gcloud"
                         :args ("--project" "my-project" "compute" "instances" "list")
                         :positional-args ("compute" "instances" "list")
                         :flags ("--project")))))
          (cloud-auth-test--validate-result result "gcloud auth --project")
          (expect (plist-get result :domain) :to-equal :authentication)
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (plist-get op :provider) :to-equal :gcloud)
            (expect (cdr (assoc :project context)) :to-equal "my-project"))))

      (it "extracts --account value"
        (let ((result (jf/bash-command-gcloud--auth-handler
                       '(:command-name "gcloud"
                         :args ("--account" "user@example.com" "auth" "list")
                         :positional-args ("auth" "list")
                         :flags ("--account")))))
          (cloud-auth-test--validate-result result "gcloud auth --account")
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (cdr (assoc :account context)) :to-equal "user@example.com"))))

      (it "extracts both --project and --account"
        (let ((result (jf/bash-command-gcloud--auth-handler
                       '(:command-name "gcloud"
                         :args ("--project" "my-project" "--account" "user@example.com" "compute" "instances" "list")
                         :positional-args ("compute" "instances" "list")
                         :flags ("--project" "--account")))))
          (cloud-auth-test--validate-result result "gcloud auth --project+--account")
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (cdr (assoc :project context)) :to-equal "my-project")
            (expect (cdr (assoc :account context)) :to-equal "user@example.com"))))

      (it "returns authentication domain even without project/account"
        (let ((result (jf/bash-command-gcloud--auth-handler
                       '(:command-name "gcloud"
                         :args ("compute" "instances" "list")
                         :positional-args ("compute" "instances" "list")
                         :flags nil))))
          (cloud-auth-test--validate-result result "gcloud auth bare")
          (expect (plist-get result :domain) :to-equal :authentication)
          (let* ((ops (plist-get result :operations))
                 (op (car ops)))
            (expect (plist-get op :provider) :to-equal :gcloud)))))

    (describe "network handler"

      (it "returns network domain for any gcloud command"
        (let ((result (jf/bash-command-gcloud--network-handler
                       '(:command-name "gcloud"
                         :args ("compute" "instances" "list")
                         :positional-args ("compute" "instances" "list")
                         :flags nil))))
          (expect (plist-get result :domain) :to-equal :network)
          (let* ((ops (plist-get result :operations))
                 (op (car ops)))
            (expect (plist-get op :protocol) :to-equal :https)
            (expect (plist-get op :endpoint) :to-equal "googleapis.com")))))

    (describe "multi-domain integration"

      (it "registers two separate handlers for gcloud"
        (let ((domains (jf/bash-lookup-command-handlers "gcloud")))
          (expect domains :not :to-be nil)
          (expect (gethash :authentication domains) :not :to-be nil)
          (expect (gethash :network domains) :not :to-be nil)))

      (it "extracts both domains via registry"
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "gcloud"
                          :args ("--project" "my-project" "compute" "instances" "list")
                          :positional-args ("compute" "instances" "list")
                          :flags ("--project"))))
               (domains (plist-get result :domains)))
          (contract-test--validate-domains domains "gcloud multi-domain registry")
          (expect (alist-get :authentication domains) :not :to-be nil)
          (expect (alist-get :network domains) :not :to-be nil)))))

  (describe "Azure CLI"

    (describe "authentication handler"

      (it "extracts --subscription value"
        (let ((result (jf/bash-command-az--auth-handler
                       '(:command-name "az"
                         :args ("--subscription" "my-sub-id" "vm" "list")
                         :positional-args ("vm" "list")
                         :flags ("--subscription")))))
          (cloud-auth-test--validate-result result "az auth --subscription")
          (expect (plist-get result :domain) :to-equal :authentication)
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (plist-get op :provider) :to-equal :azure)
            (expect (cdr (assoc :subscription context)) :to-equal "my-sub-id"))))

      (it "extracts --resource-group value"
        (let ((result (jf/bash-command-az--auth-handler
                       '(:command-name "az"
                         :args ("--resource-group" "my-rg" "vm" "list")
                         :positional-args ("vm" "list")
                         :flags ("--resource-group")))))
          (cloud-auth-test--validate-result result "az auth --resource-group")
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (cdr (assoc :resource-group context)) :to-equal "my-rg"))))

      (it "extracts -g shorthand for resource group"
        (let ((result (jf/bash-command-az--auth-handler
                       '(:command-name "az"
                         :args ("-g" "my-rg" "vm" "list")
                         :positional-args ("vm" "list")
                         :flags ("-g")))))
          (cloud-auth-test--validate-result result "az auth -g")
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (cdr (assoc :resource-group context)) :to-equal "my-rg"))))

      (it "extracts both --subscription and --resource-group"
        (let ((result (jf/bash-command-az--auth-handler
                       '(:command-name "az"
                         :args ("--subscription" "my-sub" "--resource-group" "my-rg" "vm" "list")
                         :positional-args ("vm" "list")
                         :flags ("--subscription" "--resource-group")))))
          (cloud-auth-test--validate-result result "az auth --subscription+--resource-group")
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (cdr (assoc :subscription context)) :to-equal "my-sub")
            (expect (cdr (assoc :resource-group context)) :to-equal "my-rg"))))

      (it "returns authentication domain even without subscription/resource-group"
        (let ((result (jf/bash-command-az--auth-handler
                       '(:command-name "az"
                         :args ("vm" "list")
                         :positional-args ("vm" "list")
                         :flags nil))))
          (cloud-auth-test--validate-result result "az auth bare")
          (expect (plist-get result :domain) :to-equal :authentication)
          (let* ((ops (plist-get result :operations))
                 (op (car ops)))
            (expect (plist-get op :provider) :to-equal :azure)))))

    (describe "network handler"

      (it "returns network domain for any az command"
        (let ((result (jf/bash-command-az--network-handler
                       '(:command-name "az"
                         :args ("vm" "list")
                         :positional-args ("vm" "list")
                         :flags nil))))
          (expect (plist-get result :domain) :to-equal :network)
          (let* ((ops (plist-get result :operations))
                 (op (car ops)))
            (expect (plist-get op :protocol) :to-equal :https)
            (expect (plist-get op :endpoint) :to-equal "azure.com")))))

    (describe "multi-domain integration"

      (it "registers two separate handlers for az"
        (let ((domains (jf/bash-lookup-command-handlers "az")))
          (expect domains :not :to-be nil)
          (expect (gethash :authentication domains) :not :to-be nil)
          (expect (gethash :network domains) :not :to-be nil)))

      (it "extracts both domains via registry"
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "az"
                          :args ("--subscription" "my-sub" "vm" "list")
                          :positional-args ("vm" "list")
                          :flags ("--subscription"))))
               (domains (plist-get result :domains)))
          (contract-test--validate-domains domains "az multi-domain registry")
          (expect (alist-get :authentication domains) :not :to-be nil)
          (expect (alist-get :network domains) :not :to-be nil)))))

  (describe "aws-vault"

    (describe "authentication handler"

      (it "extracts profile from exec subcommand"
        (let ((result (jf/bash-command-aws-vault--auth-handler
                       '(:command-name "aws-vault"
                         :args ("exec" "production" "--" "aws" "s3" "ls")
                         :positional-args ("exec" "production" "aws" "s3" "ls")
                         :flags nil))))
          (cloud-auth-test--validate-result result "aws-vault exec profile")
          (expect (plist-get result :domain) :to-equal :authentication)
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (plist-get op :provider) :to-equal :aws-vault)
            (expect (cdr (assoc :profile context)) :to-equal "production"))))

      (it "extracts profile from login subcommand"
        (let ((result (jf/bash-command-aws-vault--auth-handler
                       '(:command-name "aws-vault"
                         :args ("login" "staging")
                         :positional-args ("login" "staging")
                         :flags nil))))
          (cloud-auth-test--validate-result result "aws-vault login profile")
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (plist-get op :provider) :to-equal :aws-vault)
            (expect (cdr (assoc :profile context)) :to-equal "staging"))))

      (it "skips flags when finding profile"
        (let ((result (jf/bash-command-aws-vault--auth-handler
                       '(:command-name "aws-vault"
                         :args ("exec" "--duration" "1h" "production" "--" "aws" "s3" "ls")
                         :positional-args ("exec" "production" "aws" "s3" "ls")
                         :flags ("--duration")))))
          (cloud-auth-test--validate-result result "aws-vault exec with flags")
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (cdr (assoc :profile context)) :to-equal "production"))))

      (it "returns authentication domain for non-exec subcommands"
        (let ((result (jf/bash-command-aws-vault--auth-handler
                       '(:command-name "aws-vault"
                         :args ("list")
                         :positional-args ("list")
                         :flags nil))))
          (cloud-auth-test--validate-result result "aws-vault list")
          (expect (plist-get result :domain) :to-equal :authentication)
          (let* ((ops (plist-get result :operations))
                 (op (car ops)))
            (expect (plist-get op :provider) :to-equal :aws-vault)
            ;; No profile context for non-exec subcommands
            (expect (plist-get op :context) :to-be nil))))

      (it "returns authentication domain even without args"
        (let ((result (jf/bash-command-aws-vault--auth-handler
                       '(:command-name "aws-vault"
                         :args nil
                         :positional-args nil
                         :flags nil))))
          (cloud-auth-test--validate-result result "aws-vault bare")
          (expect (plist-get result :domain) :to-equal :authentication)
          (let* ((ops (plist-get result :operations))
                 (op (car ops)))
            (expect (plist-get op :provider) :to-equal :aws-vault)))))

    (describe "registry integration"

      (it "registers authentication handler for aws-vault"
        (let ((domains (jf/bash-lookup-command-handlers "aws-vault")))
          (expect domains :not :to-be nil)
          (expect (gethash :authentication domains) :not :to-be nil)))

      (it "extracts authentication domain via registry"
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "aws-vault"
                          :args ("exec" "production" "--" "aws" "s3" "ls")
                          :positional-args ("exec" "production" "aws" "s3" "ls")
                          :flags nil)))
               (domains (plist-get result :domains)))
          (contract-test--validate-domains domains "aws-vault registry")
          (expect (alist-get :authentication domains) :not :to-be nil)))))

  (describe "cross-provider isolation"

    (it "each cloud provider has independent handlers"
      (let ((gcloud-domains (jf/bash-lookup-command-handlers "gcloud"))
            (az-domains (jf/bash-lookup-command-handlers "az"))
            (aws-vault-domains (jf/bash-lookup-command-handlers "aws-vault")))
        ;; gcloud and az have auth + network
        (expect (gethash :authentication gcloud-domains) :not :to-be nil)
        (expect (gethash :network gcloud-domains) :not :to-be nil)
        (expect (gethash :authentication az-domains) :not :to-be nil)
        (expect (gethash :network az-domains) :not :to-be nil)
        ;; aws-vault has auth only
        (expect (gethash :authentication aws-vault-domains) :not :to-be nil)
        (expect (gethash :network aws-vault-domains) :to-be nil)))))

;;; cloud-auth-spec.el ends here
