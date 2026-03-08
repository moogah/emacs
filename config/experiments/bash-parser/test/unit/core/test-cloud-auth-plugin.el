;;; test-cloud-auth-plugin.el --- Tests for cloud auth plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERT tests for bash-parser cloud authentication plugin.
;; Tests authentication extraction for AWS, GCP, and Azure CLIs.
;;
;; Test naming convention: test-cloud-auth-plugin-<scenario-slug>

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory load-file-name)))
(require 'bash-parser-cloud-auth (expand-file-name "../../../plugins/bash-parser-cloud-auth.el"
                                                    (file-name-directory load-file-name)))
(require 'bash-parser-core (expand-file-name "../../../core/bash-parser-core.el"
                                             (file-name-directory load-file-name)))

;;; AWS CLI Tests

(ert-deftest test-cloud-auth-plugin-aws-profile ()
  "Test extraction of AWS profile from --profile flag."
  (let* ((parsed (jf/bash-parse "aws --profile staging s3 ls"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    ;; Should return valid plugin result
    (should (jf/bash-plugin-result-p result))
    (should (eq (jf/bash-plugin-result-domain result) :authentication))

    ;; Should extract authentication operation
    (let ((operations (jf/bash-plugin-result-operations result)))
      (should (= (length operations) 1))

      (let ((op (car operations)))
        (should (eq (plist-get op :operation) :authenticate))
        (should (eq (plist-get op :provider) 'aws-cli))
        (should (string= (plist-get op :command) "aws"))

        ;; Should extract profile context
        (let ((context (plist-get op :context)))
          (should (string= (plist-get context :profile) "staging")))))

    ;; Should claim tokens
    (let ((claimed-ids (jf/bash-plugin-result-claimed-token-ids result)))
      (should (> (length claimed-ids) 0)))))

(ert-deftest test-cloud-auth-plugin-aws-region ()
  "Test extraction of AWS region from --region flag."
  (let* ((parsed (jf/bash-parse "aws --region us-west-2 ec2 describe-instances"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :region) "us-west-2")))))

(ert-deftest test-cloud-auth-plugin-aws-profile-and-region ()
  "Test extraction of both profile and region from AWS command."
  (let* ((parsed (jf/bash-parse "aws --profile prod --region eu-west-1 s3 ls"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :profile) "prod"))
      (should (string= (plist-get context :region) "eu-west-1")))))

(ert-deftest test-cloud-auth-plugin-aws-no-auth-flags ()
  "Test AWS command without authentication flags returns nil."
  (let* ((parsed (jf/bash-parse "aws s3 ls"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    ;; Should return nil when no auth context found
    (should-not result)))

;;; AWS Vault Tests

(ert-deftest test-cloud-auth-plugin-aws-vault-exec ()
  "Test extraction of account from aws-vault exec command."
  (let* ((parsed (jf/bash-parse "aws-vault exec prod"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))
    (should (eq (jf/bash-plugin-result-domain result) :authentication))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations)))
      (should (eq (plist-get op :operation) :authenticate))
      (should (eq (plist-get op :provider) 'aws-vault))
      (should (string= (plist-get op :command) "aws-vault"))

      ;; Should extract account from position 1
      (let ((context (plist-get op :context)))
        (should (string= (plist-get context :account) "prod"))))))

(ert-deftest test-cloud-auth-plugin-aws-vault-login ()
  "Test extraction of account from aws-vault login command."
  (let* ((parsed (jf/bash-parse "aws-vault login staging"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :account) "staging")))))

(ert-deftest test-cloud-auth-plugin-aws-vault-invalid-subcommand ()
  "Test aws-vault with unlisted subcommand returns nil."
  (let* ((parsed (jf/bash-parse "aws-vault list"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    ;; 'list' is not in the subcommands list, should return nil
    (should-not result)))

(ert-deftest test-cloud-auth-plugin-aws-vault-no-account ()
  "Test aws-vault without account argument returns nil."
  (let* ((parsed (jf/bash-parse "aws-vault exec"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    ;; No account provided, should return nil
    (should-not result)))

;;; Google Cloud Tests

(ert-deftest test-cloud-auth-plugin-gcloud-project ()
  "Test extraction of GCP project from --project flag."
  (let* ((parsed (jf/bash-parse "gcloud --project my-project compute instances list"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))
    (should (eq (jf/bash-plugin-result-domain result) :authentication))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations)))
      (should (eq (plist-get op :operation) :authenticate))
      (should (eq (plist-get op :provider) 'gcloud))
      (should (string= (plist-get op :command) "gcloud"))

      ;; Should extract project context
      (let ((context (plist-get op :context)))
        (should (string= (plist-get context :project) "my-project"))))))

(ert-deftest test-cloud-auth-plugin-gcloud-zone ()
  "Test extraction of GCP zone from --zone flag."
  (let* ((parsed (jf/bash-parse "gcloud --zone us-central1-a compute instances list"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :zone) "us-central1-a")))))

(ert-deftest test-cloud-auth-plugin-gcloud-region ()
  "Test extraction of GCP region from --region flag."
  (let* ((parsed (jf/bash-parse "gcloud --region europe-west1 compute networks list"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :region) "europe-west1")))))

(ert-deftest test-cloud-auth-plugin-gcloud-multiple-flags ()
  "Test extraction of multiple GCP flags."
  (let* ((parsed (jf/bash-parse "gcloud --project my-proj --zone us-east1-b compute instances list"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :project) "my-proj"))
      (should (string= (plist-get context :zone) "us-east1-b")))))

(ert-deftest test-cloud-auth-plugin-gcloud-account ()
  "Test extraction of GCP account from --account flag."
  (let* ((parsed (jf/bash-parse "gcloud --account user@example.com auth list"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :account) "user@example.com")))))

;;; Azure CLI Tests

(ert-deftest test-cloud-auth-plugin-azure-subscription ()
  "Test extraction of Azure subscription from --subscription flag."
  (let* ((parsed (jf/bash-parse "az --subscription my-sub vm list"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))
    (should (eq (jf/bash-plugin-result-domain result) :authentication))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations)))
      (should (eq (plist-get op :operation) :authenticate))
      (should (eq (plist-get op :provider) 'azure))
      (should (string= (plist-get op :command) "az"))

      ;; Should extract subscription context
      (let ((context (plist-get op :context)))
        (should (string= (plist-get context :subscription) "my-sub"))))))

(ert-deftest test-cloud-auth-plugin-azure-location ()
  "Test extraction of Azure location from --location flag."
  (let* ((parsed (jf/bash-parse "az --location eastus group create"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :location) "eastus")))))

(ert-deftest test-cloud-auth-plugin-azure-multiple-flags ()
  "Test extraction of multiple Azure flags."
  (let* ((parsed (jf/bash-parse "az --subscription prod-sub --location westus2 vm list"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :subscription) "prod-sub"))
      (should (string= (plist-get context :location) "westus2")))))

;;; Token Claiming Tests

(ert-deftest test-cloud-auth-plugin-claims-command-name ()
  "Test plugin claims command-name token."
  (let* ((parsed (jf/bash-parse "aws --profile staging s3 ls"))
         (result (jf/bash-plugin-cloud-auth parsed))
         (tokens (plist-get parsed :tokens))
         (claimed-ids (jf/bash-plugin-result-claimed-token-ids result)))

    ;; Find command-name token ID
    (let ((cmd-token-id
           (plist-get (seq-find (lambda (token)
                                  (eq (plist-get token :type) :command-name))
                                tokens)
                      :id)))

      ;; Command-name token should be claimed
      (should (member cmd-token-id claimed-ids)))))

(ert-deftest test-cloud-auth-plugin-claims-auth-flags ()
  "Test plugin claims authentication flag and value tokens."
  (let* ((parsed (jf/bash-parse "aws --profile staging s3 ls"))
         (result (jf/bash-plugin-cloud-auth parsed))
         (tokens (plist-get parsed :tokens))
         (claimed-ids (jf/bash-plugin-result-claimed-token-ids result)))

    ;; Find --profile flag token
    (let ((profile-flag-id
           (plist-get (seq-find (lambda (token)
                                  (and (eq (plist-get token :type) :flag)
                                       (string= (plist-get token :value) "--profile")))
                                tokens)
                      :id)))

      ;; Flag should be claimed
      (should (member profile-flag-id claimed-ids)))

    ;; Find "staging" value token
    (let ((staging-token-id
           (plist-get (seq-find (lambda (token)
                                  (string= (plist-get token :value) "staging"))
                                tokens)
                      :id)))

      ;; Value should be claimed
      (should (member staging-token-id claimed-ids)))))

(ert-deftest test-cloud-auth-plugin-claims-positional-account ()
  "Test plugin claims positional account token for aws-vault."
  (let* ((parsed (jf/bash-parse "aws-vault exec prod"))
         (result (jf/bash-plugin-cloud-auth parsed))
         (tokens (plist-get parsed :tokens))
         (claimed-ids (jf/bash-plugin-result-claimed-token-ids result)))

    ;; Find "prod" token
    (let ((prod-token-id
           (plist-get (seq-find (lambda (token)
                                  (string= (plist-get token :value) "prod"))
                                tokens)
                      :id)))

      ;; Account token should be claimed
      (should (member prod-token-id claimed-ids)))))

;;; Predicate Tests

(ert-deftest test-cloud-auth-plugin-predicate-aws ()
  "Test predicate identifies AWS commands."
  (let ((parsed (jf/bash-parse "aws s3 ls")))
    (should (jf/bash-plugin-cloud-auth--is-cloud-cli-p parsed))))

(ert-deftest test-cloud-auth-plugin-predicate-aws-vault ()
  "Test predicate identifies aws-vault commands."
  (let ((parsed (jf/bash-parse "aws-vault exec prod")))
    (should (jf/bash-plugin-cloud-auth--is-cloud-cli-p parsed))))

(ert-deftest test-cloud-auth-plugin-predicate-gcloud ()
  "Test predicate identifies gcloud commands."
  (let ((parsed (jf/bash-parse "gcloud compute instances list")))
    (should (jf/bash-plugin-cloud-auth--is-cloud-cli-p parsed))))

(ert-deftest test-cloud-auth-plugin-predicate-azure ()
  "Test predicate identifies Azure commands."
  (let ((parsed (jf/bash-parse "az vm list")))
    (should (jf/bash-plugin-cloud-auth--is-cloud-cli-p parsed))))

(ert-deftest test-cloud-auth-plugin-predicate-non-cloud ()
  "Test predicate rejects non-cloud commands."
  (let ((parsed (jf/bash-parse "cat file.txt")))
    (should-not (jf/bash-plugin-cloud-auth--is-cloud-cli-p parsed))))

;;; Metadata Tests

(ert-deftest test-cloud-auth-plugin-metadata ()
  "Test plugin includes provider and command metadata."
  (let* ((parsed (jf/bash-parse "aws --profile staging s3 ls"))
         (result (jf/bash-plugin-cloud-auth parsed))
         (metadata (jf/bash-plugin-result-metadata result)))

    (should (eq (plist-get metadata :provider) 'aws-cli))
    (should (string= (plist-get metadata :command) "aws"))))

;;; Registration Tests

(ert-deftest test-cloud-auth-plugin-registration ()
  "Test cloud auth plugin can be registered successfully."
  (setq jf/bash-semantic-plugins '())

  ;; Register the plugin
  (jf/bash-register-cloud-auth-plugin)

  ;; Should be in registry
  (should (= (length jf/bash-semantic-plugins) 1))

  (let ((plugin (car jf/bash-semantic-plugins)))
    (should (eq (plist-get plugin :name) 'cloud-auth))
    (should (= (plist-get plugin :priority) 90))
    (should (functionp (plist-get plugin :extractor)))
    (should (= (length (plist-get plugin :predicates)) 1))))

(ert-deftest test-cloud-auth-plugin-in-orchestrator ()
  "Test cloud auth plugin works through orchestrator."
  (setq jf/bash-semantic-plugins '())

  ;; Register the plugin
  (jf/bash-register-cloud-auth-plugin)

  ;; Parse and extract through orchestrator
  (let* ((parsed (jf/bash-parse "aws --profile prod --region us-east-1 s3 ls"))
         (result (jf/bash-extract-semantics parsed))
         (domains (plist-get result :domains)))

    ;; Should have authentication domain
    (should (assq :authentication domains))

    ;; Should have operation
    (let ((auth-ops (cdr (assq :authentication domains))))
      (should (= (length auth-ops) 1))

      (let* ((op (car auth-ops))
             (context (plist-get op :context)))
        (should (eq (plist-get op :operation) :authenticate))
        (should (eq (plist-get op :provider) 'aws-cli))
        (should (string= (plist-get context :profile) "prod"))
        (should (string= (plist-get context :region) "us-east-1"))))))

;;; Integration Tests

(ert-deftest test-cloud-auth-plugin-complex-aws-command ()
  "Test extraction from complex AWS command with multiple flags."
  (let* ((parsed (jf/bash-parse "aws --profile production --region ap-southeast-1 ec2 describe-instances --filters Name=tag:Environment,Values=prod"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :profile) "production"))
      (should (string= (plist-get context :region) "ap-southeast-1")))))

(ert-deftest test-cloud-auth-plugin-aws-vault-exec-nested ()
  "Test aws-vault exec wrapping AWS command."
  (let* ((parsed (jf/bash-parse "aws-vault exec staging"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations)))
      (should (eq (plist-get op :provider) 'aws-vault))
      (should (string= (plist-get (plist-get op :context) :account) "staging")))))

(ert-deftest test-cloud-auth-plugin-gcloud-all-flags ()
  "Test gcloud command with all authentication flags."
  (let* ((parsed (jf/bash-parse "gcloud --project my-project --zone us-central1-a --region us-central1 --account user@example.com compute instances list"))
         (result (jf/bash-plugin-cloud-auth parsed)))

    (should (jf/bash-plugin-result-p result))

    (let* ((operations (jf/bash-plugin-result-operations result))
           (op (car operations))
           (context (plist-get op :context)))
      (should (string= (plist-get context :project) "my-project"))
      (should (string= (plist-get context :zone) "us-central1-a"))
      (should (string= (plist-get context :region) "us-central1"))
      (should (string= (plist-get context :account) "user@example.com")))))

(provide 'test-cloud-auth-plugin)
;;; test-cloud-auth-plugin.el ends here
