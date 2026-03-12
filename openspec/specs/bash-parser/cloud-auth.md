# Bash Parser Cloud Authentication Plugin

## Purpose

Extract cloud authentication scope from bash commands using AWS, GCP, and Azure CLI tools, identifying which cloud provider and account/profile/project is being accessed.

## Responsibilities

- Maintain pattern database for cloud CLI authentication patterns (AWS, GCP, Azure)
- Extract authentication scope including provider and account/profile/project identifier
- Claim tokens representing authentication commands, subcommands, flags, and identifiers
- Use plugin predicates to run only for cloud CLI commands
- Handle multiple authentication contexts (e.g., aws-vault wrapping aws)
- Extract region and other authentication context when specified

## Key Invariants

- Plugin domain is always :cloud-auth
- Operations have type :authenticate
- Plugin only runs when command-name matches cloud CLI tools (aws, aws-vault, gcloud, az)
- Each authentication operation includes provider keyword (:aws, :gcp, :azure)

## Requirements

### Requirement: Cloud authentication pattern database
The system SHALL maintain a database of cloud CLI authentication patterns for AWS, GCP, and Azure.

#### Scenario: AWS Vault pattern
- **WHEN** database includes aws-vault pattern
- **THEN** pattern specifies subcommand "exec" and account from first positional arg

#### Scenario: AWS CLI pattern
- **WHEN** database includes aws pattern
- **THEN** pattern specifies profile from --profile flag

#### Scenario: GCloud pattern
- **WHEN** database includes gcloud pattern
- **THEN** pattern specifies project from --project flag

#### Scenario: Azure CLI pattern
- **WHEN** database includes az pattern
- **THEN** pattern specifies subscription from --subscription flag

### Requirement: Extract authentication scope
The system SHALL extract authentication scope including provider and account/profile/project identifier.

#### Scenario: AWS Vault authentication
- **WHEN** extracting from "aws-vault exec oncall-production -- aws s3 ls"
- **THEN** operation is :authenticate with provider :aws and account "oncall-production"

#### Scenario: AWS profile authentication
- **WHEN** extracting from "aws --profile prod s3 ls"
- **THEN** operation is :authenticate with provider :aws and profile "prod"

#### Scenario: GCP project authentication
- **WHEN** extracting from "gcloud --project my-project compute instances list"
- **THEN** operation is :authenticate with provider :gcp and project "my-project"

#### Scenario: Azure subscription authentication
- **WHEN** extracting from "az --subscription my-sub vm list"
- **THEN** operation is :authenticate with provider :azure and subscription "my-sub"

### Requirement: Token claiming for auth operations
The system SHALL claim tokens representing authentication commands, subcommands, flags, and account identifiers.

#### Scenario: Claim command name
- **WHEN** detecting aws-vault command
- **THEN** plugin claims command-name token

#### Scenario: Claim subcommand
- **WHEN** detecting "exec" subcommand in aws-vault
- **THEN** plugin claims subcommand positional-arg token

#### Scenario: Claim account identifier
- **WHEN** detecting account name "oncall-production"
- **THEN** plugin claims account positional-arg token

#### Scenario: Claim profile flag and value
- **WHEN** detecting "--profile prod" in aws command
- **THEN** plugin claims both flag and flag-arg tokens

### Requirement: Plugin predicate for applicability
The system SHALL use predicates to determine when cloud-auth plugin applies.

#### Scenario: Predicate matches cloud CLI
- **WHEN** command-name is "aws", "aws-vault", "gcloud", or "az"
- **THEN** plugin predicate returns true

#### Scenario: Predicate rejects non-cloud commands
- **WHEN** command-name is "cat" or other non-cloud command
- **THEN** plugin predicate returns false

### Requirement: Cloud auth plugin result
The system SHALL return plugin result with domain :cloud-auth and authentication operations.

#### Scenario: Plugin result domain
- **WHEN** cloud-auth plugin extracts operations
- **THEN** result domain is :cloud-auth

#### Scenario: Plugin result operations
- **WHEN** cloud-auth plugin extracts operations
- **THEN** result operations include :operation :authenticate with provider and identifier

#### Scenario: Plugin result metadata
- **WHEN** cloud-auth plugin extracts operations
- **THEN** result metadata includes :provider keyword

### Requirement: Multiple authentication contexts
The system SHALL handle commands with multiple authentication contexts (e.g., aws-vault wrapping aws).

#### Scenario: Nested authentication
- **WHEN** extracting from "aws-vault exec prod -- aws --profile staging s3 ls"
- **THEN** plugin extracts both aws-vault account and aws profile as separate operations

#### Scenario: Token claiming for both contexts
- **WHEN** handling nested authentication
- **THEN** plugin claims tokens for both authentication mechanisms

### Requirement: Region and additional context
The system SHALL extract region and other authentication context when specified.

#### Scenario: AWS region flag
- **WHEN** extracting from "aws --region us-west-2 s3 ls"
- **THEN** operation includes :region "us-west-2"

#### Scenario: GCP zone flag
- **WHEN** extracting from "gcloud --zone us-central1-a compute instances list"
- **THEN** operation includes :zone "us-central1-a"

## Plugin Protocol Compliance

```elisp
;; Plugin registration
(jf/bash-register-plugin
  :name 'cloud-auth
  :priority 100
  :extractor #'jf/bash-plugin-cloud-auth
  :predicates (list #'jf/bash--cloud-auth-predicate))

;; Predicate function
(defun jf/bash--cloud-auth-predicate (parsed-command)
  "Return t if command is cloud CLI."
  (member (plist-get parsed-command :command-name)
          '("aws" "aws-vault" "gcloud" "az")))

;; Plugin function
(defun jf/bash-plugin-cloud-auth (parsed-command)
  "Extract cloud authentication from PARSED-COMMAND."
  (make-jf/bash-plugin-result
    :domain :cloud-auth
    :operations operations-list
    :claimed-token-ids claimed-ids
    :metadata (list :provider provider-keyword)))
```

## Pattern Database Structure

```elisp
(defconst jf/bash-cloud-auth-patterns
  '((:command "aws-vault"
     :subcommand "exec"
     :account-source :positional-arg
     :account-index 0
     :provider :aws)
    (:command "aws"
     :profile-flag "--profile"
     :region-flag "--region"
     :provider :aws)
    (:command "gcloud"
     :project-flag "--project"
     :zone-flag "--zone"
     :provider :gcp)
    (:command "az"
     :subscription-flag "--subscription"
     :provider :azure)))
```

## Integration Points

- **Plugin System**: Registered with predicate for cloud CLI commands only
- **Core Parser**: Uses token inventory to claim relevant tokens
- **gptel Scope System**: Consumer of cloud authentication scope

## Example Usage

```elisp
;; AWS Vault
(jf/bash-extract-semantics (jf/bash-parse "aws-vault exec prod -- aws s3 ls"))
;; => (:domains ((:cloud-auth . ((operation :authenticate
;;                                 provider :aws
;;                                 account "prod"
;;                                 confidence :high)))) ...)

;; GCP
(jf/bash-extract-semantics (jf/bash-parse "gcloud --project my-proj compute instances list"))
;; => (:domains ((:cloud-auth . ((operation :authenticate
;;                                 provider :gcp
;;                                 project "my-proj"
;;                                 confidence :high)))) ...)
```
