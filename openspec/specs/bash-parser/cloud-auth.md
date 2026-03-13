# Cloud Authentication Detection

## Purpose

Extract cloud authentication scope from bash commands using AWS, GCP, and Azure CLI tools, identifying which cloud provider and account/profile/project is being accessed.

## Responsibilities

- Extract authentication context (provider, profile/project/subscription) from cloud CLI commands
- Extract implicit network access from cloud CLI commands
- Support multi-domain extraction: `:authentication` and `:network` (and `:filesystem` for AWS S3)
- Use the command handler registry for dispatch — no plugin system, no predicates

## Key Invariants

- Cloud auth detection is implemented as command handlers, not plugins
- Authentication domain key is `:authentication` (not `:cloud-auth`)
- Network domain key is `:network`
- Each provider's handlers are self-contained in their command file
- Handlers execute per-simple-command — compound structures are decomposed by the orchestrator's grammar layer before handler dispatch

## Requirements

### Requirement: AWS authentication detection
The system SHALL detect AWS authentication context from `aws` and `aws-vault` commands via command handlers.

#### Scenario: AWS profile detection
- **WHEN** extracting from "aws --profile prod s3 ls"
- **THEN** `:authentication` domain contains operation with `:provider :aws` and `:context ((:profile . "prod"))`

#### Scenario: AWS region detection
- **WHEN** extracting from "aws --region us-west-2 s3 ls"
- **THEN** `:authentication` domain contains operation with `:provider :aws` and `:context ((:region . "us-west-2"))`

#### Scenario: AWS Vault profile detection
- **WHEN** extracting from "aws-vault exec oncall-production -- aws s3 ls"
- **THEN** `:authentication` domain contains operation with `:provider :aws-vault` and `:context ((:profile . "oncall-production"))`

#### Scenario: AWS implicit network access
- **WHEN** extracting from any `aws` command
- **THEN** `:network` domain contains operation with `:protocol :https` and `:endpoint "amazonaws.com"`

### Requirement: GCP authentication detection
The system SHALL detect GCP authentication context from `gcloud` commands via command handlers.

#### Scenario: GCP project detection
- **WHEN** extracting from "gcloud --project my-project compute instances list"
- **THEN** `:authentication` domain contains operation with `:provider :gcloud` and `:context ((:project . "my-project"))`

#### Scenario: GCP account detection
- **WHEN** extracting from "gcloud --account me@example.com compute instances list"
- **THEN** `:authentication` domain contains operation with `:provider :gcloud` and `:context ((:account . "me@example.com"))`

#### Scenario: GCP implicit network access
- **WHEN** extracting from any `gcloud` command
- **THEN** `:network` domain contains operation with `:protocol :https` and `:endpoint "googleapis.com"`

### Requirement: Azure authentication detection
The system SHALL detect Azure authentication context from `az` commands via command handlers.

#### Scenario: Azure subscription detection
- **WHEN** extracting from "az --subscription my-sub vm list"
- **THEN** `:authentication` domain contains operation with `:provider :azure` and `:context ((:subscription . "my-sub"))`

#### Scenario: Azure resource group detection
- **WHEN** extracting from "az -g my-rg vm list"
- **THEN** `:authentication` domain contains operation with `:provider :azure` and `:context ((:resource-group . "my-rg"))`

#### Scenario: Azure implicit network access
- **WHEN** extracting from any `az` command
- **THEN** `:network` domain contains operation with `:protocol :https` and `:endpoint "azure.com"`

### Requirement: Cloud auth in compound structures
The system SHALL detect cloud authentication in compound structures because the orchestrator decomposes compounds before dispatching to handlers.

#### Scenario: Cloud auth in chain
- **WHEN** extracting from "aws-vault exec prod -- aws s3 ls && gcloud compute instances list"
- **THEN** `:authentication` domain contains operations from both `aws-vault` and `gcloud`
- **AND** each handler receives its simple command independently

### Requirement: AWS S3 filesystem operations
The `aws` command handler SHALL also detect local filesystem operations from S3 commands.

#### Scenario: S3 copy with local source
- **WHEN** extracting from "aws s3 cp local.txt s3://bucket/key"
- **THEN** `:filesystem` domain contains operation with `:file "local.txt"` and `:operation :read`

#### Scenario: S3 copy with local destination
- **WHEN** extracting from "aws s3 cp s3://bucket/key local.txt"
- **THEN** `:filesystem` domain contains operation with `:file "local.txt"` and `:operation :write`

## Handler Implementation

```elisp
;; Each cloud CLI has its own handler file in commands/
;; Registration via standard command handler API:
(jf/bash-register-command-handler
 :command "aws" :domain :authentication :handler #'jf/bash-command-aws--auth-handler)
(jf/bash-register-command-handler
 :command "aws" :domain :network :handler #'jf/bash-command-aws--network-handler)
(jf/bash-register-command-handler
 :command "aws" :domain :filesystem :handler #'jf/bash-command-aws--filesystem-handler)
```

## Integration Points

- **Command Handler Registry**: Registered via `jf/bash-register-command-handler`
- **Orchestrator**: Dispatched per-simple-command by Layer 1
- **gptel Scope System**: Consumer of `:authentication` and `:network` domain results
