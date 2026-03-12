# Purpose

Detect and enforce policy for cloud authentication commands (AWS, GCP, Azure) using bash-parser cloud-auth plugin. Provides configurable response modes (allow, warn, deny) and provider filtering.

# Requirements

## Cloud authentication detection
The system SHALL use bash-parser cloud-auth plugin to detect cloud provider authentication commands.

#### Scenario: Detect AWS authentication
- **WHEN** command is "aws-vault exec prod -- aws s3 ls"
- **THEN** system detects :cloud-auth domain with :provider :aws

#### Scenario: Detect AWS CLI authentication
- **WHEN** command is "aws configure"
- **THEN** system detects :cloud-auth domain with :provider :aws

#### Scenario: Detect GCP authentication
- **WHEN** command is "gcloud auth login"
- **THEN** system detects :cloud-auth domain with :provider :gcp

#### Scenario: Detect Azure authentication
- **WHEN** command is "az login"
- **THEN** system detects :cloud-auth domain with :provider :azure

#### Scenario: No detection for non-auth commands
- **WHEN** command is "ls -la"
- **THEN** :cloud-auth domain is absent from extraction results

## Cloud configuration in scope.yml
The system SHALL load cloud authentication policy from scope.yml cloud section.

#### Scenario: Load allow mode
- **WHEN** scope.yml has cloud.auth_detection: "allow"
- **THEN** cloud auth commands execute without restrictions

#### Scenario: Load warn mode
- **WHEN** scope.yml has cloud.auth_detection: "warn"
- **THEN** cloud auth commands execute with warnings in output

#### Scenario: Load deny mode
- **WHEN** scope.yml has cloud.auth_detection: "deny"
- **THEN** cloud auth commands rejected unless explicitly allowed

#### Scenario: Default to warn when missing
- **WHEN** scope.yml has no cloud section
- **THEN** system defaults to "warn" mode

## Allow mode enforcement
The system SHALL permit all cloud authentication commands when auth_detection is "allow".

#### Scenario: AWS command allowed in allow mode
- **WHEN** cloud.auth_detection: "allow"
- **AND** command is "aws-vault exec prod -- aws s3 ls"
- **THEN** command executes without warnings or restrictions

#### Scenario: GCP command allowed in allow mode
- **WHEN** cloud.auth_detection: "allow"
- **AND** command is "gcloud auth login"
- **THEN** command executes without warnings or restrictions

## Warn mode enforcement
The system SHALL execute cloud authentication commands with warnings when auth_detection is "warn".

#### Scenario: AWS command warns in warn mode
- **WHEN** cloud.auth_detection: "warn"
- **AND** command is "aws-vault exec prod -- aws s3 ls"
- **THEN** command executes successfully
- **AND** result includes :warnings field with cloud auth detection notice

#### Scenario: Warning identifies provider
- **WHEN** cloud auth detected in warn mode
- **THEN** warning message identifies provider (AWS, GCP, or Azure)

#### Scenario: Warning suggests reviewing scope
- **WHEN** cloud auth detected in warn mode
- **THEN** warning suggests reviewing cloud section in scope.yml

## Deny mode enforcement
The system SHALL reject cloud authentication commands when auth_detection is "deny" unless provider explicitly allowed.

#### Scenario: AWS command denied in deny mode
- **WHEN** cloud.auth_detection: "deny"
- **AND** cloud.allowed_providers: []
- **AND** command is "aws-vault exec prod -- aws s3 ls"
- **THEN** validation fails with "cloud_auth_denied" error

#### Scenario: Allowed provider permitted in deny mode
- **WHEN** cloud.auth_detection: "deny"
- **AND** cloud.allowed_providers: ["aws"]
- **AND** command is "aws configure"
- **THEN** command executes successfully

#### Scenario: Disallowed provider rejected in deny mode
- **WHEN** cloud.auth_detection: "deny"
- **AND** cloud.allowed_providers: ["aws"]
- **AND** command is "gcloud auth login"
- **THEN** validation fails with "cloud_auth_denied" for GCP

## Provider filtering
The system SHALL filter cloud authentication commands by provider when allowed_providers list is configured.

#### Scenario: Empty allowed list denies all providers
- **WHEN** cloud.allowed_providers: []
- **AND** auth_detection: "deny"
- **THEN** all cloud auth commands rejected

#### Scenario: Specific provider allowed
- **WHEN** cloud.allowed_providers: ["aws", "gcp"]
- **AND** command uses AWS or GCP
- **THEN** command allowed

#### Scenario: Unlisted provider denied
- **WHEN** cloud.allowed_providers: ["aws"]
- **AND** command uses Azure
- **THEN** command rejected with provider not in allowed list

## Structured error responses for cloud auth
The system SHALL return detailed error information when cloud authentication commands are denied.

#### Scenario: Cloud auth denied error structure
- **WHEN** cloud auth command denied
- **THEN** error includes :error "cloud_auth_denied", :provider, :command, :allowed_providers

#### Scenario: Error message explains provider restriction
- **WHEN** cloud auth denied due to provider not in allowed list
- **THEN** message identifies detected provider and shows allowed_providers list

#### Scenario: Error suggests scope expansion
- **WHEN** cloud auth denied
- **THEN** message suggests adding provider to allowed_providers via request_scope_expansion

## Cloud auth validation combined with other validations
The system SHALL validate cloud authentication policy in addition to command categorization and file path validation.

#### Scenario: Cloud command passes all validations
- **WHEN** command is "aws s3 ls /workspace/bucket"
- **AND** "aws" in allowed commands
- **AND** cloud.auth_detection: "allow"
- **AND** /workspace/bucket in paths.read
- **THEN** all validations pass

#### Scenario: Cloud command fails auth policy
- **WHEN** command is "aws configure"
- **AND** "aws" in allowed commands
- **AND** cloud.auth_detection: "deny"
- **THEN** cloud auth validation fails before execution

#### Scenario: Cloud command allowed but file path denied
- **WHEN** command is "gcloud storage cat gs://bucket/file.txt"
- **AND** cloud.auth_detection: "allow"
- **BUT** gs:// path not in scope
- **THEN** file path validation fails
