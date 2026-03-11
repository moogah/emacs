# Multi-Domain Support

## ADDED Requirements

### Requirement: Commands contribute to multiple domains

A single command MUST be able to register handlers for multiple semantic domains and have all handlers execute during extraction.

#### Scenario: AWS command multi-domain extraction
- **WHEN** extracting semantics for `aws s3 cp local.txt s3://bucket/file.txt --profile prod`
- **THEN** the `:filesystem` handler SHALL identify `local.txt` as a read operation
- **AND** the `:authentication` handler SHALL identify `--profile prod` as authentication context
- **AND** the `:network` handler SHALL identify connection to `amazonaws.com`
- **AND** all three domains SHALL be present in the `:domains` output

#### Scenario: curl command multi-domain extraction
- **WHEN** extracting semantics for `curl https://example.com/file.txt -o output.txt`
- **THEN** the `:network` handler SHALL identify the HTTPS request to `example.com`
- **AND** the `:filesystem` handler SHALL identify `output.txt` as a write operation
- **AND** both domains SHALL be present in the `:domains` output

#### Scenario: git command multi-domain extraction
- **WHEN** extracting semantics for `git clone git@github.com:user/repo.git`
- **THEN** the `:network` handler SHALL identify the git protocol network operation
- **AND** the `:filesystem` handler SHALL identify the local directory creation
- **AND** the `:authentication` handler SHALL identify SSH credential usage
- **AND** all three domains SHALL be present in the output

### Requirement: Domain independence

Handlers for different domains SHALL execute independently and SHALL NOT depend on each other's results.

#### Scenario: Independent handler execution
- **WHEN** multiple handlers are registered for different domains of the same command
- **THEN** each handler SHALL receive the same `parsed-command` input
- **AND** each handler SHALL execute without knowledge of other handlers' results
- **AND** handler execution order across domains SHALL NOT affect results

#### Scenario: Handler failure isolation
- **WHEN** one domain's handler fails with an error
- **THEN** other domain handlers SHALL continue executing
- **AND** the failed handler's domain SHALL NOT appear in results
- **AND** successful handlers' results SHALL be included normally

### Requirement: Domain-specific operations

Each domain's operations SHALL follow domain-specific schemas appropriate for that semantic category.

#### Scenario: Filesystem domain operations
- **WHEN** a filesystem handler returns operations
- **THEN** each operation SHALL include `:file`, `:operation`, `:confidence`, `:command`
- **AND** `:operation` SHALL be one of: `:read`, `:write`, `:delete`, `:modify`, `:execute`, etc.

#### Scenario: Authentication domain operations
- **WHEN** an authentication handler returns operations
- **THEN** each operation SHALL include `:provider`, `:context`, `:command`
- **AND** `:provider` SHALL identify the auth provider (`:aws`, `:gcloud`, `:azure`, etc.)
- **AND** `:context` SHALL contain provider-specific auth data (profile, region, account, etc.)

#### Scenario: Network domain operations
- **WHEN** a network handler returns operations
- **THEN** each operation SHALL include `:protocol`, `:endpoint`, `:command`
- **AND** `:protocol` SHALL identify the network protocol (`:https`, `:ssh`, `:git`, etc.)
- **AND** `:endpoint` SHALL identify the remote host or service

### Requirement: Complete semantic coverage

Commands with complex behavior MUST have handlers for all relevant semantic domains.

#### Scenario: AWS S3 complete coverage
- **WHEN** examining registered handlers for "aws"
- **THEN** handlers SHALL exist for `:filesystem` domain (local file operations)
- **AND** handlers SHALL exist for `:authentication` domain (AWS credentials)
- **AND** handlers SHALL exist for `:network` domain (AWS API calls)

#### Scenario: Docker cp complete coverage
- **WHEN** examining registered handlers for "docker"
- **THEN** handlers SHALL exist for `:filesystem` domain (container and host paths)
- **AND** handlers MAY exist for `:authentication` domain (registry auth)
- **AND** handlers MAY exist for `:network` domain (registry pulls)

### Requirement: Handler result aggregation

Results from multiple domain handlers SHALL be aggregated into a single `:domains` alist.

#### Scenario: Multiple domains in output
- **WHEN** a command has handlers for three domains that all return results
- **THEN** the `:domains` output SHALL contain exactly three entries
- **AND** each entry SHALL be a cons cell `(domain . operations-list)`

#### Scenario: Empty domains excluded
- **WHEN** a handler returns `nil` or an empty `:operations` list
- **THEN** that domain SHALL NOT appear in the `:domains` output
- **AND** only domains with actual operations SHALL be included

#### Scenario: Same domain multiple handlers
- **WHEN** two handlers both contribute to `:filesystem` domain
- **THEN** the `:domains` output SHALL contain one `:filesystem` entry
- **AND** the operations list SHALL contain operations from both handlers
- **AND** operations SHALL be ordered by handler execution order
