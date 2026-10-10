## ADDED Requirements

### Requirement: Owned Read-Only Introspection Tools

The system SHALL provide a small set of read-only introspection tools, registered as
gptel tools in the `"introspection"` category and allowlisted by the
`emacs-agent-read` preset, covering at minimum:

- **describe symbol** — the documentation of a function and/or variable;
- **find definition** — the source location (file and position) of a function or
  variable definition, and optionally its source text;
- **variable value** — the current global value of a variable, printed readably;
- **apropos** — symbols matching a pattern, optionally restricted by kind
  (function, variable, command).

The tools SHALL be thin wrappers over built-in Emacs facilities, SHALL NOT depend on
emcp, ragmacs or any other external package, and SHALL NOT modify Emacs state.

#### Scenario: Describe a function

- **WHEN** an agent calls the describe-symbol tool with the name of a defined
  function
- **THEN** it receives that function's documentation and arglist

#### Scenario: Find where a function is defined

- **WHEN** an agent calls the find-definition tool with the name of a function
  defined in the configuration
- **THEN** it receives the file and position of the definition

#### Scenario: Read a variable

- **WHEN** an agent calls the variable-value tool with the name of a bound variable
- **THEN** it receives the variable's current value as readable text, and the
  variable's value is unchanged

#### Scenario: Search symbols by pattern and kind

- **WHEN** an agent calls the apropos tool with a pattern and the kind `command`
- **THEN** it receives only interactive commands whose names match the pattern

### Requirement: Introspection Tools Fail Softly

Each introspection tool SHALL return an explanatory message, not signal an error to
the caller, when the requested symbol does not exist or has no such property
(e.g. no documentation, no known definition).

#### Scenario: Unknown symbol

- **WHEN** an agent calls the describe-symbol tool with a name that is not an
  interned, bound or defined symbol
- **THEN** it receives a result stating that the symbol is not defined
