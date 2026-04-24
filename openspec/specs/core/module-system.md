# Module System

## Purpose

Provides the registration, path resolution, and error-handled loading mechanism used by every feature module in this configuration. The module system is the single entry point for `init.el` to discover and load files under `config/`, and the single interactive entry point for reloading a module during debugging.

This capability exists so that feature modules can be referenced by short logical identifiers (e.g. `"core/defaults"`, `"gptel/chat/chat"`) rather than absolute filesystem paths, and so that a failure in one module does not abort the entire init sequence.

Implementation: `init.org` at repository root (section "Module loading").

## Key Concepts

### Module Identifier

A **module identifier** is a forward-slash-separated logical name. The final segment is the file basename (without `.el`); any preceding segments are the subdirectory path beneath `config/`.

Examples:

| Identifier          | Resolves to                  |
|---------------------|------------------------------|
| `"transient"`       | `config/transient.el`        |
| `"core/defaults"`   | `config/core/defaults.el`    |
| `"gptel/chat/chat"` | `config/gptel/chat/chat.el`  |

Zero, one, or more `/` separators are all valid. There is no upper bound on depth.

### Module Registration

The list of modules to load at startup lives in `jf/enabled-modules` in `init.org`. This list is the **single source of truth** for what loads during init. Adding or removing a module means editing this list.

Entries are module identifiers; order is significant (see Load Order below).

### Load Order

Modules load sequentially in `jf/enabled-modules` order. Ordering constraints that do exist are **load-time dependencies** — a module that references a symbol at top level needs the defining module to have already loaded. By convention, modules that only reference sibling symbols inside function bodies are treated as load-order-independent (Emacs resolves those at call time).

Current load-time constraints documented in `init.org`:

- `transient` before `language-modes` and `major-modes/magit`
- `major-modes/magit` before `major-modes/org`

Subsystem-internal load order (e.g. the chat submodule order inside `gptel/chat/chat.el`) is out of scope for this spec and documented alongside each subsystem.

## Requirements

### Requirement: Module identifier resolution

The system SHALL resolve any module identifier to an absolute `.el` file path beneath `jf/emacs-dir` / `config/`, accepting zero or more `/` separators.

**Implementation:** `jf/resolve-module-path` in `init.org`.

The resolver SHALL NOT validate the identifier; it performs pure path construction only. Pathological inputs (empty string, leading `/`, `..`, trailing `/`) expand predictably via `expand-file-name` but may not resolve to a real file. Characterization tests in `config/core/test/resolve-module-path-spec.el` pin the observed behaviour.

#### Scenario: Single-segment identifier

**WHEN** resolving `"transient"`
**THEN** the result is `<jf/emacs-dir>/config/transient.el`

#### Scenario: Multi-segment identifier

**WHEN** resolving `"gptel/chat/chat"`
**THEN** the result is `<jf/emacs-dir>/config/gptel/chat/chat.el`

### Requirement: Error-handled module loading

The system SHALL load each registered module with a `condition-case` wrapper such that an error in one module logs a message and returns `nil`, but does NOT abort the init sequence.

**Implementation:** `jf/load-module` in `init.org`.

The loader SHALL:

- Emit a log line (gated on `jf/module-debug`) with load duration for successful loads.
- On error, emit `"ERROR in <module-path>: <error-message>"` and continue.

#### Scenario: A broken module does not break init

**WHEN** a module in `jf/enabled-modules` signals an error at load time
**THEN** `jf/load-module` catches the error and logs it
**AND** subsequent modules still attempt to load

### Requirement: Interactive module reload

The system SHALL provide `jf/reload-module` as an interactive command for reloading a specific module during development, completing against `jf/enabled-modules`.

**Implementation:** `jf/reload-module` in `init.org`.

#### Scenario: Reload a single module

**WHEN** the user invokes `M-x jf/reload-module` and selects an entry from `jf/enabled-modules`
**THEN** the corresponding `.el` is re-loaded without restarting Emacs

## What the module system does NOT promise

- **No identifier validation.** `jf/resolve-module-path` does not check that the identifier points at an existing file, contains only safe characters, or stays beneath `config/`. Callers pass only trusted, developer-authored identifiers from `jf/enabled-modules`.
- **No auto-discovery.** Modules must be explicitly registered in `jf/enabled-modules`. Files under `config/` that are not in the list are not loaded automatically.
- **No load-time dependency graph.** The system loads modules in list order; any load-time dependency is encoded by placing the dependency earlier in `jf/enabled-modules`. There is no topological sort, no provide/require resolver, no cycle detection.
- **No post-load hooks.** There is no per-module "after-load" mechanism in this system; use Emacs's built-in `with-eval-after-load` if needed.
