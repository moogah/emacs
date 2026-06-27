# Project-specific reviewer extensions (elisp / literate-org / gptel)

The core reviewer brief (rigorous-not-contrarian, three directions, author-blind)
applies unchanged. The checks below are *additional* concrete things to look for
when reviewing a diff in this repo. None of them override the core mindset — a
finding still has to clear the "would a thoughtful maintainer raise this" bar.

## Literate-org discipline

This is a literate-programming repo. `.el` files are **generated** from `.org`
files by `./bin/tangle-org.sh` (and `auto_tangle: y` headers).

- A diff that **only touches `.el` files** is almost always wrong. The author
  edited the tangled output, which `tangle-org.sh` will overwrite on the next
  run. **Severity: blocking.** The exception is hand-written modules with no
  `.org` source (rare; usually flagged in the file's first comment).
- A diff that touches `.org` but **not the corresponding `.el`** means the
  author skipped `tangle-org.sh`. The `.el` is out of sync with its source.
  **Severity: blocking.**
- New `.org` files must declare:
  - `#+title:`
  - `#+property: header-args:emacs-lisp :tangle <basename>.el`
  - `#+auto_tangle: y` (unless the file is intentionally manual-tangle)
- The **first** elisp babel block (the lexical-binding line) must carry
  `:comments no`. If org-babel adds structure comments before the mode line,
  Emacs misses `lexical-binding: t` and silently switches to dynamic binding
  — a class of bug that has cost real time here (closure breakage in callback
  chains). **Severity: blocking.**

## Lexical binding

Every new `.el` file (whether tangled or hand-written) **must** have on line 1:

```elisp
;;; foo.el --- Description -*- lexical-binding: t; -*-
```

If the diff adds a new `.el` with a missing or malformed mode line, raise it.
This is non-negotiable across the codebase.

## Common Lisp / `seq` idiom

- `(require 'cl-lib)` is required before any `cl-*` form. Missing-but-used =
  blocking.
- For new code, **prefer `seq` over `cl-lib`** when both work: `seq-find`,
  `seq-filter`, `seq-map`, `seq-some`, `seq-reduce`. Reserve `cl-loop` /
  `cl-letf` / `cl-block` for cases where `seq` is genuinely awkward.
- Flag uses of plain CL names where the `cl-` form exists: `coerce` →
  `cl-coerce`, `loop` → `cl-loop`, `return` → `cl-return`, `block` →
  `cl-block`. Plain CL names are deprecated and warn.

## Module-system contract

The repo's module loader is `jf/load-module` / `jf/resolve-module-path`. The
contract is documented at `openspec/specs/core/module-system.md`.

- Module identifiers are dotted paths under `config/`:
  `"core/defaults"` → `config/core/defaults.el`,
  `"gptel/sessions/registry"` → `config/gptel/sessions/registry.el`.
- Diffs that **hardcode** an absolute path or use ad-hoc `expand-file-name`
  to load a module bypass the loader contract. Flag: replace with
  `(jf/load-module "subsystem/module")` or
  `(jf/load-module (jf/resolve-module-path "subsystem/module"))`.
- Adding a new module: it must appear in `jf/enabled-modules` in `init.org`.
  A diff that creates the file but doesn't wire it in is incomplete.

### Module load order (load-bearing invariants)

If the diff touches `jf/enabled-modules` (re-orders, adds, or removes), check:

- `transient` loads **before** `language-modes` and `major-modes/magit`.
- `major-modes/magit` loads **before** `major-modes/org`.
- gptel sub-load order (inside `gptel.org`): `skills` → `gptel-agent` package
  + tool defs → sessions modules in dependency order
  (`constants → logging → filesystem → registry → metadata → subagent →
  commands`) → `activities-integration`.

Violations of any of the above are blocking.

## gptel subsystem boundaries

gptel lives at `config/gptel/`, organised by subsystem
(`sessions/`, `skills/`, `tools/`, `scope/`, `scope-profiles/`). Subsystem
boundaries are real:

- A new helper that crosses subsystems (e.g. `scope` calling into
  `tools` directly, or `sessions` reaching into `skills`) is a
  responsibility-leakage finding.
- Cross-subsystem coupling should go through documented integration points
  in `openspec/specs/gptel/architecture.md`.

For scope-specific work: the seven-stage operation-first validator pipeline
and the bash-parser handler-shape contract are the load-bearing surfaces.
Weakening either to make a test pass is `spec-signal`, not advisory.

## Shared mutable state

Flag uses across module boundaries of:

- `nconc`, `nreverse` on caller-owned lists,
- `setf` / `setcar` / `setcdr` on values that flow out of the function,
- `assq-delete-all`, `delete-dups`, `puthash` on hash tables passed in
  by reference,
- defvar-with-setq mutation patterns where a defcustom or buffer-local
  would be correct.

The bash-parser chain decomposer's `assq-delete-all` on a shared
`var-context` alist was the canonical "tests pass in isolation, fail
together" bug. Treat this class with prejudice.

## Test conventions

- New tests should be **buttercup** (`*-spec.el`, `describe` / `it` /
  `expect`). Existing **ert** tests (`*-test.el`) are fine to maintain;
  no forced migration.
- Tests must be **co-located** under `config/<subsystem>/test/` (or as
  `config/<subsystem>/<module>-spec.el` for small modules).
- A test file mixing both frameworks in a single file is a smell.
- Behavioural tests should mock at the boundary between our code and
  external dependencies (Emacs primitives, third-party packages),
  scoping mocks to the function under test via `cl-letf`. Global
  monkey-patching is a smell.

## Repo-specific reviewer cheatsheet

Quick checks every review should run mentally:

- Does every `.el` in the diff have a paired `.org`? (literate)
- Does line 1 of every new `.el` carry `lexical-binding: t`?
- Are mocks `cl-letf`-scoped or global? (global = smell)
- Are test names buttercup-style for new code?
- Does any module-system change preserve load order?
- Does any cross-subsystem call go through a documented integration point?

A clean review against this checklist is a valid outcome — say what you
ruled out and why, but don't pad findings.
