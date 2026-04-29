# Project-specific implementor extensions (elisp / literate-org / gptel)

The core implementor brief (deliver one task at expert level, produce a diff
+ structured report, push back on `speculated` register entries, never
modify the register directly) applies unchanged. The notes below extend it
with project-specific idioms.

## Edit `.org`, never `.el` directly

This is a literate-programming repo. **Always**:

1. Edit the `.org` file under `config/<subsystem>/<module>.org`.
2. Run `./bin/tangle-org.sh config/<subsystem>/<module>.org` to tangle.
   The script tangles **and** validates parens (`check-parens`); a
   non-zero exit means the elisp is malformed — fix before committing.
3. Stage **both** the `.org` and the `.el` together.

If you find yourself editing a `.el` file directly, stop. The next
`tangle-org.sh` run will overwrite your changes.

When adding a new `.org` module:

```org
#+title: Module Name
#+property: header-args:emacs-lisp :tangle module-name.el
#+auto_tangle: y

* Lexical Binding
#+begin_src elisp :comments no
;;; module-name.el --- Description -*- lexical-binding: t; -*-
#+end_src

* Body
#+begin_src elisp
... your code ...
#+end_src
```

The first block uses `:comments no` so org-babel does not insert
structure comments before the `lexical-binding` mode line. Without
this, Emacs misses `lexical-binding: t` and silently uses dynamic
binding. This has cost real time on this codebase (closure breakage in
gptel callback chains) — do not skip it.

## Validate parens after every non-trivial form

`check-parens` is cheap; mis-balanced parens in nested `cl-loop` /
`let*` / `lambda` are expensive to debug. After writing any of:

- `cl-loop` with multiple `for` / `collect` / `do` clauses,
- `let*` with three or more bindings whose RHS is itself a form,
- `lambda` nested inside `mapcar` / `seq-map` / `cl-letf` bindings,
- any function more than three nesting levels deep,

run `./bin/tangle-org.sh <file>.org` — it tangles + checks parens in
one pass.

If you see `Wrong type argument: sequencep, <function-name>`, you have
a missing closing paren near the named function. If you see a
`scan-error`, you have an extra closing paren earlier in the file.
Bisection works: in the `.org` file, disable file-level tangling and
add `:header-args:emacs-lisp :tangle file.el` to subtree PROPERTIES
drawers one at a time until the breakage reappears.

## Common-Lisp gotchas

- `(require 'cl-lib)` at the top of any module that uses `cl-*`. Missing
  this is the single most common "void-function" error.
- Use `cl-loop`, `cl-coerce`, `cl-return`, `cl-block` — the unprefixed
  names are deprecated.
- For new code, **prefer `seq.el` over `cl-lib`** when both work.
  `seq-find`, `seq-filter`, `seq-map`, `seq-some`, `seq-reduce`. Reach
  for `cl-loop` only when the alternative is genuinely awkward.

## Module loading

Modules go under `config/<subsystem>/<module>.el` and are referenced by
dotted path:

- `"core/defaults"` → `config/core/defaults.el`
- `"gptel/sessions/registry"` → `config/gptel/sessions/registry.el`

To wire a new module, add its dotted path to `jf/enabled-modules` in
`init.org`. The loader (`jf/load-module`) handles error recovery.

**Load-order invariants** (do not violate without an explicit task to
re-order):

- `transient` before `language-modes` and `major-modes/magit`.
- `major-modes/magit` before `major-modes/org`.
- Inside `gptel.org`: skills → gptel-agent + tool defs → sessions
  modules (constants → logging → filesystem → registry → metadata →
  subagent → commands) → activities-integration.

To live-test a module change without restarting Emacs:
`M-x jf/reload-module`.

## Test framework choice

For new tests, write **buttercup** (`*-spec.el`):

```elisp
(describe "Module name"
  (it "does something"
    (expect result :to-equal expected)))
```

For maintaining existing tests, use whichever the file already uses
(`*-test.el` = ert; `*-spec.el` = buttercup). Do not migrate ert →
buttercup as a side effect of a feature change; that's its own task.

Tests are co-located under `config/<subsystem>/test/` or alongside the
module as `config/<subsystem>/<module>-spec.el`.

## Verification command

The orchestrator's `test.command` is `./bin/run-tests.sh`. For
directory-scoped runs while iterating, use:

```bash
./bin/run-tests.sh -d config/<subsystem>
./bin/run-tests.sh -d config/gptel/scope          # whole scope subsystem
./bin/run-tests.sh -d config/gptel/scope/test/yaml  # one module's tests
```

The full run (`./bin/run-tests.sh` with no args) is the gate; use
directory-scoped runs to iterate quickly.

## Discoveries class — common cases in this repo

Beyond the core list, the discovery classes most likely to fire here:

- `shape-fragmentation`: a plist constructed in two `.org` modules with
  diverging keys. Common in gptel scope (config plists, validator
  results).
- `vocabulary-mismatch`: bash-parser produces semantic ops that scope
  must categorise; the translation `pcase` is a load-bearing boundary.
- `responsibility-leakage`: a helper that grew in `scope-expansion`
  but actually belongs in `scope-yaml` or `scope-profiles`.
- `dead-branch`: an old code path left running in parallel with a new
  one — check whether `tool-categories` / `bash-extract-file-operations`
  still has live callers when work touches the validator.
- `interface-drift`: `interfaces.org` (the register) declares a contract
  that the code no longer matches. **Highest-leverage class.** Note in
  `## Discoveries` with the affected entry ID.

## When in doubt

- The `architecture.md` and `design.md` for the change are the strategy
  references. Cite them, push back on them in `## Discoveries` if
  implementation reveals friction.
- The codebase's CLAUDE.md is comprehensive; lean on it for conventions
  rather than inventing new ones.
- If the task body itself appears wrong, **stop and report back** — do
  not implement a guess. Per the core escalation contract.
