---
name: agent-introspection-tools
description: "Owned read-only introspection gptel tools (describe, find-definition, variable-value, apropos)"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:agent-scaffold-module"
  - "enables:agent-presets"
---

## Files to modify
- config/emacs-agent/introspection-tools.org (new)
- config/emacs-agent/test/unit/introspection-tools-spec.el (new)

## Implementation steps
1. Specs first, calling each tool's `:function` directly:
   - `describe_symbol` returns documentation + arglist for a function, documentation
     for a variable, both when both exist;
   - `find_definition` returns file and position (via `find-function-noselect` /
     `find-variable-noselect` or `find-lisp-object-file-name` — avoid leaving visited
     buffers behind; kill any buffer the lookup created);
   - `variable_value` returns `prin1-to-string` of the global value (bounded length,
     with a truncation marker) and does not change it;
   - `apropos` with kind `command` returns only commands matching the pattern;
     kinds: function, variable, command, any;
   - unknown symbol → explanatory string, no error signalled.
2. Register each with `gptel-make-tool`, category `"introspection"`, sync, typed args,
   gated on gptel like `config/org-graph/tools.org`.
3. Decide find-references / manual lookup per design Open Questions; add only if cheap
   and covered by specs.

## Design rationale
emcp's operation set informed the choice, but its transport stack is the dependency
tree this change avoids; ragmacs is unmaintained since 2025-08 and mixes test tools
and `elisp_eval` into the registry. The operations are one-liners over built-ins, so
owning them gives our naming, a clean read-only tier, and tests.

## Verification
- ./bin/tangle-org.sh config/emacs-agent/introspection-tools.org
- ./bin/run-tests.sh -d config/emacs-agent

## Context
- design.md § Decisions 'D10. Introspection tools are owned, thin, and registered as gptel tools'
- specs/agent-introspection-tools/spec.md
