---
name: register-control-helpers-in-registry
description: Register type-text/send-keys/run-command/frame-state/eval as registry entries
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:implement-tool-spec-registry"
  - "blocked-by:implement-control-helpers"
  - "blocked-by:implement-frame-state-serializer"
---

## Files to modify
- config/emacs-test-targets/target-support.org (modify) — register existing helpers
- config/emacs-test-targets/test/unit/registry-coverage-spec.el (new) — Buttercup specs

## Implementation steps
1. Register the already-implemented control/observation operations into the registry
   (from implement-tool-spec-registry) so the registry is the *complete* curated surface:
   - `type-text` → `jf/type-text` (literal insertion), read-only nil, category "input".
   - `send-keys` → `jf/send-keys` (kbd notation), read-only nil, category "input".
   - `run-command` → `jf/run-command` (interactive command), read-only nil, category "input".
   - `frame-state` → `jf/frame-state` (display-free JSON observation), read-only t,
     category "observation".
   - `eval` → a thin entry that evaluates a passed Elisp string. NOTE: raw `--eval` (L0)
     remains the primary escape hatch; this registry entry exists so `eval` is *listed*
     in the manifest as a known operation, not to replace direct `--eval`. Mark
     read-only nil.
2. Give each entry a complete docstring + typed `args` schema (e.g. `type-text` takes
   `(:name "text" :type string :description ...)`).
3. Do NOT reimplement these helpers — they are implemented by their own tasks; this task
   only wraps/registers them with metadata.
4. Write unit specs (`registry-coverage-spec.el`):
   - all five ops appear in `jf/agent-tools` with correct `read-only` flags;
   - each registered `function` is `fboundp` and its `args` schema is non-empty;
   - the observation op (`frame-state`) is read-only t while the input ops are nil.
5. Tangle + validate; `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup`.

## Design rationale
The registry must cover the whole curated surface so the `jf/agent-api` manifest (L2)
advertises every available operation, not just the emcp-derived introspection ops. The
existing helpers (built in scaffold/control-helpers/frame-state tasks) are the input and
observation half of the baseline; registering them with typed metadata makes them
discoverable and gptel-adaptable on the same footing as the introspection ops. Listing
`eval` in the registry documents the escape hatch as a first-class, discoverable
operation while keeping raw `emacsclient --eval` as the actual L0 mechanism (design.md
D9).

## Verification
- `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup` passes coverage specs.
- `(length (jf/agent-tools))` includes the five entries plus the introspection ops plus
  `jf/agent-api` (baseline ~14; design.md D10).
- Each `it` cites `specs/control-surface-discoverability/spec.md § Curated Agent Control
  Registry`.

## Context
- design.md § Decisions 'D9' (L1 baseline registry) and 'D10' (baseline op count)
- specs/control-surface-discoverability/spec.md § 'Curated Agent Control Registry'
- specs/emacs-control-tools/spec.md (the input/observation helper requirements)
