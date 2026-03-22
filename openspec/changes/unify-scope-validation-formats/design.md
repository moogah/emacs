## Context

Two validation subsystems evolved independently within the scope module:

**Pipeline validators** (scope-shell-tools.el) use `:error` + `:message`:
```elisp
(list :error "path_out_of_scope" :path "/tmp/file" :operation :read
      :message "Path not in read scope: /tmp/file")
```

**Dispatch validators** (scope-core.el) use `:reason` with no `:message`:
```elisp
(list :allowed nil :reason "denied-pattern" :resource "/etc/passwd" :tool "read_file")
```

**validate-bash-tool** is a hybrid — uses `:reason` (machine code) AND `:message` (human text):
```elisp
(list :allowed nil :reason "command-not-allowed" :message "No bash_tools configuration found..."
      :tool "run_bash_command" :resource cmd :command cmd)
```

Two consumers bridge the gap:
- `build-violation-info` uses `(or :reason :message)` fallback — breaks when both fields exist with different semantics
- `format-tool-error` uses `(or :reason :error)` fallback — same issue

## Goals / Non-Goals

**Goals:**
- All validators return `:error` (machine code) + `:message` (human text)
- `build-violation-info` reads `:message` directly — no fallback chains
- `format-tool-error` reads `:error` directly — no fallback chains
- Remove `:reason` from all validator return plists
- Update tests that construct hand-built validator plists

**Non-Goals:**
- Changing pipeline validators (already correct)
- Changing expansion UI (already reads correct fields from violation-info)
- Changing scope.yml format or tool signatures
- Adding backwards compatibility shims

## Decisions

### 1. Rename `:reason` to `:error` in dispatch validators, add `:message`

**validate-path-tool** changes:

```elisp
;; Before
(list :allowed nil :reason "denied-pattern" :resource full-path :tool tool-name)
(list :allowed nil :reason "not-in-scope" :resource full-path :tool tool-name :allowed-patterns target-paths)

;; After
(list :allowed nil :error "denied-pattern" :resource full-path :tool tool-name
      :message (format "Path denied by scope: %s" full-path))
(list :allowed nil :error "not-in-scope" :resource full-path :tool tool-name
      :allowed-patterns target-paths
      :message (format "Path not in %s scope: %s" operation full-path))
```

**validate-pattern-tool** changes (4 denial sites):

```elisp
;; Before (all 4 sites)
(list :allowed nil :reason "not-in-org-roam-patterns" :resource R :tool T)
(list :allowed nil :reason "unknown-org-roam-tool" :resource T :tool T)

;; After
(list :allowed nil :error "not-in-org-roam-patterns" :resource R :tool T
      :message (format "Pattern not in org-roam configuration: %s" R))
(list :allowed nil :error "unknown-org-roam-tool" :resource T :tool T
      :message (format "Unknown org-roam tool: %s" T))
```

**validate-bash-tool** changes (2 denial sites):

```elisp
;; Before
(list :allowed nil :reason "command-not-allowed" :tool T :resource cmd :command cmd
      :message "No bash_tools configuration found...")
(list :allowed nil :reason "malformed-config" :tool T :resource cmd :command cmd
      :message "bash_tools.categories section no longer supported...")

;; After (just rename :reason → :error)
(list :allowed nil :error "command-not-allowed" :tool T :resource cmd :command cmd
      :message "No bash_tools configuration found...")
(list :allowed nil :error "malformed-config" :tool T :resource cmd :command cmd
      :message "bash_tools.categories section no longer supported...")
```

### 2. Simplify build-violation-info

Remove fallback chains. Read `:error` for error-type routing, `:message` for human-readable reason.

```elisp
;; Before
(error-type (or (plist-get validation-error :error)
                (plist-get validation-error :reason)
                "unknown"))
(reason (or (plist-get validation-error :reason)
            (plist-get validation-error :message)))

;; After
(error-type (or (plist-get validation-error :error) "unknown"))
(reason (plist-get validation-error :message))
```

### 3. Simplify format-tool-error

Same pattern — read `:error` directly, no fallback to `:reason`.

```elisp
;; Before
(error-type (or (plist-get check-result :reason)
                (plist-get check-result :error)
                "scope-violation"))

;; After
(error-type (or (plist-get check-result :error) "scope-violation"))
```

### 4. Update test plists

Tests that construct hand-built validator plists need `:reason` → `:error` + `:message`:

| File | Change |
|---|---|
| `test-violation-info-spec.el` | All hand-built plists already use `:error` + `:message` (no `:reason` remaining after earlier cleanup) |
| `expansion-integration-spec.el` | Line 105: change `:reason "denied-pattern"` to `:error "denied-pattern"` in hand-built plist |
| `expansion-ui-handlers-spec.el` | Lines 111, 160, 189, 207, 269, 314: change `:reason` to `:error`, add `:message` |
| `expansion-ui-spec.el` | Lines 503, 544, 585, 625, 672: change `:reason` to `:error`, add `:message` |
| `path-validation-spec.el` | Assertions on `:reason` need updating to `:error` |
| `test-callback-closure.el` | Line 74: change `:reason` to `:error` |

### 5. Edit .org source files, then tangle

Changes must be made in the `.org` files, not `.el` directly:
- `config/gptel/scope/scope-core.org` — validate-path-tool, validate-pattern-tool, validate-bash-tool, build-violation-info, format-tool-error
- Tangle with `./bin/tangle-org.sh config/gptel/scope/scope-core.org`
- Test files are `.el` only (no `.org` source) — edit directly

## Risks / Trade-offs

**[Risk: missed `:reason` reference]** → Grep for `:reason` across all scope `.el` files after implementation. Any remaining reference to `:reason` in validator code (not violation-info output) is a bug.

**[Risk: format-tool-error also affected]** → `format-tool-error` at scope-core.el:845 has the same `:reason`/`:error` fallback. Must be updated in the same change to avoid partial migration.

**[Risk: request_scope_expansion meta tool builds violation-info directly]** → At scope-shell-tools.el:665, this tool constructs violation-info with `:reason` (the output field, not the validator field). This is correct — `:reason` in violation-info output is the human-readable text for the UI. No change needed here, but verify during implementation.

**[Risk: build-expansion-info reads `:reason` from check-result]** → At scope-core.el:845, `format-tool-error` (which we called build-expansion-info earlier) reads `:reason`. This is the same function identified in Decision 3. Covered.
