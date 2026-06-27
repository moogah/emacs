---
name: audit-presets-for-unregistered-tools
description: Three preset frontmatter files reference tool names that have no `gptel-make-tool` / `gptel-make-scoped-tool` registration; first missing-tool lookup aborts preset auto-init with `Cannot find tool "<name>"`. Audit, reconcile, and add registration-time validation.
status: ready
source: openspec/changes/gptel-scope-in-org-properties
relations:
  - discovered-from:final-verify-and-archive-prep
---

> Surfaced during cycle-4 final-verify smoke test of `gptel-scope-in-org-properties` (Step 7, persistent-agent flow). The drawer-resident scope is working correctly; this defect is in a separate code path (preset registration / apply pipeline). Pre-existing — `executor.md`'s `TodoWrite` ref has no drawer-migration commit. Externalized so the next maintainer of preset-registration / tool-registration owns it.

## Symptom

Opening any session.org whose `:GPTEL_PRESET:` key names a preset that lists an unregistered tool produces:

```
[GPTEL-ERROR] Failed to auto-initialize branch session: gptel preset: Cannot find tool "TodoWrite"
```

Preset application aborts on the first unresolved tool name. The session buffer remains usable for tool calls (drawer-resident scope reads through a different code path), but the preset's tool list, model, temperature, and other application-time settings are not applied.

Reproduce: `find-file ~/.gptel/sessions/<any-executor-session>/branches/main/session.org`. Watch the messages buffer.

## Audit results (cycle-4 final-verify)

Comparing each `config/gptel/presets/*.md` `tools:` list against the union of `:name "..."` from `gptel-make-tool` and the first positional argument of `gptel-make-scoped-tool`:

| Preset | Missing tools |
|---|---|
| `executor.md` | `TodoWrite`, `Glob`, `Grep`, `Read`, `Insert`, `Edit`, `Write`, `Mkdir`, `Eval`, `Bash`, `WebSearch`, `WebFetch`, `YouTube` (13) |
| `research.md` | `WebSearch`, `WebFetch` (2) |
| `zettelkasten.md` | `Glob`, `Grep`, `Read` (3) |
| `explore.md`, `minimal.md`, `perplexity-researcher.md`, `plan.md`, `system-explorer.md` | clean |

The pattern in `executor.md` is striking — those names look like Claude Code's built-in tool surface (`Glob`/`Grep`/`Read`/`Edit`/`Write`/`Bash` etc.), which suggests the preset frontmatter was authored against an aspirational tool roster that was never actually registered in this gptel config. `WebSearch` / `WebFetch` / `YouTube` follow the same pattern.

## Why it isn't a drawer-migration regression

- `:GPTEL_PRESET:` lived in the drawer **before** this change; only `:GPTEL_SCOPE_*:` keys are new.
- The auto-init pipeline that reads the preset and applies it isn't part of this change.
- Git history (`git log --oneline -- config/gptel/presets/executor.md`) shows zero drawer-migration commits — the `TodoWrite` listing predates this work.
- Same error fires on YAML-pre-migration sessions; the change neither caused it nor exposes it more often than before.

## Two underlying defects

1. **No registration-time validation.** `config/gptel/preset-registration.el` calls `(apply #'gptel-make-preset preset-name cleaned)` (line 201) without verifying that every name in `:tools` resolves against the gptel tool registry. A typo or aspirational entry survives all the way to first-apply.

2. **Apply-time error is fatal to auto-init.** First missing-tool lookup aborts the entire preset application. Preferable: log a warning, skip the missing entries, apply the rest. (Or even better: refuse to register the preset at startup, see #1.)

## Plan

### Phase 1 — reconcile the audit
For each missing entry in the table above, decide:
- **Implement** the tool (cross-reference `config/gptel/tools/README.org` § "Tools" — `Glob`/`Grep`/`Read`/`Edit`/`Write`/`Bash`/`TodoWrite` are all natural fits and may already be partially scaffolded), or
- **Remove** the name from the preset frontmatter (the simplest path; preserves the preset's actual working subset), or
- **Stub-as-no-op** with a deprecation warning (lets old session.org files keep loading without the warning storm; reasonable middle path for `WebSearch`/`WebFetch`/`YouTube` if those are intended for a later milestone).

Priority order from a smoke-test usability standpoint:
1. `executor` — referenced by every cycle-4 smoke test session; broken auto-init is loudest.
2. `zettelkasten` — used in roam workflows.
3. `research` — used in WebSearch/WebFetch flows.

### Phase 2 — registration-time validation
In `config/gptel/preset-registration.el` (around line 201), before calling `gptel-make-preset`:

```elisp
(let ((missing (cl-remove-if (lambda (tool-name)
                                (gptel-get-tool tool-name))
                              tools-list)))
  (when missing
    (cond
      ((eq jf/gptel-preset-missing-tools-action 'error)
       (error "Preset %s references unregistered tools: %s"
              preset-name missing))
      ((eq jf/gptel-preset-missing-tools-action 'warn)
       (warn "Preset %s references unregistered tools: %s"
             preset-name missing)
       (setq cleaned (plist-put cleaned :tools
                                (cl-set-difference tools-list missing
                                                    :test #'string=)))))))
```

Default `jf/gptel-preset-missing-tools-action` to `'warn` for backward compatibility; flip to `'error` once the audit reconciles.

### Phase 3 — apply-time graceful degradation
If a tool *was* registered at preset-registration time but later unregistered (rare, but possible during interactive development), the apply path should also skip-and-warn rather than fail-closed. Smaller change in `gptel-mode`'s preset-application code.

### Phase 4 — regression spec
Add a buttercup spec under `config/gptel/test/preset-registration-spec.el`:
- A preset declaring `tools: [does_not_exist]` either fails registration (when `'error`) or registers without that tool and warns (when `'warn`).
- A preset declaring `tools: [PersistentAgent run_bash_command]` registers cleanly with both tools resolved.

## Files likely to touch

- `config/gptel/preset-registration.org` (and `.el`) — registration-time validation, action customization.
- `config/gptel/presets/{executor,research,zettelkasten}.md` — frontmatter reconciliation.
- `config/gptel/tools/*.org` — possible new tool registrations if Phase 1 chooses "implement" for any entries.
- `config/gptel/test/preset-registration-spec.el` (new) — regression coverage.
- `config/gptel/tools/README.org` — table-of-tools alignment with whatever lands.

## Verification

- Open a session whose `:GPTEL_PRESET:` is `executor`; the auto-init no longer logs `Cannot find tool ...`.
- `M-x gptel-make-preset` (or eval the registration-loop entry point) on a preset with an obviously-missing tool name reports the error/warning per the configured action.
- `./bin/run-tests.sh -d config/gptel` — preset-registration-spec passes; nothing else regresses.

## Context

- Smoke-test transcript: `~/.gptel/sessions/smoke-drawer-20260430085658/branches/main/session.org` line 124+ (the `o4-mini` PersistentAgent invocation that surfaced the *separate* hang bug; the auto-init `TodoWrite` error fired on the parent session at file-open time, independently of the agent failure).
- Related but separate finding: `jf/gptel-persistent-agent--task` lacks a `condition-case` around its synchronous-error-prone preamble; in-change follow-up filed as `tasks/open/fix-persistent-agent-error-propagation.md` (or similar — track with the parent change, not here).
