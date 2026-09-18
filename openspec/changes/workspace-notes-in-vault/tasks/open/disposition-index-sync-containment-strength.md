---
name: disposition-index-sync-containment-strength
description: User decision on ask-cycle-1789334430-11 — keep register/invariant/index-sync-per-file-errors-contained at its softened statement (per-file containment; debugger entry not guaranteed under debug-on-error) or require a strict guarantee by binding debug-on-error nil in the wrapper.
change: workspace-notes-in-vault
status: blocked
task_class: bug
on_critical_path: false
cites_register_entries:
  - register/invariant/index-sync-per-file-errors-contained
  - register/invariant/index-parser-never-evaluates-file-locals
relations:
  - discovered-from:ask-cycle-1789334430-11
discovered_from: review-og-parser-hardening-F2
discovered_by: reviewer
discovered_class: spec-signal
blocker_note: user decision — softened statement already applied to the register at integrate
created_at: 2026-09-17T00:00:00Z
---

> Disposition task created by cycle-1789334430 integrate. The register entry was reconciled
> to option (a) because it must describe what shipped; this task exists so the user can
> choose (b) and schedule it.

## Options
- (a) Keep the softened invariant: per-file containment on the timer queue and full scan
  (advice on `vulpea-db-sync--update-file-if-changed`, config/org-graph/discovery.org); vulpea
  2.4.0 already condition-cases per path (redundancy accepted as defence in depth);
  `with-demoted-errors` inside `run-mode-hooks` can enter the debugger at signal time — only
  the parse-side `enable-local-variables` binding closes that; force-scan and manual
  single-file update bypass the advised unit. **Orchestrator recommendation.**
- (b) Require the strict guarantee: bind `debug-on-error` nil (and `debug-on-signal`) inside
  the wrapper and advise the bypass paths too. Small follow-up in discovery.org plus a spec.

## On disposition
(a): close; nothing to change. (b): create follow-up `og-index-sync-strict-containment`.
