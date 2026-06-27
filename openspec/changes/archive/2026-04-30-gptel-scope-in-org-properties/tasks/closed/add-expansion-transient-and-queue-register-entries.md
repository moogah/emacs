---
name: add-expansion-transient-and-queue-register-entries
description: Add register/shape/expansion-transient-scope and register/invariant/expansion-queue-always-progresses with scaffolding; cite from harden-add-to-scope-action-handler.
change: gptel-scope-in-org-properties
status: ready
relations:
  - discovered-from:rewire-expansion-writer
  - discovered-from:arch-cycle-1777470320-3
  - enables:harden-add-to-scope-action-handler
---

## Cites register entries

- `register/shape/expansion-transient-scope` — NEW. Five-key plist for the expansion transient.
- `register/invariant/expansion-queue-always-progresses` — NEW. Every add-to-scope variant pumps the queue.

## Background

The cycle-2 rewire of the expansion writer (`rewire-expansion-writer`, commit `18e290a`) shipped two contracts that have no register footprint:

1. **Transient scope plist now has five keys**, including the new `:chat-buffer` (added so `--add-bash-to-scope` and friends can find their buffer when the transient closes). Per discovery `disc-rewire-expansion-writer-1`, the shape is now load-bearing in code but unprotected against regression.

2. **All five `--add-*-to-scope` action handlers uniformly call `--process-expansion-queue`** after `transient-quit-one`. Per discovery `disc-rewire-expansion-writer-2`, this fixed a latent bug where wildcard / custom-pattern paths dropped queued expansions; the invariant ("every add-to-scope yields a queue progression") is true by structure but has no register footprint or runtime test pin.

Per architect finding `arch-cycle-1777470320-3`, these entries should land in cycle-3 *before* the harden-add-to-scope-action-handler implementation, so harden's brief can cite them.

## Files to modify

- `interfaces.org` (modify) — append two new entries:
  - `* Shape :: expansion-transient-scope` under `* Shape`
  - `* Invariant :: expansion-queue-always-progresses` under `* Invariant`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/shapes/expansion-transient-scope.el` (create) — shape predicate scaffold (opt-in tier per overlay; this is a net-new shape so worth scaffolding).
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/expansion-queue-always-progresses.test.el` (create) — invariant scaffold; structural assertion that all five action handlers carry a `--process-expansion-queue` call site, plus a runtime spec that fires a synthetic queue and asserts monotonic progression.
- `openspec/changes/gptel-scope-in-org-properties/tasks/open/harden-add-to-scope-action-handler.md` (modify) — add both new entries to the `## Cites register entries` section.

## Implementation steps

1. **Read the writer's diff** (`git show 18e290a -- config/gptel/scope/scope-expansion.org`) to identify the exact transient-scope plist constructor and queue-pump call sites.

2. **Author `register/shape/expansion-transient-scope`** in `interfaces.org`:

   ```yaml
   entry_id: register/shape/expansion-transient-scope
   tier: shape
   status: speculated
   load_bearing: true
   title: Five-key plist passed as transient-scope to the add-to-scope UI
   purpose: |
     The plist that lives on (transient-scope) for the expansion transient
     menu. Producers: --prompt-expansion (queue entry construction).
     Consumers: every --add-*-to-scope action handler.
   required_keys:
     - :violation         # the violation-info plist (cycle-1 reconciled shape)
     - :command-name      # bash-command shorthand (string | nil)
     - :chat-buffer       # the chat buffer that hosted the violation (cycle-2 addition)
   optional_keys:
     - :add-to-scope-disabled
     - :add-to-deny-disabled
   producers:
     - file: config/gptel/scope/scope-expansion.org
       function: jf/gptel-scope--prompt-expansion
   consumers:
     - file: config/gptel/scope/scope-expansion.org
       function: <each --add-*-to-scope action handler>
   validator: |
     (defun shape/validate-expansion-transient-scope (val)
       (cond ((not (plistp val)) 'not-a-plist)
             ((not (plistp (plist-get val :violation))) 'violation-not-plist)
             ((not (or (stringp (plist-get val :command-name))
                       (null (plist-get val :command-name))))
              'command-name-bad-type)
             ((not (bufferp (plist-get val :chat-buffer))) 'chat-buffer-not-buffer)
             (t nil)))
   discovered_from: gptel-scope-in-org-properties
   discovered_by: architect (cycle-2 audit)
   ```

3. **Author `register/invariant/expansion-queue-always-progresses`** in `interfaces.org`:

   ```yaml
   entry_id: register/invariant/expansion-queue-always-progresses
   tier: invariant
   status: speculated
   load_bearing: true
   title: Every add-to-scope variant pumps the queue
   purpose: |
     After transient-quit-one, every --add-*-to-scope action handler calls
     --process-expansion-queue. Without this call, queued expansions
     resolved via wildcard or custom-pattern silently drop (the cycle-2
     latent bug fix). The harden task adds the same call to the bare-
     command refusal path (Stage 4 fold-in).
   l1_assertion: structural — all five action handlers carry the queue-pump call site.
   l2_assertion: runtime — fire a synthetic three-element queue, run an add-to-scope chain, assert queue length monotonically decreases.
   discovered_from: gptel-scope-in-org-properties
   discovered_by: architect (cycle-2 audit)
   ```

4. **Generate scaffolds** following templates `templates/register-entry-shape.md` and `templates/register-entry-invariant.md`. The shape scaffold is a `defun` returning a predicate; the invariant scaffold is a buttercup spec that errors with `"speculated; not implemented"` until the harden task lands its real test.

5. **Update `harden-add-to-scope-action-handler.md`** `## Cites register entries` to add both entries.

6. Tangle and commit.

## Design rationale

Two contracts were left implicit by cycle-2's rewire because the in-place implementation pattern skipped the on-touch architect trigger; cycle-2's end-of-cycle audit caught the gap. Landing these entries before harden ensures the forward-speculation muscle stays exercised — every implementor brief should cite the entries it relies on.

## Verification

- `interfaces.org` carries both new entries; `./bin/tangle-org.sh` succeeds (interfaces.org has no babel blocks but `check-parens` should still pass).
- Both scaffolding files exist and are byte-correct (lexical-binding line 1, `provide`d).
- `harden-add-to-scope-action-handler.md` cites both entries.
- `grep -rn 'expansion-transient-scope\|expansion-queue-always-progresses' interfaces.org openspec/changes/gptel-scope-in-org-properties/` returns the entries plus the scaffolding files plus the cite in the harden task.

## Context

- Architect finding: `.orchestrator/cycles/cycle-1777470320/findings/arch-cycle-1777470320-3.md`
- Implementor discoveries: `disc-rewire-expansion-writer-1` and `disc-rewire-expansion-writer-2` (in `rewire-expansion-writer.md` task body, `## Discoveries` section)
- Reviewer ruling: `.orchestrator/cycles/cycle-1777470320/reviews/rewire-expansion-writer.md` § "Queue/transient flow integrity" — flagged as net behavioral improvement (correct call as code review; this task is the structural follow-up)

## Observations

- The plan brief's prescribed YAML schema for `register/shape/expansion-transient-scope` was empirically wrong on two fronts. (1) Required keys: brief listed `:violation :command-name :chat-buffer`, but the actual cycle-2 plist (built in `--prompt-expansion` and `--process-expansion-queue` of `config/gptel/scope/scope-expansion.org`) is `:violation :callback :patterns :tool-name :chat-buffer`. There is no `:command-name` key anywhere in the cycle-2 codebase. (2) Optional keys: brief listed `:add-to-scope-disabled`/`:add-to-deny-disabled` UI-disabled flags; no such flags exist in the actual transient-menu definition (`jf/gptel-scope-expansion-menu`'s `:if` predicates instead inspect `(plist-get violation :resource)` directly). I recorded both deviations in the entry's `status_note` and built the validator + scaffolding around the as-built shape. The `:command-name` mention also propagated to `harden-add-to-scope-action-handler.md` line 18 — fixed inline as part of this task since the harden author would otherwise inherit the same drift.
- The plan brief framed the invariant as "every `--add-*-to-scope` action handler pumps the queue" (3 handlers). The cycle-2 code actually pumps from *all five* terminal action handlers, including `--deny-expansion` and `--allow-once-action` (which are not `--add-*-to-scope` variants). I encoded the broader as-built contract in the invariant's `statement` and L1 assertion.
- The harden task's `## Cites register entries` already listed both new entry IDs (lines 18-19) before this task ran — the brief's step 5 was a no-op verification step. Step 5 was effectively just step 5b (tightening the stale `:command-name` reference); recorded here so the integrate phase doesn't re-flag this as missing work.

## Discoveries

- discovery_id: disc-add-expansion-transient-and-queue-register-entries-1
  class: vocabulary-mismatch
  description: |
    The plan brief for `register/shape/expansion-transient-scope` listed
    required keys `:violation :command-name :chat-buffer`. The cycle-2
    code uses `:violation :callback :patterns :tool-name :chat-buffer`;
    `:command-name` does not exist on the transient-scope plist. The
    string "command-name" appears nowhere under `config/gptel/scope/`.
    The brief likely confused the bash-violation `:command` field (a
    field on the `register/shape/violation-info` plist, NOT the
    transient-scope plist) with a top-level transient-scope key.
  affected_register_entry: register/shape/expansion-transient-scope
  recommendation: |
    Reconcile in integrate by flipping `status: speculated → confirmed`
    once the harden task lands and exercises the validator against
    real transient-scope plists. The `status_note` records the
    deviation for cycle traceability. Architect should also audit
    other cycle-3 task briefs that reference `:command-name` (the
    harden task is updated; check `disposition-match-pattern-handling`
    if it cites the same key).

- discovery_id: disc-add-expansion-transient-and-queue-register-entries-2
  class: invariant-gap
  description: |
    The plan brief framed the queue-progression invariant in terms of
    the three `--add-*-to-scope` variants. The cycle-2 code actually
    has the queue-pump call in five terminal handlers (deny + add +
    allow-once + wildcard + custom). Framing the invariant narrowly
    would have missed the deny / allow-once handlers — exactly the
    place where a future regression is most likely (a refactor that
    "simplifies" a non-add handler to skip the pump).
  affected_register_entry: register/invariant/expansion-queue-always-progresses
  recommendation: |
    Keep the broader as-built framing. When the harden task adds new
    refusal/redirect/no-op branches inside `--add-to-scope`, every
    branch that funcalls the wrapper callback must also pump the
    queue — the L1 structural assertion in the scaffolding spec
    catches that pairing. Reviewer of the harden task should grep
    each new branch for `process-expansion-queue` and reject any
    branch that fires the callback without pumping.

- discovery_id: disc-add-expansion-transient-and-queue-register-entries-3
  class: spec-signal
  description: |
    The plan brief's "optional UI-disabled flags"
    (`:add-to-scope-disabled`, `:add-to-deny-disabled`) are
    architectural fiction. The transient menu instead uses `:if`
    predicates that read `(plist-get violation :resource)` directly,
    so there is no precedent for stuffing UI state on the transient-
    scope plist. Including these as "optional keys" in the entry
    would have added phantom surface area to the shape.
  affected_register_entry: register/shape/expansion-transient-scope
  recommendation: |
    The entry's `forbidden_keys` is currently empty; if the harden
    task surfaces a real UI-disabling concern, add a deliberate
    optional key with a documented producer/consumer pair, not the
    speculative pair from the brief. Architect should also check
    whether the plan brief's optional-keys section was lifted from
    a different transient-scope shape elsewhere in the repo (e.g.
    a session-management transient).
