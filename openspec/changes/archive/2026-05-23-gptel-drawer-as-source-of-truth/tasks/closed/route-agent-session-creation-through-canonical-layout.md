---
name: route-agent-session-creation-through-canonical-layout
description: Agent sessions created by persistent-agent.org emit a bare `#+begin_user` block with no `* System Prompt` / `* Chat` headings — a second session.org producer that bypasses jf/gptel--create-session-core and therefore fails the canonical register/shape/session-document-layout. Route the agent-creation path through the shared jf/gptel--session-headings-block helper so agent sessions get the same canonical document shape as interactive sessions.
change: gptel-drawer-as-source-of-truth
status: done
relations:
  - discovered-from:emit-system-prompt-and-chat-headings-at-creation
---

## Files to modify

- `config/gptel/tools/persistent-agent.org` (modify) — `jf/gptel-persistent-agent--initial-body` (around persistent-agent.el:111) emits the agent `session.org` content; route it through `jf/gptel--session-headings-block` so the output is config drawer + folded `* System Prompt` + `* Chat`
- `openspec/changes/gptel-drawer-as-source-of-truth/specs/gptel/sessions-persistence.md` (modify, if needed) — confirm the session-creation shape requirement covers the agent path
- An agent-creation spec under `config/gptel/sessions/test/` or `config/gptel/tools/test/` (modify/add) — assert the agent session.org matches the canonical layout

## Why

On-touch Architect finding `arch-cycle-1779477564-1` (advisory, interface-drift), discovered while auditing `emit-system-prompt-and-chat-headings-at-creation`. The `emit` task established the canonical session.org layout (file-level config drawer + folded `* System Prompt` + `* Chat`) for the interactive creation path through `jf/gptel--create-session-core`, and `make-system-prompt-heading-authoritative` made the heading body authoritative. But `persistent-agent.org` builds agent `session.org` files via its own `jf/gptel-persistent-agent--initial-body`, which does **not** route through `jf/gptel--create-session-core` and emits only a bare `#+begin_user` block. Agent sessions therefore fail the `register/shape/session-document-layout` validator and have no `* System Prompt` heading — so the cycle-7 restore/save WYSIWYG contract does not apply to them.

This is in-change work: agent sessions are session.org files, and making them a credible WYSIWYG source of truth is part of this change's stated outcome.

## Implementation steps

1. Read `jf/gptel-persistent-agent--initial-body` and confirm how it composes the agent session content (drawer text + body).
2. Reuse `jf/gptel--session-headings-block` (the single-source heading helper in `config/gptel/sessions/commands.org`) to emit the `* System Prompt` (folded, seeded from the agent's system prompt if any) + `* Chat` headings, exactly as the interactive path does.
3. Keep the file-level config drawer where it is (at `point-min`); only the heading structure is added.
4. Re-tangle the touched `.org` files.
5. Add/extend a spec asserting an agent `session.org` matches `register/shape/session-document-layout` (drawer at point-min, singleton `* System Prompt` folded, singleton `* Chat`, turn blocks under `* Chat`).
6. At integrate, `persistent-agent.org` should be added to `register/shape/session-document-layout`'s `producers` list.

## Verification

```bash
./bin/tangle-org.sh config/gptel/tools/persistent-agent.org
./bin/run-tests.sh -d config/gptel/tools
./bin/run-tests.sh -d config/gptel/sessions
```

Expect: an agent-created `session.org` matches the canonical layout; the layout validator passes for it.

## Context

Provenance: on-touch Architect finding `arch-cycle-1779477564-1` (cycle-7 execute), `discovered_by: architect`, `discovered_class: interface-drift`. Finding file: `.orchestrator/cycles/cycle-1779477564/findings/arch-cycle-1779477564-1.md`. The architect deliberately scoped this out of the `emit` task (whose files-to-modify excluded `persistent-agent.org`).

Cited register entry: `interfaces.org#register-shape-session-document-layout` — this task makes the agent-creation path a conforming producer.

## Observations

- `jf/gptel-persistent-agent--initial-body` was a thin one-line `format` call (the pre-Addendum agent body was just a populated `#+begin_user` block). Rewiring it through `jf/gptel--session-headings-block` only required (1) adding a `(require 'gptel-session-commands)` to bring the helper into scope (load order already permits this — see `gptel.org` lines 357 vs 361), (2) extending the function signature from `(prompt)` to `(system-prompt prompt)`, and (3) passing `(plist-get preset-spec :system)` from the call site in `--task`. The drawer composition (`jf/gptel-scope-profile--render-drawer-text`) stays unchanged; the change is body-only.
- The interactive `jf/gptel--initial-session-body` and the agent `jf/gptel-persistent-agent--initial-body` are now structurally parallel — both call `jf/gptel--session-headings-block` and differ only in the user-block payload (empty `#+begin_user\n\n#+end_user\n` for interactive; populated `#+begin_user\n<prompt>\n#+end_user\n` for agent). The two functions could be unified further, but I left them as separate paths because the agent path is semantically distinct (carries a non-empty initial prompt) and the duplication is one line; collapsing them would over-couple the two callers.
- The existing creation-spec test at line 147 (which asserted on `#\+begin_user\nDO THE THING\n#\+end_user` as a substring) still passes unchanged — the populated user block lives under `* Chat` now, but the substring match still finds it.
- The `with-mock-preset` test fixture in `helpers-spec.el` declares a preset *without* `:system`, so the existing tests exercise the empty-system-prompt branch by default. I added a dedicated `gptel-make-preset` registration inside the new "seeds the * System Prompt heading body" test to exercise the populated-system-prompt branch.
- The cycle-8 meta-discovery #4 ("delete glue made dead by your rewire") did not apply here: `--initial-body` had no surrounding glue beyond the one-line `format`. The parallel task `remove-dead-initial-session-content-helpers` handles `--initial-content`, which I did not touch.

## Discoveries

- discovery_id: agent-creation-now-conforms-to-canonical-layout
  what: |
    `config/gptel/tools/persistent-agent.org` is now a conforming producer of
    `register/shape/session-document-layout`. The `producers` list for that
    register entry in `interfaces.org` currently flags this file with
    `CYCLE-7 GAP: emits a bare #+begin_user block and skips the canonical
    config drawer + * System Prompt + * Chat layout`. At integrate that
    note should be replaced with a description matching the interactive
    producer's entry (the agent path emits the same canonical layout via
    `jf/gptel-persistent-agent--initial-body` → `jf/gptel--session-headings-block`).
  load_bearing: true
  evidence: |
    config/gptel/tools/persistent-agent.org §"Initial-Body Builder" (now
    delegates to `jf/gptel--session-headings-block`); new spec
    `config/gptel/tools/test/persistent-agent/creation-spec.el` describe
    block "PersistentAgent session.org matches the canonical document
    layout" asserts the four structural invariants from
    `shape/validate-session-document-layout`.
  follow_up: integrate phase should update interfaces.org register entry
    `register/shape/session-document-layout` producers[2] (the
    persistent-agent.org entry) to drop the CYCLE-7 GAP note.

- discovery_id: signature-change-to-initial-body
  what: |
    `jf/gptel-persistent-agent--initial-body` arity changed from
    `(prompt)` to `(system-prompt prompt)`. The only in-tree caller is
    `jf/gptel-persistent-agent--task` (same file). No other callers were
    found via repo-wide grep. The helper is module-private (the `--`
    naming convention) and not part of any documented public API.
  load_bearing: false
  evidence: |
    `grep -rn "initial-body" config/ openspec/` returns hits only inside
    `persistent-agent.org` / `.el` and the change's task / spec files; no
    external module references this helper.
  follow_up: none — signature change is safe.

- discovery_id: spec-file-already-anticipated-this-shape
  what: |
    The change's `sessions-persistence.md` spec at line 60 already said
    "agent session.org … is followed by a folded * System Prompt heading
    and a * Chat heading", but no acceptance scenario pinned the full
    structural-invariant set for agent sessions. I added a new scenario
    "Fresh agent session.org matches the canonical document layout"
    making the validator-style invariants explicit, and added an
    Implementation note pointing at `--initial-body` →
    `--session-headings-block`.
  load_bearing: true
  evidence: |
    `openspec/changes/gptel-drawer-as-source-of-truth/specs/gptel/sessions-persistence.md`
    — new scenario at lines 62-68 plus Implementation block.
  follow_up: none — the spec change is in-scope of this task.


## Review

Author-blind review at `.orchestrator/cycles/cycle-1779522837/reviews/route-agent-session-creation-through-canonical-layout.md` (merge_commit `f052eb9`): **clean review, 0 findings**. Function signature change `(prompt) → (system-prompt prompt)` is internal-prefix and the sole caller was updated; 4 new buttercup specs pin the four structural invariants of `register/shape/session-document-layout`'s validator end-to-end through an on-disk `session.org`; spec scenario in `sessions-persistence.md` is well-scoped; load order in `gptel.org` satisfies the new `(require 'gptel-session-commands)`.

On-touch architect audit produced one blocking interface-drift finding (`arch-cycle-1779522837-1`): the register entry's producers stanza for `persistent-agent.org` still labels it as a CYCLE-7 GAP after this task resolved the gap. **Deferred to cycle-8 integrate** — register-prose update only, no code change required; tracked in `.orchestrator/state.json` under `architect_findings`.
