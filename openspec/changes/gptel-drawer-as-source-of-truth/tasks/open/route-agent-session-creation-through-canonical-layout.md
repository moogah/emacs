---
name: route-agent-session-creation-through-canonical-layout
description: Agent sessions created by persistent-agent.org emit a bare `#+begin_user` block with no `* System Prompt` / `* Chat` headings — a second session.org producer that bypasses jf/gptel--create-session-core and therefore fails the canonical register/shape/session-document-layout. Route the agent-creation path through the shared jf/gptel--session-headings-block helper so agent sessions get the same canonical document shape as interactive sessions.
change: gptel-drawer-as-source-of-truth
status: ready
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
