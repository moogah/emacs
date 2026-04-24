---
name: nav-send-load-order-review
description: Review whether nav→send load-order is a real dependency; update chat.org and/or architecture.md
change: gptel-chat-mode
status: done
relations:
  - discovered-from:fix-chat-loader-nav-rationale
---

## Files to modify
- `config/gptel/chat/chat.org` (modify — dependency note for `nav` loader)
- `config/gptel/chat/chat.el` (tangled)
- `openspec/changes/gptel-chat-mode/architecture.md` (modify if needed —
  §Interfaces/dependency enumeration for nav)

## Implementation steps
1. Re-read the nav→send dependency claim in `chat.org` (updated in
   `ceb0168` to say: "nav loads after send because regenerate is a
   thin wrapper over `gptel-chat-send`").
2. Determine whether the symbol `gptel-chat-send` is referenced at
   **load time** from nav.el or only at **call time** (inside
   interactive command bodies). Emacs resolves symbols at call time;
   load-order only matters if a top-level form in nav.el references
   `gptel-chat-send`.
3. Inspect the planned nav implementation (scaffolded nav.el + the open
   `nav-commands.md` task body) for any top-level form that references
   send symbols.
4. Decide between:
   - (a) Keep ordering, but downgrade the comment to "convention:
     modules that call into the send pipeline load after it" — not a
     real dependency.
   - (b) Remove the ordering constraint from `architecture.md`
     §Interfaces and `chat.org` entirely. Let nav load before send.
   - (c) Keep the current "thin wrapper" rationale if a concrete
     load-time reference exists (document it).
5. Apply the chosen outcome to both `chat.org` dependency notes and
   `architecture.md` §Interfaces. Re-tangle.
6. If the decision is (b), verify order independence by swapping loader
   positions temporarily and confirming clean boot (revert after).

## Design rationale
Reviewer flagged that the updated comment may document a
non-dependency. Unlike menu→send (where transient suffix registration
is a genuine load-time reference), nav→send appears to have no
load-time coupling. If that's correct, the architecture is
over-specifying a constraint. Documenting a false dependency is worse
than no documentation: future readers will treat it as load-bearing
and design around it.

This is a small but real spec-vs-implementation signal from the review.
Resolving it before `nav-commands` implementation keeps the dependency
graph honest.

## Verification
- The comment and architecture.md agree with the actual load-time
  coupling (or deliberately documented convention).
- `./bin/tangle-org.sh config/gptel/chat/chat.org` succeeds.
- If decision (b): booting with nav loader before send loader shows
  no errors.

## Context
- Review of fix-chat-loader-nav-rationale (orchestrator session
  2026-04-20) sole finding
- `chat.org:70-75` / `chat.el:31-36`
- `architecture.md` §gptel-chat-nav (line 37: "No coupling to parser
  internals"), architecture.md §Interfaces
- `tasks/open/nav-commands.md:54` ("Regenerate is a thin wrapper over
  send")
