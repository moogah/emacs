---
name: menu-send-coupled-options-scope
description: Drop gptel-chat-menu's Send-coupled option groups (Prompt from / Response to / Dry Run) — displayed but silently dropped by the rebound Send suffix — and clarify Decision 15's enumeration of what lives in the chat-mode mirror
change: gptel-chat-mode
status: done
relations:
  - discovered-from:menu-integration
---

## Files to modify
- `openspec/changes/gptel-chat-mode/design.md` (update Decision 15 —
  explicit enumeration of included / excluded upstream groups, with
  Decision 18 as the root-cause reference)
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  (tighten "gptel-menu integration with rebound Send" to pin the new
  contract)
- `config/gptel/chat/menu.org` (remove the Prompt-from, Response-to,
  and Dry-Run groups from `gptel-chat-menu`)
- `config/gptel/chat/menu.el` (tangled)
- `config/gptel/chat/test/menu/menu-send-rebind-spec.el` (add one
  behavioral spec that invokes a configuration infix in a chat-mode
  buffer and asserts a buffer-local variable changed — currently only
  transitively verified; drop one or two redundant layout-introspection
  specs to keep suite size stable)

## Implementation steps

1. **Commit to Option A: drop the Send-coupled groups.** Discussion on
   2026-04-23 converged on this as the only position consistent with
   Decision 18 (block-based session format). Explanation in the Design
   rationale section below. The three groups being removed:

   - `" <Prompt from"` (Minibuffer / Kill-ring / Respond in place) —
     transient-args `"m"/"y"/"i"` read by `gptel--suffix-send` at send
     time. `gptel-chat--suffix-send` takes no args, so the toggles are
     silently ignored.
   - `" >Response to"` (Echo area / Other buffer / gptel session /
     Kill-ring) — same pattern; transient-args `"e"/"b"/"g"/"k"`
     silently dropped.
   - `"Dry Run"` (Inspect query Lisp / JSON) — suffixes that directly
     invoke `(gptel--suffix-send (cons "I" (transient-args ...)))`.
     Executed in a chat-mode buffer, upstream's prompt extraction reads
     the `gptel` text-property bounds that chat-mode never emits, so
     the preview is effectively "everything from point-min" — a wrong
     preview of a send path that can't run here anyway.

   Groups that stay: system-prompt, context, tools, request-parameters
   (preset, provider, model, max-tokens, temperature, use-context,
   include-reasoning, use-tools, track-response), logging, and Send.
   All of these mutate buffer-local variables that any `gptel-request`
   caller reads — genuinely "free" reuse of upstream.

   Rewrite and Tweak-Response are covered by a separate follow-up task
   (`menu-rewrite-tweak-response-scope`) — different root cause
   (response-state-coupled rather than Send-coupled), different
   urgency (dead code rather than user-visible bug).

2. **Apply the edit in `menu.org`.** Remove the three offending vector
   blocks from the `gptel-chat-menu` prefix definition (currently
   roughly :473-498 after the request-parameters block, and :519-536
   for Dry Run). Also remove the `:incompatible '(("m" "y" "i") ("e"
   "g" "b" "k"))` declaration on the prefix — it only constrains the
   args we're dropping.

3. **Update `design.md §Decision 15`.** Replace the current "reuses
   `gptel-menu`'s configuration layout" wording with an explicit
   enumeration:

   > The chat-mode mirror includes: system-prompt, context, tools,
   > request-parameters (preset, provider, model, max-tokens,
   > temperature, use-context, include-reasoning, use-tools,
   > track-response), logging, and the rebound Send suffix. It
   > excludes Send-argument groups (Prompt-from, Response-to) and the
   > Send-derived Dry-Run inspectors: all three read
   > `transient-args` at send time and route through upstream's
   > `gptel--suffix-send`, whose prompt extraction and response
   > insertion depend on `gptel-mode`'s text-property contract that
   > chat-mode does not produce (Decision 18).

   Add a short cross-reference: "This exclusion follows mechanically
   from Decision 18 — the block-based session format makes upstream's
   Send I/O contract unreachable." Keep the "advice on
   `gptel--suffix-send`" and "duplicate whole layout" entries in
   Alternatives Considered.

4. **Update `specs/gptel-chat-mode/spec.md`** under "gptel-menu
   integration with rebound Send". Add a scenario:

   > **Scenario: chat-mode menu omits Send-coupled groups**
   > - **WHEN** the user invokes `gptel-chat-menu` in a
   >   `gptel-chat-mode` buffer
   > - **THEN** the prefix layout shows configuration groups
   >   (system-prompt, context, tools, request-parameters), logging,
   >   and Send
   > - **AND** Prompt-from, Response-to, and Dry-Run groups are not
   >   present in the layout

5. **Add the missing behavioral test** (Review Finding #2). The
   current `menu-send-rebind-spec.el` has 15 specs; all are structural
   introspection (symbol membership in the prefix layout). None
   invoke the menu from a chat-mode buffer and verify a configuration
   suffix actually mutates a buffer-local variable. The spec scenario

   > **WHEN** point is in a `gptel-chat-mode` buffer **AND** the user
   > invokes `M-x gptel-menu` **THEN** configuration actions (preset
   > pick, model change, tool selection) mutate buffer-local
   > variables as upstream does

   is verified only transitively today. Add one spec:

   - In `with-temp-buffer` under `(gptel-chat-mode)`, directly set
     `gptel-model` to a known baseline, then simulate a
     `gptel--infix-provider` or equivalent infix call (via the infix's
     suffix function or a direct mutation path), and assert
     `gptel-model` changed buffer-locally. Alternative: set
     `gptel--preset` via `gptel--apply-preset` (what the preset infix
     does under the hood) and assert a tools/model change stuck in
     the buffer.

   Drop one or two redundant "references the upstream X infix" layout-
   introspection specs to keep the suite size stable.

   Also add a spec for the Send-coupled group exclusion contract:

   - Assert `gptel-chat-menu`'s flattened layout does NOT mention
     `"m"`, `"y"`, `"i"`, `"e"`, `"g"`, `"b"`, `"k"` as transient-arg
     keys (their absence is the machine-checkable form of the spec
     scenario from Step 4). Flatten via the existing helper; grep the
     argument field rather than symbol membership.

## Design rationale

**Root cause of the incompatibility.** Upstream's Send path is shaped
by gptel-mode's buffer contract: `gptel-send` → `gptel-request nil`
→ parser reads `gptel` text-property bounds → stream callback
inserts at point with `gptel-response-prefix-alist`. Decision 18
commits chat-mode to a block-based on-disk format
(`#+begin_user` / `#+begin_assistant`), which means chat-mode
necessarily owns its parser, stream callback, FSM handlers, and a
pre-send block-open step. Upstream's Send verb cannot be reused —
and therefore neither can the Send-argument groups (Prompt-from,
Response-to) nor the Send-derived Dry-Run inspectors that route
through it.

This is not an oversight in Decision 15; it is a mechanical
consequence of Decision 18 that Decision 15 did not enumerate. The
earlier "reuses gptel-menu's configuration layout" wording was
ambiguous about whether "layout" meant the configuration groups
specifically or every group present in upstream. Implementation
read it as the latter; this task commits to the former.

**What remains "free" reuse.** Configuration infixes (preset,
provider, model, max-tokens, temperature, tools, context, system,
use-tools, include-reasoning, track-response) mutate buffer-local
variables. Any `gptel-request` caller — upstream's `gptel-send`,
our `gptel-chat-send`, a custom tool, a batch — reads those
variables. So the configuration layer genuinely crosses the
upstream / chat-mode boundary with zero coupling. Symbol-level
reuse from the upstream prefix is correct.

**Why Options B (chat-mode-aware Dry-Run) and C (honor a whitelist
of transient-args) are rejected.** Both reintroduce coupling into
upstream's Send path. Option B requires a chat-mode-native Inspect
Query, which is a second send path to maintain and is not worth the
cost for a feature that shipped broken and was not missed. Option C
extends the contract across every new upstream transient-arg and
makes the behavior of `gptel-chat-send` depend on menu state, which
contradicts chat-mode's "Send reads from the buffer, nothing else"
invariant.

**Why this task also owns the behavioral test addition (Finding 2).**
Both findings touch the same artifact and the same spec section:
the scope of `gptel-chat-menu` and the coverage of the "configuration
actions mutate buffer-local variables" scenario. Grouping keeps the
design/spec/test update as one coherent diff. The test addition is
small (one behavioral spec + a negative assertion for the excluded
groups, trading off one or two redundant layout specs).

## Design pattern

When mirroring an upstream UI with a narrow override, **enumerate
what's in the mirror and what's out of scope**, citing the design
decision that makes the out-of-scope groups unreachable. Silently
reusing layouts that depend on the overridden behavior is a common
Send-button trap — the button is visible, the user presses it,
their configuration is silently discarded, and the cost is
diagnosed as a transient arg issue when the real root cause is a
format-level decision several steps upstream.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` passes,
  including the new behavioral spec and the negative assertion for
  excluded transient-arg keys.
- Manual check: invoke `M-x gptel-chat-menu` in a chat-mode buffer;
  confirm only configuration groups + Logging + Send are visible (no
  Prompt-from / Response-to / Dry-Run rows). Confirm `M-x gptel-menu`
  from the same buffer still shows the full upstream layout
  (Decision 15's upstream-preservation clause).
- `grep -n ' <Prompt from\| >Response to\|Dry Run' config/gptel/chat/menu.el`
  returns no matches after the edit.

## Context

- Review of menu-integration (2026-04-23, orch-review session),
  Findings #1 (Send-coupled menu options) and #2 (missing behavioral
  test for Scenario 1).
- Follow-up conversation 2026-04-23 on whether to pivot away from the
  chat-mode menu mirror: converged on "shrink the mirror; Decision 18
  is the wedge" — configuration reuse works unchanged, Send reuse
  cannot.
- `config/gptel/chat/menu.el:348-496` — `gptel-chat-menu` prefix
  definition; the three offending groups live at roughly :473-477
  (Prompt from), :478-498 (Response to), and :519-536 (Dry Run).
- `config/gptel/chat/test/menu/menu-send-rebind-spec.el:182-212` —
  "shared configuration infixes" describe block, target for
  replacing one or two layout-only specs with a behavioral one.
- `design.md §Decision 15` — "gptel-menu integration; configuration
  is free, Send is rebound" (to be tightened by this task).
- `design.md §Decision 18` — block-based `session.org` format (root
  cause of upstream-Send-path incompatibility; cross-referenced by
  the updated Decision 15).
- `specs/gptel-chat-mode/spec.md §"Requirement: gptel-menu
  integration with rebound Send"`.
- `runtime/straight/repos/gptel/gptel-transient.el` — upstream
  `gptel-menu` prefix, authoritative layout reference.
- Follow-up: `menu-rewrite-tweak-response-scope` — covers the
  Rewrite and Tweak-Response groups, which are response-state-
  coupled rather than Send-coupled (dead code, not user-visible
  bug); same diff location, different rationale.

## Review

Reviewed 2026-04-23 (orch-review session, batched with
`menu-rewrite-tweak-response-scope`). Reviewer-agent delegation;
consolidated findings below.

### Verification re-run

- `./bin/tangle-org.sh config/gptel/chat/menu.org` — passes (9
  blocks tangled, validation OK).
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` — 53/53 pass.
- `grep -n ' <Prompt from\| >Response to\|Dry Run' config/gptel/chat/menu.el` —
  no matches.
- Broader `./bin/run-tests.sh -d config/gptel/chat` — 320/320 pass.

### Findings

1. **Weak behavioral coverage (follow-up).** The new behavioral spec
   at `menu-send-rebind-spec.el:243-268` calls `gptel--set-with-scope`
   against a synthesized probe symbol
   (`gptel-chat-menu-test--model-probe`). The task body (Step 5)
   explicitly permitted this variant, but the scenario the spec is
   witnessing — "configuration actions (preset pick, model change,
   tool selection) mutate buffer-local variables as upstream does" —
   is better served by exercising a real `gptel-` variable along the
   actual upstream infix path. The current spec effectively verifies
   that Emacs primitives are buffer-local-safe in a chat-mode buffer,
   which is not the regression class the spec scenario is targeting.
   - Tracked as follow-up task
     `menu-behavioral-test-real-infix`
     (discovered-from: this task). Non-blocking.

2. **Pre-existing spec scenario wording (noted, no action).**
   `specs/gptel-chat-mode/spec.md:370-373` — the scenario
   "Menu configuration works in chat-mode buffer" uses `M-x gptel-menu`
   in its WHEN clause. After this task's work, `gptel-chat-menu` is
   the primary chat-mode entry point (bound on `C-c C-,`) while
   upstream `M-x gptel-menu` remains available but not primary. The
   scenario is not wrong in the narrow sense (it describes upstream's
   menu-in-chat-mode behaviour, which is separately guaranteed by the
   preceding requirement paragraph), but the title reads as if it
   covers both menus. Not introduced by this task; deferred to a
   future touch of the section rather than forcing a new task or
   inline edit.

### Findings looked for and ruled out

- `gptel--rewrite-overlays` `defvar` cleanup (covered by Task B) —
  verified removed cleanly.
- `:incompatible` declaration removal — correct; only constrained
  the dropped `m/y/i/e/g/b/k` keys.
- Decision 15 / Decision 18 cross-reference — consistent and
  composes with Task B's layered edit.
- Alternatives-considered entries ("advice on `gptel--suffix-send`"
  and "duplicate whole layout") — retained in `design.md` as
  expected.
- `menu-send-rebind-spec.el` helper `gptel-chat-menu-test--flatten-strings` —
  correctly walks vectors/strings; defensive comment about
  byte-code-vector embedded strings is acceptable.

### Blocked-by repointing

None. No open tasks depend on this one. The only dependent in
`tasks/closed/` (`menu-rewrite-tweak-response-scope`) was reviewed
in the same batch and is also closing to `done`. The follow-up task
`menu-behavioral-test-real-infix` is independent (no `blocked-by`).
