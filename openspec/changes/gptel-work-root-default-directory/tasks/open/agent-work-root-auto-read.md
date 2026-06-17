---
name: agent-work-root-auto-read
description: Auto-include work_root in the agent's read scope (work_root/** prepended to :read); remove the now-redundant D7 guardrail
change: gptel-work-root-default-directory
status: ready
relations:
  - discovered-from:agent-workroot-and-paths
---

## Provenance
- discovered_from: agent-workroot-and-paths (author-blind review spec-signal, cycle-1781718724)
- discovered_by: reviewer (corroborated by the implementor's own observation)
- discovered_class: spec-signal
- user_decision: "A work_root which is unreadable to the agent serves no purpose;
  redundancy between read_paths and work_root is acceptable for now." (resolves ask
  cycle-1781718724-d7-guardrail-prefix-match — chose Option D: auto-include, not a
  guardrail-matching tweak)

## Why
Today `work_root` and `read_paths` are fully decoupled: `--task` writes `work_root` to
the `:GPTEL_WORK_ROOT:` drawer key and feeds `read_paths` VERBATIM into `:read`, with no
link between them. That made it possible to spawn an agent whose working directory is
unreadable (read_paths omitted, or pointing elsewhere) — relative reads then silently
DENY. The cycle-2 D7 guardrail tried to *warn* about that mismatch, but (a) it fired a
false alarm on the normal `work_root=/p` + `read_paths=[/p/**]` case (the `/p/**` glob
compiles to `^/p/.*$`, which does not match the no-trailing-slash root `/p`), and (b)
the user decided the underlying decoupling isn't worth keeping: a work_root the agent
can't read serves no purpose.

Decision: make the work root readable BY CONSTRUCTION and delete the guardrail.

## Files to modify
- `config/gptel/tools/persistent-agent.org` (+ regenerated `.el`):
  - `--task` (~:560-600): after resolving `resolved-work-root` and normalizing
    `read-paths-list`, PREPEND the work-root read pattern to the read list before
    calling `build-scope-plist`. Pattern = `(concat (directory-file-name
    resolved-work-root) "/**")` (so it compiles to `^<root>/.*$` and covers every
    relative file read under the work root). Avoid a duplicate if the exact pattern is
    already present (cosmetic dedup with `member`).
  - REMOVE the D7 guardrail block (~:670-685): the `(unless
    (jf/gptel-scope--path-matches-any-pattern-p resolved-work-root read-paths-list)
    (display-warning ...))` form is now dead — the work root is always in read scope.
  - REMOVE the now-unused `(require 'jf-gptel-scope-validation)` at ~:67 (it was added
    solely for the guardrail's path-matcher; confirmed no other use in this module).
- `config/gptel/tools/test/persistent-agent/work-root-spec.el`:
  - REPLACE the two guardrail specs ("warns when work_root outside read_paths" / "no
    warn complement") with specs asserting the work-root read pattern (`<root>/**`)
    appears in `:GPTEL_SCOPE_READ:` (the work root is readable by construction), and
    that a relative file under the work root validates as ALLOWED even when the caller
    passed NO read_paths.
- `openspec/changes/gptel-work-root-default-directory/design.md`:
  - D6: add that `work_root/**` is auto-included in the agent's read scope (write stays
    separately scoped: `/tmp/**` + explicit `write_paths`).
  - D7: mark the consistency-guardrail decision SUPERSEDED — record the rationale (a
    work_root unreadable to the agent serves no purpose; read/work_root redundancy
    accepted; guardrail removed as redundant).

## Design rationale
A relative path in the agent resolves against `default-directory` (= work_root). Making
`work_root/**` part of `:read` guarantees those relative reads land in scope without the
caller having to remember to grant it. This upholds the user's principle and removes a
band-aid (the guardrail) and its cross-module `require`. It does NOT violate the
zero-inheritance invariant: the agent's own work_root joining its own read scope is
self-consistency, not inheritance of the parent's scope. We give up the (speculative,
unused) "work_root broader than read scope" case; the redundancy is accepted.

## Verification
- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` clean.
- `./bin/run-tests.sh -d config/gptel/tools` green; new specs assert work_root is in
  read scope and a relative read under work_root is ALLOWED with empty read_paths; the
  old guardrail specs are gone.
- `grep -n "path-matches-any-pattern-p\|jf-gptel-scope-validation\|display-warning" config/gptel/tools/persistent-agent.org`
  returns nothing (guardrail + require fully removed).

## Context
.orchestrator/cycles/cycle-1781718724/reviews/agent-workroot-and-paths.md (the finding);
.orchestrator/cycles/cycle-1781718724/reconciliations/vocabulary-agent-path-params.md;
register/vocabulary/agent-path-params (work_root maps_to now: :GPTEL_WORK_ROOT: drawer
key AND prepended to :GPTEL_SCOPE_READ:); design.md D6/D7.

## Observations

- **Adjacent test file required updating (in-scope side effect, not scope
  expansion).** Two specs in `config/gptel/tools/test/persistent-agent/creation-spec.el`
  asserted the OLD contract — that omitting `read_paths` yields an ABSENT
  `:GPTEL_SCOPE_READ:` key ("zero inheritance"). The auto-include changes that
  fact: omitting `read_paths` now yields a read scope of exactly
  `(<work_root>/**)`. Both specs (the `:PROPERTIES:`-drawer spec and the
  "no :GPTEL_SCOPE_READ when allowed-paths omitted" spec) were updated to assert
  the work-root pattern instead, and the latter was retitled to
  "writes :GPTEL_SCOPE_READ as the work root alone when read_paths is omitted".
  This is the changed behavior's own test surface, not new scope — but it lives
  outside the two files the task body named, so flagging it.

- **Zero-inheritance vocabulary nuance.** The creation-spec specs framed an
  absent read key as "zero inheritance". That framing conflated two distinct
  things: (a) NOT inheriting the parent's read patterns (still true and
  preserved), and (b) having an empty read scope (no longer true — the agent's
  OWN work root is now in scope). The updated comments distinguish them
  explicitly: self-consistency (own work root readable) is not inheritance
  (parent patterns are still NOT copied). A reader who only remembers the slogan
  "zero inheritance ⇒ absent read key" may be surprised; the design D6 note now
  states this distinction.

- **Read-side behavioral assertion routes through the real validator.** The new
  "ALLOWED with empty read_paths" spec reads the actual `:GPTEL_SCOPE_READ:`
  back from the rendered agent drawer and feeds it to the production
  `jf/gptel-scope--validate-path-operation`, rather than asserting on an
  internal plist. This exercises the read side of the work-root activation seam
  end-to-end (drawer render → glob compile → path match) and would catch a
  regression in either the prepend or the glob engine. Required adding a
  load-time `(require 'jf-gptel-scope-validation ...)` to the spec file (the
  in-`it` require failed under batch load because `load-file-name` is nil inside
  spec bodies).

- **The `directory-file-name` + `/**` pattern is order-sensitive vs. the seam's
  trailing-slash normalization.** The binder seam stores `default-directory` as
  `(file-name-as-directory (expand-file-name ...))` (trailing slash). The read
  pattern strips the trailing slash via `directory-file-name` before appending
  `/**`, so `<root>/**` compiles to `^<root>/.*$`. A relative file resolved
  against the slash-terminated `default-directory` (e.g. `/root/notes.txt`)
  matches. This is correct, but the two normalizations (one adds the slash, one
  strips it) are maintained independently in different modules — a future change
  to either must keep them coherent. Captured as a discovery below.

## Discoveries

- discovery_id: disc-agent-work-root-auto-read-1
  class: vocabulary-mismatch
  description: |
    register/vocabulary/agent-path-params member `work_root` previously
    mapped ONLY to the `:GPTEL_WORK_ROOT:` drawer key. This task REFINES that
    map: `work_root` now maps to BOTH the `:GPTEL_WORK_ROOT:` drawer key AND a
    prepend of `<work_root>/**` into `:GPTEL_SCOPE_READ:` (via build-scope-plist's
    `:read`). The closed param SET ({preset, description, prompt, work_root,
    read_paths, write_paths}) is UNCHANGED — only the `maps_to` of the
    `work_root` member widens. The entry's status_note already CARRIED the D7
    spec-signal that motivated this; it should now record the resolution
    (auto-include landed, guardrail removed) and update the `work_root` member's
    `maps_to`.
  affected_register_entry: register/vocabulary/agent-path-params
  recommendation: |
    Update the `work_root` member `maps_to` to:
    ":GPTEL_WORK_ROOT: drawer key AND prepended to build-scope-plist :read ->
    :GPTEL_SCOPE_READ:". Append to status_note: the carried D7 spec-signal is
    RESOLVED — work root made readable by construction (prepend), D7 guardrail +
    its `(require 'jf-gptel-scope-validation)` removed as dead. Entry stays
    CONFIRMED (closed set unchanged); this is a maps_to refinement, not a
    surface change. Recommend confirmed -> reconciled (maps_to widened).

- discovery_id: disc-agent-work-root-auto-read-2
  class: invariant-gap
  description: |
    The work-root read pattern (`directory-file-name` + "/**", strips trailing
    slash) and the binder seam's default-directory normalization
    (`file-name-as-directory`, adds trailing slash —
    register/boundary/work-root-activation-seam) are coherent today: a relative
    file resolved against the slash-terminated default-directory matches
    `^<root>/.*$`. But the two normalizations live in different modules
    (persistent-agent.org's --task vs. the sessions binder) and are maintained
    independently. There is no single shared helper or test pinning their
    coherence; a future change to either trailing-slash convention could
    silently desync them (e.g. switching the read pattern to `<root>/` or the
    seam to a no-slash form would break relative-read matching).
  affected_register_entry: register/boundary/work-root-activation-seam
  recommendation: |
    Note in the seam entry (or a new shape entry) the coherence contract: the
    work-root READ pattern derived in --task is `<directory-file-name root>/**`
    (→ `^<root>/.*$`) and MUST stay coherent with the seam's
    `file-name-as-directory` default-directory so a work-root-relative path
    matches. The new behavioral spec
    ("validates a relative read under the work root as ALLOWED with empty
    read_paths") is the cross-module guard — flag it as the regression anchor.
    No code change recommended now; this is a documentation/contract gap, not a
    defect.

- discovery_id: disc-agent-work-root-auto-read-3
  class: spec-signal
  description: |
    The "zero inheritance" framing in creation-spec.el and the read-scope
    specs is now ambiguous. Pre-change it meant BOTH "no parent read patterns"
    AND "empty read scope when read_paths omitted". Post-change those split:
    parent patterns are still NOT inherited, but the agent's own work root IS
    in scope. The phrase "zero inheritance" should be reserved for the former.
    If a downstream spec or register entry keys off "omitted read_paths ⇒ absent
    :GPTEL_SCOPE_READ:", it is now stale.
  affected_register_entry: register/vocabulary/agent-path-params
  recommendation: |
    When reconciling agent-path-params, scope-check any other spec/register
    text that equates "omitted read_paths" with "absent :GPTEL_SCOPE_READ:" or
    "empty read scope" — that equivalence no longer holds. The two
    creation-spec specs touched here were the in-tree instances and are already
    fixed. No further code change identified; this is a sweep flag.
