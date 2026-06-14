---
name: session-dir-ancestor-walk
description: Derive session-dir for an open buffer by walking up to the nearest ancestor containing a branches/ child (agents use their own dir), replacing fixed ../.. layout walks.
change: gptel-content-addressed-session-activation
status: ready
relations: []
---

## Files to modify

- `config/gptel/sessions/filesystem.org` (modify) — add `jf/gptel--session-dir-from-branch-dir` (the ancestor-marker walk).
- `config/gptel/sessions/test/filesystem/session-dir-walk-spec.el` (new) — Buttercup specs for branch and agent layouts plus depth-independence.

## Implementation steps

1. Write the spec first. Cover:
   - branch buffer at `<root>/branches/main/session.org` → session-dir = `<root>`;
   - a deeper/relocated layout (e.g. an extra wrapping dir) → still resolves to the dir whose child is `branches/` (depth-independent; no fixed `../..` count);
   - agent buffer at `<root>/branches/main/agents/<agent>/session.org` → session-dir = the agent's own directory.
2. Implement `jf/gptel--session-dir-from-branch-dir`:
   - Given the branch-dir (the file's own directory) and the session type (from `jf/gptel--session-type`, or the presence of `:GPTEL_PARENT_SESSION_ID:`):
     - `branch`: walk up from branch-dir via `locate-dominating-file` (or an explicit parent loop) to the nearest ancestor `D` such that `(file-directory-p (expand-file-name "branches" D))` and branch-dir is under `D/branches/`; return `D`.
     - `agent`: return branch-dir itself (agents do not branch; their own dir is their session root).
   - Be robust when no `branches/` ancestor is found (corrupt/standalone) — return branch-dir and log at debug.
3. Tangle and run the spec.

## Design rationale

After the `current` symlink is retired (see retire-current-symlink), session-dir's only job is "the directory under which `branches/` lives" — which this walk computes as its literal definition. It is move-safe and stores nothing. dir-locals was rejected (static-literal value is move-unsafe; cascade hands agents the parent's session-dir) and a drawer key would be redundant with what the walk derives for free. Finding a *container by structural marker* is legitimate navigation, distinct from the rejected practice of parsing *identity* out of path segments. (design.md §Decision D5.)

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/filesystem.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem` — green, including the depth-independence case.
- Done = session-dir resolves correctly for branch and agent layouts without fixed `../..` walks.

## Context

design.md § Decision "D5. session-dir: ancestor-marker walk".

## Cycle 1 updates (cycle-1781448273)

- This task was **deferred from the cycle-1 batch** to avoid a `filesystem.org` collision with
  `drawer-signature-and-head-read` (now merged, commit 15f76fb). Implement against the **current**
  `filesystem.org`, which already contains the new "Session Content Signature" section + the
  `jf/gptel--scan-session-drawer-keys` engine — add the ancestor-walk alongside, do not disturb them.
- `register/boundary/session-dir-marker-walk` remains **speculated**; this task carries its disposition.

## Cycle 2 updates (cycle-1781451784)

### Already-shipped context
- `drawer-identity-resolver` merged into `config/gptel/sessions/filesystem.org` (commit 1ec479f) — the file now contains the cycle-1 signature engine AND the new resolvers. Implement the ancestor-walk (`jf/gptel--session-dir-from-branch-dir`) **alongside** them; do not disturb existing forms.
- This task was deferred from the cycle-2 batch precisely to avoid the `filesystem.org` collision with the resolver (now resolved by sequencing). It remains the **cycle-3 critical-path candidate**, pairs cleanly with the now-unblocked `discovery-reads-drawers` (disjoint files: filesystem.org vs registry.org+filesystem.org — note the overlap on filesystem.org, sequence or split as before).

### Cited register entries
- `register/boundary/session-dir-marker-walk`: still **speculated**; this task carries its disposition.

## Observations

- **`locate-dominating-file` cleanly implements the contract.** The boundary's
  "via locate-dominating-file or an explicit parent loop" guidance held: a
  predicate of `(and (file-directory-p D/branches) (string-prefix-p D/branches branch-dir))`
  is the whole walk. No explicit parent loop was needed. The enclosure check
  (`string-prefix-p`) is load-bearing: without it, a stray `branches/` directory
  higher up the tree that does NOT contain the buffer would be falsely matched.
- **Return value is not truename-normalized.** `locate-dominating-file` returns
  the ancestor as it walked it (derived from the expanded BRANCH-DIR), not its
  truename. On macOS the temp tree resolves through `/var -> /private/var`, so
  the spec compares with `file-equal-p` rather than `string=`/`:to-equal`.
  Downstream consumers (`jf/gptel--bind-session-buffer`, a later task) should
  likewise treat the result as a path to compare semantically, not a canonical
  string. Flagged as a discovery (interface-drift, low severity).
- **Agent branch never walks.** For `type = agent` the function returns BRANCH-DIR
  immediately. This is correct per D5, but it means the agent path does NO
  structural validation — a buffer mis-typed as `agent` whose dir is not actually
  an `agents/<agent>/` leaf would silently get its own dir as session-dir. Type
  comes from the drawer (`GPTEL_PARENT_SESSION_ID` presence), so this is only
  reachable via a corrupt drawer; out of scope here, noted for completeness.
- **No-op on already-correct input.** Calling with an already-resolved root that
  has no `branches/` child returns it unchanged (the corrupt/standalone branch),
  so the function is safe to call defensively. Stores nothing, as specified.

## Discoveries

- discovery_id: disc-session-dir-ancestor-walk-1
  class: deviation
  description: |
    The speculated boundary `register/boundary/session-dir-marker-walk` held
    1:1 with implementation. Both prongs of the contract are satisfied exactly
    as written:
      - branch: nearest ancestor D with (file-directory-p D/branches) AND
        branch-dir under D/branches/ — implemented via locate-dominating-file
        with a predicate combining file-directory-p and string-prefix-p; fully
        depth-independent (no fixed ../.. count), proven by a deeper/relocated
        layout spec with an extra wrapping directory.
      - agent: returns branch-dir itself (no walk).
      - corrupt/standalone: returns branch-dir and logs at debug; never signals.
    No surprises; the entry should be CONFIRMED. The producer is
    config/gptel/sessions/filesystem.org :: jf/gptel--session-dir-from-branch-dir
    with signature (branch-dir type), where `type` is the symbol returned by
    jf/gptel--session-type (`branch' | `agent').
  affected_register_entry: register/boundary/session-dir-marker-walk
  recommendation: |
    Confirm the entry (speculated -> confirmed). Pin the producer signature as
    (branch-dir type) — the function takes the already-resolved TYPE symbol, not
    a drawer-alist, keeping it a pure structural walk decoupled from drawer
    parsing. The boundary text's "(from jf/gptel--session-type, or the presence
    of :GPTEL_PARENT_SESSION_ID:)" describes how the CALLER derives type; the
    function itself receives the resolved symbol.

- discovery_id: disc-session-dir-ancestor-walk-2
  class: interface-drift
  description: |
    The function's return value is NOT truename-normalized. locate-dominating-file
    returns the ancestor as constructed from the expanded (but not truename'd)
    BRANCH-DIR. The corrupt-path branch returns the input BRANCH-DIR (passed
    through file-name-as-directory + expand-file-name). Consumers must compare
    the result semantically (file-equal-p) rather than by string identity, and
    must not assume canonicalization (e.g. macOS /var vs /private/var, symlinked
    session roots).
  affected_register_entry: register/boundary/session-dir-marker-walk
  recommendation: |
    When wiring the downstream consumer
    (config/gptel/sessions/commands.org :: jf/gptel--bind-session-buffer),
    use file-equal-p / expand-file-name for any comparison or storage of the
    session-dir, OR decide that the boundary should specify truename
    normalization and add (file-truename ...) at the producer. Current
    implementation deliberately does NOT normalize, to keep the walk a pure
    structural operation and avoid resolving symlinks the user intentionally
    placed in the session tree. Low severity; recorded so the integrate phase
    can pick a normalization policy explicitly.
