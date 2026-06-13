---
name: serialize-window-params-readable
description: Guarantee window-state serialization is read-able — translate buffer-bearing window params (window-preserved-size) and scrub bookmark records of unreadable objects
change: workspaces-persistence-robustness
status: ready
relations: []
---

## Files to modify

- config/workspaces/layouts.org (modify) → tangle to layouts.el
- config/workspaces/test/serialization-robustness-spec.el (new, buttercup)
- config/workspaces/test/layouts-spec.el (modify, if it asserts the raw window-preserved-size shape)

## Implementation steps

1. **layouts.org — shared readability predicate.** Add
   `workspace--unreadable-object-p (obj)` returning non-nil for objects
   `read` cannot reconstruct: `bufferp`, `markerp`, `overlayp`, `framep`,
   `windowp`, `processp`, and a non-symbol `functionp` (subr/closure).
   Provide it from `workspace-layouts` (persistence.el will reuse it — it
   already `(require 'workspace-layouts)`). Keep it small and total.

2. **layouts.org — window-parameter translator table.** Add
   `workspace-window-parameter-translators`, an alist
   `(PARAM . ((serialize . FN) (deserialize . FN)))`, modelled on
   `activities-window-parameters-translators` / burly. Seed it with
   `window-preserved-size`:
   - serialize: `(BUFFER DIR SIZE)` → `((and (bufferp BUFFER) (buffer-name BUFFER)) DIR SIZE)`
   - deserialize: `(NAME DIR SIZE)` → `((and NAME (get-buffer NAME)) DIR SIZE)`
   Make it a `defcustom`/`defvar` so future buffer/marker-bearing params
   can be added in one place.

3. **layouts.org — run translators in the leaf walkers.**
   - In `workspace--window-state-serialize` (the leaf walker that already
     stores the `workspace-buffer` struct into each leaf's `parameters`
     map): after setting `workspace-buffer`, for each
     `(param . translators)` in `workspace-window-parameter-translators`
     present in `parameters`, replace its value with
     `(funcall (alist-get 'serialize translators) value)`.
   - In `workspace--window-state-deserialize`: symmetrically apply the
     `deserialize` fn, wrapped in `condition-case-unless-debug` — on
     failure, DROP the parameter (`map-delete`) rather than aborting the
     whole restore (mirrors activities' restore robustness).

4. **layouts.org — bookmark-record scrub.** In
   `workspace--serialize-buffer`, after `(bookmark-make-record)`, walk the
   record's prop alist and replace any value satisfying
   `workspace--unreadable-object-p` with nil (keep keys; null the
   offending values). This closes the bug#56643 class (a mode embedding
   an unreadable object in its bookmark record). Document why in a brief
   prose note (not an inline code comment that will rot — put rationale in
   the org prose).

5. **Do NOT** remove `window-preserved-size` from
   `workspace-window-persistent-parameters` — the decision (design D1) is
   to translate it, not drop it. Leave the persistent-params list as-is.

6. **Tangle**: `./bin/tangle-org.sh config/workspaces/layouts.org`
   (validates parens). After tangling, `git diff config/workspaces/layouts.el`
   and confirm the expected defuns landed (literate tangle hazard: a
   leading `*` in column 0 inside a src block silently truncates it).

7. **Tests — new serialization-robustness-spec.el** (buttercup; copy the
   load-prelude/header from an existing `config/workspaces/test/*-spec.el`).
   Round-trip readability property tests: for each scenario, capture via
   `workspace--capture-frameset` (or directly `workspace--window-state-serialize`
   on a `window-state-get … 'writable` form with
   `workspace-window-persistent-parameters` appended), then assert
   `(not (string-match-p "#<" (prin1-to-string state)))` AND that
   `(read (prin1-to-string state))` succeeds. Scenarios:
   - **(a) killed buffer in window-preserved-size** (the regression) —
     construction recipe:
     ```elisp
     (let* ((buf (get-buffer-create "ws-killed-test")) (win (selected-window)))
       (set-window-buffer win buf)
       (with-selected-window win (window-preserve-size win t t))
       (kill-buffer buf))
     ```
     Then capture and assert no `#<` and `read` succeeds; assert the
     translated value carries the (now-stale) buffer NAME string.
   - **(b)** a live preserved-size window (round-trips, value is a name).
   - **(c)** a narrowed buffer; **(d)** an indirect buffer; **(e)** a
     special/non-file buffer (`*scratch*`).
   - **(f)** bookmark scrub: stub a buffer's `bookmark-make-record-function`
     (or `bookmark-make-record`) to return a record embedding a marker/
     buffer; assert `workspace--serialize-buffer` produces a record with
     that value nulled and no `#<`.
   - **deserialize round-trip**: a serialized `window-preserved-size`
     (name form) → `workspace--window-state-deserialize` yields
     `(get-buffer NAME …)` (live buffer or nil if absent), and a
     deserialize whose translator throws drops the param without error.

8. **Tests — layouts-spec.el**: if any existing spec asserts the raw
   `window-preserved-size` parameter shape (buffer object), update it to
   the translated (name) shape. Otherwise leave untouched.

## Design rationale

`window-state-get … 'writable` emits writable-param VALUES verbatim — it
does not sanitize a buffer object inside `window-preserved-size`. The
canonical fix (activities.el, burly.el) is per-parameter
serialize/deserialize translators over the leaf tree; we already have the
leaf walker. The bookmark scrub closes the second source of the same
class. This task makes the serialized form readable *by construction*;
the write-time assert (other task) is the backstop. See design.md §D1/§D3
and research/findings-*.md.

## Verification

- `./bin/tangle-org.sh config/workspaces/layouts.org` validates.
- `grep -n 'window-preserved-size\|parameter-translators\|unreadable-object-p' config/workspaces/layouts.el`
  shows the translator table + predicate.
- `./bin/run-tests.sh -d config/workspaces` fully green (baseline 333/0 on
  the change branch — confirm the new spec file runs and passes).
- The killed-buffer round-trip spec fails WITHOUT the translator and
  passes WITH it (sanity: temporarily comment the translator to confirm
  the test has teeth, then restore — note this in ## Observations).

## Context

design.md §D1 (translators), §D3 (shared predicate); proposal "Layer A";
specs/workspaces/spec.md scenarios "A live Emacs object is never written
to disk", "Preserved window size round-trips across restart";
research/findings-serialization-and-corruption-safety.md (§A),
research/findings-activities-persistence-deep-dive.md (translator borrow).

## Observations

- Implemented exactly as the task body / design D1 / D3 specify: shared
  `workspace--unreadable-object-p` predicate, `workspace-window-parameter-translators`
  defvar seeded with `window-preserved-size`, serialize/deserialize loops
  in the two leaf walkers, and a `workspace--scrub-bookmark-record` helper
  called from `workspace--serialize-buffer`. `window-preserved-size` was
  LEFT in `workspace-window-persistent-parameters` per step 5.
- **Teeth check (step 7a / Verification): CONFIRMED.** Temporarily removed
  the serialize-side translator loop from the tangled `layouts.el` and ran
  the suite: scenario (a) "killed buffer in window-preserved-size" FAILED on
  `serialization-robustness-spec--readable-p` (the `#<killed buffer>` token
  reaches `prin1-to-string`), and (b) also failed (value was the buffer
  object, not a name). Restored the translator; both pass. The regression
  test has teeth.
- **`condition-case-unless-debug` under buttercup.** The deserialize-drop
  guard uses `condition-case-unless-debug` (per the task / activities
  pattern). Buttercup's batch runner sets `debug-on-error`, which makes
  `condition-case-unless-debug` *re-raise*. The first run of the
  "drops the parameter when its deserializer throws" spec therefore escaped
  the guard and failed. Resolved by binding `debug-on-error` to nil inside
  that one spec so it exercises the production catch path. This is a
  test-environment artifact, not a production behavior change — production
  runs with `debug-on-error` nil, so the drop fires as intended. Noting it
  because any future spec that wants to assert a `condition-case-unless-debug`
  catch must bind `debug-on-error` nil.
- `bookmark-make-record` returns `(NAME . ALIST)`; `bookmark-get-bookmark-record`
  on a `consp` record returns it as-is (no `bookmark-alist` lookup, no error
  path) and normalizes both `(NAME ALIST)` and `(NAME . ALIST)` shapes,
  returning the live cons cells — so the in-place `setcdr` scrub mutates the
  real record. Verified against the shipped `bookmark.el`.
- `layouts-spec.el` was left untouched: its
  `workspace--window-state-serialize / -deserialize` describe block does not
  assert the raw `window-preserved-size` shape (it asserts the
  `workspace-buffer` embed and the buffer-slot rewrite), so step 8's
  conditional edit did not apply.
- No departures from the task body. No latent issues found. The write-time
  assert remains the persistence.el task's responsibility; this serializer is
  readable by construction.

## Discoveries
- discovery_id: disc-serialize-window-params-readable-1
  class: interface-drift
  description: |
    register/shape/workspace-buffer-struct describes the `bookmark` slot as
    "Result of (bookmark-make-record)". After this task that slot holds a
    SCRUBBED bookmark record: keys are preserved verbatim, but any prop VALUE
    satisfying workspace--unreadable-object-p (marker/buffer/overlay/frame/
    window/process/non-symbol-function) is nulled before storage. The record
    remains a valid bookmark-make-record-shaped alist (a strict subset of the
    raw record's information), so the bookmark step of the reincarnation chain
    still fires for the common case. Only a record whose load-bearing value was
    nulled now falls through to the file/name fallback.
  affected_register_entry: register/shape/workspace-buffer-struct
  recommendation: |
    Add a one-line note to the `bookmark` slot description: "Scrubbed of
    unreadable prop values (workspace--scrub-bookmark-record) before storage;
    keys preserved, only unreadable VALUES nulled, so the record stays
    read-able and bookmark-shaped." No shape/key change — values-only nulling.
    Confirmed no drift in the required_keys/validator (the slot type
    `bookmark-record | nil` is unchanged).
- discovery_id: disc-serialize-window-params-readable-2
  class: mutation
  description: |
    register/shape/layout-v2-plist's :saved-state / :working-state are
    window-state forms whose leaves carry window parameters. This task adds a
    serialize→deserialize transform over those leaves: `window-preserved-size`
    now stores `(BUFFER-NAME DIR SIZE)` on disk (was `(BUFFER DIR SIZE)` with a
    live buffer object — which was the latent corruption bug). The transform is
    value-preserving across the round trip for live buffers: serialize maps
    BUFFER→buffer-name, deserialize maps name→get-buffer, round-tripping to the
    same buffer by name. A killed/absent buffer round-trips to nil (the slot
    survives, the buffer reference is dropped). Generalizes to any param added
    to workspace-window-parameter-translators.
  affected_register_entry: register/shape/layout-v2-plist
  recommendation: |
    Note in the :saved-state/:working-state descriptions that leaf window
    parameters are now passed through workspace-window-parameter-translators on
    serialize/deserialize, and that window-preserved-size's first element is a
    buffer-NAME on disk (not a buffer object). Readability-by-construction is
    now part of the shape's contract. No key add/remove.
- discovery_id: disc-serialize-window-params-readable-3
  class: invariant-gap
  description: |
    register/vocabulary/buffer-reincarnation-fallback-chain's first step
    (bookmark) is unaffected for the common case by the bookmark scrub: the
    scrub nulls only unreadable VALUES and preserves all keys, so
    workspace--bookmark-buffer still has a complete, jump-able record whenever
    the record had no unreadable values (the overwhelming majority). When an
    unreadable value WAS nulled (e.g. bug#56643 help-mode subr), the bookmark
    jump may now fail or restore partially — but that record was previously
    UNREADABLE and would have corrupted the entire save (taking down ALL
    buffers' restore, not just this one). Post-scrub, the fallback chain's
    file (step 2) and name (step 3) steps still cover that buffer. Net: the
    scrub strictly improves the chain's robustness — it converts a
    whole-file-corruption failure into a single-buffer fall-through.
  affected_register_entry: register/vocabulary/buffer-reincarnation-fallback-chain
  recommendation: |
    Confirmed no drift: the chain's order and membership are unchanged. The
    scrub is upstream of step 1 (at serialize time) and only ever weakens an
    already-unreadable record into the file/name fallback. Optionally note in
    the `bookmark` member's description that the record is pre-scrubbed of
    unreadable values, so a nulled load-bearing value is an expected
    fall-through trigger, not a defect.
