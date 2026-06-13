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
