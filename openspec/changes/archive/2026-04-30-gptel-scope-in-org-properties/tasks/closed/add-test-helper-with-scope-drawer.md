---
name: add-test-helper-with-scope-drawer
description: Add jf/gptel-test--with-scope-drawer macro and render-drawer helper to helpers-spec.el for unit-level drawer fixtures
change: gptel-scope-in-org-properties
status: done
relations: []
---

## Cites register entries

- `register/shape/drawer-text-block` — the helper renders one of these (the `jf/gptel-test--render-drawer` function returns the same shape `--render-drawer-text` will return in production).
- `register/vocabulary/drawer-key-set` — the helper's input alist accepts only members of this vocabulary.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/vocabularies/drawer-key-set.el` — share the closed-set check; the test helper should reject keys not in this set.

## Files to modify
- `config/gptel/scope/test/helpers-spec.el` (modify) — add a `jf/gptel-test--render-drawer` helper function and a `jf/gptel-test--with-scope-drawer` macro.

## Implementation steps

1. Locate `helpers-spec.el` (already exists; carries shared matchers and mocks).

2. Add a helper that renders a drawer block from an alist:

   ```elisp
   (defun jf/gptel-test--render-drawer (alist)
     "Render an org `:PROPERTIES:' drawer block from ALIST.
   ALIST is a list of (KEY . VALUES) entries where KEY is an org property
   keyword like :GPTEL_SCOPE_READ and VALUES is either a list (multi-value:
   first emitted as bare key, rest as KEY+) or a string (scalar). Returns
   the drawer text including trailing newline."
     (let ((lines (list ":PROPERTIES:")))
       (dolist (entry alist)
         (let ((key (car entry))
               (val (cdr entry)))
           (cond
            ((stringp val)
             (push (format "%s: %s" key val) lines))
            ((listp val)
             (let ((first t))
               (dolist (v val)
                 (push (format "%s%s: %s" key (if first "" "+") v) lines)
                 (setq first nil)))))))
       (push ":END:" lines)
       (concat (mapconcat #'identity (nreverse lines) "\n") "\n")))
   ```

3. Add a macro that wraps `with-temp-buffer` and inserts the drawer + chat-mode initial content:

   ```elisp
   (defmacro jf/gptel-test--with-scope-drawer (alist &rest body)
     "Run BODY in a temp buffer whose `:PROPERTIES:' drawer is built from ALIST.
   The buffer is in `org-mode' with point at `point-min' before BODY runs.
   ALIST has the same shape as `jf/gptel-test--render-drawer'."
     (declare (indent 1))
     `(with-temp-buffer
        (insert (jf/gptel-test--render-drawer ,alist))
        (insert "#+begin_user\n\n#+end_user\n")
        (org-mode)
        (goto-char (point-min))
        ,@body))
   ```

4. Add a tiny self-test (a single `describe`/`it` block in `helpers-spec.el` itself) that builds a drawer with one read pattern and confirms `org-entry-get-multivalued-property` recovers the list. This both exercises the helper and documents its expected behavior.

5. Run `./bin/run-tests.sh -d config/gptel/scope/test/helpers-spec.el` (or the file pattern your test runner accepts) to confirm the helper works.

## Design rationale

Per Decision in architecture.md § Testing Approach, unit tests for the drawer reader/writer should use an in-memory buffer fixture rather than tmpdir + real files. The macro is the workhorse — every reader/writer test starts with `(jf/gptel-test--with-scope-drawer ...)`. Keeping the rendering logic in a regular function (not just inside the macro) makes it usable for tests that need to compose drawer text without the temp buffer (e.g. asserting on session-creation file content).

The alist shape `(:KEY . values)` is chosen so multi-value lists and scalar strings are unambiguous; the helper distinguishes by `(stringp val)` vs `(listp val)`.

## Design pattern

Follow the existing `helpers-spec.el` style: documentation strings on every public symbol, no shadowing of built-in names, and `jf/gptel-test--` namespacing for test-only helpers (the established convention — see existing matchers in the file).

## Verification

- `./bin/run-tests.sh -d config/gptel/scope` passes (the helper's self-test runs as part of the suite).
- A quick manual smoke: in `M-x ielm`, eval `(jf/gptel-test--with-scope-drawer '((:GPTEL_SCOPE_READ . ("/a" "/b"))) (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ"))` — should return `("/a" "/b")`.

## Context

architecture.md § Testing Approach (Test Patterns, Fixture strategy)
design.md § Open Questions (round-trip behavior of `org-entry-get-multivalued-property`)

## Observations

- The existing `helpers-spec.el` uses `helpers-spec-` and `helpers-spec--`
  as its prefix convention; the project overlay (and this task body)
  prescribed the new symbols use `jf/gptel-test--`. The two prefixes now
  coexist in one file. The task body explicitly endorsed
  `jf/gptel-test--`, so I followed it; an integrator that wants
  uniformity should treat the older `helpers-spec-` prefix as the
  outlier and consider a follow-up renaming pass (out of scope here).
- The cited register entry `register/shape/drawer-text-block` declares
  `:GPTEL_PRESET:` as a required anchor in production drawer text. The
  *test fixture* drawer this helper renders is intentionally **not** a
  full production drawer — it omits `:GPTEL_PRESET:` and
  `:GPTEL_PARENT_SESSION_ID:` and only carries the keys the alist
  caller hands in. Recipients should not validate test-fixture drawers
  with `shape/validate-drawer-text-block` from the register, which
  would (correctly) flag missing anchors. This is recorded as a
  discovery (boundary-of-applicability, not a divergence) below.
- Helpers-spec.el is hand-maintained `.el` (no `.org` source), so the
  project's literate-programming rule doesn't apply. Confirmed via
  `find config/gptel/scope/test -name 'helpers*'` — only the `.el`.
- Local baseline failure count differed materially from the briefing's
  cited "24 buttercup + 10 ERT pre-existing failures." Running
  `./bin/run-tests.sh -d config/gptel/scope` against the parent commit
  produces only **2** buttercup failures (`run_bash_command: Timeout
  and resource limits …`) and **0** ERT failures. The same 2 failures
  remain post-change with no new failures introduced. The drift
  between the briefing's number and reality is worth reconciling at
  the orchestrator level.

## Discoveries

- discovery_id: disc-add-test-helper-with-scope-drawer-1
  class: deviation
  description: |
    The buttercup test runner double-counts every `describe` block
    declared inside `helpers-spec.el`. Mechanism:

    1. `jf/test-find-buttercup-test-files` matches every
       `*-spec.el` filename, including `helpers-spec.el`.
    2. The runner sorts files alphabetically (or by directory walk
       order) and `helpers-spec.el` lands LAST in the discovered
       set (file #35 of 36 for `config/gptel/scope`).
    3. Earlier spec files load `helpers-spec.el` via
       `(require 'helpers-spec (expand-file-name "helpers-spec.el" ...))`.
       Since `'helpers-spec` is not yet provided when the first spec
       file runs `require`, the FILENAME branch is taken and the file
       is loaded — registering its `describe` blocks once.
    4. After all spec files load, the runner separately calls
       `load-file` on `helpers-spec.el` itself (because of step 1).
       That re-evaluates the file body unconditionally, registering
       the `describe` blocks again.

    Net effect: every `describe` defined inside `helpers-spec.el`
    runs exactly twice. The 6 self-tests added by this task are
    counted as 12 in `Ran N specs`. All twelve still pass; the issue
    is harmless for green tests but doubles their wall-clock cost
    (~60-70ms here). It would also double-count any failure, which
    is the more concerning aspect should a self-test regress later.

    The task body explicitly requested the self-test live in
    `helpers-spec.el`, so I implemented as specified rather than
    relocating. The clean fixes are either:

    - Exclude `helpers-spec.el` from the test-file glob in
      `jf/test-find-buttercup-test-files` (e.g., add a `name !=
      helpers-spec.el` filter), so the helpers file is loaded only
      via `require`; OR
    - Move the self-tests out into a sibling file
      (e.g., `helpers-self-spec.el`) so the helpers module itself is
      load-once and registers no buttercup state.

    Either fix is a one-line change but lives in the test harness,
    not in this task's scope. Filing as a discovery for the
    integrate phase.
  affected_register_entry: (none — this is a test-harness deviation,
    not a register-cited contract)
  recommendation: |
    Open a follow-up task (in `.tasks/`, since it is cross-cutting
    and pre-dates this change) to either skip `helpers-spec.el` from
    test-file discovery or migrate the self-tests to a sibling spec
    file. The current change should not block on this — its self-test
    is functionally correct and the doubling is benign for passing
    tests.

- discovery_id: disc-add-test-helper-with-scope-drawer-2
  class: spec-signal
  description: |
    `register/shape/drawer-text-block` declares three required
    substring anchors for any drawer-text string:
    `:PROPERTIES:`, `:END:`, **and `:GPTEL_PRESET:`**. The shape
    validator
    (`shape/validate-drawer-text-block`) only checks the first two,
    not the preset anchor — so the validator is *narrower* than the
    declared `required_substring_anchors` list.

    The mismatch is invisible for production callers (who always pass
    a preset) but it surfaces as a question of intent for the test
    fixture: should `jf/gptel-test--render-drawer` enforce a
    `:GPTEL_PRESET:` line? I argue no, because the helper exists
    precisely to let scope-drawer reader/writer tests vary the
    drawer's contents without coupling to session-creation
    machinery. But this means the helper produces drawer text that
    would *fail* a strict reading of the register entry's
    `required_substring_anchors` — a soft drift between register
    declaration and validator implementation.
  affected_register_entry: register/shape/drawer-text-block
  recommendation: |
    Reconcile the register entry by either (a) tightening the
    validator to also assert the `:GPTEL_PRESET:` anchor, and
    documenting that the test helper produces a *partial* drawer
    not subject to the production shape contract; or (b) relaxing
    the entry's `required_substring_anchors` list to match what the
    validator actually checks (just `:PROPERTIES:` / `:END:`),
    treating `:GPTEL_PRESET:` as a producer-side convention rather
    than a shape invariant. Option (a) is more defensive.

    Status push-back: `register/shape/drawer-text-block` should
    move from `speculated` toward `reconciled` once the
    validator/anchors mismatch is resolved.

- discovery_id: disc-add-test-helper-with-scope-drawer-3
  class: vocabulary-mismatch
  description: |
    Minor: the alist key shape used by `jf/gptel-test--render-drawer`
    is `:GPTEL_SCOPE_READ` (a Lisp keyword — leading colon, no
    trailing colon). Production drawer keys per
    `register/vocabulary/drawer-key-set` are written with both
    leading AND trailing colons (`":GPTEL_SCOPE_READ:"`), and the
    canonical mapping function in that entry takes the *bare* key
    (no colons at all, e.g. `"GPTEL_SCOPE_READ"`) as its argument.

    So three forms of the same name circulate:
    1. Bare:    `"GPTEL_SCOPE_READ"`  — input to `--drawer-key-to-plist-path`
    2. Drawer:  `":GPTEL_SCOPE_READ:"` — as it appears literally in drawer text
    3. Lisp keyword: `:GPTEL_SCOPE_READ` — what the test helper accepts in alist keys

    These are interchangeable on rendering (since `(format "%s" :GPTEL_SCOPE_READ)`
    yields `":GPTEL_SCOPE_READ"`) but a future implementor reading the
    helper signature alongside the register entry could be confused
    about which form is canonical at the boundary.
  affected_register_entry: register/vocabulary/drawer-key-set
  recommendation: |
    When the production code lands and the canonical mapping
    function is implemented, document in the register entry which
    form is the entry's canonical "member value" (the entry
    currently uses form #2 in the `value:` field). Consider adding
    a brief note that test fixtures may use form #3 (Lisp keyword)
    as a stylistic shortcut.
