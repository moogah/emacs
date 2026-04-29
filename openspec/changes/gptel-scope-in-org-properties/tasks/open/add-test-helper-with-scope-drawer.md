---
name: add-test-helper-with-scope-drawer
description: Add jf/gptel-test--with-scope-drawer macro and render-drawer helper to helpers-spec.el for unit-level drawer fixtures
change: gptel-scope-in-org-properties
status: ready
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
