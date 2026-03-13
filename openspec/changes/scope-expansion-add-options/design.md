## Context

The expansion menu lives in `config/gptel/scope/scope-expansion.org` (generated to `scope-expansion.el`). It is a `transient-define-prefix` with two groups: a primary group (deny / add-to-scope / allow-once) and a secondary group (edit-scope / cancel). Each choice is a `defun` that reads `(transient-scope)` for violation info and callback, performs its action, invokes the callback, and quits via `transient-quit-one`.

The `jf/gptel-scope--add-to-scope` defun is the reference implementation:
1. Read `resource`, `validation-type`, `tool`, `callback` from transient scope
2. Get + validate the scope file path
3. Route to the right updater (`-add-path-to-scope`, `-add-pattern-to-scope`, `-add-bash-to-scope`) via `pcase` on `validation-type`
4. Invoke callback with `:success t :patterns_added <vector>`
5. Call `transient-quit-one`

Both new suffixes mirror this exactly, differing only in how `resource` is transformed before step 3.

## Goals / Non-Goals

**Goals:**
- Add "Add `/**` to scope" suffix: derives wildcard pattern from resource, writes it via existing routing
- Add "Add custom pattern to scope" suffix: prompts user with pre-populated `read-string`, writes edited value via existing routing
- Update transient menu to show both new choices alongside existing ones
- Add Buttercup tests for both new suffixes and the wildcard helper

**Non-Goals:**
- Changing updater functions (`-add-path-to-scope`, etc.)
- Validating user-entered custom patterns
- Adding new transient infix variables or changing how transient scope is structured
- Changing the callback shape beyond what's already established

## Decisions

### Decision: `read-string` over transient infix for custom edit

**Chosen:** `read-string` with pre-populated default value.

**Alternatives considered:**
- Transient infix variable: requires a two-step flow (set the infix, then a separate "confirm" suffix), adds menu complexity, and transient's infix variables don't support async or default-from-plist naturally.
- Custom transient class: significant implementation overhead for a single use case.

**Why `read-string`:** Already used elsewhere in the codebase for prompted input. Handles C-g cancellation via the `quit` condition. Pre-populating with `(read-string "Pattern: " resource)` gives users exactly what the spec requires with minimal code.

**Cancellation handling:** Wrap the `read-string` call in `(condition-case nil ... (quit nil))`. If `nil` returned, invoke callback with `:success nil :user_denied t` and quit — same shape as deny.

### Decision: Wildcard derivation logic and applicability

**Only for file resources.** The wildcard option is only meaningful when the denied resource is a file path. If the LLM requested access to a directory, the user already has the exact add option available and the wildcard option would be redundant (directory `/**` is what they'd add anyway). The menu suffix is conditionally shown based on `(file-directory-p resource)`.

**Chosen:** Pure helper `jf/gptel-scope--parent-wildcard-for (resource)`:
```elisp
(defun jf/gptel-scope--parent-wildcard-for (resource)
  "Return a parent-directory wildcard pattern for a RESOURCE file path.
RESOURCE must be a file path (not a directory)."
  (concat (string-remove-suffix "/" (file-name-directory resource)) "/**"))
```

For `~/foo/bar.txt` → `~/foo/**`
For `/Users/jeff/project/src/init.el` → `/Users/jeff/project/src/**`

This is intentionally simple: `file-name-directory` reliably strips the filename component. The `string-remove-suffix "/"` normalizes the directory path before appending `/**`.

**Dynamic menu label:** The wildcard suffix description is computed at display time to show the actual derived pattern — e.g., `"Add ~/foo/** to scope"` rather than a generic label. Transient supports this via a lambda in the `:description` slot of the suffix definition:

```elisp
("w" (lambda ()
       (let* ((resource (plist-get (plist-get (transient-scope) :violation) :resource))
              (pattern (jf/gptel-scope--parent-wildcard-for resource)))
         (format "Add %s to scope" pattern)))
     jf/gptel-scope--add-wildcard-to-scope
     :transient nil)
```

**Menu conditionality:** The wildcard suffix is disabled (or hidden) when the resource is a directory. Implementation approach: check `(not (file-directory-p resource))` in a `:if` predicate on the transient suffix, or conditionally include it via a helper that builds the group dynamically. The simplest approach is a `:predicate` in the transient suffix definition.

### Decision: Callback `:patterns_added` reflects derived pattern, not original LLM patterns

The existing `jf/gptel-scope--add-to-scope` returns `(plist-get scope :patterns)` — the original LLM-provided patterns list — as `:patterns_added`. This is technically inaccurate for wildcard/custom cases where the written pattern differs from what the LLM requested.

**Chosen:** Both new suffixes return a vector containing the pattern that was *actually written* (the derived wildcard or edited custom string). This is more accurate and more useful for the LLM to understand what changed.

### Decision: Single new transient group for variants

**Menu layout change:**

```
;; Current
[("d" "Deny..."        ...)
 ("a" "Add to scope..."...)
 ("o" "Allow once..."  ...)]
[""
 ("e" "Edit scope..."  ...)
 ("q" "Cancel"         ...)]

;; New
[("d" "Deny..."              ...)
 ("a" "Add exact to scope..."...)
 ("w" "Add /**  to scope..."  ...)
 ("c" "Add custom pattern..." ...)
 ("o" "Allow once..."         ...)]
[""
 ("e" "Edit scope manually"  ...)
 ("q" "Cancel"               ...)]
```

Keys: `w` for wildcard (mnemonic: **w**ildcard), `c` for custom. These are currently unused in the menu.

## Risks / Trade-offs

**`file-directory-p` runtime check** → If the denied path doesn't exist on disk, wildcard derivation falls back to `file-name-directory`. This can produce an unexpected parent wildcard for a path the user intended as a directory. **Mitigation:** Custom pattern option is always available for correction; the wildcard result is shown in the success message.

**Menu growing to 5 primary choices** → Slightly more visual noise. **Mitigation:** Grouping remains logical (all scope-add variants adjacent), and transient menus handle 5–6 choices cleanly.

**Duplicate `pcase` routing in each suffix** → Three near-identical routing blocks (exact, wildcard, custom). **Mitigation:** Extract a private helper `jf/gptel-scope--write-pattern-to-scope (pattern validation-type tool scope-file)` that encapsulates the pcase. All three suffixes call this helper. This reduces the duplication to one location and makes adding future variants trivial.

## Open Questions

- Should the wildcard suffix show what pattern it derived (e.g., "Added ~/foo/** to scope") in the echo area, similar to how the exact suffix shows the resource? → **Suggested yes** — surfacing the derived pattern helps users understand what was actually written.
- Should the custom prompt label include the current resource for context, or just a generic "Pattern: " prompt? → **Suggested**: `(format "Add pattern [%s]: " resource)` as the prompt string makes the default visible even before the user starts editing.
