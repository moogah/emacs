---
name: implement-profile-drawer-applicator
description: Add render-drawer-text and apply-to-drawer in scope-profiles.org; replace --write-scope-yml
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:add-drawer-encoding-contract
---

## Cites register entries

- `register/boundary/scope-profile-applicator` — the two-mode applicator (string-mode `render-drawer-text` and buffer-mode `apply-to-drawer`). Your additions implement modes 2a and 2b; `--create-for-session` is the dispatcher (stage 1 is unchanged profile resolution).
- `register/shape/drawer-text-block` — the string `--render-drawer-text` produces. Honour the structural invariants (single `:PROPERTIES:`, single `:END:`, trailing newline).
- `register/shape/scope-config-plist` — the input shape (post-:security-removal).
- `register/vocabulary/drawer-key-set` — the keys you emit in the rendered drawer text.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-profile-applicator.el`

## Files to modify
- `config/gptel/scope-profiles.org` (modify) — add `--render-drawer-text` and `--apply-to-drawer`; remove `--write-scope-yml` and the YAML emitter helpers it depends on.
- Tangle: `./bin/tangle-org.sh config/gptel/scope-profiles.org`.

## Implementation steps

1. Add `jf/gptel-scope-profile--render-drawer-text` (returns the drawer block as a string):

   ```elisp
   (defun jf/gptel-scope-profile--render-drawer-text (preset-name parent-session-id scope-plist)
     "Render a `:PROPERTIES:' drawer block as a string for prepending to a new session.org.
   PRESET-NAME (symbol or string) becomes :GPTEL_PRESET:. PARENT-SESSION-ID
   (string or nil) becomes :GPTEL_PARENT_SESSION_ID: when non-nil. SCOPE-PLIST
   has the canonical shape (:paths (...) :cloud (...)). Empty path lists are
   omitted from the output; a non-default cloud auth or non-empty providers
   are emitted. The default cloud auth (\"warn\") with no providers is emitted
   only when no other scope key is present so the drawer carries at least one
   :GPTEL_SCOPE_* line (avoiding the `no_scope_config' branch on load)."
     (let* ((paths (plist-get scope-plist :paths))
            (cloud (plist-get scope-plist :cloud))
            (lines (list ":PROPERTIES:")))
       (when preset-name
         (push (format ":GPTEL_PRESET: %s" preset-name) lines))
       (when parent-session-id
         (push (format ":GPTEL_PARENT_SESSION_ID: %s" parent-session-id) lines))
       (dolist (op '((:read    . "GPTEL_SCOPE_READ")
                     (:write   . "GPTEL_SCOPE_WRITE")
                     (:modify  . "GPTEL_SCOPE_MODIFY")
                     (:execute . "GPTEL_SCOPE_EXECUTE")
                     (:deny    . "GPTEL_SCOPE_DENY")))
         (let ((vals (plist-get paths (car op)))
               (key (cdr op))
               (first t))
           (dolist (v vals)
             (push (format "%s%s: %s" key (if first ":" "+:") v) lines)
             (setq first nil))))
       (let ((auth (plist-get cloud :auth-detection))
             (providers (plist-get cloud :allowed-providers)))
         (when (and auth (not (string= auth "warn")))
           (push (format ":GPTEL_SCOPE_CLOUD_AUTH: %s" auth) lines))
         (let ((first t))
           (dolist (p providers)
             (push (format "GPTEL_SCOPE_CLOUD_PROVIDERS%s: %s" (if first ":" "+:") p) lines)
             (setq first nil))))
       (push ":END:" lines)
       (concat (mapconcat #'identity (nreverse lines) "\n") "\n")))
   ```

   Note: the line constructed for `GPTEL_SCOPE_CLOUD_PROVIDERS` should produce `:GPTEL_SCOPE_CLOUD_PROVIDERS:` for the first value and `:GPTEL_SCOPE_CLOUD_PROVIDERS+:` for subsequent. Adjust the format string in the implementation if the prefix colon needs adding (the snippet above is illustrative; verify the exact format produces a valid drawer line — best done with a unit test in `migrate-validation-tests` or a quick ielm smoke check).

2. Add `jf/gptel-scope-profile--apply-to-drawer` (mutates an existing buffer):

   ```elisp
   (defun jf/gptel-scope-profile--apply-to-drawer (buffer scope-plist)
     "Apply SCOPE-PLIST to BUFFER's `:PROPERTIES:' drawer at point-min.
   Uses `org-entry-put' / `org-entry-put-multivalued-property' to write each
   :GPTEL_SCOPE_* key. Existing non-scope drawer keys (:GPTEL_PRESET:,
   :GPTEL_PARENT_SESSION_ID:) are preserved. Empty path lists clear the
   corresponding key (writing nil removes it). Saves the buffer."
     (with-current-buffer buffer
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (let ((paths (plist-get scope-plist :paths))
                 (cloud (plist-get scope-plist :cloud)))
             (dolist (op '((:read    . "GPTEL_SCOPE_READ")
                           (:write   . "GPTEL_SCOPE_WRITE")
                           (:modify  . "GPTEL_SCOPE_MODIFY")
                           (:execute . "GPTEL_SCOPE_EXECUTE")
                           (:deny    . "GPTEL_SCOPE_DENY")))
               (let ((vals (plist-get paths (car op))))
                 (when vals
                   (apply #'org-entry-put-multivalued-property (point) (cdr op) vals))))
             (when-let ((auth (plist-get cloud :auth-detection)))
               (unless (string= auth "warn")
                 (org-entry-put (point) "GPTEL_SCOPE_CLOUD_AUTH" auth)))
             (when-let ((providers (plist-get cloud :allowed-providers)))
               (apply #'org-entry-put-multivalued-property (point) "GPTEL_SCOPE_CLOUD_PROVIDERS" providers)))
           (save-buffer)))))
   ```

3. Update `jf/gptel-scope-profile--create-for-session` to return drawer text instead of writing `scope.yml`. The signature stays the same; the body resolves the profile, expands variables, and returns the rendered drawer string. Delete the `--write-scope-yml` call and the YAML I/O.

4. Delete `jf/gptel-scope-profile--write-scope-yml` and any YAML emitter helpers it depends on (`--scope-plist-to-yaml`, `--emit-yaml-list`, etc., depending on what's there). Delete the `(require 'jf-gptel-scope-yaml ...)` line at the top of `scope-profiles.el` (it stops being used here once the YAML writer is gone).

5. Do NOT yet rewire `sessions/commands.org` or `persistent-agent.el` — those are separate tasks (`rewire-session-creation`, `rewire-persistent-agent`). At the end of this task, `--create-for-session` returns drawer text but its callers still expect the old YAML-write side effect; tests will fail until rewires land. Mark expected failures or temporarily keep `--write-scope-yml` as a thin stub that errors with "use --apply-to-drawer or render-drawer-text" so callers fail loudly until rewired.

6. Tangle and run `./bin/run-tests.sh -d config/gptel/scope-profiles` (or the directory the profile tests live in). Expected: profile tests pass for the new functions; session-creation tests fail until `rewire-session-creation` lands. Document the expected test failures as part of this task's exit criteria.

## Design rationale

Per Decision 5 in design.md, agent creation embeds drawer text in initial content (no headless buffer dance), so the renderer is the primary export. Per Decision 4, the in-place applicator (`--apply-to-drawer`) is needed for the case where a buffer is already open and we want to update its drawer without reconstructing the file.

Splitting render (string) from apply (buffer mutation) lets each caller pick the right tool: session creation embeds in initial content, persistent-agent embeds in initial content, expansion-UI add-to-scope mutates a live buffer (via the writer task, not this one), and any future "reset scope from preset" command applies to a buffer.

## Design pattern

Follow the existing `scope-profiles.el` style: pure functions for transformation (`--load`, `--resolve`, `--expand-variables`, `--deep-merge`) and one terminal effect function (`--apply-to-drawer` replaces `--write-scope-yml`). The renderer is also pure (returns a string), keeping the I/O surface area at one function.

## Verification

- `./bin/tangle-org.sh config/gptel/scope-profiles.org` succeeds.
- New functions exist: `(fboundp 'jf/gptel-scope-profile--render-drawer-text)` and `(fboundp 'jf/gptel-scope-profile--apply-to-drawer)` both return `t`.
- `--write-scope-yml` is gone (or is a stub that signals an error if called).
- Profile-resolution tests still pass (the resolution / expansion / deep-merge logic is unchanged).
- Session-creation tests fail loudly (expected — they will be fixed in `rewire-session-creation`).

## Context

design.md § Decision 5 (Persistent-agent embeds drawer in initial content)
design.md § Decision 6 (Profile templates stay YAML on disk)
architecture.md § Components, § Interfaces
specs/gptel/scope-profiles/spec.md § MODIFIED Requirements / "Mutable scope drawer in session.org", "Integration with session creation"
specs/gptel/scope-profiles/spec.md § REMOVED Requirements / "scope.yml writer"

## Observations

- Implemented `jf/gptel-scope-profile--render-drawer-text` (Mode 2a) and
  `jf/gptel-scope-profile--apply-to-drawer` (Mode 2b) in
  `config/gptel/scope-profiles.org`. Tangled cleanly via
  `./bin/tangle-org.sh`.
- Replaced `--write-scope-yml` with a stub that signals an error referencing
  the canonical replacements; this makes unmigrated callers fail loudly.
- Deleted `--plist-to-yaml` (the only YAML emitter helper that existed in
  this file — there was no separate `--scope-plist-to-yaml`/`--emit-yaml-list`
  pair as the task scaffold suggested).
- Removed the `(require 'jf-gptel-scope-yaml ...)` line per task step 4.
  Added `(require 'org)` since `--apply-to-drawer` calls `org-entry-put`.
- Updated `--create-for-session` to *return* the rendered drawer text (Mode
  2a). Public arg list is unchanged except for an additional optional
  `parent-session-id` argument (the renderer needs it; existing callers will
  pass it once `rewire-session-creation` lands). This is a pure additive
  change to a `&optional` tail and so does not break the existing call from
  `sessions/commands.el` (which fills it with nil implicitly).
- Smoke-tested both new functions via batch `emacs --eval` against
  representative inputs (preset-only, preset+parent+cloud, multi-value paths,
  spaces in values). Round-trip through `org-entry-get-multivalued-property`
  succeeds; the cross-mode idempotency invariant holds (rendered text +
  re-applied = identical buffer).

### Renderer line shape (resolved)

The illustrative scaffold suggested rendering multi-value lists as one
line per value (`:KEY: v0\n:KEY+: v1\n…`). I switched to the single
space-separated line shape (`:KEY: v0 v1 v2`) because that is the
literal text `org-entry-put-multivalued-property` produces. Mixing the
two shapes breaks the cross-mode idempotency invariant: when applied to
text rendered in the multi-line `+:` shape, `org-entry-put` rewrites
the *first* line in place but leaves the `:KEY+:` continuations
untouched, so the buffer ends up with both the new collapsed line and
stale continuation lines. Single-line rendering eliminates this drift.
Spaces inside values are escaped via `org-entry-protect-space`,
matching the on-the-wire format `org-entry-get-multivalued-property`
expects.

### Expected test failures (for `gptel-scope-in-org-properties` integrator)

Baseline (pre-task) failure count from `./bin/run-tests.sh -d config/gptel`:
ERT 22/23, Buttercup 1027/1050. After this task: ERT 22/23, Buttercup
1014/1050. Net delta: 0 ERT, +13 Buttercup failures.

The 13 new Buttercup failures break down as:

**Five session-creation tests** (caller still expects `scope.yml` side
effect; will be fixed by `rewire-session-creation`):

- `Session creation (write-side) Scope profile integration writes scope.yml via scope profile system`
- `Session creation (write-side) Scope profile integration writes scope.yml with preset scope configuration when available`
- `Session creation (write-side) Scope profile integration expands ${project_root} in scope.yml paths when project-root provided`
- `Session creation (write-side) Scope profile integration deep-merges worktree-paths with preset scope when both provided`
- `Session creation (write-side) Scope profile integration writes minimal scope.yml when preset has no scope configuration`

**Eight YAML boolean-normalization tests** (test the now-deleted
`--plist-to-yaml`; the entire `config/gptel/scope/test/yaml/` directory
is removed by `delete-yaml-and-security-residue`):

- `YAML Boolean Normalization jf/gptel-scope-profile--plist-to-yaml with keyword booleans writes :true as 'true' in YAML`
- `YAML Boolean Normalization jf/gptel-scope-profile--plist-to-yaml with keyword booleans writes :false as 'false' in YAML`
- `YAML Boolean Normalization jf/gptel-scope-profile--plist-to-yaml with keyword booleans writes :null as 'false' in YAML`
- `YAML Boolean Normalization jf/gptel-scope-profile--plist-to-yaml with keyword booleans writes t as 'true' in YAML`
- `YAML Boolean Normalization jf/gptel-scope-profile--plist-to-yaml with keyword booleans writes nil boolean fields as 'false' in YAML`
- `YAML Boolean Normalization jf/gptel-scope-profile--plist-to-yaml with keyword booleans handles nested plists with keyword booleans`
- `YAML Boolean Normalization YAML round-trip with boolean values preserves YAML true through parse → normalize → write`
- `YAML Boolean Normalization YAML round-trip with boolean values preserves YAML false through parse → normalize → write`

The other 23 buttercup failures (Bug 4 / Bug multi-violation / Parallel
tool callback / run_bash_command integration / run_bash_command Timeout)
and the 1 ERT failure (`test-directory-creation-org-session-structure`)
are *pre-existing* — they are present on baseline 538ed48 and unaffected
by this task.

## Discoveries

- class: invariant-gap
  signal: |
    The `register/shape/drawer-text-block` validator (in
    `interfaces.org`'s required_substring_anchors) lists `:GPTEL_PRESET:`
    as a required anchor, but the test helper
    `jf/gptel-test--render-drawer` emits a bare `:PROPERTIES:\n:END:\n`
    block as legal output — i.e. the helper-spec tests document a case
    where the anchor is absent and treat that as valid. The renderer in
    this task can also emit a `:GPTEL_PRESET:`-less block when called
    with `preset-name = nil` (defensive branch). Either the validator's
    required-anchor list should be relaxed, or the renderer / helper
    should reject preset-less inputs. Flagging per the task brief; not
    resolved here because either fix touches register entries and the
    task says don't modify the register directly.
  recommendation: |
    Architect should decide. If the answer is "always require
    GPTEL_PRESET", make `(when preset-name ...)` an unconditional
    `(if preset-name … (error …))` and tighten the helper.
- class: deviation
  signal: |
    The illustrative renderer snippet in step 1 of the task body produces
    drawer lines without a leading colon (`KEY: value` rather than
    `:KEY: value`) and uses `+:` as the multi-value suffix prefix
    (`KEY+:`). The actual drawer line format demands a leading colon
    (`:KEY: value` and `:KEY+: value`), which I implemented; I also
    deviated from the per-value-on-its-own-line approach in favour of
    a single space-separated line per key (see Observations). Both
    deviations are explicit in the literate org's body; integrator can
    sanity-check.
  recommendation: |
    None — both deviations match the org-property reader contract and
    keep the cross-mode idempotency invariant intact.
- class: dead-branch
  signal: |
    `config/gptel/scope/test/yaml/boolean-normalization-spec.el` tests
    `jf/gptel-scope-profile--plist-to-yaml` (deleted in this task) and
    references `:security` (forbidden by
    `register/shape/scope-config-plist`). Eight tests in this file fail
    after this task. The cleanup is owned by
    `delete-yaml-and-security-residue` (see its step 1: `git rm -r
    config/gptel/scope/test/yaml/`). Listed under "expected failures"
    in Observations so the integrator can confirm the cleanup task
    picks them up.
  recommendation: |
    Confirm `delete-yaml-and-security-residue` runs after this task and
    removes `config/gptel/scope/test/yaml/` wholesale.
- class: interface-drift
  signal: |
    `--create-for-session`'s public signature now includes a fifth
    optional argument `parent-session-id`. Existing callers in
    `sessions/commands.el` and any other cite never pass it; they bind
    by position so adding a `&optional` tail doesn't break them. But
    the actual rewires (`rewire-session-creation`,
    `rewire-persistent-agent`) will need to *start* passing this arg
    so the rendered drawer carries the parent-session line for agent
    sessions. Calling out so the rewire tasks aren't surprised.
  recommendation: |
    Both rewire tasks should explicitly pass parent-session-id where
    they currently have it locally (e.g. agent creation already knows
    its parent; standalone session creation passes nil).
