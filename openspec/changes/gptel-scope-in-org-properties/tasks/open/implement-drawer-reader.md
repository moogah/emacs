---
name: implement-drawer-reader
description: Implement load-from-buffer, load-from-file, and load-config in scope-validation.org
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:add-drawer-encoding-contract
---

## Cites register entries

- `register/boundary/scope-config-loader` — the two-stage buffer-first / file-fallback boundary. Your three functions implement stages 1 and 2 plus the dispatcher.
- `register/shape/scope-config-plist` — the canonical loaded plist. Both stage producers must return it (or nil), exactly two top-level keys.
- `register/vocabulary/drawer-key-set` — every drawer key your reader recognises must be in this set; raising `error` on unknown keys is desirable.
- `register/invariant/scope-no-security-key-in-plist` — verify in tests that your output never carries `:security`.

Scaffolds (canonical shells you may revise):
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-config-loader.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-no-security-key-in-plist.test.el`

## Files to modify
- `config/gptel/scope/scope-validation.org` (modify) — add three new functions in a `* Drawer Reader` section.
- Tangle: `./bin/tangle-org.sh config/gptel/scope/scope-validation.org`.

## Implementation steps

1. Add `jf/gptel-scope--load-from-buffer`:

   ```elisp
   (defun jf/gptel-scope--load-from-buffer (buffer)
     "Read scope configuration from BUFFER's file-level `:PROPERTIES:' drawer.
   Returns a plist of the canonical shape (:paths (:read ... :write ... :modify
   ... :execute ... :deny ...) :cloud (:auth-detection ... :allowed-providers ...))
   with empty lists for missing list keys and \"warn\" as the default for
   missing :auth-detection. Does not consult the file on disk; the buffer is
   the source of truth."
     (with-current-buffer buffer
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (let ((auth (or (org-entry-get (point) "GPTEL_SCOPE_CLOUD_AUTH") "warn")))
             (unless (member auth '("allow" "warn" "deny"))
               (error "Scope schema: GPTEL_SCOPE_CLOUD_AUTH must be \"allow\", \"warn\", or \"deny\", got %S" auth))
             (list :paths
                   (list :read    (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_READ")
                         :write   (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_WRITE")
                         :modify  (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_MODIFY")
                         :execute (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_EXECUTE")
                         :deny    (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_DENY"))
                   :cloud
                   (list :auth-detection auth
                         :allowed-providers
                         (org-entry-get-multivalued-property
                          (point) "GPTEL_SCOPE_CLOUD_PROVIDERS"))))))))
   ```

2. Add `jf/gptel-scope--load-from-file`:

   ```elisp
   (defun jf/gptel-scope--load-from-file (file)
     "Read scope configuration from FILE's `:PROPERTIES:' drawer headlessly.
   Same return shape as `jf/gptel-scope--load-from-buffer'. Used when no live
   chat buffer is available for the session."
     (with-temp-buffer
       (insert-file-contents file)
       (org-mode)
       (jf/gptel-scope--load-from-buffer (current-buffer))))
   ```

3. Add `jf/gptel-scope--load-config`:

   ```elisp
   (defun jf/gptel-scope--load-config (&optional branch-dir)
     "Resolve scope configuration for the current session.
   Buffer-first: if a chat buffer exists for BRANCH-DIR (default: buffer-local
   `jf/gptel--branch-dir'), reads its drawer. Otherwise reads the file at
   <branch-dir>/session.org. Returns nil when no scope keys are present in
   the resolved drawer (caller treats this as `no_scope_config')."
     (let* ((dir (or branch-dir
                     (and (boundp 'jf/gptel--branch-dir) jf/gptel--branch-dir)
                     default-directory))
            (buffer (jf/gptel-scope--find-session-buffer-for-dir dir))
            (file (expand-file-name "session.org" dir))
            (config (cond
                     (buffer (jf/gptel-scope--load-from-buffer buffer))
                     ((file-exists-p file) (jf/gptel-scope--load-from-file file))
                     (t nil))))
       (and config
            (jf/gptel-scope--has-any-scope-key-p config)
            config)))
   ```

4. Add the two helpers used above:

   ```elisp
   (defun jf/gptel-scope--find-session-buffer-for-dir (dir)
     "Find a live chat buffer whose `jf/gptel--branch-dir' equals DIR.
   Returns the buffer or nil. Uses the session registry when available."
     (and (fboundp 'jf/gptel-session-find)
          (let ((entry (and dir (assoc dir (and (boundp 'jf/gptel--session-registry)
                                                jf/gptel--session-registry)))))
            ;; Fallback: scan buffer list for one with matching branch-dir.
            (or (and entry (plist-get (cdr entry) :buffer))
                (cl-loop for buf in (buffer-list)
                         when (and (buffer-local-value 'jf/gptel--branch-dir buf)
                                   (string= dir (buffer-local-value 'jf/gptel--branch-dir buf)))
                         return buf)))))

   (defun jf/gptel-scope--has-any-scope-key-p (config)
     "Return non-nil if CONFIG carries any non-empty scope list or non-default cloud."
     (let ((paths (plist-get config :paths))
           (cloud (plist-get config :cloud)))
       (or (plist-get paths :read)
           (plist-get paths :write)
           (plist-get paths :modify)
           (plist-get paths :execute)
           (plist-get paths :deny)
           (plist-get cloud :allowed-providers)
           ;; A non-default auth value also indicates intent.
           (let ((auth (plist-get cloud :auth-detection)))
             (and auth (not (string= auth "warn")))))))
   ```

5. The registry lookup in step 4 is best-effort. If `jf/gptel--session-registry` is keyed differently in the live code (it's keyed by `"session-id/branch-name"` per `sessions-persistence.md`), adapt the lookup to scan registry values for matching `:branch-dir`. The fallback buffer-list scan covers all cases.

6. Do NOT yet replace the validator's existing config-load call site — that happens in `rewire-validator-config-load`. Land this task with the new functions defined and unused.

7. Tangle and run `./bin/run-tests.sh -d config/gptel/scope` — should still pass (new functions are not yet wired in).

## Design rationale

Per Decision 2 in design.md, buffer-first is required for WYSIWYG behavior — the user's just-typed drawer edit must be visible to validation before they save. The file-fallback path covers programmatic callers like `request_scope_expansion` from outside chat-buffer context.

Extracting `--load-from-buffer` and `--load-from-file` separately makes them individually unit-testable: the buffer path uses the test helper from `add-test-helper-with-scope-drawer`; the file path uses tmpdir fixtures.

The `--has-any-scope-key-p` check preserves the existing `no_scope_config` semantics: a `session.org` with only `:GPTEL_PRESET:` and no scope keys is treated as "no scope configured" (the on-deny path bypasses the expansion UI), matching today's "no scope.yml file exists" behavior.

## Design pattern

The reader uses `org-entry-get` and `org-entry-get-multivalued-property` against `(point)` after `(goto-char (point-min))`. This targets the file-level drawer (the one before any heading), which is how `:GPTEL_PRESET:` is already accessed in `chat/menu.el`.

Existing reader convention to follow: see `gptel-chat--declared-preset` in `config/gptel/chat/menu.el` for the file-level drawer access pattern (with `save-excursion`, `save-restriction`, and `widen`).

## Verification

- `./bin/tangle-org.sh config/gptel/scope/scope-validation.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/scope` passes (new functions not yet referenced; no regression).
- Manual smoke: in ielm with the test helper loaded, eval `(jf/gptel-test--with-scope-drawer '((:GPTEL_SCOPE_READ . ("/x/**")) (:GPTEL_SCOPE_CLOUD_AUTH . "deny")) (jf/gptel-scope--load-from-buffer (current-buffer)))` — should return a plist with `:read ("/x/**")` and `:auth-detection "deny"`.

## Context

design.md § Decision 2 (Buffer-first read with file fallback)
design.md § Decision 3 (no `:security` in plist)
architecture.md § Components, § Interfaces (Buffer-vs-file resolution)
specs/gptel/scope/spec.md § MODIFIED Requirements / "Scope configuration loading", "Scope configuration shape"

## Observations

- Verified `register/boundary/scope-config-loader` round-trip semantics by smoke
  test: reading the same drawer text via `--load-from-buffer` (in a live
  org-mode buffer) and `--load-from-file` (via `with-temp-buffer` +
  `insert-file-contents`) produces equal plists. The two paths share a single
  reader implementation, so the byte-for-byte round-trip invariant is
  trivially satisfied — `--load-from-file` is a thin wrapper around
  `--load-from-buffer`.
- Verified `register/invariant/scope-no-security-key-in-plist` by smoke
  test: `(plist-member result :security)` returns nil for both populated and
  empty drawers. The reader has no path that produces `:security`.
- Followed the existing `gptel-chat--declared-preset` convention from
  `config/gptel/chat/menu.el` for file-level drawer access (`save-excursion`
  + `save-restriction` + `widen` + `goto-char point-min`). This is the same
  pattern that already reads `:GPTEL_PRESET:` from session.org.
- The session registry (`jf/gptel--session-registry`) is a hash table keyed
  by `"session-id/branch-name"` (per `config/gptel/sessions/registry.org`),
  *not* an alist keyed by branch-dir. The task body's `(assoc dir
  jf/gptel--session-registry)` lookup wouldn't have found anything; rewrote
  the helper to scan registry *values* via `maphash` for a matching
  `:branch-dir`, then fall back to a `seq-find` over the buffer list. The
  task itself anticipated this in step 5.
- Used `seq-find` in preference to `cl-loop` for the fallback buffer-list
  scan, per the `seq.el`-over-`cl-lib` project convention noted in
  `.claude/orchestrator/roles/implementor.md`.
- Closed scalar value set for `GPTEL_SCOPE_CLOUD_AUTH` is enforced at read
  time with a structured error rather than silent fallback. This matches
  `register/vocabulary/drawer-key-set`'s `allowed_scalar_values:
  ["allow", "warn", "deny"]` clause and gives the rewire task a deterministic
  error shape to surface to the user.

## Discoveries

- discovery_id: disc-implement-drawer-reader-1
  class: interface-drift
  description: |
    The task body and `architecture.md` § Components both name the new
    top-level dispatcher `jf/gptel-scope--load-config`. But a function with
    that exact name already exists at `config/gptel/scope/scope-validation.org`
    line 503 (the YAML-based loader) and is the live producer of the
    scope-config plist consumed by `jf/gptel-scope-authorize-tool-call`. The
    task simultaneously asks (step 6) to "land this task with the new
    functions defined and unused" — which is unsatisfiable if the new
    `--load-config` is defined under that exact name in the same file (the
    second `defun` would silently replace the first, rewiring the
    validator's loader path before `rewire-validator-config-load` runs).

    To satisfy "defined and unused" honestly, I introduced the new dispatcher
    under the holding name `jf/gptel-scope--load-config-from-drawer` and
    documented in the org file (`* Drawer Reader / Dispatcher (held back for
    rewire)`) that the rewire task is responsible for either renaming this
    function back to `--load-config` or replacing the YAML body of the
    existing `--load-config` with a delegation to this one. The four other
    requested functions (`--load-from-buffer`, `--load-from-file`,
    `--find-session-buffer-for-dir`, `--has-any-scope-key-p`) landed under
    their requested names because no such collision exists.

    `register/shape/scope-config-plist` and `register/boundary/scope-config-
    loader` both list `jf/gptel-scope--load-config` as a producer. After this
    task lands, the producer of the *new* shape is technically `--load-config-
    from-drawer`; the producer of the *old* shape (`:paths` + `:cloud` +
    `:security`) is still `--load-config`. The register entries are
    speculated and load_bearing — they should either name both functions
    (with a status note that the holding name is transient) or wait for the
    rewire task to rename and become accurate.

  affected_register_entry: register/boundary/scope-config-loader
  recommendation: |
    The simplest fix is for `rewire-validator-config-load` to rename
    `jf/gptel-scope--load-config-from-drawer` back to
    `jf/gptel-scope--load-config` (deleting the YAML-based body in the same
    diff), at which point the register entries become accurate. No register
    edit is needed in the interim; flag this in PM digest as a transient
    naming state. Alternatively, if the orchestrator prefers, the
    `producers:` list for both `register/shape/scope-config-plist` and
    `register/boundary/scope-config-loader` could be temporarily annotated
    "(holding name `--load-config-from-drawer` until rewire-validator-
    config-load lands)". Either resolution is fine; pushing back here so the
    inconsistency between task body, architecture, and live code is visible
    rather than buried.

- discovery_id: disc-implement-drawer-reader-2
  class: spec-signal
  description: |
    Smoke-testing exposed a behaviour the speculated invariant
    `register/invariant/scope-no-security-key-in-plist` covers but doesn't
    quite name: the canonical empty/default plist (no `:GPTEL_SCOPE_*` keys
    in the drawer at all) still returns *a plist* — `(:paths (:read nil ...
    :deny nil) :cloud (:auth-detection "warn" :allowed-providers nil))` —
    rather than nil. The dispatcher (`--load-config-from-drawer`) layers
    `--has-any-scope-key-p` on top of `--load-from-buffer` to convert that
    "all-empty plist" into nil, which is what callers expect for the
    `no_scope_config` deny path.

    The invariant's enforcement_mechanism prose says tests should assert
    `(null (plist-member result :security))`. That's correct, but it's
    reachable only at the `--load-from-buffer` / `--load-from-file` layer,
    not at `--load-config` (which returns nil for the all-empty case before
    the consumer ever gets to inspect the plist). The forthcoming spec
    file (per architecture.md `drawer/load-from-buffer-spec.el`) should
    pin both layers separately:
    - layer 1 (stage producers): plist returned, no `:security` key
    - layer 2 (dispatcher): empty drawer ⇒ nil; populated drawer ⇒ plist with
      no `:security` key

  affected_register_entry: register/invariant/scope-no-security-key-in-plist
  recommendation: |
    When the spec is written (separate task: I did not file a new test spec
    in this implementation per the "defined but unused" framing), the
    enforcement_mechanism prose for this invariant could be tightened to
    state explicitly that the test fixtures the populated case at the
    `--load-from-buffer` / `--load-from-file` layer (where every drawer
    returns a plist) and the empty case at the `--load-config` layer (where
    nil is the legitimate output). No code change needed; this is a
    spec-precision recommendation.

- discovery_id: disc-implement-drawer-reader-3
  class: vocabulary-mismatch
  description: |
    The earlier-cycle `add-drawer-encoding-contract` task's notes flagged
    the `register/vocabulary/drawer-key-set` entry as `divergent`:
    "colonised vs bare form mixing". I confirmed via implementation: the
    correct API form for `org-entry-get` and
    `org-entry-get-multivalued-property` is the *bare* string
    (`"GPTEL_SCOPE_READ"`); the colonised form (`":GPTEL_SCOPE_READ:"`) is
    the drawer-text literal a user types and what `jf/gptel-test--render-
    drawer` emits. The register entry's `members:` list mixes both forms
    in a way that's read-correct but easy to miscall.

    No code in this task uses the wrong form (I used the bare form
    consistently per the orchestrator hand-off note). Recording this as a
    follow-on signal so the eventual `status: confirmed` flip on
    `register/vocabulary/drawer-key-set` includes a clean separation
    between the wire form (drawer text) and the API form (function
    arguments).

  affected_register_entry: register/vocabulary/drawer-key-set
  recommendation: |
    When `register/vocabulary/drawer-key-set` is reconciled, split the
    `members:` list into two parallel keys: `wire_form:` (with the
    colons, what the user types and what `--render-drawer` emits) and
    `api_form:` (without the colons, what `org-entry-get` and
    `org-entry-get-multivalued-property` expect). The
    `canonical_mapping_function` already uses the bare form correctly;
    tightening the schema closes the residual ambiguity.
