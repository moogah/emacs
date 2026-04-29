---
name: migrate-session-creation-tests
description: Update session-creation-spec.el to assert on session.org drawer content instead of scope.yml
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:add-test-helper-with-scope-drawer
  - blocked-by:rewire-session-creation
---

## Cites register entries

- `register/shape/drawer-text-block` — assertions now read drawer text from the produced `session.org` rather than YAML.
- `register/boundary/scope-profile-applicator` — the producer; tests verify mode 2a's output ends up on disk verbatim.

## Files to modify
- `config/gptel/test/session-creation-spec.el` (modify) — replace `scope.yml`-on-disk assertions with `session.org` drawer assertions; remove tests that asserted on `scope.yml`-specific behavior that no longer exists.

## Implementation steps

1. Read the existing spec; the relevant `it` blocks are documented around `session-creation-spec.el:24` and `:171–:233` (per the file scan in proposal context). Each:
   - Creates a session via the creation entrypoint.
   - Asserts `scope.yml` exists at a particular path.
   - Asserts content via reading `scope.yml`.

2. For each `scope.yml` assertion, change to a `session.org` drawer assertion:

   ```elisp
   ;; Before
   (expect (file-exists-p
            (expand-file-name "branches/main/scope.yml" session-dir))
           :to-be t)

   ;; After
   (let ((session-org (expand-file-name "branches/main/session.org" session-dir)))
     (expect (file-exists-p session-org) :to-be t)
     (with-temp-buffer
       (insert-file-contents session-org)
       (org-mode)
       (expect (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ")
               :to-equal '("/expected/pattern"))))
   ```

3. Add an "and no scope.yml is written" assertion to confirm the file is not produced anywhere in the session directory:

   ```elisp
   (expect (file-exists-p
            (expand-file-name "branches/main/scope.yml" session-dir))
           :to-be nil)
   ```

   Add this to at least one creation scenario.

4. The `${project_root}` expansion test changes the same way — assert the expanded path appears under `:GPTEL_SCOPE_READ:` (or wherever the profile placed it) in the drawer.

5. The "minimal scope.yml when preset has no scope configuration" test becomes "minimal drawer". Decide what "minimal" means: per `--render-drawer-text`, an empty scope plist produces a drawer with only `:GPTEL_PRESET:` (no scope keys). That's fine — assert that no `:GPTEL_SCOPE_*` keys are present and that the loader treats this as `no_scope_config`. Or, if session creation should always emit at least a deny default, encode that decision in `--render-drawer-text` and assert it.

6. Run `./bin/run-tests.sh -d config/gptel` after migration.

## Design rationale

The session-creation tests are the integration-level proof that the profile applicator and session-creation rewires hooked up correctly. They naturally use tmpdir + real-file fixtures (per architecture.md § Testing Approach: "agent creation → tmpdir + assert file content" — same pattern applies to session creation).

The negative assertion ("no `scope.yml`") is important: it catches regressions where the YAML write step is accidentally re-introduced.

## Design pattern

Tmpdir + assert file content. Each `it` block creates its own session dir under `make-temp-file`, runs creation, asserts on the produced files. No shared session state across tests.

## Verification

- `./bin/run-tests.sh -d config/gptel/test/session-creation-spec.el` passes.
- `grep -n 'scope.yml' config/gptel/test/session-creation-spec.el` returns no results except possibly negative assertions explicitly checking the file doesn't exist.
- At least one test asserts on the drawer content via `org-entry-get-multivalued-property`.
- At least one test asserts `(not (file-exists-p ...scope.yml))`.

## Context

architecture.md § Testing Approach
specs/gptel/sessions-persistence/spec.md § MODIFIED Requirements / "Directory structure initialization", "Scope profile integration"
