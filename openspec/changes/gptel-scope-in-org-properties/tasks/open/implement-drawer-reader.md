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
