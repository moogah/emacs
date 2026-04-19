# Handoff: Session property drawer corruption on `gptel-org-mode-sessions`

## Symptom

Persistent org-mode session files accumulate malformed property drawer state as the session is saved. Two observable failure modes in the same file:

1. **Stacked `:PROPERTIES:` drawers at the top of the buffer.** Instead of one file-level drawer, multiple appear in sequence — some empty, some holding one property each.
2. **Concatenated `:GPTEL_BOUNDS:` entries at the end of the buffer**, outside any drawer, on a single run-on line, ending where the next `@user` prompt prefix was written.

## Concrete evidence

Example file (keep for reproduction):
`/Users/jefffarr/emacs-activities/tst-branch-2026-04-18/session/branches/main/session.org`

Top of file (lines 1-15):
```
:PROPERTIES:
:GPTEL_BOUNDS: ((tool (198 352 "toolu_...")) ... (response (580 867) (2068 3756)))
:END:
:PROPERTIES:

:END:
:PROPERTIES:

:END:
:PROPERTIES:
:GPTEL_PRESET: system-explorer

:END:

* Session
```

End of file (line 95), one line, before a bare `@user`:
```
:GPTEL_BOUNDS: ((tool ...)):GPTEL_BOUNDS: ((tool ...)):GPTEL_BOUNDS: ((tool ...))@user
```

The three trailing `GPTEL_BOUNDS` values have slightly different offsets from each other *and* from the top-drawer's value — state is diverging across writes.

## What's already known

- Session files are written only on explicit `C-x C-s`. No idle autosave is wired up. `jf/gptel-autosave-enabled` (defined in `config/gptel/sessions/constants.el:70`) is a vestigial variable — set to `t` in `jf/gptel--auto-init-session-buffer` (`config/gptel/sessions/commands.el:249`) and read only by the diagnostic function at `commands.el:415`. No `run-with-idle-timer`, no `gptel-post-response-functions` writer, no `after-change-functions` save trigger.
- The user's config already contains defensive scaffolding that strongly suggests this class of bug has been observed before:
  - `jf/gptel--check-duplicate-hooks` — `commands.el:32` — warns if `gptel--save-state` is registered more than once on `before-save-hook`.
  - `jf/gptel--ensure-mode-once` — `commands.el:53` — forcibly deduplicates `gptel--save-state` on `before-save-hook` after enabling `gptel-mode`.
  - `jf/gptel--clean-duplicate-local-vars` / `jf/gptel--batch-clean-sessions` — `commands.el:69`, `:96` — scan session files and strip duplicate `<!-- Local Variables: -->` blocks. **Markdown-only**; no org equivalent exists for duplicate `:PROPERTIES:` drawers.
- Session initialization runs via `find-file-hook` → `jf/gptel--auto-init-session-buffer` (`commands.el:133`) → `jf/gptel--ensure-mode-once` → `gptel-mode 1`.

## Suspected mechanism

Duplicate `gptel--save-state` hooks firing per save is the working hypothesis. Upstream gptel's org-mode save path at `runtime/straight/repos/gptel/gptel-org.el:650-669`:

```elisp
(defun gptel-org--save-state ()
  "Write the gptel state to the Org buffer as Org properties."
  (org-with-wide-buffer
   (goto-char (point-min))
   (when (org-at-heading-p)
     (org-open-line 1))
   (gptel-org-set-properties (point-min))
   (letrec ((write-bounds
             (lambda (attempts)
               (when-let* ((bounds (gptel--get-buffer-bounds))
                           (offset (caadar bounds))
                           (offset-marker (set-marker (make-marker) offset)))
                 (org-entry-put (point-min) "GPTEL_BOUNDS"
                                (prin1-to-string (gptel--get-buffer-bounds)))
                 (when (and (not (= (marker-position offset-marker) offset))
                            (> attempts 0))
                   (funcall write-bounds (1- attempts)))))))
     (funcall write-bounds 6))))
```

This function is **not idempotent across repeated calls within one save**:
- The `(when (org-at-heading-p) (org-open-line 1))` branch mutates the buffer on the first call (buffer starts with `* Session`) but not subsequent calls. Each extra call shifts structural assumptions without reaching the same target drawer.
- `(org-entry-put (point-min) ...)` is supposed to update the existing file-level drawer. But when `point-min` is ambiguous to org (inside a drawer rather than before one), it can create a new drawer instead of updating. This matches the empty-drawer stacking observed.
- The `write-bounds` retry loop runs up to 6 times. If the buffer is in an unexpected state (mid-narrowing, tracking-marker displacement from streaming, etc.), the retries can write `GPTEL_BOUNDS` at positions that are not inside a drawer at all, producing the run-on `:GPTEL_BOUNDS:` pile-up at `point-max`.

The multiple *empty* drawers (lines 4-9 of the example) are the strongest tell: only repeated calls that *find no existing drawer at their target point* would create stacked empty ones.

## Investigation plan

1. **Instrument duplicate-hook detection at init.** Call `jf/gptel--check-duplicate-hooks` (or equivalent) at the tail of `jf/gptel--auto-init-session-buffer` and at the tail of each save, and log the count via `jf/gptel--log`. If `gptel--save-state` consistently registers more than once, root cause is confirmed. Candidate locations:
   - `commands.el:248-262` (end of auto-init body)
   - Add an `after-save-hook` in `jf/gptel--ensure-mode-once` that reports count
2. **Confirm write count per save.** Advise `gptel--save-state` (upstream, temporarily) with a `:before` counter incrementing a buffer-local variable per save cycle. Clear on `before-save-hook` entry. If count > 1 per save, duplicates confirmed.
3. **Reproduce deterministically.** Likely repro: open a fresh session file, send a prompt, save. If the duplicate drawers appear after one save, the problem is not conversation-specific — it's init-time hook registration. If they accumulate across saves only, it's a per-save hook multiplication.
4. **Check if `find-file-hook` or `gptel-mode-hook` runs multiple times** in practice. Candidates for re-entry:
   - `activities-integration.el:197` — `find-file-noselect` + buffer rename may re-trigger find-file-hook in edge cases.
   - Any explicit `M-x gptel-mode` toggle by the user after auto-init.
   - `jf/gptel--ensure-mode-once` uses `(unless gptel-mode (gptel-mode 1))` which should prevent re-enable, but if `gptel-mode` is toggled off then on, hooks re-register.

## Fix directions (increasing depth)

- **A. Symptom mitigation.** Write an org equivalent of `jf/gptel--clean-duplicate-local-vars` that (1) collapses stacked top-level `:PROPERTIES:` drawers into one (preserving the most recent values per key), and (2) strips `:GPTEL_BOUNDS:` lines that are outside a drawer. Run on `find-file-hook` before `jf/gptel--auto-init-session-buffer`. Prevents existing corrupt files from degrading further. Does not address root cause.
- **B. Harden `jf/gptel--ensure-mode-once`.** Add an `after-save-hook` check that re-runs hook deduplication after each save, catching cases where `gptel-mode` was toggled during the session. Cheap belt-and-suspenders.
- **C. Root cause.** If instrumentation confirms duplicate hook registrations from a specific code path in the session init flow (most likely candidate: `activities-integration.el`'s buffer-opening sequence at `:197`), fix that path to not re-enter `gptel-mode 1`. Consider replacing `find-file-noselect` with `find-file-literally` or narrowing the `find-file-hook` predicate.
- **D. Upstream.** Consider reporting `gptel-org--save-state` non-idempotence upstream — it's fragile against *any* duplicate invocation, not just ones from this config. A minimal patch would guard against stacking: detect the existing file-level drawer before `org-entry-put` and update in place.

## Pointers

- Upstream save path: `runtime/straight/repos/gptel/gptel.el:667` (`gptel--save-state`) → `gptel-org.el:650` (`gptel-org--save-state`).
- Upstream gptel-mode hook registration: `gptel.el:884` (`add-hook 'before-save-hook`).
- Local defensive code: `config/gptel/sessions/commands.el:32-94`.
- Auto-init entry: `config/gptel/sessions/commands.el:133-267`.
- Activities integration (suspect for re-entry): `config/gptel/sessions/activities-integration.el:187-213`.

## Out of scope

The separate issue of LLM-injected org headings inside responses breaking `gptel-org-branching-context` is being addressed independently (ryanobjc's `gptel-org-heading-adjust` approach). Not this bug.
