;;; scope-profile-applicator.el --- Boundary scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/boundary/scope-profile-applicator
;; generated-at: 2026-04-29T11:05:33Z
;; license: implementor-may-revise
;;
;; Mode 2a (string mode): render-drawer-text returns a drawer-text-block
;;   string for prepending to a freshly created session.org.
;; Mode 2b (buffer mode): apply-to-drawer mutates an open buffer's drawer
;;   via org-entry-put / multi-value helpers.
;; create-for-session is the single dispatcher.

(defun jf/gptel-scope-profile--render-drawer-text/scaffold
    (preset-name parent-session-id scope-plist)
  "Mode 2a — render the drawer block as a string.

PRESET-NAME (symbol or string) becomes :GPTEL_PRESET:.
PARENT-SESSION-ID (string or nil) becomes :GPTEL_PARENT_SESSION_ID:
when non-nil. SCOPE-PLIST has the canonical shape register/shape/
scope-config-plist (i.e. (:paths (...) :cloud (...))). Empty path lists
are omitted; the default cloud auth (\"warn\") with no providers may be
emitted or omitted at the implementor's choice (document the choice in
## Discoveries).

Returns a string of register/shape/drawer-text-block."
  (ignore preset-name parent-session-id scope-plist)
  (error
   "speculated; not implemented — implement in scope-profiles.org as part of implement-profile-drawer-applicator"))

(defun jf/gptel-scope-profile--apply-to-drawer/scaffold
    (buffer preset-name parent-session-id scope-plist)
  "Mode 2b — apply to BUFFER's drawer via org-entry-put.

Same parameters as `jf/gptel-scope-profile--render-drawer-text'. Side-
effect only: writes :GPTEL_PRESET:, :GPTEL_PARENT_SESSION_ID: (when
non-nil), and each :GPTEL_SCOPE_* key into BUFFER's :PROPERTIES: drawer
at point-min. Existing non-scope drawer keys are preserved.

Idempotent: applying twice with the same SCOPE-PLIST leaves the buffer
unchanged after the first application."
  (ignore buffer preset-name parent-session-id scope-plist)
  (error
   "speculated; not implemented — implement in scope-profiles.org as part of implement-profile-drawer-applicator"))

(defun jf/gptel-scope-profile--create-for-session/scaffold
    (preset-name target-dir &optional project-root worktree-paths)
  "Top-level dispatcher — resolves PRESET-NAME, expands variables, applies.

When called with a TARGET-DIR before any buffer exists, returns the
register/shape/drawer-text-block string for embedding in initial
session.org content (Mode 2a).

When called against an already-open chat buffer at TARGET-DIR (rare;
agents and branched sessions only), mutates the buffer in place via
Mode 2b.

Returns either:
- a register/shape/drawer-text-block string (Mode 2a), OR
- nil after a successful mutation (Mode 2b)."
  (ignore preset-name target-dir project-root worktree-paths)
  (error
   "speculated; not implemented — keep the existing public signature; only the implementation changes"))

(provide 'scope-profile-applicator/scaffold)
;;; scope-profile-applicator.el ends here
