;;; workspace-broken-disposition.el --- speculated vocabulary -*- lexical-binding: t; -*-

;; scaffolding-of: register/vocabulary/workspace-broken-disposition
;; generated-at: 2026-05-25T18:04:59Z
;; license: implementor-may-revise
;;
;; Canonical mapping function shell: workspace-command op symbol →
;; disposition keyword for broken-state workspaces. The closed value
;; set is asserted by the error arm on unhandled inputs — any new
;; workspace-command added by a future cycle MUST either land in the
;; pcase (and gain a corresponding inline guard in its function body)
;; or be rejected loudly by the catch-all error arm.
;;
;; The Implementor's cycle-1 task (add-home-slot-to-data-model)
;; introduces the runtime predicate `workspace--broken-p' that the
;; switch/restore guards consult. The dispatcher below is NOT called
;; at runtime in cycle 1 — runtime guards are inline at each command's
;; entry point per design D6. This scaffold exists as the authoritative
;; vocabulary so an end-of-cycle Architect audit can compare each
;; workspace-command's inline guard against this table; drift between
;; the table and the inline guards is a vocabulary-mismatch finding.

(defun vocabulary/workspace-broken-disposition (op)
  "Return the disposition for workspace-command OP on a broken workspace.

Returns one of :refused (signal user-error; no side effects) or
:allowed (operate normally, with command-specific handling of the
missing-:home condition).

Speculation per register entry:
  switch / restore         → :refused
  re-anchor / purge / delete → :allowed

Implementor: if a new workspace-command is introduced, add it here
AND ensure its function body honours the disposition. If the
disposition shape itself needs more states (e.g. :allowed-with-warning),
that is a register-divergence — surface in `## Discoveries'."
  (pcase op
    ('switch
     (error "TODO: vocabulary/workspace-broken-disposition 'switch → :refused"))
    ('restore
     (error "TODO: vocabulary/workspace-broken-disposition 'restore → :refused"))
    ('re-anchor
     (error "TODO: vocabulary/workspace-broken-disposition 're-anchor → :allowed"))
    ('purge
     (error "TODO: vocabulary/workspace-broken-disposition 'purge → :allowed"))
    ('delete
     (error "TODO: vocabulary/workspace-broken-disposition 'delete → :allowed"))
    (_
     (error "vocabulary/workspace-broken-disposition: unknown op %S \
\(closed vocabulary; add to register/vocabulary/workspace-broken-disposition \
if genuinely a new workspace command, not a typo)" op))))

;; Validator helper — confirms closed-set membership of OP.
(defun vocabulary/validate-workspace-broken-op (op)
  "Return non-nil if OP is a known workspace-command operating on a workspace by name."
  (memq op '(switch restore re-anchor purge delete)))

(provide 'workspace-broken-disposition)
;;; workspace-broken-disposition.el ends here
