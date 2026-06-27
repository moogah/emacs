;;; operation-to-drawer-key.el --- Vocabulary scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/vocabulary/operation-to-drawer-key
;; generated-at: 2026-04-29T11:05:33Z
;; revised-at: 2026-04-29T15:50:00Z (cycle-2 plan; dispositions 10A/B/C)
;; license: implementor-may-revise
;;
;; Granular validator-pipeline / bash-parser :operation values collapse
;; to one of FIVE drawer keys (READ / READ_METADATA / WRITE / MODIFY /
;; EXECUTE). Each member of the writer's input vocabulary is an explicit
;; `error' arm so an unmapped op fails loudly at runtime.
;;
;; Cycle-2 dispositions:
;;   ask 10A: :read-metadata → GPTEL_SCOPE_READ_METADATA (separate bucket)
;;   ask 10B: :match-pattern → not in writer domain (action handler redirects)
;;   ask 10C: :delete → GPTEL_SCOPE_WRITE (kept; tradeoff documented)

(defun jf/gptel-scope--map-operation-to-drawer-key/scaffold (operation)
  "Speculated mapping from a granular OPERATION keyword to a drawer key string.

The writer's input domain is ten :operation values (eleven minus
:match-pattern, which the action handler redirects upstream). Any
OPERATION outside that domain — including nil, :match-pattern, or a
typo — must signal `error' rather than silently falling through. See
the bash-parser :read-metadata-into-:paths.write incident for why."
  (pcase operation
    ;; Content-read granular operations → GPTEL_SCOPE_READ
    (:read
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_READ\""))
    (:read-directory
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_READ\""))

    ;; Metadata-read → its own bucket (cycle-2 ask 10A disposition)
    (:read-metadata
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_READ_METADATA\" (cycle-2 ask 10A)"))

    ;; Match-pattern is action-layer-only; reaching the writer is a defect
    (:match-pattern
     (error "speculated; not implemented — should signal: \"scope-expansion: :match-pattern reached the writer — action handler should have redirected to :read-directory\" (cycle-2 ask 10B)"))

    ;; Write-like granular operations → GPTEL_SCOPE_WRITE
    (:write
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_WRITE\""))
    (:create
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_WRITE\""))
    (:create-or-modify
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_WRITE\""))
    (:append
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_WRITE\""))
    (:delete
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_WRITE\" (cycle-2 ask 10C: kept; explicit deny is the escape hatch for delete-locked-out)"))

    ;; One-to-one mappings
    (:modify
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_MODIFY\""))
    (:execute
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_EXECUTE\""))

    ;; nil :operation: refuse loudly (action handler must guard upstream)
    ('nil
     (error "speculated; not implemented — should signal: \"scope-expansion: cannot map nil :operation — handle at the action layer\""))

    (_
     (error "Unmapped operation: %S — extend the mapping or use the deny action"
            operation))))

(provide 'operation-to-drawer-key/scaffold)
;;; operation-to-drawer-key.el ends here
