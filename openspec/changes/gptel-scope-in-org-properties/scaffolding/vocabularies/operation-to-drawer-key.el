;;; operation-to-drawer-key.el --- Vocabulary scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/vocabulary/operation-to-drawer-key
;; generated-at: 2026-04-29T11:05:33Z
;; license: implementor-may-revise
;;
;; Granular validator-pipeline / bash-parser :operation values collapse
;; to one of five drawer keys. Each member of the input vocabulary is
;; an explicit `error' arm so an unmapped op fails loudly at runtime.

(defun jf/gptel-scope--map-operation-to-drawer-key/scaffold (operation)
  "Speculated mapping from a granular OPERATION keyword to a drawer key string.

The set of accepted OPERATION values is the closed set declared in
register/vocabulary/operation-to-drawer-key. Any OPERATION outside that
set must signal `error' rather than silently falling through to a
default — see the bash-parser :read-metadata-into-:paths.write incident."
  (pcase operation
    ;; Read-like granular operations → GPTEL_SCOPE_READ
    (:read
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_READ\""))
    (:read-directory
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_READ\""))
    (:read-metadata
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_READ\""))
    (:match-pattern
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_READ\""))

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
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_WRITE\""))

    ;; One-to-one mappings
    (:modify
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_MODIFY\""))
    (:execute
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_EXECUTE\""))

    ;; Safe fallback: nil → READ (most-restrictive collapse)
    ('nil
     (error "speculated; not implemented — should return \"GPTEL_SCOPE_READ\" (safest default)"))

    (_
     (error "Unmapped operation: %S — add to register/vocabulary/operation-to-drawer-key in the same change"
            operation))))

(provide 'operation-to-drawer-key/scaffold)
;;; operation-to-drawer-key.el ends here
