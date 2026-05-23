;;; drawer-key-set.el --- Vocabulary scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/vocabulary/drawer-key-set
;; generated-at: 2026-04-29T11:05:33Z
;; revised-at: 2026-04-29T15:50:00Z (cycle-2 plan; ask 10A added READ_METADATA)
;; license: implementor-may-revise
;;
;; The eight :GPTEL_SCOPE_* drawer keys and their plist-path mapping.
;; The arms are unimplemented `error' calls so any consumer that
;; accidentally feeds a key not in the canonical set fails loudly at
;; runtime — not at audit, not silently.

(defun jf/gptel-scope--drawer-key-to-plist-path/scaffold (key)
  "Speculated mapping from drawer KEY string to (top-key sub-key) plist path.

KEY is the property keyword without the leading or trailing colons and
without the `+' multi-value suffix (e.g. \"GPTEL_SCOPE_READ\" not
\":GPTEL_SCOPE_READ:\" and not \":GPTEL_SCOPE_READ+:\")."
  (pcase key
    ("GPTEL_SCOPE_READ"
     (error "speculated; not implemented — should return '(:paths :read)"))
    ("GPTEL_SCOPE_READ_METADATA"
     (error "speculated; not implemented — should return '(:paths :read-metadata) ; cycle-2 ask 10A"))
    ("GPTEL_SCOPE_WRITE"
     (error "speculated; not implemented — should return '(:paths :write)"))
    ("GPTEL_SCOPE_MODIFY"
     (error "speculated; not implemented — should return '(:paths :modify)"))
    ("GPTEL_SCOPE_EXECUTE"
     (error "speculated; not implemented — should return '(:paths :execute)"))
    ("GPTEL_SCOPE_DENY"
     (error "speculated; not implemented — should return '(:paths :deny)"))
    ("GPTEL_SCOPE_CLOUD_AUTH"
     (error "speculated; not implemented — should return '(:cloud :auth-detection)"))
    ("GPTEL_SCOPE_CLOUD_PROVIDERS"
     (error "speculated; not implemented — should return '(:cloud :allowed-providers)"))
    (_
     (error "Unknown scope drawer key: %s" key))))

;; Vocabulary-completeness check: every member listed in the register
;; entry has a `pcase' arm above. If you add a new :GPTEL_SCOPE_* key,
;; add it here AND update the register entry in the same change.
(defun jf/gptel-scope--all-drawer-keys/scaffold ()
  "Return every drawer key the vocabulary entry declares.
Used by audit code to enumerate the closed set without parsing YAML."
  (error "speculated; not implemented — should return the eight keys above as strings (six list-shape + two cloud)"))

(provide 'drawer-key-set/scaffold)
;;; drawer-key-set.el ends here
