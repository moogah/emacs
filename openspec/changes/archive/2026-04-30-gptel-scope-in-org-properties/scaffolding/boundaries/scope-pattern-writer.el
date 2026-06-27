;;; scope-pattern-writer.el --- Boundary scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/boundary/scope-pattern-writer
;; generated-at: 2026-04-29T11:05:33Z
;; license: implementor-may-revise
;;
;; The single canonical drawer mutator. Three stages:
;;   1. operation-to-drawer-key collapse (delegates to vocabulary scaffold)
;;   2. dedup against existing values (deliberate short-circuit)
;;   3. org-entry-put / multi-value emit + save-buffer

(defun jf/gptel-scope--write-pattern-to-drawer/scaffold
    (buffer operation pattern)
  "Idempotent writer — append PATTERN to the drawer key matching OPERATION.

BUFFER is the live chat buffer.
OPERATION is a member of register/vocabulary/operation-to-drawer-key
  (e.g. :read, :write, :modify, :execute, :create, :append, etc.).
PATTERN is the glob pattern string to write.

Returns a callback plist:
  (:success t :patterns_added (PATTERN ...) :message \"...\")
  or
  (:success t :patterns_added nil)         ; dedup no-op
  or
  (:success nil :error \"...\" :reason \"...\")  ; on save failure

Stages:
  1. Collapse OPERATION → drawer-key string via
     `jf/gptel-scope--map-operation-to-drawer-key'.
  2. Read existing values for the key. If PATTERN is already present,
     return :success t :patterns_added nil and DO NOT mutate the buffer
     and DO NOT call save-buffer.
  3. First write to a key uses `org-entry-put' (bare form). Subsequent
     writes use the multi-value helper (+: form). Then call save-buffer.

Invariants: register/invariant/scope-add-pattern-idempotent and
register/invariant/scope-drawer-no-duplication."
  (ignore buffer operation pattern)
  (error
   "speculated; not implemented — implement in scope-expansion.org as part of implement-drawer-writer; remove the YAML branch in --write-pattern-to-scope at the same time"))

(defun jf/gptel-scope--drawer-already-contains-p/scaffold
    (buffer drawer-key pattern)
  "Stage 2 helper — return non-nil iff PATTERN is already in BUFFER's
DRAWER-KEY values. Path normalization (trailing slash, ~ expansion)
is the implementor's choice; document in ## Discoveries."
  (ignore buffer drawer-key pattern)
  (error
   "speculated; not implemented — see register/boundary/scope-pattern-writer stage 2"))

(provide 'scope-pattern-writer/scaffold)
;;; scope-pattern-writer.el ends here
