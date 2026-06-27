;;; scope-config-loader.el --- Boundary scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/boundary/scope-config-loader
;; generated-at: 2026-04-29T11:05:33Z
;; license: implementor-may-revise
;;
;; Two-stage resolver: stage 1 buffer-first read, stage 2 file-fallback.
;; The shells below are the canonical signatures the Implementor must
;; satisfy. Bodies are TODO; the dispatcher (load-config) is the only
;; legal entry point for callers.

(defun jf/gptel-scope--load-from-buffer/scaffold (buffer)
  "Stage 1 — read scope plist from BUFFER's file-level :PROPERTIES: drawer.

Returns a plist of register/shape/scope-config-plist (i.e. exactly two
top-level keys, :paths and :cloud) or nil when the drawer carries no
:GPTEL_SCOPE_* keys.

Implementation must:
- Use `org-entry-get-multivalued-property' for list keys.
- Use `org-entry-get' for scalar keys.
- Default missing list keys to nil, missing :auth-detection to \"warn\".
- Never include :security in the returned plist."
  (ignore buffer)
  (error
   "speculated; not implemented — implement in scope-validation.org (see implement-drawer-reader task) and remove this scaffold call"))

(defun jf/gptel-scope--load-from-file/scaffold (path)
  "Stage 2 — read scope plist from PATH (a session.org file) headlessly.

Same return contract as `jf/gptel-scope--load-from-buffer'. Opens the
file via `with-temp-buffer' + `insert-file-contents', enables org-mode,
runs the same drawer reader, discards the temp buffer."
  (ignore path)
  (error
   "speculated; not implemented — implement in scope-validation.org as part of implement-drawer-reader"))

(defun jf/gptel-scope--load-config/scaffold ()
  "Top-level loader — buffer-first; falls back to file when no buffer is open.

Resolves the session's chat buffer via the registry's :buffer field for
`jf/gptel--branch-dir'. If a buffer is found, dispatch to
`jf/gptel-scope--load-from-buffer'. Otherwise, build the file path as
`(expand-file-name \"session.org\" jf/gptel--branch-dir)' and dispatch
to `jf/gptel-scope--load-from-file'.

Returns nil when neither stage yields a plist with at least one
:GPTEL_SCOPE_* key (the dispatcher denies with :error \"no_scope_config\")."
  (error
   "speculated; not implemented — implement in scope-validation.org and replace the (scope-yaml-load-schema scope-file) call site"))

;; Cross-stage round-trip assertion (used by tests):
(defun jf/gptel-scope--load-config-round-trip-p/scaffold (drawer-text)
  "Speculated: writing DRAWER-TEXT to a temp file produces the same plist
as inserting DRAWER-TEXT into a temp chat buffer. Implementor turns this
into a real test in load-from-buffer-spec or load-from-file-spec."
  (ignore drawer-text)
  (error "speculated; not implemented — see register/boundary/scope-config-loader cross_stage_invariants"))

(provide 'scope-config-loader/scaffold)
;;; scope-config-loader.el ends here
