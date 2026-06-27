;;; operation-to-drawer-key-spec.el --- Closed-set vocabulary for the writer's operation→drawer-key collapse -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Vocabulary spec for `jf/gptel-scope--map-operation-to-drawer-key' —
;; the single closed-set mapping that collapses validator-pipeline /
;; bash-parser :operation keywords to :GPTEL_SCOPE_<KEY>: drawer keys.
;;
;; Cycle-2 disposition coverage:
;;
;;   - Ask 10A: :read-metadata gets its own GPTEL_SCOPE_READ_METADATA
;;     bucket (was lumped with READ).
;;   - Ask 10B: :match-pattern is NOT in the writer's domain; the action
;;     handler must redirect upstream. If it reaches the writer, the
;;     mapper errors loudly so the defect surfaces immediately.
;;   - Ask 10C: :delete is intentionally kept under WRITE rather than
;;     getting its own bucket; this test pins the disposition decision.
;;
;; Other cases:
;;   - nil :operation errors loudly (was permissive READ fallback in
;;     earlier drafts; cycle-1 ask-arch-cycle-1777460733-1).
;;   - Unmapped operations error loudly (no silent t-fallback).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

(describe "jf/gptel-scope--map-operation-to-drawer-key"

  (describe "read-like operations"

    (it "maps :read to GPTEL_SCOPE_READ"
      (expect (jf/gptel-scope--map-operation-to-drawer-key :read)
              :to-equal "GPTEL_SCOPE_READ"))

    (it "maps :read-directory to GPTEL_SCOPE_READ"
      (expect (jf/gptel-scope--map-operation-to-drawer-key :read-directory)
              :to-equal "GPTEL_SCOPE_READ"))

    (it "maps :read-metadata to GPTEL_SCOPE_READ_METADATA (cycle-2 ask 10A)"
      ;; Cycle-2 disposition: :read-metadata gets its own bucket so a
      ;; metadata stat (e.g. `[ -f file ]') does not silently grant
      ;; persistent content read access.
      (expect (jf/gptel-scope--map-operation-to-drawer-key :read-metadata)
              :to-equal "GPTEL_SCOPE_READ_METADATA")))

  (describe "write-like operations (cycle-2 ask 10C — :delete kept under WRITE)"

    (it "maps :write to GPTEL_SCOPE_WRITE"
      (expect (jf/gptel-scope--map-operation-to-drawer-key :write)
              :to-equal "GPTEL_SCOPE_WRITE"))

    (it "maps :create to GPTEL_SCOPE_WRITE"
      (expect (jf/gptel-scope--map-operation-to-drawer-key :create)
              :to-equal "GPTEL_SCOPE_WRITE"))

    (it "maps :create-or-modify to GPTEL_SCOPE_WRITE"
      (expect (jf/gptel-scope--map-operation-to-drawer-key :create-or-modify)
              :to-equal "GPTEL_SCOPE_WRITE"))

    (it "maps :append to GPTEL_SCOPE_WRITE"
      (expect (jf/gptel-scope--map-operation-to-drawer-key :append)
              :to-equal "GPTEL_SCOPE_WRITE"))

    (it "maps :delete to GPTEL_SCOPE_WRITE (cycle-2 ask 10C disposition)"
      ;; Cycle-2 disposition: :delete intentionally collapses to WRITE
      ;; rather than its own bucket.  Adding a delete-only bucket would
      ;; not improve safety since granting WRITE already covers it.
      (expect (jf/gptel-scope--map-operation-to-drawer-key :delete)
              :to-equal "GPTEL_SCOPE_WRITE")))

  (describe "modify and execute"

    (it "maps :modify to GPTEL_SCOPE_MODIFY"
      (expect (jf/gptel-scope--map-operation-to-drawer-key :modify)
              :to-equal "GPTEL_SCOPE_MODIFY"))

    (it "maps :execute to GPTEL_SCOPE_EXECUTE"
      (expect (jf/gptel-scope--map-operation-to-drawer-key :execute)
              :to-equal "GPTEL_SCOPE_EXECUTE")))

  (describe ":match-pattern is not in the writer's domain (cycle-2 ask 10B)"

    (it "errors with a defect-signal message when :match-pattern reaches the writer"
      ;; The action handler is responsible for redirecting :match-pattern
      ;; to the sibling :read-directory operation BEFORE invoking the
      ;; writer.  If :match-pattern lands here, that redirect is broken
      ;; and we fail loudly so the defect surfaces immediately rather
      ;; than silently routing a glob into GPTEL_SCOPE_READ (which would
      ;; grant reads on every filesystem match).
      (expect (jf/gptel-scope--map-operation-to-drawer-key :match-pattern)
              :to-throw))

    (it "error message names the writer and the redirect target"
      ;; The error message is the contract — it tells the future
      ;; debugger exactly which boundary failed and where to look.
      (let ((captured-message nil))
        (condition-case err
            (jf/gptel-scope--map-operation-to-drawer-key :match-pattern)
          (error (setq captured-message (error-message-string err))))
        (expect captured-message :to-match ":match-pattern reached the writer")
        (expect captured-message :to-match ":read-directory"))))

  (describe "strict-error fallback (cycle-1 ask-arch-cycle-1777460733-1)"

    (it "errors on nil :operation rather than silently picking READ"
      ;; Cloud-auth and parse-incomplete violations carry :operation nil.
      ;; The writer is strict; the upstream fix lives at the action
      ;; handler (harden-add-to-scope-action-handler, cycle-3).
      (expect (jf/gptel-scope--map-operation-to-drawer-key nil)
              :to-throw))

    (it "errors on unmapped :operation values rather than t-fallback"
      ;; Adding a new bash-parser semantic op without extending this
      ;; mapper is a vocabulary-mismatch finding; we want it to surface
      ;; loudly at the user-visible expansion UI the moment it happens.
      (expect (jf/gptel-scope--map-operation-to-drawer-key :totally-new-op)
              :to-throw))))

(provide 'operation-to-drawer-key-spec)
;;; operation-to-drawer-key-spec.el ends here
