;;; system-prompt-heading-authoritative.test.el --- scaffolding -*- lexical-binding: t; -*-
;; scaffolding-of: register/invariant/system-prompt-heading-authoritative
;; generated-at: 2026-05-22T19:19:24Z
;; license: implementor-may-revise

;;; Commentary:
;;
;; Failing buttercup scaffold for register/invariant/system-prompt-
;; heading-authoritative.  Speculated by cycle-7 forward-mode against
;; design.md §Addendum (Finding B, Decision B).
;;
;; The invariant under test:
;;   - The `* System Prompt' heading body is the authoritative source
;;     of the buffer-local `gptel--system-message'.
;;   - Restore precedence, highest first:
;;       1. `* System Prompt' heading body, when non-blank
;;       2. legacy :GPTEL_SYSTEM: drawer entry, when present
;;       3. preset :system
;;   - A blank heading body falls through to the next source (an empty
;;     heading never silently wipes the prompt).
;;   - On before-save-hook the buffer-local `gptel--system-message' is
;;     serialised back into the heading body.
;;   - create -> restore -> save -> re-restore is idempotent.
;;
;; This scaffold MUST fail loudly until an Implementor satisfies it.
;; Each `it' body raises an error per the `error-call' failing-stub
;; style.  The Implementor's task (make-system-prompt-heading-
;; authoritative) replaces these stubs with real assertions exercising
;; the restore path (config/gptel/chat/mode.org) and the before-save
;; serialisation (config/gptel/chat/menu.org).
;;
;; This is speculation, not authority.  Make the scaffolded test pass,
;; or revise the scaffold and explain in `## Discoveries', or escalate.

;;; Code:

(require 'buttercup)

(describe "register/invariant/system-prompt-heading-authoritative"

  (describe "restore precedence"

    (it "uses the `* System Prompt' heading body when it is non-blank"
      (error "speculated; not implemented — see register/invariant/system-prompt-heading-authoritative"))

    (it "prefers the heading body over a legacy :GPTEL_SYSTEM: drawer entry"
      (error "speculated; not implemented — see register/invariant/system-prompt-heading-authoritative"))

    (it "prefers a legacy :GPTEL_SYSTEM: drawer entry over the preset :system when the heading body is absent"
      (error "speculated; not implemented — see register/invariant/system-prompt-heading-authoritative"))

    (it "falls back to the preset :system when no heading body and no drawer entry exist"
      (error "speculated; not implemented — see register/invariant/system-prompt-heading-authoritative")))

  (describe "blank-body fallback"

    (it "treats a whitespace-only heading body as not authored and falls through to the preset"
      (error "speculated; not implemented — see register/invariant/system-prompt-heading-authoritative"))

    (it "never silently wipes the prompt when the heading body is empty"
      (error "speculated; not implemented — see register/invariant/system-prompt-heading-authoritative")))

  (describe "before-save serialisation"

    (it "writes the current buffer-local `gptel--system-message' back into the `* System Prompt' heading body on before-save-hook"
      (error "speculated; not implemented — see register/invariant/system-prompt-heading-authoritative"))

    (it "never emits a :GPTEL_SYSTEM: drawer line (composes with drawer-system-key-write-exclusion)"
      (error "speculated; not implemented — see register/invariant/system-prompt-heading-authoritative")))

  (describe "round-trip stability"

    (it "is idempotent across create -> restore -> save -> re-restore (a no-change save produces no heading-body diff)"
      (error "speculated; not implemented — see register/invariant/system-prompt-heading-authoritative"))))

;;; system-prompt-heading-authoritative.test.el ends here
