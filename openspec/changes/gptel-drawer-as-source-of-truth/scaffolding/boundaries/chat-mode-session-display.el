;;; chat-mode-session-display.el --- scaffolding -*- lexical-binding: t; -*-
;; scaffolding-of: register/boundary/chat-mode-session-display
;; generated-at: 2026-05-22T19:19:24Z
;; license: implementor-may-revise

;;; Commentary:
;;
;; Failing buttercup contract-test scaffold for register/boundary/
;; chat-mode-session-display.  Speculated by cycle-7 forward-mode
;; against design.md §Addendum (Finding A / Decision A, Finding C /
;; Decision C).
;;
;; The boundary under test is the seam where `gptel-chat-mode' (a
;; `define-derived-mode' built on `org-mode') overrides org-mode's
;; generic presentation for session.org buffers.  Two override
;; contracts:
;;
;;   Override A (Finding A) — property-drawer *values* render as plain
;;   `org-property-value' data and never inherit org `/emphasis/'.  A
;;   buffer-local font-lock keyword with the OVERRIDE flag re-stamps
;;   drawer value spans.  Counter-assertion: chat-turn prose
;;   `/emphasis/' is unaffected.
;;
;;   Override C (Finding C) — the file-level config `:PROPERTIES:'
;;   drawer is folded on mode activation.  Counter-assertion: a
;;   drawerless scratch buffer is a no-op.
;;
;; This scaffold MUST fail loudly until an Implementor satisfies it.
;; Each `it' body raises an error per the `error-call' failing-stub
;; style.  The Implementor's tasks (fix-scope-drawer-value-emphasis,
;; fold-config-drawer-on-open) replace these stubs with real
;; assertions exercising `gptel-chat-mode' in config/gptel/chat/mode.org.
;;
;; This is speculation, not authority.  Make the scaffolded test pass,
;; or revise the scaffold and explain in `## Discoveries', or escalate.

;;; Code:

(require 'buttercup)

(describe "register/boundary/chat-mode-session-display"

  (describe "override A — drawer value emphasis suppression"

    (it "renders a trailing-slash drawer value (e.g. :GPTEL_SCOPE_READ: /Users/jeff/emacs/) with face org-property-value, not org italic emphasis"
      (error "speculated; not implemented — see register/boundary/chat-mode-session-display"))

    (it "re-stamps drawer value spans via a buffer-local font-lock keyword carrying the OVERRIDE flag"
      (error "speculated; not implemented — see register/boundary/chat-mode-session-display"))

    (it "leaves chat-turn prose /emphasis/ intact — the override is scoped to property-drawer value spans only"
      (error "speculated; not implemented — see register/boundary/chat-mode-session-display")))

  (describe "override C — config drawer folded on open"

    ;; Load the chat-mode module under test.  This scaffold lives at
    ;; openspec/changes/<change>/scaffolding/boundaries/; the source
    ;; tree's config/gptel/chat/ holds `mode.el'.
    (require 'org)
    (let* ((scaffold-dir (file-name-directory (or load-file-name buffer-file-name)))
           (repo-root (expand-file-name "../../../../../" scaffold-dir))
           (chat-dir (expand-file-name "config/gptel/chat/" repo-root)))
      (add-to-list 'load-path chat-dir))
    (require 'gptel-chat-mode)

    (it "folds the file-level config :PROPERTIES: drawer on gptel-chat-mode activation"
      (with-temp-buffer
        (insert ":PROPERTIES:\n:GPTEL_PRESET: default\n"
                ":GPTEL_SCOPE_READ: /Users/jeff/emacs/\n:END:\n"
                "\n* System Prompt\nbody\n\n* Chat\n"
                "#+begin_user\nhi\n#+end_user\n")
        (gptel-chat-mode)
        (goto-char (point-min))
        (forward-line 1)              ; into the drawer body
        (expect (org-fold-folded-p (point) 'drawer) :to-be-truthy)))

    (it "is a no-op for a drawerless scratch buffer (gptel-chat-new buffer with no :PROPERTIES: drawer) and raises no error"
      (expect
       (with-temp-buffer
         (insert "#+begin_user\n\n#+end_user\n")
         (gptel-chat-mode)            ; must not raise
         (goto-char (point-min))
         (invisible-p (point)))
       :not :to-be-truthy))))

;;; chat-mode-session-display.el ends here
