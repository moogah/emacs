;;; drawer-fold-spec.el --- Buttercup tests for config-drawer folding -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat-mode's config-drawer fold-on-open
;; behaviour (design.md §Addendum, Finding C / Decision C; register
;; entry `register/boundary/chat-mode-session-display', override C).
;;
;; Coverage:
;;   - The file-level config `:PROPERTIES:' drawer (the point-min
;;     drawer of a persisted `session.org') is folded on
;;     `gptel-chat-mode' activation.
;;   - Chat-turn content under `* Chat' stays visible — the fold is
;;     scoped to drawers, not the whole buffer.
;;   - A drawerless buffer (a `gptel-chat-new' scratch buffer with no
;;     `:PROPERTIES:' drawer) is a no-op: activation raises no error.
;;
;; Tests drive `gptel-chat-mode' directly and inspect the org-fold
;; state of the buffer, so the mode-activation path is exercised
;; end-to-end with no mocking.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'org)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/display/;
;; two levels up is .../config/gptel/chat/, which holds `mode.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-mode)


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-drawer-fold-test--session
  (concat ":PROPERTIES:\n"
          ":GPTEL_PRESET: default\n"
          ":GPTEL_MODEL: gpt-4o\n"
          ":GPTEL_SCOPE_READ: /Users/jeff/emacs/\n"
          ":END:\n"
          "* System Prompt\n"
          ":PROPERTIES:\n"
          ":VISIBILITY: folded\n"
          ":END:\n"
          "You are a helpful assistant.\n"
          "\n"
          "* Chat\n"
          "#+begin_user\n"
          "Hello there.\n"
          "#+end_user\n")
  "Persisted-session content: the canonical post-cycle-7 session.org layout —
file-level config drawer at point-min, then `* System Prompt' with its own
`:PROPERTIES:/:VISIBILITY: folded/:END:' drawer, then `* Chat' with a turn
block (mirrors register/shape/session-document-layout).")

(defconst gptel-chat-drawer-fold-test--drawerless
  (concat "#+begin_user\n"
          "\n"
          "#+end_user\n")
  "Scratch-buffer content with no `:PROPERTIES:' drawer (a `gptel-chat-new' buffer).")

(defun gptel-chat-drawer-fold-test--goto-line (n)
  "Move point to the start of line N (1-indexed) in the current buffer."
  (goto-char (point-min))
  (forward-line (1- n)))


;;; Specs --------------------------------------------------------------------

(describe "gptel-chat-mode config-drawer folding (Decision C)"

  (describe "override C — config drawer folded on open"

    (it "folds the file-level config :PROPERTIES: drawer on gptel-chat-mode activation"
      (with-temp-buffer
        (insert gptel-chat-drawer-fold-test--session)
        (gptel-chat-mode)
        ;; Line 2 is `:GPTEL_PRESET: default' — inside the drawer body.
        (gptel-chat-drawer-fold-test--goto-line 2)
        (expect (org-fold-folded-p (point) 'drawer) :to-be-truthy)
        (expect (invisible-p (point)) :to-be-truthy)))

    (it "leaves chat-turn content under * Chat visible — the fold is scoped to drawers"
      (with-temp-buffer
        (insert gptel-chat-drawer-fold-test--session)
        (gptel-chat-mode)
        (goto-char (point-min))
        (expect (search-forward "Hello there." nil t) :to-be-truthy)
        (expect (invisible-p (match-beginning 0)) :not :to-be-truthy)))

    (it "is a no-op for a drawerless scratch buffer (gptel-chat-new buffer with no :PROPERTIES: drawer) and raises no error"
      (expect
       (with-temp-buffer
         (insert gptel-chat-drawer-fold-test--drawerless)
         (gptel-chat-mode)
         ;; Content remains visible; activation completed without error.
         (goto-char (point-min))
         (search-forward "#+begin_user" nil t)
         (invisible-p (match-beginning 0)))
       :not :to-be-truthy))

    (it "folds the drawer when gptel-chat-mode is invoked directly on a buffer (M-x path)"
      (with-temp-buffer
        (insert gptel-chat-drawer-fold-test--session)
        (goto-char (point-min))
        ;; Direct mode invocation, distinct from the find-file/normal-mode
        ;; entry path — both reach the fold via the mode body.
        (gptel-chat-mode)
        (gptel-chat-drawer-fold-test--goto-line 3)
        (expect (org-fold-folded-p (point) 'drawer) :to-be-truthy)))

    (it "folds the drawer for a session file opened via find-file"
      (let ((file (make-temp-file "gptel-chat-drawer-fold" nil ".org")))
        (unwind-protect
            (progn
              (with-temp-file file
                (insert gptel-chat-drawer-fold-test--session))
              (with-current-buffer (find-file-noselect file)
                (unwind-protect
                    (progn
                      ;; `find-file-noselect' on a .org file activates
                      ;; org-mode; switch to chat-mode as `normal-mode'
                      ;; would for a session.org.
                      (gptel-chat-mode)
                      (gptel-chat-drawer-fold-test--goto-line 2)
                      (expect (org-fold-folded-p (point) 'drawer) :to-be-truthy))
                  (set-buffer-modified-p nil)
                  (kill-buffer))))
          (when (file-exists-p file)
            (delete-file file))))))

  (describe "override C extension — `:VISIBILITY: folded' on `* System Prompt' honored"

    (it "folds the `* System Prompt' heading body on gptel-chat-mode activation"
      ;; Regression for user-testing finding 2026-05-23: `:VISIBILITY: folded'
      ;; on the canonical layout's `* System Prompt' heading was not being
      ;; processed by `gptel-chat-mode' because org-mode only auto-processes
      ;; the property when `org-startup-folded' is non-default.
      ;; `gptel-chat--apply-startup-visibility' now calls
      ;; `org-cycle-set-visibility-according-to-property' explicitly.
      (let ((org-startup-folded 'showall))   ; force the failure mode
        (with-temp-buffer
          (insert gptel-chat-drawer-fold-test--session)
          (gptel-chat-mode)
          (goto-char (point-min))
          ;; The prompt body ("You are a helpful assistant.") sits inside
          ;; the `* System Prompt' subtree; with `:VISIBILITY: folded'
          ;; honored, that text should be invisible after activation.
          (expect (search-forward "You are a helpful assistant." nil t)
                  :to-be-truthy)
          (expect (invisible-p (match-beginning 0)) :to-be-truthy))))

    (it "leaves the `* Chat' subtree visible — :VISIBILITY: folded is heading-scoped"
      (let ((org-startup-folded 'showall))
        (with-temp-buffer
          (insert gptel-chat-drawer-fold-test--session)
          (gptel-chat-mode)
          (goto-char (point-min))
          ;; Turn-block content under `* Chat' has no `:VISIBILITY:'
          ;; property and must remain visible.
          (expect (search-forward "Hello there." nil t) :to-be-truthy)
          (expect (invisible-p (match-beginning 0)) :not :to-be-truthy))))

    (it "is a no-op for a scratch buffer with no `:VISIBILITY:' properties"
      (let ((org-startup-folded 'showall))
        (expect
         (with-temp-buffer
           (insert gptel-chat-drawer-fold-test--drawerless)
           (gptel-chat-mode)
           ;; No headings, no :VISIBILITY: — activation completes without
           ;; folding anything.
           (goto-char (point-min))
           (search-forward "#+begin_user" nil t)
           (invisible-p (match-beginning 0)))
         :not :to-be-truthy)))))

;;; drawer-fold-spec.el ends here
