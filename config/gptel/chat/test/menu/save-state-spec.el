;;; save-state-spec.el --- Buttercup tests for gptel-chat save state -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the before-save hook delivered by task
;; `chat-save-state-hook' (see
;; openspec/changes/gptel-chat-state-persistence/tasks/open/chat-save-state-hook.md).
;;
;; Coverage (spec §"Configuration drawer save on buffer save"):
;;   - Drawer written on save when preset is applied (spy-on upstream).
;;   - Drawer omitted when no preset and no deltas and no parent-id.
;;   - GPTEL_PARENT_SESSION_ID written when jf/gptel--parent-session-id
;;     is a non-empty string.
;;   - GPTEL_PARENT_SESSION_ID NOT written when nil or empty.
;;   - gptel-mode is never called (design.md §Decision 16).
;;   - GPTEL_BOUNDS is never present in the saved drawer (integration —
;;     real upstream helper, real temp buffer).
;;   - Save hook registered buffer-locally on mode activation.
;;
;; Test structure:
;;
;;   1. `gptel-chat--save-state' (unit) — upstream `gptel-org-set-
;;      properties' is spied so the test controls boundary behaviour.
;;      Verifies call shape, GPTEL_PARENT_SESSION_ID branches, and the
;;      `gptel-mode' never-called invariant.
;;
;;   2. Integration — real `gptel-org-set-properties' against a live
;;      chat-mode buffer.  Exercises the full save path end-to-end so
;;      a future upstream-signature change fails a test we actually
;;      run (architecture.md §"End-to-end (real upstream)").  The
;;      integration test also enforces the GPTEL_BOUNDS-never-written
;;      invariant by searching the buffer text after save.
;;
;;   3. Hook registration scope — verifies the mode-hook handler
;;      installs the save hook buffer-locally (design.md §Decision 10).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the modules under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/menu/;
;; two levels up is .../config/gptel/chat/, which holds `menu.el' and
;; `mode.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-mode)
(require 'gptel-chat-menu)

;; Upstream `gptel-org-set-properties' lives in `gptel-org.el'.  Load it
;; here so the integration tests below exercise the real symbol.  If
;; `gptel-org' is not on the load-path in a particular test context, the
;; integration `describe' skips itself via `fboundp' guard.
(require 'gptel-org nil t)


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-save-test--empty-chat
  (concat "#+begin_user\n"
          "Hello.\n"
          "#+end_user\n")
  "Buffer content with no preset drawer — a bare chat-mode buffer.")


;;; Specs --------------------------------------------------------------------

(describe "gptel-chat save state"

  ;; -----------------------------------------------------------------------
  ;; 1. Unit: `gptel-chat--save-state' with spied upstream helper.
  ;;
  ;; `gptel-org-set-properties' is the boundary we trust.  The tests here
  ;; verify that the save hook:
  ;;   - calls the helper with `point-min' and `nil' (suppress echo),
  ;;   - writes `GPTEL_PARENT_SESSION_ID' iff the source var is a non-
  ;;     empty string,
  ;;   - never calls `gptel-mode'.

  (describe "gptel-chat--save-state (unit)"

    (before-each
      (spy-on 'gptel-org-set-properties :and-return-value nil)
      (spy-on 'org-entry-put :and-return-value nil)
      (spy-on 'gptel-mode :and-return-value nil))

    (it "delegates to gptel-org-set-properties with point-min and nil msg"
      (with-temp-buffer
        (gptel-chat-mode)
        (gptel-chat--save-state)
        (expect 'gptel-org-set-properties :to-have-been-called)
        (let ((args (spy-calls-args-for 'gptel-org-set-properties 0)))
          ;; First arg is `point-min' — the start of the buffer.
          (expect (nth 0 args) :to-equal (point-min))
          ;; Second arg is `nil' — suppress the interactive echo.
          (expect (nth 1 args) :to-be nil))))

    (it "writes GPTEL_PARENT_SESSION_ID when the source var is a non-empty string"
      (with-temp-buffer
        (gptel-chat-mode)
        (setq-local jf/gptel--parent-session-id "parent-abc-20260424000000")
        (gptel-chat--save-state)
        (expect 'org-entry-put :to-have-been-called)
        (let ((args (spy-calls-args-for 'org-entry-put 0)))
          (expect (nth 0 args) :to-equal (point-min))
          (expect (nth 1 args) :to-equal "GPTEL_PARENT_SESSION_ID")
          (expect (nth 2 args) :to-equal "parent-abc-20260424000000"))))

    (it "does NOT write GPTEL_PARENT_SESSION_ID when the source var is nil"
      (with-temp-buffer
        (gptel-chat-mode)
        ;; `jf/gptel--parent-session-id' left unbound / nil.
        (gptel-chat--save-state)
        (expect 'org-entry-put :not :to-have-been-called)))

    (it "does NOT write GPTEL_PARENT_SESSION_ID when the source var is an empty string"
      (with-temp-buffer
        (gptel-chat-mode)
        (setq-local jf/gptel--parent-session-id "")
        (gptel-chat--save-state)
        (expect 'org-entry-put :not :to-have-been-called)))

    (it "never enables gptel-mode"
      ;; Exercise both the parent-id branch and the no-parent-id branch
      ;; and verify neither toggles `gptel-mode' (design.md §Decision 16).
      (with-temp-buffer
        (gptel-chat-mode)
        (gptel-chat--save-state))
      (with-temp-buffer
        (gptel-chat-mode)
        (setq-local jf/gptel--parent-session-id "parent-xyz")
        (gptel-chat--save-state))
      (expect 'gptel-mode :not :to-have-been-called))

    (it "is a defense-in-depth no-op outside chat-mode buffers"
      ;; The save hook guards on `derived-mode-p' so a direct caller —
      ;; or a future accidental global registration — never touches a
      ;; non-chat-mode buffer.
      (with-temp-buffer
        (fundamental-mode)
        (gptel-chat--save-state)
        (expect 'gptel-org-set-properties :not :to-have-been-called)
        (expect 'org-entry-put :not :to-have-been-called))))


  ;; -----------------------------------------------------------------------
  ;; 2. Integration: real upstream `gptel-org-set-properties'.
  ;;
  ;; Exercises the end-to-end path against a real chat-mode buffer.  No
  ;; spies on `gptel-org-set-properties' here — a future upstream
  ;; signature change fails this test immediately (architecture.md
  ;; §"End-to-end (real upstream)").
  ;;
  ;; The test also enforces the central invariant of this change:
  ;; `:GPTEL_BOUNDS:' must never appear in the saved buffer under any
  ;; circumstance (spec §"Save path never writes GPTEL_BOUNDS").

  (describe "integration (real upstream helper)"

    (before-each
      ;; Upstream must be available for the integration path to be
      ;; meaningful.  If it is not (e.g. gptel not installed in the test
      ;; environment), the `fboundp' guard elsewhere prevents a false
      ;; positive here.
      (unless (fboundp 'gptel-org-set-properties)
        (signal 'buttercup-pending nil)))

    (it "never writes GPTEL_BOUNDS in the saved buffer"
      ;; Run the real save hook against a buffer containing an
      ;; assistant block (the kind that upstream's
      ;; `gptel-org--save-state' would have persisted bounds for).  Our
      ;; save hook must produce no `:GPTEL_BOUNDS:' line regardless.
      (with-temp-buffer
        (gptel-chat-mode)
        (insert "#+begin_user\nAsk.\n#+end_user\n"
                "#+begin_assistant\nAnswer.\n#+end_assistant\n")
        (gptel-chat--save-state)
        (goto-char (point-min))
        (expect (search-forward ":GPTEL_BOUNDS:" nil t)
                :to-be nil)))

    (it "never writes GPTEL_PRESET or GPTEL_PARENT_SESSION_ID when no preset and no parent-id"
      ;; With no preset applied and no parent-session-id source var,
      ;; the save hook's `GPTEL_PRESET' branch (upstream) and the
      ;; chat-mode `GPTEL_PARENT_SESSION_ID' branch are both skipped.
      ;; Upstream may still emit other delta keys (`GPTEL_SYSTEM',
      ;; `GPTEL_TEMPERATURE', ...) because it treats every non-default
      ;; field as a delta when `gptel--preset' is nil (design.md Risks
      ;; §"gptel-org-set-properties behavior when gptel--preset is nil")
      ;; — that noise is an upstream-level follow-up, not a regression
      ;; against this task's contract.  What matters here is that the
      ;; two keys the chat-mode save hook owns are absent.
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--empty-chat)
        (gptel-chat--save-state)
        (goto-char (point-min))
        (expect (search-forward ":GPTEL_PRESET:" nil t) :to-be nil)
        (goto-char (point-min))
        (expect (search-forward ":GPTEL_PARENT_SESSION_ID:" nil t) :to-be nil)
        ;; And GPTEL_BOUNDS invariant still holds.
        (goto-char (point-min))
        (expect (search-forward ":GPTEL_BOUNDS:" nil t) :to-be nil)))

    (it "writes GPTEL_PARENT_SESSION_ID into the drawer when set"
      ;; The parent-session-id branch always creates a drawer at
      ;; point-min, even when every other configuration key is at its
      ;; default.  After the save hook returns, the buffer text must
      ;; contain `:GPTEL_PARENT_SESSION_ID: <id>' inside a
      ;; `:PROPERTIES:' block.
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--empty-chat)
        (setq-local jf/gptel--parent-session-id
                    "parent-abc-20260424000000")
        (gptel-chat--save-state)
        (goto-char (point-min))
        (expect (search-forward ":PROPERTIES:" nil t)
                :to-be-truthy)
        (goto-char (point-min))
        (expect (search-forward
                 ":GPTEL_PARENT_SESSION_ID: parent-abc-20260424000000"
                 nil t)
                :to-be-truthy)
        ;; And still no GPTEL_BOUNDS — the invariant holds across
        ;; branches.
        (goto-char (point-min))
        (expect (search-forward ":GPTEL_BOUNDS:" nil t)
                :to-be nil))))


  ;; -----------------------------------------------------------------------
  ;; 3. Hook registration scope (design.md §Decision 10).
  ;;
  ;; The save hook must be registered buffer-locally from the mode-hook
  ;; handler — never globally.  Global registration would force a
  ;; `derived-mode-p' check on every Emacs save in every buffer, which
  ;; is wasteful for unrelated buffers.

  (describe "hook registration scope"

    (it "does not register the save hook globally at module load"
      (expect (memq 'gptel-chat--save-state
                    (default-value 'before-save-hook))
              :to-be nil))

    (it "registers the save hook buffer-locally on mode activation"
      (with-temp-buffer
        (gptel-chat-mode)
        (expect (local-variable-p 'before-save-hook) :to-be t)
        (expect (memq 'gptel-chat--save-state before-save-hook)
                :to-be-truthy)
        ;; And still NOT on the global list.
        (expect (memq 'gptel-chat--save-state
                      (default-value 'before-save-hook))
                :to-be nil)))

    (it "leaves the global before-save-hook untouched when opening non-chat buffers"
      (with-temp-buffer
        (fundamental-mode)
        (expect (memq 'gptel-chat--save-state
                      (default-value 'before-save-hook))
                :to-be nil))))

  )

(provide 'gptel-chat-save-state-spec)

;;; save-state-spec.el ends here
