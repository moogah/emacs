;;; save-state-spec.el --- Buttercup tests for gptel-chat save state -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the chat-mode `before-save-hook' delivered
;; originally by `chat-save-state-hook' (cycle-1 of the gptel-chat-
;; state-persistence change) and rewritten by task `replace-chat-save-
;; with-full-snapshot-writer' of gptel-drawer-as-source-of-truth
;; (Decision 1).
;;
;; The save path now writes the *full snapshot* of current buffer-
;; local configuration on every save — no delta-from-preset deletion.
;; Coverage:
;;
;;   - `gptel-chat--write-config-drawer' writes each upstream-
;;     compatible drawer key from buffer-local state, and DELETES
;;     each key when the source variable is nil or wrong-typed.
;;   - `:GPTEL_SYSTEM:' is *never* written, even when
;;     `gptel--system-message' (or the preset's `:system') is set
;;     (write-side enforcement of register/invariant/drawer-system-
;;     key-write-exclusion; Decision 2).
;;   - `:GPTEL_BOUNDS:' is never written (chat-mode block format,
;;     spec §"Save path never writes GPTEL_BOUNDS").
;;   - `:GPTEL_PARENT_SESSION_ID:' is written iff the source var is
;;     a non-empty string.
;;   - `gptel-mode' is never enabled (design.md §Decision 16).
;;   - The save path no longer calls upstream
;;     `gptel-org-set-properties' (Decision 1: dedicated chat-mode
;;     writer, full snapshot, no delta semantics).
;;   - Cold-load: `gptel-org' is still required by the save hook so
;;     the read-side overlay (`gptel-org--entry-properties') is
;;     available (regression caught by `save-hook-require-gptel-org').
;;   - Save hook is registered buffer-locally on mode activation,
;;     never globally (design.md §Decision 10).

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

(require 'gptel)
(require 'gptel-chat-mode)
(require 'gptel-chat-menu)

;; The save hook requires `gptel-org' lazily for the read-side overlay.
;; Soft-require here so existing tests can pre-load it; the cold-load
;; regression below runs `unload-feature' before-each to exercise the
;; lazy-require path.
(require 'gptel-org nil t)


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-save-test--empty-chat
  (concat "#+begin_user\n"
          "Hello.\n"
          "#+end_user\n")
  "Buffer content with no preset drawer — a bare chat-mode buffer.")

(defun gptel-chat-save-test--has-line (key)
  "Return non-nil if KEY (a string like \":GPTEL_TOOLS:\") appears at point-min.
Searches from `point-min'; non-destructive on point."
  (save-excursion
    (goto-char (point-min))
    (search-forward key nil t)))


;;; Specs --------------------------------------------------------------------

(describe "gptel-chat save state"

  ;; -----------------------------------------------------------------------
  ;; 1. Unit: gptel-chat--write-config-drawer with simulated buffer state.
  ;;
  ;; The new writer reads buffer-local variables and writes the upstream-
  ;; compatible drawer keys via `org-entry-put' / `org-entry-put-multivalued-
  ;; property'.  No spies on upstream `gptel-org-set-properties' — the new
  ;; writer no longer calls it.

  (describe "gptel-chat--write-config-drawer (unit)"

    (it "writes GPTEL_PRESET when gptel--preset is a non-nil symbol"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:END:\n")
        (setq-local gptel--preset 'system-explorer)
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_PRESET")
                :to-equal "system-explorer")))

    (it "deletes GPTEL_PRESET when gptel--preset is nil"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:GPTEL_PRESET: stale\n:END:\n")
        ;; Local override defaults gptel--preset to nil.
        (setq-local gptel--preset nil)
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_PRESET")
                :to-be nil)))

    (it "writes GPTEL_MODEL when gptel-model is non-nil"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:END:\n")
        (setq-local gptel-model 'claude-sonnet-4-6)
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_MODEL")
                :to-equal "claude-sonnet-4-6")))

    (it "writes GPTEL_TEMPERATURE / GPTEL_MAX_TOKENS / GPTEL_NUM_MESSAGES_TO_SEND as scalars"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:END:\n")
        (setq-local gptel-temperature 0.7)
        (setq-local gptel-max-tokens 4096)
        (setq-local gptel--num-messages-to-send 8)
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_TEMPERATURE")
                :to-equal "0.7")
        (expect (org-entry-get (point-min) "GPTEL_MAX_TOKENS")
                :to-equal "4096")
        (expect (org-entry-get (point-min) "GPTEL_NUM_MESSAGES_TO_SEND")
                :to-equal "8")))

    (it "deletes scalar keys when their source variables are nil"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert (concat ":PROPERTIES:\n"
                        ":GPTEL_TEMPERATURE: 0.5\n"
                        ":GPTEL_MAX_TOKENS: 1024\n"
                        ":END:\n"))
        (setq-local gptel-temperature nil)
        (setq-local gptel-max-tokens nil)
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_TEMPERATURE")
                :to-be nil)
        (expect (org-entry-get (point-min) "GPTEL_MAX_TOKENS")
                :to-be nil)))

    (it "writes GPTEL_TOOLS as a multi-valued property of names from gptel-tools"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:END:\n")
        ;; The simulated tool list uses bare symbols, mirroring the
        ;; preset-spec smoke fixtures in scope-profile snapshot tests.
        ;; The writer maps symbols to their `symbol-name'.
        (setq-local gptel-tools '(PersistentAgent run_bash_command))
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get-multivalued-property (point-min) "GPTEL_TOOLS")
                :to-equal '("PersistentAgent" "run_bash_command"))))

    (it "deletes GPTEL_TOOLS when gptel-tools is empty"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:GPTEL_TOOLS: stale\n:END:\n")
        (setq-local gptel-tools nil)
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_TOOLS")
                :to-be nil)))

    ;; Decision 2 / register/invariant/drawer-system-key-write-exclusion:
    ;; the writer never emits :GPTEL_SYSTEM: even when the preset has a
    ;; non-nil :system or `gptel--system-message' is set buffer-locally.
    (it "NEVER writes GPTEL_SYSTEM even with gptel--system-message set"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:END:\n")
        (setq-local gptel--preset 'foo)
        (setq-local gptel--system-message
                    "Long multi-line system prompt with `backticks` and *asterisks*.")
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_SYSTEM")
                :to-be nil)
        ;; The presence-of-other-keys check guards against a regression
        ;; where the writer aborts before reaching :GPTEL_PRESET:.
        (expect (org-entry-get (point-min) "GPTEL_PRESET")
                :to-equal "foo")))

    ;; Read-side back-compat: the writer does NOT delete a manually
    ;; authored :GPTEL_SYSTEM: line.  This is the asymmetric contract
    ;; (Decision 2): writer skips, reader still respects.
    (it "preserves a manually authored GPTEL_SYSTEM entry across save"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert (concat ":PROPERTIES:\n"
                        ":GPTEL_PRESET: foo\n"
                        ":GPTEL_SYSTEM: User-authored prompt.\n"
                        ":END:\n"))
        (setq-local gptel--preset 'foo)
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_SYSTEM")
                :to-equal "User-authored prompt."))))


  ;; -----------------------------------------------------------------------
  ;; 2. Integration: gptel-chat--save-state against a real chat-mode buffer.
  ;;
  ;; Exercises the full save path end-to-end.  Asserts text content of the
  ;; saved drawer (string-level assertions).  The full-snapshot contract is
  ;; demonstrably stronger than the previous spy-based assertions: a
  ;; future writer-internal change is caught by direct content checks.

  (describe "gptel-chat--save-state (integration)"

    (it "writes the full configuration snapshot to the drawer"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--empty-chat)
        (setq-local gptel--preset 'system-explorer)
        (setq-local gptel-model 'claude-sonnet-4-6)
        (setq-local gptel-temperature 0.7)
        (setq-local gptel-tools '(PersistentAgent run_bash_command))
        (gptel-chat--save-state)
        (expect (gptel-chat-save-test--has-line ":GPTEL_PRESET: system-explorer")
                :to-be-truthy)
        (expect (gptel-chat-save-test--has-line ":GPTEL_MODEL: claude-sonnet-4-6")
                :to-be-truthy)
        (expect (gptel-chat-save-test--has-line ":GPTEL_TEMPERATURE: 0.7")
                :to-be-truthy)
        (expect (gptel-chat-save-test--has-line ":GPTEL_TOOLS:")
                :to-be-truthy)
        (expect (org-entry-get-multivalued-property (point-min) "GPTEL_TOOLS")
                :to-equal '("PersistentAgent" "run_bash_command"))))

    (it "writes GPTEL_PARENT_SESSION_ID when the source var is a non-empty string"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--empty-chat)
        (setq-local jf/gptel--parent-session-id "parent-abc-20260424000000")
        (gptel-chat--save-state)
        (expect (org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID")
                :to-equal "parent-abc-20260424000000")))

    (it "does NOT write GPTEL_PARENT_SESSION_ID when the source var is nil or empty"
      (dolist (val (list nil ""))
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--empty-chat)
          (setq-local jf/gptel--parent-session-id val)
          (gptel-chat--save-state)
          (expect (org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID")
                  :to-be nil))))

    ;; Decision 1: the save path no longer routes through upstream's
    ;; `gptel-org-set-properties'.  Spy on the upstream helper and assert
    ;; it was never called — this demonstrates the contract bit-for-bit
    ;; against a future regression that re-introduces the delegation.
    (it "does NOT call upstream gptel-org-set-properties"
      (when (fboundp 'gptel-org-set-properties)
        (spy-on 'gptel-org-set-properties)
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--empty-chat)
          (setq-local gptel--preset 'foo)
          (gptel-chat--save-state)
          (expect 'gptel-org-set-properties :not :to-have-been-called))))

    (it "never writes GPTEL_BOUNDS in the saved buffer"
      (with-temp-buffer
        (gptel-chat-mode)
        ;; Buffer with an assistant block — the kind upstream's bounds-
        ;; tracker would have persisted spans for.  Our save path skips
        ;; bounds entirely (block-based response storage).
        (insert "#+begin_user\nAsk.\n#+end_user\n"
                "#+begin_assistant\nAnswer.\n#+end_assistant\n")
        (setq-local gptel--preset 'foo)
        (gptel-chat--save-state)
        (expect (gptel-chat-save-test--has-line ":GPTEL_BOUNDS:")
                :to-be nil)))

    (it "never writes GPTEL_SYSTEM even when gptel--system-message is set buffer-locally"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--empty-chat)
        (setq-local gptel--preset 'foo)
        (setq-local gptel--system-message
                    "You are a careful collaborator.")
        (gptel-chat--save-state)
        (expect (gptel-chat-save-test--has-line ":GPTEL_SYSTEM:")
                :to-be nil)))

    (it "never enables gptel-mode"
      ;; Exercise both the parent-id branch and the no-parent-id branch
      ;; and verify neither toggles `gptel-mode' (design.md §Decision 16).
      (spy-on 'gptel-mode)
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--empty-chat)
        (gptel-chat--save-state))
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--empty-chat)
        (setq-local jf/gptel--parent-session-id "parent-xyz")
        (gptel-chat--save-state))
      (expect 'gptel-mode :not :to-have-been-called))

    (it "is a defense-in-depth no-op outside chat-mode buffers"
      (with-temp-buffer
        (fundamental-mode)
        (insert ":PROPERTIES:\n:END:\n")
        ;; Simulate a stray buffer-local preset to make sure the guard
        ;; doesn't accidentally write into a non-chat-mode buffer.
        (setq-local gptel--preset 'should-not-write)
        (gptel-chat--save-state)
        (expect (org-entry-get (point-min) "GPTEL_PRESET")
                :to-be nil))))


  ;; -----------------------------------------------------------------------
  ;; 3. Cold-load: gptel-org is still required by the save hook so the
  ;; read-side overlay (`gptel-org--entry-properties') has the feature
  ;; available.  The save hook itself no longer calls upstream
  ;; `gptel-org-set-properties', but the require remains load-bearing
  ;; for the overlay.

  (describe "cold-load of gptel-org (regression)"

    (before-each
      (when (featurep 'gptel-org)
        (unload-feature 'gptel-org t)))

    (after-each
      (require 'gptel-org nil t))

    (it "save hook succeeds without pre-loading gptel-org"
      ;; Precondition: cold.
      (expect (featurep 'gptel-org) :to-be nil)
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--empty-chat)
        (setq-local gptel--preset 'foo)
        ;; Must not signal void-function (overlay reader may need
        ;; `gptel-org--entry-properties' on next mode activation).
        (gptel-chat--save-state))
      ;; Postcondition: the save hook itself loaded `gptel-org'.
      (expect (featurep 'gptel-org) :to-be-truthy)))


  ;; -----------------------------------------------------------------------
  ;; 4. Hook registration scope (design.md §Decision 10).

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
