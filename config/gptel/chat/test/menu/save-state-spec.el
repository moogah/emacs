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
;;   - The writer emits the same snapshot keys, in the same order
;;     and with the same value escaping, as the preset-spec producer
;;     `jf/gptel-scope-profile--snapshot-lines' — both consume the
;;     single `jf/gptel-scope-profile--snapshot-spec' registry (task
;;     `harden-snapshot-emission-cross-stage-parity', Findings 1, 2).
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
;; `mode.el'.  `.../config/gptel/sessions/' holds `commands.el', the
;; module that owns the shared `jf/gptel--session-headings-block'
;; heading helper used by the save-path materialiser.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir))
       (gptel-dir (expand-file-name "../../../" spec-dir))
       (sessions-dir (expand-file-name "../../../sessions/" spec-dir)))
  (add-to-list 'load-path chat-dir)
  (add-to-list 'load-path gptel-dir)
  (add-to-list 'load-path sessions-dir))

(require 'gptel)
(require 'gptel-chat-mode)
(require 'gptel-chat-menu)
;; `gptel-chat--write-config-drawer' single-sources its snapshot key
;; set through the `gptel-scope-profiles' registry (task
;; `harden-snapshot-emission-cross-stage-parity', Finding 1).  The
;; writer lazily requires it; load it eagerly here so the unit specs
;; that call the writer directly have the registry available.
(require 'gptel-scope-profiles)
;; `gptel-chat--write-system-prompt-heading' materialises the
;; `* System Prompt' / `* Chat' headings for pre-Addendum sessions via
;; `jf/gptel--session-headings-block' in `gptel-session-commands'.
;; The writer lazily requires it; load it eagerly so the
;; materialisation spec exercises the real heading helper.
(require 'gptel-session-commands nil t)

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

(defun gptel-chat-save-test--drawer-snapshot-keys ()
  "Return the snapshot drawer keys present in the current buffer, in order.
Scans `:GPTEL_*:' property lines from `point-min' and keeps only
those in `jf/gptel-scope-profile--snapshot-keys' — i.e. the result
ignores `:GPTEL_PRESET:' / `:GPTEL_PARENT_SESSION_ID:' and any scope
keys, isolating the snapshot key set for cross-producer comparison."
  (let ((snapshot (jf/gptel-scope-profile--snapshot-keys))
        keys)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^:\\(GPTEL_[A-Z_]+\\): " nil t)
        (let ((key (match-string 1)))
          (when (member key snapshot)
            (push key keys)))))
    (nreverse keys)))


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

    (it "deletes GPTEL_MODEL when gptel-model is nil"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert (concat ":PROPERTIES:\n"
                        ":GPTEL_MODEL: claude-sonnet-4-6\n"
                        ":END:\n"))
        (setq-local gptel-model nil)
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_MODEL")
                :to-be nil)))

    (it "deletes GPTEL_BACKEND when gptel-backend is nil"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert (concat ":PROPERTIES:\n"
                        ":GPTEL_BACKEND: stale\n"
                        ":END:\n"))
        (setq-local gptel-backend nil)
        (gptel-chat--write-config-drawer)
        (expect (org-entry-get (point-min) "GPTEL_BACKEND")
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
  ;; 1b. Cross-producer key-set parity.
  ;;
  ;; Task `harden-snapshot-emission-cross-stage-parity', Finding 1: the
  ;; chat-mode save path must emit the *same* snapshot keys, in the same
  ;; canonical order, as the preset-spec producer
  ;; `jf/gptel-scope-profile--snapshot-lines'.  Both now consume the
  ;; single `jf/gptel-scope-profile--snapshot-spec' registry, so the
  ;; writer is no longer an independent third enumeration that can
  ;; drift when a snapshot key is added.

  (describe "cross-producer key-set parity (Finding 1)"

    (it "emits the same snapshot keys, in the same order, as --snapshot-lines"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:END:\n")
        (setq-local gptel-model 'claude-sonnet-4-6)
        (setq-local gptel-backend "Claude")
        (setq-local gptel-tools '(PersistentAgent run_bash_command))
        (setq-local gptel-temperature 0.7)
        (setq-local gptel-max-tokens 4096)
        (setq-local gptel--num-messages-to-send 8)
        (gptel-chat--write-config-drawer)
        (let ((writer-keys (gptel-chat-save-test--drawer-snapshot-keys))
              (lines-keys
               (mapcar (lambda (line)
                         (and (string-match "^:\\(GPTEL_[A-Z_]+\\):" line)
                              (match-string 1 line)))
                       (jf/gptel-scope-profile--snapshot-lines
                        '(:model claude-sonnet-4-6
                          :backend "Claude"
                          :tools (PersistentAgent run_bash_command)
                          :temperature 0.7
                          :max-tokens 4096
                          :num-messages-to-send 8)))))
          ;; Writer drawer order == --snapshot-lines order.
          (expect writer-keys :to-equal lines-keys)
          ;; And both equal the canonical registry order.
          (expect writer-keys
                  :to-equal
                  '("GPTEL_MODEL" "GPTEL_BACKEND" "GPTEL_TOOLS"
                    "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS"
                    "GPTEL_NUM_MESSAGES_TO_SEND")))))

    (it "writes a snapshot value byte-identical to the --snapshot-lines token"
      ;; Guards Finding 2's escaping parity at the writer: a multivalued
      ;; value with whitespace must serialise identically through the
      ;; writer (org-entry-put-multivalued-property) and the string
      ;; renderer (--snapshot-lines).
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:END:\n")
        (setq-local gptel-tools '("name with space"))
        (gptel-chat--write-config-drawer)
        (let* ((lines-line (car (jf/gptel-scope-profile--snapshot-lines
                                 '(:tools ("name with space")))))
               (lines-raw (and (string-match "^:GPTEL_TOOLS: \\(.*\\)$"
                                             lines-line)
                               (match-string 1 lines-line))))
          ;; `org-entry-get' returns the raw (escaped) drawer text.
          (expect (org-entry-get (point-min) "GPTEL_TOOLS")
                  :to-equal lines-raw)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_TOOLS")
                  :to-equal '("name with space"))))))


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


  ;; -----------------------------------------------------------------------
  ;; 5. System Prompt heading save
  ;; (task `make-system-prompt-heading-authoritative', design.md
  ;; §Addendum Finding B / Decision B;
  ;; register/invariant/system-prompt-heading-authoritative).
  ;;
  ;; On `before-save-hook', after the config drawer is written, the
  ;; current buffer-local `gptel--system-message' is serialised back
  ;; into the `* System Prompt' heading body — never as a
  ;; :GPTEL_SYSTEM: drawer line (composes with
  ;; register/invariant/drawer-system-key-write-exclusion).  For a
  ;; pre-Addendum session with no heading, the `* System Prompt' /
  ;; `* Chat' headings are materialised via the shared
  ;; `jf/gptel--session-headings-block' helper.

  (describe "system prompt heading save"

    (defconst gptel-chat-save-test--with-headings
      (concat ":PROPERTIES:\n:END:\n"
              "\n* System Prompt\n"
              ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
              "Old body.\n"
              "\n* Chat\n#+begin_user\n\nHi.\n#+end_user\n")
      "Current-layout session: drawer + folded heading + chat heading.")

    (it "rewrites the heading body from buffer-local gptel--system-message"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--with-headings)
        (setq-local gptel--system-message "Updated system prompt.")
        (gptel-chat--save-state)
        (expect (gptel-chat--system-prompt-heading-body)
                :to-equal "Updated system prompt.")))

    (it "never writes a :GPTEL_SYSTEM: drawer line on save"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--with-headings)
        (setq-local gptel--system-message
                    "Long prompt with `backticks`, *asterisks*, and\nnewlines.")
        (gptel-chat--save-state)
        (expect (org-entry-get (point-min) "GPTEL_SYSTEM") :to-be nil)
        (expect (gptel-chat-save-test--has-line ":GPTEL_SYSTEM:")
                :to-be nil)
        ;; The heading body did receive the value.
        (expect (gptel-chat--system-prompt-heading-body)
                :to-equal
                "Long prompt with `backticks`, *asterisks*, and\nnewlines.")))

    (it "keeps the heading a singleton after save"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--with-headings)
        (setq-local gptel--system-message "A")
        (gptel-chat--save-state)
        (goto-char (point-min))
        (expect (how-many "^\\* System Prompt[ \t]*$") :to-equal 1)
        (expect (how-many "^\\* Chat[ \t]*$") :to-equal 1)))

    (it "materialises the heading for a pre-Addendum session with no heading"
      ;; Old session: config drawer + turn blocks, no `* System
      ;; Prompt' / `* Chat' headings.  Save materialises them.
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:GPTEL_PRESET: foo\n:END:\n"
                "\n#+begin_user\nHello.\n#+end_user\n")
        (setq-local gptel--system-message "Materialised prompt.")
        (gptel-chat--save-state)
        ;; Headings now exist exactly once.
        (goto-char (point-min))
        (expect (how-many "^\\* System Prompt[ \t]*$") :to-equal 1)
        (expect (how-many "^\\* Chat[ \t]*$") :to-equal 1)
        ;; The body carries the buffer-local system message.
        (expect (gptel-chat--system-prompt-heading-body)
                :to-equal "Materialised prompt.")
        ;; The folded-visibility property is present (shared helper).
        (expect (gptel-chat-save-test--has-line ":VISIBILITY: folded")
                :to-be-truthy)
        ;; The original turn block is preserved, under `* Chat'.
        (expect (gptel-chat-save-test--has-line "#+begin_user")
                :to-be-truthy)
        (let ((chat-pos (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "^\\* Chat[ \t]*$" nil t)))
              (turn-pos (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "^#\\+begin_user" nil t))))
          (expect (and chat-pos turn-pos (< chat-pos turn-pos))
                  :to-be-truthy))
        ;; Still no :GPTEL_SYSTEM: drawer line.
        (expect (gptel-chat-save-test--has-line ":GPTEL_SYSTEM:")
                :to-be nil)))

    (it "is idempotent — a no-change save produces no buffer diff"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert gptel-chat-save-test--with-headings)
        (setq-local gptel--system-message "Stable prompt.")
        (gptel-chat--save-state)
        (let ((after-first (buffer-string)))
          (gptel-chat--save-state)
          (expect (buffer-string) :to-equal after-first))))

    (it "leaves a materialised session stable on a second save"
      (with-temp-buffer
        (gptel-chat-mode)
        (insert ":PROPERTIES:\n:GPTEL_PRESET: foo\n:END:\n"
                "\n#+begin_user\nHello.\n#+end_user\n")
        (setq-local gptel--system-message "Materialised prompt.")
        (gptel-chat--save-state)
        (let ((after-materialise (buffer-string)))
          (gptel-chat--save-state)
          (expect (buffer-string) :to-equal after-materialise))))

    ;; ---------------------------------------------------------------------
    ;; Off-nominal layout regression: `* System Prompt' heading present,
    ;; `* Chat' heading absent (task `harden-system-prompt-save-against-
    ;; missing-chat-heading').  Without a `* Chat' heading the body-region
    ;; helper's SUBTREE-END falls through to `point-max', so a naive
    ;; delete-region from BODY-START would silently delete every
    ;; `#+begin_user' / `#+begin_assistant' turn block sitting in the
    ;; orphaned subtree.  The writer must (a) preserve those turn blocks
    ;; and (b) re-materialise `* Chat' so the document satisfies the
    ;; singleton-`* Chat' / turn-blocks-under-`* Chat' invariants of
    ;; `register/shape/session-document-layout'.
    (describe "off-nominal layout: `* System Prompt' without `* Chat'"

      (defconst gptel-chat-save-test--orphan-system-prompt
        (concat ":PROPERTIES:\n:GPTEL_PRESET: foo\n:END:\n"
                "\n* System Prompt\n"
                ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                "Old body.\n"
                "\n"
                "#+begin_user\n"
                "Hi.\n"
                "#+end_user\n"
                "\n"
                "#+begin_assistant\n"
                "Hello.\n"
                "#+end_assistant\n")
        "Off-nominal layout: drawer + folded `* System Prompt' (with turn
blocks below it), no `* Chat' heading.")

      (it "preserves turn blocks when `* Chat' is missing"
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--orphan-system-prompt)
          (setq-local gptel--system-message "Recovered prompt.")
          (gptel-chat--save-state)
          ;; The user and assistant turn blocks are still present —
          ;; the off-nominal save did not silently delete them.
          (expect (gptel-chat-save-test--has-line "#+begin_user")
                  :to-be-truthy)
          (expect (gptel-chat-save-test--has-line "#+begin_assistant")
                  :to-be-truthy)
          (expect (gptel-chat-save-test--has-line "Hi.")
                  :to-be-truthy)
          (expect (gptel-chat-save-test--has-line "Hello.")
                  :to-be-truthy)))

      (it "re-materialises `* Chat' as a singleton"
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--orphan-system-prompt)
          (setq-local gptel--system-message "Recovered prompt.")
          (gptel-chat--save-state)
          (goto-char (point-min))
          ;; The shape invariants from
          ;; `register/shape/session-document-layout' now hold.
          (expect (how-many "^\\* System Prompt[ \t]*$") :to-equal 1)
          (expect (how-many "^\\* Chat[ \t]*$") :to-equal 1)))

      (it "places every turn block under `* Chat'"
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--orphan-system-prompt)
          (setq-local gptel--system-message "Recovered prompt.")
          (gptel-chat--save-state)
          (let ((chat-pos (save-excursion
                            (goto-char (point-min))
                            (re-search-forward "^\\* Chat[ \t]*$" nil t)))
                (first-turn (save-excursion
                              (goto-char (point-min))
                              (re-search-forward
                               "^#\\+begin_\\(user\\|assistant\\)"
                               nil t))))
            (expect chat-pos :to-be-truthy)
            (expect first-turn :to-be-truthy)
            ;; Every turn block now lives after `* Chat' (no
            ;; turn-block-before-chat-heading violation).
            (expect chat-pos :to-be-less-than first-turn))))

      (it "writes the recovered system-prompt body into the heading"
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--orphan-system-prompt)
          (setq-local gptel--system-message "Recovered prompt.")
          (gptel-chat--save-state)
          (expect (gptel-chat--system-prompt-heading-body)
                  :to-equal "Recovered prompt.")))

      (it "satisfies all four invariants of session-document-layout"
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--orphan-system-prompt)
          (setq-local gptel--system-message "Recovered prompt.")
          (gptel-chat--save-state)
          ;; Inlined copy of the validator from
          ;; `register/shape/session-document-layout' so the test
          ;; reads the invariants from the same predicate prose the
          ;; register publishes.
          (save-excursion
            (goto-char (point-min))
            (expect (looking-at-p "[ \t\n]*:PROPERTIES:")
                    :to-be-truthy))
          (expect (how-many "^\\* System Prompt[ \t]*$"
                            (point-min) (point-max))
                  :to-equal 1)
          (expect (how-many "^\\* Chat[ \t]*$"
                            (point-min) (point-max))
                  :to-equal 1)
          (let ((chat-pos (save-excursion
                            (goto-char (point-min))
                            (re-search-forward "^\\* Chat[ \t]*$" nil t)))
                (turn-pos (save-excursion
                            (goto-char (point-min))
                            (re-search-forward
                             "^#\\+begin_\\(user\\|assistant\\)"
                             nil t))))
            (expect (or (null turn-pos)
                        (null chat-pos)
                        (< chat-pos turn-pos))
                    :to-be-truthy))))

      (it "still never writes a :GPTEL_SYSTEM: drawer line on recovery"
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--orphan-system-prompt)
          (setq-local gptel--system-message "Recovered prompt.")
          (gptel-chat--save-state)
          ;; The recovery path must compose with
          ;; `register/invariant/drawer-system-key-write-exclusion'.
          (expect (gptel-chat-save-test--has-line ":GPTEL_SYSTEM:")
                  :to-be nil)
          (expect (org-entry-get (point-min) "GPTEL_SYSTEM") :to-be nil)))

      (it "is idempotent — second save after recovery produces no diff"
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--orphan-system-prompt)
          (setq-local gptel--system-message "Recovered prompt.")
          (gptel-chat--save-state)
          (let ((after-recovery (buffer-string)))
            (gptel-chat--save-state)
            (expect (buffer-string) :to-equal after-recovery))))

      (it "still re-materialises `* Chat' when no turn blocks exist"
        ;; Edge case: orphaned `* System Prompt' with no turn blocks
        ;; at all (only the system-prompt body).  There is no data to
        ;; preserve, but the canonical layout still requires `* Chat'.
        (with-temp-buffer
          (gptel-chat-mode)
          (insert ":PROPERTIES:\n:GPTEL_PRESET: foo\n:END:\n"
                  "\n* System Prompt\n"
                  ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  "Body only, no chat.\n")
          (setq-local gptel--system-message "Recovered prompt.")
          (gptel-chat--save-state)
          (goto-char (point-min))
          (expect (how-many "^\\* System Prompt[ \t]*$") :to-equal 1)
          (expect (how-many "^\\* Chat[ \t]*$") :to-equal 1))))

    ;; ---------------------------------------------------------------------
    ;; Structural spec for `gptel-chat--system-prompt-heading-body-region',
    ;; the shared reader/writer scan helper introduced by task
    ;; `extract-system-prompt-heading-region-helper'.  The helper returns
    ;; the post-drawer body region (BODY-START . SUBTREE-END) verbatim;
    ;; the reader trims the substring, the writer pads with the layout's
    ;; required newlines.  Pinning the contract here prevents silent
    ;; reader/writer divergence on what "body" means (the shape-
    ;; corruption class register/shape/session-document-layout guards).
    (describe "gptel-chat--system-prompt-heading-body-region (helper)"
      (it "returns the post-drawer region that backs both reader and writer"
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--with-headings)
          (let* ((region (gptel-chat--system-prompt-heading-body-region))
                 (body-start (car region))
                 (subtree-end (cdr region))
                 (verbatim (buffer-substring-no-properties
                            body-start subtree-end)))
            ;; A region is returned (heading is present).
            (expect region :not :to-be nil)
            (expect body-start :to-be-truthy)
            (expect subtree-end :to-be-truthy)
            (expect body-start :to-be-less-than subtree-end)
            ;; The reader's result is the trimmed verbatim region.
            (expect (gptel-chat--system-prompt-heading-body)
                    :to-equal (string-trim-right verbatim))
            ;; SUBTREE-END is the start of the next `* ' heading.
            (save-excursion
              (goto-char subtree-end)
              (expect (looking-at-p "^\\* ") :to-be-truthy))
            ;; BODY-START is positioned past the heading's `:PROPERTIES:'
            ;; drawer — `:END:' lies before BODY-START, and no drawer line
            ;; appears in the returned region.
            (expect (string-match-p ":PROPERTIES:" verbatim) :to-be nil)
            (expect (string-match-p ":END:" verbatim) :to-be nil))))

      (it "returns nil when the `* System Prompt' heading is absent"
        (with-temp-buffer
          (gptel-chat-mode)
          (insert ":PROPERTIES:\n:GPTEL_PRESET: foo\n:END:\n"
                  "\n#+begin_user\nHello.\n#+end_user\n")
          (expect (gptel-chat--system-prompt-heading-body-region)
                  :to-be nil)))

      (it "delimits the region the writer's delete-region acts on"
        ;; A round-trip that replaces the body verifies that the writer
        ;; operates on exactly the bounds the helper returned: every
        ;; character between BODY-START and SUBTREE-END is replaced, and
        ;; nothing outside it is touched (the heading line and its
        ;; `:PROPERTIES:' drawer survive, and the next `* Chat' heading
        ;; survives intact).
        (with-temp-buffer
          (gptel-chat-mode)
          (insert gptel-chat-save-test--with-headings)
          (setq-local gptel--system-message "Replacement.")
          (gptel-chat--write-system-prompt-heading)
          ;; Heading line preserved exactly once.
          (goto-char (point-min))
          (expect (how-many "^\\* System Prompt[ \t]*$") :to-equal 1)
          ;; Heading's own `:VISIBILITY: folded' drawer survives.
          (expect (gptel-chat-save-test--has-line ":VISIBILITY: folded")
                  :to-be-truthy)
          ;; `* Chat' heading survives intact past SUBTREE-END.
          (goto-char (point-min))
          (expect (how-many "^\\* Chat[ \t]*$") :to-equal 1)
          ;; Reader sees the replacement (round-trip stable).
          (expect (gptel-chat--system-prompt-heading-body)
                  :to-equal "Replacement.")))))

  )

(provide 'gptel-chat-save-state-spec)

;;; save-state-spec.el ends here
