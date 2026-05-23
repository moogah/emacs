;;; preset-wiring-spec.el --- Buttercup tests for gptel-chat preset wiring -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the preset-application hook delivered by task
;; `preset-wiring' (see
;; openspec/changes/gptel-chat-mode/tasks/open/preset-wiring.md).
;;
;; Coverage (spec §"Preset system integration"):
;;   - Preset applied from Org property drawer (`:GPTEL_PRESET: coding').
;;   - Preset applied from file-local `gptel--preset' variable.
;;   - No preset declared → hook is a no-op.
;;   - Property drawer wins over file-local when both are present and
;;     name different presets.
;;   - `gptel-mode' is NOT enabled as a side effect of preset application
;;     (design.md §Decision 16 — chat-mode owns the major-mode role).
;;
;; Test structure:
;;
;;   1. `gptel-chat--declared-preset' (pure resolver) — exercised against
;;      a plain buffer populated with the relevant source of truth.
;;      Uses `org-mode' (not `gptel-chat-mode') so `kill-all-local-
;;      variables' does not wipe the file-local we are testing.
;;
;;   2. `gptel-chat--apply-declared-preset' (hook entry point) —
;;      invoked directly with controlled preconditions.  `gptel-get-
;;      preset', `gptel--apply-preset', and `gptel-mode' are spied.
;;
;;   3. `gptel-chat-mode' activation — end-to-end test that the mode-
;;      hook path applies the drawer preset and does not toggle
;;      `gptel-mode'.  File-local activation is covered by a separate
;;      spec that simulates `hack-local-variables-hook'.
;;
;; The declared-preset resolver uses the real `org-entry-get' parser,
;; which is the same entry point upstream uses in
;; `gptel-org--entry-properties' (`gptel-org.el:523-531').

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


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-preset-test--drawer-coding
  (concat ":PROPERTIES:\n"
          ":GPTEL_PRESET: coding\n"
          ":END:\n"
          "\n"
          "#+begin_user\n"
          "Hello.\n"
          "#+end_user\n")
  "Buffer content with a :GPTEL_PRESET: coding drawer at point-min.")

(defconst gptel-chat-preset-test--no-preset
  (concat "#+begin_user\n"
          "Hello.\n"
          "#+end_user\n")
  "Buffer content with no preset declaration of any kind.")


;;; Specs --------------------------------------------------------------------

(describe "gptel-chat preset wiring"

  ;; -----------------------------------------------------------------------
  ;; 1. Pure resolver: `gptel-chat--declared-preset'.
  ;;
  ;; Uses `org-mode' (not `gptel-chat-mode') so `kill-all-local-variables'
  ;; does not erase the buffer-local values we install for the test.  The
  ;; resolver itself does not depend on `gptel-chat-mode' being active —
  ;; it reads the drawer via `org-entry-get' and `gptel--preset'
  ;; directly.  Testing against `org-mode' isolates the resolver's
  ;; behaviour from the mode-activation hooks that are tested separately
  ;; below.

  (describe "gptel-chat--declared-preset (pure resolver)"

    (it "returns the drawer preset as a symbol"
      (with-temp-buffer
        (insert gptel-chat-preset-test--drawer-coding)
        (org-mode)
        (expect (gptel-chat--declared-preset) :to-equal 'coding)))

    (it "returns the file-local gptel--preset value when no drawer is present"
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (org-mode)
        (setq-local gptel--preset 'coding)
        (expect (gptel-chat--declared-preset) :to-equal 'coding)))

    (it "prefers the drawer value when both sources disagree"
      (with-temp-buffer
        (insert gptel-chat-preset-test--drawer-coding)
        (org-mode)
        (setq-local gptel--preset 'research)
        (expect (gptel-chat--declared-preset) :to-equal 'coding)))

    (it "returns nil when neither source supplies a preset"
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (org-mode)
        (expect (gptel-chat--declared-preset) :to-be nil)))

    (it "coerces a stringy file-local value to a symbol"
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (org-mode)
        (setq-local gptel--preset "coding")
        (expect (gptel-chat--declared-preset) :to-equal 'coding)))

    (it "ignores an empty drawer value"
      (with-temp-buffer
        (insert (concat ":PROPERTIES:\n"
                        ":GPTEL_PRESET:\n"
                        ":END:\n"))
        (org-mode)
        (expect (gptel-chat--declared-preset) :to-be nil))))


  ;; -----------------------------------------------------------------------
  ;; 2. Hook entry point: `gptel-chat--apply-declared-preset'.
  ;;
  ;; Invoked directly (not through a mode-activation path) so the test
  ;; controls every precondition.  All three upstream dependencies are
  ;; spied:
  ;;
  ;;   `gptel-get-preset'   — controls the "resolvable preset" branch.
  ;;   `gptel--apply-preset' — captures the call and lets the test
  ;;                           exercise the setter lambda.
  ;;   `gptel-mode'         — asserted never-called (design.md §16).

  (describe "gptel-chat--apply-declared-preset (hook entry)"

    (before-each
      (spy-on 'gptel-get-preset :and-call-fake
              (lambda (name) (memq name '(coding research))))
      (spy-on 'gptel--apply-preset :and-call-fake
              (lambda (_preset setter)
                ;; Exercise the setter once so the "buffer-local"
                ;; assertion in the next spec can verify the shape.
                (when setter (funcall setter 'gptel--preset _preset))))
      (spy-on 'gptel-mode :and-return-value nil))

    (it "applies the preset when a drawer declares one"
      (with-temp-buffer
        (insert gptel-chat-preset-test--drawer-coding)
        (org-mode)
        (gptel-chat--apply-declared-preset)
        (expect 'gptel--apply-preset :to-have-been-called)
        (let ((args (spy-calls-args-for 'gptel--apply-preset 0)))
          (expect (nth 0 args) :to-equal 'coding))))

    (it "passes a buffer-local setter lambda to gptel--apply-preset"
      (with-temp-buffer
        (insert gptel-chat-preset-test--drawer-coding)
        (org-mode)
        (gptel-chat--apply-declared-preset)
        (let* ((args (spy-calls-args-for 'gptel--apply-preset 0))
               (setter (nth 1 args)))
          (expect (functionp setter) :to-be t)
          ;; The setter must install a BUFFER-local binding — not a
          ;; global one.  Invoke it with a probe symbol and verify
          ;; locality.
          (funcall setter 'gptel-chat-test--probe 'sentinel)
          (expect (local-variable-p 'gptel-chat-test--probe) :to-be t)
          (expect gptel-chat-test--probe :to-equal 'sentinel))))

    (it "applies the preset when only a file-local gptel--preset is set"
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (org-mode)
        (setq-local gptel--preset 'coding)
        (gptel-chat--apply-declared-preset)
        (expect 'gptel--apply-preset :to-have-been-called)
        (let ((args (spy-calls-args-for 'gptel--apply-preset 0)))
          (expect (nth 0 args) :to-equal 'coding))))

    (it "prefers the drawer value when both sources disagree"
      (with-temp-buffer
        (insert gptel-chat-preset-test--drawer-coding)
        (org-mode)
        (setq-local gptel--preset 'research)
        (gptel-chat--apply-declared-preset)
        (expect 'gptel--apply-preset :to-have-been-called)
        (let ((args (spy-calls-args-for 'gptel--apply-preset 0)))
          (expect (nth 0 args) :to-equal 'coding))))

    (it "does nothing when no preset is declared"
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (org-mode)
        (gptel-chat--apply-declared-preset)
        (expect 'gptel--apply-preset :not :to-have-been-called)))

    (it "never enables gptel-mode"
      ;; Exercise all three resolution outcomes and verify none of
      ;; them turn on `gptel-mode' (design.md §Decision 16).
      (with-temp-buffer
        (insert gptel-chat-preset-test--drawer-coding)
        (org-mode)
        (gptel-chat--apply-declared-preset))
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (org-mode)
        (setq-local gptel--preset 'coding)
        (gptel-chat--apply-declared-preset))
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (org-mode)
        (gptel-chat--apply-declared-preset))
      (expect 'gptel-mode :not :to-have-been-called))

    (it "warns but does not error when the preset name does not resolve"
      (let ((warnings 0))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest _) (cl-incf warnings))))
          (with-temp-buffer
            (insert (concat ":PROPERTIES:\n"
                            ":GPTEL_PRESET: nonexistent-preset\n"
                            ":END:\n"))
            (org-mode)
            (gptel-chat--apply-declared-preset)
            (expect warnings :to-equal 1)
            (expect 'gptel--apply-preset :not :to-have-been-called))))))


  ;; -----------------------------------------------------------------------
  ;; 3. Mode activation: `gptel-chat-mode' activation must run the
  ;; preset-application hook for drawer content (which survives
  ;; `kill-all-local-variables' because it lives in buffer text, not in
  ;; a buffer-local variable).
  ;;
  ;; File-local `gptel--preset' activation during a real file open is
  ;; delivered by the second hook (`hack-local-variables-hook') and is
  ;; verified separately below.

  (describe "gptel-chat-mode activation"

    (before-each
      (spy-on 'gptel-get-preset :and-call-fake
              (lambda (name) (memq name '(coding research))))
      (spy-on 'gptel--apply-preset :and-return-value nil)
      (spy-on 'gptel-mode :and-return-value nil))

    (it "applies the drawer preset when the mode activates"
      (with-temp-buffer
        (insert gptel-chat-preset-test--drawer-coding)
        (gptel-chat-mode)
        (expect 'gptel--apply-preset :to-have-been-called)
        (let ((args (spy-calls-args-for 'gptel--apply-preset 0)))
          (expect (nth 0 args) :to-equal 'coding))))

    (it "is a no-op when no preset is declared"
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (gptel-chat-mode)
        (expect 'gptel--apply-preset :not :to-have-been-called)))

    (it "does not enable gptel-mode"
      (with-temp-buffer
        (insert gptel-chat-preset-test--drawer-coding)
        (gptel-chat-mode)
        (expect 'gptel-mode :not :to-have-been-called))))


  ;; -----------------------------------------------------------------------
  ;; 4. `hack-local-variables-hook' path.
  ;;
  ;; File-local `gptel--preset' is set *after* mode hooks run (see
  ;; `run-mode-hooks' in subr.el: hooks run before `hack-local-
  ;; variables'), so the mode-hook entry alone cannot catch it on a
  ;; real file-open.  `gptel-chat--apply-declared-preset-after-locals'
  ;; runs on `hack-local-variables-hook' and, gated on the current
  ;; major mode, re-invokes the applier.  These tests simulate that
  ;; second hook firing.

  (describe "hack-local-variables-hook handler"

    (before-each
      (spy-on 'gptel-get-preset :and-call-fake
              (lambda (name) (memq name '(coding research))))
      (spy-on 'gptel--apply-preset :and-return-value nil)
      (spy-on 'gptel-mode :and-return-value nil))

    (it "applies the preset when run in a chat-mode buffer"
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (gptel-chat-mode)
        ;; `hack-local-variables' would install this buffer-local after
        ;; the mode-hook runs; simulate that timing.
        (setq-local gptel--preset 'coding)
        ;; Clear the mode-hook's earlier call (if any) so this spec
        ;; observes only the post-locals invocation.
        (spy-calls-reset 'gptel--apply-preset)
        (gptel-chat--apply-declared-preset-after-locals)
        (expect 'gptel--apply-preset :to-have-been-called)
        (let ((args (spy-calls-args-for 'gptel--apply-preset 0)))
          (expect (nth 0 args) :to-equal 'coding))))

    (it "is a no-op in buffers that are not chat-mode"
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (org-mode)  ; not chat-mode
        (setq-local gptel--preset 'coding)
        (gptel-chat--apply-declared-preset-after-locals)
        (expect 'gptel--apply-preset :not :to-have-been-called))))


  ;; -----------------------------------------------------------------------
  ;; 5. Buffer-local hook registration (task `preset-wiring-robustness',
  ;; Finding 1).
  ;;
  ;; Loading `gptel-chat-menu' must NOT install
  ;; `gptel-chat--apply-declared-preset-after-locals' onto the *global*
  ;; `hack-local-variables-hook'.  A global registration makes every
  ;; file-open in every unrelated buffer pay a `derived-mode-p' check,
  ;; even when the user never touches chat-mode.  Registration must
  ;; happen buffer-locally from the mode-hook handler, so the hook only
  ;; fires in chat-mode buffers.

  (describe "hook registration scope"

    (it "does not register the after-locals hook globally at module load"
      ;; The module has already loaded via `(require 'gptel-chat-menu)'
      ;; at the top of this spec file.  Assert the GLOBAL value of
      ;; `hack-local-variables-hook' does not contain our function.
      (expect (memq 'gptel-chat--apply-declared-preset-after-locals
                    (default-value 'hack-local-variables-hook))
              :to-be nil))

    (it "registers the after-locals hook buffer-locally on mode activation"
      (with-temp-buffer
        (insert gptel-chat-preset-test--no-preset)
        (gptel-chat-mode)
        ;; The mode-hook handler `gptel-chat--install-preset-hooks'
        ;; should have added the after-locals hook as a buffer-local
        ;; entry on `hack-local-variables-hook'.
        (expect (local-variable-p 'hack-local-variables-hook) :to-be t)
        (expect (memq 'gptel-chat--apply-declared-preset-after-locals
                      hack-local-variables-hook)
                :to-be-truthy)
        ;; And must still NOT be on the global list.
        (expect (memq 'gptel-chat--apply-declared-preset-after-locals
                      (default-value 'hack-local-variables-hook))
                :to-be nil)))

    (it "leaves the global hack-local-variables-hook untouched when opening non-chat buffers"
      ;; Simulate opening an ordinary (non-chat) buffer.  The
      ;; registration path should never run, so the global hook stays
      ;; clean.
      (with-temp-buffer
        (fundamental-mode)
        (expect (memq 'gptel-chat--apply-declared-preset-after-locals
                      (default-value 'hack-local-variables-hook))
                :to-be nil))))


  ;; -----------------------------------------------------------------------
  ;; 6. Native drawer parser (task `preset-wiring-robustness', Finding 2).
  ;;
  ;; `gptel-chat--declared-preset' must parse a `:PROPERTIES:' drawer
  ;; using its own `re-search-forward' — not via `org-entry-get'.  This
  ;; lets chat-mode buffers (which may derive from `text-mode' in some
  ;; configurations) find a declared preset without requiring `org' to
  ;; be loaded, and insulates us from any future change to org's drawer
  ;; API.

  (describe "native drawer parser"

    (it "reads the preset from a drawer in a fundamental-mode buffer"
      ;; No `org-mode' activation — exercises the native regex path.
      (with-temp-buffer
        (fundamental-mode)
        (insert gptel-chat-preset-test--drawer-coding)
        (expect (gptel-chat--declared-preset) :to-equal 'coding)))

    (it "reads the preset from a drawer in a text-mode buffer"
      ;; `text-mode' does NOT autoload or require `org'.
      (with-temp-buffer
        (text-mode)
        (insert gptel-chat-preset-test--drawer-coding)
        (expect (gptel-chat--declared-preset) :to-equal 'coding)))

    (it "does not call org-entry-get on the drawer path"
      ;; Spy on `org-entry-get' and confirm the resolver never reaches
      ;; it.  This catches accidental regressions toward the org-
      ;; coupled implementation.
      (spy-on 'org-entry-get :and-call-through)
      (with-temp-buffer
        (fundamental-mode)
        (insert gptel-chat-preset-test--drawer-coding)
        (gptel-chat--declared-preset)
        (expect 'org-entry-get :not :to-have-been-called)))

    (it "tolerates trailing whitespace on the GPTEL_PRESET line"
      (with-temp-buffer
        (fundamental-mode)
        (insert ":PROPERTIES:\n"
                ":GPTEL_PRESET: coding   \n"
                ":END:\n")
        (expect (gptel-chat--declared-preset) :to-equal 'coding)))

    (it "tolerates leading indentation on drawer lines"
      (with-temp-buffer
        (fundamental-mode)
        (insert "  :PROPERTIES:\n"
                "  :GPTEL_PRESET: coding\n"
                "  :END:\n")
        (expect (gptel-chat--declared-preset) :to-equal 'coding)))

    (it "ignores a GPTEL_PRESET line outside the first drawer at point-min"
      ;; When the first meaningful content is not a `:PROPERTIES:'
      ;; block, a later drawer-like stanza is not consulted — mirrors
      ;; upstream's `selective' scope at `point-min'.
      (with-temp-buffer
        (fundamental-mode)
        (insert "Some prose.\n\n"
                ":PROPERTIES:\n"
                ":GPTEL_PRESET: coding\n"
                ":END:\n")
        (expect (gptel-chat--declared-preset) :to-be nil)))

    (it "returns nil cleanly on a malformed drawer (missing :END:)"
      (with-temp-buffer
        (fundamental-mode)
        (insert ":PROPERTIES:\n"
                ":GPTEL_PRESET: coding\n")
        (expect (gptel-chat--declared-preset) :to-be nil))))


  ;; -----------------------------------------------------------------------
  ;; 7. Drawer overrides overlay
  ;; (task `chat-drawer-overrides-overlay', design.md §Decisions 2, 3, 5).
  ;;
  ;; `gptel-chat--apply-drawer-overrides' overlays non-preset drawer
  ;; properties buffer-locally.  It runs from
  ;; `gptel-chat--apply-declared-preset' after preset application (and
  ;; again in the no-preset branch) so drawers with tools/model/token
  ;; overrides AND drawers containing only `GPTEL_PARENT_SESSION_ID'
  ;; are both honored on mode activation.
  ;;
  ;; Tests use `spy-on' to intercept upstream
  ;; `gptel-org--entry-properties' and drive each branch from a
  ;; controlled tuple.  The real function depends on
  ;; `gptel--known-backends' and `gptel-get-tool' lookups that we do
  ;; not want to exercise at the unit-test boundary (those are covered
  ;; by integration tests in `save-state-spec.el').

  (describe "drawer overrides overlay"

    (describe "gptel-chat--apply-drawer-overrides (unit)"

      (it "installs gptel-tools buffer-locally from the drawer tuple"
        ;; Simulate upstream returning a tools list.  Preset is nil
        ;; because this is the "overlay-only" call-site — the caller
        ;; has already applied any preset.
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list nil nil nil nil nil nil nil '(tool-a tool-b tool-c)))
        (with-temp-buffer
          (org-mode)
          (gptel-chat--apply-drawer-overrides)
          (expect (local-variable-p 'gptel-tools) :to-be t)
          (expect gptel-tools :to-equal '(tool-a tool-b tool-c))))

      (it "installs every upstream-compatible key when present"
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list nil "sys-msg" 'backend-obj 'model-sym 0.5 4096 3
                      '(tool-a)))
        (with-temp-buffer
          (org-mode)
          (gptel-chat--apply-drawer-overrides)
          (expect (local-variable-p 'gptel--system-message) :to-be t)
          (expect gptel--system-message :to-equal "sys-msg")
          (expect (local-variable-p 'gptel-backend) :to-be t)
          (expect gptel-backend :to-equal 'backend-obj)
          (expect (local-variable-p 'gptel-model) :to-be t)
          (expect gptel-model :to-equal 'model-sym)
          (expect (local-variable-p 'gptel-temperature) :to-be t)
          (expect gptel-temperature :to-equal 0.5)
          (expect (local-variable-p 'gptel-max-tokens) :to-be t)
          (expect gptel-max-tokens :to-equal 4096)
          (expect (local-variable-p 'gptel--num-messages-to-send) :to-be t)
          (expect gptel--num-messages-to-send :to-equal 3)
          (expect (local-variable-p 'gptel-tools) :to-be t)
          (expect gptel-tools :to-equal '(tool-a))))

      (it "is a no-op for each absent field"
        ;; Tuple is all-nil (other than preset, which is discarded).
        ;; Assert the overlay installs no buffer-local for any key —
        ;; neither the upstream keys nor `jf/gptel--parent-session-id'
        ;; (the drawer supplies no `GPTEL_PARENT_SESSION_ID').
        ;;
        ;; Asserts on `local-variable-p' of each candidate key rather
        ;; than spying `make-local-variable': `org-mode' activation and
        ;; the org-element cache init triggered by the overlay's own
        ;; `org-entry-get' call make ~83 org-internal
        ;; `make-local-variable' calls, which a global spy cannot
        ;; distinguish from the overlay's.  `local-variable-p' tests
        ;; the actual contract — no overlaid binding leaks in.
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list nil nil nil nil nil nil nil nil))
        (with-temp-buffer
          (org-mode)
          ;; Drawer does not contain `GPTEL_PARENT_SESSION_ID'.
          (insert ":PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n")
          (gptel-chat--apply-drawer-overrides)
          ;; The overlay installs no buffer-local for any absent key.
          (dolist (key '(gptel--system-message gptel-backend gptel-model
                         gptel-temperature gptel-max-tokens
                         gptel--num-messages-to-send gptel-tools
                         jf/gptel--parent-session-id))
            (expect (local-variable-p key) :to-be nil))))

      (it "discards the preset field from the upstream tuple"
        ;; `gptel--preset' is not overlayed — the caller (apply-
        ;; declared-preset) owns that assignment.  Even if upstream
        ;; returns a preset symbol, this overlay does not write
        ;; `gptel--preset'.
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list 'coding nil nil nil nil nil nil nil))
        (with-temp-buffer
          (org-mode)
          (gptel-chat--apply-drawer-overrides)
          ;; `gptel--preset' was never assigned locally.
          (expect (local-variable-p 'gptel--preset) :to-be nil)))

      (it "installs jf/gptel--parent-session-id from GPTEL_PARENT_SESSION_ID"
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list nil nil nil nil nil nil nil nil))
        (with-temp-buffer
          (org-mode)
          (insert ":PROPERTIES:\n"
                  ":GPTEL_PARENT_SESSION_ID: parent-abc-20260424000000\n"
                  ":END:\n")
          (gptel-chat--apply-drawer-overrides)
          (expect (local-variable-p 'jf/gptel--parent-session-id) :to-be t)
          (expect jf/gptel--parent-session-id
                  :to-equal "parent-abc-20260424000000")))

      (it "is a no-op when GPTEL_PARENT_SESSION_ID is absent"
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list nil nil nil nil nil nil nil nil))
        (with-temp-buffer
          (org-mode)
          (insert ":PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n")
          (gptel-chat--apply-drawer-overrides)
          (expect (local-variable-p 'jf/gptel--parent-session-id)
                  :to-be nil)))

      (it "is a no-op when GPTEL_PARENT_SESSION_ID is empty"
        ;; A drawer key with no value (common in Org property drawers
        ;; that were authored by hand) must not install an empty
        ;; parent-session-id.
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list nil nil nil nil nil nil nil nil))
        (with-temp-buffer
          (org-mode)
          (insert ":PROPERTIES:\n"
                  ":GPTEL_PARENT_SESSION_ID:\n"
                  ":END:\n")
          (gptel-chat--apply-drawer-overrides)
          (expect (local-variable-p 'jf/gptel--parent-session-id)
                  :to-be nil)))

      (it "every overlaid value is buffer-local, not global"
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list nil "sys-msg" 'backend-obj 'model-sym 0.5 4096 3
                      '(tool-a)))
        (let ((global-tools (default-value 'gptel-tools))
              (global-model (default-value 'gptel-model))
              (global-backend (default-value 'gptel-backend))
              (global-sys (default-value 'gptel--system-message)))
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n"
                    ":GPTEL_PARENT_SESSION_ID: pid\n"
                    ":END:\n")
            (gptel-chat--apply-drawer-overrides)
            (expect (local-variable-p 'gptel-tools) :to-be t)
            (expect (local-variable-p 'gptel-model) :to-be t)
            (expect (local-variable-p 'gptel-backend) :to-be t)
            (expect (local-variable-p 'gptel--system-message) :to-be t)
            (expect (local-variable-p 'jf/gptel--parent-session-id) :to-be t))
          ;; Defaults unchanged after the buffer closes.
          (expect (default-value 'gptel-tools) :to-equal global-tools)
          (expect (default-value 'gptel-model) :to-equal global-model)
          (expect (default-value 'gptel-backend) :to-equal global-backend)
          (expect (default-value 'gptel--system-message) :to-equal global-sys))))


    ;; -------------------------------------------------------------------
    ;; Overlay firing from `gptel-chat--apply-declared-preset'.
    ;;
    ;; Covers the integration points in spec §"Preset system
    ;; integration" (MODIFIED): after preset apply AND in the no-
    ;; preset branch.

    (describe "apply-declared-preset invokes the overlay"

      (before-each
        (spy-on 'gptel-get-preset :and-call-fake
                (lambda (name) (memq name '(coding research))))
        (spy-on 'gptel--apply-preset :and-return-value nil)
        (spy-on 'gptel-mode :and-return-value nil))

      (it "overlays GPTEL_TOOLS so the drawer wins over the applied preset"
        ;; Drawer wins over preset for every key it carries (Decision 3
        ;; / register/invariant/drawer-overlay-wins-over-preset).  The
        ;; upstream tuple's `tools' field is non-nil → buffer-local
        ;; `gptel-tools' takes the drawer's value, regardless of what
        ;; the preset's `:tools' resolved to.
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list 'coding nil nil nil nil nil nil
                      '(tool-a tool-b tool-c)))
        (with-temp-buffer
          (insert gptel-chat-preset-test--drawer-coding)
          (org-mode)
          (gptel-chat--apply-declared-preset)
          (expect 'gptel--apply-preset :to-have-been-called)
          (expect (local-variable-p 'gptel-tools) :to-be t)
          (expect gptel-tools :to-equal '(tool-a tool-b tool-c))))

      ;; Decision 3 / register/invariant/drawer-overlay-wins-over-preset:
      ;; every drawer-present key wins over the preset on apply, with
      ;; :GPTEL_SYSTEM: as the asymmetric exception (writer never emits;
      ;; preset's :system survives when drawer omits the key).

      (it "drawer GPTEL_MODEL wins over preset model"
        ;; Upstream tuple's `model' field is non-nil — overlay sets
        ;; buffer-local `gptel-model' to that value, regardless of
        ;; what the preset's `:model' resolved to.  The preset's
        ;; model is whatever `gptel--apply-preset' would install
        ;; (we spy on it as a no-op so the only setter that runs is
        ;; the overlay).
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list 'coding nil nil 'drawer-model-symbol nil nil nil nil))
        (with-temp-buffer
          (insert gptel-chat-preset-test--drawer-coding)
          (org-mode)
          (gptel-chat--apply-declared-preset)
          (expect 'gptel--apply-preset :to-have-been-called)
          (expect (local-variable-p 'gptel-model) :to-be t)
          (expect gptel-model :to-equal 'drawer-model-symbol)))

      (it "preset :system survives when drawer omits :GPTEL_SYSTEM:"
        ;; The chat-mode save path never writes :GPTEL_SYSTEM:
        ;; (Decision 2 / register/invariant/drawer-system-key-write-
        ;; exclusion), so a typical drawer omits the key and the
        ;; upstream tuple's `system' field is nil.  In that branch
        ;; the overlay must NOT touch `gptel--system-message' — the
        ;; preset's :system (already installed by the caller via the
        ;; buffer-local setter) must survive intact.
        ;;
        ;; Discriminator: the default no-op spy on `gptel--apply-preset'
        ;; never installs anything, which makes "binding absent after
        ;; overlay" indistinguishable from "overlay killed the
        ;; binding."  Override it here with a stub that *simulates*
        ;; what the real preset setter does: install
        ;; `gptel--system-message' buffer-locally to a sentinel value.
        ;; If a future refactor adds
        ;; `(kill-local-variable 'gptel--system-message)' to the
        ;; overlay path, this assertion will fail — that is the
        ;; contract guarded by
        ;; register/invariant/drawer-overlay-wins-over-preset.
        (spy-on 'gptel--apply-preset :and-call-fake
                (lambda (_preset setter)
                  (funcall setter 'gptel--system-message "preset-system")))
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list 'coding nil nil nil nil nil nil nil))
        (with-temp-buffer
          (insert gptel-chat-preset-test--drawer-coding)
          (org-mode)
          (gptel-chat--apply-declared-preset)
          (expect 'gptel--apply-preset :to-have-been-called)
          ;; The preset setter installed it buffer-locally; the
          ;; overlay must leave it alone (no kill-local-variable, no
          ;; reset to default).
          (expect (local-variable-p 'gptel--system-message) :to-be t)
          (expect gptel--system-message :to-equal "preset-system")))

      (it "drawer-authored :GPTEL_SYSTEM: still wins on read (back-compat)"
        ;; Asymmetric contract: writer never emits :GPTEL_SYSTEM:, but
        ;; the read-side overlay still respects a manually authored
        ;; entry.  This protects users who customised a per-session
        ;; system prompt directly in the drawer.
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list 'coding "drawer-system" nil nil nil nil nil nil))
        (with-temp-buffer
          (insert gptel-chat-preset-test--drawer-coding)
          (org-mode)
          (gptel-chat--apply-declared-preset)
          (expect 'gptel--apply-preset :to-have-been-called)
          (expect (local-variable-p 'gptel--system-message) :to-be t)
          (expect gptel--system-message :to-equal "drawer-system")))

      (it "overlays GPTEL_PARENT_SESSION_ID even when no preset is declared"
        ;; Upstream tuple is all-nil (no `GPTEL_PRESET' → resolver
        ;; returns nil, preset branch is skipped).  The unconditional
        ;; overlay call at the end of `apply-declared-preset' must
        ;; still fire and install the parent-session-id.
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list nil nil nil nil nil nil nil nil))
        (with-temp-buffer
          (org-mode)
          ;; Drawer with NO `GPTEL_PRESET' — resolver returns nil.
          (insert ":PROPERTIES:\n"
                  ":GPTEL_PARENT_SESSION_ID: parent-xyz\n"
                  ":END:\n")
          (gptel-chat--apply-declared-preset)
          (expect 'gptel--apply-preset :not :to-have-been-called)
          (expect (local-variable-p 'jf/gptel--parent-session-id) :to-be t)
          (expect jf/gptel--parent-session-id :to-equal "parent-xyz")))

      (it "does not overlay anything when the buffer has no drawer"
        ;; No drawer → no `GPTEL_PRESET' → preset branch skipped.
        ;; Upstream tuple is all-nil → overlay is a no-op for every
        ;; upstream key.  `GPTEL_PARENT_SESSION_ID' is absent too.
        ;;
        ;; Asserts `local-variable-p' per candidate key rather than
        ;; spying `make-local-variable' — see the "is a no-op for each
        ;; absent field" spec above for why the global spy is unsound
        ;; here (org-mode / org-element-cache init calls it ~83 times).
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list nil nil nil nil nil nil nil nil))
        (with-temp-buffer
          (insert gptel-chat-preset-test--no-preset)
          (org-mode)
          (gptel-chat--apply-declared-preset)
          (expect 'gptel--apply-preset :not :to-have-been-called)
          (dolist (key '(gptel--system-message gptel-backend gptel-model
                         gptel-temperature gptel-max-tokens
                         gptel--num-messages-to-send gptel-tools
                         jf/gptel--parent-session-id))
            (expect (local-variable-p key) :to-be nil))))

      (it "does not overlay absent upstream keys even when preset applies"
        ;; Drawer with `GPTEL_PRESET: coding' only — overlay tuple has
        ;; only the preset set; every other field is nil.  The overlay
        ;; MUST NOT install a buffer-local for absent keys.
        ;;
        ;; The `gptel--apply-preset' spy is a no-op (it does not run the
        ;; setter lambda), so any buffer-local that appears here came
        ;; from the overlay — and the overlay's tuple is all-nil, so
        ;; none should.  Asserts `local-variable-p' per key rather than
        ;; spying `make-local-variable' (see the no-op spec above).
        (spy-on 'gptel-org--entry-properties :and-return-value
                (list 'coding nil nil nil nil nil nil nil))
        (with-temp-buffer
          (insert gptel-chat-preset-test--drawer-coding)
          (org-mode)
          (gptel-chat--apply-declared-preset)
          (expect 'gptel--apply-preset :to-have-been-called)
          (dolist (key '(gptel--system-message gptel-backend gptel-model
                         gptel-temperature gptel-max-tokens
                         gptel--num-messages-to-send gptel-tools
                         jf/gptel--parent-session-id))
            (expect (local-variable-p key) :to-be nil)))))


    ;; -------------------------------------------------------------------
    ;; Integration with the real upstream helper.
    ;;
    ;; The unit specs above spy on `gptel-org--entry-properties' to
    ;; drive the overlay from a controlled tuple.  That isolates the
    ;; overlay's decision logic from upstream's parser, but it also
    ;; means a future upstream-signature change (e.g. adding a new
    ;; field to the tuple) slips past the mock untouched.  This
    ;; integration spec fills that gap: it exercises the real parser
    ;; against a real `:PROPERTIES:' drawer and asserts the overlay
    ;; installed the expected buffer-local binding end-to-end.
    ;;
    ;; We choose `GPTEL_TEMPERATURE' (and `GPTEL_SYSTEM' where
    ;; applicable) because those keys round-trip through
    ;; `gptel-org--entry-properties' without requiring
    ;; `gptel--known-backends' population or preset registration —
    ;; keeping the test self-contained.  `GPTEL_BACKEND',
    ;; `GPTEL_MODEL', and `GPTEL_TOOLS' are intentionally avoided
    ;; here; they are covered by the unit specs above with controlled
    ;; tuples.

    (describe "integration with real gptel-org helper"

      (before-each
        (unless (fboundp 'gptel-org--entry-properties)
          (signal 'buttercup-pending nil)))

      (it "overlays GPTEL_TEMPERATURE from a real drawer end-to-end"
        ;; Real upstream parser, real org-mode buffer, no spies on the
        ;; boundary.  A future change to the upstream tuple shape —
        ;; such as adding a new leading field — fails this spec via a
        ;; `pcase-let' destructuring mismatch, which is precisely the
        ;; signal the overlay's unit specs cannot provide.
        (with-temp-buffer
          (insert ":PROPERTIES:\n"
                  ":GPTEL_PRESET: coding\n"
                  ":GPTEL_TEMPERATURE: 0.5\n"
                  ":END:\n"
                  "\n"
                  "#+begin_user\n"
                  "\n"
                  "#+end_user\n")
          (org-mode)
          (gptel-chat--apply-drawer-overrides)
          (expect (local-variable-p 'gptel-temperature) :to-be t)
          (expect gptel-temperature :to-equal 0.5)))))


  ;; -----------------------------------------------------------------------
  ;; 8. System Prompt heading restore
  ;; (task `make-system-prompt-heading-authoritative', design.md
  ;; §Addendum Finding B / Decision B;
  ;; register/invariant/system-prompt-heading-authoritative).
  ;;
  ;; The `* System Prompt' heading body is the authoritative system
  ;; prompt.  Restore precedence, highest first:
  ;;   1. `* System Prompt' heading body, when non-blank
  ;;   2. legacy :GPTEL_SYSTEM: drawer entry, when present
  ;;   3. preset :system
  ;; A blank heading body falls through — an empty heading never
  ;; silently wipes the prompt.
  ;;
  ;; `gptel-chat--system-prompt-heading-body' is the pure reader;
  ;; `gptel-chat--apply-system-prompt-heading' is the restore step
  ;; (runs last from `gptel-chat--apply-declared-preset', after the
  ;; drawer overlay, so it supersedes both the preset and a legacy
  ;; drawer entry).

  (describe "system prompt heading restore"

    (describe "gptel-chat--system-prompt-heading-body (pure reader)"

      (it "returns the heading body text, trimmed, when non-blank"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:END:\n"
                  "\n* System Prompt\n"
                  ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  "First line.\nSecond line.\n"
                  "\n* Chat\n#+begin_user\n\n#+end_user\n")
          (org-mode)
          (expect (gptel-chat--system-prompt-heading-body)
                  :to-equal "First line.\nSecond line.")))

      (it "returns nil when there is no `* System Prompt' heading"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n"
                  "\n#+begin_user\n\n#+end_user\n")
          (org-mode)
          (expect (gptel-chat--system-prompt-heading-body) :to-be nil)))

      (it "returns nil for a whitespace-only heading body"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:END:\n"
                  "\n* System Prompt\n"
                  ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  "   \n\t\n"
                  "\n* Chat\n#+begin_user\n\n#+end_user\n")
          (org-mode)
          (expect (gptel-chat--system-prompt-heading-body) :to-be nil)))

      (it "returns nil for an empty heading body"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:END:\n"
                  "\n* System Prompt\n"
                  ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  "\n* Chat\n#+begin_user\n\n#+end_user\n")
          (org-mode)
          (expect (gptel-chat--system-prompt-heading-body) :to-be nil))))

    (describe "gptel-chat--apply-system-prompt-heading (restore step)"

      (it "installs the heading body buffer-locally as gptel--system-message"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:END:\n"
                  "\n* System Prompt\n"
                  ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  "Authored prompt body.\n"
                  "\n* Chat\n#+begin_user\n\n#+end_user\n")
          (org-mode)
          (gptel-chat--apply-system-prompt-heading)
          (expect (local-variable-p 'gptel--system-message) :to-be t)
          (expect gptel--system-message :to-equal "Authored prompt body.")))

      (it "is a no-op when the heading is absent (preset/drawer value stands)"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n"
                  "\n#+begin_user\n\n#+end_user\n")
          (org-mode)
          ;; Simulate a value installed by an earlier restore step.
          (setq-local gptel--system-message "preset-installed")
          (gptel-chat--apply-system-prompt-heading)
          (expect gptel--system-message :to-equal "preset-installed")))

      (it "is a no-op for a blank heading body (never wipes the prompt)"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:END:\n"
                  "\n* System Prompt\n"
                  ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  "  \n"
                  "\n* Chat\n#+begin_user\n\n#+end_user\n")
          (org-mode)
          (setq-local gptel--system-message "preset-installed")
          (gptel-chat--apply-system-prompt-heading)
          (expect gptel--system-message :to-equal "preset-installed"))))

    (describe "restore precedence end-to-end via gptel-chat-mode"

      (before-each
        (spy-on 'gptel-get-preset :and-call-fake
                (lambda (name) (memq name '(coding research))))
        ;; Simulate the real preset setter: install :system buffer-
        ;; locally so the heading read has something to supersede.
        (spy-on 'gptel--apply-preset :and-call-fake
                (lambda (_preset setter)
                  (when setter
                    (funcall setter 'gptel--system-message
                             "preset-system-text"))))
        (spy-on 'gptel-mode :and-return-value nil))

      (it "heading body wins over the preset :system on activation"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n"
                  "\n* System Prompt\n"
                  ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  "Heading body text.\n"
                  "\n* Chat\n#+begin_user\n\n#+end_user\n")
          (gptel-chat-mode)
          (expect gptel--system-message :to-equal "Heading body text.")))

      (it "heading body wins over a legacy :GPTEL_SYSTEM: drawer entry"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:GPTEL_PRESET: coding\n"
                  ":GPTEL_SYSTEM: Legacy drawer prompt.\n:END:\n"
                  "\n* System Prompt\n"
                  ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  "Heading body text.\n"
                  "\n* Chat\n#+begin_user\n\n#+end_user\n")
          (gptel-chat-mode)
          (expect gptel--system-message :to-equal "Heading body text.")))

      (it "falls back to the preset :system when no heading exists (old session)"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n"
                  "\n#+begin_user\n\n#+end_user\n")
          (gptel-chat-mode)
          (expect gptel--system-message :to-equal "preset-system-text")))

      (it "falls back to the preset :system when the heading body is blank"
        (with-temp-buffer
          (insert ":PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n"
                  "\n* System Prompt\n"
                  ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  "\n* Chat\n#+begin_user\n\n#+end_user\n")
          (gptel-chat-mode)
          (expect gptel--system-message :to-equal "preset-system-text")))))

  )

(provide 'gptel-chat-preset-wiring-spec)

;;; preset-wiring-spec.el ends here
