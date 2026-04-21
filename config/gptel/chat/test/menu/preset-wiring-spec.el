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

  )

(provide 'gptel-chat-preset-wiring-spec)

;;; preset-wiring-spec.el ends here
