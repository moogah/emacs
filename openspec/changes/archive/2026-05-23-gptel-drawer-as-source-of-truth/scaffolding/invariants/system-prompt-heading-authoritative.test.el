;;; system-prompt-heading-authoritative.test.el --- scaffolding -*- lexical-binding: t; -*-
;; scaffolding-of: register/invariant/system-prompt-heading-authoritative
;; generated-at: 2026-05-22T19:19:24Z
;; license: implementor-may-revise

;;; Commentary:
;;
;; Register-pinned buttercup contract test for register/invariant/
;; system-prompt-heading-authoritative.  Speculated by cycle-7
;; forward-mode against design.md §Addendum (Finding B, Decision B);
;; satisfied by task make-system-prompt-heading-authoritative.
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
;;     serialised back into the heading body — and never as a
;;     :GPTEL_SYSTEM: drawer line.
;;   - create -> restore -> save -> re-restore is idempotent.
;;
;; The contract is exercised against the real `gptel-chat-mode'
;; restore path (config/gptel/chat/mode.org wires the major mode;
;; config/gptel/chat/menu.org delivers `gptel-chat--apply-system-
;; prompt-heading' and `gptel-chat--write-system-prompt-heading') and
;; against real registered presets via `gptel-make-preset' — the same
;; fixture pattern the co-located regression specs in
;; config/gptel/chat/test/menu/ use.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; This scaffold lives at openspec/changes/<change>/scaffolding/
;; invariants/; the source tree's config/gptel/chat/ holds `mode.el'
;; and `menu.el', config/gptel/sessions/ holds `commands.el' (the
;; shared `jf/gptel--session-headings-block' heading helper), and
;; config/gptel/scope/ holds `gptel-scope-profiles.el' (the snapshot
;; registry the save path consumes).
(let* ((scaffold-dir (file-name-directory
                      (or load-file-name buffer-file-name)))
       (repo-root (expand-file-name "../../../../../" scaffold-dir)))
  (dolist (rel '("config/gptel/chat/"
                 "config/gptel/sessions/"
                 "config/gptel/scope/"))
    (let ((dir (expand-file-name rel repo-root)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(require 'gptel)
(require 'gptel-chat-mode)
(require 'gptel-chat-menu)
(require 'gptel-org nil t)
;; The save path materialises the `* System Prompt' / `* Chat'
;; headings via the shared helper in `gptel-session-commands' for
;; pre-Addendum sessions.  Load it so the materialise path is live.
(require 'gptel-session-commands nil t)


;;; Helpers ------------------------------------------------------------------

(defvar sysprompt-auth-test--counter 0
  "Monotonic counter for unique per-spec preset names.")

(defun sysprompt-auth-test--fresh-preset (&rest keys)
  "Register and return a uniquely named preset carrying KEYS.
KEYS is a plist passed verbatim to `gptel-make-preset'."
  (let ((name (intern (format "sysprompt-auth-test-preset-%d"
                              (cl-incf sysprompt-auth-test--counter)))))
    (apply #'gptel-make-preset name keys)
    name))

(defun sysprompt-auth-test--unregister (name)
  "Remove preset NAME from `gptel--known-presets'."
  (setq gptel--known-presets
        (assq-delete-all name gptel--known-presets)))

(defun sysprompt-auth-test--session (preset-name &optional system-body
                                                 drawer-system)
  "Return session.org buffer text for PRESET-NAME.

When SYSTEM-BODY is non-nil it is the `* System Prompt' heading body.
When DRAWER-SYSTEM is non-nil a legacy `:GPTEL_SYSTEM:' drawer line
is included.  Always carries a `* Chat' heading with an empty user
block so the layout is canonical."
  (concat ":PROPERTIES:\n"
          (format ":GPTEL_PRESET: %s\n" preset-name)
          (if drawer-system
              (format ":GPTEL_SYSTEM: %s\n" drawer-system)
            "")
          ":END:\n"
          "\n"
          "* System Prompt\n"
          ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
          (if system-body (concat system-body "\n") "")
          "\n"
          "* Chat\n"
          "#+begin_user\n\n#+end_user\n"))

(defun sysprompt-auth-test--legacy-session (preset-name &optional drawer-system)
  "Return a pre-Addendum session.org text — drawer + turn blocks, no headings.
PRESET-NAME seeds `:GPTEL_PRESET:'; DRAWER-SYSTEM, when non-nil, adds
a legacy `:GPTEL_SYSTEM:' drawer line."
  (concat ":PROPERTIES:\n"
          (format ":GPTEL_PRESET: %s\n" preset-name)
          (if drawer-system
              (format ":GPTEL_SYSTEM: %s\n" drawer-system)
            "")
          ":END:\n"
          "\n"
          "#+begin_user\nHello.\n#+end_user\n"))


;;; Specs --------------------------------------------------------------------

(describe "register/invariant/system-prompt-heading-authoritative"

  (describe "restore precedence"

    (it "uses the `* System Prompt' heading body when it is non-blank"
      (let ((preset (sysprompt-auth-test--fresh-preset
                     :model 'sysprompt-model
                     :system "Preset system text.")))
        (unwind-protect
            (with-temp-buffer
              (insert (sysprompt-auth-test--session
                       preset "Heading body system text."))
              (gptel-chat-mode)
              (expect (local-variable-p 'gptel--system-message) :to-be t)
              (expect gptel--system-message
                      :to-equal "Heading body system text."))
          (sysprompt-auth-test--unregister preset))))

    (it "prefers the heading body over a legacy :GPTEL_SYSTEM: drawer entry"
      (let ((preset (sysprompt-auth-test--fresh-preset
                     :model 'sysprompt-model
                     :system "Preset system text.")))
        (unwind-protect
            (with-temp-buffer
              (insert (sysprompt-auth-test--session
                       preset "Heading body wins." "Legacy drawer prompt."))
              (gptel-chat-mode)
              (expect gptel--system-message :to-equal "Heading body wins."))
          (sysprompt-auth-test--unregister preset))))

    (it "prefers a legacy :GPTEL_SYSTEM: drawer entry over the preset :system when the heading body is absent"
      (let ((preset (sysprompt-auth-test--fresh-preset
                     :model 'sysprompt-model
                     :system "Preset system text.")))
        (unwind-protect
            (with-temp-buffer
              ;; Pre-Addendum layout: drawer carries :GPTEL_SYSTEM:,
              ;; there is no `* System Prompt' heading at all.
              (insert (sysprompt-auth-test--legacy-session
                       preset "Legacy drawer prompt."))
              (gptel-chat-mode)
              (expect gptel--system-message
                      :to-equal "Legacy drawer prompt."))
          (sysprompt-auth-test--unregister preset))))

    (it "falls back to the preset :system when no heading body and no drawer entry exist"
      (let ((preset (sysprompt-auth-test--fresh-preset
                     :model 'sysprompt-model
                     :system "Preset system text.")))
        (unwind-protect
            (with-temp-buffer
              (insert (sysprompt-auth-test--legacy-session preset))
              (gptel-chat-mode)
              (expect gptel--system-message
                      :to-equal "Preset system text."))
          (sysprompt-auth-test--unregister preset)))))

  (describe "blank-body fallback"

    (it "treats a whitespace-only heading body as not authored and falls through to the preset"
      (let ((preset (sysprompt-auth-test--fresh-preset
                     :model 'sysprompt-model
                     :system "Preset system text.")))
        (unwind-protect
            (with-temp-buffer
              ;; `* System Prompt' heading present, body is only
              ;; whitespace — must NOT override the preset.
              (insert (sysprompt-auth-test--session preset "  \n\t"))
              (gptel-chat-mode)
              (expect gptel--system-message
                      :to-equal "Preset system text."))
          (sysprompt-auth-test--unregister preset))))

    (it "never silently wipes the prompt when the heading body is empty"
      (let ((preset (sysprompt-auth-test--fresh-preset
                     :model 'sysprompt-model
                     :system "Preset system text.")))
        (unwind-protect
            (with-temp-buffer
              ;; Heading present with a completely empty body.
              (insert (sysprompt-auth-test--session preset nil))
              (gptel-chat-mode)
              ;; The prompt is still the preset's — never nil/empty.
              (expect gptel--system-message
                      :to-equal "Preset system text.")
              (expect gptel--system-message :not :to-equal "")
              (expect gptel--system-message :not :to-be nil))
          (sysprompt-auth-test--unregister preset)))))

  (describe "before-save serialisation"

    (it "writes the current buffer-local `gptel--system-message' back into the `* System Prompt' heading body on before-save-hook"
      (with-temp-buffer
        (insert (concat ":PROPERTIES:\n:END:\n"
                        "\n* System Prompt\n"
                        ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                        "Old body.\n"
                        "\n* Chat\n#+begin_user\n\n#+end_user\n"))
        (gptel-chat-mode)
        (setq-local gptel--system-message "Newly authored system prompt.")
        (gptel-chat--save-state)
        ;; The heading body now reflects the buffer-local value.
        (expect (gptel-chat--system-prompt-heading-body)
                :to-equal "Newly authored system prompt.")
        ;; And the heading is still a singleton.
        (goto-char (point-min))
        (expect (how-many "^\\* System Prompt[ \t]*$") :to-equal 1)
        (expect (how-many "^\\* Chat[ \t]*$") :to-equal 1)))

    (it "never emits a :GPTEL_SYSTEM: drawer line (composes with drawer-system-key-write-exclusion)"
      (with-temp-buffer
        (insert (concat ":PROPERTIES:\n:END:\n"
                        "\n* System Prompt\n"
                        ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                        "Body.\n"
                        "\n* Chat\n#+begin_user\n\n#+end_user\n"))
        (gptel-chat-mode)
        (setq-local gptel--system-message
                    "Multi-line prompt\nwith `backticks` and *asterisks*.")
        (gptel-chat--save-state)
        ;; The drawer write-exclusion still holds — the system prompt
        ;; lives in the heading body, never as a drawer property.
        (expect (org-entry-get (point-min) "GPTEL_SYSTEM") :to-be nil)
        (goto-char (point-min))
        (expect (search-forward ":GPTEL_SYSTEM:" nil t) :to-be nil)
        ;; The heading body did get the value.
        (expect (gptel-chat--system-prompt-heading-body)
                :to-equal
                "Multi-line prompt\nwith `backticks` and *asterisks*."))))

  (describe "round-trip stability"

    (it "is idempotent across create -> restore -> save -> re-restore (a no-change save produces no heading-body diff)"
      (assume (fboundp 'jf/gptel--session-headings-block)
              "gptel-session-commands (heading helper) not loaded")
      (let ((preset (sysprompt-auth-test--fresh-preset
                     :model 'sysprompt-model
                     :system "Round-trip system prompt.")))
        (unwind-protect
            ;; Create: a fresh session.org with the preset-seeded body.
            (let ((created
                   (concat ":PROPERTIES:\n"
                           (format ":GPTEL_PRESET: %s\n" preset)
                           ":END:\n"
                           "\n"
                           (jf/gptel--session-headings-block
                            "Round-trip system prompt."
                            "#+begin_user\n\n#+end_user\n"))))
              (with-temp-buffer
                (insert created)
                ;; Restore.
                (gptel-chat-mode)
                (expect gptel--system-message
                        :to-equal "Round-trip system prompt.")
                ;; Save (no change to the system message).
                (gptel-chat--save-state)
                (let ((after-first-save (buffer-string)))
                  ;; Re-restore from the saved buffer.
                  (gptel-chat-mode)
                  (expect gptel--system-message
                          :to-equal "Round-trip system prompt.")
                  ;; A second no-change save produces no diff.
                  (gptel-chat--save-state)
                  (expect (buffer-string) :to-equal after-first-save)
                  ;; Layout invariants survive the round trip.
                  (goto-char (point-min))
                  (expect (how-many "^\\* System Prompt[ \t]*$")
                          :to-equal 1)
                  (expect (how-many "^\\* Chat[ \t]*$") :to-equal 1)
                  (expect (org-entry-get (point-min) "GPTEL_SYSTEM")
                          :to-be nil))))
          (sysprompt-auth-test--unregister preset))))))

;;; system-prompt-heading-authoritative.test.el ends here
