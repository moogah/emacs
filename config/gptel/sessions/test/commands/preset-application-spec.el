;;; preset-application-spec.el --- Drawer-driven preset application on content-addressed activation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verify drawer-driven preset application for content-addressed
;; session buffers (activated via `magic-mode-alist' + the
;; `gptel-chat-mode-hook' binder `jf/gptel--bind-session-buffer'):
;;
;; 1. A `:GPTEL_PRESET:' in the session.org `:PROPERTIES:' drawer drives
;;    the call to `gptel--apply-preset' via the chat-mode hook
;;    (`gptel-chat--apply-declared-preset', installed on
;;    `gptel-chat-mode-hook' in `config/gptel/chat/menu.org').
;; 2. Non-preset drawer deltas (e.g. `GPTEL_TOOLS') are overlaid
;;    buffer-locally after preset application.
;; 3. `GPTEL_PARENT_SESSION_ID' in the drawer populates the buffer-local
;;    `jf/gptel--parent-session-id'.
;; 4. `gptel-mode' (minor mode) is NEVER enabled — session buffers run
;;    `gptel-chat-mode' exclusively (Decision 16).
;; 5. Auto-init does NOT read `metadata.yml' — no `insert-file-contents'
;;    call targets any path matching `metadata\\.yml$'.  The drawer in
;;    `session.org' is the single source of truth (design.md §Decisions
;;    5, 6, 9).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-commands)

;; Ensure the real `gptel-chat-mode' and `gptel-chat--apply-declared-preset'
;; hook are loaded so the real-mode integration spec exercises the full
;; mode-activation path (including `gptel-chat-mode-hook').
(require 'gptel-chat-mode)
(require 'gptel-chat-menu)
(require 'gptel)

(defvar jf-gptel-preset-app-test--registry-keys nil
  "Registry keys to clean up after each example.")

(defun jf-gptel-preset-app-test--register-cleanup (session-id branch-name)
  (push (jf/gptel--registry-key session-id branch-name)
        jf-gptel-preset-app-test--registry-keys))

(defun jf-gptel-test--write-session-with-drawer
    (branch-dir preset &optional parent-id extra-properties)
  "Write a `session.org' file into BRANCH-DIR with a PROPERTIES drawer.

PRESET is a symbol that will be written as `:GPTEL_PRESET:'.
PARENT-ID, when non-nil, is written as `:GPTEL_PARENT_SESSION_ID:'.
EXTRA-PROPERTIES is an optional alist of (KEY . VALUE) additional
drawer entries (e.g. ((\"GPTEL_TOOLS\" . \"(foo bar)\"))).

Used by both legacy preset-only scenarios and the new chat-mode
snapshot scenarios from gptel-drawer-as-source-of-truth.  For
snapshot scenarios, callers pass entries like
`((\"GPTEL_MODEL\" . \"drawer-model\") (\"GPTEL_TOOLS\" . \"toolA toolB\"))'
in EXTRA-PROPERTIES — the helper does not interpret them; they
become literal drawer lines.

Returns the absolute path of the created `session.org'."
  (make-directory branch-dir t)
  (let ((session-file (expand-file-name "session.org" branch-dir)))
    (with-temp-file session-file
      (insert ":PROPERTIES:\n")
      (insert (format ":GPTEL_PRESET: %s\n" preset))
      (when parent-id
        (insert (format ":GPTEL_PARENT_SESSION_ID: %s\n" parent-id)))
      (dolist (kv extra-properties)
        (insert (format ":%s: %s\n" (car kv) (cdr kv))))
      (insert ":END:\n"
              "\n"
              "#+begin_user\n"
              "hello\n"
              "#+end_user\n"))
    session-file))

(describe "Drawer-driven auto-init (metadata.yml is NOT consulted)"

  (after-each
    (dolist (key jf-gptel-preset-app-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-preset-app-test--registry-keys nil))

  (describe "real-mode integration: drawer preset drives gptel--apply-preset"

    ;; Exercises the FULL real path — a real `session.org' on disk with
    ;; a `:GPTEL_PRESET:' drawer, real content-addressed activation via
    ;; `magic-mode-alist' driving `gptel-chat-mode' (which fires
    ;; `gptel-chat-mode-hook' and thus `gptel-chat--apply-declared-preset'),
    ;; and real `find-file' triggering.  No `metadata.yml' is created.

    (let ((temp-root nil)
          (session-dir nil)
          (branch-dir nil)
          (session-file nil)
          (buf nil)
          (drawer-preset 'drawer-wins-preset))

      (before-each
        (setq temp-root (make-temp-file "gptel-drawer-preset-" t))
        (setq session-dir
              (expand-file-name "sess-drawer-20260421120000" temp-root))
        (setq branch-dir (expand-file-name "branches/main" session-dir))
        ;; Register a preset with a distinguishable temperature.
        (gptel-make-preset drawer-preset :temperature 0.77)
        ;; Write session.org with a drawer declaring the preset.
        ;; NO metadata.yml is created.
        (setq session-file
              (jf-gptel-test--write-session-with-drawer
               branch-dir drawer-preset)))

      (after-each
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (set-buffer-modified-p nil))
          (kill-buffer buf))
        (setq buf nil)
        (setq gptel--known-presets
              (assq-delete-all drawer-preset gptel--known-presets))
        (when (and temp-root (file-directory-p temp-root))
          (delete-directory temp-root t)))

      (it "applies the drawer-declared preset via the chat-mode hook"
        (setq buf (find-file-noselect session-file))
        (with-current-buffer buf
          (jf-gptel-preset-app-test--register-cleanup
           "sess-drawer-20260421120000" "main")
          ;; Chat-mode is active.
          (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
          ;; Drawer preset was applied — `gptel--preset' buffer-local
          ;; holds the drawer value.
          (expect gptel--preset :to-equal drawer-preset)
          ;; And the preset's keys landed as buffer-locals.
          (expect gptel-temperature :to-equal 0.77)))))

  (describe "drawer-overrides overlay fires during chat-mode activation"

    ;; Detailed overlay semantics (which drawer keys map to which
    ;; buffer-locals, no-op for absent fields, buffer-local scoping)
    ;; live in `config/gptel/chat/test/menu/preset-wiring-spec.el'.
    ;; Here we assert the integration point that matters for auto-
    ;; init: when the chat-mode hook runs (which is what auto-init
    ;; delegates drawer handling to), the overlay function
    ;; `gptel-chat--apply-drawer-overrides' is invoked.  That proves
    ;; auto-init does NOT need to do any drawer-handling work itself.

    (let ((temp-root nil)
          (branch-dir nil)
          (session-file nil)
          (buf nil)
          (overlay-called nil)
          (overlay-preset 'drawer-overlay-preset))

      (before-each
        (setq overlay-called 0)
        (setq temp-root (make-temp-file "gptel-drawer-overlay-" t))
        (setq branch-dir
              (expand-file-name
               "sess-overlay-20260421120000/branches/main" temp-root))
        (gptel-make-preset overlay-preset :temperature 0.41)
        (setq session-file
              (jf-gptel-test--write-session-with-drawer
               branch-dir overlay-preset)))

      (after-each
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (set-buffer-modified-p nil))
          (kill-buffer buf))
        (setq buf nil)
        (setq gptel--known-presets
              (assq-delete-all overlay-preset gptel--known-presets))
        (when (and temp-root (file-directory-p temp-root))
          (delete-directory temp-root t)))

      (it "invokes gptel-chat--apply-drawer-overrides during auto-init"
        (cl-letf* ((real-overlay
                    (symbol-function 'gptel-chat--apply-drawer-overrides))
                   ((symbol-function 'gptel-chat--apply-drawer-overrides)
                    (lambda (&rest args)
                      (cl-incf overlay-called)
                      (apply real-overlay args))))
          (setq buf (find-file-noselect session-file))
          (with-current-buffer buf
            (jf-gptel-preset-app-test--register-cleanup
             "sess-overlay-20260421120000" "main")
            (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
            ;; The overlay is called at least once from
            ;; `gptel-chat--apply-declared-preset' during mode
            ;; activation.  (It may run twice — once after preset
            ;; apply, once unconditionally — that's a safe no-op.)
            (expect overlay-called :to-be-greater-than 0))))))

  (describe "real-mode integration: GPTEL_PARENT_SESSION_ID from drawer"

    (let ((temp-root nil)
          (branch-dir nil)
          (session-file nil)
          (buf nil)
          (parent-preset 'drawer-parent-preset))

      (before-each
        (setq temp-root (make-temp-file "gptel-drawer-parent-" t))
        (setq branch-dir
              (expand-file-name
               "sess-child-20260421120000/branches/main" temp-root))
        (gptel-make-preset parent-preset :temperature 0.55)
        (setq session-file
              (jf-gptel-test--write-session-with-drawer
               branch-dir parent-preset "parent-sess-20260420000000")))

      (after-each
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (set-buffer-modified-p nil))
          (kill-buffer buf))
        (setq buf nil)
        (setq gptel--known-presets
              (assq-delete-all parent-preset gptel--known-presets))
        (when (and temp-root (file-directory-p temp-root))
          (delete-directory temp-root t)))

      (it "populates jf/gptel--parent-session-id from drawer GPTEL_PARENT_SESSION_ID"
        (setq buf (find-file-noselect session-file))
        (with-current-buffer buf
          (jf-gptel-preset-app-test--register-cleanup
           "sess-child-20260421120000" "main")
          (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
          (expect jf/gptel--parent-session-id
                  :to-equal "parent-sess-20260420000000")))))

  (describe "content-addressed binding does NOT read metadata.yml"

    ;; Activation/binding is content-addressed: the drawer is the
    ;; authoritative source.  The `gptel-chat-mode-hook' binder
    ;; (`jf/gptel--bind-session-buffer') must NEVER read `metadata.yml'.
    ;; We verify by spying on `insert-file-contents' during binding and
    ;; asserting no call targets any path matching `metadata\\.yml$'.

    (it "never calls insert-file-contents on a metadata.yml path"
      (let ((temp-root (make-temp-file "gptel-no-meta-" t))
            (buf nil)
            (metadata-reads nil)
            (original-insert-file-contents
             (symbol-function 'insert-file-contents)))
        (unwind-protect
            (let* ((branch-dir
                    (expand-file-name
                     "sess-no-meta-20260501000000/branches/main" temp-root))
                   (session-file
                    (jf-gptel-test--write-session-with-drawer
                     branch-dir 'default)))
              (make-directory branch-dir t)
              (setq buf (generate-new-buffer "session.org"))
              (with-current-buffer buf
                (setq buffer-file-name session-file)
                (funcall original-insert-file-contents session-file)
                (cl-letf (((symbol-function 'insert-file-contents)
                           (lambda (f &rest args)
                             (when (and (stringp f)
                                        (string-match-p "metadata\\.yml\\'" f))
                               (push f metadata-reads))
                             (apply original-insert-file-contents f args))))
                  (jf/gptel--bind-session-buffer))
                (jf-gptel-preset-app-test--register-cleanup
                 "sess-no-meta-20260501000000" "main")
                (expect metadata-reads :to-be nil)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t))))))

  (describe "gptel-mode (minor mode) is NEVER enabled"

    (it "does NOT call (gptel-mode 1) during content-addressed binding"
      (let ((temp-root (make-temp-file "gptel-no-gptelmode-" t))
            (buf nil)
            (gptel-mode-called nil)
            (original-insert-file-contents
             (symbol-function 'insert-file-contents)))
        (unwind-protect
            (let* ((branch-dir
                    (expand-file-name
                     "sess-no-gptelmode-20260501000000/branches/main" temp-root))
                   (session-file
                    (jf-gptel-test--write-session-with-drawer
                     branch-dir 'default)))
              (make-directory branch-dir t)
              (setq buf (generate-new-buffer "session.org"))
              (with-current-buffer buf
                (setq buffer-file-name session-file)
                (funcall original-insert-file-contents session-file)
                (cl-letf (((symbol-function 'gptel-mode)
                           (lambda (&optional _)
                             (setq gptel-mode-called t))))
                  (jf/gptel--bind-session-buffer))
                (jf-gptel-preset-app-test--register-cleanup
                 "sess-no-gptelmode-20260501000000" "main")
                (expect gptel-mode-called :to-be nil)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t)))))))

;;; --------------------------------------------------------------------------
;;; gptel-drawer-as-source-of-truth scenarios
;;;
;;; The describes below exercise Decision 1, 3, and 4 end-to-end:
;;;   - Fresh session.org carries the full preset snapshot drawer
;;;     (Decision 4 — renderer extension; cycle-5
;;;     wire-snapshot-into-session-creation).
;;;   - Drawer wins over preset for every key it carries on reopen
;;;     (Decision 3 — register/invariant/drawer-overlay-wins-over-
;;;     preset).
;;;   - Save round-trips drawer values, not preset values, so the
;;;     drawer remains the WYSIWYG source of truth across save cycles
;;;     (Decision 1 — full-snapshot writer; cycle-5
;;;     replace-chat-save-with-full-snapshot-writer).
;;; --------------------------------------------------------------------------

(describe "gptel-drawer-as-source-of-truth: full snapshot end-to-end"

  ;; Shared registry drain.  Each inner describe registers cleanup keys
  ;; via `jf-gptel-preset-app-test--register-cleanup' inside its `it'
  ;; bodies; without an `after-each' draining
  ;; `jf-gptel-preset-app-test--registry-keys' those entries leak into
  ;; `jf/gptel--session-registry' for the test process lifetime, which
  ;; can cause false hits in registry-lookup assertions in single-
  ;; process runs of the sessions suite.
  (after-each
    (dolist (key jf-gptel-preset-app-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-preset-app-test--registry-keys nil))

  (describe "fresh session.org carries the preset snapshot drawer"

    (let ((temp-root nil)
          (session-id "sess-snapshot-20260501120000")
          (session-dir nil)
          (snapshot-preset 'snapshot-test-preset))

      (before-each
        (setq temp-root (make-temp-file "gptel-snapshot-fresh-" t))
        (setq session-dir (expand-file-name session-id temp-root))
        (gptel-make-preset snapshot-preset
          :model 'snapshot-model
          :temperature 0.42
          :tools '(toolA toolB)))

      (after-each
        (setq gptel--known-presets
              (assq-delete-all snapshot-preset gptel--known-presets))
        (when (and temp-root (file-directory-p temp-root))
          (delete-directory temp-root t)))

      (it "writes :GPTEL_MODEL: and :GPTEL_TEMPERATURE: from the preset"
        (jf/gptel--create-session-core session-id session-dir snapshot-preset)
        (jf-gptel-preset-app-test--register-cleanup session-id "main")
        (let* ((session-file
                (expand-file-name "branches/main/session.org" session-dir))
               (content (with-temp-buffer
                          (insert-file-contents session-file)
                          (buffer-string))))
          (expect content :to-match ":GPTEL_PRESET: snapshot-test-preset\n")
          (expect content :to-match ":GPTEL_MODEL: snapshot-model\n")
          (expect content :to-match ":GPTEL_TEMPERATURE: 0.42\n")
          ;; Tools registers as a list; presence-match suffices here —
          ;; exact list formatting is implementation detail and the
          ;; scope-profile-applicator dedupe task covers it more
          ;; rigorously.
          (expect content :to-match ":GPTEL_TOOLS: ")
          (expect (string-match-p ":GPTEL_SYSTEM:" content) :to-be nil)
          (expect (string-match-p ":GPTEL_BOUNDS:" content) :to-be nil))))

    ;; The "preset :system seeds the document body" describes were
    ;; deleted in `revert-initial-session-body-and-delete-headings-block'
    ;; (replace-system-prompt-heading-with-sibling-file).  The
    ;; preset's `:system' is no longer woven into session.org — it is
    ;; materialised in a sibling `system-prompt.<ext>' file by a
    ;; subsequent task in the same change.  Focused tests for that
    ;; writer live in
    ;; `config/gptel/sessions/test/commands/sibling-system-prompt-file-spec.el'.
    )

  (describe "drawer wins over preset on reopen (Decision 3)"

    ;; Asymmetric scenario: preset declares one model; drawer declares
    ;; a different model on disk.  When the file is opened, chat-mode
    ;; activation runs gptel-chat--apply-declared-preset which (a)
    ;; calls gptel--apply-preset (installs the preset's :model) and
    ;; then (b) overlays the drawer (gptel-chat--apply-drawer-
    ;; overrides) which OVERRIDES the preset's :model with the
    ;; drawer's value.

    (let ((temp-root nil)
          (branch-dir nil)
          (session-file nil)
          (buf nil)
          (overlay-preset 'snapshot-overlay-preset))

      (before-each
        (setq temp-root (make-temp-file "gptel-snapshot-overlay-" t))
        (setq branch-dir
              (expand-file-name "sess-overlay-snap/branches/main" temp-root))
        (gptel-make-preset overlay-preset :model 'preset-model-symbol)
        (setq session-file
              (jf-gptel-test--write-session-with-drawer
               branch-dir overlay-preset
               nil
               '(("GPTEL_MODEL" . "drawer-model-symbol")))))

      (after-each
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (setq buf nil)
        (setq gptel--known-presets
              (assq-delete-all overlay-preset gptel--known-presets))
        (when (and temp-root (file-directory-p temp-root))
          (delete-directory temp-root t)))

      (it "drawer GPTEL_MODEL wins over preset model on chat-mode activation"
        (setq buf (find-file-noselect session-file))
        (with-current-buffer buf
          (jf-gptel-preset-app-test--register-cleanup
           "sess-overlay-snap" "main")
          (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
          (expect gptel--preset :to-equal overlay-preset)
          ;; The drawer's model overrode the preset's; buffer-local
          ;; gptel-model holds the drawer's value (read by upstream's
          ;; gptel-org--entry-properties as a symbol).
          (expect (local-variable-p 'gptel-model) :to-be t)
          (expect gptel-model :to-equal 'drawer-model-symbol)))))

  (describe "save round-trips drawer values (Decision 1)"

    ;; Once the drawer has won (per the previous describe), saving the
    ;; buffer must re-emit the drawer's values — not the preset's.
    ;; This is the WYSIWYG contract: open the file again and it still
    ;; shows what the buffer was doing.

    (let ((temp-root nil)
          (branch-dir nil)
          (session-file nil)
          (buf nil)
          (rt-preset 'snapshot-roundtrip-preset))

      (before-each
        (setq temp-root (make-temp-file "gptel-snapshot-roundtrip-" t))
        (setq branch-dir
              (expand-file-name "sess-rt-snap/branches/main" temp-root))
        (gptel-make-preset rt-preset :model 'preset-rt-model)
        (setq session-file
              (jf-gptel-test--write-session-with-drawer
               branch-dir rt-preset
               nil
               '(("GPTEL_MODEL" . "drawer-rt-model")))))

      (after-each
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (setq buf nil)
        (setq gptel--known-presets
              (assq-delete-all rt-preset gptel--known-presets))
        (when (and temp-root (file-directory-p temp-root))
          (delete-directory temp-root t)))

      (it "save after drawer-wins reopen re-emits the drawer model, not the preset model"
        (setq buf (find-file-noselect session-file))
        (with-current-buffer buf
          (jf-gptel-preset-app-test--register-cleanup
           "sess-rt-snap" "main")
          (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
          (expect gptel-model :to-equal 'drawer-rt-model)
          ;; Save and re-read.
          (save-buffer)
          (let ((after (with-temp-buffer
                         (insert-file-contents session-file)
                         (buffer-string))))
            ;; Drawer model survived the round-trip.
            (expect after :to-match ":GPTEL_MODEL: drawer-rt-model\n")
            ;; Preset model did not leak in.
            (expect (string-match-p ":GPTEL_MODEL: preset-rt-model"
                                    after)
                    :to-be nil)
            ;; :GPTEL_SYSTEM: still absent (writer never emits it).
            (expect (string-match-p ":GPTEL_SYSTEM:" after)
                    :to-be nil)))))))

(provide 'preset-application-spec)
;;; preset-application-spec.el ends here
