;;; preset-application-spec.el --- Preset application during auto-init -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verify preset application behavior in
;; `jf/gptel--auto-init-session-buffer':
;;
;; 1. The preset named in `metadata.yml' is applied via
;;    `gptel--apply-preset' with a buffer-local setter.
;; 2. `gptel-mode' is NOT called — session buffers run
;;    `gptel-chat-mode' exclusively (Decision 16).
;; 3. When both a property-drawer preset (simulated via a buffer-local
;;    `gptel--preset') AND a `metadata.yml'-declared preset are present,
;;    `metadata.yml' wins.  Sessions are authoritative for their own
;;    configuration (Decision 16, point 2).

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'yaml)

(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-metadata)
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

(describe "Preset application during auto-init"

  (after-each
    (dolist (key jf-gptel-preset-app-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-preset-app-test--registry-keys nil))

  (describe "metadata.yml preset path"

    ;; These are unit-level tests that focus on auto-init's OWN call
    ;; to `gptel--apply-preset' and its choice not to enable
    ;; `gptel-mode'.  They stub `gptel-chat-mode' to avoid pulling in
    ;; org-mode/yasnippet initialisation side-effects in the batch
    ;; test runner — but critically, they DO NOT exercise precedence.
    ;; The real-mode integration spec below is the authoritative
    ;; precedence check; it does NOT stub `gptel-chat-mode'.  See the
    ;; note on `auto-init-metadata-preset-precedence'.

    (it "calls gptel--apply-preset with buffer-local setter"
      (let ((buf (generate-new-buffer "session.org"))
            (apply-preset-args nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    "/sessions/sess-one/branches/main/session.org")
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"sess-one\"\npreset: \"executor\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (name setter)
                           (setq apply-preset-args (list name setter))
                           ;; Exercise the setter to confirm it produces
                           ;; buffer-local bindings (not globals).
                           (when setter
                             (funcall setter 'gptel-model "from-preset"))))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) nil))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-preset-app-test--register-cleanup "sess-one" "main")

                (expect apply-preset-args :to-be-truthy)
                (expect (car apply-preset-args) :to-equal 'executor)
                (expect (functionp (cadr apply-preset-args)) :to-be t)
                ;; The setter's effect is buffer-local.
                (expect (local-variable-p 'gptel-model) :to-be t)
                (expect gptel-model :to-equal "from-preset")))
          (kill-buffer buf))))

    (it "does NOT call (gptel-mode 1)"
      (let ((buf (generate-new-buffer "session.org"))
            (gptel-mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    "/sessions/sess-two/branches/main/session.org")
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"sess-two\"\npreset: \"executor\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (_name _setter) nil))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) nil))
                        ((symbol-function 'gptel-mode)
                         (lambda (&optional _)
                           (setq gptel-mode-called t)))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-preset-app-test--register-cleanup "sess-two" "main")
                (expect gptel-mode-called :to-be nil)))
          (kill-buffer buf)))))

  ;; NOTE: an earlier unit-level precedence test lived here but was
  ;; REMOVED by the `auto-init-metadata-preset-precedence' task.  That
  ;; test stubbed `gptel-chat-mode' as a no-op lambda
  ;; (`(lambda (&optional _) nil)'), which masked a real precedence bug
  ;; where `gptel-chat-mode-hook' re-applied the drawer preset AFTER
  ;; `metadata.yml', silently clobbering metadata-set buffer-locals.
  ;; Keeping that unit-level stub in place after swapping the
  ;; ordering would continue to give false confidence, because the
  ;; stub neutralises the exact hook whose behaviour the test was
  ;; meant to constrain.  The real-mode integration spec below
  ;; exercises the full path — real `gptel-chat-mode' activation, real
  ;; `gptel-chat-mode-hook', real `gptel--apply-preset', real
  ;; `find-file' — and is the only honest check of precedence.

  (describe "real-mode integration: metadata.yml preset beats drawer preset"

    ;; This spec exercises the FULL real path — a real `session.org' file
    ;; on disk, a real `metadata.yml' next to it, a real `gptel-chat-mode'
    ;; activation (which fires `gptel-chat-mode-hook' and thus
    ;; `gptel-chat--apply-declared-preset'), and real `find-file' /
    ;; `find-file-hook' triggering.
    ;;
    ;; Two presets are registered via the upstream `gptel-make-preset'
    ;; helper with disambiguating `:temperature' values:
    ;;
    ;;   metadata-wins-meta  → temperature 0.11 (from metadata.yml)
    ;;   metadata-wins-drawer → temperature 0.99 (from :GPTEL_PRESET:)
    ;;
    ;; After auto-init completes, the buffer-local `gptel-temperature'
    ;; MUST equal 0.11 (metadata preset wins over drawer preset).
    ;; `gptel--preset' MUST equal 'metadata-wins-meta.
    ;;
    ;; Earlier revisions of `preset-application-spec.el' stubbed
    ;; `gptel-chat-mode' as `(lambda (&optional _) nil)', which masked a
    ;; bug where the chat-mode hook re-applied the drawer preset AFTER
    ;; the metadata apply, silently clobbering `gptel-temperature' back
    ;; to 0.99.  Running the real mode is the only way to catch this.

    (let ((temp-root nil)
          (session-dir nil)
          (branch-dir nil)
          (session-file nil)
          (buf nil)
          (meta-preset 'metadata-wins-meta)
          (drawer-preset 'metadata-wins-drawer))

      (before-each
        (setq temp-root (make-temp-file "gptel-preset-precedence-" t))
        (setq session-dir
              (expand-file-name "sess-precedence-20260421120000" temp-root))
        (setq branch-dir (expand-file-name "branches/main" session-dir))
        (make-directory branch-dir t)
        ;; Register two distinct presets with distinguishable temperatures.
        (gptel-make-preset meta-preset :temperature 0.11)
        (gptel-make-preset drawer-preset :temperature 0.99)
        ;; Write metadata.yml declaring the "meta" preset.
        (with-temp-file (expand-file-name "metadata.yml" branch-dir)
          (insert (format "session_id: \"sess-precedence-20260421120000\"\n"))
          (insert (format "preset: \"%s\"\n" meta-preset)))
        ;; Write session.org with a :PROPERTIES: drawer naming the
        ;; "drawer" preset at point-min.
        (setq session-file (expand-file-name "session.org" branch-dir))
        (with-temp-file session-file
          (insert ":PROPERTIES:\n"
                  (format ":GPTEL_PRESET: %s\n" drawer-preset)
                  ":END:\n"
                  "\n"
                  "#+begin_user\n"
                  "hello\n"
                  "#+end_user\n")))

      (after-each
        ;; Kill the buffer (if any) so `find-file-hook' doesn't keep
        ;; running against stale state.
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (set-buffer-modified-p nil))
          (kill-buffer buf))
        (setq buf nil)
        ;; Unregister the presets we added so the global registry
        ;; stays clean for other tests.
        (setq gptel--known-presets
              (assq-delete-all meta-preset gptel--known-presets))
        (setq gptel--known-presets
              (assq-delete-all drawer-preset gptel--known-presets))
        ;; Clean up the temp tree.
        (when (and temp-root (file-directory-p temp-root))
          (delete-directory temp-root t)))

      (it "applies metadata.yml's preset (NOT the drawer preset) after find-file"
        ;; Open the session file via the real `find-file'. That runs:
        ;;   1. `set-auto-mode' (org-mode via auto-mode-alist),
        ;;   2. `hack-local-variables' + `hack-local-variables-hook',
        ;;   3. `find-file-hook' (which includes
        ;;      `jf/gptel--auto-init-session-buffer').
        ;; Auto-init activates `gptel-chat-mode' (firing its hook, which
        ;; applies the drawer preset — temperature 0.99) and then
        ;; applies metadata.yml's preset (temperature 0.11). metadata
        ;; must win.
        (setq buf (find-file-noselect session-file))
        (with-current-buffer buf
          (jf-gptel-preset-app-test--register-cleanup
           "sess-precedence-20260421120000" "main")
          ;; Chat-mode is active.
          (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
          ;; metadata.yml's preset is the authoritative one.
          (expect gptel--preset :to-equal meta-preset)
          ;; Temperature reflects metadata.yml's preset, not drawer's.
          (expect gptel-temperature :to-equal 0.11))))))

(provide 'preset-application-spec)
;;; preset-application-spec.el ends here
