;;; workspace-assistant-spec.el --- Specs for the workspace-assistant preset -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral specs for the `workspace-assistant' preset
;; (`config/gptel/presets/workspace-assistant/preset.el').
;;
;; Covered contract:
;;   - Loading the tangled preset.el self-registers `workspace-assistant'
;;     (name = source basename) with its native-Elisp config keys.
;;   - The registered :system is the role fragment pre-rendered for the Claude
;;     backend, byte-for-byte equal to the committed golden snapshot.
;;   - The :scope-profile scope key lands in `jf/gptel-preset--scope-defaults'
;;     and is stripped from the registration plist.
;;   - The :system is rendered once (a stable static string), not re-rendered
;;     per send (invariant static-prerender-dynamic-compose).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the golden reader, the renderer, and registration (with a self-contained
;; gptel preset-store stub) so the suite runs directory-scoped without a full
;; gptel load.
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (presets-dir (expand-file-name ".." test-dir)))
  (require 'presets-helpers-spec (expand-file-name "helpers-spec.el" test-dir))
  (require 'jf-gptel-fragments (expand-file-name "fragments.el" presets-dir))
  (require 'gptel-preset-registration
           (expand-file-name "registration.el" presets-dir)))

(unless (boundp 'gptel--known-presets)
  (defvar gptel--known-presets nil))
(unless (fboundp 'gptel-make-preset)
  (defun gptel-make-preset (name &rest keys)
    "Test stub mirroring upstream `gptel-make-preset' (idempotent by NAME)."
    (if-let* ((p (assoc name gptel--known-presets)))
        (setcdr p keys)
      (setq gptel--known-presets
            (nconc gptel--known-presets (list (cons name keys)))))))
(unless (fboundp 'gptel-get-preset)
  (defun gptel-get-preset (name)
    "Test stub mirroring upstream `gptel-get-preset'."
    (alist-get name gptel--known-presets nil nil #'equal)))

(defconst workspace-assistant-spec--preset-el
  (let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
    (expand-file-name "../workspace-assistant/preset.el" test-dir))
  "Path to the tangled workspace-assistant preset artifact under test.")

(describe "workspace-assistant preset"

  (before-each
    (setq jf/gptel-preset--scope-defaults nil)
    (setq jf/gptel-preset--mode-defaults nil)
    (setq gptel--known-presets nil)
    ;; Load the tangled artifact; it self-registers via jf/gptel-preset-register.
    (load workspace-assistant-spec--preset-el nil t))

  (after-each
    (setq jf/gptel-preset--scope-defaults nil)
    (setq jf/gptel-preset--mode-defaults nil)
    (setq gptel--known-presets nil))

  (it "registers under the basename symbol `workspace-assistant'"
    (expect (gptel-get-preset 'workspace-assistant) :not :to-be nil))

  (it "resolves the expected native config keys"
    (let ((spec (gptel-get-preset 'workspace-assistant)))
      (expect (plist-get spec :description)
              :to-equal "General-purpose helper for the active project workspace.")
      (expect (plist-get spec :backend) :to-equal 'claude)
      (expect (plist-get spec :model) :to-equal 'claude-sonnet-4-6)
      (expect (plist-get spec :temperature) :to-equal 0.3)))

  (it "registers :system as the role rendered to the golden Claude snapshot"
    (expect (plist-get (gptel-get-preset 'workspace-assistant) :system)
            :to-equal (presets-test-read-golden "workspace-assistant.claude.txt")))

  (it "renders the role from the static fragment source (not a hand-authored literal)"
    ;; The committed :system must be exactly what the renderer produces from the
    ;; declared role source, proving the fragment model end-to-end.
    (expect (plist-get (gptel-get-preset 'workspace-assistant) :system)
            :to-equal
            (jf/gptel-fragment-render
             (jf/gptel-fragment--parse-source
              jf/gptel-preset-workspace-assistant--role-source)
             'claude)))

  (it "pre-renders :system once into a stable static string"
    ;; static-prerender-dynamic-compose: the role text is fixed at load time, so
    ;; the registered :system is `eq' to the cached `--system' constant -- it is
    ;; not recomputed per access/send.
    (expect (plist-get (gptel-get-preset 'workspace-assistant) :system)
            :to-be jf/gptel-preset-workspace-assistant--system))

  (describe "scope extraction"

    (it "stores :scope-profile in scope-defaults"
      (let ((scope (cdr (assq 'workspace-assistant
                              jf/gptel-preset--scope-defaults))))
        (expect (plist-get scope :scope-profile) :to-equal "coding")))

    (it "strips :scope-profile from the registration plist"
      (expect (plist-member (gptel-get-preset 'workspace-assistant) :scope-profile)
              :to-be nil)))

  (it "registers no palette/agent tools yet (out of scope for this change)"
    (expect (plist-member (gptel-get-preset 'workspace-assistant) :tools)
            :to-be nil)))

(provide 'gptel-presets-workspace-assistant-spec)

;;; workspace-assistant-spec.el ends here
