;;; registration-spec.el --- Specs for fragment-era preset registration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral specs for `config/gptel/presets/registration.el', the fragment-era
;; preset registration pipeline.  Presets are authored as `.org' sources tangling
;; to self-registering `<name>/preset.el' artifacts; the YAML parse/normalize/
;; coerce pipeline has been deleted.
;;
;; Covered contract:
;;   - A preset registers from its tangled `.el' (name = source basename).
;;   - Re-loading a preset `.el' updates the entry (idempotent, no duplicate).
;;   - Scope keys (:paths :shell-commands :bash-tools :scope-profile) are stored
;;     in `jf/gptel-preset--scope-defaults' AND stripped from the registration
;;     plist; :scope-profile is extracted; :org-roam-patterns is NOT extracted.
;;   - A preset with no scope keys is registered unchanged (no scope-defaults
;;     entry).
;;   - :mode is extracted into `jf/gptel-preset--mode-defaults' (default
;;     "org-mode").
;;   - A registered preset is visible to gptel's transient menu / inline
;;     @mention surface (via `gptel--known-presets' / `gptel-get-preset', with
;;     its :description annotation).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (presets-dir (expand-file-name "../" test-dir)))
  (require 'gptel-preset-registration
           (expand-file-name "registration.el" presets-dir)))

;; Minimal stubs for the upstream gptel preset store so the suite is
;; self-contained when run directory-scoped (no full gptel load).
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

(describe "fragment-era preset registration"

  (before-each
    (setq jf/gptel-preset--scope-defaults nil)
    (setq jf/gptel-preset--mode-defaults nil)
    (setq gptel--known-presets nil))

  (after-each
    (setq jf/gptel-preset--scope-defaults nil)
    (setq jf/gptel-preset--mode-defaults nil)
    (setq gptel--known-presets nil))

  (describe "jf/gptel-preset-register"

    (it "registers a preset under its name symbol"
      (jf/gptel-preset-register 'sample-preset
                                :description "A sample"
                                :backend "Claude"
                                :model 'claude-opus-4-6
                                :system "rendered role text")
      (let ((spec (gptel-get-preset 'sample-preset)))
        (expect spec :not :to-be nil)
        (expect (plist-get spec :description) :to-equal "A sample")
        (expect (plist-get spec :system) :to-equal "rendered role text")))

    (it "passes the pre-rendered :system through verbatim"
      ;; Registration does NOT render -- the role fragment is rendered at
      ;; tangle time and arrives as :system.
      (jf/gptel-preset-register 'role-preset
                                :system "<role>\nbe helpful\n</role>")
      (expect (plist-get (gptel-get-preset 'role-preset) :system)
              :to-equal "<role>\nbe helpful\n</role>"))

    (it "is idempotent: re-registration updates rather than duplicates"
      (jf/gptel-preset-register 'dup-preset :description "first")
      (jf/gptel-preset-register 'dup-preset :description "second")
      (expect (cl-count 'dup-preset gptel--known-presets :key #'car)
              :to-equal 1)
      (expect (plist-get (gptel-get-preset 'dup-preset) :description)
              :to-equal "second"))

    (describe "scope key extraction"

      (it "stores scope keys in scope-defaults and strips them from registration"
        (jf/gptel-preset-register 'scoped-preset
                                  :description "scoped"
                                  :paths '(:read ("/tmp/**"))
                                  :shell-commands '(:allow ("ls"))
                                  :bash-tools '(:deny ("rm"))
                                  :scope-profile "coding"
                                  :model 'claude-opus-4-6)
        (let ((scope (cdr (assq 'scoped-preset jf/gptel-preset--scope-defaults)))
              (spec (gptel-get-preset 'scoped-preset)))
          ;; Stored in scope-defaults
          (expect (plist-get scope :paths) :to-equal '(:read ("/tmp/**")))
          (expect (plist-get scope :shell-commands) :to-equal '(:allow ("ls")))
          (expect (plist-get scope :bash-tools) :to-equal '(:deny ("rm")))
          (expect (plist-get scope :scope-profile) :to-equal "coding")
          ;; Stripped from the registration plist
          (expect (plist-member spec :paths) :to-be nil)
          (expect (plist-member spec :shell-commands) :to-be nil)
          (expect (plist-member spec :bash-tools) :to-be nil)
          (expect (plist-member spec :scope-profile) :to-be nil)
          ;; Non-scope keys survive
          (expect (plist-get spec :description) :to-equal "scoped")
          (expect (plist-get spec :model) :to-equal 'claude-opus-4-6)))

      (it "extracts :scope-profile on its own"
        (jf/gptel-preset-register 'profile-preset :scope-profile "coding")
        (let ((scope (cdr (assq 'profile-preset jf/gptel-preset--scope-defaults))))
          (expect (plist-get scope :scope-profile) :to-equal "coding"))
        (expect (plist-member (gptel-get-preset 'profile-preset) :scope-profile)
                :to-be nil))

      (it "does NOT extract :org-roam-patterns (not a scope key)"
        (spy-on 'display-warning)
        (jf/gptel-preset-register 'legacy-preset
                                  :paths '(:read ("/tmp/**"))
                                  :org-roam-patterns '(:tags ("gptel")))
        (let ((scope (cdr (assq 'legacy-preset jf/gptel-preset--scope-defaults))))
          (expect (plist-get scope :paths) :to-equal '(:read ("/tmp/**")))
          (expect (plist-member scope :org-roam-patterns) :to-be nil))
        ;; And it is dropped from the registration plist too.
        (expect (plist-member (gptel-get-preset 'legacy-preset) :org-roam-patterns)
                :to-be nil))

      (it "adds no scope-defaults entry for a preset with no scope keys"
        (jf/gptel-preset-register 'plain-preset
                                  :description "plain"
                                  :model 'claude-opus-4-6)
        (expect (assq 'plain-preset jf/gptel-preset--scope-defaults) :to-be nil)
        ;; Registration plist passes through unchanged (sans :mode default extract).
        (let ((spec (gptel-get-preset 'plain-preset)))
          (expect (plist-get spec :description) :to-equal "plain")
          (expect (plist-get spec :model) :to-equal 'claude-opus-4-6))))

    (describe "mode extraction"

      (it "extracts :mode into mode-defaults and strips it from registration"
        (jf/gptel-preset-register 'md-preset :mode "markdown-mode")
        (expect (plist-get (cdr (assq 'md-preset jf/gptel-preset--mode-defaults)) :mode)
                :to-equal "markdown-mode")
        (expect (plist-member (gptel-get-preset 'md-preset) :mode) :to-be nil))

      (it "defaults to org-mode when :mode is absent"
        (jf/gptel-preset-register 'nomode-preset :description "x")
        (expect (plist-get (cdr (assq 'nomode-preset jf/gptel-preset--mode-defaults)) :mode)
                :to-equal "org-mode"))

      (it "falls back to org-mode for an invalid :mode"
        (jf/gptel-preset-register 'badmode-preset :mode "fundamental-mode")
        (expect (plist-get (cdr (assq 'badmode-preset jf/gptel-preset--mode-defaults)) :mode)
                :to-equal "org-mode")))

    (describe "transient menu / inline @mention visibility"

      (it "makes the preset visible with its :description annotation"
        (jf/gptel-preset-register 'menu-preset
                                  :description "Shown in the menu"
                                  :model 'claude-opus-4-6)
        ;; The transient menu and @mention surface both read
        ;; `gptel--known-presets'; presence there (with :description) is the
        ;; contract those surfaces depend on.
        (expect (assq 'menu-preset gptel--known-presets) :not :to-be nil)
        (expect (plist-get (gptel-get-preset 'menu-preset) :description)
                :to-equal "Shown in the menu"))))

  (describe "jf/gptel-preset-register-all"

    (let (tmp-dir orig-dir)

      (before-each
        (setq orig-dir (and (boundp 'jf/gptel-presets-directory)
                            jf/gptel-presets-directory))
        (setq tmp-dir (make-temp-file "gptel-presets-test" t))
        (unless (boundp 'jf/gptel-presets-directory)
          (defvar jf/gptel-presets-directory nil))
        (setq jf/gptel-presets-directory tmp-dir))

      (after-each
        (when (and tmp-dir (file-directory-p tmp-dir))
          (delete-directory tmp-dir t))
        (setq jf/gptel-presets-directory orig-dir))

      (cl-flet ((write-preset
                  (name body)
                  (let ((dir (expand-file-name name tmp-dir)))
                    (make-directory dir t)
                    (with-temp-file (expand-file-name "preset.el" dir)
                      (insert body)))))

        (it "loads each <name>/preset.el so it self-registers"
          (write-preset "alpha"
                        "(jf/gptel-preset-register 'alpha :description \"A\")")
          (write-preset "beta"
                        "(jf/gptel-preset-register 'beta :description \"B\")")
          (let ((loaded (jf/gptel-preset-register-all)))
            (expect loaded :to-equal 2))
          (expect (gptel-get-preset 'alpha) :not :to-be nil)
          (expect (gptel-get-preset 'beta) :not :to-be nil))

        (it "derives the preset name from the source basename"
          ;; The preset.el itself names the preset; canonical authoring uses the
          ;; subdirectory basename as the name symbol.
          (write-preset "system-explorer"
                        "(jf/gptel-preset-register 'system-explorer :description \"E\")")
          (jf/gptel-preset-register-all)
          (expect (gptel-get-preset 'system-explorer) :not :to-be nil))

        (it "is idempotent across re-loads (updates, no duplicate)"
          (write-preset "gamma"
                        "(jf/gptel-preset-register 'gamma :description \"v1\")")
          (jf/gptel-preset-register-all)
          ;; Re-author and re-load.
          (write-preset "gamma"
                        "(jf/gptel-preset-register 'gamma :description \"v2\")")
          (jf/gptel-preset-register-all)
          (expect (cl-count 'gamma gptel--known-presets :key #'car) :to-equal 1)
          (expect (plist-get (gptel-get-preset 'gamma) :description)
                  :to-equal "v2"))

        (it "ignores subdirectories without a preset.el"
          (make-directory (expand-file-name "not-a-preset" tmp-dir) t)
          (write-preset "real" "(jf/gptel-preset-register 'real :description \"R\")")
          (expect (jf/gptel-preset-register-all) :to-equal 1)
          (expect (gptel-get-preset 'real) :not :to-be nil))))))

(provide 'gptel-presets-registration-spec)

;;; registration-spec.el ends here
