;;; registration.el --- GPTEL Preset Registration -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: gptel, llm, presets

;;; Commentary:

;; Fragment-era preset registration.  Each preset .org tangles to an .el that
;; renders its role fragment at tangle time and calls `jf/gptel-preset-register'
;; with native-Elisp config and a pre-rendered :system.  This module extracts
;; scope/mode keys into side tables and registers via `gptel-make-preset'.
;; The YAML parse/normalize/coerce pipeline has been removed.

;;; Code:

(require 'cl-lib)

(defvar jf/gptel-preset--scope-defaults nil
  "Alist mapping preset name symbols to scope plists.
Each entry is (PRESET-NAME . SCOPE-PLIST) where SCOPE-PLIST contains
keys :paths, :shell-commands, :bash-tools, :scope-profile that were
extracted from the preset config during registration.  The legacy
:org-roam-patterns key is not extracted; see
`jf/gptel-preset--extract-scope' for the legacy-key warning behavior.")

(defvar jf/gptel-preset--mode-defaults nil
  "Alist mapping preset name symbols to mode plists.
Each entry is (PRESET-NAME . MODE-PLIST) where MODE-PLIST contains
the :mode key extracted from the preset config during registration.
Valid :mode values are \"org-mode\" and \"markdown-mode\".
Defaults to \"org-mode\" when not specified in the preset.")

(defun jf/gptel-preset--extract-scope (plist preset-name)
  "Extract scope keys from PLIST and store under PRESET-NAME.
Scope keys are :paths, :shell-commands, :bash-tools, and :scope-profile.
If any scope keys are present, store them in `jf/gptel-preset--scope-defaults'
keyed by PRESET-NAME (a symbol).
Return a new plist with scope keys removed.

Legacy keys (currently `:org-roam-patterns'): cycle-3 removed pattern-based
validation from the scope subsystem; the key no longer has a consumer.  Presets
that still carry it trigger a `display-warning' naming the preset and the
ignored key, and the key is dropped from both the scope plist and the returned
config (it is not a scope key, so it never reaches scope-defaults)."
  (let ((scope-keys '(:paths :shell-commands :bash-tools :scope-profile))
        (legacy-scope-keys '(:org-roam-patterns))
        (scope-plist nil)
        (legacy-keys-present nil)
        (result nil))
    ;; Walk the plist once: collect live scope keys, note legacy keys (and
    ;; drop them from both scope-plist and result), pass through everything
    ;; else.
    (let ((remaining plist))
      (while remaining
        (let ((key (pop remaining))
              (val (pop remaining)))
          (cond
           ((memq key legacy-scope-keys)
            (push key legacy-keys-present))
           ((memq key scope-keys)
            (setq scope-plist (plist-put scope-plist key val)))
           (t
            (setq result (plist-put result key val)))))))
    ;; Surface legacy keys via display-warning so the user can clean up.
    (when legacy-keys-present
      (display-warning
       'jf-gptel-preset
       (format "Preset %S carries legacy scope key(s) %S; the key(s) are ignored. Remove from the preset definition."
               preset-name (nreverse legacy-keys-present))
       :warning))
    ;; Store scope defaults if any live scope keys were found.
    (when scope-plist
      (let ((existing (assq preset-name jf/gptel-preset--scope-defaults)))
        (if existing
            (setcdr existing scope-plist)
          (push (cons preset-name scope-plist) jf/gptel-preset--scope-defaults))))
    result))

(defun jf/gptel-preset--extract-mode (plist preset-name)
  "Extract :mode key from PLIST and store under PRESET-NAME.
If :mode is present, validate it and store in `jf/gptel-preset--mode-defaults'
keyed by PRESET-NAME (a symbol).  If :mode is absent, store the default
\"org-mode\".  Valid values are \"org-mode\" and \"markdown-mode\".
Return a new plist with :mode removed."
  (let ((mode-val (plist-get plist :mode))
        (result nil))
    ;; Build result plist without :mode.
    (let ((remaining plist))
      (while remaining
        (let ((key (pop remaining))
              (val (pop remaining)))
          (unless (eq key :mode)
            (setq result (plist-put result key val))))))
    ;; Default to "org-mode" if not specified.
    (let ((effective-mode (or mode-val "org-mode")))
      ;; Validate.
      (unless (member effective-mode '("org-mode" "markdown-mode"))
        (when (fboundp 'jf/gptel--log)
          (jf/gptel--log 'warn "Preset %s has invalid :mode \"%s\", defaulting to org-mode"
                         preset-name effective-mode))
        (setq effective-mode "org-mode"))
      ;; Store in side table.
      (let ((mode-plist (list :mode effective-mode))
            (existing (assq preset-name jf/gptel-preset--mode-defaults)))
        (if existing
            (setcdr existing mode-plist)
          (push (cons preset-name mode-plist) jf/gptel-preset--mode-defaults))))
    result))

(defun jf/gptel-preset-register (name &rest config)
  "Register the preset NAME with native-Elisp CONFIG.
NAME is a symbol (typically the preset's source basename).  CONFIG is a
plist of native-Elisp gptel-preset keys; :system, when present, is the
preset's pre-rendered role text.

Scope keys (:paths :shell-commands :bash-tools :scope-profile) are
extracted into `jf/gptel-preset--scope-defaults' and removed from the
plist; the legacy :org-roam-patterns key is dropped (see
`jf/gptel-preset--extract-scope').  The :mode key is extracted into
`jf/gptel-preset--mode-defaults' (default \"org-mode\").  The remaining
config is registered via upstream `gptel-make-preset', keyed by NAME.

Idempotent: re-calling with the same NAME updates the existing entry.
Returns NAME."
  (let* ((scope-cleaned (jf/gptel-preset--extract-scope config name))
         (cleaned (jf/gptel-preset--extract-mode scope-cleaned name)))
    (apply #'gptel-make-preset name cleaned)
    name))

(defun jf/gptel-preset-register-all ()
  "Load all tangled preset .el artifacts so they self-register.
Discovers `<jf/gptel-presets-directory>/<name>/preset.el' files and
`load's each.  Each preset.el renders its role fragment (at tangle time)
and calls `jf/gptel-preset-register', which extracts scope/mode keys and
registers via `gptel-make-preset'.

This function is idempotent -- re-loading a preset.el updates the
existing entry.  Logs the count of loaded preset artifacts."
  (interactive)
  (let ((presets-dir (and (boundp 'jf/gptel-presets-directory)
                          jf/gptel-presets-directory))
        (count 0))
    (if (or (null presets-dir) (not (file-directory-p presets-dir)))
        (when (fboundp 'jf/gptel--log)
          (jf/gptel--log 'warn "Presets directory does not exist: %s" presets-dir))
      (dolist (subdir (directory-files presets-dir t directory-files-no-dot-files-regexp))
        (when (file-directory-p subdir)
          (let ((preset-el (expand-file-name "preset.el" subdir)))
            (when (file-readable-p preset-el)
              (condition-case err
                  (progn
                    (load preset-el nil t)
                    (setq count (1+ count)))
                (error
                 (when (fboundp 'jf/gptel--log)
                   (jf/gptel--log 'warn "Error loading preset %s: %s"
                                  preset-el (error-message-string err)))))))))
      (when (fboundp 'jf/gptel--log)
        (jf/gptel--log 'info "Loaded %d preset artifacts from %s" count presets-dir)))
    count))

(defvar jf/gptel-fragment-sources-directory
  (and (boundp 'jf/gptel-presets-directory)
       (expand-file-name "sources/" jf/gptel-presets-directory))
  "Directory holding flat fragment SOURCE .el files.
Each file under this directory is `load'ed at gptel init by
`jf/gptel-fragment--load-sources-all' so its composer-seam side effect
runs in production.  Derived from `jf/gptel-presets-directory'.")

(defun jf/gptel-fragment--load-sources-all ()
  "Load every fragment SOURCE .el under `jf/gptel-fragment-sources-directory'.
Sources are flat `.el' files (not `<name>/' subdirs); each `load's the
renderer feature and populates a composer seam as a side effect.  Files
are loaded once each, in sorted filename order (no inter-source ordering
dependency exists today).  Must be called AFTER `presets/fragments.el'
(which defines the seam vars) and BEFORE the chat/agent/env consumers.

Idempotent at the source layer: re-loading a source re-runs its
seam-population `setq', which is the same assignment.  Logs the count of
loaded sources.  Returns the count."
  (interactive)
  (let ((sources-dir jf/gptel-fragment-sources-directory)
        (count 0))
    (if (or (null sources-dir) (not (file-directory-p sources-dir)))
        (when (fboundp 'jf/gptel--log)
          (jf/gptel--log 'warn "Fragment sources directory does not exist: %s"
                         sources-dir))
      (dolist (source-el (sort (directory-files sources-dir t "\\.el\\'")
                               #'string<))
        (when (file-readable-p source-el)
          (condition-case err
              (progn
                (load source-el nil t)
                (setq count (1+ count)))
            (error
             (when (fboundp 'jf/gptel--log)
               (jf/gptel--log 'warn "Error loading fragment source %s: %s"
                              source-el (error-message-string err)))))))
      (when (fboundp 'jf/gptel--log)
        (jf/gptel--log 'info "Loaded %d fragment source(s) from %s"
                       count sources-dir)))
    count))

(provide 'gptel-preset-registration)
;;; registration.el ends here
