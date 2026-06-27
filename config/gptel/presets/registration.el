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
(require 'seq)

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

(defvar jf/gptel--loader-failures nil
  "List of load-error failure records from the directory loaders.
Each element is a plist (:file FILE :seam SEAM :error MESSAGE) recorded
by `jf/gptel--load-directory' when an entry's `.el' signals on load.
SEAM is the composer-seam symbol that entry failed to populate (e.g.
`jf/gptel-fragment-environment-fn' for a fragment source,
`gptel--known-presets' for a preset).  A post-init self-check
(`jf/gptel-loader-seam-dark-p') reads this to report which seams are
dark.  Cleared with `jf/gptel-loader-clear-failures'.")

(defun jf/gptel-loader-clear-failures ()
  "Clear the recorded directory-loader failure registry.
Call before a fresh (re-)load pass so stale failures from a previous
pass do not linger in `jf/gptel--loader-failures'."
  (setq jf/gptel--loader-failures nil))

(defun jf/gptel-loader-failures (&optional seam)
  "Return recorded directory-loader failures.
With SEAM non-nil, return only the failure records whose :seam matches
SEAM; otherwise return all records.  Each record is a plist
\(:file FILE :seam SEAM :error MESSAGE)."
  (if seam
      (seq-filter (lambda (rec) (eq (plist-get rec :seam) seam))
                  jf/gptel--loader-failures)
    jf/gptel--loader-failures))

(defun jf/gptel-loader-seam-dark-p (seam)
  "Return non-nil if SEAM has a recorded load failure.
A truthy result means at least one entry that should have populated SEAM
failed to load, so SEAM may still hold its dark default.  This is the
post-init self-check the load-error fail-policy exposes: callers can
assert seams are live, or surface which seams are dark, instead of a
silent warn-and-continue."
  (and (jf/gptel-loader-failures seam) t))

(defun jf/gptel--load-directory (directory discover-fn seam label)
  "Load every entry DISCOVER-FN finds under DIRECTORY, guarding each load.
DISCOVER-FN is a function of one argument (DIRECTORY) returning the
ordered list of `.el' files to `load'.  SEAM is the composer-seam symbol
the loaded entries are expected to populate (used in error logs and
recorded against failures).  LABEL is a short human string for log
messages (e.g. \"fragment source\", \"preset\").

Each entry is `load'ed inside a `condition-case'.  A load that signals is
NOT swallowed silently: it is logged at ERROR level naming the file and
SEAM, and recorded in `jf/gptel--loader-failures' so
`jf/gptel-loader-seam-dark-p' can report SEAM as dark.  One bad entry
does not abort the others.

A missing DIRECTORY is a graceful no-op (warn-logged) returning 0.
Returns the count of entries that loaded successfully."
  (let ((count 0))
    (if (or (null directory) (not (file-directory-p directory)))
        (when (fboundp 'jf/gptel--log)
          (jf/gptel--log 'warn "%s directory does not exist: %s"
                         (capitalize label) directory))
      (dolist (entry (funcall discover-fn directory))
        (when (file-readable-p entry)
          (condition-case err
              (progn
                (load entry nil t)
                (setq count (1+ count)))
            (error
             (let ((msg (error-message-string err)))
               (when (fboundp 'jf/gptel--log)
                 (jf/gptel--log
                  'error
                  "Error loading %s %s: %s (seam %s left unpopulated)"
                  label entry msg seam))
               (push (list :file entry :seam seam :error msg)
                     jf/gptel--loader-failures))))))
      (when (fboundp 'jf/gptel--log)
        (jf/gptel--log 'info "Loaded %d %s(s) from %s"
                       count label directory)))
    count))

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

(defun jf/gptel-preset--discover-preset-els (presets-dir)
  "Return the list of `<name>/preset.el' files under PRESETS-DIR.
Descends each immediate subdirectory of PRESETS-DIR and collects its
`preset.el' (the tangled, self-registering preset artifact).
Subdirectories without a `preset.el' are skipped.  This is the
discovery half of `jf/gptel-preset-register-all'; the load + fail-policy
half lives in `jf/gptel--load-directory'."
  (let (entries)
    (dolist (subdir (directory-files presets-dir t directory-files-no-dot-files-regexp))
      (when (file-directory-p subdir)
        (let ((preset-el (expand-file-name "preset.el" subdir)))
          (when (file-exists-p preset-el)
            (push preset-el entries)))))
    (nreverse entries)))

(defun jf/gptel-preset-register-all ()
  "Load all tangled preset .el artifacts so they self-register.
Discovers `<jf/gptel-presets-directory>/<name>/preset.el' files and
`load's each.  Each preset.el renders its role fragment (at tangle time)
and calls `jf/gptel-preset-register', which extracts scope/mode keys and
registers via `gptel-make-preset'.

This function is idempotent -- re-loading a preset.el updates the
existing entry.  Routes through `jf/gptel--load-directory', which owns the
per-entry `condition-case' and the non-silent load-error fail-policy: a
preset whose .el signals on load is logged at ERROR level and recorded in
`jf/gptel--loader-failures' (seam `gptel--known-presets'), not silently
swallowed.  Logs the count of loaded preset artifacts."
  (interactive)
  (jf/gptel--load-directory
   (and (boundp 'jf/gptel-presets-directory) jf/gptel-presets-directory)
   #'jf/gptel-preset--discover-preset-els
   'gptel--known-presets
   "preset"))

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
seam-population `setq', which is the same assignment.  Routes through
`jf/gptel--load-directory', which owns the per-entry `condition-case' and
the non-silent load-error fail-policy: a source whose .el signals on load
is logged at ERROR level and recorded in `jf/gptel--loader-failures'
\(seam `jf/gptel-fragment-environment-fn'), not silently swallowed.  Logs
the count of loaded sources.  Returns the count."
  (interactive)
  (jf/gptel--load-directory
   jf/gptel-fragment-sources-directory
   #'jf/gptel-fragment--discover-source-els
   'jf/gptel-fragment-environment-fn
   "fragment source"))

(defun jf/gptel-fragment--discover-source-els (sources-dir)
  "Return the flat `.el' files under SOURCES-DIR in sorted filename order.
Fragment SOURCE modules are flat `.el' files (not `<name>/' subdirs).
Sorted with `string<' for determinism; no inter-source ordering
dependency exists today.  This is the discovery half of
`jf/gptel-fragment--load-sources-all'; the load + fail-policy half lives
in `jf/gptel--load-directory'."
  (sort (directory-files sources-dir t "\\.el\\'") #'string<))

(provide 'gptel-preset-registration)
;;; registration.el ends here
