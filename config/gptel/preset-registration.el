;;; preset-registration.el --- GPTEL Preset Registration -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Pipeline for registering gptel presets from .md files with YAML frontmatter.
;; Parses, normalizes, coerces, extracts scope, and registers via gptel-make-preset.

;;; Code:

(require 'cl-lib)
(require 'yaml)

(defvar jf/gptel-preset--scope-defaults nil
  "Alist mapping preset name symbols to scope plists.
Each entry is (PRESET-NAME . SCOPE-PLIST) where SCOPE-PLIST contains
keys like :paths, :org-roam-patterns, :shell-commands, :scope-profile
that were extracted from the preset file during registration.")

(defun jf/gptel-preset--parse-file (filepath)
  "Parse a preset .md file at FILEPATH.
Extract YAML frontmatter between --- delimiters and parse it.
The markdown body after the closing --- becomes the :system value.
Return a plist on success, or nil on failure (with warning logged)."
  (condition-case err
      (if (not (file-readable-p filepath))
          (progn
            (jf/gptel--log 'warn "Preset file not readable: %s" filepath)
            nil)
        (with-temp-buffer
          (insert-file-contents filepath)
          (goto-char (point-min))
          ;; Expect opening ---
          (unless (looking-at-p "^---[[:space:]]*$")
            (jf/gptel--log 'warn "Preset file missing opening frontmatter delimiter: %s" filepath)
            (cl-return-from jf/gptel-preset--parse-file nil))
          (forward-line 1)
          (let ((yaml-start (point)))
            ;; Find closing ---
            (unless (re-search-forward "^---[[:space:]]*$" nil t)
              (jf/gptel--log 'warn "Preset file missing closing frontmatter delimiter: %s" filepath)
              (cl-return-from jf/gptel-preset--parse-file nil))
            (let* ((yaml-end (match-beginning 0))
                   (yaml-content (buffer-substring-no-properties yaml-start yaml-end))
                   (body-start (match-end 0))
                   (body (string-trim (buffer-substring-no-properties body-start (point-max))))
                   (parsed (yaml-parse-string yaml-content
                                              :object-type 'plist
                                              :object-key-type 'keyword
                                              :sequence-type 'list)))
              ;; Set :system to the body text
              (when (and body (not (string-empty-p body)))
                (setq parsed (plist-put parsed :system body)))
              parsed))))
    (error
     (jf/gptel--log 'warn "Error parsing preset file %s: %s" filepath (error-message-string err))
     nil)))

(defun jf/gptel-preset--normalize-keys (plist)
  "Normalize all snake_case keywords in PLIST to kebab-case.
Return a new plist with converted keys.  Values are unchanged.
Already-hyphenated keys pass through unchanged."
  (let ((result nil))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (key-name (symbol-name key))
             (normalized-name (replace-regexp-in-string "_" "-" key-name))
             (normalized-key (intern normalized-name)))
        (setq result (plist-put result normalized-key val))))
    result))

(defun jf/gptel-preset--coerce-values (plist)
  "Coerce YAML-parsed values in PLIST to proper elisp types.
Handles:
- :model string values interned to symbols
- :confirm-tool-calls string mapping (\"nil\" -> nil, \"auto\" -> \\='auto, \"always\" -> t)
- General :false -> nil for any key
Return a new plist with coerced values."
  (let ((result nil))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (coerced-val
              (cond
               ;; :model -- intern string to symbol
               ((and (eq key :model) (stringp val))
                (intern val))
               ;; :confirm-tool-calls -- map string values
               ((eq key :confirm-tool-calls)
                (cond
                 ((equal val "nil") nil)
                 ((eq val :false) nil)
                 ((equal val "auto") 'auto)
                 ((equal val "always") t)
                 (t val)))
               ;; General :false -> nil for any key
               ((eq val :false) nil)
               ;; Everything else passes through
               (t val))))
        (setq result (plist-put result key coerced-val))))
    result))

(defun jf/gptel-preset--extract-scope (plist preset-name)
  "Extract scope keys from PLIST and store under PRESET-NAME.
Scope keys are :paths, :org-roam-patterns, :shell-commands, and :scope-profile.
If any scope keys are present, store them in `jf/gptel-preset--scope-defaults'
keyed by PRESET-NAME (a symbol).
Return a new plist with scope keys removed."
  (let ((scope-keys '(:paths :org-roam-patterns :shell-commands :scope-profile))
        (scope-plist nil)
        (result nil))
    ;; Collect scope keys and non-scope keys
    (let ((remaining plist))
      (while remaining
        (let ((key (pop remaining))
              (val (pop remaining)))
          (if (memq key scope-keys)
              (setq scope-plist (plist-put scope-plist key val))
            (setq result (plist-put result key val))))))
    ;; Store scope defaults if any scope keys were found
    (when scope-plist
      (let ((existing (assq preset-name jf/gptel-preset--scope-defaults)))
        (if existing
            (setcdr existing scope-plist)
          (push (cons preset-name scope-plist) jf/gptel-preset--scope-defaults))))
    result))

(defun jf/gptel-preset-register-all ()
  "Register all preset files from `jf/gptel-presets-directory'.
Scan for .md files and for each one: parse YAML frontmatter,
normalize keys, coerce values, extract scope config, and register
via `gptel-make-preset'.

This function is idempotent -- re-registration updates existing entries.
Logs the count of registered presets."
  (interactive)
  (let ((presets-dir jf/gptel-presets-directory)
        (count 0))
    (if (not (file-directory-p presets-dir))
        (jf/gptel--log 'warn "Presets directory does not exist: %s" presets-dir)
      (let ((files (directory-files presets-dir t "\\.md\\'" t)))
        (dolist (file files)
          (let* ((basename (file-name-sans-extension (file-name-nondirectory file)))
                 (preset-name (intern basename))
                 (parsed (jf/gptel-preset--parse-file file)))
            (when parsed
              (let* ((normalized (jf/gptel-preset--normalize-keys parsed))
                     (coerced (jf/gptel-preset--coerce-values normalized))
                     (cleaned (jf/gptel-preset--extract-scope coerced preset-name)))
                (apply #'gptel-make-preset preset-name cleaned)
                (setq count (1+ count))))))
        (jf/gptel--log 'info "Registered %d presets from %s" count presets-dir)))))

(provide 'gptel-preset-registration)
;;; preset-registration.el ends here
