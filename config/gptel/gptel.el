;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;;  (use-package shell-maker
;;    :straight (shell-maker :type git :host github :repo "xenodium/shell-maker" :files ("shell-maker.el")))
;;
;;  ; add :build (:not compile) if byte code has problems
;;  (use-package chatgpt-shell
;;    :requires shell-maker
;;    :straight (:host github :repo "xenodium/chatgpt-shell" :files ("*.el") :build (:not compile)))
;;
;;  (setq chatgpt-shell-openai-key
;;        (lambda ()
;;          (auth-source-pick-first-password :host "api.openai.com")))
;;
;;  ;;(setq chatgpt-shell-model-version "gpt-4o")

(use-package gptel
  :straight t
  :custom
  (gptel-model 'gpt-4o) ;; model is now a symbol, not a string
  (gptel-log-level 'debug) ;; Enable debug logging for testing
  :config
  ;; Enable prompt caching for Anthropic models
  (setq gptel-cache t)
  ;; Enable expert commands to show advanced options in transient menu
  (setq gptel-expert-commands t)
  ;; Configure Perplexity backend
  (gptel-make-perplexity "Perplexity"
    :key (lambda () (auth-source-pick-first-password :host "api.perplexity.ai"))
    :stream t)

  ;; Configure Anthropic backend with curated Claude 4.5/4.6 models
  ;; Includes lightweight (Haiku), balanced (Sonnet), and heavyweight (Opus) options
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda () (auth-source-pick-first-password :host "api.anthropic.com"))
    :models '(claude-haiku-4-5-20251001      ; Fast & economical for simple tasks
              claude-sonnet-4-6              ; Balanced for most use cases
              claude-opus-4-6))              ; Most capable for complex reasoning

  ;; Configure Claude Sonnet 4.6 with extended thinking mode for complex problems
  ;; Thinking mode provides detailed reasoning before answering
  ;; Note: gptel automatically handles anthropic-beta headers for caching
  (gptel-make-anthropic "Claude-thinking"
    :key (lambda () (auth-source-pick-first-password :host "api.anthropic.com"))
    :stream t
    :models '(claude-sonnet-4-6)
    :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                      :max_tokens 4096))

  ;; Configure ChatGPT/OpenAI backend
  (gptel-make-openai "ChatGPT"
    :key (lambda () (auth-source-pick-first-password :host "api.openai.com"))
    :stream t
    :models '(gpt-4o gpt-4o-mini gpt-4-turbo gpt-3.5-turbo)))

;; Load skills system (core, org-roam integration, transient UI)
(jf/load-module (expand-file-name "config/gptel/skills/skills-core.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/skills/skills-roam.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/skills/skills-transient.el" jf/emacs-dir))

(defun jf/gptel-preset--expand-skills (system-text)
  "Expand @skill mentions in SYSTEM-TEXT using gptel-skills.
Returns updated system text with skill content injected.
If skills system not loaded or no mentions found, returns text unchanged."
  (if (and (fboundp 'jf/gptel-skills--detect-mentions)
           (boundp 'jf/gptel-skills--registry)
           (string-match-p "@" system-text))
      (with-temp-buffer
        (insert system-text)
        (let* ((mentions-data (jf/gptel-skills--detect-mentions (current-buffer)))
               (skill-names (mapcar #'car mentions-data)))
          (when skill-names
            ;; For each unique skill, replace @mention with content
            (dolist (skill-name (delete-dups skill-names))
              (let* ((metadata (gethash skill-name jf/gptel-skills--registry))
                     (skill-path (plist-get metadata :path))
                     (skill-content (when skill-path
                                     (jf/gptel-skills--parse-content skill-path))))
                (when skill-content
                  (goto-char (point-min))
                  (while (re-search-forward
                          (concat "@" (regexp-quote skill-name) "\\>")
                          nil t)
                    (replace-match skill-content t t))))))
          (buffer-string)))
    ;; No skills system or no mentions - return unchanged
    system-text))

(defun jf/gptel-preset--expand-all-preset-skills ()
  "Expand @skill mentions in all registered preset system prompts.
Run this after preset registration to inject skill content into presets."
  (when (and (bound-and-true-p gptel--known-presets)
             (fboundp 'jf/gptel-skills--discover))
    ;; Ensure skills are discovered first
    (jf/gptel-skills--discover)

    ;; Process each preset
    (dolist (preset-entry gptel--known-presets)
      (let* ((preset-name (car preset-entry))
             (plist (cdr preset-entry))
             (system (plist-get plist :system)))
        (when (and system (stringp system))
          (let ((expanded (jf/gptel-preset--expand-skills system)))
            (unless (string= expanded system)
              (plist-put plist :system expanded)
              (when jf/gptel-skills-verbose
                (message "Expanded skills in preset: %s" preset-name)))))))))

;; Load constants and logging first (needed by preset registration and all session modules)
(jf/load-module (expand-file-name "config/gptel/sessions/constants.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/sessions/logging.el" jf/emacs-dir))

;; Ensure yaml parser is available
(require 'yaml)

;; Load preset registration module
(jf/load-module (expand-file-name "config/gptel/preset-registration.el" jf/emacs-dir))

;; Register all presets from config/gptel/presets/*.md
(jf/gptel-preset-register-all)

;; After preset registration, expand skills in preset system prompts
(jf/gptel-preset--expand-all-preset-skills)

;; Load scope-core (required by scope-controlled tools)
(jf/load-module (expand-file-name "config/gptel/scope/scope-core.el" jf/emacs-dir))

;; Load scope-profiles (used by session creation to write scope.yml)
(jf/load-module (expand-file-name "config/gptel/scope-profiles.el" jf/emacs-dir))

;; Load custom tools (tools must be registered before presets reference them)
(jf/load-module (expand-file-name "config/gptel/tools/filesystem-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/tools/projectile-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/tools/ggtags-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/tools/treesitter-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/tools/org-roam-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/tools/sql-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/tools/meta-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/tools/community-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/tools/transient-tools.el" jf/emacs-dir))
;; Note: persistent-agent.el loaded later after session modules

;; Note: constants.el and logging.el already loaded above (before preset registration)

;; Load filesystem utilities before other modules (registry and hooks depend on it)
(jf/load-module (expand-file-name "config/gptel/sessions/filesystem.el" jf/emacs-dir))

;;; Load session modules in dependency order
(jf/load-module (expand-file-name "config/gptel/sessions/registry.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/sessions/metadata.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/sessions/branching.el" jf/emacs-dir))

;; scope-core already loaded above (before tools); scope-commands requires it
;; Load scope-commands BEFORE session-commands (session-commands requires scope-commands)
(jf/load-module (expand-file-name "config/gptel/scope/scope-commands.el" jf/emacs-dir))

;; Load scope-expansion (UI for handling scope violations)
(jf/load-module (expand-file-name "config/gptel/scope/scope-expansion.el" jf/emacs-dir))

;; Load PersistentAgent tool (requires session modules to be loaded first)
(jf/load-module (expand-file-name "config/gptel/tools/persistent-agent.el" jf/emacs-dir))

;; Load user-facing commands
(jf/load-module (expand-file-name "config/gptel/sessions/commands.el" jf/emacs-dir))

;; Load remaining scope system modules (scope-core, scope-commands, scope-expansion already loaded above)
;; Scope-aware tools check approved patterns internally and return
;; structured errors to LLM when operations are outside scope
(jf/load-module (expand-file-name "config/gptel/scope/scope-filesystem-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/scope/scope-org-roam-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/tools/scope-shell-tools.el" jf/emacs-dir))

;; Load activities integration (optional - only if activities package is loaded)
;; Enables creating persistent gptel sessions as part of activity creation
(when (featurep 'activities)
  (jf/load-module (expand-file-name "config/gptel/sessions/activities-integration.el" jf/emacs-dir)))

(defun jf/gptel-launcher ()
  "Launch gptel session with a selected backend and model.
Prompts for display method, then backend:model selection using
completing-read with all available options from gptel's configuration."
  (interactive)
  ;; First, ask where to display the buffer
  (let* ((display-options '(("Current window" . current)
                            ("New tab" . tab)
                            ("Split window" . split)))
         (display-choice (completing-read "Where to open? "
                                          (mapcar #'car display-options)
                                          nil t nil nil "Current window"))
         (display-method (cdr (assoc display-choice display-options)))
         ;; Build models-alist similar to gptel--infix-provider
         (models-alist
          (cl-loop
           for (name . backend) in gptel--known-backends
           nconc (cl-loop for model in (gptel-backend-models backend)
                          collect (list (concat name ":" (gptel--model-name model))
                                        backend model))))
         (selected (completing-read "Select model: "
                                     (mapcar #'car models-alist)
                                     nil t))
         (choice (assoc selected models-alist))
         (backend (nth 1 choice))
         (model (nth 2 choice))
         (buffer-name (format "*gptel-%s*" (replace-regexp-in-string ":" "-" selected)))
         ;; Create buffer without displaying it (pass nil for interactivep)
         ;; Note: When interactivep is t, gptel calls (display-buffer) with
         ;; gptel-display-buffer-action, which would display the buffer before
         ;; we can control where it goes. By passing nil, we handle all display
         ;; logic ourselves based on the user's selection.
         (buffer (gptel buffer-name nil nil nil)))
    ;; Display buffer based on user's choice
    (pcase display-method
      ('split (pop-to-buffer buffer))
      ('tab (tab-bar-new-tab)
            (switch-to-buffer buffer))
      ('current (switch-to-buffer buffer)))
    ;; Set backend and model as buffer-local
    (with-current-buffer buffer
      (setq-local gptel-backend backend)
      (setq-local gptel-model model))))

;; Keybinding for gptel launcher
;; Direct access with <SPC> l
(with-eval-after-load 'gptel
  (evil-define-key 'normal 'global (kbd "<SPC> l") 'jf/gptel-launcher))
