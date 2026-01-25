;;; metadata.el --- GPTEL Session Metadata -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Session metadata persistence for gptel.
;; Reads metadata from scope-plan.yml and preset.md.
;;
;; - Session fields: session_id, created, type, parent_session_id, agent_type
;; - Preset fields: backend, model

;;; Code:

(require 'cl-lib)
(require 'yaml)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)

(defun jf/gptel--read-session-metadata (session-dir)
  "Read session metadata from scope-plan.yml in SESSION-DIR.
Returns plist with :session-id, :created, :updated, :type, :parent-session-id, :agent-type.
Returns nil if file doesn't exist or can't be parsed."
  (let ((plan-file (expand-file-name "scope-plan.yml" session-dir)))
    (when (file-exists-p plan-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents plan-file)
            (let* ((parsed (yaml-parse-string (buffer-string) :object-type 'plist))
                   ;; Convert snake_case to kebab-case keywords
                   (session-id (plist-get parsed :session_id))
                   (created (plist-get parsed :created))
                   (updated (plist-get parsed :updated))
                   (type (plist-get parsed :type))
                   (parent-id (plist-get parsed :parent_session_id))
                   (agent-type (plist-get parsed :agent_type)))
              (list :session-id session-id
                    :created created
                    :updated updated
                    :type type
                    :parent-session-id parent-id
                    :agent-type agent-type)))
        (error
         (jf/gptel--log 'error "Failed to parse scope-plan.yml in %s: %s"
                       session-dir (error-message-string err))
         nil)))))

(defun jf/gptel--read-preset-metadata (session-dir)
  "Read backend and model from preset.md YAML frontmatter in SESSION-DIR.
Returns plist with :backend and :model.
Returns nil if file doesn't exist or can't be parsed."
  (let ((preset-file (expand-file-name "preset.md" session-dir)))
    (when (file-exists-p preset-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents preset-file)
            (goto-char (point-min))
            ;; Look for YAML frontmatter (between --- markers)
            (when (re-search-forward "^---\n" nil t)
              (let ((yaml-start (point)))
                (when (re-search-forward "^---\n" nil t)
                  (let* ((yaml-end (match-beginning 0))
                         (yaml-content (buffer-substring yaml-start yaml-end))
                         (parsed (yaml-parse-string yaml-content :object-type 'plist))
                         (backend (plist-get parsed :backend))
                         (model (plist-get parsed :model)))
                    (list :backend backend
                          :model model))))))
        (error
         (jf/gptel--log 'error "Failed to parse preset.md in %s: %s"
                       session-dir (error-message-string err))
         nil)))))

(defun jf/gptel--is-subagent-session-p (session-dir)
  "Return t if session in SESSION-DIR is a subagent session.
Checks :type field in scope-plan.yml."
  (when-let ((metadata (jf/gptel--read-session-metadata session-dir)))
    (equal (plist-get metadata :type) "subagent")))

(defun jf/gptel--get-parent-session-id (session-dir)
  "Get parent session ID from SESSION-DIR's scope-plan.yml.
Returns nil if not a subagent or no parent specified."
  (when-let ((metadata (jf/gptel--read-session-metadata session-dir)))
    (plist-get metadata :parent-session-id)))

(provide 'gptel-session-metadata)
;;; metadata.el ends here
