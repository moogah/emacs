;;; activities-integration.el --- GPTEL Sessions Activities Integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;; This file integrates persistent gptel sessions with activities-extensions.

;;; Code:

(require 'gptel nil t)
(require 'jf-gptel-registry nil t)
(require 'jf-gptel-scope-core nil t)

;; Optional dependency - checked at runtime
(defvar activities-ext--slugify)

(defgroup jf-gptel-activities nil
  "Integration between gptel sessions and activities."
  :group 'gptel
  :prefix "jf/gptel-activities-")

(defcustom jf/gptel-activities-auto-open-on-resume t
  "Automatically open gptel session buffer when resuming activity."
  :type 'boolean
  :group 'jf-gptel-activities)

(defcustom jf/gptel-activities-create-scope-plan t
  "Create default deny-all scope plan for activity sessions."
  :type 'boolean
  :group 'jf-gptel-activities)

(defun jf/gptel-session--create-session-file (file-path activity-name backend model session-id)
  "Create session.org file at FILE-PATH with proper headers.
ACTIVITY-NAME is the human-readable activity name.
BACKEND and MODEL are the gptel backend and model names.
SESSION-ID is the unique session identifier."
  (with-temp-file file-path
    (insert (format "#+title: Activity: %s\n" activity-name))
    (insert "#+filetags: :gptel:activity:\n")
    (insert (format "#+property: session_id %s\n" session-id))
    (insert (format "#+property: activity_name %s\n"
                    (if (fboundp 'activities-ext--slugify)
                        (activities-ext--slugify activity-name)
                      activity-name)))
    (insert "\n")
    (insert "* Session Info\n")
    (insert (format "- Backend: %s\n" backend))
    (insert (format "- Model: %s\n" model))
    (insert (format "- Created: %s\n" (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
    (insert "\n")
    (insert "* Notes\n")
    (insert "Use this file for session notes, context, and planning.\n")
    (insert "The conversation is stored in metadata.json.\n")
    (insert "\n")
    (insert "* Tasks\n")
    (insert "** TODO Document activity goals\n")))

(defun jf/gptel-scope--create-default-plan (session-id)
  "Create default deny-all scope plan for SESSION-ID.
Only creates plan if jf/gptel-activities-create-scope-plan is non-nil."
  (when (and jf/gptel-activities-create-scope-plan
             (fboundp 'jf/gptel-session-find))
    (when-let* ((session (jf/gptel-session-find session-id))
                (session-dir (plist-get session :directory)))
      (let ((plan-file (expand-file-name "scope-plan.yml" session-dir))
            (plan-content (jf/gptel-scope--template-deny-all session-id)))
        (with-temp-file plan-file
          (insert plan-content))
        (jf/gptel--log 'info "Created default scope plan: %s" plan-file)))))

(defun jf/gptel-scope--template-deny-all (session-id)
  "Generate deny-all scope plan template for SESSION-ID."
  (format "# Scope Plan for Session: %s
# Created: %s
#
# This is a deny-all scope plan. The LLM agent cannot access any
# files, directories, or tools by default. To grant access:
#
# 1. Use the request_scope_expansion tool in the chat
# 2. Or manually edit this file to add allowed_paths/allowed_tools

version: 1
session_id: %s
default_policy: deny

# Example allowed_paths:
# allowed_paths:
#   - path: \"/path/to/project\"
#     mode: read-write
#     description: \"Project source code\"

# Example allowed_tools:
# allowed_tools:
#   - name: \"bash\"
#     description: \"Shell command execution\"

allowed_paths: []
allowed_tools: []

# Denied paths (explicit denials take precedence)
denied_paths:
  - path: \"/\"
    description: \"Deny all filesystem access by default\"
"
          session-id
          (format-time-string "%Y-%m-%dT%H:%M:%SZ")
          session-id))

(defun jf/gptel-session-create-persistent (activity-name &optional backend model)
  "Create persistent gptel session immediately for ACTIVITY-NAME.

Unlike lazy initialization, this creates all resources upfront:
- Session directory (~/gptel-sessions/ACTIVITY-NAME-TIMESTAMP/)
- metadata.json with initial tree
- scope-plan.yml with deny-all default
- session.org file for buffer to visit
- Registers in global registry

BACKEND and MODEL default to current gptel-backend and gptel-model.

Returns plist: (:session-id ... :session-dir ... :buffer-name ... :session-file ...)"
  (unless (and (fboundp 'jf/gptel--create-metadata)
               (fboundp 'jf/gptel--write-metadata)
               (fboundp 'jf/gptel--register-session))
    (error "GPTEL session registry not available"))

  (let* ((slug (if (fboundp 'activities-ext--slugify)
                   (activities-ext--slugify activity-name)
                 (replace-regexp-in-string "[^a-z0-9-]" "-"
                                          (downcase activity-name))))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (dirname (format "%s-%s" slug timestamp))
         (session-dir (expand-file-name dirname
                                       (expand-file-name jf/gptel-sessions-directory)))
         (session-file (expand-file-name "session.org" session-dir))
         (backend (or backend gptel-backend))
         (model (or model gptel-model))
         (backend-name (gptel-backend-name backend)))

    ;; Create session directory
    (make-directory session-dir t)
    (jf/gptel--log 'info "Created session directory: %s" session-dir)

    ;; Create metadata.json
    (let ((metadata (jf/gptel--create-metadata session-dir dirname model backend-name)))
      (jf/gptel--write-metadata session-dir metadata)

      ;; Register in global registry
      (let ((session-id (jf/gptel--register-session session-dir metadata (current-buffer))))
        (jf/gptel--log 'info "Registered session: %s" session-id)

        ;; Create deny-all scope plan
        (jf/gptel-scope--create-default-plan session-id)

        ;; Create session.org file
        (jf/gptel-session--create-session-file session-file activity-name
                                               backend-name model session-id)
        (jf/gptel--log 'info "Created session file: %s" session-file)

        ;; Return session info
        (list :session-id session-id
              :session-dir session-dir
              :buffer-name (format "*gptel-%s*" slug)
              :session-file session-file
              :backend backend-name
              :model (if (symbolp model) (symbol-name model) model))))))

(defun jf/gptel-session--create-buffer (session-info)
  "Create gptel buffer associated with session file.
SESSION-INFO is plist from jf/gptel-session-create-persistent."
  (let* ((buffer-name (plist-get session-info :buffer-name))
         (session-file (plist-get session-info :session-file))
         (session-id (plist-get session-info :session-id))
         (backend-name (plist-get session-info :backend))
         (model-name (plist-get session-info :model))
         (model (if (stringp model-name) (intern model-name) model-name)))

    ;; Open session.org file in buffer
    (let ((buffer (find-file-noselect session-file)))
      (with-current-buffer buffer
        ;; Rename buffer to expected name
        (rename-buffer buffer-name t)

        ;; Enable gptel-mode if not already active
        (unless gptel-mode
          (gptel-mode 1))

        ;; Set buffer-local variables
        (setq-local jf/gptel--session-id session-id)
        (setq-local jf/gptel--session-dir (plist-get session-info :session-dir))
        (setq-local gptel-backend (alist-get backend-name gptel--known-backends
                                             nil nil #'equal))
        (setq-local gptel-model model)

        ;; Force autosave enabled for this buffer
        (setq-local jf/gptel-autosave-enabled t)

        (jf/gptel--log 'info "Created buffer: %s (session: %s)" buffer-name session-id)
        buffer))))

(defun jf/gptel-session--open-existing (session-file)
  "Open existing gptel session from SESSION-FILE.
Restores session-id and other state from file properties.
Returns the buffer visiting the session file."
  (unless (file-exists-p session-file)
    (error "Session file does not exist: %s" session-file))

  (let ((buffer (find-file-noselect session-file)))
    (with-current-buffer buffer
      ;; Parse org properties to get session-id
      (org-with-point-at (point-min)
        (let ((session-id (org-entry-get nil "session_id")))
          (if (not session-id)
              (progn
                (jf/gptel--log 'warn "No session_id found in %s" session-file)
                buffer)

            ;; Find session in registry or disk
            (if-let ((session (and (fboundp 'jf/gptel-session-find)
                                  (jf/gptel-session-find session-id))))
                (progn
                  ;; Set buffer-local variables
                  (setq-local jf/gptel--session-id session-id)
                  (setq-local jf/gptel--session-dir (plist-get session :directory))

                  ;; Enable gptel-mode if not already
                  (unless gptel-mode
                    (gptel-mode 1))

                  ;; Force autosave
                  (setq-local jf/gptel-autosave-enabled t)

                  ;; Restore backend/model from metadata
                  (let ((metadata (plist-get session :metadata)))
                    (when-let ((backend-name (plist-get metadata :backend)))
                      (setq-local gptel-backend (alist-get backend-name gptel--known-backends
                                                           nil nil #'equal)))
                    (when-let ((model-name (plist-get metadata :model)))
                      (setq-local gptel-model (if (stringp model-name)
                                                 (intern model-name)
                                               model-name))))

                  (jf/gptel--log 'info "Opened existing session: %s" session-id)
                  buffer)

              ;; Session not found in registry
              (jf/gptel--log 'warn "Session %s not found in registry" session-id)
              buffer)))))))

(provide 'jf-gptel-activities-integration)
;;; activities-integration.el ends here
