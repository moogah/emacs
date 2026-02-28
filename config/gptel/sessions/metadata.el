;;; metadata.el --- GPTEL Session Metadata -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Session metadata persistence for gptel.
;; Primary source: metadata.yml (session_id, created, type, parent_session_id, preset)

;;; Code:

(require 'cl-lib)
(require 'yaml)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)

(defun jf/gptel--read-session-metadata (session-dir)
  "Read session metadata from metadata.yml in SESSION-DIR.
Returns plist with :session-id, :created, :updated, :type, :parent-session-id, :preset.
Returns nil if metadata.yml does not exist or can't be parsed."
  (let ((metadata-file (expand-file-name jf/gptel-session--metadata-file session-dir)))
    (when (file-exists-p metadata-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents metadata-file)
            (let* ((parsed (yaml-parse-string (buffer-string) :object-type 'plist))
                   ;; Convert snake_case to kebab-case keywords
                   (session-id (plist-get parsed :session_id))
                   (created (plist-get parsed :created))
                   (updated (plist-get parsed :updated))
                   (type (plist-get parsed :type))
                   (parent-id (plist-get parsed :parent_session_id))
                   (preset (plist-get parsed :preset)))
              (list :session-id session-id
                    :created created
                    :updated updated
                    :type type
                    :parent-session-id parent-id
                    :preset preset)))
        (error
         (jf/gptel--log 'error "Failed to parse session metadata in %s: %s"
                       session-dir (error-message-string err))
         nil)))))

(defun jf/gptel--is-agent-session-p (session-dir)
  "Return t if session in SESSION-DIR is an agent session.
Checks :type field in metadata.yml."
  (when-let ((metadata (jf/gptel--read-session-metadata session-dir)))
    (equal (plist-get metadata :type) "agent")))

(defun jf/gptel--is-branch-session-p (session-dir)
  "Return t if session in SESSION-DIR is a branch session.
Checks :type field in metadata.yml."
  (when-let ((metadata (jf/gptel--read-session-metadata session-dir)))
    (equal (plist-get metadata :type) "branch")))

(defun jf/gptel--get-parent-session-id (session-dir)
  "Get parent session ID from SESSION-DIR's metadata.yml.
Returns nil if not an agent/branch or no parent specified."
  (when-let ((metadata (jf/gptel--read-session-metadata session-dir)))
    (plist-get metadata :parent-session-id)))

(defun jf/gptel--read-branch-metadata (branch-dir)
  "Read branch metadata from branch-metadata.yml in BRANCH-DIR.
Returns plist with :parent-branch, :created, :branch-point-position.
Returns nil if file doesn't exist or can't be parsed."
  (let ((metadata-file (jf/gptel--branch-metadata-file-path branch-dir)))
    (when (file-exists-p metadata-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents metadata-file)
            (let* ((parsed (yaml-parse-string (buffer-string) :object-type 'plist))
                   (parent-branch (plist-get parsed :parent_branch))
                   (created (plist-get parsed :created))
                   (branch-point (plist-get parsed :branch_point_position)))
              (list :parent-branch parent-branch
                    :created created
                    :branch-point-position branch-point)))
        (error
         (jf/gptel--log 'error "Failed to parse branch-metadata.yml in %s: %s"
                       branch-dir (error-message-string err))
         nil)))))

(defun jf/gptel--write-branch-metadata (branch-dir parent-branch-name &optional branch-point-position)
  "Write branch metadata to BRANCH-DIR.
PARENT-BRANCH-NAME is the name of the parent branch (e.g., \"main\").
BRANCH-POINT-POSITION is optional position in parent where branch was created."
  (let ((metadata-file (jf/gptel--branch-metadata-file-path branch-dir))
        (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    (with-temp-file metadata-file
      (insert "parent_branch: " parent-branch-name "\n")
      (insert "created: " timestamp "\n")
      (when branch-point-position
        (insert "branch_point_position: " (number-to-string branch-point-position) "\n")))
    (jf/gptel--log 'info "Created branch-metadata.yml with parent: %s" parent-branch-name)))

(provide 'gptel-session-metadata)
;;; metadata.el ends here
