;;; filesystem.el --- GPTEL Session Filesystem Utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Filesystem utilities for gptel session management.
;; Handles directory creation, file path resolution, and session discovery.

;;; Code:

(require 'cl-lib)
(require 'jf-gptel-session-constants)
(require 'jf-gptel-session-logging)

(defun jf/gptel--ensure-sessions-root ()
  "Ensure the root sessions directory exists.
Creates jf/gptel-sessions-directory if it doesn't exist.
Returns the absolute path to the sessions directory."
  (let ((dir (expand-file-name jf/gptel-sessions-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (jf/gptel--log 'info "Created sessions directory: %s" dir))
    dir))

(defun jf/gptel--create-session-directory (session-id)
  "Create directory for SESSION-ID.
Returns the absolute path to the created directory."
  (let* ((root (jf/gptel--ensure-sessions-root))
         (session-dir (expand-file-name session-id root)))
    (if (file-directory-p session-dir)
        (progn
          (jf/gptel--log 'warn "Session directory already exists: %s" session-dir)
          session-dir)
      (make-directory session-dir t)
      (jf/gptel--log 'info "Created session directory: %s" session-dir)
      session-dir)))

(defun jf/gptel--generate-session-id (base-name)
  "Generate unique session ID from BASE-NAME.
Format: <slugified-base-name>-<timestamp>
Example: 'react-refactoring-20260120153042'"
  (let* ((slug (replace-regexp-in-string "[^a-z0-9-]" "-"
                                        (downcase base-name)))
         (timestamp (format-time-string "%Y%m%d%H%M%S")))
    (format "%s-%s" slug timestamp)))

(defun jf/gptel--session-id-from-directory (session-dir)
  "Extract session ID from SESSION-DIR path.
Returns the directory name (last path component)."
  (file-name-nondirectory (directory-file-name session-dir)))

(defun jf/gptel--session-file-path (session-dir filename)
  "Get absolute path to FILENAME in SESSION-DIR.
Does not check if file exists."
  (expand-file-name filename session-dir))

(defun jf/gptel--context-file-path (session-dir)
  "Get path to context file in SESSION-DIR."
  (jf/gptel--session-file-path session-dir jf/gptel-session--context-file))

(defun jf/gptel--metadata-file-path (session-dir)
  "Get path to metadata file in SESSION-DIR."
  (jf/gptel--session-file-path session-dir jf/gptel-session--metadata-file))

(defun jf/gptel--tools-log-path (session-dir)
  "Get path to tools log file in SESSION-DIR."
  (jf/gptel--session-file-path session-dir jf/gptel-session--tools-log-file))

(defun jf/gptel--system-prompts-log-path (session-dir)
  "Get path to system prompts log file in SESSION-DIR."
  (jf/gptel--session-file-path session-dir jf/gptel-session--system-prompts-file))

(defun jf/gptel--subagents-dir-path (session-dir)
  "Get path to subagents directory in SESSION-DIR."
  (jf/gptel--session-file-path session-dir jf/gptel-session--subagents-dir))

(defun jf/gptel--list-session-directories ()
  "List all session directories in jf/gptel-sessions-directory.
Returns list of absolute paths to session directories."
  (let ((root (expand-file-name jf/gptel-sessions-directory)))
    (when (file-directory-p root)
      (seq-filter
       (lambda (path)
         (and (file-directory-p path)
              (not (string-prefix-p "." (file-name-nondirectory path)))))
       (directory-files root t "^[^.]")))))

(defun jf/gptel--find-session-directory (session-id)
  "Find session directory for SESSION-ID.
Returns absolute path if found, nil otherwise."
  (let ((root (expand-file-name jf/gptel-sessions-directory))
        (expected (expand-file-name session-id
                                   (expand-file-name jf/gptel-sessions-directory))))
    (when (file-directory-p expected)
      expected)))

(defun jf/gptel--create-subagent-directory (parent-session-dir agent-type description)
  "Create subagent directory under PARENT-SESSION-DIR.
AGENT-TYPE is the subagent type (e.g., 'researcher', 'executor').
DESCRIPTION is a brief description for the directory name.
Returns the absolute path to the created subagent directory."
  (let* ((subagents-dir (jf/gptel--subagents-dir-path parent-session-dir))
         (slug (replace-regexp-in-string "[^a-z0-9-]" "-"
                                        (downcase description)))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (dirname (format "%s-%s-%s" agent-type timestamp slug))
         (subagent-dir (expand-file-name dirname subagents-dir)))
    ;; Create subagents directory if needed
    (unless (file-directory-p subagents-dir)
      (make-directory subagents-dir t))
    ;; Create subagent session directory
    (make-directory subagent-dir t)
    (jf/gptel--log 'info "Created subagent directory: %s" subagent-dir)
    subagent-dir))

(defun jf/gptel--list-subagent-directories (parent-session-dir)
  "List all subagent directories under PARENT-SESSION-DIR.
Returns list of absolute paths."
  (let ((subagents-dir (jf/gptel--subagents-dir-path parent-session-dir)))
    (when (file-directory-p subagents-dir)
      (seq-filter
       #'file-directory-p
       (directory-files subagents-dir t "^[^.]")))))

(defun jf/gptel--valid-session-directory-p (dir)
  "Return t if DIR is a valid session directory.
Checks for existence and presence of metadata file."
  (and (file-directory-p dir)
       (file-exists-p (jf/gptel--metadata-file-path dir))))

(provide 'jf-gptel-session-filesystem)
;;; filesystem.el ends here
