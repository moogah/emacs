;;; subagent.el --- GPTEL Subagent Session Helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Helper functions for subagent session management.
;; Subagent creation is now handled by the VisibleAgent tool.

;;; Code:

(require 'cl-lib)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-metadata)

(defun jf/gptel--link-subagent-to-parent (subagent-dir parent-session-id)
  "Link SUBAGENT-DIR to parent session PARENT-SESSION-ID.
Updates parent session metadata to include subagent reference."
  (when-let* ((parent-session (jf/gptel-session-find parent-session-id))
              (parent-dir (plist-get parent-session :directory))
              (parent-metadata-file (jf/gptel--metadata-file-path parent-dir)))
    (when (file-exists-p parent-metadata-file)
      ;; Read existing parent metadata
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (metadata (json-read-file parent-metadata-file))
             (subagents (alist-get 'subagents metadata))
             (subagent-id (jf/gptel--session-id-from-directory subagent-dir)))

        ;; Add subagent to list
        (push subagent-id subagents)
        (setq metadata (cons (cons 'subagents subagents)
                            (assq-delete-all 'subagents metadata)))

        ;; Write back to file
        (with-temp-file parent-metadata-file
          (insert (json-encode metadata)))

        (jf/gptel--log 'info "Linked subagent %s to parent %s"
                      subagent-id parent-session-id)))))

(defun jf/gptel--session-id-from-directory (session-dir)
  "Extract session ID from SESSION-DIR path.
Returns the directory name (last path component)."
  (file-name-nondirectory (directory-file-name session-dir)))

(provide 'gptel-session-subagent)
;;; subagent.el ends here
