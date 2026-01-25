;;; metadata.el --- GPTEL Session Metadata -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Session metadata persistence for gptel.
;;
;; NEW (Phase 1): Reads metadata from scope-plan.yml and preset.md
;; - Session fields: session_id, created, type, parent_session_id, agent_type
;; - Preset fields: backend, model
;;
;; OLD (deprecated): metadata.json functions preserved for backward compatibility
;; Will be removed in Phase 4 after complete migration.

;;; Code:

(require 'cl-lib)
(require 'json)
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

(defun jf/gptel--create-metadata (session-dir session-id model backend)
  "Create initial metadata plist for new session.
SESSION-DIR is the session directory path.
SESSION-ID is the unique session identifier.
MODEL is the gptel model name or symbol.
BACKEND is the backend name string."
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
    (list :session-id session-id
          :created timestamp
          :modified timestamp
          :backend backend
          :model (if (symbolp model) (symbol-name model) model)
          :tree (list :root "msg-1"
                     :current "msg-1"
                     :nodes (list (list :id "msg-1"
                                       :type "user"
                                       :parent nil
                                       :children nil))))))

(defun jf/gptel--metadata-to-json (metadata)
  "Convert METADATA plist to JSON string.
Uses json.el to serialize the plist. Handles any plist fields flexibly."
  (let ((json-encoding-pretty-print t)
        (alist nil))
    ;; Convert plist to alist, transforming kebab-case keywords to snake_case strings
    (cl-loop for (key val) on metadata by #'cddr
             do (push (cons (intern (replace-regexp-in-string "-" "_"
                                     (substring (symbol-name key) 1)))
                           val)
                     alist))
    (json-encode (nreverse alist))))

(defun jf/gptel--metadata-from-json (json-string)
  "Parse JSON-STRING into metadata plist.
Converts from snake_case JSON keys to kebab-case keywords.
Handles any JSON fields flexibly."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-data (json-read-from-string json-string))
         (plist nil))
    ;; Convert alist to plist, transforming snake_case to kebab-case keywords
    (dolist (pair json-data)
      (let* ((key-str (symbol-name (car pair)))
             (key-keyword (intern (concat ":"
                                   (replace-regexp-in-string "_" "-" key-str))))
             (val (cdr pair)))
        (setq plist (plist-put plist key-keyword val))))
    plist))

(defun jf/gptel--write-metadata (session-dir metadata)
  "Write METADATA to metadata.json in SESSION-DIR.
Overwrites existing file if present."
  (let* ((metadata-file (jf/gptel--metadata-file-path session-dir))
         (json-content (jf/gptel--metadata-to-json metadata)))
    (with-temp-file metadata-file
      (insert json-content))
    (jf/gptel--log 'debug "Wrote metadata: %s" metadata-file)
    metadata))

(defun jf/gptel--read-metadata-file (metadata-file)
  "Read metadata from METADATA-FILE.
Returns metadata plist or nil if file doesn't exist or can't be parsed."
  (when (file-exists-p metadata-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents metadata-file)
          (jf/gptel--metadata-from-json (buffer-string)))
      (error
       (jf/gptel--log 'error "Failed to parse metadata file %s: %s"
                     metadata-file (error-message-string err))
       nil))))

(defun jf/gptel--read-metadata (session-dir)
  "Read metadata from SESSION-DIR.
Convenience wrapper around jf/gptel--read-metadata-file."
  (let ((metadata-file (jf/gptel--metadata-file-path session-dir)))
    (jf/gptel--read-metadata-file metadata-file)))

(defun jf/gptel--update-metadata-modified (session-dir)
  "Update the :modified timestamp in SESSION-DIR metadata."
  (when-let ((metadata (jf/gptel--read-metadata session-dir)))
    (plist-put metadata :modified
              (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
    (jf/gptel--write-metadata session-dir metadata)))

(defun jf/gptel--update-metadata-tree (session-dir tree)
  "Update the conversation tree in SESSION-DIR metadata.
TREE is the new tree structure plist."
  (when-let ((metadata (jf/gptel--read-metadata session-dir)))
    (plist-put metadata :tree tree)
    (plist-put metadata :modified
              (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
    (jf/gptel--write-metadata session-dir metadata)))

(defun jf/gptel--load-metadata-from-file (session-dir)
  "Load metadata from SESSION-DIR/metadata.json.
Convenience alias for jf/gptel--read-metadata."
  (jf/gptel--read-metadata session-dir))

(defun jf/gptel--metadata-is-subagent-p (metadata)
  "Return t if METADATA represents a subagent session."
  (and metadata
       (or (equal (plist-get metadata :type) "subagent")
           (plist-get metadata :parent-session-id))))

(defun jf/gptel--metadata-get-parent-id (metadata)
  "Get parent session ID from METADATA, or nil if not a subagent."
  (plist-get metadata :parent-session-id))

(provide 'gptel-session-metadata)
;;; metadata.el ends here
