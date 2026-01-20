;;; metadata.el --- GPTEL Session Metadata -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Session metadata persistence for gptel.
;; Reads and writes metadata.json files containing session configuration.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)

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
Uses json.el to serialize the plist."
  (let ((json-encoding-pretty-print t))
    (json-encode
     (list :session_id (plist-get metadata :session-id)
           :created (plist-get metadata :created)
           :modified (plist-get metadata :modified)
           :backend (plist-get metadata :backend)
           :model (plist-get metadata :model)
           :tree (plist-get metadata :tree)))))

(defun jf/gptel--metadata-from-json (json-string)
  "Parse JSON-STRING into metadata plist.
Converts from snake_case JSON keys to kebab-case keywords."
  (let ((json-data (json-read-from-string json-string)))
    (list :session-id (alist-get 'session_id json-data)
          :created (alist-get 'created json-data)
          :modified (alist-get 'modified json-data)
          :backend (alist-get 'backend json-data)
          :model (alist-get 'model json-data)
          :tree (alist-get 'tree json-data))))

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

(provide 'gptel-session-metadata)
;;; metadata.el ends here
