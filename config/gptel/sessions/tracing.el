;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun jf/gptel--create-trace (session-id agent-type description prompt &optional parent-trace-id)
  "Create new agent trace entry in session.
Looks up session from registry, updates metadata, returns trace-id."
  (when-let* ((session-data (jf/gptel--get-session-data session-id))
              (metadata (plist-get session-data :metadata)))
    (let* ((trace-counter (plist-get session-data :trace-counter))
           (trace-id (format "%s%d" jf/gptel-session-node-prefix-trace (cl-incf trace-counter)))
           (trace-stack (plist-get session-data :trace-stack))
           (depth (length trace-stack))
           (trace (list :trace_id trace-id
                       :agent_type agent-type
                       :description description
                       :prompt_preview (substring prompt 0 (min 200 (length prompt)))
                       :timestamp_start (format-time-string "%Y-%m-%dT%H:%M:%SZ")
                       :timestamp_end nil
                       :status "running"
                       :parent_trace_id parent-trace-id
                       :depth depth
                       :tool_calls []
                       :associated_nodes nil)))
      ;; Update registry: increment counter, push to stack
      (jf/gptel--update-session-data session-id :trace-counter trace-counter)
      (jf/gptel--update-session-data session-id :trace-stack (cons trace-id trace-stack))

      ;; Add to metadata's agent_traces array
      (let ((traces (plist-get metadata :agent_traces)))
        (plist-put metadata :agent_traces
                   (vconcat traces (vector trace))))

      ;; Write metadata to disk (incremental)
      (let ((session-dir (plist-get session-data :directory)))
        (jf/gptel--write-metadata session-dir metadata))

      trace-id)))

(defun jf/gptel--complete-trace (session-id trace-id status)
  "Mark trace as completed, pop from stack."
  (when-let* ((session-data (jf/gptel--get-session-data session-id))
              (metadata (plist-get session-data :metadata))
              (traces (plist-get metadata :agent_traces)))
    ;; Find and update the trace
    (let ((trace (cl-find-if (lambda (tr)
                              (equal (plist-get tr :trace_id) trace-id))
                            (append traces nil))))
      (when trace
        (plist-put trace :timestamp_end (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (plist-put trace :status status)

        ;; Pop from trace stack in registry
        (let ((trace-stack (plist-get session-data :trace-stack)))
          (jf/gptel--update-session-data session-id :trace-stack
                                        (remove trace-id trace-stack)))

        ;; Write metadata to disk
        (let ((session-dir (plist-get session-data :directory)))
          (jf/gptel--write-metadata session-dir metadata))))))

(defun jf/gptel--current-trace-id (session-id)
  "Get current trace ID (top of stack) from session."
  (when-let ((session-data (jf/gptel--get-session-data session-id)))
    (car (plist-get session-data :trace-stack))))

(defun jf/gptel--generate-tool-use-id (tool-name)
  "Generate a tool use ID in Claude API format.
Returns format: toolu_TIMESTAMP_RANDOM."
  (format "toolu_%s_%04x"
          (format-time-string "%Y%m%d%H%M%S")
          (random 65536)))

(defun jf/gptel--record-complete-tool-call (session-id trace-id tool-call-plist)
  "Record a complete tool call with ID, name, args, and result.
TOOL-CALL-PLIST has :id, :name, :args, and :result from gptel.
If TRACE-ID is nil, records to session-level. Otherwise records to trace."
  (let* ((tool-id (plist-get tool-call-plist :id))
         (tool-name (plist-get tool-call-plist :name))
         (args (plist-get tool-call-plist :args))
         (result (plist-get tool-call-plist :result))
         (args-str (prin1-to-string args))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
         ;; Convert result to string
         (result-str (cond
                      ((stringp result) result)
                      ((listp result) (format "%S" result))
                      (t (format "%s" result))))
         ;; Create tool call entry
         (tool-call (list :tool_use_id tool-id
                         :tool_name tool-name
                         :timestamp timestamp
                         :args_str args-str
                         :result_preview nil
                         :result_file nil
                         :error nil)))

    ;; Decide storage strategy for result
    (if (< (length result-str) 1024)
        ;; Small result: store inline
        (plist-put tool-call :result_preview
                  (jf/gptel--preview-string result-str 500))
      ;; Large result: save to file
      (when-let* ((session-data (jf/gptel--get-session-data session-id))
                  (session-dir (plist-get session-data :directory)))
        (let* ((results-dir (if trace-id
                               (expand-file-name
                                (format "%s/%s/%s"
                                        jf/gptel-session-traces-dirname
                                        trace-id
                                        jf/gptel-session-trace-tool-results-dirname)
                                session-dir)
                             (expand-file-name jf/gptel-session-trace-tool-results-dirname session-dir)))
               (result-file (format "%s.txt" tool-id))
               (result-path (expand-file-name result-file results-dir)))
          (make-directory results-dir t)
          (with-temp-file result-path
            (insert result-str))
          (plist-put tool-call :result_preview
                    (jf/gptel--preview-string result-str 200))
          (plist-put tool-call :result_file result-file))))

    ;; Store in appropriate location
    (when-let* ((session-data (jf/gptel--get-session-data session-id))
                (metadata (plist-get session-data :metadata)))
      (if trace-id
          ;; Store in trace
          (when-let* ((traces (plist-get metadata :agent_traces))
                      (trace (cl-find-if (lambda (tr)
                                          (equal (plist-get tr :trace_id) trace-id))
                                        (append traces nil))))
            (let ((calls (plist-get trace :tool_calls)))
              (plist-put trace :tool_calls (vconcat calls (vector tool-call)))))
        ;; Store in session
        (let ((calls (or (plist-get metadata :session_tool_calls) [])))
          (plist-put metadata :session_tool_calls (vconcat calls (vector tool-call)))))

      ;; Write session metadata immediately
      (let ((session-dir (plist-get session-data :directory)))
        (jf/gptel--write-metadata session-dir metadata))

      ;; Also update trace directory metadata if this is a trace
      (when trace-id
        (when-let* ((session-dir (plist-get session-data :directory))
                    (trace-dir (expand-file-name (format "%s/%s" jf/gptel-session-traces-dirname trace-id) session-dir))
                    (metadata-file (expand-file-name jf/gptel-session-trace-metadata-filename trace-dir)))
          (let* ((trace-metadata (if (file-exists-p metadata-file)
                                    (with-temp-buffer
                                      (insert-file-contents metadata-file)
                                      (goto-char (point-min))
                                      (json-read))
                                  ;; Initial structure as alist (compatible with json-read output)
                                  (list (cons 'trace_id trace-id)
                                        (cons 'messages [])
                                        (cons 'tool_calls [])
                                        (cons 'total_tokens 0))))
                 ;; Use alist-get since json-read returns alist
                 (tool-calls (alist-get 'tool_calls trace-metadata))
                 ;; Convert tool-call plist to alist for JSON encoding
                 (tool-call-alist
                  (list (cons 'tool_use_id (plist-get tool-call :tool_use_id))
                        (cons 'tool_name (plist-get tool-call :tool_name))
                        (cons 'timestamp (plist-get tool-call :timestamp))
                        (cons 'args_str (plist-get tool-call :args_str))
                        (cons 'result_preview (plist-get tool-call :result_preview))
                        (cons 'result_file (plist-get tool-call :result_file))
                        (cons 'error (plist-get tool-call :error)))))
            ;; Update tool_calls in alist using setf
            (setf (alist-get 'tool_calls trace-metadata)
                  (vconcat tool-calls (vector tool-call-alist)))
            ;; Write updated trace metadata
            (make-directory trace-dir t)
            (with-temp-file metadata-file
              (let ((json-encoding-pretty-print t))
                (insert (json-encode trace-metadata))))))))))

(defun jf/gptel--preview-string (obj &optional max-len)
  "Create preview string from OBJ (string, list, etc)."
  (let* ((max-len (or max-len 200))
         (str (cond
               ((stringp obj) obj)
               ((listp obj) (format "%S" obj))
               (t (format "%s" obj)))))
    (if (> (length str) max-len)
        (concat (substring str 0 max-len) "...")
      str)))

(defun jf/gptel--save-trace-message (session-id trace-id message-type content &optional metadata)
  "Save a message to trace directory.
MESSAGE-TYPE is 'prompt', 'message', 'response', or 'result'.
CONTENT is the message text.
METADATA is optional plist with :timestamp, :tokens, :tool_calls, etc."
  (when-let* ((session-data (jf/gptel--get-session-data session-id))
              (session-dir (plist-get session-data :directory)))
    (let* ((trace-dir (expand-file-name (format "%s/%s" jf/gptel-session-traces-dirname trace-id) session-dir))
           (metadata-file (expand-file-name jf/gptel-session-trace-metadata-filename trace-dir))
           ;; Count existing message files to get next ID
           (message-files (when (file-directory-p trace-dir)
                           (directory-files trace-dir nil "^\\(message\\|response\\)-[0-9]+\\.txt$")))
           (next-id (if message-files (length message-files) 0))
           (file-name (pcase message-type
                       ('prompt "prompt.txt")
                       ('result "result.txt")
                       ('message (format "message-%d.txt" next-id))
                       ('response (format "response-%d.txt" next-id))))
           (file-path (expand-file-name file-name trace-dir)))

      ;; Create trace directory if needed
      (make-directory trace-dir t)

      ;; Save message content
      (with-temp-file file-path
        (insert content))

      ;; Update trace metadata
      (let* ((trace-metadata (if (file-exists-p metadata-file)
                                (with-temp-buffer
                                  (insert-file-contents metadata-file)
                                  (let ((json-object-type 'plist)
                                        (json-array-type 'vector)
                                        (json-key-type 'keyword))
                                    (json-read)))
                              ;; Initialize if doesn't exist
                              (list :trace_id trace-id
                                    :messages []
                                    :tool_calls []
                                    :total_tokens 0)))
             (messages (plist-get trace-metadata :messages))
             (new-message (list :id file-name
                               :file file-name
                               :timestamp (or (plist-get metadata :timestamp)
                                            (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
                               :tokens (or (plist-get metadata :tokens) 0))))
        ;; Add optional fields
        (when (plist-get metadata :tool_calls)
          (plist-put new-message :tool_calls (plist-get metadata :tool_calls)))

        ;; Append to messages array
        (plist-put trace-metadata :messages
                   (vconcat messages (vector new-message)))

        ;; Update total tokens
        (plist-put trace-metadata :total_tokens
                   (+ (or (plist-get trace-metadata :total_tokens) 0)
                      (or (plist-get metadata :tokens) 0)))

        ;; Write updated metadata
        (with-temp-file metadata-file
          (let ((json-encoding-pretty-print t))
            (insert (json-encode trace-metadata))))))))
