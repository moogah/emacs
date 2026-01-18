;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun jf/gptel--trace-agent-task (orig-fn main-cb agent-type description prompt)
  "Advice around `gptel-agent--task' to capture execution trace.
Session-id is passed via :context (as overlay property or plist), records agent lifecycle."
  ;; Get session-id from parent buffer (buffer-local variable)
  (let ((session-id jf/gptel--session-id))
    (message "DEBUG trace-agent-task: session-id=%s, agent-type=%s" session-id agent-type)
    (if (not session-id)
        ;; No active session, run agent without tracing
        (progn
          (message "DEBUG trace-agent-task: No session-id, skipping trace")
          (funcall orig-fn main-cb agent-type description prompt))
      ;; Create trace entry using registry
      (let* ((parent-trace-id (jf/gptel--current-trace-id session-id))
             (trace-id (jf/gptel--create-trace session-id agent-type description
                                               prompt parent-trace-id)))
        (message "DEBUG trace-agent-task: Created trace-id=%s, parent=%s" trace-id parent-trace-id)

        ;; Save initial subagent prompt
        (jf/gptel--save-trace-message session-id trace-id 'prompt prompt
                                      (list :timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ")))

        ;; Wrap callback to capture completion and result
        (funcall orig-fn
                 (lambda (result)
                   (let ((status (cond
                                  ((null result) "error")
                                  ((string-prefix-p "Error:" result) "error")
                                  (t "completed"))))
                     (message "DEBUG trace-agent-task: Completing trace-id=%s, status=%s" trace-id status)

                     ;; Save final result if successful
                     (when (and (stringp result) (not (string-prefix-p "Error:" result)))
                       (jf/gptel--save-trace-message session-id trace-id 'result result
                                                     (list :timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))))

                     ;; Complete trace in registry
                     (jf/gptel--complete-trace session-id trace-id status)

                     ;; Call original callback
                     (funcall main-cb result)))
                 agent-type description prompt)))))

(defun jf/gptel--inject-session-context (orig-fn prompt &rest args)
  "Advice around `gptel-request' to inject session-id into :context.
This allows subagents in separate buffers to access session data via registry.
If :context is an overlay (from gptel-agent), stores session-id as overlay property.
Otherwise, wraps or merges session-id into :context plist.
Also initializes session if needed BEFORE the request is sent."
  (let ((session-id jf/gptel--session-id)) ; From parent buffer
    (message "DEBUG inject-session-context: session-id=%s" session-id)

    ;; Initialize session if needed (autosave is enabled and in gptel-mode)
    (when (and (not session-id)
               jf/gptel-autosave-enabled
               gptel-mode
               (not (minibufferp))) ; Don't create sessions for minibuffer
      (message "DEBUG inject-session-context: No session-id, initializing now")
      (jf/gptel--initialize-session)
      (setq session-id jf/gptel--session-id))

    (if (not session-id)
        ;; Still no session (autosave disabled), call original
        (progn
          (message "DEBUG inject-session-context: No session-id, not injecting")
          (apply orig-fn prompt args))
      ;; Inject session-id into :context
      (let* ((existing-context (plist-get args :context))
             (new-context (cond
                           ;; If context is nil, create plist with session-id
                           ((null existing-context)
                            (list :session-id session-id))
                           ;; If context is an overlay (from gptel-agent), attach session-id as property
                           ((overlayp existing-context)
                            (overlay-put existing-context 'jf/session-id session-id)
                            existing-context)  ; Return overlay unchanged
                           ;; If context is already a plist, merge session-id into it
                           ((and (listp existing-context)
                                 (keywordp (car existing-context)))
                            (plist-put (copy-sequence existing-context) :session-id session-id))
                           ;; Unknown type, wrap it in plist
                           (t
                            (list :original-context existing-context
                                  :session-id session-id))))
             (new-args (plist-put (copy-sequence args) :context new-context)))
        (message "DEBUG inject-session-context: Injected session-id into context (type: %s)"
                 (cond ((overlayp new-context) "overlay")
                       ((listp new-context) "plist")
                       (t "other")))
        (apply orig-fn prompt new-args)))))

(defun jf/gptel--capture-subagent-messages (orig-fn prompt &rest args)
  "Advice to capture messages within subagent execution.
Wraps callback to intercept and save responses during subagent execution."
  (let* ((context (plist-get args :context))
         ;; Extract session-id from context (overlay property or plist)
         (session-id (cond
                      ((null context) nil)
                      ;; If context is overlay, get session-id from property
                      ((overlayp context)
                       (overlay-get context 'jf/session-id))
                      ;; If context is plist, get session-id from it
                      ((and (listp context) (keywordp (car context)))
                       (plist-get context :session-id))
                      (t nil)))
         (existing-callback (plist-get args :callback)))
    (if (not session-id)
        ;; Not in a traced session, pass through
        (apply orig-fn prompt args)
      ;; In a session, get current trace (if any)
      (let ((trace-id (jf/gptel--current-trace-id session-id)))
        ;; Only wrap callback if we're in a trace OR there's an existing callback
        ;; Main conversation has no :callback (uses stream processor directly)
        (if (and (not trace-id) (not existing-callback))
            ;; Main conversation with no callback - pass through without wrapping
            (progn
              (message "DEBUG capture: Skipping callback wrap for main conversation (no existing callback)")
              (apply orig-fn prompt args))
          ;; Subagent or main conversation with callback - wrap it
          (let ((wrapped-callback
               (lambda (response info)
                 ;; Log all response types for debugging
                 (message "DEBUG capture: response type=%s, trace-id=%s, session-id=%s"
                         (cond
                          ((stringp response) "string")
                          ((eq response t) "stream-end")
                          ((and (consp response) (eq (car response) 'tool-call)) "tool-call")
                          ((and (consp response) (eq (car response) 'tool-result)) "tool-result")
                          ((consp response) (format "cons:%s" (car response)))
                          (t (format "other:%s" (type-of response))))
                         trace-id session-id)
                 ;; Capture streaming text and tool completions
                 (cond
                  ;; String response (final or chunk)
                  ((and (stringp response) trace-id)
                   (when (> (length response) 0)  ; Ignore empty chunks
                     (message "DEBUG capture: Saving response chunk for trace %s" trace-id)
                     (jf/gptel--save-trace-message
                      session-id trace-id 'response response
                      (list :timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ")
                            ;; TODO: Extract token count from response metadata if available
                            :tokens 0))))

                  ;; Tool results - capture completed tool calls from FSM info
                  ((and (consp response) (eq (car response) 'tool-result))
                   (when-let* ((tool-use (plist-get info :tool-use)))
                     (if trace-id
                         (message "DEBUG capture: Capturing %d completed tools for trace %s"
                                  (length tool-use) trace-id)
                       (message "DEBUG capture: Capturing %d completed tools for session %s"
                                (length tool-use) session-id))
                     (dolist (tool-call tool-use)
                       (when (plist-get tool-call :result)
                         (message "DEBUG capture: Recording tool %s (id=%s)%s"
                                  (plist-get tool-call :name)
                                  (plist-get tool-call :id)
                                  (if trace-id (format " in trace %s" trace-id) " in main conversation"))
                         ;; Wrap in condition-case to prevent errors from breaking callback chain
                         (condition-case err
                             (jf/gptel--record-complete-tool-call session-id trace-id tool-call)
                           (error
                            (message "ERROR capturing tool call: %s" (error-message-string err))))))))

                  ;; Stream end marker
                  ((and (eq response t) trace-id)
                   (message "DEBUG capture: Stream completed for trace %s" trace-id)))

                 ;; Call original callback
                 (if existing-callback
                     (funcall existing-callback response info)
                   ;; No existing callback - this is unexpected for main conversation
                   (when (and (stringp response) (not trace-id))
                     (message "DEBUG capture: WARNING - No existing-callback for main conversation string response!"))))))
          ;; Replace callback with wrapped version
          (setq args (plist-put args :callback wrapped-callback))
          (if trace-id
              (message "DEBUG capture: Installed capture for trace %s (existing-callback: %s)"
                       trace-id (if existing-callback "present" "nil"))
            (message "DEBUG capture: Installed capture for session %s (main conversation, existing-callback: %s)"
                     session-id (if existing-callback "present" "nil")))
          (apply orig-fn prompt args)))))))

(defun jf/gptel--autosave-session (response-start response-end)
  "Automatically save gptel session after LLM response.
RESPONSE-START and RESPONSE-END mark the response boundaries.
This function is added to `gptel-post-response-functions'."
  (when (and jf/gptel-autosave-enabled
             gptel-mode)
    (condition-case err
        (progn
          ;; Initialize session if needed
          (unless jf/gptel--session-dir
            (jf/gptel--initialize-session))

          ;; Determine message ID (branch or sequential)
          (let* ((ext (jf/gptel--get-file-extension))
                 (msg-id (if (bound-and-true-p jf/gptel--branching-next)
                             ;; Use predetermined branch ID
                             (prog1 jf/gptel--branch-id
                               (setq jf/gptel--branching-next nil)
                               (setq jf/gptel--branch-id nil))
                           ;; Normal sequential ID
                           (progn
                             (setq jf/gptel--message-counter (1+ jf/gptel--message-counter))
                             (format "message-%d" jf/gptel--message-counter))))
                 ;; Response ID matches message number
                 (resp-id (replace-regexp-in-string "message-" "response-" msg-id))
                 (msg-file (format "%s.%s" msg-id ext))
                 (resp-file (format "%s.%s" resp-id ext))
                 ;; For now, extract the last message and response
                 ;; TODO: Improve to extract exact content
                 (message-content (jf/gptel--extract-last-message response-start))
                 (response-content (buffer-substring response-start response-end))
                 (msg-preview (substring message-content 0 (min 80 (length message-content))))
                 (resp-preview (substring response-content 0 (min 80 (length response-content)))))

            ;; Save message file
            (with-temp-file (expand-file-name msg-file jf/gptel--session-dir)
              (insert message-content))

            ;; Save response file
            (with-temp-file (expand-file-name resp-file jf/gptel--session-dir)
              (insert response-content))

            ;; Sync buffer-local metadata FROM registry (get latest with traces)
            (when jf/gptel--session-id
              (when-let ((session-data (jf/gptel--get-session-data jf/gptel--session-id)))
                (setq jf/gptel--session-metadata (plist-get session-data :metadata))))

            ;; Update metadata tree (uses buffer-local metadata, which now has traces)
            (jf/gptel--update-metadata-tree msg-id msg-file msg-preview
                                            resp-id resp-file resp-preview)

            ;; Sync registry with updated buffer-local (has both tree + traces)
            (when jf/gptel--session-id
              (jf/gptel--update-session-data jf/gptel--session-id :metadata
                                             jf/gptel--session-metadata))

            ;; Link any completed traces to this message/response
            (when jf/gptel--session-id
              (when-let* ((session-data (jf/gptel--get-session-data jf/gptel--session-id))
                          (metadata (plist-get session-data :metadata))
                          (traces (plist-get metadata :agent_traces)))
                (dolist (trace (append traces nil))  ; Convert vector to list
                  (when (and (equal (plist-get trace :status) "completed")
                             (null (plist-get trace :associated_nodes)))
                    ;; This trace just completed, associate it
                    (plist-put trace :associated_nodes (vector msg-id resp-id))))
                ;; Write updated metadata
                (let ((session-dir (plist-get session-data :directory)))
                  (jf/gptel--write-metadata session-dir metadata))))

            (message "Session saved: %s/%s"
                     (file-name-nondirectory jf/gptel--session-dir)
                     resp-file)))
      (file-error
       (message "gptel autosave failed: %s" (error-message-string err)))
      (error
       (message "gptel autosave error: %s" (error-message-string err))))))

(defun jf/gptel--extract-last-message (response-start)
  "Extract user's last message before RESPONSE-START using text properties."
  (save-excursion
    (goto-char response-start)
    ;; Find start of last user message (previous property change from response)
    (let ((msg-end response-start)
          (msg-start (previous-single-property-change response-start 'gptel nil (point-min))))
      ;; If we're at a response, go back one more to get the user message
      (when (eq (get-char-property msg-start 'gptel) 'response)
        (setq msg-end msg-start)
        (setq msg-start (previous-single-property-change msg-start 'gptel nil (point-min))))
      ;; Extract and trim
      (let ((content (buffer-substring-no-properties msg-start msg-end)))
        (gptel--trim-prefixes content)))))

(defun jf/gptel--get-all-messages ()
  "Extract all message/response pairs from current buffer.
Returns list of plists: (:type 'message|'response :content string)."
  (let (messages (prev-pt (point-max)))
    (save-excursion
      (goto-char (point-max))
      (while (> prev-pt (point-min))
        (let ((prop-change (previous-single-property-change prev-pt 'gptel nil (point-min))))
          (when prop-change
            (let* ((prop (get-char-property prop-change 'gptel))
                   (content (gptel--trim-prefixes
                            (buffer-substring-no-properties prop-change prev-pt)))
                   (type (if (eq prop 'response) 'response 'message)))
              (when (and content (not (string-empty-p content)))
                (push (list :type type :content content) messages)))
            (setq prev-pt prop-change)))))
    messages))

(defun jf/gptel--get-context-before-point (pt)
  "Get all message/response pairs before point PT.
Returns list of plists in chronological order."
  (let ((context nil)
        (scan-pt (point-min)))
    (save-excursion
      (while (< scan-pt pt)
        (goto-char scan-pt)
        (let ((next-change (next-single-property-change scan-pt 'gptel nil pt)))
          (when next-change
            (let* ((prop (get-char-property scan-pt 'gptel))
                   (content (gptel--trim-prefixes
                            (buffer-substring-no-properties scan-pt next-change)))
                   (type (if (eq prop 'response) 'response 'message)))
              (when (and content (not (string-empty-p content)))
                (push (list :type type :content content) context)))
            (setq scan-pt next-change))
          (unless next-change
            (setq scan-pt pt)))))
    (nreverse context)))

;; Install advice after packages load
(with-eval-after-load 'gptel-agent
  (advice-add 'gptel-agent--task :around #'jf/gptel--trace-agent-task))

(with-eval-after-load 'gptel
  ;; IMPORTANT: Order matters! Last added runs first.
  ;; capture-subagent-messages must run AFTER inject-session-context has added session-id
  ;; So we add inject-session-context LAST (it will run first in the chain)
  (advice-add 'gptel-request :around #'jf/gptel--capture-subagent-messages)
  (advice-add 'gptel-request :around #'jf/gptel--inject-session-context))

;; Install auto-save hook
(with-eval-after-load 'gptel
  (add-hook 'gptel-post-response-functions #'jf/gptel--autosave-session))
