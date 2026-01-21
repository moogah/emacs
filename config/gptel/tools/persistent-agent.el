(defun jf/gptel--auto-save-session-buffer (&rest _args)
  "Auto-save session buffer after each gptel response.
Hooked into gptel-post-response-functions.
Only saves if buffer has associated file and session directory."
  (when (and jf/gptel--session-dir
             (buffer-file-name))
    (save-buffer)
    (jf/gptel--update-metadata-modified jf/gptel--session-dir)))

(defun jf/gptel-persistent-agent--create-overlay (where agent-type description)
  "Create status overlay in parent buffer at WHERE.
AGENT-TYPE is the type of agent (e.g., \"researcher\").
DESCRIPTION is a short summary of the task.
Returns overlay to pass as :context to gptel-request."
  (let* ((buffer (if (markerp where) (marker-buffer where) (current-buffer)))
         (pos (if (markerp where) (marker-position where) where)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        ;; Ensure we're not at beginning of buffer
        (when (bobp) (insert "\n") (backward-char 1))
        (let ((ov (make-overlay (line-beginning-position)
                               (line-end-position) nil t)))
          (overlay-put ov 'gptel-persistent-agent t)
          (overlay-put ov 'count 0)
          (overlay-put ov 'msg (format "[%s: %s]" agent-type description))
          (overlay-put ov 'after-string
                      (propertize (format "\n[%s: %s]\nWaiting...\n"
                                         agent-type description)
                                 'face '(:foreground "orange" :weight bold)))
          ov)))))

(defun jf/gptel-persistent-agent--indicate-wait (fsm)
  "Update overlay to show waiting status.
FSM is the finite state machine managing the request."
  (when-let* ((info (gptel-fsm-info fsm))
              (ov (plist-get info :context)))
    (run-at-time 1.5 nil  ; Delayed update like gptel-agent
      (lambda (overlay)
        (when (overlay-buffer overlay)
          (overlay-put overlay 'after-string
                      (concat (overlay-get overlay 'msg)
                             (propertize "\nWaiting...\n"
                                        'face 'warning)))))
      ov)))

(defun jf/gptel-persistent-agent--indicate-tool-call (fsm)
  "Update overlay to show tool calls in progress.
FSM is the finite state machine managing the request."
  (when-let* ((info (gptel-fsm-info fsm))
              (tool-use (plist-get info :tool-use))
              (ov (plist-get info :context)))
    (let* ((count (overlay-get ov 'count))
           (new-count (+ count (length tool-use)))
           (tool-summary
            (mapconcat (lambda (call)
                        (format "  - %s" (plist-get call :name)))
                      tool-use "\n")))
      (overlay-put ov 'count new-count)
      (overlay-put ov 'after-string
                  (concat (overlay-get ov 'msg)
                         (propertize (format "\nTools (+%d):\n%s\n"
                                            new-count tool-summary)
                                    'face 'mode-line-emphasis))))))

(defvar jf/gptel-persistent-agent--fsm-handlers
  `((WAIT ,#'jf/gptel-persistent-agent--indicate-wait
          ,#'gptel--handle-wait)
    (TOOL ,#'jf/gptel-persistent-agent--indicate-tool-call
          ,#'gptel--handle-tool-use))
  "Custom FSM handlers for persistent agents.
Each entry is (STATE UI-HANDLER CORE-HANDLER).")

(defun jf/gptel-persistent-agent--task (agent_type description prompt main-cb)
  "Launch a persistent agent in a new session buffer.

AGENT_TYPE is the type of agent (e.g., \"researcher\").
DESCRIPTION is a short (3-5 word) task summary.
PROMPT is the detailed task instructions.
MAIN-CB is the callback function to invoke with the final result.

Creates a nested subagent session under the current persistent session,
launches the agent with tool support, displays progress in parent buffer
via overlay, and returns the final result to the parent.

The agent's configuration comes ONLY from the agent definition file,
with zero inheritance from the parent session."
  ;; Validate parent session exists
  (unless jf/gptel--session-dir
    (user-error "PersistentAgent requires parent persistent session"))

  ;; Get where to insert result in parent buffer
  (let ((where (point-marker)))

    ;; Create nested subagent directory and session ID
    (let* ((session-dir (jf/gptel--create-subagent-directory
                         jf/gptel--session-dir agent_type description))
           (session-id (jf/gptel--session-id-from-directory session-dir)))

      ;; Get agent config to determine backend/model (NO parent inheritance)
      (let* ((agent-config (cdr (assoc agent_type gptel-agent--agents)))
             (agent-backend-name (or (plist-get agent-config :backend)
                                    (gptel-backend-name gptel-backend)))
             (agent-model (or (plist-get agent-config :model)
                             gptel-model)))

        ;; Create and write metadata using agent's config, not parent's
        (let ((metadata (jf/gptel--create-metadata
                         session-dir session-id agent-model agent-backend-name)))
          (plist-put metadata :type "subagent")
          (plist-put metadata :parent-session-id jf/gptel--session-id)
          (plist-put metadata :agent-type agent_type)
          (plist-put metadata :description description)
          (jf/gptel--write-metadata session-dir metadata)
          (jf/gptel--link-subagent-to-parent session-dir jf/gptel--session-id))

        ;; Create preset file ONLY from agent definition (NO parent inheritance)
        (let* ((preset-plist (copy-sequence agent-config)))
          ;; Override to ensure tool results are included
          (plist-put preset-plist :include-tool-results t)
          (jf/gptel--write-preset-file session-dir preset-plist))

        ;; Create agent buffer with session infrastructure
        (let* ((buffer-name (format "*gptel-agent:%s:%s*" agent_type description))
               (agent-buffer (generate-new-buffer buffer-name)))

          ;; Initialize buffer with session tracking and auto-save
          (with-current-buffer agent-buffer
            (markdown-mode)
            (gptel-mode 1)

            ;; Set persistent session vars BEFORE preset scope
            (setq-local jf/gptel--session-id session-id)
            (setq-local jf/gptel--session-dir session-dir)
            (setq-local gptel-tools nil)  ; Prevent inheritance EXPLICITLY

            ;; Add auto-save via gptel's post-response hook
            (add-hook 'gptel-post-response-functions
                      #'jf/gptel--auto-save-session-buffer
                      nil t)  ; buffer-local hook

            ;; Write initial file
            (write-file (jf/gptel--context-file-path session-dir)))

          ;; Register session globally
          (jf/gptel--register-session session-dir metadata agent-buffer session-id)

          ;; Create overlay for parent feedback
          (let ((ov (jf/gptel-persistent-agent--create-overlay
                     where agent_type description)))

            ;; Accumulator for response
            (let ((partial ""))

              ;; Execute with preset scope for configuration only
              (gptel-with-preset
                  (nconc (list :include-reasoning nil
                               :use-tools t
                               :use-context nil)
                         (cdr (assoc agent_type gptel-agent--agents)))

                ;; Launch the agent request
                (gptel-request prompt
                  :buffer agent-buffer
                  :context ov  ; Parent buffer overlay
                  :fsm jf/gptel-persistent-agent--fsm-handlers

                  :callback
                  (lambda (resp info)
                    (let ((ov (plist-get info :context)))
                      (pcase resp
                        ;; Network/API error
                        ('nil
                         (delete-overlay ov)
                         (funcall main-cb
                                  (format "Error: Network failure\n%S"
                                          (plist-get info :error))))

                        ;; User aborted tool confirmation
                        ('abort
                         (delete-overlay ov)
                         (funcall main-cb "Error: User aborted agent"))

                        ;; Tool calls pending - wait for completion
                        (`(tool-call . ,calls)
                         (gptel--display-tool-calls calls info))

                        ;; String response - accumulate and return when done
                        ((pred stringp)
                         ;; Accumulate response
                         (setq partial (concat partial resp))

                         ;; Only fire callback when NOT in tool-use
                         (unless (plist-get info :tool-use)
                           (delete-overlay ov)
                           ;; Apply transformer if present
                           (when-let ((transform (plist-get info :transformer)))
                             (setq partial (funcall transform partial)))
                           (funcall main-cb partial)))))))))))))))

(gptel-make-tool
 :name "PersistentAgent"
 :description "Launch specialized agent in persistent session buffer.

Agents run autonomously and return results in one message.
Sessions persist to disk with full conversation history.

Use for complex research, open-ended exploration, or iterative tasks."

 :function #'jf/gptel-persistent-agent--task

 :args '(( :name "agent_type"
           :type string
           :enum ["researcher" "executor" "explorer" "planner"]
           :description "Type of specialized agent")

         ( :name "description"
           :type string
           :description "Short (3-5 word) task description")

         ( :name "prompt"
           :type string
           :description "Detailed task instructions"))

 :category "gptel-persistent"
 :async t      ; Runs asynchronously
 :confirm t    ; User confirmation required
 :include t)   ; Results appear in parent buffer

(provide 'gptel-persistent-agent)
