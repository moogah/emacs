;;; persistent-agent.el --- GPTEL Persistent Agent Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Persistent agent tool for gptel that launches specialized agents
;; in persistent session buffers with full tool access and conversation history.

;;; Code:

(require 'gptel)
(require 'gptel-agent)
(require 'gptel-session-filesystem)
(require 'gptel-session-metadata)
(require 'gptel-session-registry)
(require 'gptel-session-logging)

(defconst jf/gptel-persistent-agent--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t))
  "Horizontal rule for separating overlay sections.")

(defun jf/gptel--auto-save-session-buffer (&rest _args)
  "Auto-save session buffer after each gptel response.
Hooked into gptel-post-response-functions.
Only saves if buffer has associated file and session directory."
  (when (and jf/gptel--session-dir
             (buffer-file-name))
    (save-buffer)
    ;; Note: scope-plan.yml updated timestamp is managed by scope commands,
    ;; not auto-save
    ))

(defun jf/gptel-persistent-agent--create-overlay (where agent-type description)
  "Create status overlay in parent buffer at WHERE.
AGENT-TYPE is the type of agent (e.g., \"researcher\").
DESCRIPTION is a short summary of the task.
Returns overlay to pass as :context to gptel-request."
  (let* ((buffer (if (markerp where) (marker-buffer where) (current-buffer)))
         (pos (if (markerp where) (marker-position where) where)))
    (with-current-buffer buffer
      (let* ((bounds
              (save-excursion
                (goto-char pos)
                (when (bobp) (insert "\n"))
                (if (and (bolp) (eolp))
                    (cons (1- (point)) (point))
                  (cons (line-beginning-position) (line-end-position)))))
             (ov (make-overlay (car bounds) (cdr bounds) nil t))
             (msg (concat
                   (unless (eq (char-after (car bounds)) 10) "\n")
                   "\n" jf/gptel-persistent-agent--hrule
                   (propertize (concat (capitalize agent-type) " Task: ")
                               'face 'font-lock-escape-face)
                   (propertize description 'face 'font-lock-doc-face) "\n")))
        (overlay-put ov 'gptel-persistent-agent t)
        (overlay-put ov 'count 0)
        (overlay-put ov 'msg msg)
        (overlay-put ov 'line-prefix "")
        (overlay-put ov 'after-string
                    (concat msg (propertize "Waiting... " 'face 'warning) "\n"
                            jf/gptel-persistent-agent--hrule))
        ov))))

(defun jf/gptel-persistent-agent--indicate-wait (fsm)
  "Update overlay to show waiting status.
FSM is the finite state machine managing the request."
  (when-let* ((info (gptel-fsm-info fsm))
              (ov (plist-get info :context)))
    (let ((count (overlay-get ov 'count)))
      (run-at-time 1.5 nil
        (lambda (overlay count)
          (when (and (overlay-buffer overlay)
                     (eql (overlay-get overlay 'count) count))
            (let* ((task-msg (overlay-get overlay 'msg))
                   (new-info-msg
                    (concat task-msg
                            (concat
                             (propertize "Waiting... " 'face 'warning) "\n"
                             (propertize "\n" 'face
                                        '(:inherit shadow :underline t :extend t))))))
              (overlay-put overlay 'after-string new-info-msg))))
        ov count))))

(defun jf/gptel-persistent-agent--indicate-tool-call (fsm)
  "Update overlay to show tool calls in progress.
FSM is the finite state machine managing the request."
  (when-let* ((info (gptel-fsm-info fsm))
              (tool-use (plist-get info :tool-use))
              (ov (plist-get info :context)))
    (when (overlay-buffer ov)
      (let* ((task-msg (overlay-get ov 'msg))
             (info-count (overlay-get ov 'count))
             (new-info-msg))
        (setq new-info-msg
              (concat task-msg
                      (concat
                       (propertize "Calling Tools... " 'face 'mode-line-emphasis)
                       (if (= info-count 0) "\n" (format "(+%d)\n" info-count))
                       (mapconcat (lambda (call)
                                    (gptel--format-tool-call
                                     (plist-get call :name)
                                     (map-values (plist-get call :args))))
                                  tool-use)
                       "\n" jf/gptel-persistent-agent--hrule)))
        (overlay-put ov 'count (+ info-count (length tool-use)))
        (overlay-put ov 'after-string new-info-msg)))))

(defvar jf/gptel-persistent-agent--fsm-handlers
  `((WAIT ,#'jf/gptel-persistent-agent--indicate-wait
          ,#'gptel--handle-wait)
    (TOOL ,#'jf/gptel-persistent-agent--indicate-tool-call
          ,#'gptel--handle-tool-use))
  "Custom FSM handlers for persistent agents.
Each entry is (STATE UI-HANDLER CORE-HANDLER).")

(defun jf/gptel-persistent-agent--task (main-cb agent_type description prompt)
  "Launch a persistent agent in a new session buffer.

MAIN-CB is the callback function to invoke with the final result.
AGENT_TYPE is the type of agent (e.g., \"researcher\").
DESCRIPTION is a short (3-5 word) task summary.
PROMPT is the detailed task instructions.

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

        ;; Create scope-plan.yml with subagent fields (deny-all template)
        (let* ((scope-yaml (jf/gptel-scope--template-deny-all
                           session-id "subagent" jf/gptel--session-id agent_type))
               (scope-file (expand-file-name "scope-plan.yml" session-dir)))
          (with-temp-file scope-file
            (insert scope-yaml))
          (jf/gptel--log 'info "Created subagent scope plan: %s" scope-file))

        ;; Parent-child relationship is now tracked via parent_session_id in scope-plan.yml

        ;; Create preset file ONLY from agent definition (NO parent inheritance)
        (let* ((preset-plist (copy-sequence agent-config)))
          ;; Override to ensure tool results are included
          (plist-put preset-plist :include-tool-results t)
          (jf/gptel--write-preset-file session-dir preset-plist))

        ;; Read metadata from scope-plan.yml for registry
        (let ((metadata (or (jf/gptel--read-session-metadata session-dir)
                           ;; Fallback if scope-plan.yml read fails
                           (list :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                                 :type "subagent"
                                 :parent-session-id jf/gptel--session-id
                                 :agent-type agent_type))))

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
              (setq-local gptel-include-tool-results t)  ; Enable tool result persistence

              ;; Add auto-save via gptel's post-response hook
              (add-hook 'gptel-post-response-functions
                        #'jf/gptel--auto-save-session-buffer
                        nil t)  ; buffer-local hook

              ;; Insert prompt into buffer
              ;; This allows gptel-request with PROMPT=nil to read from buffer
              (insert prompt)
              (insert "\n\n")  ; Separator for response

              ;; Associate buffer with file but don't write yet
              ;; Let auto-save handle first write (ensures Local Variables at end)
              (set-visited-file-name (jf/gptel--context-file-path session-dir))
              (set-buffer-modified-p t))

            ;; Register session globally (metadata no longer needed in registry)
            (jf/gptel--register-session session-dir agent-buffer session-id)

          ;; Create overlay for parent feedback
          (let ((ov (jf/gptel-persistent-agent--create-overlay
                     where agent_type description)))

            ;; Execute with preset scope for configuration only
            (gptel-with-preset
                (nconc (list :include-reasoning nil
                             :use-tools t
                             :use-context nil
                             :include-tool-results t)
                       (cdr (assoc agent_type gptel-agent--agents)))

              ;; Accumulator for response (must be inside preset scope)
              (let ((partial ""))

                ;; Launch the agent request
                ;; PROMPT=nil reads from agent-buffer (which now contains prompt)
                (gptel-request nil
                  :buffer agent-buffer
                  :position (point-max)  ; Insert response at end of buffer
                  :context ov  ; Parent buffer overlay
                  :fsm (gptel-make-fsm :handlers jf/gptel-persistent-agent--fsm-handlers)

                  :callback
                  (lambda (resp info &optional raw)
                    (let ((ov (plist-get info :context))
                          (buf (plist-get info :buffer)))
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

                        ;; Tool results ready - display in agent buffer
                        (`(tool-result . ,tool-results)
                         (gptel--display-tool-results tool-results info))

                        ;; String response - DUAL DUTY: insert to buffer AND accumulate
                        ((pred stringp)
                         ;; 1. Insert into agent-buffer for persistence
                         (with-current-buffer buf
                           (save-excursion
                             (goto-char (point-max))
                             (if raw
                                 ;; Raw (tool results): properties already set
                                 (insert resp)
                               ;; Regular response: add properties
                               (let ((start (point)))
                                 (insert resp)
                                 (when gptel-mode
                                   (put-text-property start (point)
                                                     'gptel 'response))))))

                         ;; 2-3. Accumulate and callback only for non-raw responses
                         (unless raw
                           ;; 2. Accumulate for parent callback
                           (setq partial (concat partial resp))

                           ;; 3. Return to parent when done (not in tool-use)
                           (unless (plist-get info :tool-use)
                             (delete-overlay ov)
                             ;; Apply transformer if present
                             (when-let ((transform (plist-get info :transformer)))
                               (setq partial (funcall transform partial)))
                             (funcall main-cb partial)))))))))))))))))

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
;;; persistent-agent.el ends here
