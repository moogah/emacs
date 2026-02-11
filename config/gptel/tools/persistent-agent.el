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

(defun jf/gptel-persistent-agent--create-overlay (where preset description)
  "Create status overlay in parent buffer at WHERE.
PRESET is the preset name (e.g., \"researcher\").
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
                   (propertize (concat (capitalize preset) " Task: ")
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

(defun jf/gptel-persistent-agent--task (main-cb preset description prompt &optional allowed-paths denied-paths)
  "Launch a persistent agent in a new session buffer.

MAIN-CB is the callback function to invoke with the final result.
PRESET is the preset name (e.g., \"researcher\").
DESCRIPTION is a short (3-5 word) task summary.
PROMPT is the detailed task instructions.
ALLOWED-PATHS is optional array of file paths the agent can access.
DENIED-PATHS is optional array of file paths the agent cannot access.

If ALLOWED-PATHS is not specified or specified as empty array [], the agent
has no read permissions. Paths are never inherited from the parent session -
they must be explicitly provided.

Creates a nested agent session under the current persistent session,
launches the agent with tool support, displays progress in parent buffer
via overlay, and returns the final result to the parent.

The agent's configuration comes ONLY from the agent definition file,
with zero inheritance from the parent session."
  ;; Validate parent session exists
  (unless jf/gptel--session-dir
    (user-error "PersistentAgent requires parent persistent session"))

  ;; Get where to insert result in parent buffer
  (let ((where (point-marker)))

    ;; Create nested agent directory and session ID
    ;; Agents are created under the current branch, not at session root
    (let* ((session-dir (jf/gptel--create-agent-directory
                         jf/gptel--branch-dir preset description))
           (session-id (jf/gptel--session-id-from-directory session-dir)))

      ;; Copy preset template directly (file-first approach)
      (jf/gptel--copy-preset-template preset session-dir)

      ;; Load preset from copied file (like regular sessions)
      (let* ((preset-plist (jf/gptel--load-preset-from-file session-dir)))
        (unless preset-plist
          (error "Failed to load preset from %s" session-dir))

        ;; Handle allowed/denied paths
        ;; Convert from vectors (JSON arrays) to lists if needed
        (let* ((allowed-paths-list (if (vectorp allowed-paths)
                                      (append allowed-paths nil)
                                    allowed-paths))
               (denied-paths-list (if (vectorp denied-paths)
                                     (append denied-paths nil)
                                   denied-paths))
               ;; Use allowed-paths as-is (nil or empty means no read permissions)
               ;; No inheritance from parent - paths must be explicitly provided
               (effective-allowed-paths allowed-paths-list)
               ;; Use denied-paths if provided, otherwise nil (helper will use defaults)
               (effective-denied-paths denied-paths-list)
               ;; Path to agent's preset.md
               (preset-file (expand-file-name "preset.md" session-dir)))

          ;; Update preset.md with paths section
          (jf/gptel-scope--update-preset-paths
           preset-file
           effective-allowed-paths
           effective-denied-paths)

          (jf/gptel--log 'info "Updated agent preset.md: %s with %d allowed path(s)"
                        preset-file
                        (length effective-allowed-paths))

          ;; Create scope-plan.yml with session metadata
          ;; Note: scope configuration is now in preset.md, this file only has metadata
          (let ((scope-file (expand-file-name "scope-plan.yml" session-dir))
                (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
            (with-temp-file scope-file
              (insert (format "version: \"3.0\"\n"))
              (insert (format "session_id: \"%s\"\n" session-id))
              (insert (format "created: \"%s\"\n" timestamp))
              (insert (format "updated: \"%s\"\n" timestamp))
              (insert (format "type: \"agent\"\n"))
              (insert (format "parent_session_id: \"%s\"\n" jf/gptel--session-id))
              (insert (format "preset: \"%s\"\n" preset)))
            (jf/gptel--log 'info "Created agent scope-plan.yml (metadata only): %s" scope-file)))

        ;; Read metadata from scope-plan.yml for registry
        (let ((metadata (or (jf/gptel--read-session-metadata session-dir)
                           ;; Fallback if scope-plan.yml read fails
                           (list :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                                 :type "agent"
                                 :parent-session-id jf/gptel--session-id
                                 :preset preset))))

          ;; Create agent buffer with session infrastructure
          (let* ((buffer-name (format "*gptel-agent:%s:%s*" preset description))
                 (agent-buffer (generate-new-buffer buffer-name))
                 ;; Capture agent tools AFTER buffer initialization
                 (agent-tools nil))

            ;; Initialize buffer with session tracking and preset configuration
            (with-current-buffer agent-buffer
              (markdown-mode)
              (gptel-mode 1)

              ;; Set persistent session vars BEFORE preset application
              (setq-local jf/gptel--session-id session-id)
              (setq-local jf/gptel--session-dir session-dir)
              (setq-local jf/gptel--branch-name "main")
              (setq-local jf/gptel--branch-dir session-dir)

              ;; Apply preset from file (sets backend, model, tools, system message, etc.)
              ;; This replaces manual setting of gptel-tools and gptel-include-tool-results
              (jf/gptel--apply-session-preset preset-plist)
              (jf/gptel--log 'info "Applied preset to agent buffer from file")

              ;; Add auto-save via gptel's post-response hook
              (add-hook 'gptel-post-response-functions
                        #'jf/gptel--auto-save-session-buffer
                        nil t)

              ;; Insert prompt into buffer
              (insert prompt)
              (insert "\n\n")

              ;; Associate buffer with file
              (set-visited-file-name (jf/gptel--context-file-path session-dir))
              (set-buffer-modified-p t)

              ;; CRITICAL: Capture agent's buffer-local tools before leaving buffer context
              ;; This must be INSIDE with-current-buffer, otherwise we get parent's tools
              (setq agent-tools gptel-tools))

            ;; Register session globally with branch info
            ;; Agents don't support branching, so use "main" as default branch and session-dir as branch-dir
            (jf/gptel--register-session session-dir agent-buffer session-id "main" session-dir)

            ;; Create overlay for parent feedback
            (let ((ov (jf/gptel-persistent-agent--create-overlay
                       where preset description)))

              ;; Execute with request-specific overrides
              ;; CRITICAL: Use captured agent-tools, not current buffer's gptel-tools
              ;; We're in parent buffer context here, so gptel-tools would be parent's tools
              (gptel-with-preset
                  (list :include-reasoning nil
                        :use-tools t
                        :use-context nil
                        :include-tool-results t
                        :tools agent-tools)  ; Use captured tools from agent buffer for isolation

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

Use for complex research, open-ended exploration, or iterative tasks.

IMPORTANT: You should typically pass allowed_paths to control the agent's file access.
Use the inspect_scope_plan tool to get your current allowed paths, then pass them
to the agent. Example:
  allowed_paths: [\"/path/to/project/**\", \"/another/path/**\"]

If allowed_paths is omitted, the agent inherits your paths automatically.
If you want to restrict the agent to a subset of your paths, specify only those paths.

Note: denied_paths parameter is reserved for future use. Agents always deny
access to .git, runtime, node_modules, and .env paths."

 :function #'jf/gptel-persistent-agent--task

 :args '(( :name "preset"
           :type string
           :enum ["researcher" "executor" "explorer" "planner"]
           :description "Preset name for specialized agent")

         ( :name "description"
           :type string
           :description "Short (3-5 word) task description")

         ( :name "prompt"
           :type string
           :description "Detailed task instructions")

         ( :name "allowed_paths"
           :type array
           :items (:type string)
           :description "Array of glob patterns for paths the agent can access. Use /** suffix for recursive access. Example: [\"/path/to/project/**\"]. If omitted, inherits from parent session. Use inspect_scope_plan to see your current paths.")

         ( :name "denied_paths"
           :type array
           :items (:type string)
           :description "Array of glob patterns for paths the agent cannot access (reserved for future use)"))

 :category "gptel-persistent"
 :async t      ; Runs asynchronously
 :confirm t    ; User confirmation required
 :include t)   ; Results appear in parent buffer

(provide 'gptel-persistent-agent)
;;; persistent-agent.el ends here
