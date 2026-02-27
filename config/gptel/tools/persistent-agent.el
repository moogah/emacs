;;; persistent-agent.el --- GPTEL Persistent Agent Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Persistent agent tool for gptel that launches specialized agents
;; in persistent session buffers with full tool access and conversation history.

;;; Code:

(require 'gptel)
(require 'gptel-session-constants)
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

The agent's configuration comes ONLY from the preset in
gptel--known-presets, with zero inheritance from the parent session."
  ;; Validate parent session exists
  (unless jf/gptel--session-dir
    (user-error "PersistentAgent requires parent persistent session"))

  ;; Validate preset exists in registry
  (let ((preset-name (intern preset)))
    (unless (gptel-get-preset preset-name)
      (user-error "Preset '%s' not found in gptel--known-presets" preset)))

  ;; Get where to insert result in parent buffer
  (let ((where (point-marker)))

    ;; Create nested agent directory and session ID
    ;; Agents are created under the current branch, not at session root
    (let* ((session-dir (jf/gptel--create-agent-directory
                         jf/gptel--branch-dir preset description))
           (session-id (jf/gptel--session-id-from-directory session-dir)))

      ;; Write scope.yml with paths from allowed_paths parameter
      ;; Zero inheritance: paths come from tool invocation, not parent
      (let* ((allowed-paths-list (if (vectorp allowed-paths)
                                     (append allowed-paths nil)
                                   allowed-paths))
             (denied-paths-list (if (vectorp denied-paths)
                                    (append denied-paths nil)
                                  denied-paths))
             (scope-file (expand-file-name jf/gptel-session--scope-file session-dir)))
        (with-temp-file scope-file
          (insert "paths:\n")
          (insert "  read:\n")
          (if allowed-paths-list
              (dolist (p allowed-paths-list)
                (insert (format "    - \"%s\"\n" p)))
            (insert "    []\n"))
          (insert "  write:\n")
          (insert "    - \"/tmp/**\"\n")
          (insert "  deny:\n")
          (dolist (p (or denied-paths-list
                         '("**/.git/**" "**/runtime/**" "**/.env" "**/node_modules/**")))
            (insert (format "    - \"%s\"\n" p))))
        (jf/gptel--log 'info "Created agent scope.yml with %d read path(s)"
                      (length allowed-paths-list)))

      ;; Write metadata.yml with session metadata
      (let ((metadata-file (expand-file-name jf/gptel-session--metadata-file session-dir))
            (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
        (with-temp-file metadata-file
          (insert (format "version: \"3.0\"\n"))
          (insert (format "session_id: \"%s\"\n" session-id))
          (insert (format "created: \"%s\"\n" timestamp))
          (insert (format "updated: \"%s\"\n" timestamp))
          (insert (format "type: \"agent\"\n"))
          (insert (format "parent_session_id: \"%s\"\n" jf/gptel--session-id))
          (insert (format "preset: \"%s\"\n" preset)))
        (jf/gptel--log 'info "Created agent metadata.yml: %s" metadata-file))

      ;; Create agent buffer with session infrastructure
      (let* ((buffer-name (format "*gptel-agent:%s:%s*" preset description))
             (agent-buffer (generate-new-buffer buffer-name)))

        ;; Initialize buffer with session tracking and preset configuration
        (with-current-buffer agent-buffer
          (markdown-mode)

          ;; Set persistent session vars BEFORE preset application
          (setq-local jf/gptel--session-id session-id)
          (setq-local jf/gptel--session-dir session-dir)
          (setq-local jf/gptel--branch-name "main")
          (setq-local jf/gptel--branch-dir session-dir)

          ;; Apply preset via upstream with buffer-local setter
          (gptel--apply-preset (intern preset)
                               (lambda (var val) (set (make-local-variable var) val)))

          ;; Enable gptel-mode AFTER preset application
          (gptel-mode 1)

          ;; Add auto-save via gptel's post-response hook
          (add-hook 'gptel-post-response-functions
                    #'jf/gptel--auto-save-session-buffer
                    nil t)

          ;; Insert prompt into buffer
          (insert prompt)
          (insert "\n\n")

          ;; Associate buffer with file
          (set-visited-file-name (jf/gptel--context-file-path session-dir))
          (set-buffer-modified-p t))

        ;; Register session globally with branch info
        ;; Agents don't support branching, so use "main" as default branch and session-dir as branch-dir
        (jf/gptel--register-session session-dir agent-buffer session-id "main" session-dir)

        ;; Create overlay for parent feedback
        (let ((ov (jf/gptel-persistent-agent--create-overlay
                   where preset description)))

          ;; Execute request from agent buffer
          ;; Buffer-local settings from gptel--apply-preset are sufficient
          (let ((partial ""))
            (gptel-request nil
              :buffer agent-buffer
              :position (with-current-buffer agent-buffer (point-max))
              :context ov
              :fsm (gptel-make-fsm :handlers jf/gptel-persistent-agent--fsm-handlers)
              :use-tools t
              :include-tool-results t

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
                         (funcall main-cb partial))))))))))))))

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

If allowed_paths is omitted or empty, the agent has NO read access to any paths.
You must explicitly provide paths for the agent to read files.

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
           :description "Array of glob patterns for paths the agent can access. Use /** suffix for recursive access. Example: [\"/path/to/project/**\"]. If omitted, agent has no read access. Use inspect_scope_plan to see your current paths.")

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
