;;; persistent-agent.el --- GPTEL Persistent Agent Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Persistent-agent tool for gptel that launches specialized agents in
;; persistent chat-mode session buffers with full tool access and
;; conversation history. The agent's request runs through chat-mode's
;; public programmatic-send API; FSM handlers are composed for parent
;; overlay feedback (via upstream gptel-agent helpers) and final-text
;; return to the parent's tool callback.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-agent)            ; gptel-agent--task-overlay,
                                  ;   --indicate-wait, --indicate-tool-call
(require 'gptel-chat-parser)      ; gptel-chat-parse-buffer,
                                  ;   gptel-chat-turns-to-messages
(require 'gptel-chat-send)        ; gptel-chat-open-assistant-block,
                                  ;   gptel-chat-fsm-handlers
(require 'gptel-chat-stream)      ; gptel-chat-stream-callback
(require 'gptel-session-constants)
(require 'gptel-session-filesystem) ; jf/gptel--create-agent-directory,
                                    ;   jf/gptel--context-file-path,
                                    ;   jf/gptel--session-id-from-directory
(require 'gptel-session-commands)   ; jf/gptel--create-session-core
(require 'gptel-session-logging)

(defun jf/gptel-persistent-agent--write-scope-file (session-dir allowed-paths)
  "Write SESSION-DIR/scope.yml with read paths from ALLOWED-PATHS.
ALLOWED-PATHS is a normalized list (caller has converted any vector).
Empty/nil → `paths.read: []` (zero inheritance).

Side effect only; returns nil."
  (let ((scope-file (expand-file-name jf/gptel-session--scope-file session-dir)))
    (with-temp-file scope-file
      (insert "paths:\n")
      (insert "  read:\n")
      (if allowed-paths
          (dolist (p allowed-paths)
            (insert (format "    - \"%s\"\n" p)))
        (insert "    []\n"))
      (insert "  write:\n")
      (insert "    - \"/tmp/**\"\n")
      (insert "  deny:\n")
      (dolist (p '("**/.git/**" "**/runtime/**" "**/.env" "**/node_modules/**"))
        (insert (format "    - \"%s\"\n" p))))))

(defun jf/gptel-persistent-agent--initial-content (preset-sym parent-id prompt)
  "Build initial session.org content for an agent: drawer + populated user block.
Format mirrors `jf/gptel--initial-session-content' (sessions/commands.el).
Cross-reference: keep shapes aligned. The creation-spec test asserts
on the full layout, so format drift in either function fails the test."
  (format ":PROPERTIES:\n:GPTEL_PRESET: %s\n:GPTEL_PARENT_SESSION_ID: %s\n:END:\n#+begin_user\n%s\n#+end_user\n"
          (symbol-name preset-sym) parent-id prompt))

(defun jf/gptel-persistent-agent--extract-final-text (agent-buffer)
  "Return trailing text of the last assistant turn in AGENT-BUFFER.
Returns the empty string when the last assistant turn has no text
segment (Decision 2 of design.md: empty-text fallback)."
  (with-current-buffer agent-buffer
    (let* ((turns     (gptel-chat-parse-buffer))
           (last-asst (cl-loop for turn in (reverse turns)
                               when (eq (plist-get turn :role) 'assistant)
                               return turn))
           (segments  (and last-asst (plist-get last-asst :segments)))
           (last-text (cl-loop for seg in (reverse (or segments '()))
                               when (eq (plist-get seg :type) 'text)
                               return (plist-get seg :content))))
      (or last-text ""))))

(defun jf/gptel-persistent-agent--make-on-done (main-cb agent-buffer)
  "Return a DONE FSM handler that returns the final assistant text to MAIN-CB."
  (lambda (fsm)
    (let* ((info (gptel-fsm-info fsm))
           (overlay (plist-get info :context))
           (text (jf/gptel-persistent-agent--extract-final-text agent-buffer)))
      (when (overlayp overlay) (delete-overlay overlay))
      (funcall main-cb text))))

(defun jf/gptel-persistent-agent--make-on-errs (main-cb)
  "Return an ERRS FSM handler that returns an error message to MAIN-CB."
  (lambda (fsm)
    (let* ((info (gptel-fsm-info fsm))
           (overlay (plist-get info :context))
           (err (plist-get info :error)))
      (when (overlayp overlay) (delete-overlay overlay))
      (funcall main-cb (format "Error: agent request failed\n%S" err)))))

(defun jf/gptel-persistent-agent--make-on-abrt (main-cb)
  "Return an ABRT FSM handler that returns an abort message to MAIN-CB."
  (lambda (fsm)
    (let* ((info (gptel-fsm-info fsm))
           (overlay (plist-get info :context)))
      (when (overlayp overlay) (delete-overlay overlay))
      (funcall main-cb "Error: agent aborted by user"))))

(defun jf/gptel-persistent-agent--build-fsm-handlers (base main-cb agent-buffer)
  "Return an FSM handler alist composing agent handlers atop BASE.
BASE is the chat-mode handler alist `gptel-chat-fsm-handlers'.
For WAIT and TOOL: prepend the upstream gptel-agent overlay updater.
For DONE/ERRS/ABRT: prepend the agent's terminal handler returning to MAIN-CB."
  (mapcar
   (lambda (entry)
     (pcase (car entry)
       ('WAIT `(WAIT ,#'gptel-agent--indicate-wait ,@(cdr entry)))
       ('TOOL `(TOOL ,#'gptel-agent--indicate-tool-call ,@(cdr entry)))
       ('DONE `(DONE ,(jf/gptel-persistent-agent--make-on-done main-cb agent-buffer)
                     ,@(cdr entry)))
       ('ERRS `(ERRS ,(jf/gptel-persistent-agent--make-on-errs main-cb)
                     ,@(cdr entry)))
       ('ABRT `(ABRT ,(jf/gptel-persistent-agent--make-on-abrt main-cb)
                     ,@(cdr entry)))
       (_     entry)))
   base))

(defun jf/gptel-persistent-agent--task (main-cb preset description prompt
                                                 &optional allowed-paths)
  "Spawn an autonomous agent in a chat-mode session.
On completion, return the final assistant text to MAIN-CB.

PRESET is the registered gptel preset name (string).
DESCRIPTION is a 3-5 word slug used in the directory and overlay header.
PROMPT is the initial user message text.
ALLOWED-PATHS is an optional vector or list of glob patterns; nil/empty ⇒
no read access (zero inheritance).

The agent's session.org is created with a self-describing :PROPERTIES:
drawer (preset + parent session id), opened with `find-file-noselect',
and configured by the find-file-hook auto-init pipeline. The agent's
request runs through chat-mode's public programmatic-send API with
FSM handlers composed for parent overlay feedback and final-text return."
  (unless jf/gptel--session-dir
    (user-error "PersistentAgent requires parent persistent session"))
  (let ((preset-sym (intern preset)))
    (unless (gptel-get-preset preset-sym)
      (user-error "Preset '%s' not found in gptel--known-presets" preset))
    (let* ((info (and (boundp 'gptel--fsm-last)
                      gptel--fsm-last
                      (gptel-fsm-info gptel--fsm-last)))
           (where (or (plist-get info :tracking-marker)
                      (plist-get info :position)
                      (point-marker)))
           (parent-id jf/gptel--session-id)
           (allowed-paths-list (if (vectorp allowed-paths)
                                   (append allowed-paths nil)
                                 allowed-paths))
           (session-dir (jf/gptel--create-agent-directory
                         jf/gptel--branch-dir preset description))
           (session-id  (jf/gptel--session-id-from-directory session-dir))
           (initial-content
            (jf/gptel-persistent-agent--initial-content
             preset-sym parent-id prompt)))
      (jf/gptel-persistent-agent--write-scope-file session-dir allowed-paths-list)
      (let* ((session-info
              (jf/gptel--create-session-core
               session-id session-dir preset-sym initial-content
               nil nil parent-id))
             (session-file (plist-get session-info :session-file))
             (agent-buffer (find-file-noselect session-file))
             (overlay (gptel-agent--task-overlay where preset description)))
        (with-current-buffer agent-buffer
          (let* ((turns     (gptel-chat-parse-buffer))
                 (user-turn (cl-loop for turn in (reverse turns)
                                     when (eq (plist-get turn :role) 'user)
                                     return turn))
                 (messages  (gptel-chat-turns-to-messages turns))
                 (insertion (gptel-chat-open-assistant-block user-turn))
                 (stream-cb (gptel-chat-stream-callback insertion))
                 (handlers  (jf/gptel-persistent-agent--build-fsm-handlers
                             gptel-chat-fsm-handlers main-cb agent-buffer))
                 (fsm       (gptel-make-fsm :handlers handlers)))
            (gptel-request messages
                           :stream   t
                           :callback stream-cb
                           :context  overlay
                           :fsm      fsm)))))))

(gptel-make-tool
 :name "PersistentAgent"
 :description "Launch a specialized agent in a persistent chat-mode session.
Agents run autonomously and return their final text in one message.
Sessions persist to disk with full conversation history.

Use for complex research, open-ended exploration, or iterative tasks.

IMPORTANT: typically pass `allowed_paths' to control the agent's file access.
Use the read_file_in_scope tool on scope.yml to get your current allowed
paths, then pass them to the agent. If allowed_paths is omitted or empty,
the agent has NO read access."
 :function #'jf/gptel-persistent-agent--task
 :args '(( :name "preset"
           :type string
           :description "Registered gptel preset name for the agent")
         ( :name "description"
           :type string
           :description "Short (3-5 word) task description")
         ( :name "prompt"
           :type string
           :description "Detailed task instructions; becomes the agent's first user turn")
         ( :name "allowed_paths"
           :type array
           :items (:type string)
           :description "Array of glob patterns for paths the agent can access. If omitted, agent has no read access."))
 :category "gptel-persistent"
 :async t
 :confirm t
 :include t)

(provide 'gptel-persistent-agent)
;;; persistent-agent.el ends here
