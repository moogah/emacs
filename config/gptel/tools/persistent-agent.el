;;; persistent-agent.el --- GPTEL Persistent Agent Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Persistent agent tool for gptel that launches specialized agents in
;; chat-mode session buffers. Agents run autonomously and return their
;; final assistant text in one message. Sessions persist to disk and
;; reload as fully interactive chat-mode sessions.
;;
;; The module composes its behaviour atop the public chat-mode pipeline
;; (gptel-chat-parser, gptel-chat-send, gptel-chat-stream) and the
;; session-creation pipeline (gptel-session-commands). The local overlay
;; helpers stay in this file (no `gptel-agent' dependency).

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-chat-parser)        ; gptel-chat-parse-buffer,
                                    ;   gptel-chat-turns-to-messages
(require 'gptel-chat-send)          ; gptel-chat-open-assistant-block,
                                    ;   gptel-chat-fsm-handlers
(require 'gptel-chat-stream)        ; gptel-chat-stream-callback
(require 'gptel-session-constants)
(require 'gptel-session-filesystem) ; jf/gptel--create-agent-directory,
                                    ;   jf/gptel--context-file-path
(require 'gptel-session-logging)
(require 'gptel-scope-profiles)     ; jf/gptel-scope-profile--render-drawer-text

(defconst jf/gptel-persistent-agent--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t))
  "Horizontal rule for separating overlay sections.")

(defun jf/gptel-persistent-agent--task-overlay (where preset description)
  "Create a status overlay in the parent buffer at WHERE.
PRESET is the preset name (e.g., \"researcher\").
DESCRIPTION is a short summary of the task.
Returns the overlay; pass it as :context to `gptel-request'."
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

(defconst jf/gptel-persistent-agent--standard-deny-paths
  '("**/.git/**" "**/runtime/**" "**/.env" "**/node_modules/**")
  "Standard glob patterns denied for every persistent agent.
Embedded into the agent's `:GPTEL_SCOPE_DENY:' drawer key on creation
\(cycle-2 task `rewire-persistent-agent'). Previously hard-coded inline
in `jf/gptel-persistent-agent--write-scope-file' before that function
was deleted.")

(defun jf/gptel-persistent-agent--build-scope-plist (allowed-paths)
  "Build the agent's scope plist from ALLOWED-PATHS.
ALLOWED-PATHS is a normalized list of glob patterns (caller has
converted any vector). nil/empty ⇒ no read access (zero inheritance).

Returns a plist of `register/shape/scope-config-plist' shape:
  (:paths (:read (...) :write (\"/tmp/**\") :deny (<standard set>)))

Caller renders this via
`jf/gptel-scope-profile--render-drawer-text' to obtain the
`:PROPERTIES:' drawer-text-block embedded in the agent's
`session.org'."
  (list :paths
        (list :read  (or allowed-paths nil)
              :write '("/tmp/**")
              :deny  jf/gptel-persistent-agent--standard-deny-paths)))

(defun jf/gptel-persistent-agent--initial-body (prompt)
  "Build the user-block body of a fresh agent session.org.
PROMPT becomes the body of the first `#+begin_user' block.

Returns a string of the form
  #+begin_user
  <prompt>
  #+end_user
\\n

Caller is expected to prepend a `:PROPERTIES:' drawer rendered by
`jf/gptel-scope-profile--render-drawer-text' (Mode 2a) before
writing the file, so that the composed
`(concat drawer-text body)' carries exactly one `:PROPERTIES:' /
`:END:' pair (`register/invariant/scope-drawer-no-duplication')."
  (format "#+begin_user\n%s\n#+end_user\n" prompt))

(defun jf/gptel-persistent-agent--initial-content (preset-sym parent-id prompt)
  "Build initial session.org content: drawer (preset + parent only) + user block.
PRESET-SYM is the preset name as a symbol.
PARENT-ID is the parent session id string.
PROMPT becomes the body of the first `#+begin_user' block.

NOTE: Production — `jf/gptel-persistent-agent--task' — no longer
routes through this helper. It composes the drawer via
`jf/gptel-scope-profile--render-drawer-text' (which carries the
resolved `:GPTEL_SCOPE_*' keys) and the body via
`jf/gptel-persistent-agent--initial-body'. This helper is retained
for direct callers and helper-level tests."
  (format ":PROPERTIES:\n:GPTEL_PRESET: %s\n:GPTEL_PARENT_SESSION_ID: %s\n:END:\n#+begin_user\n%s\n#+end_user\n"
          (symbol-name preset-sym) parent-id prompt))

(defun jf/gptel-persistent-agent--extract-final-text (agent-buffer)
  "Return trailing text of the last assistant turn in AGENT-BUFFER.
Returns the empty string when AGENT-BUFFER has been killed before
DONE fires, or when the last assistant turn has no text segment
\(Decision 2 of design.md: empty-text fallback)."
  (if (not (buffer-live-p agent-buffer))
      ""
    (with-current-buffer agent-buffer
      (let* ((turns     (gptel-chat-parse-buffer))
             (last-asst (cl-loop for turn in (reverse turns)
                                 when (eq (plist-get turn :role) 'assistant)
                                 return turn))
             (segments  (and last-asst (plist-get last-asst :segments)))
             (last-text (cl-loop for seg in (reverse (or segments '()))
                                 when (eq (plist-get seg :type) 'text)
                                 return (plist-get seg :content))))
        (or last-text "")))))

(defun jf/gptel-persistent-agent--make-on-done (main-cb agent-buffer)
  "Return a DONE FSM handler that returns the final assistant text to MAIN-CB.
AGENT-BUFFER is the agent's session buffer; the handler reads its last
assistant turn's last text segment via
`jf/gptel-persistent-agent--extract-final-text'."
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
MAIN-CB is the parent's tool callback.
AGENT-BUFFER is the agent's session buffer (for final-text extraction).

For WAIT and TOOL: prepend the local overlay updater
  (`jf/gptel-persistent-agent--indicate-wait',
  `jf/gptel-persistent-agent--indicate-tool-call').
For DONE/ERRS/ABRT: prepend the agent's terminal handler returning to
MAIN-CB."
  (mapcar
   (lambda (entry)
     (pcase (car entry)
       ('WAIT `(WAIT ,#'jf/gptel-persistent-agent--indicate-wait ,@(cdr entry)))
       ('TOOL `(TOOL ,#'jf/gptel-persistent-agent--indicate-tool-call ,@(cdr entry)))
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
  "Spawn an autonomous agent in a chat-mode session and return final text to MAIN-CB.

PRESET is the registered gptel preset name (string).
DESCRIPTION is a 3-5 word slug used in the directory and overlay header.
PROMPT is the initial user message text.
ALLOWED-PATHS is an optional vector or list of glob patterns; nil/empty ⇒
no read access (zero inheritance).

The agent's session.org is created with a self-describing
`:PROPERTIES:' drawer (preset + parent session id + scope keys),
opened with `find-file-noselect', and configured by the
find-file-hook auto-init pipeline. The agent's request runs through
chat-mode's public programmatic-send API with FSM handlers composed
for parent overlay feedback and final-text return."
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
           ;; Build the scope plist from allowed-paths + standard
           ;; denies, render the drawer-text-block (Mode 2a) carrying
           ;; preset + parent + :GPTEL_SCOPE_*: keys, and compose the
           ;; full session.org content as drawer + body. No scope.yml
           ;; is written — drawer-resident scope (cycle-2 task
           ;; rewire-persistent-agent).
           (scope-plist
            (jf/gptel-persistent-agent--build-scope-plist allowed-paths-list))
           (drawer-text
            (jf/gptel-scope-profile--render-drawer-text
             preset-sym parent-id scope-plist))
           (body (jf/gptel-persistent-agent--initial-body prompt))
           (initial-content (concat drawer-text body)))
      (let* ((session-file (jf/gptel--context-file-path session-dir))
             (_ (with-temp-file session-file
                  (insert initial-content)))
             (_ (jf/gptel--log 'info "Created agent session file: %s" session-file))
             (agent-buffer (find-file-noselect session-file))
             (overlay (jf/gptel-persistent-agent--task-overlay
                       where preset description)))
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
Use the read_file_in_scope tool on session.org to inspect the parent
session's :PROPERTIES: drawer (the :GPTEL_SCOPE_READ: and
:GPTEL_SCOPE_WRITE: keys); pass relevant paths to the agent via
allowed_paths. If allowed_paths is omitted or empty, the agent has NO
read access."
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
