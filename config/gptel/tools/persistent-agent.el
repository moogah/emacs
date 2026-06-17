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
(require 'gptel-session-commands)   ; jf/gptel--initial-session-body, sibling-file writer

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

(defun jf/gptel-persistent-agent--build-scope-plist (read-paths write-paths)
  "Build the agent's scope plist from READ-PATHS and WRITE-PATHS.
READ-PATHS and WRITE-PATHS are normalized lists of glob patterns
\(caller has converted any vector).

`:read' is the supplied READ-PATHS verbatim — nil/empty ⇒ no read
access (zero inheritance).  `:write' is the supplied WRITE-PATHS
with `/tmp/**' appended as the last element: `/tmp/**' is a
guaranteed scratch grant, NOT the write default (when WRITE-PATHS
is nil/empty the agent gets scratch-only write).  `:deny' is the
standard set from `jf/gptel-persistent-agent--standard-deny-paths'.

Returns a plist of `register/shape/scope-config-plist' shape:
  (:paths (:read (...) :write (... \"/tmp/**\") :deny (<standard set>)))

Caller renders this via
`jf/gptel-scope-profile--render-drawer-text' to obtain the
`:PROPERTIES:' drawer-text-block embedded in the agent's
`session.org'."
  (list :paths
        (list :read  (or read-paths nil)
              :write (append write-paths '("/tmp/**"))
              :deny  jf/gptel-persistent-agent--standard-deny-paths)))

(defun jf/gptel-persistent-agent--initial-body (prompt)
  "Build the user-block body of a fresh agent session.org.

PROMPT becomes the body of the first `#+begin_user' block.
Returns a string of the form

  #+begin_user
  <prompt>
  #+end_user

Caller is expected to prepend a `:PROPERTIES:' drawer rendered by
`jf/gptel-scope-profile--render-drawer-text' (Mode 2a) before
writing the file, so that the composed
`(concat drawer-text body)' carries exactly one file-level
`:PROPERTIES:' / `:END:' pair at point-min
\(`register/invariant/scope-drawer-no-duplication') followed by
the user turn block."
  (format "#+begin_user\n%s\n#+end_user\n" prompt))

(defconst jf/gptel-persistent-agent--system-preamble
  "You are an autonomous sub-agent launched by a parent agent to carry out one task.

Operating rules:
- Do the task YOURSELF, directly, using the tools available to you. Do
  NOT delegate it to another agent — never call the PersistentAgent
  tool to perform your work. You are the agent that does the work.
- You run headless and cannot ask the user follow-up questions. When
  something is ambiguous, make a reasonable assumption, state it, and
  proceed.
- Stay within the task you were given and the file scope you were
  granted. If an operation is refused as out of scope and the task
  genuinely needs it, request a scope expansion rather than abandoning
  the task.
- When the task is complete, STOP and write a single final message
  that fully answers it (results, file paths, and anything the parent
  needs). That final message is the ONLY thing returned to the parent,
  so make it self-contained — do not rely on intermediate tool output
  being visible upstream."
  "Baseline system-prompt preamble prepended to every persistent agent.

Prepended ahead of the agent preset's `:system' by
`jf/gptel-persistent-agent--write-system-prompt'.  Tells the spawned
model that it is a headless sub-agent that must do the work itself
\(not delegate via `PersistentAgent'), cannot ask follow-up questions,
and must terminate with one self-contained final message — the only
text returned to the parent.")

(defun jf/gptel-persistent-agent--write-system-prompt (session-dir preset-name preset-spec)
  "Write the agent's `system-prompt.<ext>' into SESSION-DIR; return its basename.

Composes the agent's effective system prompt as the baseline
`jf/gptel-persistent-agent--system-preamble' followed by the preset's
`:system' body (PRESET-SPEC is the plist from `gptel-get-preset').
When the preset declares no non-empty `:system', the file is the
preamble alone.  PRESET-NAME resolves the file extension via
`jf/gptel--preset-source-file-extension', matching the sibling-file
convention used by interactive sessions.

Always writes (the preamble is always present) and always returns the
basename, so the caller always threads a `:GPTEL_SYSTEM_PROMPT_FILE:'
drawer key — every agent has a system prompt.  Uses `write-region' to
avoid mutating buffer-local state (mirrors
`jf/gptel--write-system-prompt-sibling-file' §Decision 3)."
  (let* ((preset-system (plist-get preset-spec :system))
         (body (if (and (stringp preset-system)
                        (not (string-blank-p preset-system)))
                   (concat jf/gptel-persistent-agent--system-preamble
                           "\n\n" preset-system)
                 jf/gptel-persistent-agent--system-preamble))
         (ext (jf/gptel--preset-source-file-extension preset-name))
         (basename (concat "system-prompt." ext))
         (path (expand-file-name basename session-dir)))
    (write-region body nil path nil 'silent)
    basename))

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
opened with `find-file-noselect', and configured by content-addressed
activation: the drawer signature drives `magic-mode-alist' into
`gptel-chat-mode', whose mode hook binds the buffer-local session vars.
The agent's request runs through
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
           ;; denies, resolve the agent's preset spec for the chat-
           ;; mode snapshot keys (Decision 4 / Layer 2 of gptel-
           ;; drawer-as-source-of-truth), render the drawer-text-block
           ;; (Mode 2a) carrying preset + parent + the chat-mode
           ;; snapshot + :GPTEL_SCOPE_*: keys, and compose the full
           ;; session.org content as drawer + body. :GPTEL_SYSTEM: is
           ;; never emitted (Decision 2). No scope.yml is written —
           ;; drawer-resident scope (cycle-2 task
           ;; rewire-persistent-agent).
           (scope-plist
            (jf/gptel-persistent-agent--build-scope-plist allowed-paths-list))
           (preset-spec (and preset-sym
                             (fboundp 'gptel-get-preset)
                             (gptel-get-preset preset-sym)))
           ;; Materialise the agent's effective system prompt as
           ;; `system-prompt.<ext>' next to `session.org': the baseline
           ;; agent harness preamble followed by the preset's `:system'
           ;; body (Agent System-Prompt Preamble section).  Unlike the
           ;; shared interactive writer this ALWAYS writes (the preamble
           ;; is always present) and always returns a basename, so every
           ;; agent gets a system prompt and a `:GPTEL_SYSTEM_PROMPT_FILE:'
           ;; drawer key.  The agent directory has no `branches/'
           ;; subdirectory (constants.el: agents nest directly under the
           ;; parent branch-dir), so SESSION-DIR is the directory the
           ;; sibling file lands in.
           (sibling-basename
            (jf/gptel-persistent-agent--write-system-prompt
             session-dir preset-sym preset-spec))
           (drawer-text
            (jf/gptel-scope-profile--render-drawer-text
             preset-sym parent-id scope-plist preset-spec))
           ;; Thread the sibling file's basename into the drawer as
           ;; `:GPTEL_SYSTEM_PROMPT_FILE:' so chat-mode restore can
           ;; resolve it relative to `session.org's directory.
           (drawer-text (if sibling-basename
                            (jf/gptel--append-drawer-property
                             drawer-text "GPTEL_SYSTEM_PROMPT_FILE"
                             sibling-basename)
                          drawer-text))
           ;; Emit the agent's OWN identity keys
           ;; (register/vocabulary/identity-drawer-keys): its own
           ;; `:GPTEL_SESSION_ID:' and `:GPTEL_BRANCH: main'.  At
           ;; creation the agent's `session.org' does not yet exist, so
           ;; there is no drawer to read; `jf/gptel--resolve-session-id'
           ;; is called with a nil DRAWER-ALIST and resolves through its
           ;; sanctioned basename fallback — the agent directory's
           ;; basename IS the canonical id at mint time
           ;; (register/boundary/drawer-first-identity-resolution).
           ;; Routing through the resolver (rather than calling
           ;; `jf/gptel--session-id-from-directory' directly) keeps this
           ;; the SINGLE content-first identity seam.  These sit alongside the
           ;; `:GPTEL_PARENT_SESSION_ID:' already rendered by
           ;; `--render-drawer-text' from PARENT-ID above — the uniform
           ;; identity rule (design.md D3) is that an agent carries its
           ;; OWN id AND the parent link, so session TYPE is inferable
           ;; from parent-id presence (register/boundary/drawer-first-
           ;; identity-resolution).  Spliced via the append helper to
           ;; preserve the drawer's `:PROPERTIES:' / `:END:' adjacency
           ;; invariant (register/shape/drawer-text-block).
           (agent-session-id (jf/gptel--resolve-session-id nil session-dir))
           (drawer-text (jf/gptel--append-drawer-property
                         drawer-text "GPTEL_SESSION_ID" agent-session-id))
           (drawer-text (jf/gptel--append-drawer-property
                         drawer-text "GPTEL_BRANCH" "main"))
           ;; Compose the agent session.org body: drawer + populated
           ;; user block.  No headings are emitted; the preset's
           ;; `:system' lives in the sibling file resolved via
           ;; `:GPTEL_SYSTEM_PROMPT_FILE:'.
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
 ;; :confirm t intentionally omitted: gptel-chat-mode lacks a
 ;; tool-confirm UI for :confirm t tools, causing the FSM to hang in
 ;; TOOL state with an empty rendered tool block. Tracked in
 ;; .tasks/chat-mode-tool-confirm-ui-missing.md. Restore :confirm t
 ;; once that .tasks/ item lands.
 :include t)

(provide 'gptel-persistent-agent)
;;; persistent-agent.el ends here
