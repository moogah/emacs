---
name: rebuild-persistent-agent-module
description: Full rewrite of persistent-agent.org/el on chat-mode pipeline; drop denied_paths and byte-identical overlay copies
change: persistent-agent-rebuild
status: ready
relations: []
---

## Files to modify

- `config/gptel/tools/persistent-agent.org` (rewrite — full replacement of the module body)
- `config/gptel/tools/persistent-agent.el` (tangled output — overwritten by tangle)

## Implementation steps

1. **Replace the file header `require` list** with the new dependency set:
   ```elisp
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
   ```

2. **Delete obsolete code**. Remove the following from the source `.org`:
   - `jf/gptel-persistent-agent--hrule` constant (no longer needed; upstream owns the overlay rendering)
   - `jf/gptel--auto-save-session-buffer` function and any hook installation of it
   - `jf/gptel-persistent-agent--create-overlay`
   - `jf/gptel-persistent-agent--indicate-wait`
   - `jf/gptel-persistent-agent--indicate-tool-call`
   - `jf/gptel-persistent-agent--fsm-handlers` (the defvar)
   - The current `jf/gptel-persistent-agent--task` body
   - The current `gptel-make-tool` registration

3. **Add `jf/gptel-persistent-agent--write-scope-file`**. Hand-format the YAML behind a named function. Decision 1 of design.md: this is intentionally a private wrapper, not a public scope-module API. The output shape matches what the current implementation produces:
   ```elisp
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
   ```

4. **Add `jf/gptel-persistent-agent--initial-content`**. Builds the drawer + populated user block (Decision 4 of design.md):
   ```elisp
   (defun jf/gptel-persistent-agent--initial-content (preset-sym parent-id prompt)
     "Build initial session.org content for an agent: drawer + populated user block.
   Format mirrors `jf/gptel--initial-session-content' (sessions/commands.el).
   Cross-reference: keep shapes aligned. The creation-spec test asserts
   on the full layout, so format drift in either function fails the test."
     (format ":PROPERTIES:\n:GPTEL_PRESET: %s\n:GPTEL_PARENT_SESSION_ID: %s\n:END:\n#+begin_user\n%s\n#+end_user\n"
             (symbol-name preset-sym) parent-id prompt))
   ```

5. **Add `jf/gptel-persistent-agent--extract-final-text`**. Reads the agent buffer's last assistant turn's last text segment (Decision 2 of design.md):
   ```elisp
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
   ```

6. **Add the three terminal-state handler builders**. Each returns a closure capturing `main-cb` and (for DONE) `agent-buffer`:
   ```elisp
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
   ```

7. **Add `jf/gptel-persistent-agent--build-fsm-handlers`**. Composes our handlers atop the chat-mode base. Order: agent first, then chat-mode lifecycle, then upstream:
   ```elisp
   (defun jf/gptel-persistent-agent--build-fsm-handlers (base main-cb agent-buffer)
     "Return an FSM handler alist composing agent handlers atop BASE.
   BASE is the chat-mode handler alist `gptel-chat-fsm-handlers`.
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
   ```

8. **Rewrite `jf/gptel-persistent-agent--task`** as the orchestrator. The new body validates, creates the session, opens it via `find-file-noselect` (Decision 3), parses, sends. No custom `:callback` — chat-mode's stream callback owns text insertion:
   ```elisp
   (defun jf/gptel-persistent-agent--task (main-cb preset description prompt
                                                    &optional allowed-paths)
     "Spawn an autonomous agent in a chat-mode session and return final text to MAIN-CB.

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
   ```

9. **Rewrite the tool registration**. Drop `denied_paths`. New `:args`:
   ```elisp
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
   ```
   Note: the `:enum` constraint on `preset` from the old registration is dropped here; the validation happens at task entry via `(gptel-get-preset preset-sym)`. If the existing tool registration's `:enum` form is desirable, the enum should be derived from `gptel--known-presets` at registration time — settle that within the task if it matters, or leave open enum.

10. **Footer** stays:
    ```elisp
    (provide 'gptel-persistent-agent)
    ;;; persistent-agent.el ends here
    ```

11. **Tangle** the file:
    ```
    ./bin/tangle-org.sh config/gptel/tools/persistent-agent.org
    ```

12. **Byte-compile clean**: load the tangled `.el` in a fresh Emacs and confirm no warnings:
    ```
    emacs --batch -L runtime/straight/build/gptel \
                  -L runtime/straight/build/gptel-agent \
                  -L config/gptel \
                  -L config/gptel/sessions \
                  -L config/gptel/chat \
                  -L config/gptel/tools \
                  -f batch-byte-compile config/gptel/tools/persistent-agent.el
    ```

## Design rationale

The persistent-agent rebuild is layer 2 of the change. Its core insight: an agent session is just a chat-mode session whose first user turn is provided programmatically and whose final assistant text is returned to a callback. Everything else — buffer creation, mode setup, drawer parsing, registry, autosave, streaming — is shared infrastructure that activates automatically when the session file is opened with `find-file-noselect`.

This rewrites away ~130 lines of reinvented infrastructure: hand-formatted YAML, manual buffer creation via `generate-new-buffer`, direct preset application, manual `set-visited-file-name`, custom auto-save hook, dual-duty `:callback` that re-implements `gptel--insert-response`, byte-identical local copies of upstream overlay handlers.

Decision points captured in design.md:
- Decision 1: scope.yml writing stays in this module (private wrapper, not promoted).
- Decision 2: empty final-text returns `""`; no sentinel, no error.
- Decision 3: `find-file-noselect` not `find-file` (parent retains focus).
- Decision 4: prompt embedded in `initial-content` passed to `create-session-core` (no post-creation buffer mutation).

## Design pattern

The handler-composition pattern (prepending agent handlers to chat-mode's chained list) mirrors what `config/gptel/chat/send.org` does for chat-mode itself: chat-mode's handlers chain ahead of upstream's. We add another layer ahead of chat-mode's. Order: agent → chat-lifecycle → upstream.

The closure-returning constructors (`--make-on-done` etc.) are an Emacs idiom for "stateful FSM handlers" — see `gptel-chat--stream-callback` in chat/stream.el for a parallel pattern (returns a closure capturing the insertion marker).

## Verification

- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` succeeds (paren validation passes).
- Tangled `.el` byte-compiles without warnings (command above).
- The rewritten file does NOT contain any of: `jf/gptel--auto-save-session-buffer`, `jf/gptel-persistent-agent--create-overlay`, `jf/gptel-persistent-agent--indicate-wait`, `jf/gptel-persistent-agent--indicate-tool-call`, `jf/gptel-persistent-agent--hrule`, `denied-paths`, `denied_paths`. (`grep` to confirm.)
- The tool args list contains exactly four entries: `preset`, `description`, `prompt`, `allowed_paths`.
- After loading, calling `(gptel-get-tool "PersistentAgent")` returns a non-nil tool struct.
- The agent module's tests (tasks 5–8) will exercise the actual behavior; this task confirms the module compiles and registers.

**Done means**: file rewritten with the eight new functions and updated tool registration, all obsolete code removed, byte-compile clean, ready for the dependent test tasks to exercise behavior.

## Context

design.md § "Layer 2: Persistent-agent rebuild" (function inventory + sketches)
design.md § "Decisions" 1-4
specs/persistent-agent/spec.md (delta) — every MODIFIED requirement maps to a piece of this implementation
architecture.md § "Components" → "Persistent-agent tool (rebuilt)"
