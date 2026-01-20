;;; jf-gptel-session-subagent.el --- GPTEL Subagent Sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Subagent session integration.
;; Creates nested session directories when Agent tool is invoked from a parent session.

;;; Code:

(require 'cl-lib)
(require 'jf-gptel-session-constants)
(require 'jf-gptel-session-logging)
(require 'jf-gptel-session-filesystem)
(require 'jf-gptel-session-registry)
(require 'jf-gptel-session-metadata)
(require 'jf-gptel-session-tracing)

(defun jf/gptel--create-subagent-session (parent-session-dir agent-type description)
  "Create a subagent session directory under PARENT-SESSION-DIR.
AGENT-TYPE is the type of agent (e.g., 'researcher', 'executor').
DESCRIPTION is a brief task description.
Returns the absolute path to the created subagent directory."
  (let* ((slug (replace-regexp-in-string "[^a-z0-9-]" "-" (downcase description)))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (dirname (format "%s-%s-%s" agent-type timestamp slug))
         (subagents-dir (jf/gptel--subagents-dir-path parent-session-dir))
         (subagent-dir (expand-file-name dirname subagents-dir)))

    ;; Create subagents directory if needed
    (unless (file-directory-p subagents-dir)
      (make-directory subagents-dir t))

    ;; Create subagent session directory
    (make-directory subagent-dir t)

    (jf/gptel--log 'info "Created subagent session: %s" dirname)
    subagent-dir))

(defun jf/gptel--create-subagent-metadata (subagent-dir parent-session-id agent-type description)
  "Create metadata for subagent session.
SUBAGENT-DIR is the subagent directory.
PARENT-SESSION-ID is the parent session identifier.
AGENT-TYPE and DESCRIPTION describe the subagent task."
  (let* ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
         (metadata (list :type "subagent"
                        :parent-session parent-session-id
                        :agent-type agent-type
                        :description description
                        :created timestamp)))

    ;; Write metadata as JSON
    (let ((metadata-file (jf/gptel--metadata-file-path subagent-dir))
          (json-content (json-encode metadata)))
      (with-temp-file metadata-file
        (insert json-content)))

    (jf/gptel--log 'debug "Created subagent metadata: parent=%s" parent-session-id)
    metadata))

(defvar jf/gptel--current-parent-session nil
  "Dynamically bound to parent session directory during subagent execution.")

(defvar jf/gptel--current-subagent-session nil
  "Dynamically bound to subagent session directory during subagent execution.
Used to log subagent tool calls to the subagent's own tools.org.")

(defun jf/gptel--write-subagent-prompt (subagent-dir agent-type description prompt)
  "Write initial prompt to SUBAGENT-DIR context.md.
AGENT-TYPE, DESCRIPTION, and PROMPT describe the task."
  (let ((context-file (jf/gptel--context-file-path subagent-dir))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-file context-file
      (insert (format "# %s Task: %s\n\n" (capitalize agent-type) description))
      (insert (format "*Started: %s*\n\n" timestamp))
      (insert "## Task Prompt\n\n")
      (insert prompt)
      (insert "\n\n## Agent Response\n\n"))))

(defun jf/gptel--write-subagent-result (subagent-dir result)
  "Append RESULT to SUBAGENT-DIR context.md."
  (let ((context-file (jf/gptel--context-file-path subagent-dir))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (when (file-exists-p context-file)
      (with-temp-buffer
        (insert-file-contents context-file)
        (goto-char (point-max))
        (insert result)
        (insert (format "\n\n*Completed: %s*\n" timestamp))
        (write-region (point-min) (point-max) context-file)))))

(defun jf/gptel--advice-agent-task (args)
  "Advice for gptel-agent--task to capture parent session context.
ARGS is the list (MAIN-CB AGENT-TYPE DESCRIPTION PROMPT).
Creates subagent session and wraps callback to capture final result."
  (if jf/gptel--session-dir
      (let* ((main-cb (nth 0 args))
             (agent-type (nth 1 args))
             (description (nth 2 args))
             (prompt (nth 3 args))
             (subagent-dir (jf/gptel--create-subagent-session
                           jf/gptel--session-dir
                           agent-type
                           description))
             (subagent-id (jf/gptel--session-id-from-directory subagent-dir))
             (wrapped-cb
              (lambda (result)
                "Wrapped callback that saves result to context.md then calls original."
                (when (stringp result)
                  (jf/gptel--write-subagent-result subagent-dir result))
                (funcall main-cb result))))

        ;; Create subagent metadata
        (jf/gptel--create-subagent-metadata
         subagent-dir
         jf/gptel--session-id
         agent-type
         description)

        ;; Link subagent to parent
        (jf/gptel--link-subagent-to-parent subagent-dir jf/gptel--session-id)

        ;; Write initial prompt to context.md
        (jf/gptel--write-subagent-prompt subagent-dir agent-type description prompt)

        (jf/gptel--log 'info "Subagent session created: %s" subagent-id)

        ;; Store subagent session dir for gptel-request advice to use
        (setq jf/gptel--current-parent-session jf/gptel--session-dir)
        (setq jf/gptel--current-subagent-session subagent-dir)

        ;; Return modified args with wrapped callback
        (list wrapped-cb agent-type description prompt))
    ;; No session active, return original arguments unchanged
    args))

;; Install advice when gptel-agent is loaded
(with-eval-after-load 'gptel-agent
  (when (fboundp 'gptel-agent--task)
    (advice-add 'gptel-agent--task :filter-args
               #'jf/gptel--advice-agent-task)
    (jf/gptel--log 'info "Installed subagent session advice")))

(defun jf/gptel--advice-inject-session-to-fsm (orig-fun &rest args)
  "Around advice for gptel-request to inject subagent session into FSM.
When `jf/gptel--current-subagent-session' is set, adds it to the FSM info
so tool logging can find the correct session directory.
Handles both positional and keyword arguments properly."
  (if jf/gptel--current-subagent-session
      (let* ((prompt (if (and args (not (keywordp (car args))))
                        (car args)
                      nil))
             (keyword-args (if prompt (cdr args) args))
             (fsm-arg (plist-get keyword-args :fsm))
             (fsm (or fsm-arg (gptel-make-fsm)))
             (session-dir jf/gptel--current-subagent-session))

        ;; Update keyword args with FSM
        (setq keyword-args (plist-put keyword-args :fsm fsm))

        ;; Clear the dynamic variable before calling orig-fun
        (setq jf/gptel--current-subagent-session nil)

        (jf/gptel--log 'info "About to inject subagent session dir: %s" session-dir)

        ;; Call the original function to let it set up FSM
        (let ((result (if prompt
                          (apply orig-fun prompt keyword-args)
                        (apply orig-fun keyword-args))))

          ;; NOW inject session directory into the populated FSM info
          (let ((info (gptel-fsm-info fsm)))
            (jf/gptel--log 'debug "FSM info after gptel-request: %S" info)
            (setq info (plist-put info :jf/session-dir session-dir))
            (setf (gptel-fsm-info fsm) info)
            (jf/gptel--log 'info "Injected subagent session dir into FSM: %s" session-dir)
            (jf/gptel--log 'debug "FSM info after injection: %S" (gptel-fsm-info fsm)))

          result))
    ;; No subagent session active, proceed normally
    (apply orig-fun args)))

;; Install advice on gptel-request
(with-eval-after-load 'gptel
  (when (fboundp 'gptel-request)
    (advice-add 'gptel-request :around
               #'jf/gptel--advice-inject-session-to-fsm)
    (jf/gptel--log 'info "Installed gptel-request session injection advice")))

(defun jf/gptel--get-subagent-link-for-tool (tool-name tool-call)
  "Get subagent session link if TOOL-NAME is 'Agent'.
TOOL-CALL is the tool call plist.
Returns path to subagent session directory or nil."
  (when (equal tool-name "Agent")
    ;; Extract description from args to find the subagent directory
    (when-let* ((args (plist-get tool-call :args))
                (description (plist-get args :description)))
      ;; Find most recent matching subagent directory
      (let* ((subagents-dir (jf/gptel--subagents-dir-path jf/gptel--session-dir))
             (subdirs (when (file-directory-p subagents-dir)
                       (directory-files subagents-dir t "^[^.]")))
             (matching (seq-filter
                       (lambda (dir)
                         (string-match-p (regexp-quote
                                         (replace-regexp-in-string "[^a-z0-9-]" "-"
                                                                  (downcase description)))
                                        (file-name-nondirectory dir)))
                       subdirs)))
        ;; Return most recent (last in sorted list)
        (car (last matching))))))

(provide 'jf-gptel-session-subagent)
;;; jf-gptel-session-subagent.el ends here
