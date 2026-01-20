;;; tracing.el --- GPTEL Session Tracing -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Execution tracing for gptel sessions.
;; Logs tool calls, system prompt changes, and agent invocations.

;;; Code:

(require 'cl-lib)
(require 'jf-gptel-session-constants)
(require 'jf-gptel-session-logging)
(require 'jf-gptel-session-filesystem)

(defun jf/gptel--log-tool-call (tool-name args result &optional session-dir context-line subagent-link)
  "Log a tool call to the tools.md file.
TOOL-NAME is the name of the tool.
ARGS is the arguments plist or JSON.
RESULT is the tool result.
SESSION-DIR is optional; uses current session if nil.
CONTEXT-LINE is optional line number in context file.
SUBAGENT-LINK is optional path to subagent session (for Agent tool)."
  (let ((session-dir (or session-dir jf/gptel--session-dir)))
    (when session-dir
      (let ((tools-file (jf/gptel--tools-log-path session-dir))
            (timestamp (format-time-string "%a %b %d %H:%M:%S %Y")))
        (with-temp-buffer
          ;; Insert header
          (insert (format "* Tool Call: %s\n" tool-name))
          (insert "  :PROPERTIES:\n")
          (insert (format "  :TIMESTAMP: %s\n" timestamp))
          (when context-line
            (insert (format "  :CONTEXT_LINE: [[file:%s::%d]]\n"
                          jf/gptel-session--context-file
                          context-line)))
          (when subagent-link
            (insert (format "  :SUBAGENT_SESSION: [[file:%s]]\n" subagent-link)))
          (insert "  :END:\n\n")

          ;; Insert arguments
          (insert "** Arguments\n")
          (insert "#+begin_src json\n")
          (insert (if (stringp args)
                     args
                   (json-encode args)))
          (insert "\n#+end_src\n\n")

          ;; Insert result
          (insert "** Result\n")
          (insert "#+begin_example\n")
          (insert (if (stringp result)
                     result
                   (format "%S" result)))
          (insert "\n#+end_example\n\n")

          ;; Append to tools file
          (append-to-file (point-min) (point-max) tools-file))

        (jf/gptel--log 'debug "Logged tool call: %s" tool-name)))))

(defun jf/gptel--log-system-prompt-change (old-prompt new-prompt &optional session-dir)
  "Log system prompt change to system-prompts.md file.
OLD-PROMPT is the previous system prompt (or nil if first set).
NEW-PROMPT is the new system prompt.
SESSION-DIR is optional; uses current session if nil."
  (let ((session-dir (or session-dir jf/gptel--session-dir)))
    (when session-dir
      (let ((prompts-file (jf/gptel--system-prompts-log-path session-dir))
            (timestamp (format-time-string "%a %b %d %H:%M:%S %Y")))
        (with-temp-buffer
          ;; Insert header
          (insert (format "* System Prompt Change: %s\n" timestamp))
          (insert "  :PROPERTIES:\n")
          (insert (format "  :TIMESTAMP: %s\n" timestamp))
          (insert "  :END:\n\n")

          ;; Insert old prompt
          (when old-prompt
            (insert "** Previous Prompt\n")
            (insert "#+begin_example\n")
            (insert old-prompt)
            (insert "\n#+end_example\n\n"))

          ;; Insert new prompt
          (insert "** New Prompt\n")
          (insert "#+begin_example\n")
          (insert new-prompt)
          (insert "\n#+end_example\n\n")

          ;; Append to prompts file
          (append-to-file (point-min) (point-max) prompts-file))

        (jf/gptel--log 'debug "Logged system prompt change")))))

(defun jf/gptel--link-subagent-to-parent (subagent-dir parent-session-id)
  "Create bidirectional link between subagent and parent.
SUBAGENT-DIR is the subagent session directory.
PARENT-SESSION-ID is the parent session identifier.
Writes parent link to subagent metadata and logs to parent tools.md."
  (let ((subagent-metadata-file (jf/gptel--metadata-file-path subagent-dir)))
    ;; Write parent link to subagent metadata
    (with-temp-file (expand-file-name "parent.txt" subagent-dir)
      (insert parent-session-id))

    (jf/gptel--log 'debug "Linked subagent to parent: %s -> %s"
                  (file-name-nondirectory subagent-dir)
                  parent-session-id)))

(provide 'jf-gptel-session-tracing)
;;; tracing.el ends here
