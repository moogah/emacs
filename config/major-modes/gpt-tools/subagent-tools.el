;;; subagent-tools.el --- GPTEL tools for subagent delegation -*- lexical-binding: t; -*-
;;
;; Author: Jeff Farr
;; Keywords: tools, ai, delegation
;;
;;; Commentary:
;;
;; Provides subagent delegation capability for gptel, enabling primary agents
;; to delegate specialized tasks to isolated subagents with their own tool sets.
;;
;;; Code:

;; No requires needed - gptel is loaded before this module

(defvar-local jf/gptel--delegation-depth 0
  "Current depth of subagent delegation.
Used to prevent infinite recursion by tracking how many levels deep
we are in nested subagent calls.")

(defvar jf/gptel-max-delegation-depth 3
  "Maximum allowed subagent delegation depth.
Prevents excessive nesting that could lead to runaway delegation chains.
Set to 3 by default to allow: primary -> plan-agent -> explore-agent.")

(gptel-make-preset 'explore-agent
  :description "Subagent for read-only code exploration and semantic analysis"
  :backend "Claude"
  :model 'claude-3-7-sonnet-20250219
  :system "You are an exploration agent. Your role is to gather semantic information about code, summarize functionality, implementation details, and architecture. You have read-only access to the codebase.

Focus on:
- Understanding code structure and organization
- Identifying key functions, classes, and modules
- Tracing data flow and dependencies
- Summarizing implementation approaches
- Noting architectural patterns

You can read files, navigate projects with projectile, use ggtags for semantic navigation, and search the web for context."
  :tools '(;; Filesystem tools
           "read_file"
           "list_directory"
           ;; Projectile tools
           "list_known_projects"
           "get_project_info"
           "list_project_files"
           "list_project_directories"
           "search_project_content"
           ;; Ggtags tools
           "check_ggtags_project"
           "find_definition"
           "find_references"
           "find_symbol"
           "grep_project")
  :temperature 0.5)

(gptel-make-preset 'plan-agent
  :description "Subagent for planning and requirements gathering with delegation"
  :backend "Claude"
  :model 'claude-3-7-sonnet-20250219
  :system "You are a planning agent. Your role is to understand user requirements, develop detailed implementation plans, and delegate exploration tasks when needed.

Focus on:
- Gathering requirements through dialog
- Breaking down complex tasks into steps
- Identifying what information is needed
- Delegating exploration to explore-agent when you need code analysis
- Including questions in your result when you need user input

When you need the user to answer questions, include them clearly in your response
formatted for the primary agent to forward to the user.

When you need to understand code structure or gather information about a codebase,
use the invoke_subagent tool to delegate to explore-agent."
  :tools '("invoke_subagent"
           "read_file"
           "list_directory")
  :temperature 0.7)

(defun jf/gptel--format-subagent-result (agent-profile task response)
  "Format subagent final response for parent agent consumption.
AGENT-PROFILE is the profile name used (e.g., 'explore-agent').
TASK is the task description that was delegated.
RESPONSE is the final response from the subagent."
  (format "=== Subagent Result ===
Agent: %s
Task: %s

%s

=== End Subagent Result ==="
          agent-profile task response))

(defun jf/gptel-invoke-subagent (callback agent-profile task-description
                                 &optional context-files)
  "Invoke a subagent with AGENT-PROFILE to handle TASK-DESCRIPTION.
CALLBACK is called with the result when subagent completes.
CONTEXT-FILES is optional list of file paths to include in context.

The subagent runs in a visible buffer with its own tools, backend, model,
and system message as defined by the profile. Results are streamed back
and the buffer remains open for inspection."

  ;; 1. Resolve preset
  (let* ((preset (alist-get (intern agent-profile) gptel--known-presets))
         (buffer-name (format "*gptel-subagent-%s-%s*"
                             agent-profile
                             (format-time-string "%Y%m%d-%H%M%S")))
         (parent-buffer (current-buffer)))

    ;; Validation
    (unless preset
      (funcall callback (format "Error: Unknown agent profile '%s'" agent-profile))
      (cl-return-from jf/gptel-invoke-subagent))

    ;; Depth check
    (when (>= (or jf/gptel--delegation-depth 0)
              jf/gptel-max-delegation-depth)
      (funcall callback
               (format "Error: Maximum delegation depth (%d) reached. Cannot delegate further to avoid runaway chains."
                       jf/gptel-max-delegation-depth))
      (cl-return-from jf/gptel-invoke-subagent))

    ;; 2. Create visible buffer
    (with-current-buffer (get-buffer-create buffer-name)
      ;; Enable gptel-mode to get buffer-local state
      (markdown-mode)
      (gptel-mode 1)

      ;; 3. Apply preset to buffer (sets tools, backend, model, system)
      (gptel--apply-preset preset)

      ;; Validate that preset was applied correctly
      (unless gptel-backend
        (with-current-buffer parent-buffer
          (funcall callback "Error: Backend not set after applying preset"))
        (kill-buffer (current-buffer))
        (cl-return-from jf/gptel-invoke-subagent))

      (unless gptel-model
        (with-current-buffer parent-buffer
          (funcall callback "Error: Model not set after applying preset"))
        (kill-buffer (current-buffer))
        (cl-return-from jf/gptel-invoke-subagent))

      ;; 4. Track delegation depth
      (setq-local jf/gptel--delegation-depth
                  (1+ (with-current-buffer parent-buffer
                        (or jf/gptel--delegation-depth 0))))

      ;; 5. Set up context if provided
      (when context-files
        (setq-local gptel-context
                    (mapcar (lambda (f) (list :source f)) context-files)))

      ;; 5.5. Insert the task into the buffer (gptel expects buffer content)
      (goto-char (point-max))
      (insert "# Task\n\n" task-description "\n\n")

      ;; 6. Build callback wrapper
      (let ((result-accumulator "")
            (had-tool-calls nil))  ; Track if we've seen tool calls
        (letrec ((subagent-callback
                  (lambda (response info)
                    (message "Subagent callback: response type=%s, length=%s, had-tools=%s"
                             (type-of response)
                             (if (stringp response) (length response) "N/A")
                             had-tool-calls)
                    (cond
                     ;; Streaming: accumulate chunks
                     ((stringp response)
                      (setq result-accumulator
                            (concat result-accumulator response)))

                     ;; Success: but check if there are pending tool calls
                     ((eq response t)
                      ;; Check info plist for tool-use - it's set BEFORE this callback
                      (let* ((tool-use (plist-get info :tool-use))
                             (has-pending-tools (and tool-use (> (length tool-use) 0))))
                        (message "Subagent response complete: pending-tools=%s, result length=%d"
                                 has-pending-tools (length result-accumulator))
                        (if (or has-pending-tools had-tool-calls)
                            ;; Tool calls pending or were made, more turns coming
                            (progn
                              (message "Subagent has tool calls, waiting for continuation...")
                              (setq had-tool-calls nil))
                          ;; No tool calls, this is the final response
                          (progn
                            (message "Subagent FULLY completed! Returning to parent.")
                            (with-current-buffer parent-buffer
                              (funcall callback
                                       (jf/gptel--format-subagent-result
                                        agent-profile task-description result-accumulator)))))))

                     ;; Tool calls: let FSM handle and set flag
                     ((and (consp response)
                           (memq (car response) '(tool-call tool-result)))
                      (message "Subagent tool operation detected: %s" (car response))
                      (setq had-tool-calls t)
                      nil)

                     ;; Error or abort
                     (t
                      (with-current-buffer parent-buffer
                        (funcall callback
                                 (format "Subagent error: %s"
                                         (plist-get info :status)))))))))

          ;; 7. Fire the request
          (message "Subagent firing request in buffer %s with backend=%s model=%s"
                   (buffer-name) gptel-backend gptel-model)
          ;; Use buffer as the prompt source, not the string
          ;; This is more reliable for streaming mode
          (gptel-request nil  ; nil means use buffer content
                         :buffer (current-buffer)
                         :position (point-marker)
                         :stream t
                         :callback subagent-callback))))))

(gptel-make-tool
 :name "invoke_subagent"
 :function #'jf/gptel-invoke-subagent
 :async t
 :description "Delegate a task to a specialized subagent running in an isolated context with its own tools and configuration.

Use this to break complex tasks into focused subtasks that benefit from specialized tool access or system prompts.

The subagent will execute independently, use its tools as needed, and return a final result summarizing its findings.

Available agents:
- explore-agent: Read-only code investigation with filesystem, projectile, ggtags, and web tools. Use for analyzing code structure, understanding implementations, or gathering information about a codebase.
- plan-agent: Planning and requirements gathering with delegation capability. Use for developing implementation plans or coordinating multiple exploration tasks.

Important: The subagent's full conversation history is NOT returned to you - only its final response. The subagent may use multiple tool calls internally but you'll just see the summary."
 :args (list
        '(:name "agent_profile"
          :type string
          :enum ["explore-agent" "plan-agent"]
          :description "Which subagent profile to use. explore-agent: read-only code investigation. plan-agent: planning with delegation.")
        '(:name "task_description"
          :type string
          :description "Clear, specific task for the subagent. Include context about what you need and what deliverables you expect. Be specific about the scope and any constraints.")
        '(:name "context_files"
          :type array
          :items (:type string)
          :optional t
          :description "Optional list of file paths to include in the subagent's context. These files will be available for the subagent to reference."))
 :category "delegation"
 :confirm t)

(message "Loaded subagent-tools: presets=%S"
         (mapcar #'car (cl-remove-if-not
                        (lambda (p) (memq (car p) '(explore-agent plan-agent)))
                        gptel--known-presets)))

(provide 'subagent-tools)
;;; subagent-tools.el ends here
