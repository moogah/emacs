;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;;  (use-package shell-maker
;;    :straight (shell-maker :type git :host github :repo "xenodium/shell-maker" :files ("shell-maker.el")))
;;
;;  ; add :build (:not compile) if byte code has problems
;;  (use-package chatgpt-shell
;;    :requires shell-maker
;;    :straight (:host github :repo "xenodium/chatgpt-shell" :files ("*.el") :build (:not compile)))
;;
;;  (setq chatgpt-shell-openai-key
;;        (lambda ()
;;          (auth-source-pick-first-password :host "api.openai.com")))
;;
;;  ;;(setq chatgpt-shell-model-version "gpt-4o")

(use-package gptel
  :straight t
  :custom
  (gptel-model 'gpt-4o) ;; model is now a symbol, not a string
  (gptel-log-level 'debug) ;; Enable debug logging for testing
  :config
  ;; Enable prompt caching for Anthropic models
  (setq gptel-cache t)
  ;; Enable expert commands to show advanced options in transient menu
  (setq gptel-expert-commands t)
  ;; Configure Perplexity backend
  (gptel-make-perplexity "Perplexity"
    :key (lambda () (auth-source-pick-first-password :host "api.perplexity.ai"))
    :stream t)
  
  ;; Configure Anthropic backend with curated Claude 4.5 models
  ;; Includes lightweight (Haiku), balanced (Sonnet), and heavyweight (Opus) options
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda () (auth-source-pick-first-password :host "api.anthropic.com"))
    :models '(claude-haiku-4-5-20251001      ; Fast & economical for simple tasks
              claude-sonnet-4-5-20250929     ; Balanced for most use cases
              claude-opus-4-5-20251101))     ; Most capable for complex reasoning

  ;; Configure Claude Sonnet 4.5 with extended thinking mode for complex problems
  ;; Thinking mode provides detailed reasoning before answering
  (gptel-make-anthropic "Claude-thinking"
    :key (lambda () (auth-source-pick-first-password :host "api.anthropic.com"))
    :stream t
    :models '(claude-sonnet-4-5-20250929)
    :header (lambda ()
              (let ((key (gptel--get-api-key
                          (lambda () (auth-source-pick-first-password :host "api.anthropic.com")))))
                `(("x-api-key" . ,key)
                  ("anthropic-version" . "2023-06-01")
                  ("anthropic-beta" . "pdfs-2024-09-25")
                  ("anthropic-beta" . "output-128k-2025-02-19")
                  ("anthropic-beta" . "prompt-caching-2024-07-31"))))
    :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                      :max_tokens 4096))

  ;; Configure ChatGPT/OpenAI backend
  (gptel-make-openai "ChatGPT"
    :key (lambda () (auth-source-pick-first-password :host "api.openai.com"))
    :stream t
    :models '(gpt-4o gpt-4o-mini gpt-4-turbo gpt-3.5-turbo)))

(use-package gptel-agent
  :straight t
  :after gptel
  :demand t
  :config
  ;; Add our custom agent directory to the scan path
  (add-to-list 'gptel-agent-dirs
               (expand-file-name "config/major-modes/gpt-tools/agents/" jf/emacs-dir))

  ;; Load custom tools BEFORE agent update so agents can reference them
  (jf/load-module (expand-file-name "config/major-modes/gpt-tools/filesystem-tools.el" jf/emacs-dir))
  (jf/load-module (expand-file-name "config/major-modes/gpt-tools/projectile-tools.el" jf/emacs-dir))
  (jf/load-module (expand-file-name "config/major-modes/gpt-tools/ggtags-tools.el" jf/emacs-dir))
  (jf/load-module (expand-file-name "config/major-modes/gpt-tools/treesitter-tools.el" jf/emacs-dir))
  (jf/load-module (expand-file-name "config/major-modes/gpt-tools/org-roam-tools.el" jf/emacs-dir))
  (jf/load-module (expand-file-name "config/major-modes/gpt-tools/meta-tools.el" jf/emacs-dir))

  ;; Load community tools (other than gptel-agent which is loaded above)
  (jf/load-module (expand-file-name "config/major-modes/gpt-tools/community-tools.el" jf/emacs-dir))

  ;; Scan and register agents from all configured directories
  (gptel-agent-update)

  ;; Enable Agent tool by default for all gptel sessions
  ;; This allows the main LLM to delegate tasks to specialized agents
  (setq-default gptel-tools (list (gptel-get-tool "Agent"))))

(defun jf/gptel-agent--expand-skills (system-text)
  "Expand @skill mentions in SYSTEM-TEXT using gptel-skills.
Returns updated system text with skill content injected.
If skills system not loaded or no mentions found, returns text unchanged."
  (if (and (fboundp 'jf/gptel-skills--detect-mentions)
           (boundp 'jf/gptel-skills--registry)
           (string-match-p "@" system-text))
      (with-temp-buffer
        (insert system-text)
        (let* ((mentions-data (jf/gptel-skills--detect-mentions (current-buffer)))
               (skill-names (mapcar #'car mentions-data)))
          (when skill-names
            ;; For each unique skill, replace @mention with content
            (dolist (skill-name (delete-dups skill-names))
              (let* ((metadata (gethash skill-name jf/gptel-skills--registry))
                     (skill-path (plist-get metadata :path))
                     (skill-content (when skill-path
                                     (jf/gptel-skills--parse-content skill-path))))
                (when skill-content
                  (goto-char (point-min))
                  (while (re-search-forward
                          (concat "@" (regexp-quote skill-name) "\\>")
                          nil t)
                    (replace-match skill-content t t))))))
          (buffer-string)))
    ;; No skills system or no mentions - return unchanged
    system-text))

(defun jf/gptel-agent--expand-all-agent-skills ()
  "Expand @skill mentions in all registered agent system prompts.
Run this after gptel-agent-update to inject skill content into agents."
  (when (and (boundp 'gptel-agent--agents)
             (fboundp 'jf/gptel-skills--discover))
    ;; Ensure skills are discovered first
    (jf/gptel-skills--discover)

    ;; Process each agent
    (dolist (agent-entry gptel-agent--agents)
      (let* ((agent-name (car agent-entry))
             (plist (cdr agent-entry))
             (system (plist-get plist :system)))
        (when (and system (stringp system))
          (let ((expanded (jf/gptel-agent--expand-skills system)))
            (unless (string= expanded system)
              (plist-put plist :system expanded)
              (when jf/gptel-skills-verbose
                (message "Expanded skills in agent: %s" agent-name)))))))))

;; Hook into gptel-agent-update to auto-expand skills
(with-eval-after-load 'gptel-agent
  (advice-add 'gptel-agent-update :after #'jf/gptel-agent--expand-all-agent-skills))

(defun jf/gptel--trace-agent-task (orig-fn main-cb agent-type description prompt)
  "Advice around `gptel-agent--task' to capture execution trace.
Passes session-id to subagent via :context, records agent lifecycle."
  ;; Get session-id from parent buffer (buffer-local variable)
  (let ((session-id jf/gptel--session-id))
    (if (not session-id)
        ;; No active session, run agent without tracing
        (funcall orig-fn main-cb agent-type description prompt)
      ;; Create trace entry using registry
      (let* ((parent-trace-id (jf/gptel--current-trace-id session-id))
             (trace-id (jf/gptel--create-trace session-id agent-type description
                                               prompt parent-trace-id)))
        ;; Wrap callback to capture completion
        (funcall orig-fn
                 (lambda (result)
                   (let ((status (cond
                                  ((null result) "error")
                                  ((string-prefix-p "Error:" result) "error")
                                  (t "completed"))))
                     ;; Complete trace in registry
                     (jf/gptel--complete-trace session-id trace-id status)
                     ;; Call original callback
                     (funcall main-cb result)))
                 agent-type description prompt)))))

(defun jf/gptel--inject-session-context (orig-fn prompt &rest args)
  "Advice around `gptel-request' to inject session-id into :context.
This allows subagents in separate buffers to access session data via registry."
  (let ((session-id jf/gptel--session-id) ; From parent buffer
        (context (plist-get args :context)))
    (if (not session-id)
        ;; No session, call original
        (apply orig-fn prompt args)
      ;; Inject session-id into context
      (let* ((new-context (if (listp context)
                             (plist-put (copy-sequence context) :session-id session-id)
                           (list :session-id session-id)))
             (new-args (plist-put (copy-sequence args) :context new-context)))
        (apply orig-fn prompt new-args)))))

;; Install advice after packages load
(with-eval-after-load 'gptel-agent
  (advice-add 'gptel-agent--task :around #'jf/gptel--trace-agent-task))

(with-eval-after-load 'gptel
  (advice-add 'gptel-request :around #'jf/gptel--inject-session-context))

;; DEPRECATED: Legacy subagent implementation
;; Kept for reference only - functionality replaced by gptel-agent
;; (jf/load-module (expand-file-name "config/major-modes/gpt-tools/subagent-tools.el" jf/emacs-dir))

;; Load skills system
(jf/load-module (expand-file-name "config/major-modes/gptel-skills.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/major-modes/gptel-skills-roam.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/major-modes/gptel-skills-transient.el" jf/emacs-dir))

(defun jf/gptel-launcher ()
  "Launch gptel session with a selected backend and model.
Prompts for display method, then backend:model selection using
completing-read with all available options from gptel's configuration."
  (interactive)
  ;; First, ask where to display the buffer
  (let* ((display-options '(("Current window" . current)
                            ("New tab" . tab)
                            ("Split window" . split)))
         (display-choice (completing-read "Where to open? "
                                          (mapcar #'car display-options)
                                          nil t nil nil "Current window"))
         (display-method (cdr (assoc display-choice display-options)))
         ;; Build models-alist similar to gptel--infix-provider
         (models-alist
          (cl-loop
           for (name . backend) in gptel--known-backends
           nconc (cl-loop for model in (gptel-backend-models backend)
                          collect (list (concat name ":" (gptel--model-name model))
                                        backend model))))
         (selected (completing-read "Select model: "
                                     (mapcar #'car models-alist)
                                     nil t))
         (choice (assoc selected models-alist))
         (backend (nth 1 choice))
         (model (nth 2 choice))
         (buffer-name (format "*gptel-%s*" (replace-regexp-in-string ":" "-" selected)))
         ;; Create buffer without displaying it (pass nil for interactivep)
         ;; Note: When interactivep is t, gptel calls (display-buffer) with
         ;; gptel-display-buffer-action, which would display the buffer before
         ;; we can control where it goes. By passing nil, we handle all display
         ;; logic ourselves based on the user's selection.
         (buffer (gptel buffer-name nil nil nil)))
    ;; Display buffer based on user's choice
    (pcase display-method
      ('split (pop-to-buffer buffer))
      ('tab (tab-bar-new-tab)
            (switch-to-buffer buffer))
      ('current (switch-to-buffer buffer)))
    ;; Set backend and model as buffer-local
    (with-current-buffer buffer
      (setq-local gptel-backend backend)
      (setq-local gptel-model model))))

;; Keybinding for gptel launcher
;; Direct access with <SPC> l
(with-eval-after-load 'gptel
  (evil-define-key 'normal 'global (kbd "<SPC> l") 'jf/gptel-launcher))

;; Configuration for gptel session auto-save
(defcustom jf/gptel-autosave-enabled t
  "Whether to automatically save gptel sessions after each response."
  :type 'boolean
  :group 'gptel)

(defcustom jf/gptel-sessions-directory "~/gptel-sessions/"
  "Directory for storing gptel sessions.
Will be created if it doesn't exist."
  :type 'directory
  :group 'gptel)

(defcustom jf/gptel-session-filename-format "%Y%m%d-%H%M%S"
  "Format string for timestamp portion of gptel session filenames.
Uses `format-time-string' syntax."
  :type 'string
  :group 'gptel)

(defun jf/gptel--create-metadata (session-dir session-id model backend)
  "Create initial metadata structure for a new session.
SESSION-DIR is the directory path, SESSION-ID is the session identifier,
MODEL is the model symbol, BACKEND is the backend name."
  (list :session_id session-id
        :model (symbol-name model)
        :backend backend
        :created (format-time-string "%Y-%m-%dT%H:%M:%SZ")
        :tree (list :id "root"
                    :children [])
        :current_path ["root"]
        :agent_traces []))

(defun jf/gptel--read-metadata (session-dir)
  "Read metadata.json from SESSION-DIR.
Returns metadata plist or nil if file doesn't exist."
  (let ((metadata-file (expand-file-name "metadata.json" session-dir)))
    (when (file-exists-p metadata-file)
      (condition-case nil
          (let ((json-object-type 'plist)
                (json-array-type 'vector)
                (json-key-type 'keyword))
            (json-read-file metadata-file))
        (error nil)))))

(defun jf/gptel--write-metadata (session-dir metadata)
  "Write METADATA plist to metadata.json in SESSION-DIR."
  (let ((metadata-file (expand-file-name "metadata.json" session-dir))
        (json-encoding-pretty-print t))
    (with-temp-file metadata-file
      (insert (json-encode metadata)))))

(defun jf/gptel--add-tree-node (tree parent-path node-id node-type filename preview)
  "Add a new node to TREE at PARENT-PATH.
NODE-ID is the node identifier, NODE-TYPE is 'message' or 'response',
FILENAME is the file containing this node's content,
PREVIEW is a short preview of the content.
Returns the updated tree."
  (let ((new-node (list :id node-id
                        :type (symbol-name node-type)
                        :file filename
                        :timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ")
                        :preview preview
                        :children [])))
    (if (equal parent-path ["root"])
        ;; Add to root's children
        (let ((children (plist-get tree :children)))
          (plist-put tree :children
                     (vconcat children (vector new-node)))
          tree)
      ;; Navigate to parent and add
      (jf/gptel--add-node-at-path tree (cl-coerce parent-path 'list) new-node 1)
      tree)))

(defun jf/gptel--add-node-at-path (node path new-child depth)
  "Navigate to parent node in TREE following PATH and add NEW-CHILD.
DEPTH tracks current position in path (skipping 'root').
Mutates tree in place."
  (if (>= depth (length path))
      ;; At parent, add child
      (let ((children (plist-get node :children)))
        (plist-put node :children
                   (vconcat children (vector new-child))))
    ;; Navigate deeper
    (let* ((target-id (nth depth path))
           (children (plist-get node :children))
           (found nil))
      (cl-dotimes (i (length children))
        (unless found
          (let ((child (aref children i)))
            (when (equal (plist-get child :id) target-id)
              (jf/gptel--add-node-at-path child path new-child (1+ depth))
              (setq found t))))))))

(defun jf/gptel--validate-metadata (metadata)
  "Validate metadata structure. Returns t if valid, nil otherwise."
  (and (plist-get metadata :session_id)
       (plist-get metadata :model)
       (plist-get metadata :backend)
       (plist-get metadata :tree)
       (vectorp (plist-get metadata :current_path))))

(defun jf/gptel--find-leaf-nodes (tree &optional path)
  "Find all leaf nodes in TREE.
Returns list of (path . node) pairs where path is vector of node IDs."
  (let* ((current-path (or path ["root"]))
         (children (plist-get tree :children)))
    (if (or (null children)
            (and (vectorp children) (zerop (length children))))
        ;; This is a leaf - only return if not the root with no children
        (if (equal current-path ["root"])
            nil  ; Empty tree, return empty list
          (list (cons current-path tree)))
      ;; Recurse into children
      (apply #'append
             (mapcar
              (lambda (child)
                (jf/gptel--find-leaf-nodes
                 child
                 (vconcat current-path (vector (plist-get child :id)))))
              (append children nil))))))

(defun jf/gptel--get-conversation-path (metadata)
  "Get the current conversation path from METADATA.
Returns vector of node IDs representing the current branch."
  (plist-get metadata :current_path))

(defun jf/gptel--sanitize-model-name (model)
  "Sanitize MODEL symbol for use in filename.
Converts to lowercase, replaces special chars with hyphens."
  (let ((name (symbol-name model)))
    (replace-regexp-in-string
     "-+" "-"  ; collapse multiple hyphens
     (replace-regexp-in-string
      "[^a-z0-9-]" "-"  ; replace special chars
      (downcase name)))))

(defun jf/gptel--generate-session-dirname ()
  "Generate directory name for current gptel session.
Format: TIMESTAMP-MODELNAME"
  (let* ((timestamp (format-time-string jf/gptel-session-filename-format))
         (model-name (jf/gptel--sanitize-model-name gptel-model)))
    (format "%s-%s" timestamp model-name)))

(defun jf/gptel--get-file-extension ()
  "Get file extension based on current major-mode."
  (cond
   ((derived-mode-p 'org-mode) "org")
   ((derived-mode-p 'markdown-mode) "md")
   (t "txt")))

(defun jf/gptel--get-next-message-id (metadata)
  "Get the next message ID from METADATA.
Returns format 'message-N' or 'message-N-altM' for forks."
  (let* ((current-path (plist-get metadata :current_path))
         (last-node-id (when (> (length current-path) 1)
                         (aref current-path (1- (length current-path)))))
         ;; Extract number from last node (e.g., "response-2" -> 2)
         (last-num (when last-node-id
                     (if (string-match "\\([0-9]+\\)" (format "%s" last-node-id))
                         (string-to-number (match-string 1 (format "%s" last-node-id)))
                       0)))
         (next-num (if last-num (1+ last-num) 1)))
    (format "message-%d" next-num)))

;; Global session registry for cross-buffer session access
(defvar jf/gptel--session-registry (make-hash-table :test 'equal)
  "Global registry mapping session-id to session data plist.
Each entry: session-id -> (:directory path
                           :metadata plist
                           :parent-buffer buffer
                           :trace-stack list
                           :trace-counter int
                           :created time)

This allows subagents running in separate buffers to access
session context via session-id lookup.")

(defun jf/gptel--generate-session-id ()
  "Generate unique session ID using timestamp and random component."
  (format "%s-%04x" (format-time-string "%Y%m%d%H%M%S") (random 65536)))

(defun jf/gptel--register-session (session-dir session-metadata parent-buffer)
  "Register new session in global registry. Returns session-id."
  (let ((session-id (jf/gptel--generate-session-id)))
    (puthash session-id
             (list :directory session-dir
                   :metadata session-metadata
                   :parent-buffer parent-buffer
                   :trace-stack nil
                   :trace-counter 0
                   :created (current-time))
             jf/gptel--session-registry)
    session-id))

(defun jf/gptel--unregister-session (session-id)
  "Remove session from registry (manual cleanup)."
  (remhash session-id jf/gptel--session-registry))

(defun jf/gptel--get-session-data (session-id)
  "Lookup session data from registry. Returns plist or nil."
  (gethash session-id jf/gptel--session-registry))

(defun jf/gptel--update-session-data (session-id key value)
  "Update specific key in session data."
  (when-let ((session-data (gethash session-id jf/gptel--session-registry)))
    (plist-put session-data key value)
    (puthash session-id session-data jf/gptel--session-registry)))

;; Buffer-local variables to track session state
;; NOTE: Only session-id is buffer-local now. All other state lives in global registry.
(defvar-local jf/gptel--session-id nil
  "Session ID for this buffer. Used to lookup data from global registry.
All other session state (directory, metadata, traces) stored in registry.")

(defvar-local jf/gptel--session-dir nil
  "Directory where current session is stored.
DEPRECATED: Kept for backwards compatibility. Use registry lookup instead.")

(defvar-local jf/gptel--session-metadata nil
  "Metadata plist for current session.")

(defvar-local jf/gptel--message-counter 0
  "Counter for message/response pairs in current session.")

(defvar-local jf/gptel--branching-next nil
  "Flag indicating next message should be a branch.")

(defvar-local jf/gptel--branch-id nil
  "Branch ID to use for next message when branching.")

(defvar-local jf/gptel--agent-name nil
  "Name of the agent currently active in this buffer.
Set when delegating to a subagent via gptel-agent.")

(defun jf/gptel--create-trace (session-id agent-type description prompt &optional parent-trace-id)
  "Create new agent trace entry in session.
Looks up session from registry, updates metadata, returns trace-id."
  (when-let* ((session-data (jf/gptel--get-session-data session-id))
              (metadata (plist-get session-data :metadata)))
    (let* ((trace-counter (plist-get session-data :trace-counter))
           (trace-id (format "trace-%d" (cl-incf trace-counter)))
           (trace-stack (plist-get session-data :trace-stack))
           (depth (length trace-stack))
           (trace (list :trace_id trace-id
                       :agent_type agent-type
                       :description description
                       :prompt_preview (substring prompt 0 (min 200 (length prompt)))
                       :timestamp_start (format-time-string "%Y-%m-%dT%H:%M:%SZ")
                       :timestamp_end nil
                       :status "running"
                       :parent_trace_id parent-trace-id
                       :depth depth
                       :tool_calls []
                       :associated_nodes nil)))
      ;; Update registry: increment counter, push to stack
      (jf/gptel--update-session-data session-id :trace-counter trace-counter)
      (jf/gptel--update-session-data session-id :trace-stack (cons trace-id trace-stack))

      ;; Add to metadata's agent_traces array
      (let ((traces (plist-get metadata :agent_traces)))
        (plist-put metadata :agent_traces
                   (vconcat traces (vector trace))))

      ;; Write metadata to disk (incremental)
      (let ((session-dir (plist-get session-data :directory)))
        (jf/gptel--write-metadata session-dir metadata))

      trace-id)))

(defun jf/gptel--record-tool-call (session-id trace-id tool-name args result)
  "Record tool call incrementally to metadata.json."
  (when-let* ((session-data (jf/gptel--get-session-data session-id))
              (metadata (plist-get session-data :metadata))
              (traces (plist-get metadata :agent_traces)))
    ;; Find the trace
    (let ((trace (cl-find-if (lambda (tr)
                              (equal (plist-get tr :trace_id) trace-id))
                            (append traces nil))))
      (when trace
        ;; Create tool call entry
        (let ((tool-call (list :tool_name tool-name
                              :args args
                              :timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ")
                              :result_preview (jf/gptel--preview-string result 200))))
          ;; Append to trace's tool_calls
          (let ((calls (plist-get trace :tool_calls)))
            (plist-put trace :tool_calls (vconcat calls (vector tool-call)))))

        ;; Write metadata immediately (incremental recording)
        (let ((session-dir (plist-get session-data :directory)))
          (jf/gptel--write-metadata session-dir metadata))))))

(defun jf/gptel--complete-trace (session-id trace-id status)
  "Mark trace as completed, pop from stack."
  (when-let* ((session-data (jf/gptel--get-session-data session-id))
              (metadata (plist-get session-data :metadata))
              (traces (plist-get metadata :agent_traces)))
    ;; Find and update the trace
    (let ((trace (cl-find-if (lambda (tr)
                              (equal (plist-get tr :trace_id) trace-id))
                            (append traces nil))))
      (when trace
        (plist-put trace :timestamp_end (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
        (plist-put trace :status status)

        ;; Pop from trace stack in registry
        (let ((trace-stack (plist-get session-data :trace-stack)))
          (jf/gptel--update-session-data session-id :trace-stack
                                        (remove trace-id trace-stack)))

        ;; Write metadata to disk
        (let ((session-dir (plist-get session-data :directory)))
          (jf/gptel--write-metadata session-dir metadata))))))

(defun jf/gptel--current-trace-id (session-id)
  "Get current trace ID (top of stack) from session."
  (when-let ((session-data (jf/gptel--get-session-data session-id)))
    (car (plist-get session-data :trace-stack))))

(defun jf/gptel--preview-string (obj &optional max-len)
  "Create preview string from OBJ (string, list, etc)."
  (let* ((max-len (or max-len 200))
         (str (cond
               ((stringp obj) obj)
               ((listp obj) (format "%S" obj))
               (t (format "%s" obj)))))
    (if (> (length str) max-len)
        (concat (substring str 0 max-len) "...")
      str)))

(defun jf/gptel--insert-context (context &optional include-final-prompt)
  "Insert CONTEXT (list of message/response plists) with proper formatting.
Adds text properties, prefixes, and separators.
If INCLUDE-FINAL-PROMPT is non-nil, adds a final prompt prefix for user input."
  (let ((prompt-prefix (gptel-prompt-prefix-string))
        (response-prefix (gptel-response-prefix-string))
        (separator gptel-response-separator))
    (dolist (item context)
      (let* ((type (plist-get item :type))
             (content (plist-get item :content))
             (is-response (eq type 'response))
             (prefix (if is-response response-prefix prompt-prefix))
             (start-pos (point)))

        ;; Insert separator before responses
        (when is-response
          (insert separator))

        ;; Insert prefix and content
        (insert prefix content "\n")

        ;; Add text properties for responses
        (when is-response
          (add-text-properties start-pos (point)
                              '(gptel response rear-nonsticky t)))))

    ;; Add final prompt prefix if requested
    (when include-final-prompt
      (insert "\n\n" prompt-prefix))))

(defun jf/gptel--restore-backend-model (metadata)
  "Restore backend and model from METADATA."
  (let ((backend-name (plist-get metadata :backend))
        (model-name (plist-get metadata :model)))

    ;; Restore backend
    (when backend-name
      (let ((backend (alist-get backend-name gptel--known-backends
                               nil nil #'equal)))
        (when backend
          (setq-local gptel-backend backend))))

    ;; Restore model (convert string to symbol)
    (when model-name
      (setq-local gptel-model (intern model-name)))))

(defun jf/gptel--get-session-context ()
  "Extract gptel session context from current buffer.
Returns plist with :session-id, :model, :backend, :agent-name, :timestamp.
Returns nil if not in a gptel session."
  (when (and gptel-mode jf/gptel--session-dir)
    (list :session-id (when jf/gptel--session-metadata
                        (plist-get jf/gptel--session-metadata :session_id))
          :model (when gptel-model (symbol-name gptel-model))
          :backend (when gptel-backend (gptel-backend-name gptel-backend))
          :agent-name jf/gptel--agent-name
          :timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))))

(defun jf/gptel--autosave-session (response-start response-end)
  "Automatically save gptel session after LLM response.
RESPONSE-START and RESPONSE-END mark the response boundaries.
This function is added to `gptel-post-response-functions'."
  (when (and jf/gptel-autosave-enabled
             gptel-mode)
    (condition-case err
        (progn
          ;; Initialize session if needed
          (unless jf/gptel--session-dir
            (jf/gptel--initialize-session))

          ;; Determine message ID (branch or sequential)
          (let* ((ext (jf/gptel--get-file-extension))
                 (msg-id (if (bound-and-true-p jf/gptel--branching-next)
                             ;; Use predetermined branch ID
                             (prog1 jf/gptel--branch-id
                               (setq jf/gptel--branching-next nil)
                               (setq jf/gptel--branch-id nil))
                           ;; Normal sequential ID
                           (progn
                             (setq jf/gptel--message-counter (1+ jf/gptel--message-counter))
                             (format "message-%d" jf/gptel--message-counter))))
                 ;; Response ID matches message number
                 (resp-id (replace-regexp-in-string "message-" "response-" msg-id))
                 (msg-file (format "%s.%s" msg-id ext))
                 (resp-file (format "%s.%s" resp-id ext))
                 ;; For now, extract the last message and response
                 ;; TODO: Improve to extract exact content
                 (message-content (jf/gptel--extract-last-message response-start))
                 (response-content (buffer-substring response-start response-end))
                 (msg-preview (substring message-content 0 (min 80 (length message-content))))
                 (resp-preview (substring response-content 0 (min 80 (length response-content)))))

            ;; Save message file
            (with-temp-file (expand-file-name msg-file jf/gptel--session-dir)
              (insert message-content))

            ;; Save response file
            (with-temp-file (expand-file-name resp-file jf/gptel--session-dir)
              (insert response-content))

            ;; Update metadata tree
            (jf/gptel--update-metadata-tree msg-id msg-file msg-preview
                                            resp-id resp-file resp-preview)

            ;; Link any completed traces to this message/response
            (when jf/gptel--session-id
              (when-let* ((session-data (jf/gptel--get-session-data jf/gptel--session-id))
                          (metadata (plist-get session-data :metadata))
                          (traces (plist-get metadata :agent_traces)))
                (dolist (trace (append traces nil))  ; Convert vector to list
                  (when (and (equal (plist-get trace :status) "completed")
                             (null (plist-get trace :associated_nodes)))
                    ;; This trace just completed, associate it
                    (plist-put trace :associated_nodes (vector msg-id resp-id))))
                ;; Write updated metadata
                (let ((session-dir (plist-get session-data :directory)))
                  (jf/gptel--write-metadata session-dir metadata))))

            (message "Session saved: %s/%s"
                     (file-name-nondirectory jf/gptel--session-dir)
                     resp-file)))
      (file-error
       (message "gptel autosave failed: %s" (error-message-string err)))
      (error
       (message "gptel autosave error: %s" (error-message-string err))))))

(defun jf/gptel--initialize-session ()
  "Initialize a new session directory, metadata, and register in global registry."
  (let* ((dirname (jf/gptel--generate-session-dirname))
         (sessions-base (expand-file-name jf/gptel-sessions-directory))
         (session-dir (expand-file-name dirname sessions-base))
         (backend-name (gptel-backend-name gptel-backend)))
    ;; Create session directory
    (make-directory session-dir t)

    ;; Create initial metadata (now includes :agent_traces [])
    (setq jf/gptel--session-metadata
          (jf/gptel--create-metadata session-dir dirname gptel-model backend-name))

    ;; Write metadata
    (jf/gptel--write-metadata session-dir jf/gptel--session-metadata)

    ;; Register in global registry
    (let ((session-id (jf/gptel--register-session session-dir jf/gptel--session-metadata
                                                  (current-buffer))))
      ;; Store session-id as buffer-local variable
      (setq jf/gptel--session-id session-id)
      ;; Keep old vars for backwards compatibility
      (setq jf/gptel--session-dir session-dir)

      (message "Created session: %s (ID: %s)" dirname session-id))))

(defun jf/gptel--extract-last-message (response-start)
  "Extract user's last message before RESPONSE-START using text properties."
  (save-excursion
    (goto-char response-start)
    ;; Find start of last user message (previous property change from response)
    (let ((msg-end response-start)
          (msg-start (previous-single-property-change response-start 'gptel nil (point-min))))
      ;; If we're at a response, go back one more to get the user message
      (when (eq (get-char-property msg-start 'gptel) 'response)
        (setq msg-end msg-start)
        (setq msg-start (previous-single-property-change msg-start 'gptel nil (point-min))))
      ;; Extract and trim
      (let ((content (buffer-substring-no-properties msg-start msg-end)))
        (gptel--trim-prefixes content)))))

(defun jf/gptel--get-all-messages ()
  "Extract all message/response pairs from current buffer.
Returns list of plists: (:type 'message|'response :content string)."
  (let (messages (prev-pt (point-max)))
    (save-excursion
      (goto-char (point-max))
      (while (> prev-pt (point-min))
        (let ((prop-change (previous-single-property-change prev-pt 'gptel nil (point-min))))
          (when prop-change
            (let* ((prop (get-char-property prop-change 'gptel))
                   (content (gptel--trim-prefixes
                            (buffer-substring-no-properties prop-change prev-pt)))
                   (type (if (eq prop 'response) 'response 'message)))
              (when (and content (not (string-empty-p content)))
                (push (list :type type :content content) messages)))
            (setq prev-pt prop-change)))))
    messages))

(defun jf/gptel--get-context-before-point (pt)
  "Get all message/response pairs before point PT.
Returns list of plists in chronological order."
  (let ((context nil)
        (scan-pt (point-min)))
    (save-excursion
      (while (< scan-pt pt)
        (goto-char scan-pt)
        (let ((next-change (next-single-property-change scan-pt 'gptel nil pt)))
          (when next-change
            (let* ((prop (get-char-property scan-pt 'gptel))
                   (content (gptel--trim-prefixes
                            (buffer-substring-no-properties scan-pt next-change)))
                   (type (if (eq prop 'response) 'response 'message)))
              (when (and content (not (string-empty-p content)))
                (push (list :type type :content content) context)))
            (setq scan-pt next-change))
          (unless next-change
            (setq scan-pt pt)))))
    (nreverse context)))

(defun jf/gptel--update-metadata-tree (msg-id msg-file msg-preview resp-id resp-file resp-preview)
  "Update metadata tree with new message and response nodes."
  (let* ((tree (plist-get jf/gptel--session-metadata :tree))
         (current-path (plist-get jf/gptel--session-metadata :current_path)))

    (message "DEBUG update-tree: msg-id=%s, current-path=%S" msg-id current-path)

    ;; Add message node
    (jf/gptel--add-tree-node tree current-path msg-id 'message msg-file msg-preview)
    ;; Add response node (child of message)
    (let* ((new-msg-path (vconcat current-path (vector msg-id))))
      (jf/gptel--add-tree-node tree new-msg-path resp-id 'response resp-file resp-preview)
      ;; Update current path
      (let ((new-path (vconcat new-msg-path (vector resp-id))))
        (plist-put jf/gptel--session-metadata :current_path new-path)
        (message "DEBUG update-tree: new current-path=%S" new-path)))

    ;; Write to disk
    (jf/gptel--write-metadata jf/gptel--session-dir jf/gptel--session-metadata)))

(defun jf/gptel-branch-session ()
  "Create new branch by selecting which messages to include.
Presents a list of message/response pairs and lets user choose
where to branch from."
  (interactive)
  (unless (and jf/gptel--session-dir jf/gptel--session-metadata)
    (user-error "No active session to branch"))

  ;; Get all messages and build candidates
  (let* ((all-messages (jf/gptel--get-all-messages))
         (candidates (jf/gptel--build-branch-candidates all-messages))
         (choice (completing-read "Branch before: " candidates nil t))
         (choice-index (cdr (assoc choice candidates)))
         ;; Take first N messages (up to but NOT including the chosen point)
         (branch-context (seq-take all-messages choice-index))
         ;; Get the next message text (the one we're branching before)
         (next-message-text (when (< choice-index (length all-messages))
                             (let ((next-msg (nth choice-index all-messages)))
                               (when (eq (plist-get next-msg :type) 'message)
                                 (plist-get next-msg :content)))))
         (context-length (length branch-context))
         ;; Get the parent path
         (current-path (plist-get jf/gptel--session-metadata :current_path))
         (desired-path-length (1+ context-length))
         (actual-path-length (length current-path))
         (parent-path (if (and (> context-length 0)
                              (>= actual-path-length desired-path-length))
                         (seq-subseq current-path 0 desired-path-length)
                       ["root"]))
         (next-msg-num (1+ (/ context-length 2)))
         (branch-id (jf/gptel--get-next-branch-id next-msg-num)))

    (message "DEBUG branch: choice=%s, context-length=%d, parent-path=%S"
             choice context-length parent-path)

    ;; Create new buffer with branch context
    (jf/gptel--create-branch-buffer branch-context branch-id parent-path next-message-text)))

(defun jf/gptel--build-branch-candidates (messages)
  "Build completing-read candidates from MESSAGES list.
Returns alist of (display-string . message-count).
Each option represents branching BEFORE that message."
  (let ((candidates '())
        (count 0))
    ;; Add option to branch before first message (fresh start)
    (push (cons "[Beginning] - Start fresh conversation" 0) candidates)

    ;; Add option for each message (branch before this message)
    (while (< count (length messages))
      (let* ((msg (nth count messages))
             (msg-content (plist-get msg :content))
             (msg-preview (substring msg-content 0
                                    (min 50 (length msg-content))))
             (msg-num (/ (+ count 2) 2)))
        ;; Only add option if this is a message (not response)
        (when (eq (plist-get msg :type) 'message)
          (push (cons (format "Before message %d: \"%s...\""
                             msg-num msg-preview)
                      count)
                candidates))
        (setq count (1+ count))))

    (nreverse candidates)))

(defun jf/gptel--get-next-branch-id (message-num)
  "Get next branch ID for MESSAGE-NUM by scanning directory.
Returns 'message-N' if no branches exist, or 'message-N-altM' for next alt."
  (let* ((base-pattern (format "message-%d" message-num))
         (alt-pattern (format "%s-alt\\([0-9]+\\)" base-pattern))
         (files (directory-files jf/gptel--session-dir nil base-pattern))
         (max-alt 0))
    ;; Scan for existing alts
    (dolist (file files)
      (when (string-match alt-pattern file)
        (let ((alt-num (string-to-number (match-string 1 file))))
          (setq max-alt (max max-alt alt-num)))))
    (if (> max-alt 0)
        (format "message-%d-alt%d" message-num (1+ max-alt))
      ;; Check if base message exists
      (if (seq-find (lambda (f) (string-match (format "^%s\\." base-pattern) f)) files)
          (format "message-%d-alt1" message-num)
        (format "message-%d" message-num)))))

(defun jf/gptel--create-branch-buffer (context branch-id parent-path &optional next-message-text)
  "Create new gptel buffer with CONTEXT loaded, ready for branching at BRANCH-ID.
If NEXT-MESSAGE-TEXT is provided, insert it as the starting prompt for editing."
  (let* ((metadata jf/gptel--session-metadata)
         (session-id (plist-get metadata :session_id))
         (buffer-name (format "*gptel-%s-branch-%s*" session-id branch-id))
         (buf (get-buffer-create buffer-name))
         (parent-session-dir jf/gptel--session-dir))

    (with-current-buffer buf
      ;; Set up mode
      (markdown-mode)
      (gptel-mode 1)

      ;; Reconstruct conversation with proper formatting
      (erase-buffer)
      (jf/gptel--insert-context context t) ; t = include final prompt prefix

      ;; If we have next message text, insert it as editable content
      (when next-message-text
        (insert next-message-text))

      ;; Set up session state (shared with parent)
      (setq jf/gptel--session-dir parent-session-dir)
      (setq jf/gptel--session-metadata (copy-sequence metadata))

      ;; Update current path to branch parent
      (plist-put jf/gptel--session-metadata :current_path parent-path)
      (message "DEBUG branch-buffer: set current-path=%S" parent-path)

      ;; Set message counter for next message
      (setq jf/gptel--message-counter (/ (length context) 2))

      ;; Mark that next message is a branch
      (setq-local jf/gptel--branching-next t)
      (setq-local jf/gptel--branch-id branch-id)

      ;; Restore backend/model
      (jf/gptel--restore-backend-model metadata)

      (goto-char (point-max))
      (message "Created branch at %s. Edit and send message to continue." branch-id))

    ;; Display buffer
    (switch-to-buffer buf)))

(defun jf/gptel--find-node-by-path (tree path)
  "Find node in TREE by following PATH (vector of node IDs)."
  (if (or (null path) (equal path ["root"]))
      tree
    (jf/gptel--find-node-recursive tree (cl-coerce path 'list) 1)))

(defun jf/gptel--find-node-recursive (tree path depth)
  "Recursively find node in TREE following PATH at DEPTH."
  (if (>= depth (length path))
      tree
    (let* ((target-id (nth depth path))
           (children (plist-get tree :children))
           (found nil))
      (seq-doseq (child children)
        (when (equal (plist-get child :id) (format "%s" target-id))
          (setq found (jf/gptel--find-node-recursive child path (1+ depth)))))
      found)))

(defun jf/gptel-browse-sessions ()
  "Browse and open saved gptel sessions from directory.
Shows leaf nodes (endpoints) of each branch."
  (interactive)
  (let* ((sessions-dir (expand-file-name jf/gptel-sessions-directory))
         (session-dirs (when (file-directory-p sessions-dir)
                         (seq-filter #'file-directory-p
                                    (directory-files sessions-dir t "^[^.]"))))
         (candidates (jf/gptel--build-session-candidates session-dirs)))

    (message "DEBUG browse: candidates=%S" candidates)
    (message "DEBUG browse: candidates type=%s" (type-of candidates))

    (if (null candidates)
        (message "No gptel sessions found in %s" sessions-dir)
      (let* ((choice-list (mapcar #'car candidates)))
        (message "DEBUG browse: choice-list=%S" choice-list)
        (let* ((choice (completing-read "Open session branch: " choice-list nil t))
               (session-info (cdr (assoc choice candidates))))
          (message "DEBUG browse: selected=%s, info=%S" choice session-info)
          (jf/gptel--open-session-branch session-info))))))

(defun jf/gptel--build-session-candidates (session-dirs)
  "Build list of session branch candidates from SESSION-DIRS.
Returns alist of (display-string . (session-dir path leaf-node))."
  (let (candidates)
    (dolist (dir session-dirs)
      (let ((metadata (jf/gptel--read-metadata dir)))
        (when metadata
          (let* ((tree (plist-get metadata :tree))
                 (leaves (jf/gptel--find-leaf-nodes tree))
                 (session-id (plist-get metadata :session_id)))
            ;; Add candidate for each leaf
            (dolist (leaf leaves)
              (let* ((path (car leaf))
                     (node (cdr leaf))
                     (depth (1- (length path))) ; subtract root
                     (last-id (aref path (1- (length path))))
                     (is-alt (string-match "alt\\([0-9]+\\)" (format "%s" last-id)))
                     (branch-label (if is-alt
                                      (format " [alt%s, depth:%d]"
                                             (match-string 1 (format "%s" last-id))
                                             depth)
                                    (format " [main, depth:%d]" depth)))
                     (preview (plist-get node :preview))
                     (display (format "%-35s %-20s | %s"
                                     session-id
                                     branch-label
                                     (if preview
                                         (substring preview 0 (min 60 (length preview)))
                                       "..."))))
                (push (cons display (list :dir dir :path path :node node))
                      candidates)))))))
    (nreverse candidates)))

(defun jf/gptel--open-session-branch (session-info)
  "Open a session branch and reconstruct conversation.
SESSION-INFO is a plist with :dir, :path, and :node."
  (let* ((dir (plist-get session-info :dir))
         (path (plist-get session-info :path))
         (metadata (jf/gptel--read-metadata dir))
         (tree (plist-get metadata :tree)))
    ;; Reconstruct conversation from root to this leaf
    (jf/gptel--reconstruct-conversation dir tree path)))

(defun jf/gptel--reconstruct-conversation (session-dir tree path)
  "Reconstruct conversation from root to PATH endpoint in TREE."
  (let* ((metadata (jf/gptel--read-metadata session-dir))
         (session-id (plist-get metadata :session_id))
         (buffer-name (format "*gptel-%s*" session-id))
         (context (jf/gptel--load-context-from-path session-dir tree path)))

    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        (markdown-mode)
        (gptel-mode 1)
        (erase-buffer)

        ;; Insert conversation with final prompt
        (jf/gptel--insert-context context t)

        ;; Set up session state
        (setq jf/gptel--session-dir session-dir)
        (setq jf/gptel--session-metadata metadata)
        (plist-put jf/gptel--session-metadata :current_path path)
        (jf/gptel--write-metadata session-dir jf/gptel--session-metadata)

        ;; Set counter from path depth
        (setq jf/gptel--message-counter (/ (length context) 2))

        ;; Restore backend/model
        (jf/gptel--restore-backend-model metadata)

        (goto-char (point-max)))

      (switch-to-buffer buf)
      (message "Loaded session: %s" session-id))))

(defun jf/gptel--load-context-from-path (session-dir tree path)
  "Load message/response pairs from SESSION-DIR following PATH in TREE.
Returns list of plists suitable for jf/gptel--insert-context."
  (let ((context '())
        (node tree))
    ;; Walk path (skip root at index 0)
    (cl-loop for i from 1 below (length path)
             for target-id = (aref path i)
             do
             (let ((children (plist-get node :children)))
               ;; Find matching child
               (setq node
                     (seq-find (lambda (child)
                                 (equal (plist-get child :id) target-id))
                               children))
               (when node
                 (let* ((file (plist-get node :file))
                        (type (intern (plist-get node :type)))
                        (full-path (expand-file-name file session-dir))
                        (content (when (file-exists-p full-path)
                                  (with-temp-buffer
                                    (insert-file-contents full-path)
                                    (buffer-string)))))
                   (when content
                     (push (list :type type :content content) context))))))
    (nreverse context)))

;; Install auto-save hook
(with-eval-after-load 'gptel
  (add-hook 'gptel-post-response-functions #'jf/gptel--autosave-session))

;; TODO: Add keybinding for jf/gptel-browse-sessions
;; For now, access via M-x jf/gptel-browse-sessions
