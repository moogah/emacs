(require 'cl-lib)

(defconst jf/gptel-session-tree-root-id "root"
  "The ID of the root node in the session conversation tree.
All conversation paths start from this node.")

(defconst jf/gptel-session-node-prefix-message "message-"
  "Prefix for user message node IDs.
Format: message-N or message-N-altM for branches.")

(defconst jf/gptel-session-node-prefix-response "response-"
  "Prefix for assistant response node IDs.
Format: response-N or response-N-altM for branches.")

(defconst jf/gptel-session-node-prefix-trace "trace-"
  "Prefix for agent execution trace node IDs.
Format: trace-N")

(defconst jf/gptel-session-metadata-filename "metadata.json"
  "Name of the session metadata file stored in each session directory.")

(defconst jf/gptel-session-traces-dirname "traces"
  "Name of the subdirectory containing agent execution traces.")

(defconst jf/gptel-session-trace-metadata-filename "metadata.json"
  "Name of the trace metadata file within each trace directory.")

(defconst jf/gptel-session-trace-tool-results-dirname "tool-results"
  "Name of the subdirectory containing large tool result outputs.")

(defconst jf/gptel-session-node-patterns
  '((:message . "^message-\\([0-9]+\\)\\(?:-alt\\([0-9]+\\)\\)?$")
    (:response . "^response-\\([0-9]+\\)\\(?:-alt\\([0-9]+\\)\\)?$")
    (:trace . "^trace-\\([0-9]+\\)$"))
  "Regular expression patterns for parsing node IDs.
Each pattern captures the sequence number and optional alternate number.")

(defun jf/gptel--node-id-parse (node-id)
  "Parse NODE-ID and return its components.
Returns a plist (:type TYPE :number N :alt M) or nil if invalid.
TYPE is one of :message, :response, or :trace.
N is the sequence number (integer).
M is the alternate/branch number (integer or nil for main path)."
  (let ((result nil))
    (cl-loop for (type . pattern) in jf/gptel-session-node-patterns
             when (string-match pattern node-id)
             do (setq result
                      (list :type type
                            :number (string-to-number (match-string 1 node-id))
                            :alt (when (match-string 2 node-id)
                                   (string-to-number (match-string 2 node-id)))))
             and return t)
    result))

(defconst jf/gptel-session-overlay-properties
  '((jf/session-id . "Session ID for cross-buffer context tracking"))
  "Documented overlay properties used by the session system.
This defines the contract for overlay properties that components
may inspect or modify.")

(provide 'jf/gptel-session-constants)
