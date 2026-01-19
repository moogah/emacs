;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)

(defun jf/gptel--create-metadata (session-dir session-id model backend)
  "Create initial metadata structure for a new session.
SESSION-DIR is the directory path, SESSION-ID is the session identifier,
MODEL is the model symbol, BACKEND is the backend name.
Returns simplified metadata plist for filesystem-based persistence.
Tree structure is now represented by filesystem directories, not metadata."
  (list :session_id session-id
        :model (symbol-name model)
        :backend backend
        :created (format-time-string "%Y-%m-%dT%H:%M:%SZ")))

(defun jf/gptel--create-metadata-legacy (session-dir session-id model backend)
  "DEPRECATED: Create metadata with tree structure (old format).
Use jf/gptel--create-metadata instead, which creates simplified metadata
for filesystem-based persistence."
  (list :session_id session-id
        :model (symbol-name model)
        :backend backend
        :created (format-time-string "%Y-%m-%dT%H:%M:%SZ")
        :tree (list :id jf/gptel-session-tree-root-id
                    :children [])
        :current_path (vector jf/gptel-session-tree-root-id)
        :agent_traces []))

(defun jf/gptel--read-metadata (session-dir)
  "Read metadata.json from SESSION-DIR.
Returns metadata plist or nil if file doesn't exist."
  (let ((metadata-file (expand-file-name jf/gptel-session-metadata-filename session-dir)))
    (when (file-exists-p metadata-file)
      (condition-case nil
          (let ((json-object-type 'plist)
                (json-array-type 'vector)
                (json-key-type 'keyword))
            (json-read-file metadata-file))
        (error nil)))))

(defun jf/gptel--write-metadata (session-dir metadata)
  "Write METADATA plist to metadata.json in SESSION-DIR."
  (let ((metadata-file (expand-file-name jf/gptel-session-metadata-filename session-dir))
        (json-encoding-pretty-print t))
    (with-temp-file metadata-file
      (insert (json-encode metadata)))))

(defun jf/gptel--validate-metadata (metadata)
  "Validate metadata structure. Returns t if valid, nil otherwise.
Works with both simplified (filesystem-based) and legacy (tree-based) formats."
  (and (plist-get metadata :session_id)
       (plist-get metadata :model)
       (plist-get metadata :backend)
       (plist-get metadata :created)))

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
    (if (equal parent-path (vector jf/gptel-session-tree-root-id))
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

(defun jf/gptel--find-leaf-nodes (tree &optional path)
  "Find all leaf nodes in TREE.
Returns list of (path . node) pairs where path is vector of node IDs."
  (let* ((current-path (or path (vector jf/gptel-session-tree-root-id)))
         (children (plist-get tree :children)))
    (if (or (null children)
            (and (vectorp children) (zerop (length children))))
        ;; This is a leaf - only return if not the root with no children
        (if (equal current-path (vector jf/gptel-session-tree-root-id))
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

(defun jf/gptel--find-node-by-path (tree path)
  "Find node in TREE by following PATH (vector of node IDs)."
  (if (or (null path) (equal path (vector jf/gptel-session-tree-root-id)))
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

(defun jf/gptel--update-metadata-tree (msg-id msg-file msg-preview resp-id resp-file resp-preview)
  "Update metadata tree with new message and response nodes."
  (let* ((tree (plist-get jf/gptel--session-metadata :tree))
         (current-path (plist-get jf/gptel--session-metadata :current_path)))

    (jf/gptel--log 'debug "update-tree: msg-id=%s, current-path=%S" msg-id current-path)

    ;; Add message node
    (jf/gptel--add-tree-node tree current-path msg-id 'message msg-file msg-preview)
    ;; Add response node (child of message)
    (let* ((new-msg-path (vconcat current-path (vector msg-id))))
      (jf/gptel--add-tree-node tree new-msg-path resp-id 'response resp-file resp-preview)
      ;; Update current path
      (let ((new-path (vconcat new-msg-path (vector resp-id))))
        (plist-put jf/gptel--session-metadata :current_path new-path)
        (jf/gptel--log 'debug "update-tree: new current-path=%S" new-path)))

    ;; Write to disk
    (jf/gptel--write-metadata jf/gptel--session-dir jf/gptel--session-metadata)))
