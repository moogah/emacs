;; -*- lexical-binding: t; -*-
(require 'org-roam)

(defvar gptel-org-roam-result-limit 40000
  "Maximum character count for tool results to prevent context overflow.")

(defun gptel-org-roam--result-limit (result)
  "Limit RESULT to gptel-org-roam-result-limit characters.
If exceeded, return a warning message instead."
  (if (>= (length (format "%s" result)) gptel-org-roam-result-limit)
      (format "Results over %s characters. Stop. Analyze. Find a different solution, or use read_roam_node with 'heading' or 'body' granularity."
              gptel-org-roam-result-limit)
    result))

(defun gptel-org-roam--validate-node (node-id)
  "Validate NODE-ID exists in org-roam database.
Returns node object if valid, nil otherwise."
  (org-roam-node-from-id node-id))

(defun gptel-org-roam--extract-heading ()
  "Extract only the heading line at point."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun gptel-org-roam--extract-body ()
  "Extract heading and body text without subheadings."
  (save-excursion
    (let ((start (line-beginning-position)))
      (outline-next-heading)
      (buffer-substring-no-properties start (line-beginning-position)))))

(defun gptel-org-roam--extract-subtree ()
  "Extract heading with all subheadings and body text."
  (save-excursion
    (buffer-substring-no-properties
     (line-beginning-position)
     (org-end-of-subtree t))))

(defun gptel-org-roam--extract-content (node granularity)
  "Extract content from NODE at specified GRANULARITY level.
GRANULARITY can be 'heading', 'body', 'subtree', or 'full'.
NODE must be an org-roam-node object."
  (let ((file (org-roam-node-file node))
        (point (org-roam-node-point node))
        (level (org-roam-node-level node)))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (cond
       ;; For full file content
       ((equal granularity "full")
        (buffer-string))
       ;; For file-level nodes (level 0)
       ((= level 0)
        (goto-char (point-min))
        (if (equal granularity "heading")
            ;; Just the file title
            (or (org-roam-node-title node) "")
          ;; Full file content for body/subtree
          (buffer-string)))
       ;; For heading-level nodes
       (t
        (goto-char point)
        (cond
         ((equal granularity "heading")
          (gptel-org-roam--extract-heading))
         ((equal granularity "body")
          (gptel-org-roam--extract-body))
         ((equal granularity "subtree")
          (gptel-org-roam--extract-subtree))
         (t
          (gptel-org-roam--extract-body))))))))

(gptel-make-tool
 :name "search_roam_nodes"
 :function (lambda (query &optional tags match-type limit)
             (let* ((search-limit (or limit 20))
                    (match-type (or match-type "title"))
                    (results '())
                    (count 0))
               ;; Search based on match type
               (cond
                ;; Title search with optional tag filter
                ((or (equal match-type "title") (equal match-type "both"))
                 (let ((nodes (org-roam-node-list)))
                   (dolist (node nodes)
                     (when (and (< count search-limit)
                                (string-match-p (regexp-quote query)
                                               (org-roam-node-title node))
                                (or (not tags)
                                    (let ((node-tags (org-roam-node-tags node)))
                                      (cl-every (lambda (tag)
                                                 (member tag node-tags))
                                               tags))))
                       (let* ((id (org-roam-node-id node))
                              (title (org-roam-node-title node))
                              (node-tags (org-roam-node-tags node))
                              (file (org-roam-node-file node))
                              (preview (substring (or (gptel-org-roam--extract-content node "body") "")
                                                0 (min 100 (length (or (gptel-org-roam--extract-content node "body") ""))))))
                         (push (format "%d. ID: %s\n   Title: %s\n   Tags: %s\n   File: %s\n   Preview: %s...\n"
                                      (1+ count) id title
                                      (if node-tags (mapconcat 'identity node-tags " ") "none")
                                      file preview)
                               results)
                         (setq count (1+ count)))))))
                ;; Content search
                ((equal match-type "content")
                 (let ((nodes (org-roam-node-list)))
                   (dolist (node nodes)
                     (when (< count search-limit)
                       (let ((content (gptel-org-roam--extract-content node "full")))
                         (when (and content
                                   (string-match-p (regexp-quote query) content)
                                   (or (not tags)
                                       (let ((node-tags (org-roam-node-tags node)))
                                         (cl-every (lambda (tag)
                                                    (member tag node-tags))
                                                  tags))))
                           (let* ((id (org-roam-node-id node))
                                  (title (org-roam-node-title node))
                                  (node-tags (org-roam-node-tags node))
                                  (file (org-roam-node-file node))
                                  ;; Find context around the match
                                  (match-pos (string-match (regexp-quote query) content))
                                  (start (max 0 (- match-pos 50)))
                                  (end (min (length content) (+ match-pos (length query) 50)))
                                  (preview (substring content start end)))
                             (push (format "%d. ID: %s\n   Title: %s\n   Tags: %s\n   File: %s\n   Match: ...%s...\n"
                                          (1+ count) id title
                                          (if node-tags (mapconcat 'identity node-tags " ") "none")
                                          file preview)
                                   results)
                             (setq count (1+ count))))))))))
               ;; Format and return results
               (if (> count 0)
                   (gptel-org-roam--result-limit
                    (concat (format "Found %d node%s matching '%s':\n\n"
                                   count (if (> count 1) "s" "") query)
                           (mapconcat 'identity (nreverse results) "\n")
                           "\n[Use read_roam_node to get full content of specific nodes]"))
                 (format "No nodes found matching '%s'%s"
                        query
                        (if tags
                            (format " with tags: %s" (mapconcat 'identity tags ", "))
                          "")))))
 :description "Search org-roam knowledge base by title, tags, or content. Returns matching nodes with previews.

Use this when you need to find nodes by partial title match, locate nodes with specific tags, or search node content for keywords.

Returns up to 'limit' results (default 20, max 50). Results may be truncated for large knowledge bases.

After using this tool, STOP. Evaluate which results are most relevant. Then use read_roam_node to get full content of specific nodes, or refine your search with more specific query or tags."
 :args (list '(:name "query"
               :type string
               :description "Search query string to match against node titles or content")
             '(:name "tags"
               :type array
               :items (:type string)
               :optional t
               :description "Array of tags to filter by (nodes must have ALL specified tags)")
             '(:name "match_type"
               :type string
               :enum ["title" "content" "both"]
               :optional t
               :description "Where to search: 'title' (default), 'content', or 'both'")
             '(:name "limit"
               :type integer
               :optional t
               :description "Maximum number of results to return (default: 20, max: 50)"))
 :category "org-roam")

(gptel-make-tool
 :name "list_roam_nodes"
 :function (lambda (&optional tags sort-by limit)
             (let* ((nodes (org-roam-node-list))
                    (list-limit (or limit 50))
                    (sort-by (or sort-by "title"))
                    (filtered-nodes nodes)
                    (count 0))
               ;; Filter by tags if specified
               (when tags
                 (setq filtered-nodes
                       (cl-remove-if-not
                        (lambda (node)
                          (let ((node-tags (org-roam-node-tags node)))
                            (cl-every (lambda (tag)
                                       (member tag node-tags))
                                     tags)))
                        filtered-nodes)))
               ;; Sort nodes
               (setq filtered-nodes
                     (cl-sort filtered-nodes
                             (cond
                              ((equal sort-by "mtime")
                               (lambda (a b)
                                 (time-less-p (org-roam-node-file-mtime b)
                                            (org-roam-node-file-mtime a))))
                              ((equal sort-by "atime")
                               (lambda (a b)
                                 (time-less-p (org-roam-node-file-atime b)
                                            (org-roam-node-file-atime a))))
                              (t ; default to title
                               (lambda (a b)
                                 (string< (org-roam-node-title a)
                                        (org-roam-node-title b)))))))
               ;; Format results
               (let ((total (length filtered-nodes))
                     (results '()))
                 (dolist (node filtered-nodes)
                   (when (< count list-limit)
                     (let* ((id (org-roam-node-id node))
                            (title (org-roam-node-title node))
                            (node-tags (org-roam-node-tags node)))
                       (push (format "%d. [%s] %s%s"
                                    (1+ count)
                                    (substring id 0 (min 8 (length id)))
                                    title
                                    (if node-tags
                                        (format " #%s" (mapconcat 'identity node-tags " #"))
                                      ""))
                             results))
                     (setq count (1+ count))))
                 (gptel-org-roam--result-limit
                  (concat (format "Total nodes: %d (showing first %d, sorted by %s)\n\n"
                                 total count sort-by)
                         (mapconcat 'identity (nreverse results) "\n")
                         "\n\n[Use search_roam_nodes for targeted search or read_roam_node to view content]")))))
 :description "List all org-roam nodes with optional tag filtering and sorting. Returns a compact overview.

Use this to get an overview of your knowledge base, find nodes by tag, or browse recently modified/accessed nodes.

Returns up to 'limit' nodes (default 50). Results are sorted by title (alphabetical), mtime (recently modified), or atime (recently accessed).

After using this tool, STOP. Evaluate the list. Then use read_roam_node to view specific nodes, or search_roam_nodes for more targeted searches."
 :args (list '(:name "tags"
               :type array
               :items (:type string)
               :optional t
               :description "Filter by tags (nodes must have ALL specified tags)")
             '(:name "sort_by"
               :type string
               :enum ["title" "mtime" "atime"]
               :optional t
               :description "Sort order: 'title' (alphabetical, default), 'mtime' (modified), 'atime' (accessed)")
             '(:name "limit"
               :type integer
               :optional t
               :description "Maximum number of nodes to return (default: 50)"))
 :category "org-roam")

(gptel-make-tool
 :name "read_roam_node"
 :function (lambda (node-id &optional include-content include-metadata)
             (let ((node (gptel-org-roam--validate-node node-id)))
               (if (not node)
                   (format "Error: Node ID '%s' not found. Use search_roam_nodes to find valid nodes." node-id)
                 (let* ((granularity (or include-content "body"))
                        (content (gptel-org-roam--extract-content node granularity))
                        (result (format "Node: %s\nID: %s\nFile: %s\n\n"
                                       (org-roam-node-title node)
                                       node-id
                                       (org-roam-node-file node))))
                   ;; Add metadata if requested
                   (when include-metadata
                     (let ((tags (org-roam-node-tags node))
                           (aliases (org-roam-node-aliases node))
                           (refs (org-roam-node-refs node))
                           (level (org-roam-node-level node))
                           (todo (org-roam-node-todo node))
                           (priority (org-roam-node-priority node)))
                       (setq result
                             (concat result
                                    (format "Level: %d%s\n" level
                                           (if (= level 0) " (file-level node)" " (heading-level node)"))
                                    (when tags
                                      (format "Tags: #%s\n" (mapconcat 'identity tags " #")))
                                    (when aliases
                                      (format "Aliases: %s\n" (mapconcat 'identity aliases ", ")))
                                    (when refs
                                      (format "Refs: %s\n" (mapconcat 'identity refs ", ")))
                                    (when todo
                                      (format "TODO: %s\n" todo))
                                    (when priority
                                      (format "Priority: %s\n" priority))
                                    "\n"))))
                   ;; Add content
                   (setq result (concat result "Content:\n" content))
                   (gptel-org-roam--result-limit result)))))
 :description "Read org-roam node content with configurable detail level.

Use this after search_roam_nodes to read the full content of specific nodes.

Content granularity levels:
- 'heading': Just the node title/heading line
- 'body': Heading and immediate body text (no subheadings) - DEFAULT
- 'subtree': Full subtree including all subheadings
- 'full': Entire file content

Set include_metadata to true to see tags, aliases, refs, TODO state, and priority.

Results are limited to 40,000 characters. For large nodes, use 'heading' or 'body' granularity.

After using this tool, STOP. Analyze the content. Then continue with your task or use other tools as needed."
 :args (list '(:name "node_id"
               :type string
               :description "The org-roam node ID to read (from search_roam_nodes or list_roam_nodes)")
             '(:name "include_content"
               :type string
               :enum ["heading" "body" "subtree" "full"]
               :optional t
               :description "Content level: 'heading', 'body' (default), 'subtree', or 'full'")
             '(:name "include_metadata"
               :type boolean
               :optional t
               :description "Include node metadata (tags, aliases, refs, properties)"))
 :category "org-roam")

(gptel-make-tool
 :name "get_roam_node_metadata"
 :function (lambda (node-id)
             (let ((node (gptel-org-roam--validate-node node-id)))
               (if (not node)
                   (format "Error: Node ID '%s' not found. Use search_roam_nodes to find valid nodes." node-id)
                 (org-roam-populate node)
                 ;; Query backlinks
                 (let* ((backlinks (org-roam-db-query
                                   [:select [source] :from links
                                    :where (= dest $s1)]
                                   node-id))
                        (backlink-count (length backlinks))
                        ;; Query forward links
                        (forward-links (org-roam-db-query
                                       [:select [dest] :from links
                                        :where (= source $s1)]
                                       node-id))
                        (forward-count (length forward-links)))
                   (format "Node Metadata:\n\nID: %s\nTitle: %s\nFile: %s\nFile Title: %s\nLevel: %d%s\n\nTags: %s\nAliases: %s\nRefs: %s\n\nTODO: %s\nPriority: %s\nScheduled: %s\nDeadline: %s\n\nCreated: %s\nModified: %s\n\nBacklinks: %d\nForward Links: %d\n\n[Use query_roam_backlinks to see which nodes link here, or read_roam_node to view content]"
                          (org-roam-node-id node)
                          (org-roam-node-title node)
                          (org-roam-node-file node)
                          (or (org-roam-node-file-title node) "N/A")
                          (org-roam-node-level node)
                          (if (= (org-roam-node-level node) 0)
                              " (file-level node)"
                            " (heading-level node)")
                          (let ((tags (org-roam-node-tags node)))
                            (if tags (mapconcat 'identity tags ", ") "none"))
                          (let ((aliases (org-roam-node-aliases node)))
                            (if aliases (mapconcat 'identity aliases ", ") "none"))
                          (let ((refs (org-roam-node-refs node)))
                            (if refs (mapconcat 'identity refs ", ") "none"))
                          (or (org-roam-node-todo node) "none")
                          (or (org-roam-node-priority node) "none")
                          (or (org-roam-node-scheduled node) "none")
                          (or (org-roam-node-deadline node) "none")
                          (format-time-string "%Y-%m-%d %H:%M"
                                            (org-roam-node-file-atime node))
                          (format-time-string "%Y-%m-%d %H:%M"
                                            (org-roam-node-file-mtime node))
                          backlink-count
                          forward-count)))))
 :description "Get detailed metadata for an org-roam node without reading its content.

Use this to understand a node's properties, relationships, and context before reading its content. Useful for understanding node structure and connections.

Returns: ID, title, file info, level, tags, aliases, refs, TODO/priority/dates, timestamps, and link counts.

After using this tool, STOP. Evaluate the metadata. Then use read_roam_node to view content, or query_roam_backlinks to explore connections."
 :args (list '(:name "node_id"
               :type string
               :description "The org-roam node ID"))
 :category "org-roam")

(gptel-make-tool
 :name "query_roam_backlinks"
 :function (lambda (node-id &optional include-context)
             (let ((node (gptel-org-roam--validate-node node-id)))
               (if (not node)
                   (format "Error: Node ID '%s' not found. Use search_roam_nodes to find valid nodes." node-id)
                 ;; Query backlinks from database
                 (let* ((backlinks (org-roam-db-query
                                   [:select [source pos] :from links
                                    :where (= dest $s1)]
                                   node-id))
                        (count (length backlinks))
                        (results '()))
                   (if (= count 0)
                       (format "No backlinks found for node '%s' (ID: %s)"
                              (org-roam-node-title node) node-id)
                     (dolist (link backlinks)
                       (let* ((source-id (car link))
                              (pos (cadr link))
                              (source-node (gptel-org-roam--validate-node source-id)))
                         (when source-node
                           (let ((entry (format "- %s (ID: %s)\n  File: %s"
                                              (org-roam-node-title source-node)
                                              (substring source-id 0 (min 8 (length source-id)))
                                              (org-roam-node-file source-node))))
                             ;; Add context if requested
                             (when include-context
                               (let* ((file (org-roam-node-file source-node))
                                      (context (with-temp-buffer
                                                (insert-file-contents file)
                                                (goto-char pos)
                                                (let ((start (max (point-min) (- pos 100)))
                                                      (end (min (point-max) (+ pos 100))))
                                                  (buffer-substring-no-properties start end)))))
                                 (setq entry (concat entry "\n  Context: ..." context "..."))))
                             (push entry results)))))
                     (format "Found %d backlink%s to '%s':\n\n%s\n\n[Use read_roam_node to view the full content of these nodes]"
                            count
                            (if (> count 1) "s" "")
                            (org-roam-node-title node)
                            (mapconcat 'identity (nreverse results) "\n\n")))))))
 :description "Find all nodes that link to a target node (backlinks).

Use this to discover connections and see which nodes reference the target. Useful for understanding a node's context and relationships in your knowledge graph.

Set include_context to true to see the surrounding text where each link appears.

After using this tool, STOP. Analyze the backlinks. Then use read_roam_node to view specific linking nodes, or continue with your task."
 :args (list '(:name "node_id"
               :type string
               :description "ID of the target node to find backlinks for")
             '(:name "include_context"
               :type boolean
               :optional t
               :description "Include surrounding context text for each backlink"))
 :category "org-roam")

(defvar jf/markdown-to-org-debug t
  "When non-nil, log conversion inputs and outputs to ~/markdown-conversion-debug.log
Set to nil to disable debug logging once issues are resolved.")

(defun jf/markdown-to-org (text)
  "Convert markdown text to org-mode format using pandoc.
Falls back to returning text unchanged if pandoc is unavailable.
Removes PROPERTIES drawers that pandoc adds for cleaner output.
Detects if input is already org-mode format and skips conversion."
  ;; Debug logging if enabled
  (when jf/markdown-to-org-debug
    (with-temp-buffer
      (insert (format "\n\n========== CONVERSION INPUT [%s] ==========\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert text)
      (insert "\n========== END INPUT ==========\n")
      (append-to-file (point-min) (point-max) "~/markdown-conversion-debug.log")))

  ;; Check if input is already org-mode format
  ;; Org-mode indicators: lines starting with "* ", "#+begin_src", "#+end_src"
  (let ((is-org-mode (or (string-match-p "^\\* " text)
                         (string-match-p "^#\\+begin_src" text)
                         (string-match-p "^#\\+end_src" text))))
    (when (and jf/markdown-to-org-debug is-org-mode)
      (with-temp-buffer
        (insert (format "\n========== DETECTED ORG-MODE FORMAT - SKIPPING CONVERSION ==========\n"))
        (append-to-file (point-min) (point-max) "~/markdown-conversion-debug.log")))

    (if is-org-mode
        ;; Already org-mode, return as-is
        text
      ;; Not org-mode, proceed with conversion
      (if (executable-find "pandoc")
      (with-temp-buffer
        (insert text)
        ;; Call pandoc to convert from markdown to org
        ;; --wrap=none prevents pandoc from wrapping long lines
        ;; -f markdown = from markdown
        ;; -t org = to org-mode
        (let ((exit-code
               (call-process-region
                (point-min) (point-max)
                "pandoc"
                t    ; delete input text
                t    ; output to current buffer
                nil  ; no display
                "-f" "markdown"
                "-t" "org"
                "--wrap=none")))
          (if (eq exit-code 0)
              (progn
                ;; Remove PROPERTIES drawers that pandoc adds (CUSTOM_ID, etc.)
                ;; These add clutter and aren't needed for our use case
                (goto-char (point-min))
                (while (re-search-forward "^:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n" nil t)
                  (replace-match ""))

                ;; Clean up any empty lines left behind
                (goto-char (point-min))
                (while (re-search-forward "\n\n\n+" nil t)
                  (replace-match "\n\n"))

                (let ((output (buffer-string)))
                  ;; Debug logging if enabled
                  (when jf/markdown-to-org-debug
                    (with-temp-buffer
                      (insert (format "\n========== CONVERSION OUTPUT [%s] ==========\n"
                                      (format-time-string "%Y-%m-%d %H:%M:%S")))
                      (insert output)
                      (insert "\n========== END OUTPUT ==========\n\n")
                      (append-to-file (point-min) (point-max) "~/markdown-conversion-debug.log")))
                  output))
            (progn
              (message "Warning: pandoc conversion failed, using original text")
              text))))
        (progn
          (message "Warning: pandoc not found, markdown will not be converted. Install with: brew install pandoc")
          text)))))

(gptel-make-tool
 :name "create_roam_node"
 :function (lambda (title &optional tags content refs subdirectory capture-session-metadata)
             (let* ((subdir (or subdirectory ""))
                    (slug (org-roam-node-slug (org-roam-node-create :title title)))
                    (filename (format "%s-%s.org"
                                     (format-time-string "%Y%m%d%H%M%S")
                                     slug))
                    (target-dir (expand-file-name subdir org-roam-directory))
                    (filepath (expand-file-name filename target-dir))
                    (id (org-id-new))
                    (session-ctx (when capture-session-metadata
                                   (jf/gptel--get-session-context))))

               ;; Ensure target directory exists
               (unless (file-directory-p target-dir)
                 (make-directory target-dir t))

               ;; Create the file with org-roam structure
               (with-temp-file filepath
                 (insert (format ":PROPERTIES:\n:ID:       %s\n" id))

                 ;; Add gptel session metadata if requested and available
                 (when session-ctx
                   (when-let ((session-id (plist-get session-ctx :session-id)))
                     (insert (format ":GPTEL_SESSION: %s\n" session-id)))
                   (when-let ((agent (plist-get session-ctx :agent-name)))
                     (insert (format ":GPTEL_AGENT: %s\n" agent)))
                   (when-let ((model (plist-get session-ctx :model)))
                     (insert (format ":GPTEL_MODEL: %s\n" model)))
                   (when-let ((backend (plist-get session-ctx :backend)))
                     (insert (format ":GPTEL_BACKEND: %s\n" backend)))
                   (when-let ((timestamp (plist-get session-ctx :timestamp)))
                     (insert (format ":GPTEL_CREATED: %s\n" timestamp))))

                 (insert ":END:\n")
                 (insert (format "#+title: %s\n" title))

                 (when tags
                   (insert (format "#+filetags: :%s:\n" (mapconcat 'identity tags ":"))))
                 (when refs
                   (dolist (ref refs)
                     (insert (format "#+roam_refs: %s\n" ref))))
                 (insert "\n")
                 (when content
                   (insert (jf/markdown-to-org content))
                   (insert "\n")))

               ;; Update org-roam database
               (org-roam-db-update-file filepath)

               (format "Created new org-roam node:\n\nTitle: %s\nID: %s\nFile: %s\nDirectory: %s\nTags: %s\nRefs: %s%s\n\n[Use read_roam_node to view the node, or link_roam_nodes to connect it to other nodes]"
                      title
                      id
                      filepath
                      (if (string-empty-p subdir) "root" subdir)
                      (if tags (mapconcat 'identity tags ", ") "none")
                      (if refs (mapconcat 'identity refs ", ") "none")
                      (if session-ctx
                          (format "\nSession metadata captured: %s"
                                 (plist-get session-ctx :session-id))
                        ""))))
 :description "Create a new org-roam node programmatically.

Use this to add new nodes to your knowledge base. The node will be created with a unique ID and filename based on the title.

For gptel agents: Set capture_session_metadata to true and subdirectory to 'gptel' to create agent-attributed notes.

The node is created as a file-level node (level 0) in your org-roam-directory (or a subdirectory). You can add tags, initial content, and reference URLs.

MARKDOWN CONVERSION: Content will be automatically converted from markdown to org-mode format using pandoc (if available).

IMPORTANT: This operation creates a new file. Confirm before proceeding.

After creation, use read_roam_node to verify the node, or link_roam_nodes to connect it to existing nodes."
 :args (list '(:name "title"
               :type string
               :description "Title of the new node")
             '(:name "tags"
               :type array
               :items (:type string)
               :optional t
               :description "Tags to add to the node (filetags)")
             '(:name "content"
               :type string
               :optional t
               :description "Initial content for the node body")
             '(:name "refs"
               :type array
               :items (:type string)
               :optional t
               :description "Reference URLs to associate with the node")
             '(:name "subdirectory"
               :type string
               :optional t
               :description "Subdirectory within org-roam-directory (e.g., 'gptel', 'inbox'). Defaults to root.")
             '(:name "capture_session_metadata"
               :type boolean
               :optional t
               :description "If true, capture gptel session metadata (session_id, agent, model, backend) as properties"))
 :category "org-roam"
 :confirm nil)

(gptel-make-tool
 :name "create_reference_node"
 :function (lambda (url title summary &optional tags capture-session-metadata)
             (let* ((slug (org-roam-node-slug (org-roam-node-create :title title)))
                    (filename (format "%s-%s.org"
                                     (format-time-string "%Y%m%d%H%M%S")
                                     slug))
                    (target-dir (expand-file-name "reference" org-roam-directory))
                    (filepath (expand-file-name filename target-dir))
                    (id (org-id-new))
                    (session-ctx (when capture-session-metadata
                                   (jf/gptel--get-session-context))))

               ;; Ensure target directory exists
               (unless (file-directory-p target-dir)
                 (make-directory target-dir t))

               ;; Create the reference node file
               (with-temp-file filepath
                 (insert (format ":PROPERTIES:\n:ID:       %s\n" id))

                 ;; Add ROAM_REFS property for the URL
                 (insert (format ":ROAM_REFS: %s\n" url))

                 ;; Add gptel session metadata if requested and available
                 (when session-ctx
                   (when-let ((session-id (plist-get session-ctx :session-id)))
                     (insert (format ":GPTEL_SESSION: %s\n" session-id)))
                   (when-let ((agent (plist-get session-ctx :agent-name)))
                     (insert (format ":GPTEL_AGENT: %s\n" agent)))
                   (when-let ((model (plist-get session-ctx :model)))
                     (insert (format ":GPTEL_MODEL: %s\n" model)))
                   (when-let ((backend (plist-get session-ctx :backend)))
                     (insert (format ":GPTEL_BACKEND: %s\n" backend)))
                   (when-let ((timestamp (plist-get session-ctx :timestamp)))
                     (insert (format ":GPTEL_CREATED: %s\n" timestamp))))

                 (insert ":END:\n")
                 (insert (format "#+title: %s\n" title))

                 (when tags
                   (insert (format "#+filetags: :%s:\n" (mapconcat 'identity tags ":"))))

                 (insert "\n")
                 (insert "* Summary\n\n")
                 (insert (jf/markdown-to-org summary))
                 (insert "\n"))

               ;; Update org-roam database
               (org-roam-db-update-file filepath)

               (format "Created reference node:\n\nTitle: %s\nID: %s\nURL: %s\nFile: %s\nTags: %s%s\n\n[Use read_roam_node to view the reference, or link_roam_nodes to connect it to other nodes]"
                      title
                      id
                      url
                      filepath
                      (if tags (mapconcat 'identity tags ", ") "none")
                      (if session-ctx
                          (format "\nSession metadata captured: %s"
                                 (plist-get session-ctx :session-id))
                        ""))))
 :description "Create a reference node that summarizes external source material.

Reference nodes are specialized org-roam nodes that capture summaries of external
resources like web pages, papers, or documentation. They live in the reference/
subdirectory and use the ROAM_REFS property to link to their source URL.

Use this when:
- Summarizing a web article, blog post, or documentation page
- Capturing key points from an academic paper or book
- Creating a concise summary of any external reference material

The summary should be:
- Complete but concise (2-5 paragraphs typically)
- Focused on the key insights or information
- Written in your own words, not copied verbatim
- Sufficient to understand the main points without reading the source

MARKDOWN CONVERSION: Summary will be automatically converted from markdown to org-mode format using pandoc (if available).

Reference nodes can then be linked to your knowledge notes using link_roam_nodes
to show provenance and enable following sources.

IMPORTANT: This operation creates a new file. Confirm before proceeding."
 :args (list '(:name "url"
               :type string
               :description "URL of the source material (web page, paper, documentation)")
             '(:name "title"
               :type string
               :description "Title for the reference node (usually the source title)")
             '(:name "summary"
               :type string
               :description "Complete but concise summary of the reference content (2-5 paragraphs)")
             '(:name "tags"
               :type array
               :items (:type string)
               :optional t
               :description "Tags for categorization (e.g., 'paper', 'documentation', 'article')")
             '(:name "capture_session_metadata"
               :type boolean
               :optional t
               :description "If true, capture gptel session metadata as properties"))
 :category "org-roam"
 :confirm nil)

(gptel-make-tool
 :name "link_roam_nodes"
 :function (lambda (source-node-id target-node-id &optional position)
             (let ((source (gptel-org-roam--validate-node source-node-id))
                   (target (gptel-org-roam--validate-node target-node-id)))
               (if (not source)
                   (format "Error: Source node ID '%s' not found." source-node-id)
                 (if (not target)
                     (format "Error: Target node ID '%s' not found." target-node-id)
                   ;; Open source node and insert link
                   (let* ((file (org-roam-node-file source))
                          (point (org-roam-node-point source))
                          (level (org-roam-node-level source))
                          (position (or position "end"))
                          (target-title (org-roam-node-title target)))
                     (with-current-buffer (find-file-noselect file)
                       (save-excursion
                         ;; Navigate to appropriate position
                         (cond
                          ;; File-level node
                          ((= level 0)
                           (if (equal position "beginning")
                               (goto-char (point-min))
                             (goto-char (point-max))))
                          ;; Heading-level node
                          (t
                           (goto-char point)
                           (if (equal position "beginning")
                               (progn
                                 (forward-line 1)
                                 (when (looking-at "^[ \t]*:PROPERTIES:")
                                   (re-search-forward "^[ \t]*:END:" nil t)
                                   (forward-line 1)))
                             (outline-next-heading))))
                         ;; Insert the link
                         (insert (format "\n[[id:%s][%s]]\n" target-node-id target-title)))
                       (save-buffer))
                     ;; Update database
                     (org-roam-db-update-file file)
                     (format "Created link from '%s' to '%s'\n\nSource: %s (ID: %s)\nTarget: %s (ID: %s)\nPosition: %s\n\n[The link is bidirectional - org-roam tracks it in both directions]"
                            (org-roam-node-title source)
                            target-title
                            (org-roam-node-title source)
                            (substring source-node-id 0 (min 8 (length source-node-id)))
                            target-title
                            (substring target-node-id 0 (min 8 (length target-node-id)))
                            position))))))
 :description "Create a bidirectional link between two org-roam nodes.

Use this to connect related nodes in your knowledge graph. An id: link will be inserted in the source node pointing to the target node.

The link is bidirectional: org-roam automatically tracks it in both directions in its database. You can later use query_roam_backlinks to find this connection.

Position options:
- 'end' (default): Append link at the end of the node
- 'beginning': Insert link at the beginning (after properties drawer)

After linking, use read_roam_node or query_roam_backlinks to verify the connection."
 :args (list '(:name "source_node_id"
               :type string
               :description "ID of the source node (where the link will be inserted)")
             '(:name "target_node_id"
               :type string
               :description "ID of the target node (what the link points to)")
             '(:name "position"
               :type string
               :enum ["end" "beginning"]
               :optional t
               :description "Where to insert link: 'end' (default) or 'beginning'"))
 :category "org-roam")

(gptel-make-tool
 :name "add_roam_tags"
 :function (lambda (node-id tags)
             (let ((node (gptel-org-roam--validate-node node-id)))
               (if (not node)
                   (format "Error: Node ID '%s' not found." node-id)
                 (let* ((file (org-roam-node-file node))
                        (level (org-roam-node-level node))
                        (existing-tags (org-roam-node-tags node))
                        (new-tags (cl-remove-duplicates
                                  (append existing-tags tags)
                                  :test 'string=)))
                   (with-current-buffer (find-file-noselect file)
                     (save-excursion
                       (cond
                        ;; File-level node: use filetags
                        ((= level 0)
                         (goto-char (point-min))
                         ;; Look for existing filetags line
                         (if (re-search-forward "^#\\+filetags:" nil t)
                             (progn
                               (beginning-of-line)
                               (kill-line)
                               (insert (format "#+filetags: :%s:"
                                             (mapconcat 'identity new-tags ":"))))
                           ;; Insert new filetags line after title
                           (goto-char (point-min))
                           (when (re-search-forward "^#\\+title:" nil t)
                             (forward-line 1)
                             (insert (format "#+filetags: :%s:\n"
                                           (mapconcat 'identity new-tags ":"))))))
                        ;; Heading-level node: use heading tags
                        (t
                         (goto-char (org-roam-node-point node))
                         (org-set-tags (mapconcat 'identity new-tags ":")))))
                     (save-buffer))
                   ;; Update database
                   (org-roam-db-update-file file)
                   (format "Added tags to node '%s'\n\nNode ID: %s\nPrevious tags: %s\nNew tags: %s\n\n[Use get_roam_node_metadata or read_roam_node to verify the tags]"
                          (org-roam-node-title node)
                          (substring node-id 0 (min 8 (length node-id)))
                          (if existing-tags
                              (mapconcat 'identity existing-tags ", ")
                            "none")
                          (mapconcat 'identity new-tags ", "))))))
 :description "Add tags to an existing org-roam node.

Use this to categorize and organize nodes in your knowledge base. Existing tags are preserved, and duplicate tags are ignored.

For file-level nodes (level 0), tags are added as #+filetags.
For heading-level nodes, tags are added to the heading.

After adding tags, use search_roam_nodes or list_roam_nodes with tag filtering to find related nodes, or get_roam_node_metadata to verify the tags."
 :args (list '(:name "node_id"
               :type string
               :description "ID of the node to add tags to")
             '(:name "tags"
               :type array
               :items (:type string)
               :description "Tags to add (existing tags are preserved, duplicates ignored)"))
 :category "org-roam")

(provide 'org-roam-tools)
;;; org-roam-tools.el ends here
