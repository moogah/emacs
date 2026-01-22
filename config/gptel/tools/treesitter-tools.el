;;; treesitter-tools.el --- GPTEL tools for AST-based code exploration -*- lexical-binding: t; -*-
(require 'treesit)
(require 'jf-gptel-scope-core)

(defvar gptel-treesitter-result-limit 40000
  "Maximum character count for tool results to prevent context overflow.")

(defun gptel-treesitter--result-limit (result)
  "Limit RESULT to gptel-treesitter-result-limit characters.
If exceeded, return a warning message instead."
  (if (>= (length (format "%s" result)) gptel-treesitter-result-limit)
      (format "Results over %s characters. Use limit parameter to narrow results or increase limit."
              gptel-treesitter-result-limit)
    result))

(defun gptel-treesitter--detect-language (filepath)
  "Detect tree-sitter language from FILEPATH extension.
Returns language symbol or nil if not supported."
  (let ((ext (file-name-extension filepath)))
    (cond
     ((member ext '("py" "pyw")) 'python)
     ((member ext '("js" "mjs" "cjs")) 'javascript)
     ((member ext '("ts")) 'typescript)
     ((member ext '("tsx")) 'tsx)
     ((member ext '("go")) 'go)
     ((member ext '("rs")) 'rust)
     ((member ext '("c" "h")) 'c)
     ((member ext '("cpp" "hpp" "cc" "hh" "cxx")) 'cpp)
     ((member ext '("java")) 'java)
     ((member ext '("rb")) 'ruby)
     ((member ext '("el")) 'elisp)
     ((member ext '("sh" "bash")) 'bash)
     ((member ext '("json")) 'json)
     ((member ext '("yaml" "yml")) 'yaml)
     ((member ext '("toml")) 'toml)
     ((member ext '("html" "htm")) 'html)
     ((member ext '("css")) 'css)
     ((member ext '("md" "markdown")) 'markdown)
     ((member ext '("cmake")) 'cmake)
     (t nil))))

(defun gptel-treesitter--get-parser (filepath)
  "Get tree-sitter parser for FILEPATH.
Returns parser or nil if language not supported/available."
  (let ((language (gptel-treesitter--detect-language filepath)))
    (when (and language (treesit-language-available-p language))
      (with-temp-buffer
        (insert-file-contents filepath)
        (treesit-parser-create language)))))

(defconst gptel-treesitter-function-nodes
  '((python . ("function_definition" "async_function_definition"))
    (javascript . ("function_declaration" "method_definition" "arrow_function" "function_expression"))
    (typescript . ("function_declaration" "method_definition" "arrow_function" "function_signature"))
    (tsx . ("function_declaration" "method_definition" "arrow_function" "function_signature"))
    (go . ("function_declaration" "method_declaration"))
    (rust . ("function_item"))
    (c . ("function_definition"))
    (cpp . ("function_definition"))
    (java . ("method_declaration" "constructor_declaration"))
    (ruby . ("method" "singleton_method"))
    (elisp . ("defun"))
    (bash . ("function_definition")))
  "Node types representing functions/methods for each language.")

(defconst gptel-treesitter-class-nodes
  '((python . ("class_definition"))
    (javascript . ("class_declaration"))
    (typescript . ("class_declaration" "interface_declaration"))
    (tsx . ("class_declaration" "interface_declaration"))
    (go . ("type_declaration"))
    (rust . ("struct_item" "enum_item" "trait_item" "impl_item"))
    (c . ("struct_specifier"))
    (cpp . ("class_specifier" "struct_specifier"))
    (java . ("class_declaration" "interface_declaration" "enum_declaration"))
    (ruby . ("class" "module")))
  "Node types representing classes/types for each language.")

(defconst gptel-treesitter-import-nodes
  '((python . ("import_statement" "import_from_statement"))
    (javascript . ("import_statement"))
    (typescript . ("import_statement"))
    (tsx . ("import_statement"))
    (go . ("import_declaration"))
    (rust . ("use_declaration"))
    (c . ("preproc_include"))
    (cpp . ("preproc_include"))
    (java . ("import_declaration"))
    (ruby . ("require" "require_relative"))
    (elisp . ("require")))
  "Node types representing imports/requires for each language.")

(defun gptel-treesitter--get-node-types (language category)
  "Get node types for LANGUAGE and CATEGORY (function, class, or import)."
  (let ((mapping (cond
                  ((eq category 'function) gptel-treesitter-function-nodes)
                  ((eq category 'class) gptel-treesitter-class-nodes)
                  ((eq category 'import) gptel-treesitter-import-nodes)
                  (t nil))))
    (cdr (assoc language mapping))))

(defun gptel-treesitter--format-node (node &optional include-text)
  "Format NODE as readable string with type, position, and optionally text.
If INCLUDE-TEXT is non-nil, includes node text (truncated if long)."
  (let* ((type (treesit-node-type node))
         (start (treesit-node-start node))
         (end (treesit-node-end node))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (text (when include-text
                 (let ((node-text (treesit-node-text node t)))
                   (if (> (length node-text) 200)
                       (concat (substring node-text 0 197) "...")
                     node-text)))))
    (format "%s [%d:%d-%d:%d]%s"
            type start-line start end-line end
            (if text (format "\n  %s" text) ""))))

(defun gptel-treesitter--node-to-range-string (node)
  "Convert NODE position to readable range string 'L123-L456'."
  (format "L%d-L%d"
          (line-number-at-pos (treesit-node-start node))
          (line-number-at-pos (treesit-node-end node))))

(defun gptel-treesitter--validate-query (language query)
  "Validate QUERY string for LANGUAGE.
Returns error message string if invalid, nil if valid."
  (condition-case err
      (progn
        (treesit-query-compile language query)
        nil)
    (error (format "Invalid query: %s" (error-message-string err)))))

(defun gptel-treesitter--extract-function-signature (node language)
  "Extract function signature from NODE in LANGUAGE.
Returns string with function name and parameters."
  (let* ((name-node (treesit-node-child-by-field-name node "name"))
         (params-node (treesit-node-child-by-field-name node "parameters"))
         (name (when name-node (treesit-node-text name-node t)))
         (params (when params-node (treesit-node-text params-node t))))
    (if (and name params)
        (format "%s%s" name params)
      (treesit-node-text node t))))

(defun gptel-treesitter--extract-class-info (node language)
  "Extract class name and optional inheritance from NODE in LANGUAGE."
  (let* ((name-node (treesit-node-child-by-field-name node "name"))
         (name (when name-node (treesit-node-text name-node t)))
         (base-node (treesit-node-child-by-field-name node "superclass"))
         (base (when base-node (treesit-node-text base-node t))))
    (if base
        (format "%s (extends %s)" name base)
      name)))

(gptel-make-tool
 :name "list_treesitter_languages"
 :function (lambda ()
             (let* ((available-langs (mapcar #'car treesit-language-source-alist))
                    (installed-langs (seq-filter #'treesit-language-available-p available-langs))
                    (not-installed (seq-difference available-langs installed-langs)))
               (format "Tree-sitter Language Support:\n\n=== INSTALLED (%d) ===\n%s\n\n=== AVAILABLE BUT NOT INSTALLED (%d) ===\n%s\n\nTo install a language:\n  M-x treesit-install-language-grammar\n\nOr install all:\n  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))\n\nInstalled languages can be used with all tree-sitter tools."
                       (length installed-langs)
                       (mapconcat (lambda (lang) (format "  - %s" lang)) installed-langs "\n")
                       (length not-installed)
                       (if not-installed
                           (mapconcat (lambda (lang) (format "  - %s" lang)) not-installed "\n")
                         "  (all languages installed)"))))
 :description "List available tree-sitter language grammars and their installation status.

Shows which languages are:
- Installed and ready to use
- Available but need installation

Use this tool to:
- Check if a specific language is supported
- Verify parser availability before using other tree-sitter tools
- Understand what languages can be analyzed

Supported languages include:
- Python, JavaScript, TypeScript, TSX
- Go, Rust, C, C++, Java
- Ruby, Elisp, Bash
- JSON, YAML, TOML
- HTML, CSS, Markdown
- And more...

No arguments needed - returns complete language inventory.

Typical workflow:
1. list_treesitter_languages() - Check available languages
2. check_treesitter_parser(filepath) - Verify specific file support
3. Use other tree-sitter tools with confidence

Complements: check_treesitter_parser for file-specific validation."
 :args (list)
 :category "treesitter")

(gptel-make-tool
 :name "check_treesitter_parser"
 :function (lambda (filepath)
             (if (not (file-exists-p filepath))
                 (format "File not found: %s" filepath)
               (let* ((language (gptel-treesitter--detect-language filepath))
                      (available (when language (treesit-language-available-p language))))
                 (if (not language)
                     (format "Parser Status: NOT SUPPORTED\n\nFile: %s\nExtension: %s\nReason: File extension not recognized\n\nSupported extensions:\n  .py .js .ts .tsx .go .rs .c .cpp .java .rb .el .sh .json .yaml .toml .html .css .md\n\nUse list_treesitter_languages() to see all supported languages."
                             filepath (file-name-extension filepath))
                   (if available
                       (format "Parser Status: AVAILABLE\n\nFile: %s\nLanguage: %s\nParser: Installed and ready\n\nYou can use these tools:\n- list_functions(\"%s\")\n- list_classes(\"%s\")\n- query_nodes(\"%s\", query)\n- get_syntax_tree(\"%s\")\n\nNext steps:\n1. Use list_functions or list_classes for overview\n2. Use query_nodes for precise extraction\n3. Use get_syntax_tree to understand structure"
                               filepath language filepath filepath filepath filepath)
                     (format "Parser Status: NOT INSTALLED\n\nFile: %s\nLanguage: %s (recognized but parser not installed)\n\nTo install:\n  M-x treesit-install-language-grammar RET %s RET\n\nOr install all languages:\n  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))\n\nAfter installation, all tree-sitter tools will work with this file."
                             filepath language language))))))
 :description "Verify tree-sitter parser availability for a specific file.

CRITICAL: Use this tool BEFORE attempting other tree-sitter operations on a new file.

Returns:
- Parser availability status (AVAILABLE/NOT SUPPORTED/NOT INSTALLED)
- Detected language from file extension
- Installation instructions if needed
- Recommended next steps if parser is available

Use this to:
- Validate that a file can be parsed with tree-sitter
- Get language-specific information
- Troubleshoot parser issues
- Understand why tree-sitter tools might fail

Typical workflow:
1. check_treesitter_parser(filepath) - Verify support
2. If available: Use list_functions, query_nodes, etc.
3. If not installed: Follow installation instructions
4. If not supported: Use grep/Read tools instead

Arguments:
- filepath: Absolute path to file to check (required)

This tool prevents 'parser not available' errors by validating upfront.

Complements: list_treesitter_languages for general language inventory."
 :args (list '(:name "filepath"
               :type string
               :description "Absolute path to file to check"))
 :category "treesitter")

(gptel-make-scoped-tool
 "get_node_at_position"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Get AST node at a specific position in the file.

Use this to:
- Understand what syntactic element exists at a line/position
- Get node type and boundaries
- See immediate context around a position
- Start navigation from a specific point

Position can be:
- Line number (e.g., 42 or 'L42')
- Character position (e.g., 1234)

Returns:
- Node type (e.g., function_definition, class_declaration)
- Exact start/end positions (line and column)
- Parent node type
- Node text content
- Suggested next tools to use

Typical workflow:
1. get_node_at_position(file, line) - Identify node at location
2. get_node_info(file, line) - Get detailed node analysis
3. get_node_context(file, line) - See surrounding structure
4. extract_definition(file, symbol) - Get complete definition

Arguments:
- filepath: Absolute path to file (required)
- position: Line number (42 or 'L42') or character position (required)

IMPORTANT: Use check_treesitter_parser first to verify parser availability.

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- get_node_info: More detailed node analysis
- get_node_context: Surrounding code structure
- Read tool: Full file content"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "position"
         :type string
         :description "Line number (42 or 'L42') or character position"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed. Use check_treesitter_parser(\"%s\")"
                                      language filepath))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (pos (cond
                          ((numberp position) position)
                          ((string-match "^L?\\([0-9]+\\)$" position)
                           (goto-char (point-min))
                           (forward-line (1- (string-to-number (match-string 1 position))))
                           (point))
                          (t (error "Invalid position format"))))
                    (node (treesit-node-at pos)))
               (if (not node)
                   (list :success nil
                         :error "node_not_found"
                         :message (format "No node found at position %s" position))
                 (let ((node-type (treesit-node-type node))
                       (start-line (line-number-at-pos (treesit-node-start node)))
                       (start-col (save-excursion (goto-char (treesit-node-start node)) (current-column)))
                       (end-line (line-number-at-pos (treesit-node-end node)))
                       (end-col (save-excursion (goto-char (treesit-node-end node)) (current-column)))
                       (parent-type (if (treesit-node-parent node)
                                        (treesit-node-type (treesit-node-parent node))
                                      "none (root)"))
                       (node-text (treesit-node-text node t)))
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :position position
                         :node_type node-type
                         :start_line start-line
                         :start_col start-col
                         :end_line end-line
                         :end_col end-col
                         :parent_type parent-type
                         :node_text node-text
                         :message (format "Node at Position %s:\n\nFile: %s\nLanguage: %s\n\nNode Type: %s\nStart: Line %d, Col %d\nEnd: Line %d, Col %d\nParent Type: %s\n\nNode Text:\n%s\n\nNext steps:\n- Use get_node_info(\"%s\", %s) for detailed analysis\n- Use get_node_context(\"%s\", %s) to see surrounding code"
                                          position filepath (symbol-name language)
                                          node-type start-line start-col end-line end-col
                                          parent-type node-text filepath position filepath position)))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nPossible causes:\n1. Tree-sitter parser not available\n2. File syntax errors\n3. Invalid position\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "get_node_info"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Get detailed information about a node including parent, children, and fields.

More comprehensive than get_node_at_position - shows full node details.

Returns:
- Node type, range, and text
- Parent node information
- Children count and optional list
- Field values (name, parameters, body, type, etc.)

Use this when:
- You need to understand node structure in depth
- You want to see relationships (parent/children)
- You're building a mental model of the AST
- You need specific field values

Arguments:
- filepath: Absolute path to file (required)
- position: Line number or character position (required)
- show_children: Show all children nodes (optional, default false)

Position format: 42 or 'L42' for line numbers

Typical workflow:
1. get_node_at_position(file, line) - Quick node identification
2. get_node_info(file, line, true) - Deep analysis with children
3. query_nodes(file, pattern) - Extract similar nodes
4. extract_definition(file, symbol) - Get complete definition

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- get_node_at_position: Quick node lookup
- get_syntax_tree: Broader structural view
- get_node_context: Surrounding code context"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "position"
         :type string
         :description "Line number (42 or 'L42') or character position")
       '(:name "show_children"
         :type boolean
         :optional t
         :description "Show all children nodes (default false)"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (pos (cond
                          ((numberp position) position)
                          ((string-match "^L?\\([0-9]+\\)$" position)
                           (goto-char (point-min))
                           (forward-line (1- (string-to-number (match-string 1 position))))
                           (point))
                          (t (error "Invalid position"))))
                    (node (treesit-node-at pos))
                    (parent (when node (treesit-node-parent node)))
                    (children (when node (treesit-node-children node))))
               (if (not node)
                   (list :success nil
                         :error "node_not_found"
                         :message (format "No node found at position %s" position))
                 (let* ((node-type (treesit-node-type node))
                        (node-range (gptel-treesitter--node-to-range-string node))
                        (node-text-full (treesit-node-text node t))
                        (node-text (if (> (length node-text-full) 100)
                                       (concat (substring node-text-full 0 97) "...")
                                     node-text-full))
                        (parent-type (if parent (treesit-node-type parent) "none (root)"))
                        (parent-range (if parent (gptel-treesitter--node-to-range-string parent) "N/A"))
                        (children-count (length children))
                        (children-list (when (and show-children children)
                                         (mapcar (lambda (child)
                                                   (list :type (treesit-node-type child)
                                                         :range (gptel-treesitter--node-to-range-string child)))
                                                 children)))
                        (field-values (let ((fields '("name" "parameters" "body" "type" "value" "superclass"))
                                            (result '()))
                                        (dolist (field fields)
                                          (let ((field-node (treesit-node-child-by-field-name node field)))
                                            (when field-node
                                              (push (cons (intern (concat ":" field))
                                                          (treesit-node-text field-node t))
                                                    result))))
                                        (nreverse result)))
                        (children-display (if (and show-children children)
                                              (mapconcat (lambda (child)
                                                           (format "  - %s: %s"
                                                                   (treesit-node-type child)
                                                                   (gptel-treesitter--node-to-range-string child)))
                                                         children "\n")
                                            (if children
                                                (format "  [Use show_children=true to see all %d children]" children-count)
                                              "  none")))
                        (tip (if (not show-children) "\n\nTip: Use show_children=true for full child list" ""))
                        (fields-display (let ((fields '("name" "parameters" "body" "type" "value" "superclass")))
                                          (mapconcat (lambda (field)
                                                       (let ((field-node (treesit-node-child-by-field-name node field)))
                                                         (if field-node
                                                             (format "  - %s: %s" field (treesit-node-text field-node t))
                                                           nil)))
                                                     fields "\n"))))
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :position position
                         :node (list :type node-type
                                     :range node-range
                                     :text node-text)
                         :parent (list :type parent-type
                                       :range parent-range)
                         :children_count children-count
                         :children children-list
                         :fields field-values
                         :message (format "Detailed Node Info:\n\nFile: %s (%s)\nPosition: %s\n\n=== NODE ===\nType: %s\nRange: %s\nText: %s\n\n=== PARENT ===\nType: %s\nRange: %s\n\n=== CHILDREN (%d) ===\n%s%s\n\nField names available:\n%s"
                                          filepath (symbol-name language) position
                                          node-type node-range node-text
                                          parent-type parent-range
                                          children-count children-display tip fields-display)))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "get_syntax_tree"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Get AST subtree representation with configurable depth.

Visualizes the abstract syntax tree structure as a hierarchical tree.

Use this to:
- Understand overall code structure
- See how nodes are nested
- Identify structural patterns
- Debug AST-related issues
- Learn tree-sitter node types for a language

Returns hierarchical tree showing:
- Node types at each level
- Line ranges for each node
- Parent-child relationships
- Nesting depth

Arguments:
- filepath: Absolute path to file (required)
- depth: Tree depth to show (default 3, max 5) (optional)
- start_line: Start of range to analyze (optional, requires end_line)
- end_line: End of range to analyze (optional, requires start_line)

Depth levels:
- 1: Top-level structure only (very fast)
- 3: Good balance (default)
- 5: Maximum detail (may be large)

Typical workflow:
1. get_syntax_tree(file) - See overall structure at depth 3
2. get_syntax_tree(file, 5, 100, 150) - Deep dive on specific range
3. query_nodes(file, pattern) - Extract specific patterns found in tree
4. list_functions(file) - Get semantic summary

IMPORTANT: Large files with depth 5 may produce huge output.
Use start_line/end_line to focus on specific sections.

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- list_functions/list_classes: Semantic extraction
- get_node_info: Detailed single-node analysis
- query_nodes: Pattern-based extraction"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "depth"
         :type integer
         :optional t
         :description "Tree depth (default 3, max 5)")
       '(:name "start_line"
         :type integer
         :optional t
         :description "Start line of range (requires end_line)")
       '(:name "end_line"
         :type integer
         :optional t
         :description "End line of range (requires start_line)"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath))
              (max-depth (min (or depth 3) 5)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (start-pos (when start-line
                                 (progn (goto-char (point-min))
                                        (forward-line (1- start-line))
                                        (point))))
                    (end-pos (when end-line
                               (progn (goto-char (point-min))
                                      (forward-line end-line)
                                      (point))))
                    (target-node (if (and start-pos end-pos)
                                     (treesit-node-at start-pos)
                                   root-node)))
               (cl-labels ((format-tree (node current-depth)
                                        (let ((indent (make-string (* current-depth 2) ?\s)))
                                          (if (>= current-depth max-depth)
                                              (format "%s%s [...]\n" indent (treesit-node-type node))
                                            (let ((children (treesit-node-children node)))
                                              (concat
                                               (format "%s%s [%s]\n"
                                                       indent
                                                       (treesit-node-type node)
                                                       (gptel-treesitter--node-to-range-string node))
                                               (mapconcat (lambda (child)
                                                            (format-tree child (1+ current-depth)))
                                                          children "")))))))
                 (let ((result (format "Syntax Tree:\n\nFile: %s (%s)\nDepth: %d (max 5)\n%s\n%s"
                                       filepath (symbol-name language) max-depth
                                       (if (and start-line end-line)
                                           (format "Range: L%d-L%d\n" start-line end-line)
                                         "Range: Full file\n")
                                       (format-tree target-node 0))))
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :depth max-depth
                         :start_line start-line
                         :end_line end-line
                         :tree result
                         :message (gptel-treesitter--result-limit result)))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "list_functions"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Extract all function/method definitions with signatures.

PRIMARY tool for understanding what functions exist in a file.

Use this to:
- Get a quick overview of all functions in a file
- Find function locations (file:line)
- See function signatures (with include_signatures=true)
- Understand file's functional structure

Returns list of functions with:
- Function name
- Line number
- Optional signature (name + parameters)

Arguments:
- filepath: Absolute path to file (required)
- include_signatures: Show full signatures (default false) (optional)
- limit: Max functions to return (default 50, max 200) (optional)

Supported languages:
Python, JavaScript, TypeScript, Go, Rust, C, C++, Java, Ruby, Elisp, Bash

Typical workflow:
1. list_functions(file) - Quick overview with names only
2. list_functions(file, true) - Detailed view with signatures
3. extract_definition(file, \"function_name\") - Get full implementation
4. find_references(dir, \"function_name\") [ggtags] - See usage

Example output:
  utils.py:42: process_data(input, options)
  utils.py:67: validate_input(data)
  utils.py:89: transform_output(result)

IMPORTANT: This is much faster than grep for finding functions because
it understands code structure, not just text patterns.

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- list_classes: See class definitions
- extract_definition: Get complete function code
- find_definition [ggtags]: Cross-file function location
- Read tool: See full file content"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "include_signatures"
         :type boolean
         :optional t
         :description "Show full signatures (default false)")
       '(:name "limit"
         :type integer
         :optional t
         :description "Max functions (default 50, max 200)"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath))
              (file-limit (min (or limit 50) 200)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (function-types (gptel-treesitter--get-node-types language 'function))
                    (functions '()))
               (when function-types
                 (dolist (func-type function-types)
                   (let ((nodes (treesit-query-capture
                                root-node
                                (format "(%s) @func" func-type))))
                     (dolist (capture nodes)
                       (push (cdr capture) functions)))))
               (setq functions (seq-take (nreverse functions) file-limit))
               (if (null functions)
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :count 0
                         :functions '()
                         :message (format "No functions found in %s\n\nPossible reasons:\n1. File has no function definitions\n2. Language '%s' not configured for function extraction\n\nTry:\n- get_syntax_tree(\"%s\") to see structure\n- query_nodes(\"%s\", \"(function_definition) @f\") for custom queries"
                                          filepath (symbol-name language) filepath filepath))
                 (let* ((functions-data (mapcar
                                         (lambda (node)
                                           (let* ((line (line-number-at-pos (treesit-node-start node)))
                                                  (sig (if include-signatures
                                                           (gptel-treesitter--extract-function-signature node language)
                                                         (let ((name-node (treesit-node-child-by-field-name node "name")))
                                                           (if name-node
                                                               (treesit-node-text name-node t)
                                                             (format "%s" (treesit-node-type node)))))))
                                             (list :line line
                                                   :name sig
                                                   :type (treesit-node-type node))))
                                         functions))
                        (result (format "Functions in %s (%s):\n\nFound: %d function%s%s\n\n%s"
                                        (file-name-nondirectory filepath)
                                        (symbol-name language)
                                        (length functions)
                                        (if (= (length functions) 1) "" "s")
                                        (if (>= (length functions) file-limit)
                                            (format " [showing %d, use limit to adjust]" file-limit)
                                          "")
                                        (mapconcat
                                         (lambda (func-data)
                                           (format "  %s:%d: %s"
                                                   (file-name-nondirectory filepath)
                                                   (plist-get func-data :line)
                                                   (plist-get func-data :name)))
                                         functions-data "\n"))))
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :count (length functions)
                         :functions functions-data
                         :message (gptel-treesitter--result-limit result)))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "list_classes"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Extract all class/struct/interface declarations.

Use this to:
- Get overview of all classes in a file
- See class hierarchy (with include_inheritance=true)
- List class members/methods (with include_members=true)
- Understand file's object-oriented structure

Returns list of classes with:
- Class name
- Line number
- Optional inheritance info
- Optional member list

Arguments:
- filepath: Absolute path to file (required)
- include_members: Show class members/methods (optional, default false)
- include_inheritance: Show base classes (optional, default false)
- limit: Max classes to return (default 50, max 200) (optional)

Supported languages:
Python, JavaScript, TypeScript, Go (types), Rust (structs), C++, Java, Ruby

Typical workflow:
1. list_classes(file) - Quick overview with names only
2. list_classes(file, true, true) - Detailed view with members and inheritance
3. extract_definition(file, \"ClassName\") - Get full class code
4. list_functions(file) - See all methods including those outside classes

Example output:
  models.py:15: User (extends BaseModel)
      Members: __init__, save, validate, to_dict
  models.py:67: Session
      Members: __init__, start, end

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- list_functions: See all functions/methods
- extract_definition: Get complete class code
- find_definition [ggtags]: Cross-file class location"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "include_members"
         :type boolean
         :optional t
         :description "Show class members/methods")
       '(:name "include_inheritance"
         :type boolean
         :optional t
         :description "Show base classes")
       '(:name "limit"
         :type integer
         :optional t
         :description "Max classes (default 50, max 200)"))

 "treesitter"

 (condition-case err
       (with-temp-buffer
         (insert-file-contents filepath)
         (let* ((language (gptel-treesitter--detect-language filepath))
                (file-limit (min (or limit 50) 200)))
           (if (not language)
               (list :success nil
                     :error "unsupported_file"
                     :message (format "Error: Unsupported file type for %s" filepath))
             (if (not (treesit-language-available-p language))
                 (list :success nil
                       :error "parser_not_installed"
                       :message (format "Error: Parser for %s not installed" language))
               (let* ((parser (treesit-parser-create language))
                      (root-node (treesit-parser-root-node parser))
                      (class-types (gptel-treesitter--get-node-types language 'class))
                      (classes '()))
                 (when class-types
                   (dolist (class-type class-types)
                     (let ((nodes (treesit-query-capture
                                  root-node
                                  (format "(%s) @class" class-type))))
                       (dolist (capture nodes)
                         (push (cdr capture) classes)))))
                 (setq classes (seq-take (nreverse classes) file-limit))
                 (if (null classes)
                     (list :success t
                           :filepath filepath
                           :language (symbol-name language)
                           :count 0
                           :classes '()
                           :message (format "No classes found in %s\n\nPossible reasons:\n1. File has no class definitions\n2. Language '%s' may not have class concept\n\nTry:\n- get_syntax_tree(\"%s\") to see structure\n- list_functions(\"%s\") for function-based code"
                                            filepath (symbol-name language) filepath filepath))
                   (let* ((classes-data (mapcar
                                         (lambda (node)
                                           (let* ((line (line-number-at-pos (treesit-node-start node)))
                                                  (name (if include-inheritance
                                                            (gptel-treesitter--extract-class-info node language)
                                                          (let ((name-node (treesit-node-child-by-field-name node "name")))
                                                            (if name-node
                                                                (treesit-node-text name-node t)
                                                              (treesit-node-type node)))))
                                                  (members (when include-members
                                                             (let ((body-node (treesit-node-child-by-field-name node "body")))
                                                               (when body-node
                                                                 (let* ((func-types (gptel-treesitter--get-node-types language 'function))
                                                                        (methods '()))
                                                                   (when func-types
                                                                     (dolist (func-type func-types)
                                                                       (let ((method-nodes (treesit-query-capture
                                                                                           body-node
                                                                                           (format "(%s) @method" func-type))))
                                                                         (dolist (capture method-nodes)
                                                                           (push (cdr capture) methods)))))
                                                                   (when methods
                                                                     (mapcar
                                                                      (lambda (m)
                                                                        (let ((mname (treesit-node-child-by-field-name m "name")))
                                                                          (if mname (treesit-node-text mname t) "?")))
                                                                      (nreverse methods)))))))))
                                             (append (list :line line :name name :type (treesit-node-type node))
                                                     (when members (list :members members)))))
                                         classes))
                          (result (format "Classes in %s (%s):\n\nFound: %d class%s%s\n\n%s"
                                          (file-name-nondirectory filepath)
                                          (symbol-name language)
                                          (length classes)
                                          (if (= (length classes) 1) "" "es")
                                          (if (>= (length classes) file-limit)
                                              (format " [showing %d, use limit to adjust]" file-limit)
                                            "")
                                          (mapconcat
                                           (lambda (class-data)
                                             (let* ((line (plist-get class-data :line))
                                                    (name (plist-get class-data :name))
                                                    (members (plist-get class-data :members))
                                                    (members-info (when members
                                                                    (format "\n      Members: %s"
                                                                            (mapconcat #'identity members ", ")))))
                                               (format "  %s:%d: %s%s"
                                                       (file-name-nondirectory filepath)
                                                       line
                                                       name
                                                       (or members-info ""))))
                                           classes-data "\n"))))
                     (list :success t
                           :filepath filepath
                           :language (symbol-name language)
                           :count (length classes)
                           :classes classes-data
                           :message (gptel-treesitter--result-limit result))))))))
     (error (list :success nil
                  :error "tool_exception"
                  :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                   (error-message-string err) filepath))))))

(gptel-make-scoped-tool
 "list_imports"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Extract import/require/include statements.

Use this to:
- Understand file dependencies
- See what modules/packages are used
- Identify external vs internal dependencies
- Map dependency graph

Returns list of imports with:
- Import statement text
- Line number
- Optional grouping by source module

Arguments:
- filepath: Absolute path to file (required)
- group_by_source: Group imports by source module (optional, default false)

Supported languages:
Python, JavaScript, TypeScript, Go, Rust, C/C++, Java, Ruby, Elisp

Typical workflow:
1. list_imports(file) - See all dependencies
2. list_imports(file, true) - Group by source for clarity
3. Check if imported modules exist in project
4. list_functions(file) - See what those imports are used for

Example output:
  utils.py:1: import os
  utils.py:2: import sys
  utils.py:3: from datetime import datetime
  utils.py:4: from .models import User, Session

With grouping:
  From os: import os
  From sys: import sys
  From datetime: from datetime import datetime
  From .models: from .models import User, Session

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- list_functions: See what imports are used for
- find_references [ggtags]: Find where imported symbols are used
- projectile tools: Locate imported files in project"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "group_by_source"
         :type boolean
         :optional t
         :description "Group imports by source module"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (import-types (gptel-treesitter--get-node-types language 'import))
                    (imports '()))
               (when import-types
                 (dolist (import-type import-types)
                   (let ((nodes (treesit-query-capture
                                root-node
                                (format "(%s) @import" import-type))))
                     (dolist (capture nodes)
                       (push (cdr capture) imports)))))
               (setq imports (nreverse imports))
               (if (null imports)
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :count 0
                         :imports '()
                         :message (format "No imports found in %s\n\nPossible reasons:\n1. File has no import statements\n2. Language '%s' may not have import concept\n3. Using relative imports not captured by parser\n\nFile dependencies may be:\n- Implicit (no imports needed)\n- Configured externally (build system)\n- Using alternative import syntax"
                                          filepath (symbol-name language)))
                 (let* ((imports-data (mapcar
                                       (lambda (node)
                                         (list :line (line-number-at-pos (treesit-node-start node))
                                               :text (treesit-node-text node t)))
                                       imports))
                        (result (format "Imports in %s (%s):\n\nFound: %d import%s\n\n%s"
                                        (file-name-nondirectory filepath)
                                        (symbol-name language)
                                        (length imports)
                                        (if (= (length imports) 1) "" "s")
                                        (if group-by-source
                                            (let ((grouped (make-hash-table :test 'equal)))
                                              (dolist (node imports)
                                                (let* ((text (treesit-node-text node t))
                                                       (source (if (string-match "from \\([^ ]+\\)" text)
                                                                   (match-string 1 text)
                                                                 (if (string-match "import \\([^ ]+\\)" text)
                                                                     (match-string 1 text)
                                                                   "unknown"))))
                                                  (push text (gethash source grouped))))
                                              (let ((result-str ""))
                                                (maphash (lambda (source imports-list)
                                                           (setq result-str
                                                                 (concat result-str
                                                                         (format "From %s:\n%s\n\n"
                                                                                 source
                                                                                 (mapconcat (lambda (i)
                                                                                              (format "  - %s" i))
                                                                                            (nreverse imports-list)
                                                                                            "\n")))))
                                                         grouped)
                                                result-str))
                                          (mapconcat
                                           (lambda (import-data)
                                             (format "  %s:%d: %s"
                                                     (file-name-nondirectory filepath)
                                                     (plist-get import-data :line)
                                                     (plist-get import-data :text)))
                                           imports-data "\n")))))
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :count (length imports)
                         :imports imports-data
                         :message (gptel-treesitter--result-limit result)))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "extract_definition"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Get complete definition of a specific symbol (function, class, etc.).

PRIMARY tool for extracting specific symbol code.

Use this to:
- Get full function/method implementation
- Extract complete class definition
- See exact code for a symbol
- Understand implementation details

Returns:
- Complete source code for the symbol
- Symbol type (function_definition, class_declaration, etc.)
- Line range
- Multiple matches if symbol is overloaded

Arguments:
- filepath: Absolute path to file (required)
- symbol_name: Name of symbol to extract (required, case-sensitive)

IMPORTANT: Symbol name must exactly match (case-sensitive).
Use list_functions or list_classes first to see available symbols.

Typical workflow:
1. list_functions(file) - See what functions exist
2. extract_definition(file, \"function_name\") - Get full code
3. get_scope_structure(file, start_line, end_line) - Analyze complexity
4. find_references(dir, \"function_name\") [ggtags] - See usage

Example output:
  Definition of 'process_data' in utils.py:
  Type: function_definition
  Lines: L42-L58

  --- CODE ---
  def process_data(input, options=None):
      if options is None:
          options = {}
      # ... full implementation ...
      return result
  --- END ---

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- list_functions: Discover available functions
- find_definition [ggtags]: Cross-file symbol location
- get_node_info: Detailed AST analysis
- Read tool: Full file context"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "symbol_name"
         :type string
         :description "Symbol name (case-sensitive)"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (all-types (append (gptel-treesitter--get-node-types language 'function)
                                       (gptel-treesitter--get-node-types language 'class)))
                    (matches '()))
               (when all-types
                 (dolist (node-type all-types)
                   (let ((nodes (treesit-query-capture
                                root-node
                                (format "(%s) @def" node-type))))
                     (dolist (capture nodes)
                       (let* ((node (cdr capture))
                              (name-node (treesit-node-child-by-field-name node "name"))
                              (name (when name-node (treesit-node-text name-node t))))
                         (when (and name (string= name symbol-name))
                           (push node matches)))))))
               (if (null matches)
                   (list :success nil
                         :error "symbol_not_found"
                         :symbol symbol-name
                         :message (format "Symbol '%s' not found in %s\n\nPossible reasons:\n1. Symbol doesn't exist in file\n2. Typo in symbol name (case-sensitive)\n3. Symbol is not a function/class\n\nTry:\n- list_functions(\"%s\") to see available functions\n- list_classes(\"%s\") to see available classes\n- grep_project or find_definition [ggtags] for cross-file search"
                                          symbol-name filepath filepath filepath))
                 (let* ((node (car matches))
                        (count (length matches))
                        (node-type (treesit-node-type node))
                        (node-range (gptel-treesitter--node-to-range-string node))
                        (node-text (treesit-node-text node t)))
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :symbol_name symbol-name
                         :matches_count count
                         :definition (list :type node-type
                                           :range node-range
                                           :code node-text)
                         :message (format "Definition of '%s' in %s:\n\nMatches found: %d%s\nType: %s\nLines: %s\n\n--- CODE ---\n%s\n--- END ---\n\n%s"
                                          symbol-name
                                          (file-name-nondirectory filepath)
                                          count
                                          (if (> count 1) " (showing first)" "")
                                          node-type
                                          node-range
                                          node-text
                                          (if (> count 1)
                                              (format "Note: Found %d definitions. Use find_symbol [ggtags] for multi-file search." count)
                                            "Use find_references [ggtags] to see where this is called."))))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "query_nodes"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Execute tree-sitter query patterns (most powerful tool).

ADVANCED tool for precise AST extraction using query language.

Use this when:
- Standard tools (list_functions, list_classes) don't extract what you need
- You need specific node patterns
- You want precise control over extraction
- You're familiar with tree-sitter query syntax

Query syntax examples:
Python:
  (function_definition name: (identifier) @name)
  (class_definition name: (identifier) @class body: (block) @body)
  (call function: (identifier) @func_name)

JavaScript/TypeScript:
  (arrow_function parameter: (_) @params body: (_) @body)
  (export_statement) @export
  (method_definition name: (property_identifier) @method)

Returns:
- All captured nodes matching query
- Capture names (@ labels)
- Line numbers
- Node text (truncated if long)

Arguments:
- filepath: Absolute path to file (required)
- query: Tree-sitter query pattern (required)
- limit: Max results (default 100, max 300) (optional)

Query pattern format:
  (node_type field_name: (field_type) @capture_name)

Components:
- node_type: AST node type (use get_syntax_tree to discover)
- field_name: Named field (name, parameters, body, etc.)
- @capture_name: Label for capturing the match

Typical workflow:
1. get_syntax_tree(file, 2) - See available node types
2. query_nodes(file, \"(function_definition) @f\") - Test simple query
3. query_nodes(file, complex_query) - Extract precise patterns
4. extract_definition or Read for full context

IMPORTANT: Query syntax errors are common. Start simple and build up.

Example queries:
- All functions: \"(function_definition) @func\"
- Function names only: \"(function_definition name: (identifier) @name)\"
- Async functions (Python): \"(async_function_definition) @async_func\"
- All calls: \"(call) @call\"

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- find_nodes_by_type: Simpler type-based search
- get_syntax_tree: Discover node types for queries
- list_functions/list_classes: Standard semantic extraction"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "query"
         :type string
         :description "Tree-sitter query pattern")
       '(:name "limit"
         :type integer
         :optional t
         :description "Max results (default 100, max 300)"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath))
              (result-limit (min (or limit 100) 300)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let ((validation-error (gptel-treesitter--validate-query language query)))
               (if validation-error
                   (list :success nil
                         :error "invalid_query"
                         :query query
                         :message (format "%s\n\nQuery syntax guide:\n- Match nodes: (function_definition) @func\n- Match fields: (function_definition name: (identifier) @name)\n- Example: \"(class_definition name: (identifier) @classname)\"\n\nUse get_syntax_tree(\"%s\") to see available node types."
                                          validation-error filepath))
                 (let* ((parser (treesit-parser-create language))
                        (root-node (treesit-parser-root-node parser))
                        (captures (treesit-query-capture root-node query))
                        (limited-captures (seq-take captures result-limit)))
                   (if (null captures)
                       (list :success t
                             :filepath filepath
                             :language (symbol-name language)
                             :query query
                             :count 0
                             :captures '()
                             :message (format "No matches for query: %s\n\nThe query is valid but found no matches.\n\nTry:\n- get_syntax_tree(\"%s\", 2) to see node types\n- Simpler query: \"(function_definition) @func\"\n- Check node type spelling"
                                              query filepath))
                     (let* ((captures-data (mapcar
                                            (lambda (capture)
                                              (let* ((capture-name (car capture))
                                                     (node (cdr capture))
                                                     (line (line-number-at-pos (treesit-node-start node)))
                                                     (text (treesit-node-text node t))
                                                     (short-text (if (> (length text) 100)
                                                                     (concat (substring text 0 97) "...")
                                                                   text)))
                                                (list :capture_name (symbol-name capture-name)
                                                      :line line
                                                      :type (treesit-node-type node)
                                                      :text short-text)))
                                            limited-captures))
                            (result (format "Query Results for %s (%s):\n\nQuery: %s\nMatches: %d%s\n\n%s"
                                            (file-name-nondirectory filepath)
                                            (symbol-name language)
                                            query
                                            (length captures)
                                            (if (> (length captures) result-limit)
                                                (format " [showing %d, use limit to adjust]" result-limit)
                                              "")
                                            (mapconcat
                                             (lambda (capture-data)
                                               (format "@%s at %s:%d:\n  %s"
                                                       (plist-get capture-data :capture_name)
                                                       (file-name-nondirectory filepath)
                                                       (plist-get capture-data :line)
                                                       (plist-get capture-data :text)))
                                             captures-data "\n\n"))))
                       (list :success t
                             :filepath filepath
                             :language (symbol-name language)
                             :query query
                             :count (length captures)
                             :captures captures-data
                             :message (gptel-treesitter--result-limit result)))))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "find_nodes_by_type"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Find all nodes of specific type(s) - simpler alternative to query_nodes.

Use this when:
- You know the node type you want
- You don't need complex query patterns
- You want quick extraction by type
- You're new to tree-sitter

Simpler than query_nodes - just specify node type(s).

Arguments:
- filepath: Absolute path to file (required)
- node_types: Node type string or list of types (required)
- limit: Max results (default 50, max 200) (optional)

Node type examples by language:
Python:
  - function_definition, class_definition
  - import_statement, for_statement, if_statement

JavaScript/TypeScript:
  - function_declaration, arrow_function
  - class_declaration, method_definition
  - import_statement, export_statement

Returns:
- All nodes matching the type(s)
- Line ranges
- Node text (truncated if long)

Typical workflow:
1. get_syntax_tree(file, 2) - See what node types exist
2. find_nodes_by_type(file, \"function_definition\") - Extract all functions
3. find_nodes_by_type(file, [\"class_definition\", \"function_definition\"]) - Multiple types
4. Read or extract_definition for full code

Example usage:
  find_nodes_by_type(\"/path/file.py\", \"function_definition\")
  find_nodes_by_type(\"/path/file.js\", [\"arrow_function\", \"function_declaration\"])

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- query_nodes: More powerful query-based extraction
- list_functions/list_classes: Semantic extraction with better formatting
- get_syntax_tree: Discover available node types"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "node_types"
         :type string
         :description "Node type or list of types")
       '(:name "limit"
         :type integer
         :optional t
         :description "Max results (default 50, max 200)"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath))
              (result-limit (min (or limit 50) 200))
              (types-list (if (listp node-types)
                              node-types
                            (list node-types))))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (all-matches '()))
               (dolist (node-type types-list)
                 (let ((matches (treesit-query-capture
                                root-node
                                (format "(%s) @match" node-type))))
                   (dolist (capture matches)
                     (push (cdr capture) all-matches))))
               (setq all-matches (seq-take (nreverse all-matches) result-limit))
               (if (null all-matches)
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :node_types types-list
                         :count 0
                         :nodes '()
                         :message (format "No nodes found of type(s): %s\n\nPossible reasons:\n1. Node type doesn't exist in file\n2. Incorrect node type name\n\nTry:\n- get_syntax_tree(\"%s\", 2) to see available types\n- Check spelling (use underscore_case)\n- Use list_functions or list_classes for common types"
                                          (mapconcat #'identity types-list ", ")
                                          filepath))
                 (let* ((nodes-data (mapcar
                                     (lambda (node)
                                       (let* ((line (line-number-at-pos (treesit-node-start node)))
                                              (type (treesit-node-type node))
                                              (text (treesit-node-text node t))
                                              (short-text (if (> (length text) 80)
                                                              (concat (substring text 0 77) "...")
                                                            text))
                                              (range (gptel-treesitter--node-to-range-string node)))
                                         (list :line line
                                               :type type
                                               :range range
                                               :text short-text)))
                                     all-matches))
                        (result (format "Nodes in %s (%s):\n\nSearching for: %s\nFound: %d node%s%s\n\n%s"
                                        (file-name-nondirectory filepath)
                                        (symbol-name language)
                                        (mapconcat #'identity types-list ", ")
                                        (length all-matches)
                                        (if (= (length all-matches) 1) "" "s")
                                        (if (>= (length all-matches) result-limit)
                                            (format " [showing %d, use limit to adjust]" result-limit)
                                          "")
                                        (mapconcat
                                         (lambda (node-data)
                                           (format "  %s [%s]:\n    %s"
                                                   (plist-get node-data :range)
                                                   (plist-get node-data :type)
                                                   (plist-get node-data :text)))
                                         nodes-data "\n\n"))))
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :node_types types-list
                         :count (length all-matches)
                         :nodes nodes-data
                         :message (gptel-treesitter--result-limit result)))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "find_nodes_in_range"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Find nodes within a specific line range.

Use this to:
- Analyze nodes in a specific section of code
- Focus on a particular function or class
- Understand structure of a code block
- Filter nodes by location

Arguments:
- filepath: Absolute path to file (required)
- start_line: Start line (inclusive) (required)
- end_line: End line (inclusive) (required)
- node_type: Optional node type filter (optional)

Returns:
- All nodes within the range
- Node types and text
- Line positions

Typical workflow:
1. list_functions(file) - Identify function at L100-L150
2. find_nodes_in_range(file, 100, 150) - See all nodes in function
3. find_nodes_in_range(file, 100, 150, \"call\") - Just function calls
4. extract_definition for full code

Example usage:
  find_nodes_in_range(\"/path/file.py\", 50, 100)
  find_nodes_in_range(\"/path/file.js\", 20, 40, \"call\")

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- get_node_at_position: Single position lookup
- query_nodes: Pattern-based extraction
- get_syntax_tree: Structural view of range"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "start_line"
         :type integer
         :description "Start line (inclusive)")
       '(:name "end_line"
         :type integer
         :description "End line (inclusive)")
       '(:name "node_type"
         :type string
         :optional t
         :description "Optional node type filter"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (start-pos (progn (goto-char (point-min))
                                      (forward-line (1- start-line))
                                      (point)))
                    (end-pos (progn (goto-char (point-min))
                                    (forward-line end-line)
                                    (point)))
                    (nodes-in-range '()))
               (let ((query-pattern (if node-type
                                        (format "(%s) @node" node-type)
                                      "(_ ) @node")))
                 (let ((all-nodes (treesit-query-capture root-node query-pattern)))
                   (dolist (capture all-nodes)
                     (let ((node (cdr capture)))
                       (when (and (>= (treesit-node-start node) start-pos)
                                  (<= (treesit-node-end node) end-pos))
                         (push node nodes-in-range))))))
               (setq nodes-in-range (nreverse nodes-in-range))
               (if (null nodes-in-range)
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :start_line start-line
                         :end_line end-line
                         :node_type node-type
                         :count 0
                         :nodes '()
                         :message (format "No nodes found in range L%d-L%d%s\n\nTry:\n- Expanding the range\n- Removing node_type filter\n- Use get_syntax_tree(\"%s\", 3, %d, %d) for structure"
                                          start-line end-line
                                          (if node-type (format " of type '%s'" node-type) "")
                                          filepath start-line end-line))
                 (let* ((nodes-data (mapcar
                                     (lambda (node)
                                       (list :type (treesit-node-type node)
                                             :line (line-number-at-pos (treesit-node-start node))
                                             :range (gptel-treesitter--node-to-range-string node)
                                             :text (treesit-node-text node t)))
                                     nodes-in-range))
                        (result (format "Nodes in range L%d-L%d of %s:\n\n%s\nFound: %d node%s\n\n%s"
                                        start-line end-line
                                        (file-name-nondirectory filepath)
                                        (if node-type (format "Filter: %s\n" node-type) "Filter: all types\n")
                                        (length nodes-in-range)
                                        (if (= (length nodes-in-range) 1) "" "s")
                                        (mapconcat
                                         (lambda (node)
                                           (gptel-treesitter--format-node node t))
                                         nodes-in-range "\n\n"))))
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :start_line start-line
                         :end_line end-line
                         :node_type node-type
                         :count (length nodes-in-range)
                         :nodes nodes-data
                         :message (gptel-treesitter--result-limit result)))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "get_node_context"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Get surrounding context for a node (parent chain, siblings).

Use this to:
- Understand where a node fits in the AST hierarchy
- See parent scopes (function -> class -> module)
- Navigate up the AST tree
- See sibling nodes at same level

Returns:
- Current node details
- Parent chain (all ancestors up to root)
- Optional sibling nodes

Arguments:
- filepath: Absolute path to file (required)
- position: Line number or character position (required)
- show_siblings: Show sibling nodes (optional, default false)

Position format: 42 or 'L42' for line numbers

Typical workflow:
1. get_node_at_position(file, line) - Identify node
2. get_node_context(file, line, true) - See full context with siblings
3. Navigate to parent with extracted info
4. extract_definition for complete code

Example context chain:
  identifier [L45:10-L45:15]
  -> call [L45:10-L45:25]
  -> expression_statement [L45:10-L45:26]
  -> block [L44:20-L50:5]
  -> function_definition [L44:1-L50:5]
  -> module [L1:1-L100:1]

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- get_node_info: Detailed single-node analysis
- get_scope_structure: Nesting depth analysis
- get_syntax_tree: Broader structural view"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "position"
         :type string
         :description "Line number (42 or 'L42') or character position")
       '(:name "show_siblings"
         :type boolean
         :optional t
         :description "Show sibling nodes (default false)"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (pos (cond
                          ((numberp position) position)
                          ((string-match "^L?\\([0-9]+\\)$" position)
                           (goto-char (point-min))
                           (forward-line (1- (string-to-number (match-string 1 position))))
                           (point))
                          (t (error "Invalid position"))))
                    (node (treesit-node-at pos))
                    (parent-chain '())
                    (current node))
               (while (and current (treesit-node-parent current))
                 (setq current (treesit-node-parent current))
                 (push current parent-chain))
               (if (not node)
                   (list :success nil
                         :error "node_not_found"
                         :message (format "No node found at position %s" position))
                 (let* ((node-formatted (gptel-treesitter--format-node node t))
                        (parent-chain-formatted (if parent-chain
                                                    (mapconcat (lambda (p)
                                                                 (format "  %s" (gptel-treesitter--format-node p nil)))
                                                               parent-chain "\n  -> ")
                                                  "  (root node)"))
                        (parent-chain-data (mapcar (lambda (p)
                                                     (list :type (treesit-node-type p)
                                                           :range (gptel-treesitter--node-to-range-string p)))
                                                   parent-chain))
                        (siblings-info (when show-siblings
                                         (let ((parent (treesit-node-parent node)))
                                           (when parent
                                             (let ((siblings (treesit-node-children parent)))
                                               (list :count (length siblings)
                                                     :siblings (mapcar (lambda (s)
                                                                         (list :type (treesit-node-type s)
                                                                               :range (gptel-treesitter--node-to-range-string s)
                                                                               :is_current (eq s node)))
                                                                       siblings)))))))
                        (siblings-display (if show-siblings
                                              (let ((parent (treesit-node-parent node)))
                                                (if parent
                                                    (let ((siblings (treesit-node-children parent)))
                                                      (format "=== SIBLINGS (%d) ===\n%s\n\n"
                                                              (length siblings)
                                                              (mapconcat (lambda (s)
                                                                           (format "  %s%s"
                                                                                   (if (eq s node) "* " "  ")
                                                                                   (gptel-treesitter--format-node s nil)))
                                                                         siblings "\n")))
                                                  ""))
                                            "Tip: Use show_siblings=true to see sibling nodes\n\n")))
                   (list :success t
                         :filepath filepath
                         :language (symbol-name language)
                         :position position
                         :current_node (list :type (treesit-node-type node)
                                             :formatted node-formatted)
                         :parent_chain parent-chain-data
                         :siblings siblings-info
                         :message (format "Context for node at %s:\n\nFile: %s (%s)\n\n=== CURRENT NODE ===\n%s\n\n=== PARENT CHAIN (%d levels) ===\n%s\n\n%s%s"
                                          position
                                          (file-name-nondirectory filepath)
                                          (symbol-name language)
                                          node-formatted
                                          (length parent-chain)
                                          parent-chain-formatted
                                          siblings-display
                                          "Next steps:\n- Use get_node_info for detailed node analysis\n- Use extract_definition to get full code\n- Use get_syntax_tree to see broader structure")))))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(gptel-make-scoped-tool
 "get_scope_structure"

 "SCOPE-AWARE: Requires filepath to match allowed patterns.

Understand nesting depth and scope hierarchy.

Use this to:
- Assess code complexity
- Find deeply nested code that needs refactoring
- Understand scope boundaries
- Analyze control flow structure

Returns:
- Maximum nesting depth
- Count of scope blocks (functions, loops, conditions)
- Hierarchical list of scopes with line ranges
- Complexity assessment

Arguments:
- filepath: Absolute path to file (required)
- start_line: Start line (optional, requires end_line)
- end_line: End line (optional, requires start_line)

Complexity guidelines:
- Depth 1-3: Low complexity, good structure
- Depth 4-5: Moderate, consider simplification
- Depth 6+: High complexity, refactoring recommended

Typical workflow:
1. list_functions(file) - Identify function locations
2. get_scope_structure(file, func_start, func_end) - Analyze complexity
3. If complex: get_syntax_tree for detailed structure
4. extract_definition to see full code
5. Refactor to reduce nesting

Example output:
  Max Nesting Depth: 4
  Scope Blocks: 8

  function_definition [L10-L50] depth=1
    if_statement [L15-L25] depth=2
      for_statement [L17-L23] depth=3
        if_statement [L19-L21] depth=4
    for_statement [L30-L45] depth=2

  Complexity: MODERATE - Acceptable but watch for nesting

Returns scope_violation error if filepath not in allowed patterns.
Use request_scope_expansion to request access.

Complements:
- list_functions: Identify functions to analyze
- get_node_context: Understand specific node context
- extract_definition: Get full code for refactoring"

 (list '(:name "filepath"
         :type string
         :description "Absolute path to file")
       '(:name "start_line"
         :type integer
         :optional t
         :description "Start line (requires end_line)")
       '(:name "end_line"
         :type integer
         :optional t
         :description "End line (requires start_line)"))

 "treesitter"

 (condition-case err
     (with-temp-buffer
       (insert-file-contents filepath)
       (let* ((language (gptel-treesitter--detect-language filepath)))
         (if (not language)
             (list :success nil
                   :error "unsupported_file"
                   :message (format "Error: Unsupported file type for %s" filepath))
           (if (not (treesit-language-available-p language))
               (list :success nil
                     :error "parser_not_installed"
                     :message (format "Error: Parser for %s not installed" language))
             (let* ((parser (treesit-parser-create language))
                    (root-node (treesit-parser-root-node parser))
                    (start-pos (if start-line
                                   (progn (goto-char (point-min))
                                          (forward-line (1- start-line))
                                          (point))
                                 (point-min)))
                    (end-pos (if end-line
                                 (progn (goto-char (point-min))
                                        (forward-line end-line)
                                        (point))
                               (point-max)))
                    (scope-nodes '())
                    (max-depth 0))
               (cl-labels ((analyze-depth (node depth)
                                          (when (and (>= (treesit-node-start node) start-pos)
                                                     (<= (treesit-node-start node) end-pos))
                                            (setq max-depth (max max-depth depth))
                                            (let ((type (treesit-node-type node)))
                                              (when (member type '("function_definition" "class_definition"
                                                                  "if_statement" "for_statement" "while_statement"
                                                                  "block" "lambda"))
                                                (push (list depth
                                                           type
                                                           (line-number-at-pos (treesit-node-start node))
                                                           (line-number-at-pos (treesit-node-end node)))
                                                      scope-nodes)))
                                            (dolist (child (treesit-node-children node))
                                              (analyze-depth child (1+ depth))))))
                 (analyze-depth root-node 0))
               (setq scope-nodes (nreverse scope-nodes))
               (let* ((scope-data (mapcar (lambda (scope-info)
                                            (list :depth (nth 0 scope-info)
                                                  :type (nth 1 scope-info)
                                                  :start_line (nth 2 scope-info)
                                                  :end_line (nth 3 scope-info)))
                                          scope-nodes))
                      (complexity (cond
                                   ((> max-depth 5) "HIGH - Consider refactoring to reduce nesting")
                                   ((> max-depth 3) "MODERATE - Acceptable but watch for further nesting")
                                   (t "LOW - Good, flat structure")))
                      (result (format "Scope Structure for %s:\n\n%sLines: %s\nMax Nesting Depth: %d\nScope Blocks: %d\n\n%s\n\nComplexity Assessment:\n%s\n\nNext steps:\n- High complexity? Consider refactoring\n- Use extract_definition to see full code\n- Use find_nodes_in_range for detailed analysis"
                                      (file-name-nondirectory filepath)
                                      (if (and start-line end-line)
                                          (format "Range: L%d-L%d\n" start-line end-line)
                                        "Range: Full file\n")
                                      (if (and start-line end-line)
                                          (format "L%d-L%d" start-line end-line)
                                        "Full file")
                                      max-depth
                                      (length scope-nodes)
                                      (if scope-nodes
                                          (mapconcat (lambda (scope-info)
                                                       (let ((depth (nth 0 scope-info))
                                                             (type (nth 1 scope-info))
                                                             (start (nth 2 scope-info))
                                                             (end (nth 3 scope-info)))
                                                         (format "%s%s [L%d-L%d] depth=%d"
                                                                 (make-string (* depth 2) ?\s)
                                                                 type start end depth)))
                                                     scope-nodes "\n")
                                        "  No significant scope blocks found")
                                      complexity)))
                 (list :success t
                       :filepath filepath
                       :language (symbol-name language)
                       :start_line start-line
                       :end_line end-line
                       :max_depth max-depth
                       :scope_blocks_count (length scope-nodes)
                       :scopes scope-data
                       :complexity complexity
                       :message result)))))))
   (error (list :success nil
                :error "tool_exception"
                :message (format "Error: %s\n\nTry: check_treesitter_parser(\"%s\")"
                                 (error-message-string err) filepath)))))

(provide 'treesitter-tools)
;;; treesitter-tools.el ends here
