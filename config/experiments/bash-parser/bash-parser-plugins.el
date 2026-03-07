;;; bash-parser-plugins.el --- Plugin infrastructure for bash parser -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct jf/bash-plugin-result
  "Structure for plugin extraction results.

Fields:
- domain: Keyword identifying the semantic domain (e.g., :file-operations, :variables)
- operations: List of operation plists containing domain-specific data
- claimed-token-ids: List of token IDs that this plugin has processed and claims
- metadata: Plist of domain-specific metadata for additional context"
  domain
  operations
  claimed-token-ids
  metadata)

(defvar jf/bash-semantic-plugins '()
  "List of registered semantic extraction plugins.

Each entry is a plist with the following keys:
- :name - Symbol identifying the plugin
- :priority - Integer priority (higher values processed first)
- :extractor - Function that extracts semantics from parse tree
- :predicates - List of predicate functions for applicability checks")

(defun jf/bash-register-plugin (&rest args)
  "Register a semantic extraction plugin.

Required keyword arguments:
- :name - Symbol identifying the plugin
- :priority - Integer priority for execution order (higher = earlier)
- :extractor - Function taking parse tree and returning jf/bash-plugin-result
- :predicates - List of predicate functions that determine applicability

Plugins are sorted by priority (higher first), with registration order
as a stable fallback for equal priorities."
  (let ((name (plist-get args :name))
        (priority (plist-get args :priority))
        (extractor (plist-get args :extractor))
        (predicates (plist-get args :predicates)))

    ;; Validate required arguments
    (unless name
      (error "Plugin registration requires :name"))
    (unless (numberp priority)
      (error "Plugin registration requires numeric :priority"))
    (unless (functionp extractor)
      (error "Plugin registration requires :extractor function"))
    (unless (listp predicates)
      (error "Plugin :predicates must be a list"))

    ;; Remove existing registration with same name
    (setq jf/bash-semantic-plugins
          (cl-remove-if (lambda (plugin)
                          (eq (plist-get plugin :name) name))
                        jf/bash-semantic-plugins))

    ;; Add plugin entry to the end to maintain registration order
    (setq jf/bash-semantic-plugins
          (append jf/bash-semantic-plugins
                  (list (list :name name
                              :priority priority
                              :extractor extractor
                              :predicates predicates))))

    ;; Sort by priority (higher first), maintaining registration order for ties
    (setq jf/bash-semantic-plugins
          (cl-stable-sort jf/bash-semantic-plugins
                          #'>
                          :key (lambda (plugin)
                                 (plist-get plugin :priority)))))

  ;; Return the plugin name for convenience
  (plist-get args :name))

(defun jf/bash-extract-semantics (parsed-command)
  "Extract semantic information from PARSED-COMMAND using registered plugins.

PARSED-COMMAND is a plist with:
  :tokens - List of token plists
  :parse-complete - Boolean indicating if parse was fully understood

Returns a plist with:
  :domains - Alist of (domain . operations) from all successful plugins
  :coverage - Coverage plist from `jf/bash-calculate-coverage'
  :parse-complete - Pass-through from input
  :plugin-results - List of raw plugin results for debugging

Orchestration behavior:
  - Evaluates plugin predicates to determine applicability
  - Empty predicate list means plugin always runs
  - Any predicate returning nil skips the plugin
  - Wraps each plugin call in error isolation (condition-case)
  - Logs errors but continues with remaining plugins
  - Partial results are better than no results"
  (let* ((tokens (plist-get parsed-command :tokens))
         (parse-complete (plist-get parsed-command :parse-complete))
         (plugin-results '())
         (all-claimed-token-ids '())
         (domains-alist '()))

    ;; Execute each registered plugin
    (dolist (plugin jf/bash-semantic-plugins)
      (let* ((plugin-name (plist-get plugin :name))
             (predicates (plist-get plugin :predicates))
             (extractor (plist-get plugin :extractor))
             (applicable t))

        ;; Check predicates for applicability
        (when predicates
          (dolist (predicate predicates)
            (unless (condition-case err
                        (funcall predicate parsed-command)
                      (error
                       (message "Error in predicate for plugin %s: %s"
                                plugin-name (error-message-string err))
                       nil))
              (setq applicable nil))))

        ;; Execute plugin if applicable
        (when applicable
          (condition-case err
              (let ((result (funcall extractor parsed-command)))
                (when result
                  ;; Collect plugin result
                  (push result plugin-results)

                  ;; Accumulate claimed token IDs
                  (let ((claimed-ids (jf/bash-plugin-result-claimed-token-ids result)))
                    (when claimed-ids
                      (setq all-claimed-token-ids
                            (append claimed-ids all-claimed-token-ids))))

                  ;; Group operations by domain
                  (let ((domain (jf/bash-plugin-result-domain result))
                        (operations (jf/bash-plugin-result-operations result)))
                    (when operations
                      (let ((existing (assq domain domains-alist)))
                        (if existing
                            ;; Append to existing domain
                            (setcdr existing (append (cdr existing) operations))
                          ;; Add new domain entry
                          (push (cons domain operations) domains-alist)))))))
            (error
             (message "Error executing plugin %s: %s"
                      plugin-name (error-message-string err)))))))

    ;; Calculate coverage
    (let ((coverage (jf/bash-calculate-coverage tokens all-claimed-token-ids)))
      ;; Return semantic analysis result
      (list :domains domains-alist
            :coverage coverage
            :parse-complete parse-complete
            :plugin-results (nreverse plugin-results)))))

(require 'bash-parser-coverage)

(require 'bash-parser-file-ops)

(defun jf/bash-plugin-filesystem--find-token-for-path (path tokens)
  "Find token in TOKENS list matching PATH string.

PATH is a file path string from an operation.
TOKENS is the list of token plists from parsed command.

Returns token ID if found, nil otherwise.

Matching strategy:
  - For literal paths: exact string match against token :value
  - For variable references: match variable token types
  - For patterns: match pattern token values"
  (when (and path tokens)
    (let ((found-token nil))
      (dolist (token tokens)
        (when (null found-token)
          (let ((token-value (plist-get token :value))
                (token-type (plist-get token :type))
                (token-id (plist-get token :id)))
            ;; Match exact path value
            (when (and token-value (string= path token-value))
              (setq found-token token-id)))))
      found-token)))

(defun jf/bash-plugin-filesystem--claim-redirection-tokens (parsed-command)
  "Claim tokens for redirections in PARSED-COMMAND.

PARSED-COMMAND is the parse result plist.

Returns list of token IDs for all redirection tokens."
  (let ((tokens (plist-get parsed-command :tokens))
        (claimed-ids '()))
    (when tokens
      (dolist (token tokens)
        (when (eq (plist-get token :type) :redirection)
          (push (plist-get token :id) claimed-ids))))
    (nreverse claimed-ids)))

(defun jf/bash-plugin-filesystem--claim-operation-tokens (operations parsed-command)
  "Claim tokens for file OPERATIONS in PARSED-COMMAND.

OPERATIONS is list of operation plists from extraction.
PARSED-COMMAND is the parse result plist.

Returns list of claimed token IDs.

Token claiming strategy:
  - For redirection operations: claim redirection tokens
  - For positional-arg operations: claim command-name and matching positional tokens
  - For exec-block operations: claim command-name tokens in exec blocks"
  (let ((tokens (plist-get parsed-command :tokens))
        (claimed-ids '())
        (command-name (plist-get parsed-command :command-name)))

    ;; Claim redirection tokens
    (setq claimed-ids (append claimed-ids
                              (jf/bash-plugin-filesystem--claim-redirection-tokens parsed-command)))

    ;; Claim tokens for each operation
    (dolist (op operations)
      (let* ((source (plist-get op :source))
             (file (plist-get op :file))
             (op-command (plist-get op :command)))

        (pcase source
          ;; Redirection operations already claimed above
          (:redirection nil)

          ;; Positional arg operations: claim command-name and file path tokens
          (:positional-arg
           ;; Claim command-name token if it matches operation command
           (when (and command-name op-command (string= command-name op-command))
             (dolist (token tokens)
               (when (and (eq (plist-get token :type) :command-name)
                          (string= (plist-get token :value) command-name))
                 (push (plist-get token :id) claimed-ids))))

           ;; Claim token for file path
           (when-let ((token-id (jf/bash-plugin-filesystem--find-token-for-path file tokens)))
             (push token-id claimed-ids)))

          ;; Exec block operations: claim command-name and file path tokens
          (:exec-block
           (when op-command
             (dolist (token tokens)
               (when (and (eq (plist-get token :type) :command-name)
                          (string= (plist-get token :value) op-command))
                 (push (plist-get token :id) claimed-ids))))

           (when-let ((token-id (jf/bash-plugin-filesystem--find-token-for-path file tokens)))
             (push token-id claimed-ids)))

          ;; Other sources: claim based on file path only
          (_
           (when-let ((token-id (jf/bash-plugin-filesystem--find-token-for-path file tokens)))
             (push token-id claimed-ids))))))

    ;; Remove duplicates and return
    (delete-dups (nreverse claimed-ids))))

(defun jf/bash-plugin-filesystem--has-file-tokens-p (parsed-command)
  "Return t if PARSED-COMMAND has tokens indicating file operations.

PARSED-COMMAND is the parse result plist with :tokens field.

File operation indicators:
  - Redirection tokens (>, >>, <, etc.)
  - Commands known to operate on files

Returns t if file operations likely present, nil otherwise.

Note: We do NOT use presence of positional args as a predicate since many
commands like echo, printf, etc. have positional args but don't operate on files."
  (let ((tokens (plist-get parsed-command :tokens))
        (command-name (plist-get parsed-command :command-name)))
    (or
     ;; Has redirection tokens
     (seq-some (lambda (token)
                 (eq (plist-get token :type) :redirection))
               tokens)

     ;; Command name suggests file operations
     (and command-name
          (member command-name
                  '("cat" "ls" "rm" "cp" "mv" "touch" "mkdir"
                    "grep" "find" "sed" "awk" "head" "tail"
                    "chmod" "chown" "tar" "zip" "unzip"))))))

(defun jf/bash-plugin-filesystem (parsed-command)
  "Extract filesystem operations from PARSED-COMMAND using existing logic.

PARSED-COMMAND is the parse result plist from jf/bash-parse.

Returns jf/bash-plugin-result struct with:
  - domain: :filesystem
  - operations: List of file operation plists
  - claimed-token-ids: List of token IDs for understood operations
  - metadata: Plist with extraction metadata

This plugin wraps jf/bash-extract-file-operations to validate the plugin
architecture with existing mature extraction logic."
  (condition-case err
      (let* ((operations (jf/bash-extract-file-operations parsed-command nil))
             (claimed-ids (jf/bash-plugin-filesystem--claim-operation-tokens
                          operations parsed-command)))

        (make-jf/bash-plugin-result
         :domain :filesystem
         :operations operations
         :claimed-token-ids claimed-ids
         :metadata (list :operation-count (length operations)
                        :extraction-source 'jf/bash-extract-file-operations)))
    (error
     ;; Return nil on error - orchestrator will log and continue
     (message "Filesystem plugin error: %s" (error-message-string err))
     nil)))

(defun jf/bash-register-filesystem-plugin ()
  "Register the filesystem extraction plugin.

Priority: 100 (universal predicates - high priority)
Predicates: Check for file-related tokens
Extractor: jf/bash-plugin-filesystem

Call this function to activate the filesystem plugin."
  (jf/bash-register-plugin
   :name 'filesystem
   :priority 100
   :extractor #'jf/bash-plugin-filesystem
   :predicates (list #'jf/bash-plugin-filesystem--has-file-tokens-p)))

(provide 'bash-parser-plugins)
;;; bash-parser-plugins.el ends here
