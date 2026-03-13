;;; bash-parser-orchestrator.el --- Semantic extraction orchestrator for bash parser -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun jf/bash--claim-tokens-for-operations (operations parsed-command)
  "Claim tokens for file OPERATIONS in PARSED-COMMAND.

OPERATIONS is a list of operation plists from the recursive engine.
PARSED-COMMAND is the parse result plist.

Returns list of claimed token IDs.

Token claiming strategy:
  - For redirection operations: claim redirection tokens
  - For positional-arg operations: claim command-name and matching positional tokens
  - For exec-block operations: claim command-name and file path tokens
  - For other sources: claim based on file path only"
  (let ((tokens (plist-get parsed-command :tokens))
        (claimed-ids '())
        (command-name (plist-get parsed-command :command-name)))

    ;; Claim all redirection tokens
    (when tokens
      (dolist (token tokens)
        (when (eq (plist-get token :type) :redirection)
          (push (plist-get token :id) claimed-ids))))

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
           (when file
             (dolist (token tokens)
               (when (and (null (cl-find (plist-get token :id) claimed-ids))
                          (plist-get token :value)
                          (string= file (plist-get token :value)))
                 (push (plist-get token :id) claimed-ids)
                 (cl-return)))))

          ;; Exec block operations: claim command-name and file path tokens
          (:exec-block
           (when op-command
             (dolist (token tokens)
               (when (and (eq (plist-get token :type) :command-name)
                          (string= (plist-get token :value) op-command))
                 (push (plist-get token :id) claimed-ids))))

           (when file
             (dolist (token tokens)
               (when (and (plist-get token :value)
                          (string= file (plist-get token :value)))
                 (push (plist-get token :id) claimed-ids)
                 (cl-return)))))

          ;; Other sources: claim based on file path only
          (_
           (when file
             (dolist (token tokens)
               (when (and (plist-get token :value)
                          (string= file (plist-get token :value)))
                 (push (plist-get token :id) claimed-ids)
                 (cl-return))))))))

    ;; Remove duplicates and return
    (delete-dups (nreverse claimed-ids))))

(defun jf/bash-extract-semantics (parsed-command &optional var-context)
  "Extract semantic information from PARSED-COMMAND.

Implements a two-layer extraction architecture:
  Layer 0: Grammar-level extraction (unconditional)
           Calls the recursive engine to extract file operations from
           redirections and compound structures.
  Layer 1: Command handlers
           Per-command semantic dispatch for all domains.

PARSED-COMMAND is a plist with:
  :tokens - List of token plists
  :parse-complete - Boolean indicating if parse was fully understood

VAR-CONTEXT is an optional alist mapping variable names to values,
threaded to the recursive engine for variable resolution.

Returns a plist with:
  :domains - Alist of (domain . operations) from all extraction layers
  :coverage - Coverage plist from `jf/bash-calculate-coverage'
  :parse-complete - Pass-through from input"
  (let* ((tokens (plist-get parsed-command :tokens))
         (parse-complete (plist-get parsed-command :parse-complete))
         (all-claimed-token-ids '())
         (domains-alist '()))

    ;; Layer 0: Grammar-level extraction (unconditional)
    ;; Call the recursive engine directly — no predicate gate
    (condition-case err
        (let* ((fs-operations (jf/bash--extract-file-operations-impl
                               parsed-command var-context))
               (fs-claimed-ids (jf/bash--claim-tokens-for-operations
                                fs-operations parsed-command)))
          ;; Add filesystem domain if operations were found
          (when fs-operations
            (push (cons :filesystem fs-operations) domains-alist))
          ;; Accumulate claimed tokens
          (when fs-claimed-ids
            (setq all-claimed-token-ids
                  (append fs-claimed-ids all-claimed-token-ids))))
      (error
       (message "Error in grammar-level extraction: %s"
                (error-message-string err))))

    ;; Layer 1: Command handlers
    ;; Per-command semantic dispatch for all domains.
    ;; Known limitation: for compound commands (pipelines, chains), the top-level
    ;; parsed-command has no :command-name, so jf/bash-extract-command-semantics
    ;; returns empty results.  Non-filesystem domain ops from subcommands within
    ;; compounds are not captured here.  The recursive engine (Layer 0) dispatches
    ;; to handlers per-simple-command internally but currently returns only
    ;; filesystem ops.  Follow-up: update the recursive engine to surface all
    ;; domain results from per-subcommand handler dispatch.
    (let* ((cmd-result (jf/bash-extract-command-semantics parsed-command))
           (cmd-domains (plist-get cmd-result :domains))
           (cmd-claimed-ids (plist-get cmd-result :claimed-token-ids)))
      ;; Merge command handler domains not already provided by Layer 0
      (dolist (domain-entry cmd-domains)
        (let ((domain (car domain-entry))
              (operations (cdr domain-entry)))
          (when operations
            (unless (assq domain domains-alist)
              (push (cons domain operations) domains-alist)))))
      ;; Include command handler claimed token IDs in coverage
      (when cmd-claimed-ids
        (setq all-claimed-token-ids
              (append cmd-claimed-ids all-claimed-token-ids))))

    ;; Calculate coverage
    (let ((coverage (jf/bash-calculate-coverage tokens all-claimed-token-ids)))
      ;; Return semantic analysis result
      (list :domains domains-alist
            :coverage coverage
            :parse-complete parse-complete))))

(require 'bash-parser-coverage)

(require 'bash-parser-file-ops)

;; Load command handler index (auto-discovers and loads all command handlers)
(let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
  (when (file-exists-p index-path)
    (load index-path nil t)))

(provide 'bash-parser-orchestrator)
;;; bash-parser-orchestrator.el ends here
