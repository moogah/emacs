;;; bash-parser-cloud-auth.el --- Cloud authentication extraction plugin -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'bash-parser-plugins)

(defvar jf/bash-cloud-auth-patterns
  '((aws-cli
     :commands ("aws")
     :auth-flags (("--profile" . :profile)
                  ("--region" . :region))
     :subcommands t)

    (aws-vault
     :commands ("aws-vault")
     :subcommands ("exec" "login" "remove" "rotate")
     :account-position 1
     :region-flag "--region")

    (gcloud
     :commands ("gcloud")
     :auth-flags (("--project" . :project)
                  ("--zone" . :zone)
                  ("--region" . :region)
                  ("--account" . :account))
     :subcommands t)

    (azure
     :commands ("az")
     :auth-flags (("--subscription" . :subscription)
                  ("--location" . :location))
     :subcommands t))
  "Authentication patterns for cloud CLI commands.

Each entry is a plist with:
- :commands - List of command names
- :auth-flags - Alist of (flag-name . context-key)
- :subcommands - t for any subcommand, or list of specific subcommands
- :account-position - Position index for account/profile argument (for aws-vault)
- :region-flag - Flag name for region specification")

(defun jf/bash-plugin-cloud-auth--find-pattern (command-name)
  "Find cloud auth pattern matching COMMAND-NAME.

COMMAND-NAME is the command string.

Returns pattern plist if found, nil otherwise."
  (cl-loop for (provider . pattern) in jf/bash-cloud-auth-patterns
           when (member command-name (plist-get pattern :commands))
           return (cons provider pattern)))

(defun jf/bash-plugin-cloud-auth--extract-flag-value (flag-name tokens)
  "Extract value for FLAG-NAME from TOKENS list.

FLAG-NAME is the flag string (e.g., \"--profile\").
TOKENS is the list of token plists.

Returns (value . token-ids) where:
- value is the flag's argument string
- token-ids is list of token IDs for flag and value

Returns nil if flag not found."
  (let ((flag-token nil)
        (value-token nil))
    ;; Find flag token
    (cl-loop for token in tokens
             when (and (eq (plist-get token :type) :flag)
                       (string= (plist-get token :value) flag-name))
             do (setq flag-token token)
             (cl-return))

    ;; Find value token (next positional-arg after flag by ID)
    (when flag-token
      (let ((flag-id (plist-get flag-token :id)))
        (cl-loop for token in tokens
                 when (and (eq (plist-get token :type) :positional-arg)
                           (> (plist-get token :id) flag-id))
                 do (setq value-token token)
                 (cl-return))))

    ;; Return value and token IDs
    (when (and flag-token value-token)
      (cons (plist-get value-token :value)
            (list (plist-get flag-token :id)
                  (plist-get value-token :id))))))

(defun jf/bash-plugin-cloud-auth--extract-positional-account (position tokens)
  "Extract account name from POSITION in TOKENS list.

POSITION is the 0-indexed position of the account argument.
TOKENS is the list of token plists.

Returns (account . token-ids) or nil."
  (let ((positionals (cl-remove-if-not
                      (lambda (token)
                        (eq (plist-get token :type) :positional-arg))
                      tokens)))
    (when (< position (length positionals))
      (let ((token (nth position positionals)))
        (cons (plist-get token :value)
              (list (plist-get token :id)))))))

(defun jf/bash-plugin-cloud-auth--subcommand-matches-p (pattern tokens)
  "Check if subcommand in TOKENS matches PATTERN requirements.

PATTERN is the provider pattern plist.
TOKENS is the list of token plists.

Returns t if pattern has no subcommand restrictions,
or if the first positional matches a listed subcommand."
  (let ((subcommands (plist-get pattern :subcommands)))
    (cond
     ;; Pattern accepts any subcommand
     ((eq subcommands t) t)

     ;; Pattern lists specific subcommands
     ((listp subcommands)
      (let ((first-positional
             (cl-find-if (lambda (token)
                           (eq (plist-get token :type) :positional-arg))
                         tokens)))
        (if first-positional
            (member (plist-get first-positional :value) subcommands)
          ;; No subcommand present - only valid if subcommands is empty list
          (null subcommands))))

     ;; No subcommand specification
     (t t))))

(defun jf/bash-plugin-cloud-auth (parsed-command)
  "Extract cloud authentication context from PARSED-COMMAND.

PARSED-COMMAND is the parse result plist from jf/bash-parse.

Returns jf/bash-plugin-result struct with:
  - domain: :authentication
  - operations: List of auth operation plists
  - claimed-token-ids: List of token IDs for auth components
  - metadata: Plist with provider and extraction info

Returns nil if command is not a cloud CLI command."
  (let* ((command-name (plist-get parsed-command :command-name))
         (tokens (plist-get parsed-command :tokens))
         (pattern-entry (jf/bash-plugin-cloud-auth--find-pattern command-name)))

    (when (and pattern-entry
               (jf/bash-plugin-cloud-auth--subcommand-matches-p
                (cdr pattern-entry) tokens))
      (let* ((provider (car pattern-entry))
             (pattern (cdr pattern-entry))
             (auth-context '())
             (claimed-token-ids '())
             (operations '()))

        ;; Claim command-name token
        (dolist (token tokens)
          (when (and (eq (plist-get token :type) :command-name)
                     (string= (plist-get token :value) command-name))
            (push (plist-get token :id) claimed-token-ids)))

        ;; Extract flag-based authentication
        (let ((auth-flags (plist-get pattern :auth-flags)))
          (dolist (flag-entry auth-flags)
            (let* ((flag-name (car flag-entry))
                   (context-key (cdr flag-entry))
                   (extraction (jf/bash-plugin-cloud-auth--extract-flag-value
                               flag-name tokens)))
              (when extraction
                (let ((value (car extraction))
                      (token-ids (cdr extraction)))
                  ;; Add to auth context
                  (setq auth-context (plist-put auth-context context-key value))
                  ;; Claim tokens
                  (setq claimed-token-ids
                        (append token-ids claimed-token-ids)))))))

        ;; Extract positional-based authentication (e.g., aws-vault)
        (let ((account-position (plist-get pattern :account-position)))
          (when account-position
            (let ((extraction (jf/bash-plugin-cloud-auth--extract-positional-account
                              account-position tokens)))
              (when extraction
                (let ((account (car extraction))
                      (token-ids (cdr extraction)))
                  (setq auth-context (plist-put auth-context :account account))
                  (setq claimed-token-ids
                        (append token-ids claimed-token-ids)))))))

        ;; Create authentication operation if we found auth context
        (when auth-context
          (push (list :operation :authenticate
                      :provider provider
                      :context auth-context
                      :command command-name)
                operations))

        ;; Return plugin result if we found anything
        (when operations
          (make-jf/bash-plugin-result
           :domain :authentication
           :operations (nreverse operations)
           :claimed-token-ids (delete-dups (nreverse claimed-token-ids))
           :metadata (list :provider provider
                          :command command-name)))))))

(defun jf/bash-plugin-cloud-auth--is-cloud-cli-p (parsed-command)
  "Return t if PARSED-COMMAND is a cloud CLI command.

PARSED-COMMAND is the parse result plist.

Returns t if command name matches any cloud CLI pattern."
  (let ((command-name (plist-get parsed-command :command-name)))
    (and command-name
         (jf/bash-plugin-cloud-auth--find-pattern command-name))))

(defun jf/bash-register-cloud-auth-plugin ()
  "Register the cloud authentication extraction plugin.

Priority: 90 (domain-specific - runs after universal plugins)
Predicates: Check for cloud CLI commands
Extractor: jf/bash-plugin-cloud-auth

Call this function to activate the cloud auth plugin."
  (jf/bash-register-plugin
   :name 'cloud-auth
   :priority 90
   :extractor #'jf/bash-plugin-cloud-auth
   :predicates (list #'jf/bash-plugin-cloud-auth--is-cloud-cli-p)))

(provide 'bash-parser-cloud-auth)
;;; bash-parser-cloud-auth.el ends here
