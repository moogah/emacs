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

(provide 'bash-parser-plugins)
;;; bash-parser-plugins.el ends here
