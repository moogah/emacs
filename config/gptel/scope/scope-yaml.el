;;; scope-yaml.el --- Scope YAML Boundary Module -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Single entry point for YAML <-> plist conversion.
;; Consolidates normalize-keys, parse-string, parse-file,
;; and kebab-to-snake into one boundary module.

;;; Code:

(require 'cl-lib)
(require 'yaml)

(defun jf/gptel-scope-yaml--convert-vectors (obj)
  "Convert vectors to lists in OBJ, recursively.
YAML parser returns vectors for sequences; convert to lists for plist compatibility."
  (cond
   ((vectorp obj)
    (mapcar #'jf/gptel-scope-yaml--convert-vectors (append obj nil)))
   ((and (listp obj) (not (null obj)))
    (if (keywordp (car obj))
        ;; Plist: process values
        (let ((result nil))
          (while obj
            (push (car obj) result)
            (push (jf/gptel-scope-yaml--convert-vectors (cadr obj)) result)
            (setq obj (cddr obj)))
          (nreverse result))
      ;; Regular list
      (mapcar #'jf/gptel-scope-yaml--convert-vectors obj)))
   (t obj)))

(defun jf/gptel-scope-yaml--normalize-keys (plist)
  "Normalize PLIST keys from snake_case to kebab-case.
Also normalizes YAML boolean keywords (:true, :false, :null) to elisp booleans.
Recursively processes nested plists."
  (let ((result nil))
    (while plist
      (let* ((key (car plist))
             (value (cadr plist))
             (normalized-key (intern (replace-regexp-in-string
                                     "_" "-"
                                     (symbol-name key))))
             (normalized-value (cond
                                ;; Nested plist: recurse
                                ((and (listp value)
                                      (not (null value))
                                      (keywordp (car value)))
                                 (jf/gptel-scope-yaml--normalize-keys value))
                                ;; Boolean keywords: normalize
                                ((eq value :true) t)
                                ((or (eq value :false) (eq value :null)) nil)
                                ;; Other values: pass through
                                (t value))))
        (setq result (plist-put result normalized-key normalized-value))
        (setq plist (cddr plist))))
    result))

(defun jf/gptel-scope-yaml--kebab-to-snake (key)
  "Convert KEY from kebab-case to snake_case for YAML output.
E.g., :org-roam-patterns becomes org_roam_patterns."
  (replace-regexp-in-string "-" "_" (substring (symbol-name key) 1)))

(defun jf/gptel-scope-yaml--parse-string (yaml-string)
  "Parse YAML-STRING and return normalized plist.
Handles: yaml-parse-string, vector->list conversion, snake_case->kebab-case,
boolean normalization (:true->t, :false->nil, :null->nil)."
  (let* ((parsed (yaml-parse-string yaml-string
                                    :object-type 'plist
                                    :sequence-type 'list))
         (converted (jf/gptel-scope-yaml--convert-vectors parsed)))
    (jf/gptel-scope-yaml--normalize-keys converted)))

(defun jf/gptel-scope-yaml--parse-file (file-path)
  "Parse YAML file at FILE-PATH and return normalized plist."
  (with-temp-buffer
    (insert-file-contents file-path)
    (jf/gptel-scope-yaml--parse-string (buffer-string))))

(defconst jf/gptel-scope-yaml--schema-defaults
  '(:paths (:read ()
            :write ()
            :execute ()
            :modify ()
            :deny ())
    :cloud (:auth-detection "warn")
    :security (:enforce-parse-complete t
               :max-coverage-threshold 0.8))
  "Safe defaults for scope.yml schema.
Missing sections in YAML are merged with these defaults.")

(defun jf/gptel-scope-yaml--validate-cloud-config (cloud-plist)
  "Validate CLOUD-PLIST configuration."
  (when cloud-plist
    (let ((auth-detection (plist-get cloud-plist :auth-detection)))
      (unless (member auth-detection '("allow" "warn" "deny"))
        (error "Scope schema: cloud.auth-detection must be \"allow\", \"warn\", or \"deny\", got %S"
               auth-detection))))
  t)

(defun jf/gptel-scope-yaml--validate-security-config (security-plist)
  "Validate SECURITY-PLIST configuration."
  (when security-plist
    (let ((enforce-parse-complete (plist-get security-plist :enforce-parse-complete))
          (max-coverage-threshold (plist-get security-plist :max-coverage-threshold)))
      (when (plist-member security-plist :enforce-parse-complete)
        (setq enforce-parse-complete
              (cond ((eq enforce-parse-complete :true) t)
                    ((or (eq enforce-parse-complete :false)
                         (eq enforce-parse-complete :null)) nil)
                    (t enforce-parse-complete)))
        (unless (or (eq enforce-parse-complete t)
                   (eq enforce-parse-complete nil))
          (error "Scope schema: security.enforce-parse-complete must be boolean, got %S"
                 enforce-parse-complete)))
      (when (plist-member security-plist :max-coverage-threshold)
        (unless (and (numberp max-coverage-threshold)
                    (>= max-coverage-threshold 0.0)
                    (<= max-coverage-threshold 1.0))
          (error "Scope schema: security.max-coverage-threshold must be in [0.0, 1.0], got %S"
                 max-coverage-threshold)))))
  t)

(defun jf/gptel-scope-yaml--load-schema (scope-file)
  "Load scope schema from SCOPE-FILE and merge with defaults.
Returns merged plist with normalized kebab-case keys."
  (let* ((defaults jf/gptel-scope-yaml--schema-defaults)
         (raw (jf/gptel-scope-yaml--parse-file scope-file))
         (normalized (jf/gptel-scope-yaml--normalize-keys raw))
         (paths (plist-get normalized :paths))
         (cloud (plist-get normalized :cloud))
         (security (plist-get normalized :security))
         (bash-tools (plist-get normalized :bash-tools))
         (categories (when bash-tools (plist-get bash-tools :categories))))

    ;; Reject deprecated categories
    (when categories
      (error "bash_tools.categories section no longer supported. Remove categories section, keep only deny list"))

    ;; Merge with defaults
    (let ((merged-paths (if paths
                            (list :read (or (plist-get paths :read) ())
                                  :write (or (plist-get paths :write) ())
                                  :execute (or (plist-get paths :execute) ())
                                  :modify (or (plist-get paths :modify) ())
                                  :deny (or (plist-get paths :deny) ()))
                          (plist-get defaults :paths)))
          (merged-cloud (if cloud
                            (list :auth-detection (or (plist-get cloud :auth-detection)
                                                     (plist-get (plist-get defaults :cloud) :auth-detection))
                                  :allowed-providers (plist-get cloud :allowed-providers))
                          (plist-get defaults :cloud)))
          (merged-security (if security
                               (list :enforce-parse-complete
                                     (if (plist-member security :enforce-parse-complete)
                                         (plist-get security :enforce-parse-complete)
                                       (plist-get (plist-get defaults :security) :enforce-parse-complete))
                                     :max-coverage-threshold
                                     (if (plist-member security :max-coverage-threshold)
                                         (plist-get security :max-coverage-threshold)
                                       (plist-get (plist-get defaults :security) :max-coverage-threshold)))
                             (plist-get defaults :security))))
      (when cloud
        (jf/gptel-scope-yaml--validate-cloud-config merged-cloud))
      (when security
        (jf/gptel-scope-yaml--validate-security-config merged-security))
      (list :paths merged-paths
            :cloud merged-cloud
            :security merged-security
            :bash-tools bash-tools))))

(provide 'jf-gptel-scope-yaml)
;;; scope-yaml.el ends here
