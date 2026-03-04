;;; collect-parser-output.el --- Collect parser output for all research commands -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, research, gap-analysis
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Phase 1 of systematic gap analysis: Run all 1,552 research commands through
;; the bash parser and capture structured output for analysis.
;;
;; This script:
;; 1. Loads the bash parser modules
;; 2. Reads commands from bash-commands-from-sessions.jsonl
;; 3. Runs each command through jf/bash-parse-command and jf/bash-extract-file-operations
;; 4. Captures parse results, file operations, and any errors
;; 5. Writes structured JSONL output for Phase 2 analysis
;;
;; Output format per command:
;; {
;;   "command": "original bash command",
;;   "session": "session-uuid",
;;   "timestamp": "original timestamp",
;;   "complexity_patterns": {...},
;;   "parse_output": {
;;     "result": {...},
;;     "error": null
;;   },
;;   "file_ops_output": {
;;     "result": [...],
;;     "error": null
;;   }
;; }

;;; Code:

(require 'json)
(require 'cl-lib)

;; Load parser modules
(defvar jf/emacs-dir (file-name-directory
                      (directory-file-name
                       (file-name-directory
                        (directory-file-name
                         (directory-file-name
                          (file-name-directory load-file-name))))))
  "Repository root directory.")

(add-to-list 'load-path (expand-file-name "config/experiments/bash-parser" jf/emacs-dir))
(require 'bash-parser-semantics)  ; Load first - defines command semantics
(require 'bash-parser-security)   ; Load second - defines wrapper-commands and dangerous-patterns
(require 'bash-parser-core)
(require 'bash-parser-file-ops)
(require 'bash-parser-variables)

(defvar jf/bash-parser-output-version "1.0"
  "Version of parser output format for tracking changes.")

(defun jf/bash-research--safe-parse (command-string)
  "Parse COMMAND-STRING, capturing errors gracefully.
Returns (:result PARSE-RESULT :error nil) or (:result nil :error ERROR-STRING)."
  (condition-case err
      (let ((result (jf/bash-parse command-string)))
        (list :result result :error nil))
    (error
     (list :result nil :error (error-message-string err)))))

(defun jf/bash-research--safe-extract-file-ops (parse-result)
  "Extract file operations from PARSE-RESULT, capturing errors gracefully.
Returns (:result FILE-OPS :error nil) or (:result nil :error ERROR-STRING)."
  (condition-case err
      (if (and parse-result (plist-get parse-result :success))
          (let ((file-ops (jf/bash-extract-file-operations parse-result)))
            (list :result file-ops :error nil))
        ;; Parse failed, can't extract file ops
        (list :result nil :error "Parse failed, cannot extract file operations"))
    (error
     (list :result nil :error (error-message-string err)))))

(defun jf/bash-research--process-command (command-entry)
  "Process a single COMMAND-ENTRY from research JSONL.
Returns enriched entry with parse_output and file_ops_output."
  (let* ((command (gethash "command" command-entry))
         (parse-output (jf/bash-research--safe-parse command))
         (parse-result (plist-get parse-output :result))
         (file-ops-output (jf/bash-research--safe-extract-file-ops parse-result)))

    ;; Build output entry
    (let ((output (make-hash-table :test 'equal)))
      ;; Copy original fields
      (puthash "command" command output)
      (puthash "session" (gethash "session" command-entry) output)
      (puthash "timestamp" (gethash "timestamp" command-entry) output)
      (puthash "complexity_patterns" (gethash "complexity_patterns" command-entry) output)

      ;; Add parser output (convert plists to hash tables for JSON)
      (puthash "parse_output"
               (let ((parse-hash (make-hash-table :test 'equal))
                     (result (plist-get parse-output :result))
                     (error (plist-get parse-output :error)))
                 (puthash "result" (if result
                                       (jf/bash-research--plist-to-hash result)
                                     :null)
                          parse-hash)
                 (puthash "error" (if error error :null) parse-hash)
                 parse-hash)
               output)

      ;; Add file ops output
      (puthash "file_ops_output"
               (let ((ops-hash (make-hash-table :test 'equal))
                     (result (plist-get file-ops-output :result))
                     (error (plist-get file-ops-output :error)))
                 (puthash "result" (if result
                                       (jf/bash-research--file-ops-to-hash result)
                                     :null)
                          ops-hash)
                 (puthash "error" (if error error :null) ops-hash)
                 ops-hash)
               output)

      output)))

(defun jf/bash-research--plist-to-hash (plist)
  "Convert PLIST to hash table for JSON serialization, handling nested structures."
  (cond
   ;; Nil case
   ((null plist) :null)  ; JSON null

   ;; Keyword symbol - convert to string without colon
   ((keywordp plist)
    (substring (symbol-name plist) 1))

   ;; Regular symbol - convert to string
   ((symbolp plist)
    (symbol-name plist))

   ;; Tree-sitter node - convert to string representation
   ((and (fboundp 'treesit-node-p) (treesit-node-p plist))
    (format "<treesit-node: %s>" (treesit-node-type plist)))

   ;; List - check if it's a plist or regular list
   ((listp plist)
    (if (and plist (keywordp (car plist)))
        ;; Is a plist, convert to hash table (excluding :ast field)
        (let ((result (make-hash-table :test 'equal))
              (remaining plist))
          (while remaining
            (let ((key (pop remaining))
                  (value (if remaining (pop remaining) nil)))
              ;; Skip :ast field (contains unparseable tree-sitter nodes)
              (unless (eq key :ast)
                ;; Convert key to string (remove leading colon)
                (let ((key-str (substring (symbol-name key) 1)))
                  ;; Recursively convert value
                  (puthash key-str (jf/bash-research--plist-to-hash value) result)))))
          result)
      ;; Is a list of items, convert each
      (mapcar #'jf/bash-research--plist-to-hash plist)))

   ;; Anything else, return as-is
   (t plist)))

(defun jf/bash-research--file-ops-to-hash (file-ops)
  "Convert FILE-OPS list (of plists) to list of hash tables for JSON."
  (when file-ops
    (mapcar #'jf/bash-research--plist-to-hash file-ops)))

(defun jf/bash-research--read-jsonl-file (file-path)
  "Read JSONL file at FILE-PATH, return list of hash tables."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((entries nil))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
          (unless (string-empty-p (string-trim line))
            (push (json-parse-string line) entries)))
        (forward-line 1))
      (nreverse entries))))

(defun jf/bash-research--write-jsonl-file (file-path entries)
  "Write ENTRIES to JSONL file at FILE-PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file-path
      ;; Write metadata header
      (insert (json-serialize
               (let ((header (make-hash-table :test 'equal)))
                 (puthash "metadata" t header)
                 (puthash "parser_version" jf/bash-parser-output-version header)
                 (puthash "timestamp" (format-time-string "%Y-%m-%dT%H:%M:%S%z") header)
                 (puthash "total_commands" (length entries) header)
                 header))
              "\n")

      ;; Write each entry as JSON line
      (dolist (entry entries)
        (insert (json-serialize entry) "\n")))))

(defun jf/bash-research-collect-parser-output ()
  "Main entry point: process all research commands and write output.

Reads: config/experiments/bash-parser/research/bash-commands-from-sessions.jsonl
Writes: config/experiments/bash-parser/research/parser-output-complete.jsonl"
  (interactive)
  (let* ((base-dir (expand-file-name "config/experiments/bash-parser/research" jf/emacs-dir))
         (input-file (expand-file-name "bash-commands-from-sessions.jsonl" base-dir))
         (output-file (expand-file-name "parser-output-complete.jsonl" base-dir)))

    (message "Starting parser output collection...")
    (message "Reading commands from: %s" input-file)

    (let ((input-entries (jf/bash-research--read-jsonl-file input-file))
          (output-entries nil)
          (processed-count 0)
          (total-count 0))

      (setq total-count (length input-entries))
      (message "Processing %d commands..." total-count)

      ;; Process each command
      (dolist (entry input-entries)
        (setq processed-count (1+ processed-count))
        (when (zerop (mod processed-count 100))
          (message "  Processed %d/%d (%.1f%%)"
                   processed-count total-count
                   (* 100.0 (/ (float processed-count) total-count))))

        (push (jf/bash-research--process-command entry) output-entries))

      (setq output-entries (nreverse output-entries))

      (message "Writing results to: %s" output-file)
      (jf/bash-research--write-jsonl-file output-file output-entries)

      (message "Collection complete!")
      (message "  Total commands: %d" total-count)
      (message "  Output file: %s" output-file)
      (message "  Parser version: %s" jf/bash-parser-output-version))))

;; Batch mode support
(defun jf/bash-research-collect-parser-output-batch ()
  "Run collection in batch mode and exit."
  (jf/bash-research-collect-parser-output)
  (kill-emacs 0))

(provide 'collect-parser-output)
;;; collect-parser-output.el ends here
