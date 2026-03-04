;;; collect-parser-summary.el --- Collect parser summaries as text -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, research, gap-analysis
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Phase 1 of systematic gap analysis: Run all 1,552 research commands through
;; the bash parser and capture text summaries for analysis.
;;
;; Output format: Tab-separated values (TSV) with one line per command:
;; command | parse_success | parse_error | file_ops_count | has_pipeline | has_chain

;;; Code:

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
(require 'bash-parser-semantics)
(require 'bash-parser-security)
(require 'bash-parser-core)
(require 'bash-parser-file-ops)
(require 'bash-parser-variables)

(defun jf/bash-research--safe-parse (command-string)
  "Parse COMMAND-STRING, capturing errors gracefully.
Returns plist with :success, :result, and :error fields."
  (condition-case err
      (let ((result (jf/bash-parse command-string)))
        (list :success (plist-get result :success)
              :result result
              :error (plist-get result :error)))
    (error
     (list :success nil
           :result nil
           :error (error-message-string err)))))

(defun jf/bash-research--safe-extract-file-ops (parse-result)
  "Extract file operations from PARSE-RESULT, capturing errors gracefully.
Returns plist with :success, :count, and :operations fields."
  (condition-case err
      (if (and parse-result (plist-get parse-result :success))
          (let ((file-ops (jf/bash-extract-file-operations parse-result)))
            (list :success t
                  :count (length file-ops)
                  :operations file-ops))
        (list :success nil
              :count 0
              :operations nil))
    (error
     (list :success nil
           :count 0
           :error (error-message-string err)))))

(defun jf/bash-research--escape-field (text)
  "Escape TEXT for TSV output (replace tabs and newlines)."
  (when text
    (replace-regexp-in-string
     "\n" "\\\\n"
     (replace-regexp-in-string "\t" " " text))))

(defun jf/bash-research--summarize-command (command-string)
  "Generate summary line for COMMAND-STRING.
Returns TSV line: command | success | error | file_ops | type | dangerous"
  (let* ((parse-output (jf/bash-research--safe-parse command-string))
         (parse-success (plist-get parse-output :success))
         (parse-result (plist-get parse-output :result))
         (parse-error (plist-get parse-output :error))
         (file-ops-output (jf/bash-research--safe-extract-file-ops parse-result))
         (file-ops-count (plist-get file-ops-output :count))
         (command-type (when parse-result (plist-get parse-result :type)))
         (is-dangerous (when parse-result (plist-get parse-result :dangerous-p))))

    (format "%s\t%s\t%s\t%d\t%s\t%s"
            (jf/bash-research--escape-field command-string)
            (if parse-success "SUCCESS" "FAILURE")
            (jf/bash-research--escape-field (or parse-error ""))
            file-ops-count
            (or (when command-type (symbol-name command-type)) "")
            (if is-dangerous "dangerous" ""))))

(defun jf/bash-research--read-jsonl-commands (file-path)
  "Read commands from JSONL file at FILE-PATH, return list of command strings."
  (require 'json)
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((commands nil))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
          (unless (string-empty-p (string-trim line))
            (let* ((entry (json-parse-string line))
                   (command (gethash "command" entry)))
              (when command
                (push command commands)))))
        (forward-line 1))
      (nreverse commands))))

(defun jf/bash-research-collect-summary ()
  "Main entry point: process all research commands and write TSV summary.

Reads: config/experiments/bash-parser/research/bash-commands-from-sessions.jsonl
Writes: config/experiments/bash-parser/research/parser-summary.tsv"
  (interactive)
  (let* ((base-dir (expand-file-name "config/experiments/bash-parser/research" jf/emacs-dir))
         (input-file (expand-file-name "bash-commands-from-sessions.jsonl" base-dir))
         (output-file (expand-file-name "parser-summary.tsv" base-dir)))

    (message "Starting parser summary collection...")
    (message "Reading commands from: %s" input-file)

    (let ((commands (jf/bash-research--read-jsonl-commands input-file))
          (processed-count 0)
          (total-count 0)
          (coding-system-for-write 'utf-8-unix))

      (setq total-count (length commands))
      (message "Processing %d commands..." total-count)

      (with-temp-file output-file
        ;; Write header
        (insert "command\tparse_status\tparse_error\tfile_ops_count\tcommand_type\tdangerous\n")

        ;; Process each command
        (dolist (command commands)
          (setq processed-count (1+ processed-count))
          (when (zerop (mod processed-count 100))
            (message "  Processed %d/%d (%.1f%%)"
                     processed-count total-count
                     (* 100.0 (/ (float processed-count) total-count))))

          (insert (jf/bash-research--summarize-command command) "\n")))

      (message "Collection complete!")
      (message "  Total commands: %d" total-count)
      (message "  Output file: %s" output-file))))

;; Batch mode support
(defun jf/bash-research-collect-summary-batch ()
  "Run collection in batch mode and exit."
  (jf/bash-research-collect-summary)
  (kill-emacs 0))

(provide 'collect-parser-summary)
;;; collect-parser-summary.el ends here
