;;; analyze-with-coverage.el --- Analyze corpus with parse coverage metrics -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, research, coverage

;;; Commentary:

;; Phase 2 analysis: Compute parse coverage for all research commands
;; to identify commands where parser misses significant portions of input.
;;
;; Output format: TSV with columns:
;; command | parse_status | coverage_pct | missing_count | file_ops_count |
;; command_type | dangerous | missing_tokens

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
(add-to-list 'load-path (expand-file-name "config/experiments/bash-parser/research" jf/emacs-dir))

(require 'bash-parser)
(require 'compute-parse-coverage)

(defun jf/bash-coverage-research--escape-field (text)
  "Escape TEXT for TSV output (replace tabs and newlines)."
  (when text
    (replace-regexp-in-string
     "\n" "\\\\n"
     (replace-regexp-in-string "\t" " " text))))

(defun jf/bash-coverage-research--format-token-list (tokens)
  "Format list of TOKENS for TSV output (comma-separated, max 10)."
  (if tokens
      (let ((display-tokens (seq-take tokens 10)))
        (mapconcat #'identity display-tokens ", "))
    ""))

(defun jf/bash-coverage-research--analyze-command (command-string)
  "Analyze COMMAND-STRING with coverage metrics.
Returns TSV line with all analysis data."
  (condition-case err
      (let* ((parse-result (jf/bash-parse command-string))
             (parse-success (plist-get parse-result :success))
             (parse-error (plist-get parse-result :error))
             (command-type (plist-get parse-result :type))
             (is-dangerous (plist-get parse-result :dangerous-p))

             ;; Compute coverage
             (coverage-result (jf/bash-parse-coverage--compute command-string))
             (coverage-pct (plist-get coverage-result :coverage-percentage))
             (missing-count (plist-get coverage-result :missing-count))
             (missing-tokens (plist-get coverage-result :missing-tokens))

             ;; File operations (if parse succeeded)
             (file-ops (if parse-success
                          (condition-case _
                              (jf/bash-extract-file-operations parse-result)
                            (error nil))
                        nil))
             (file-ops-count (if file-ops (length file-ops) 0)))

        (format "%s\t%s\t%.1f\t%d\t%d\t%s\t%s\t%s"
                (jf/bash-coverage-research--escape-field command-string)
                (if parse-success "SUCCESS" "FAILURE")
                coverage-pct
                missing-count
                file-ops-count
                (if command-type (symbol-name command-type) "")
                (if is-dangerous "dangerous" "")
                (jf/bash-coverage-research--format-token-list missing-tokens)))

    (error
     (format "%s\tERROR\t0.0\t0\t0\t\t\t%s"
             (jf/bash-coverage-research--escape-field command-string)
             (jf/bash-coverage-research--escape-field (error-message-string err))))))

(defun jf/bash-coverage-research--read-jsonl-commands (file-path)
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

(defun jf/bash-coverage-research-analyze ()
  "Main entry point: analyze all research commands with coverage metrics.

Reads: config/experiments/bash-parser/research/bash-commands-from-sessions.jsonl
Writes: config/experiments/bash-parser/research/coverage-analysis.tsv"
  (interactive)
  (let* ((base-dir (expand-file-name "config/experiments/bash-parser/research" jf/emacs-dir))
         (input-file (expand-file-name "bash-commands-from-sessions.jsonl" base-dir))
         (output-file (expand-file-name "coverage-analysis.tsv" base-dir)))

    (message "Starting coverage analysis...")
    (message "Reading commands from: %s" input-file)

    (let ((commands (jf/bash-coverage-research--read-jsonl-commands input-file))
          (processed-count 0)
          (total-count 0)
          (perfect-coverage 0)
          (high-coverage 0)    ; >= 90%
          (medium-coverage 0)  ; >= 70%
          (low-coverage 0)     ; < 70%
          (coverage-values '())
          (coding-system-for-write 'utf-8-unix))

      (setq total-count (length commands))
      (message "Processing %d commands..." total-count)

      (with-temp-file output-file
        ;; Write header
        (insert "command\tparse_status\tcoverage_pct\tmissing_count\tfile_ops_count\tcommand_type\tdangerous\tmissing_tokens\n")

        ;; Process each command
        (dolist (command commands)
          (setq processed-count (1+ processed-count))
          (when (zerop (mod processed-count 100))
            (message "  Processed %d/%d (%.1f%%)"
                     processed-count total-count
                     (* 100.0 (/ (float processed-count) total-count))))

          (let ((line (jf/bash-coverage-research--analyze-command command)))
            (insert line "\n")

            ;; Extract coverage for statistics
            (when (string-match "\t\\([0-9.]+\\)\t" line)
              (let ((coverage (string-to-number (match-string 1 line))))
                (push coverage coverage-values)
                (cond
                 ((= coverage 100.0) (setq perfect-coverage (1+ perfect-coverage)))
                 ((>= coverage 90.0) (setq high-coverage (1+ high-coverage)))
                 ((>= coverage 70.0) (setq medium-coverage (1+ medium-coverage)))
                 (t (setq low-coverage (1+ low-coverage)))))))))

      ;; Compute statistics
      (setq coverage-values (nreverse coverage-values))
      (let* ((mean (/ (apply #'+ coverage-values) (float total-count)))
             (sorted-vals (sort coverage-values #'<))
             (median (nth (/ total-count 2) sorted-vals)))

        (message "\n=== Coverage Analysis Complete ===")
        (message "Total commands: %d" total-count)
        (message "\nCoverage Distribution:")
        (message "  Perfect (100%%):     %4d (%.1f%%)"
                 perfect-coverage
                 (* 100.0 (/ (float perfect-coverage) total-count)))
        (message "  High (90-99%%):      %4d (%.1f%%)"
                 high-coverage
                 (* 100.0 (/ (float high-coverage) total-count)))
        (message "  Medium (70-89%%):    %4d (%.1f%%)"
                 medium-coverage
                 (* 100.0 (/ (float medium-coverage) total-count)))
        (message "  Low (<70%%):         %4d (%.1f%%)"
                 low-coverage
                 (* 100.0 (/ (float low-coverage) total-count)))
        (message "\nStatistics:")
        (message "  Mean coverage:   %.1f%%" mean)
        (message "  Median coverage: %.1f%%" median)
        (message "\nOutput file: %s" output-file)
        (message "\nNext steps:")
        (message "  1. Sort by coverage_pct to find worst cases")
        (message "  2. Filter to missing_count > 0 to see all gaps")
        (message "  3. Examine missing_tokens for patterns")))))

;; Batch mode support
(defun jf/bash-coverage-research-analyze-batch ()
  "Run coverage analysis in batch mode and exit."
  (jf/bash-coverage-research-analyze)
  (kill-emacs 0))

(provide 'analyze-with-coverage)
;;; analyze-with-coverage.el ends here
