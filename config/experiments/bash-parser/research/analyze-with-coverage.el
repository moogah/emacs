;;; analyze-with-coverage.el --- Analyze corpus with parse coverage metrics -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, research, coverage

;;; Commentary:

;; Phase 2 analysis: Compute parse coverage AND semantic gap detection
;; for all research commands.
;;
;; Two-layer analysis:
;;   Layer 1: Text coverage - measures token capture completeness
;;   Layer 2: Semantic gaps - detects structures in AST missing from parser output
;;
;; Output format: TSV with columns:
;; command | parse_status | coverage_pct | missing_count | file_ops_count |
;; command_type | dangerous | missing_tokens | semantic_gap_count |
;; critical_gaps | gap_types

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
(require 'semantic-gap-detection)

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

(defun jf/bash-coverage-research--format-gap-types (gaps)
  "Format semantic GAPS as comma-separated type list."
  (if gaps
      (mapconcat (lambda (gap)
                   (symbol-name (plist-get gap :type)))
                 gaps ", ")
    ""))

(defun jf/bash-coverage-research--analyze-command (command-string)
  "Analyze COMMAND-STRING with coverage metrics AND semantic gap detection.
Returns TSV line with all analysis data."
  (condition-case err
      (let* ((parse-result (jf/bash-parse command-string))
             (parse-success (plist-get parse-result :success))
             (parse-error (plist-get parse-result :error))
             (command-type (plist-get parse-result :type))
             (is-dangerous (plist-get parse-result :dangerous-p))

             ;; Layer 1: Text coverage
             (coverage-result (jf/bash-parse-coverage--compute command-string))
             (coverage-pct (plist-get coverage-result :coverage-percentage))
             (missing-count (plist-get coverage-result :missing-count))
             (missing-tokens (plist-get coverage-result :missing-tokens))

             ;; Layer 2: Semantic gap detection
             (semantic-gaps (if parse-success
                               (jf/bash-parse-semantic--detect-gaps command-string parse-result)
                             nil))
             (gap-count (length semantic-gaps))
             (critical-gaps (if semantic-gaps
                               (length (seq-filter
                                       (lambda (g) (eq (plist-get g :severity) 'critical))
                                       semantic-gaps))
                             0))
             (gap-types (jf/bash-coverage-research--format-gap-types semantic-gaps))

             ;; File operations (if parse succeeded)
             (file-ops (if parse-success
                          (condition-case _
                              (jf/bash-extract-file-operations parse-result)
                            (error nil))
                        nil))
             (file-ops-count (if file-ops (length file-ops) 0)))

        (format "%s\t%s\t%.1f\t%d\t%d\t%s\t%s\t%s\t%d\t%d\t%s"
                (jf/bash-coverage-research--escape-field command-string)
                (if parse-success "SUCCESS" "FAILURE")
                coverage-pct
                missing-count
                file-ops-count
                (if command-type (symbol-name command-type) "")
                (if is-dangerous "dangerous" "")
                (jf/bash-coverage-research--format-token-list missing-tokens)
                gap-count          ; NEW: Total semantic gaps
                critical-gaps      ; NEW: Critical severity gaps
                gap-types))        ; NEW: Gap type list

    (error
     (format "%s\tERROR\t0.0\t0\t0\t\t\t%s\t0\t0\t"
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
          ;; NEW: Semantic gap tracking
          (commands-with-semantic-gaps 0)
          (commands-with-critical-gaps 0)
          (total-semantic-gaps 0)
          (total-critical-gaps 0)
          (coding-system-for-write 'utf-8-unix))

      (setq total-count (length commands))
      (message "Processing %d commands..." total-count)

      (with-temp-file output-file
        ;; Write header (updated with semantic gap columns)
        (insert "command\tparse_status\tcoverage_pct\tmissing_count\tfile_ops_count\tcommand_type\tdangerous\tmissing_tokens\tsemantic_gap_count\tcritical_gaps\tgap_types\n")

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
                 (t (setq low-coverage (1+ low-coverage))))))

            ;; Extract semantic gap counts for statistics
            ;; Line format: ...missing_tokens\tsemantic_gap_count\tcritical_gaps\tgap_types
            ;; Split by tabs and get fields 9 (semantic_gap_count) and 10 (critical_gaps)
            (let ((fields (split-string line "\t")))
              (when (>= (length fields) 11)  ; Ensure we have all fields
                (let ((gap-count (string-to-number (nth 8 fields)))      ; Column 9 (0-indexed)
                      (critical-count (string-to-number (nth 9 fields)))) ; Column 10 (0-indexed)
                  (setq total-semantic-gaps (+ total-semantic-gaps gap-count))
                  (setq total-critical-gaps (+ total-critical-gaps critical-count))
                  (when (> gap-count 0)
                    (setq commands-with-semantic-gaps (1+ commands-with-semantic-gaps)))
                  (when (> critical-count 0)
                    (setq commands-with-critical-gaps (1+ commands-with-critical-gaps)))))))))

      ;; Compute statistics
      (setq coverage-values (nreverse coverage-values))
      (let* ((mean (/ (apply #'+ coverage-values) (float total-count)))
             (sorted-vals (sort coverage-values #'<))
             (median (nth (/ total-count 2) sorted-vals)))

        (message "\n=== Coverage Analysis Complete ===")
        (message "Total commands: %d" total-count)
        (message "\nText Coverage Distribution:")
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
        (message "\nText Coverage Statistics:")
        (message "  Mean coverage:   %.1f%%" mean)
        (message "  Median coverage: %.1f%%" median)
        (message "\nSemantic Gap Analysis:")
        (message "  Commands with semantic gaps:    %4d (%.1f%%)"
                 commands-with-semantic-gaps
                 (* 100.0 (/ (float commands-with-semantic-gaps) total-count)))
        (message "  Commands with CRITICAL gaps:    %4d (%.1f%%) ⚠️"
                 commands-with-critical-gaps
                 (* 100.0 (/ (float commands-with-critical-gaps) total-count)))
        (message "  Total semantic gaps detected:   %4d" total-semantic-gaps)
        (message "  Total CRITICAL gaps detected:   %4d" total-critical-gaps)
        (message "\nOutput file: %s" output-file)
        (message "\nNext steps:")
        (message "  1. Filter critical_gaps > 0 to find security-critical gaps")
        (message "  2. Filter semantic_gap_count > 0 AND coverage_pct = 100 for invisible gaps")
        (message "  3. Group by gap_types to identify patterns")
        (message "  4. Sort by coverage_pct to find text capture issues")))))

;; Batch mode support
(defun jf/bash-coverage-research-analyze-batch ()
  "Run coverage analysis in batch mode and exit."
  (jf/bash-coverage-research-analyze)
  (kill-emacs 0))

(provide 'analyze-with-coverage)
;;; analyze-with-coverage.el ends here
