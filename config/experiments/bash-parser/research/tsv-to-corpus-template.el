;;; tsv-to-corpus-template.el --- Convert TSV analysis to corpus templates -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Helper tool to convert TSV analysis results from bash-parser research
;; into corpus file template format for manual refinement.
;;
;; Usage:
;;   M-x jf/generate-corpus-from-tsv
;;   Select TSV file, filter by gap type, specify count
;;   Review generated templates in buffer and manually refine

;;; Code:

(require 'cl-lib)

;;; Configuration

(defvar jf/corpus-id-counters (make-hash-table :test 'equal)
  "Hash table tracking ID counters for each category.")

;;; Utility Functions

(defun jf/reset-corpus-id-counters ()
  "Reset all corpus ID counters to zero."
  (interactive)
  (clrhash jf/corpus-id-counters)
  (message "Corpus ID counters reset"))

(defun jf/generate-corpus-id (category)
  "Generate unique corpus ID for CATEGORY.
Returns format like \"heredoc-001\", \"pipe-002\", etc."
  (let* ((counter (or (gethash category jf/corpus-id-counters) 0))
         (new-counter (1+ counter)))
    (puthash category new-counter jf/corpus-id-counters)
    (format "%s-%03d" category new-counter)))

(defun jf/infer-category-from-command (command)
  "Categorize COMMAND as simple/nested/quoted/complex.
Returns category symbol based on command structure."
  (cond
   ;; Complex patterns
   ((string-match-p "<<['\"]?\\w+" command) 'heredoc)
   ((string-match-p "\\$(" command) 'command-subst)
   ((> (cl-count ?\| command) 1) 'multi-pipe)
   ((string-match-p "&&\\|;" command) 'compound)
   ((string-match-p "\\[\\[.*\\]\\]" command) 'test-expr)

   ;; Moderate complexity
   ((string-match-p "\\$(" command) 'subshell)
   ((string-match-p "|" command) 'pipe)
   ((string-match-p "['\"].*['\"]" command) 'quoted)

   ;; Simple
   (t 'simple)))

(defun jf/extract-expect-structure (command)
  "Parse COMMAND and suggest :expect plist structure.
Returns a plist with inferred structure elements."
  (let ((expect-list '()))

    ;; Extract command name (first word)
    (when (string-match "^\\s-*\\([a-zA-Z0-9_-]+\\)" command)
      (push :command-name expect-list)
      (push (match-string 1 command) expect-list))

    ;; Detect flags
    (let ((flags '()))
      (save-match-data
        (let ((pos 0))
          (while (string-match "\\s-\\(-[a-zA-Z0-9]+\\)" command pos)
            (push (match-string 1 command) flags)
            (setq pos (match-end 0)))))
      (when flags
        (push :flags expect-list)
        (push (nreverse flags) expect-list)))

    ;; Detect heredocs
    (when (string-match "<<\\(['\"]?\\)\\(\\w+\\)\\1" command)
      (let ((quoted (not (string-empty-p (match-string 1 command))))
            (delimiter (match-string 2 command)))
        (push :heredocs expect-list)
        (push (list (list :delimiter delimiter
                         :quoted quoted
                         :content "TODO"))
              expect-list)))

    ;; Detect command substitution
    (when (string-match-p "\\$(" command)
      (push :command-substitution expect-list)
      (push t expect-list))

    ;; Detect pipes
    (when (string-match-p "|" command)
      (push :pipes expect-list)
      (push t expect-list))

    (nreverse expect-list)))

(defun jf/parse-missing-tokens (tokens-str)
  "Parse TOKENS-STR into a list of token symbols.
Handles comma-separated token names."
  (when (and tokens-str (not (string-empty-p tokens-str)))
    (mapcar (lambda (s) (intern (string-trim s)))
            (split-string tokens-str ","))))

(defun jf/parse-gap-types (gaps-str)
  "Parse GAPS-STR into a list of gap type strings.
Handles comma-separated gap types."
  (when (and gaps-str (not (string-empty-p gaps-str)))
    (mapcar #'string-trim
            (split-string gaps-str ","))))

;;; Core Conversion Functions

(defun jf/tsv-to-corpus-entry (row)
  "Convert TSV ROW to elisp plist template for corpus entry.
ROW is a list: (command parse_status coverage_pct missing_count
                missing_tokens semantic_gap_count critical_gaps gap_types)"
  (let* ((command (nth 0 row))
         (parse-status (nth 1 row))
         (coverage-pct (string-to-number (nth 2 row)))
         (missing-count (string-to-number (nth 3 row)))
         (missing-tokens (jf/parse-missing-tokens (nth 4 row)))
         (semantic-gap-count (string-to-number (nth 5 row)))
         (critical-gaps (nth 6 row))
         (gap-types (jf/parse-gap-types (nth 7 row)))
         (category (jf/infer-category-from-command command))
         (id (jf/generate-corpus-id (symbol-name category)))
         (expect-struct (jf/extract-expect-structure command)))

    (list :id id
          :category (symbol-name category)
          :command command
          :expect expect-struct
          :notes (format "REAL: From research - Status: %s, Coverage: %.1f%%, Missing: %d, Gaps: %s"
                        parse-status
                        coverage-pct
                        missing-count
                        (string-join gap-types ", ")))))

(defun jf/format-corpus-entry (entry)
  "Format corpus ENTRY plist as readable elisp code string."
  (let ((id (plist-get entry :id))
        (category (plist-get entry :category))
        (command (plist-get entry :command))
        (expect (plist-get entry :expect))
        (notes (plist-get entry :notes)))

    (concat
     (format "(:id \"%s\"\n" id)
     (format " :category \"%s\"\n" category)
     (format " :command %S\n" command)
     (format " :expect %s\n" (pp-to-string expect))
     (format " :notes %S)\n" notes))))

;;; TSV Reading

(defun jf/read-tsv-file (filepath)
  "Read TSV file at FILEPATH and return list of rows.
Skips header row. Returns list of lists."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let ((lines (split-string (buffer-string) "\n" t))
          (rows '()))
      ;; Skip header (first line)
      (dolist (line (cdr lines))
        (unless (string-empty-p (string-trim line))
          (push (split-string line "\t") rows)))
      (nreverse rows))))

(defun jf/filter-rows-by-gap-type (rows gap-type)
  "Filter ROWS to only those containing GAP-TYPE in gap_types column.
If GAP-TYPE is nil or empty, return all rows."
  (if (or (null gap-type) (string-empty-p gap-type))
      rows
    (cl-remove-if-not
     (lambda (row)
       (let ((gap-types (nth 7 row)))
         (and gap-types
              (string-match-p (regexp-quote gap-type) gap-types))))
     rows)))

(defun jf/sort-rows-by-coverage (rows)
  "Sort ROWS by coverage_pct ascending (lowest coverage first)."
  (sort rows
        (lambda (a b)
          (< (string-to-number (nth 2 a))
             (string-to-number (nth 2 b))))))

;;; Interactive Command

;;;###autoload
(defun jf/generate-corpus-from-tsv (filepath gap-type count)
  "Generate corpus entries from TSV file at FILEPATH.
Filter by GAP-TYPE (empty string for all).
Generate COUNT entries (sorted by lowest coverage first).
Output to buffer for manual review and editing."
  (interactive
   (list
    (read-file-name "TSV file: "
                    (expand-file-name "config/experiments/bash-parser/research/"
                                     (or (bound-and-true-p jf/emacs-dir)
                                         user-emacs-directory)))
    (completing-read "Filter by gap type (empty for all): "
                     '("heredoc" "command-substitution" "pipe" "test-expr"
                       "parameter-expansion" "brace-expansion" "arithmetic")
                     nil nil "")
    (read-number "Number of entries to generate: " 10)))

  ;; Reset counters for fresh ID generation
  (jf/reset-corpus-id-counters)

  ;; Read and process TSV
  (let* ((rows (jf/read-tsv-file filepath))
         (filtered (jf/filter-rows-by-gap-type rows gap-type))
         (sorted (jf/sort-rows-by-coverage filtered))
         (selected (cl-subseq sorted 0 (min count (length sorted))))
         (entries (mapcar #'jf/tsv-to-corpus-entry selected))
         (output-buffer (generate-new-buffer "*corpus-templates*")))

    ;; Generate output
    (with-current-buffer output-buffer
      (emacs-lisp-mode)
      (insert ";; Generated Corpus Templates\n")
      (insert (format ";; Source: %s\n" filepath))
      (insert (format ";; Filter: %s\n" (if (string-empty-p gap-type) "all" gap-type)))
      (insert (format ";; Count: %d\n\n" (length entries)))
      (insert ";; NOTE: Review and manually refine these templates before adding to corpus.\n")
      (insert ";; - Verify :expect structure matches command behavior\n")
      (insert ";; - Fill in TODO placeholders\n")
      (insert ";; - Adjust category if needed\n")
      (insert ";; - Add more specific :expect fields as needed\n\n")

      (insert "(\n")
      (dolist (entry entries)
        (insert (jf/format-corpus-entry entry))
        (insert "\n"))
      (insert ")\n")

      (goto-char (point-min)))

    ;; Display results
    (pop-to-buffer output-buffer)
    (message "Generated %d corpus entries from %d filtered rows"
             (length entries)
             (length filtered))))

;;;###autoload
(defun jf/inspect-tsv-gap-types (filepath)
  "Inspect TSV file at FILEPATH and show distribution of gap types.
Useful for understanding what filters to use."
  (interactive
   (list
    (read-file-name "TSV file: "
                    (expand-file-name "config/experiments/bash-parser/research/"
                                     (or (bound-and-true-p jf/emacs-dir)
                                         user-emacs-directory)))))

  (let* ((rows (jf/read-tsv-file filepath))
         (gap-counts (make-hash-table :test 'equal)))

    ;; Count gap types
    (dolist (row rows)
      (let ((gaps (jf/parse-gap-types (nth 7 row))))
        (dolist (gap gaps)
          (puthash gap (1+ (or (gethash gap gap-counts) 0)) gap-counts))))

    ;; Display results
    (let ((output-buffer (generate-new-buffer "*gap-type-distribution*")))
      (with-current-buffer output-buffer
        (insert (format "Gap Type Distribution for %s\n" filepath))
        (insert (format "Total rows: %d\n\n" (length rows)))
        (insert "Gap Type                        Count\n")
        (insert "----------------------------------------\n")

        (let ((sorted-gaps (sort (hash-table-keys gap-counts)
                                (lambda (a b)
                                  (> (gethash a gap-counts)
                                     (gethash b gap-counts))))))
          (dolist (gap sorted-gaps)
            (insert (format "%-30s  %5d\n" gap (gethash gap gap-counts)))))

        (goto-char (point-min)))

      (pop-to-buffer output-buffer))))

(provide 'tsv-to-corpus-template)
;;; tsv-to-corpus-template.el ends here
