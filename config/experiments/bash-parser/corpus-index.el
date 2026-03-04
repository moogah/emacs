;;; corpus-index.el --- Unified index for all bash parser corpus files -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides a unified interface to all bash parser corpus files.
;; It includes helper functions to load all corpus data, filter tests by
;; various criteria, and generate corpus statistics.
;;
;; Usage:
;;
;;   ;; Load all corpus files
;;   (jf/bash-corpus-load-all)
;;
;;   ;; Access combined corpus
;;   jf/bash-all-corpus  ; => list of all 154 tests
;;
;;   ;; Query functions
;;   (jf/bash-corpus-by-category "simple")
;;   (jf/bash-corpus-by-gap-type 'command-substitution)
;;   (jf/bash-corpus-real-only)
;;   (jf/bash-corpus-by-file 'command-substitution)
;;
;;   ;; Statistics
;;   (jf/bash-corpus-stats)
;;

;;; Code:

(require 'seq)

;; ============================================================
;; Corpus File Loading
;; ============================================================

(defvar jf/bash-corpus-files
  '((command-substitution . "command-substitution-corpus.el")
    (heredoc . "heredoc-corpus.el")
    (for-loop . "for-loop-corpus.el")
    (conditional . "conditional-corpus.el")
    (process-substitution . "process-substitution-corpus.el")
    (combined-patterns . "combined-patterns-corpus.el"))
  "Alist mapping corpus types to filenames.")

(defvar jf/bash-corpus-loaded nil
  "Boolean flag indicating whether all corpus files have been loaded.")

(defvar jf/bash-all-corpus nil
  "Combined list of all test cases from all corpus files.
Each test case is a plist with keys :id, :category, :command, :expect, :notes.
Additionally includes :corpus-type key to identify which corpus it came from.")

(defun jf/bash-corpus-load-all ()
  "Load all corpus files and populate `jf/bash-all-corpus'.
Returns total test count."
  (interactive)
  (unless jf/bash-corpus-loaded
    (let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
      (dolist (corpus-entry jf/bash-corpus-files)
        (let ((corpus-type (car corpus-entry))
              (filename (cdr corpus-entry)))
          (load (expand-file-name filename base-dir))))

      ;; Build combined corpus with :corpus-type tag
      (setq jf/bash-all-corpus
            (append
             (mapcar (lambda (test) (plist-put (copy-sequence test) :corpus-type 'command-substitution))
                     jf/bash-command-substitution-corpus)
             (mapcar (lambda (test) (plist-put (copy-sequence test) :corpus-type 'heredoc))
                     jf/bash-heredoc-corpus)
             (mapcar (lambda (test) (plist-put (copy-sequence test) :corpus-type 'for-loop))
                     jf/bash-for-loop-corpus)
             (mapcar (lambda (test) (plist-put (copy-sequence test) :corpus-type 'conditional))
                     jf/bash-conditional-corpus)
             (mapcar (lambda (test) (plist-put (copy-sequence test) :corpus-type 'process-substitution))
                     jf/bash-process-substitution-corpus)
             (mapcar (lambda (test) (plist-put (copy-sequence test) :corpus-type 'combined-patterns))
                     jf/bash-combined-patterns-corpus)))

      (setq jf/bash-corpus-loaded t)
      (message "Loaded %d tests from %d corpus files"
               (length jf/bash-all-corpus)
               (length jf/bash-corpus-files))))

  (length jf/bash-all-corpus))

;; ============================================================
;; Query Functions
;; ============================================================

(defun jf/bash-corpus-by-category (category)
  "Return all tests with matching CATEGORY.
CATEGORY is a string like \"simple\", \"nested\", \"complex\", etc."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))
  (seq-filter (lambda (test)
                (equal (plist-get test :category) category))
              jf/bash-all-corpus))

(defun jf/bash-corpus-by-gap-type (gap-type)
  "Return all tests for the given GAP-TYPE.
GAP-TYPE is a symbol: 'command-substitution, 'heredoc, 'for-loop,
'conditional, 'process-substitution, or 'combined-patterns."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))
  (seq-filter (lambda (test)
                (eq (plist-get test :corpus-type) gap-type))
              jf/bash-all-corpus))

(defun jf/bash-corpus-by-file (corpus-type)
  "Return tests from a specific corpus file.
CORPUS-TYPE is a symbol from `jf/bash-corpus-files' keys.
Alias for `jf/bash-corpus-by-gap-type'."
  (jf/bash-corpus-by-gap-type corpus-type))

(defun jf/bash-corpus-real-only ()
  "Return only tests marked as real-world examples.
Filters tests whose :notes field contains \"REAL: From research\"."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))
  (seq-filter (lambda (test)
                (let ((notes (plist-get test :notes)))
                  (and notes (string-match-p "REAL:" notes))))
              jf/bash-all-corpus))

(defun jf/bash-corpus-pedagogical-only ()
  "Return only pedagogical tests (non-real-world).
Filters tests whose :notes field does NOT contain \"REAL: From research\"."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))
  (seq-filter (lambda (test)
                (let ((notes (plist-get test :notes)))
                  (not (and notes (string-match-p "REAL:" notes)))))
              jf/bash-all-corpus))

(defun jf/bash-corpus-by-id (test-id)
  "Return test case with matching TEST-ID.
TEST-ID is a string like \"cmdsub-simple-001\"."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))
  (seq-find (lambda (test)
              (equal (plist-get test :id) test-id))
            jf/bash-all-corpus))

(defun jf/bash-corpus-by-command-pattern (regexp)
  "Return tests whose :command field matches REGEXP."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))
  (seq-filter (lambda (test)
                (string-match-p regexp (plist-get test :command)))
              jf/bash-all-corpus))

;; ============================================================
;; Statistics and Reporting
;; ============================================================

(defun jf/bash-corpus-stats ()
  "Return statistics about the corpus as an alist.
Keys: :total, :by-corpus, :by-category, :real-count, :pedagogical-count."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))

  (let ((total (length jf/bash-all-corpus))
        (by-corpus (make-hash-table :test 'eq))
        (by-category (make-hash-table :test 'equal))
        (real-count 0)
        (pedagogical-count 0))

    ;; Count by corpus type and category
    (dolist (test jf/bash-all-corpus)
      (let ((corpus-type (plist-get test :corpus-type))
            (category (plist-get test :category))
            (notes (plist-get test :notes)))

        ;; Corpus type count
        (puthash corpus-type
                 (1+ (gethash corpus-type by-corpus 0))
                 by-corpus)

        ;; Category count
        (puthash category
                 (1+ (gethash category by-category 0))
                 by-category)

        ;; Real vs pedagogical
        (if (and notes (string-match-p "REAL:" notes))
            (cl-incf real-count)
          (cl-incf pedagogical-count))))

    ;; Convert hash tables to alists
    (let ((corpus-alist '())
          (category-alist '()))
      (maphash (lambda (k v) (push (cons k v) corpus-alist)) by-corpus)
      (maphash (lambda (k v) (push (cons k v) category-alist)) by-category)

      (list :total total
            :by-corpus (sort corpus-alist (lambda (a b) (> (cdr a) (cdr b))))
            :by-category (sort category-alist (lambda (a b) (> (cdr a) (cdr b))))
            :real-count real-count
            :pedagogical-count pedagogical-count))))

(defun jf/bash-corpus-print-stats ()
  "Print corpus statistics in human-readable format."
  (interactive)
  (let* ((stats (jf/bash-corpus-stats))
         (total (plist-get stats :total))
         (by-corpus (plist-get stats :by-corpus))
         (by-category (plist-get stats :by-category))
         (real-count (plist-get stats :real-count))
         (pedagogical-count (plist-get stats :pedagogical-count)))

    (with-output-to-temp-buffer "*Bash Corpus Statistics*"
      (princ (format "Bash Parser Corpus Statistics\n"))
      (princ (format "==============================\n\n"))
      (princ (format "Total Tests: %d\n\n" total))

      (princ (format "By Corpus Type:\n"))
      (dolist (entry by-corpus)
        (princ (format "  %-25s %3d tests\n" (car entry) (cdr entry))))

      (princ (format "\nBy Category:\n"))
      (dolist (entry by-category)
        (princ (format "  %-25s %3d tests\n" (car entry) (cdr entry))))

      (princ (format "\nReal vs Pedagogical:\n"))
      (princ (format "  Real-world examples:      %3d tests (%d%%)\n"
                     real-count (/ (* 100 real-count) total)))
      (princ (format "  Pedagogical tests:        %3d tests (%d%%)\n"
                     pedagogical-count (/ (* 100 pedagogical-count) total))))))

;; ============================================================
;; Test Runner Integration
;; ============================================================

(defun jf/bash-corpus-validate-structure (test)
  "Validate that TEST has required plist fields.
Returns t if valid, or a list of missing fields if invalid."
  (let ((required-fields '(:id :category :command :expect :notes))
        (missing '()))
    (dolist (field required-fields)
      (unless (plist-member test field)
        (push field missing)))
    (if missing
        (nreverse missing)
      t)))

(defun jf/bash-corpus-validate-all ()
  "Validate structure of all tests in the corpus.
Returns list of (test-id . missing-fields) for tests with issues."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))

  (let ((invalid-tests '()))
    (dolist (test jf/bash-all-corpus)
      (let ((validation (jf/bash-corpus-validate-structure test)))
        (unless (eq validation t)
          (push (cons (plist-get test :id) validation) invalid-tests))))
    (nreverse invalid-tests)))

(defun jf/bash-corpus-check-duplicate-ids ()
  "Check for duplicate test IDs across all corpus files.
Returns list of duplicate IDs."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))

  (let ((id-table (make-hash-table :test 'equal))
        (duplicates '()))
    (dolist (test jf/bash-all-corpus)
      (let ((id (plist-get test :id)))
        (if (gethash id id-table)
            (push id duplicates)
          (puthash id t id-table))))
    (delete-dups duplicates)))

;; ============================================================
;; Export and Reporting
;; ============================================================

(defun jf/bash-corpus-export-to-tsv (filename)
  "Export all corpus tests to TSV file at FILENAME.
Columns: id, corpus_type, category, command (truncated), has_real_marker."
  (interactive "FExport corpus to TSV file: ")
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))

  (with-temp-file filename
    (insert "id\tcorpus_type\tcategory\tcommand_preview\treal_example\n")
    (dolist (test jf/bash-all-corpus)
      (let* ((id (plist-get test :id))
             (corpus-type (symbol-name (plist-get test :corpus-type)))
             (category (plist-get test :category))
             (command (plist-get test :command))
             (notes (plist-get test :notes))
             (command-preview (if (> (length command) 60)
                                  (concat (substring command 0 57) "...")
                                command))
             (is-real (if (and notes (string-match-p "REAL:" notes)) "yes" "no")))
        (insert (format "%s\t%s\t%s\t%s\t%s\n"
                        id corpus-type category
                        (replace-regexp-in-string "\n" "\\\\n" command-preview)
                        is-real)))))

  (message "Exported %d tests to %s" (length jf/bash-all-corpus) filename))

;; ============================================================
;; Corpus Index Summary
;; ============================================================

(defvar jf/bash-corpus-version "1.0"
  "Version of the bash parser corpus.")

(defun jf/bash-corpus-summary ()
  "Return a summary string of the corpus."
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))

  (let* ((stats (jf/bash-corpus-stats))
         (total (plist-get stats :total))
         (by-corpus (plist-get stats :by-corpus))
         (real-count (plist-get stats :real-count))
         (pedagogical-count (plist-get stats :pedagogical-count)))

    (format "Bash Parser Corpus v%s: %d tests (%d real, %d pedagogical) across %d files"
            jf/bash-corpus-version
            total
            real-count
            pedagogical-count
            (length by-corpus))))

;; ============================================================
;; Quick Access Functions
;; ============================================================

(defun jf/bash-corpus-get-command-substitution-tests ()
  "Get all command substitution tests."
  (jf/bash-corpus-by-gap-type 'command-substitution))

(defun jf/bash-corpus-get-heredoc-tests ()
  "Get all heredoc tests."
  (jf/bash-corpus-by-gap-type 'heredoc))

(defun jf/bash-corpus-get-for-loop-tests ()
  "Get all for-loop tests."
  (jf/bash-corpus-by-gap-type 'for-loop))

(defun jf/bash-corpus-get-conditional-tests ()
  "Get all conditional tests."
  (jf/bash-corpus-by-gap-type 'conditional))

(defun jf/bash-corpus-get-process-substitution-tests ()
  "Get all process substitution tests."
  (jf/bash-corpus-by-gap-type 'process-substitution))

(defun jf/bash-corpus-get-combined-tests ()
  "Get all combined pattern tests (integration tests)."
  (jf/bash-corpus-by-gap-type 'combined-patterns))

;; ============================================================
;; Interactive Exploration
;; ============================================================

(defun jf/bash-corpus-browse ()
  "Interactively browse corpus tests by category or gap type."
  (interactive)
  (unless jf/bash-corpus-loaded (jf/bash-corpus-load-all))

  (let* ((choice (completing-read "Browse by: "
                                   '("Gap Type" "Category" "Real Examples" "All")
                                   nil t))
         (tests (cond
                 ((equal choice "Gap Type")
                  (let ((gap-type (intern (completing-read
                                           "Gap type: "
                                           '("command-substitution"
                                             "heredoc"
                                             "for-loop"
                                             "conditional"
                                             "process-substitution"
                                             "combined-patterns")
                                           nil t))))
                    (jf/bash-corpus-by-gap-type gap-type)))

                 ((equal choice "Category")
                  (let* ((categories (delete-dups
                                      (mapcar (lambda (test) (plist-get test :category))
                                              jf/bash-all-corpus)))
                         (category (completing-read "Category: " categories nil t)))
                    (jf/bash-corpus-by-category category)))

                 ((equal choice "Real Examples")
                  (jf/bash-corpus-real-only))

                 ((equal choice "All")
                  jf/bash-all-corpus)

                 (t nil))))

    (if tests
        (with-output-to-temp-buffer "*Bash Corpus Browser*"
          (princ (format "Found %d tests\n\n" (length tests)))
          (dolist (test tests)
            (princ (format "ID: %s\n" (plist-get test :id)))
            (princ (format "Corpus: %s\n" (plist-get test :corpus-type)))
            (princ (format "Category: %s\n" (plist-get test :category)))
            (princ (format "Command: %s\n" (plist-get test :command)))
            (princ (format "Notes: %s\n" (plist-get test :notes)))
            (princ "\n---\n\n")))
      (message "No tests found."))))

;; ============================================================
;; Auto-load on require
;; ============================================================

;; Automatically load corpus when this file is required
(unless jf/bash-corpus-loaded
  (jf/bash-corpus-load-all))

(provide 'corpus-index)
;;; corpus-index.el ends here
