;;; compute-parse-coverage.el --- Measure parse completeness -*- lexical-binding: t; -*-

;; Compute what percentage of input bash command is captured in parser output.
;; This measures the foundational parsing layer, independent of file operations.

(require 'bash-parser)

;;; Token Extraction

(defun jf/bash-parse-coverage--extract-ast-tokens (root-node)
  "Extract all meaningful tokens from tree-sitter AST starting at ROOT-NODE.
Returns list of normalized strings (quotes removed, trimmed)."
  (let ((tokens '()))
    (jf/bash-parse--visit-node
     root-node
     (lambda (node)
       (let ((node-type (treesit-node-type node)))
         (cond
          ;; Terminal text nodes - extract and normalize
          ((member node-type '("word" "string" "raw_string" "ansi_c_string"
                               "simple_expansion" "expansion" "command_substitution"
                               "number" "concatenation"))
           (let ((text (treesit-node-text node t)))
             (when (and text (> (length text) 0))
               (setq text (string-trim text))
               ;; Remove surrounding quotes (matches parser behavior)
               (when (and (> (length text) 1)
                         (or (and (string-prefix-p "\"" text) (string-suffix-p "\"" text))
                             (and (string-prefix-p "'" text) (string-suffix-p "'" text))))
                 (setq text (substring text 1 -1)))
               (unless (string-empty-p text)
                 (push text tokens))))
           :skip-children)  ; Don't recurse into these nodes

          ;; Redirection operators - capture destinations
          ((string= node-type "file_redirect")
           (when-let ((dest (treesit-node-child-by-field-name node "destination")))
             (let ((text (treesit-node-text dest t)))
               (when (and text (> (length text) 0))
                 (push (string-trim text) tokens))))
           nil)

          ;; Continue recursing for structural nodes
          (t nil)))))
    (nreverse tokens)))

(defun jf/bash-parse-coverage--extract-output-tokens (parsed-result)
  "Extract all text tokens from parser output PARSED-RESULT.
Returns list of strings found in the parsed structure."
  (let ((tokens '()))
    ;; Recursively walk the plist/nested list structure
    (cl-labels ((extract (obj)
                  (cond
                   ;; String - capture it
                   ((stringp obj)
                    (let ((text (string-trim obj)))
                      (unless (string-empty-p text)
                        (push text tokens))))

                   ;; List - could be plist or regular list
                   ((consp obj)
                    (cond
                     ;; Plist (keyword-keyed)
                     ((and (car obj) (keywordp (car obj)))
                      (let ((rest obj))
                        (while rest
                          (when (cdr rest)  ; Ensure we have a value
                            (extract (cadr rest))
                            (setq rest (cddr rest)))
                          (unless (cdr rest)
                            (setq rest nil)))))

                     ;; Regular list
                     (t
                      (dolist (item obj)
                        (extract item))))))))

      (extract parsed-result))

    ;; Return unique tokens (some may appear multiple times in structure)
    (delete-dups (nreverse tokens))))

;;; Coverage Computation

(defun jf/bash-parse-coverage--compute (command-string)
  "Compute parse coverage for COMMAND-STRING.

Returns plist with:
  :input-tokens - tokens from tree-sitter AST (list of strings)
  :output-tokens - tokens from parser output (list of strings)
  :missing-tokens - tokens in input but not in output
  :extra-tokens - tokens in output but not in input (shouldn't happen)
  :coverage-percentage - percentage of input tokens captured
  :input-count - total input tokens
  :missing-count - number of missing tokens"
  (condition-case err
      (with-temp-buffer
        (insert command-string)
        (let* ((parser (treesit-parser-create 'bash))
               (root-node (treesit-parser-root-node parser))
               (input-tokens (jf/bash-parse-coverage--extract-ast-tokens root-node))
               (parsed-result (jf/bash-parse command-string))
               (output-tokens (jf/bash-parse-coverage--extract-output-tokens parsed-result))

               ;; Build sets for comparison
               (output-set (make-hash-table :test 'equal))
               (input-set (make-hash-table :test 'equal))
               (missing '())
               (extra '()))

          ;; Populate sets
          (dolist (token output-tokens)
            (puthash token t output-set))
          (dolist (token input-tokens)
            (puthash token t input-set))

          ;; Find missing tokens (in input, not in output)
          (dolist (token input-tokens)
            (unless (gethash token output-set)
              (push token missing)))

          ;; Find extra tokens (in output, not in input - shouldn't happen)
          (dolist (token output-tokens)
            (unless (gethash token input-set)
              (push token extra)))

          (let* ((input-count (length input-tokens))
                 (missing-count (length missing))
                 (matched-count (- input-count missing-count))
                 (coverage (if (> input-count 0)
                              (* 100.0 (/ (float matched-count)
                                        (float input-count)))
                            100.0)))

            (list :input-tokens input-tokens
                  :output-tokens output-tokens
                  :missing-tokens (nreverse missing)
                  :extra-tokens (nreverse extra)
                  :coverage-percentage coverage
                  :input-count input-count
                  :missing-count missing-count))))

    (error
     (list :error (error-message-string err)
           :coverage-percentage 0.0
           :input-count 0
           :missing-count 0))))

;;; Analysis Helpers

(defun jf/bash-parse-coverage--summarize (coverage-result)
  "Create human-readable summary from COVERAGE-RESULT."
  (let ((coverage (plist-get coverage-result :coverage-percentage))
        (input-count (plist-get coverage-result :input-count))
        (missing-count (plist-get coverage-result :missing-count))
        (missing-tokens (plist-get coverage-result :missing-tokens)))

    (format "Coverage: %.1f%% (%d/%d tokens captured)%s"
            coverage
            (- input-count missing-count)
            input-count
            (if (> missing-count 0)
                (format "\n  Missing: %s"
                        (mapconcat #'identity missing-tokens ", "))
              ""))))

(defun jf/bash-parse-coverage--analyze-corpus (commands)
  "Analyze parse coverage for list of COMMANDS.
Returns summary statistics."
  (let ((total 0)
        (perfect-coverage 0)
        (high-coverage 0)   ; >= 90%
        (medium-coverage 0) ; >= 70%
        (low-coverage 0)    ; < 70%
        (coverage-values '())
        (worst-commands '())) ; Track worst 10

    (dolist (cmd commands)
      (let* ((coverage-result (jf/bash-parse-coverage--compute cmd))
             (coverage (plist-get coverage-result :coverage-percentage))
             (missing (plist-get coverage-result :missing-count)))

        (setq total (1+ total))
        (push coverage coverage-values)

        (cond
         ((= coverage 100.0) (setq perfect-coverage (1+ perfect-coverage)))
         ((>= coverage 90.0) (setq high-coverage (1+ high-coverage)))
         ((>= coverage 70.0) (setq medium-coverage (1+ medium-coverage)))
         (t (setq low-coverage (1+ low-coverage))))

        ;; Track worst commands
        (when (< coverage 100.0)
          (push (list :command cmd
                     :coverage coverage
                     :missing-count missing
                     :missing-tokens (plist-get coverage-result :missing-tokens))
                worst-commands))))

    ;; Sort worst commands by coverage (lowest first)
    (setq worst-commands
          (seq-take (sort worst-commands
                         (lambda (a b)
                           (< (plist-get a :coverage)
                              (plist-get b :coverage))))
                    10))

    ;; Compute statistics
    (let* ((mean (/ (apply #'+ coverage-values) (float total)))
           (sorted-vals (sort coverage-values #'<))
           (median (nth (/ total 2) sorted-vals)))

      (list :total total
            :perfect-coverage perfect-coverage
            :high-coverage high-coverage
            :medium-coverage medium-coverage
            :low-coverage low-coverage
            :mean-coverage mean
            :median-coverage median
            :worst-commands worst-commands))))

;;; Interactive Commands

(defun jf/bash-parse-coverage-test ()
  "Test coverage computation on example commands."
  (interactive)
  (let ((test-cases
         '("ls -la /tmp"
           "git commit -m 'message'"
           "find . -name '*.el' -exec grep 'pattern' {} \\;"
           "cat file.txt | grep pattern | sort"
           "if [ -f file ]; then cat file; fi"
           "for f in *.txt; do echo $f; done"
           "bash -c 'rm /tmp/file.txt'"
           "$(command substitution)")))

    (with-current-buffer (get-buffer-create "*Parse Coverage Test*")
      (erase-buffer)
      (dolist (cmd test-cases)
        (let* ((result (jf/bash-parse-coverage--compute cmd))
               (summary (jf/bash-parse-coverage--summarize result)))
          (insert (format "\n=== %s ===\n" cmd))
          (insert summary)
          (insert "\n")))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'compute-parse-coverage)
;;; compute-parse-coverage.el ends here
