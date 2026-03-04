;;; semantic-gap-detection.el --- Detect semantic parsing gaps -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, research, semantics

;;; Commentary:

;; Semantic Gap Detection: Identifies when the parser captures TEXT but misses MEANING.
;;
;; Problem: Coverage analysis measures text capture, not semantic understanding.
;; Example: `echo $(whoami)` may show 100% coverage but parser doesn't extract
;;          the nested command `whoami`, creating a critical security gap.
;;
;; Solution: Use tree-sitter AST to detect semantic structures (command substitution,
;;           process substitution, loops, conditionals), then verify parser output
;;           has corresponding semantic fields.
;;
;; This complements text-based coverage analysis by detecting gaps that are
;; invisible to token-matching approaches.

;;; Code:

(require 'bash-parser)

;;; AST Structure Detection

(defun jf/bash-parse--find-nodes-by-type (node type-name)
  "Find all descendant nodes of TYPE-NAME under NODE.
Returns list of matching nodes (empty list if none found)."
  (let ((found '()))
    (jf/bash-parse--visit-node
     node
     (lambda (n)
       (when (string= (treesit-node-type n) type-name)
         (push n found))
       nil))  ; Continue traversal
    (nreverse found)))

(defun jf/bash-parse--has-node-type-p (node type-name)
  "Return non-nil if NODE has any descendant of TYPE-NAME."
  (catch 'found
    (jf/bash-parse--visit-node
     node
     (lambda (n)
       (when (string= (treesit-node-type n) type-name)
         (throw 'found t))
       nil))))

;;; Semantic Gap Detection

(defun jf/bash-parse-semantic--detect-command-substitution-gap (root-node parsed-result)
  "Check if command substitution exists in AST but not in parsed result.
Returns gap descriptor or nil."
  (when (jf/bash-parse--has-node-type-p root-node "command_substitution")
    ;; Tree-sitter found command substitution - does parser extract it?
    (let ((substitutions (plist-get parsed-result :command-substitutions)))
      (unless substitutions
        ;; Parser doesn't have :command-substitutions field - critical gap
        (let ((nodes (jf/bash-parse--find-nodes-by-type root-node "command_substitution"))
              (examples '()))
          ;; Extract up to 3 examples
          (dolist (node (seq-take nodes 3))
            (push (treesit-node-text node t) examples))
          (list :type 'command-substitution
                :severity 'critical
                :count (length nodes)
                :examples (nreverse examples)
                :reason "Command substitution detected in AST but not extracted by parser"
                :security-impact "Nested commands invisible to security validation"))))))

(defun jf/bash-parse-semantic--detect-process-substitution-gap (root-node parsed-result)
  "Check if process substitution exists in AST but not in parsed result.
Returns gap descriptor or nil."
  (when (jf/bash-parse--has-node-type-p root-node "process_substitution")
    (let ((substitutions (plist-get parsed-result :process-substitutions)))
      (unless substitutions
        (let ((nodes (jf/bash-parse--find-nodes-by-type root-node "process_substitution"))
              (examples '()))
          (dolist (node (seq-take nodes 3))
            (push (treesit-node-text node t) examples))
          (list :type 'process-substitution
                :severity 'critical
                :count (length nodes)
                :examples (nreverse examples)
                :reason "Process substitution detected in AST but not extracted by parser"
                :security-impact "Process substitution commands invisible to validation"))))))

(defun jf/bash-parse-semantic--detect-for-loop-gap (root-node parsed-result)
  "Check if for loop exists in AST but variables not extracted.
Returns gap descriptor or nil."
  (when (jf/bash-parse--has-node-type-p root-node "for_statement")
    (let ((loop-vars (plist-get parsed-result :loop-variables)))
      (unless loop-vars
        (let ((nodes (jf/bash-parse--find-nodes-by-type root-node "for_statement")))
          (list :type 'for-loop
                :severity 'medium
                :count (length nodes)
                :examples nil  ; For loops are complex, skip examples
                :reason "For loop detected but iteration variables not extracted"
                :security-impact "Loop iteration values invisible (may be file paths)"))))))

(defun jf/bash-parse-semantic--detect-conditional-gap (root-node parsed-result)
  "Check if conditional exists in AST but branches not extracted.
Returns gap descriptor or nil."
  (when (jf/bash-parse--has-node-type-p root-node "if_statement")
    (let ((branches (plist-get parsed-result :conditional-branches)))
      (unless branches
        (let ((nodes (jf/bash-parse--find-nodes-by-type root-node "if_statement")))
          (list :type 'conditional
                :severity 'medium
                :count (length nodes)
                :examples nil
                :reason "Conditional statement detected but branches not extracted"
                :security-impact "Conditional file operations may be invisible"))))))

(defun jf/bash-parse-semantic--detect-heredoc-gap (root-node parsed-result)
  "Check if heredoc exists in AST but content not extracted.
Returns gap descriptor or nil."
  (when (jf/bash-parse--has-node-type-p root-node "heredoc_body")
    (let ((heredoc-content (plist-get parsed-result :heredoc-content)))
      (unless heredoc-content
        (let ((nodes (jf/bash-parse--find-nodes-by-type root-node "heredoc_body")))
          (list :type 'heredoc
                :severity 'low
                :count (length nodes)
                :examples nil
                :reason "Heredoc detected but content not extracted"
                :security-impact "Low (heredoc is literal text, not executed)"))))))

(defun jf/bash-parse-semantic--detect-while-loop-gap (root-node parsed-result)
  "Check if while loop exists in AST but not fully parsed.
Returns gap descriptor or nil."
  (when (jf/bash-parse--has-node-type-p root-node "while_statement")
    (let ((while-info (plist-get parsed-result :while-loops)))
      (unless while-info
        (let ((nodes (jf/bash-parse--find-nodes-by-type root-node "while_statement")))
          (list :type 'while-loop
                :severity 'medium
                :count (length nodes)
                :examples nil
                :reason "While loop detected but not extracted"
                :security-impact "Loop body commands may be invisible"))))))

(defun jf/bash-parse-semantic--detect-case-statement-gap (root-node parsed-result)
  "Check if case statement exists in AST but not fully parsed.
Returns gap descriptor or nil."
  (when (jf/bash-parse--has-node-type-p root-node "case_statement")
    (let ((case-info (plist-get parsed-result :case-branches)))
      (unless case-info
        (let ((nodes (jf/bash-parse--find-nodes-by-type root-node "case_statement")))
          (list :type 'case-statement
                :severity 'medium
                :count (length nodes)
                :examples nil
                :reason "Case statement detected but branches not extracted"
                :security-impact "Case branch commands may be invisible"))))))

;;; Main Entry Point

(defun jf/bash-parse-semantic--detect-gaps (command-string parsed-result)
  "Detect semantic structures in COMMAND-STRING missing from PARSED-RESULT.

Uses tree-sitter AST to identify semantic patterns (command substitution,
loops, conditionals, etc.), then verifies parser output has corresponding
semantic fields.

Returns list of gap descriptors. Each descriptor is a plist with:
  :type - Symbol identifying gap type (e.g., 'command-substitution)
  :severity - 'critical, 'medium, or 'low
  :count - Number of instances found in AST
  :examples - List of example text snippets (up to 3, may be nil)
  :reason - Human-readable explanation
  :security-impact - Description of security implications

Returns empty list if no semantic gaps detected."
  (condition-case err
      (with-temp-buffer
        (insert command-string)
        (let* ((parser (treesit-parser-create 'bash))
               (root-node (treesit-parser-root-node parser))
               (gaps '()))

          ;; Run all gap detectors
          (dolist (detector '(jf/bash-parse-semantic--detect-command-substitution-gap
                             jf/bash-parse-semantic--detect-process-substitution-gap
                             jf/bash-parse-semantic--detect-for-loop-gap
                             jf/bash-parse-semantic--detect-conditional-gap
                             jf/bash-parse-semantic--detect-heredoc-gap
                             jf/bash-parse-semantic--detect-while-loop-gap
                             jf/bash-parse-semantic--detect-case-statement-gap))
            (when-let ((gap (funcall detector root-node parsed-result)))
              (push gap gaps)))

          (nreverse gaps)))

    (error
     ;; If tree-sitter parsing fails, return error gap
     (list (list :type 'ast-error
                :severity 'critical
                :count 1
                :examples (list (error-message-string err))
                :reason "Failed to parse AST with tree-sitter"
                :security-impact "Cannot verify semantic completeness")))))

;;; Summary Helpers

(defun jf/bash-parse-semantic--summarize-gap (gap)
  "Create human-readable summary of GAP descriptor."
  (let ((type (plist-get gap :type))
        (severity (plist-get gap :severity))
        (count (plist-get gap :count))
        (examples (plist-get gap :examples))
        (reason (plist-get gap :reason)))
    (format "[%s] %s: %s (%d instance%s)%s"
            (upcase (symbol-name severity))
            (symbol-name type)
            reason
            count
            (if (= count 1) "" "s")
            (if examples
                (format "\n    Examples: %s" (mapconcat #'identity examples ", "))
              ""))))

(defun jf/bash-parse-semantic--count-by-severity (gaps)
  "Count gaps by severity level. Returns alist of (severity . count)."
  (let ((critical 0)
        (medium 0)
        (low 0))
    (dolist (gap gaps)
      (pcase (plist-get gap :severity)
        ('critical (setq critical (1+ critical)))
        ('medium (setq medium (1+ medium)))
        ('low (setq low (1+ low)))))
    (list (cons 'critical critical)
          (cons 'medium medium)
          (cons 'low low))))

(defun jf/bash-parse-semantic--has-critical-gaps-p (gaps)
  "Return non-nil if GAPS contains any critical-severity gaps."
  (seq-some (lambda (gap) (eq (plist-get gap :severity) 'critical))
            gaps))

;;; Interactive Testing

(defun jf/bash-parse-semantic-test ()
  "Test semantic gap detection on example commands."
  (interactive)
  (let ((test-cases
         '(("echo $(whoami)" . "Command substitution not extracted")
           ("ls -la $(dirname $(which openspec))" . "Nested command substitution")
           ("diff <(ls dir1) <(ls dir2)" . "Process substitution")
           ("for f in *.txt; do cat $f; done" . "For loop variables")
           ("if [ -f file ]; then rm file; fi" . "Conditional branches")
           ("cat <<EOF\nContent here\nEOF" . "Heredoc content")
           ("ls -la /tmp" . "Simple command - no gaps expected")
           ("git commit -m 'message'" . "Simple command with args"))))

    (with-current-buffer (get-buffer-create "*Semantic Gap Test*")
      (erase-buffer)
      (insert "=== Semantic Gap Detection Test ===\n\n")

      (dolist (test test-cases)
        (let* ((command (car test))
               (description (cdr test))
               (parsed (jf/bash-parse command))
               (gaps (jf/bash-parse-semantic--detect-gaps command parsed))
               (gap-count (length gaps))
               (critical-count (length (seq-filter
                                       (lambda (g) (eq (plist-get g :severity) 'critical))
                                       gaps))))

          (insert (format "\n--- %s ---\n" description))
          (insert (format "Command: %s\n" command))
          (insert (format "Gaps found: %d" gap-count))
          (when (> critical-count 0)
            (insert (format " (%d critical)" critical-count)))
          (insert "\n")

          (if gaps
              (dolist (gap gaps)
                (insert (format "  %s\n" (jf/bash-parse-semantic--summarize-gap gap))))
            (insert "  ✓ No semantic gaps\n"))))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'semantic-gap-detection)
;;; semantic-gap-detection.el ends here
