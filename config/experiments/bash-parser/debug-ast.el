;;; debug-ast.el --- Debug bash AST structure -*- lexical-binding: t; -*-

(require 'treesit)

(defun jf/bash-debug-ast (command-string)
  "Debug the AST structure of COMMAND-STRING."
  (with-temp-buffer
    (insert command-string)
    (let* ((parser (treesit-parser-create 'bash))
           (root-node (treesit-parser-root-node parser)))
      (list :command command-string
            :root-type (treesit-node-type root-node)
            :root-text (treesit-node-text root-node t)
            :tree (jf/bash-debug--node-tree root-node 0)))))

(defun jf/bash-debug--node-tree (node depth)
  "Build a tree representation of NODE at DEPTH."
  (when (< depth 10) ; limit depth to avoid infinite recursion
    (let* ((type (treesit-node-type node))
           (text (treesit-node-text node t))
           (child-count (treesit-node-child-count node))
           (children '()))
      (dotimes (i child-count)
        (push (jf/bash-debug--node-tree (treesit-node-child node i) (1+ depth))
              children))
      (list :type type
            :text (if (> (length text) 50)
                      (concat (substring text 0 47) "...")
                    text)
            :children (nreverse children)))))

(defun jf/bash-debug-print-tree (tree &optional indent)
  "Pretty print TREE with INDENT."
  (setq indent (or indent 0))
  (let ((prefix (make-string indent ?\ ))
        (type (plist-get tree :type))
        (text (plist-get tree :text))
        (children (plist-get tree :children)))
    (insert (format "%s%s: \"%s\"\n" prefix type text))
    (dolist (child children)
      (jf/bash-debug-print-tree child (+ indent 2)))))

;; Test with some commands
(let ((tests '("ls"
               "ls -la"
               "git log"
               "git log --oneline")))
  (with-current-buffer (get-buffer-create "*bash-ast-debug*")
    (erase-buffer)
    (dolist (cmd tests)
      (let ((ast (jf/bash-debug-ast cmd)))
        (insert (format "\n=== Command: %s ===\n" cmd))
        (insert (format "Root type: %s\n" (plist-get ast :root-type)))
        (insert "\nTree:\n")
        (jf/bash-debug-print-tree (plist-get ast :tree))))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'debug-ast)
;;; debug-ast.el ends here
