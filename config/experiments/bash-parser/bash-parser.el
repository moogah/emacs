;;; bash-parser.el --- Parse bash commands with tree-sitter -*- lexical-binding: t; -*-

(require 'treesit)

(defvar jf/bash-parser-dangerous-patterns
  '((rm . ((:flags ("-rf" "-fr" "-Rf" "-fR" "-r"))
           (:any-flag-contains ("f" "r"))))
    (git . ((:subcommand "push" :flags ("--force" "-f"))
            (:subcommand "reset" :flags ("--hard"))
            (:subcommand "clean" :flags ("-f" "-fd" "-df"))))
    (docker . ((:subcommand "rm" :flags ("-f" "--force"))))
    (npm . ((:subcommand "uninstall" :flags ("--force"))
            (:subcommand "install" :flags ("--force"))))
    (python . ((:flags ("-c"))
               (:any-flag-contains ("c"))))
    (bash . ((:flags ("-c"))
             (:any-flag-contains ("c"))))
    (sh . ((:flags ("-c"))
           (:any-flag-contains ("c")))))
  "Database of dangerous command patterns.
Format: (command . ((:subcommand \"name\" :flags (list))
                    (:flags (list))
                    (:any-flag-contains (list))))")

(defun jf/bash-parse (command-string)
  "Parse COMMAND-STRING using tree-sitter.
Returns plist with:
  :success - t if parsing succeeded
  :command-name - base command
  :subcommand - subcommand if detected (e.g., 'log' in 'git log')
  :flags - list of flags (arguments starting with -)
  :positional-args - list of non-flag arguments
  :dangerous-p - t if command matches dangerous patterns
  :ast - tree-sitter root node (for debugging)
  :error - error message if parsing failed"
  (condition-case err
      (jf/bash-parse--internal command-string)
    (error (list :success nil
                 :error (error-message-string err)))))

(defun jf/bash-parse--internal (command-string)
  "Internal parser implementation for COMMAND-STRING."
  (with-temp-buffer
    (insert command-string)
    (let* ((parser (treesit-parser-create 'bash))
           (root-node (treesit-parser-root-node parser))
           (command-node (jf/bash-parse--find-first-command root-node)))

      (if (null command-node)
          (list :success nil
                :error "No command found in input")

        (let* ((words (jf/bash-parse--extract-words command-node))
               (command-name (car words))
               (remaining-words (cdr words))
               (subcommand (jf/bash-parse--detect-subcommand command-name remaining-words))
               (args-start (if subcommand (cdr remaining-words) remaining-words))
               (flags (jf/bash-parse--extract-flags args-start))
               (positional-args (jf/bash-parse--extract-positional-args args-start))
               (dangerous-p (jf/bash-parse--is-dangerous command-name subcommand flags)))

          (list :success t
                :command-name command-name
                :subcommand subcommand
                :flags flags
                :positional-args positional-args
                :dangerous-p dangerous-p
                :ast root-node))))))

(defun jf/bash-parse--find-first-command (node)
  "Find the first command node in tree starting from NODE."
  (if (null node)
      nil
    (let ((node-type (treesit-node-type node)))
      (cond
       ;; If this node is a command, return it
       ((string= node-type "command")
        node)

       ;; Otherwise search children recursively
       (t
        (let ((child-count (treesit-node-child-count node))
              (result nil))
          (dotimes (i child-count)
            (when (null result)
              (let ((child (treesit-node-child node i)))
                (when child
                  (setq result (jf/bash-parse--find-first-command child))))))
          result))))))

(defun jf/bash-parse--extract-words (command-node)
  "Extract all word nodes from COMMAND-NODE as strings.
Returns list of strings representing all words in the command."
  (let ((words '()))
    (jf/bash-parse--visit-node
     command-node
     (lambda (node)
       (let ((node-type (treesit-node-type node)))
         (when (or (string= node-type "word")
                   (string= node-type "string")
                   (string= node-type "raw_string")
                   (string= node-type "concatenation")
                   (string= node-type "number"))
           (let ((text (treesit-node-text node t)))
             ;; Remove quotes from strings
             (when (and text (> (length text) 0))
               (setq text (string-trim text))
               ;; Remove surrounding quotes if present
               (when (or (and (string-prefix-p "\"" text)
                             (string-suffix-p "\"" text))
                        (and (string-prefix-p "'" text)
                             (string-suffix-p "'" text)))
                 (setq text (substring text 1 -1)))
               (unless (string-empty-p text)
                 (push text words))))))))
    (nreverse words)))

(defun jf/bash-parse--visit-node (node visitor-fn)
  "Visit NODE and all children, calling VISITOR-FN on each."
  (funcall visitor-fn node)
  (let ((child-count (treesit-node-child-count node)))
    (dotimes (i child-count)
      (jf/bash-parse--visit-node
       (treesit-node-child node i)
       visitor-fn))))

(defun jf/bash-parse--detect-subcommand (command-name remaining-words)
  "Detect if COMMAND-NAME has a subcommand in REMAINING-WORDS.
Returns subcommand string or nil."
  (when (and remaining-words
             (member command-name '("git" "docker" "npm" "cargo" "kubectl")))
    ;; First non-flag word after command is the subcommand
    (let ((first-word (car remaining-words)))
      (when (and first-word
                 (not (string-prefix-p "-" first-word)))
        first-word))))

(defun jf/bash-parse--extract-flags (words)
  "Extract flag arguments from WORDS.
Returns list of strings that start with -."
  (seq-filter (lambda (word)
                (string-prefix-p "-" word))
              words))

(defun jf/bash-parse--extract-positional-args (words)
  "Extract non-flag arguments from WORDS.
Returns list of strings that don't start with -."
  (seq-remove (lambda (word)
                (string-prefix-p "-" word))
              words))

(defun jf/bash-parse--is-dangerous (command-name subcommand flags)
  "Check if command matches dangerous patterns.
COMMAND-NAME is the base command.
SUBCOMMAND is the subcommand (or nil).
FLAGS is list of flag strings.
Returns t if dangerous, nil otherwise."
  (when-let ((patterns (alist-get (intern command-name)
                                   jf/bash-parser-dangerous-patterns)))
    (catch 'dangerous
      (dolist (pattern patterns)
        (let ((pattern-subcommand (plist-get pattern :subcommand))
              (pattern-flags (plist-get pattern :flags))
              (pattern-any-contains (plist-get pattern :any-flag-contains)))

          ;; Check if subcommand matches (or pattern has no subcommand requirement)
          (when (or (null pattern-subcommand)
                    (equal pattern-subcommand subcommand))

            ;; Check if any dangerous flag is present
            (when pattern-flags
              (dolist (dangerous-flag pattern-flags)
                (when (member dangerous-flag flags)
                  (throw 'dangerous t))))

            ;; Check if any flag contains dangerous substring
            (when pattern-any-contains
              (dolist (flag flags)
                (dolist (dangerous-substr pattern-any-contains)
                  (when (string-match-p dangerous-substr flag)
                    (throw 'dangerous t))))))))
      nil)))

(provide 'bash-parser)
;;; bash-parser.el ends here
