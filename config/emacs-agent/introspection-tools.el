;;; introspection-tools.el --- Read-only Emacs introspection gptel tools -*- lexical-binding: t; -*-

(require 'find-func)
(require 'seq)

(declare-function gptel-make-tool "gptel-request" (&rest slots))

(defun jf/agent-introspection--symbol (name)
  "Return (SYMBOL) for the interned symbol named NAME, or nil.
NAME is a string; it is looked up with `intern-soft', so nothing is
interned.  Nil when NAME is not a string or no symbol has that name."
  (when (stringp name)
    (let ((symbol (intern-soft name)))
      (and (or symbol (equal name "nil")) (list symbol)))))

(defun jf/agent-introspection--kind (kind)
  "Return the optional argument KIND as a non-empty string, or nil."
  (and (stringp kind) (not (string-empty-p kind)) kind))

(defun jf/agent-introspection--bad-kind (kind kinds)
  "Explanatory result for the KIND argument, which is none of KINDS."
  (format "Unknown kind %S: the kind is one of %s, or omitted"
          kind (mapconcat #'identity kinds ", ")))

(defun jf/agent-introspection--bad-name (name)
  "Explanatory result for a NAME argument that is not a string."
  (format "The name must be a string, not %S" name))

(defconst jf/agent-introspection--max-chars 20000
  "Characters of a value or source text a tool returns at most.")

(defconst jf/agent-introspection--max-names 300
  "Symbol names `emacs_apropos' returns at most.")

(defun jf/agent-introspection--cap (text)
  "Return TEXT, cut at `jf/agent-introspection--max-chars' with a note."
  (if (<= (length text) jf/agent-introspection--max-chars)
      text
    (format "%s\n... [truncated: %d of %d characters shown]"
            (substring text 0 jf/agent-introspection--max-chars)
            jf/agent-introspection--max-chars (length text))))

(defun jf/agent-introspection--prin1 (value)
  "Return the `prin1' text of VALUE, bounded.
Shared and circular structure prints with #N= labels; a list shows at
most 1000 elements and 30 levels; the text stops after
`jf/agent-introspection--max-chars' characters, followed by a note."
  (let ((chars nil)
        (count 0)
        (truncated nil))
    (catch 'jf/agent-introspection--full
      (let ((print-circle t)
            (print-length 1000)
            (print-level 30)
            (print-escape-newlines nil))
        (prin1 value
               (lambda (char)
                 (when (>= count jf/agent-introspection--max-chars)
                   (setq truncated t)
                   (throw 'jf/agent-introspection--full nil))
                 (push char chars)
                 (setq count (1+ count))))))
    (concat (apply #'string (nreverse chars))
            (and truncated
                 (format "\n... [truncated after %d characters]" count)))))

(defun jf/agent-introspection--function-kind (symbol)
  "Describe what kind of function SYMBOL's definition is."
  (cond ((special-form-p symbol) "a special form")
        ((macrop symbol) "a macro")
        ((commandp symbol) "an interactive command")
        ((autoloadp (indirect-function symbol)) "an autoloaded function, not loaded yet")
        (t "a function")))

(defun jf/agent-introspection--usage (symbol)
  "Return the usage line \"(SYMBOL ARGS...)\" of the function SYMBOL."
  (let ((arglist (condition-case nil (help-function-arglist symbol t) (error nil))))
    (cond ((listp arglist)
           (format "(%s)" (mapconcat (lambda (arg) (format "%s" arg))
                                     (cons symbol arglist) " ")))
          ((stringp arglist) arglist)
          (t "(arglist unknown)"))))

(defun jf/agent-introspection--describe-function (symbol)
  "Return the description of the function SYMBOL."
  (let* ((doc (condition-case err (documentation symbol)
                (error (format "(its documentation cannot be read: %s)"
                               (error-message-string err)))))
         (split (and (stringp doc) (help-split-fundoc doc symbol)))
         (usage (or (car split) (jf/agent-introspection--usage symbol)))
         (body (if split (cdr split) doc)))
    (format "%s is %s.\n\n%s\n\n%s"
            symbol (jf/agent-introspection--function-kind symbol) usage
            (if (and (stringp body) (not (string-blank-p body)))
                (string-trim body)
              "It has no documentation."))))

(defun jf/agent-introspection--describe-variable (symbol)
  "Return the description of the variable SYMBOL."
  (let ((doc (condition-case err (documentation-property symbol 'variable-documentation)
               (error (format "(its documentation cannot be read: %s)"
                              (error-message-string err))))))
    (format "%s is a variable%s.\n\n%s"
            symbol
            (if (default-boundp symbol) "" " with no global value")
            (if (and (stringp doc) (not (string-blank-p doc)))
                (string-trim doc)
              "It has no documentation."))))

(defun jf/agent-introspection--variable-p (symbol)
  "Non-nil when SYMBOL is a bound or documented variable."
  (or (boundp symbol) (get symbol 'variable-documentation)))

(defun jf/agent-introspection-describe-symbol (name)
  "Return the documentation of the function and/or variable named NAME.
A function part gives the kind of function, its usage line and its
docstring; a variable part gives its docstring.  A NAME that names
neither gets a result saying it is not defined."
  (if (not (stringp name))
      (jf/agent-introspection--bad-name name)
    (let* ((found (jf/agent-introspection--symbol name))
           (symbol (car found))
           (parts (and found
                       (delq nil
                             (list (and (fboundp symbol)
                                        (jf/agent-introspection--describe-function symbol))
                                   (and (jf/agent-introspection--variable-p symbol)
                                        (jf/agent-introspection--describe-variable symbol)))))))
      (if parts
          (jf/agent-introspection--cap (mapconcat #'identity parts "\n\n----\n\n"))
        (format "%s is not defined: no function or variable has that name" name)))))

(defun jf/agent-introspection--source-file (symbol kind)
  "Return the source file defining SYMBOL as KIND (`function' or `variable').
A string: the absolute name of the source file.  `C' when SYMBOL is a
primitive or a variable defined in C, nil when no file is recorded."
  (let* ((definition (and (eq kind 'function) (indirect-function symbol)))
         (file (or (symbol-file symbol (if (eq kind 'function) 'defun 'defvar))
                   (and (autoloadp definition) (nth 1 definition)))))
    (cond
     ((stringp file)
      (condition-case nil (find-library-name file)
        (error (and (file-readable-p file) file))))
     ((or (subrp definition)
          (and (eq kind 'variable) (integerp (get symbol 'variable-documentation))))
      'C))))

(defun jf/agent-introspection--definition-regexp (symbol kind)
  "Return the regexp locating SYMBOL's definition as KIND, or nil."
  (let ((regexp-symbol (cdr (assq (if (eq kind 'function) nil 'defvar)
                                  find-function-regexp-alist))))
    (and regexp-symbol (boundp regexp-symbol)
         (stringp (symbol-value regexp-symbol))
         (format (symbol-value regexp-symbol)
                 (concat "\\\\?" (regexp-quote (symbol-name symbol)))))))

(defun jf/agent-introspection--locate (symbol kind file)
  "Return (LINE . SOURCE) of SYMBOL's KIND definition in FILE, or nil.
FILE is read into a temporary buffer; no buffer visits it.  As in
`find-function-search-for-symbol', a symbol defined as part of another
definition (its `definition-name' property) is searched for by that name."
  (while (and (symbolp symbol) (get symbol 'definition-name))
    (setq symbol (get symbol 'definition-name)))
  (with-temp-buffer
    (insert-file-contents file)
    (with-syntax-table emacs-lisp-mode-syntax-table
      (let ((case-fold-search nil)
            (regexp (jf/agent-introspection--definition-regexp symbol kind)))
        (goto-char (point-min))
        (when (or (and regexp (re-search-forward regexp nil t))
                  (re-search-forward
                   (concat "^([^ ]+" find-function-space-re "['(]?"
                           (regexp-quote (symbol-name symbol)) "\\_>")
                   nil t))
          (beginning-of-line)
          (let ((start (point)))
            (cons (line-number-at-pos start t)
                  (buffer-substring-no-properties
                   start
                   (condition-case nil
                       (progn (forward-sexp) (point))
                     (error (line-end-position)))))))))))

(defun jf/agent-introspection--definition-kind (symbol kind)
  "Return the kind of definition to find for SYMBOL: `function', `variable' or nil.
KIND is the tool's argument, already normalised: nil prefers a function."
  (pcase kind
    ("function" (and (fboundp symbol) 'function))
    ("variable" (and (jf/agent-introspection--variable-p symbol) 'variable))
    (_ (cond ((fboundp symbol) 'function)
             ((jf/agent-introspection--variable-p symbol) 'variable)))))

(defun jf/agent-introspection-find-definition (name &optional kind)
  "Return where the function or variable named NAME is defined.
KIND is \"function\" or \"variable\"; omitted, a function definition is
preferred.  The first line is FILE:LINE; the definition's source
follows.  A name with no such definition gets an explanatory result."
  (let ((kind (jf/agent-introspection--kind kind)))
    (cond
     ((not (stringp name)) (jf/agent-introspection--bad-name name))
     ((and kind (not (member kind '("function" "variable"))))
      (jf/agent-introspection--bad-kind kind '("function" "variable")))
     (t
      (let* ((found (jf/agent-introspection--symbol name))
             (symbol (car found))
             (as (and found (jf/agent-introspection--definition-kind symbol kind)))
             (what (or kind "function or variable"))
             (file (and as (jf/agent-introspection--source-file symbol as))))
        (cond
         ((not as) (format "%s is not defined as a %s" name what))
         ((eq file 'C)
          (format "%s is a %s defined in Emacs's C source; there is no Lisp file to point to"
                  name as))
         ((not file)
          (format "%s is a %s with no recorded source file (it was defined by evaluation, not by loading a file)"
                  name as))
         (t
          (let ((location (condition-case nil
                              (jf/agent-introspection--locate symbol as file)
                            (error nil))))
            (if location
                (jf/agent-introspection--cap
                 (format "%s:%d\n\n%s" file (car location) (cdr location)))
              (format "%s\n\nThe %s %s is defined in this file, but its definition form was not found in it (a macro probably generates it)"
                      file as name))))))))))

(defun jf/agent-introspection-variable-value (name)
  "Return the global value of the variable named NAME, printed readably.
The text is bounded (see `jf/agent-introspection--prin1').  A NAME with
no global value gets a result saying it is not defined."
  (if (not (stringp name))
      (jf/agent-introspection--bad-name name)
    (let* ((found (jf/agent-introspection--symbol name))
           (symbol (car found)))
      (if (and found (default-boundp symbol))
          (jf/agent-introspection--prin1 (default-value symbol))
        (format "%s is not defined as a variable: it has no global value" name)))))

(defconst jf/agent-introspection--apropos-kinds
  `(("function" . fboundp)
    ("variable" . boundp)
    ("command" . commandp))
  "The kinds `emacs_apropos' accepts, each with its predicate.")

(defun jf/agent-introspection-apropos (pattern &optional kind)
  "Return the symbol names matching the regexp PATTERN, one per line.
KIND is \"function\", \"variable\" or \"command\"; omitted, functions and
variables both match.  No match, or an invalid PATTERN, gets an
explanatory result."
  (let* ((kind (jf/agent-introspection--kind kind))
         (predicate (if kind
                        (cdr (assoc kind jf/agent-introspection--apropos-kinds))
                      (lambda (symbol) (or (fboundp symbol) (boundp symbol))))))
    (cond
     ((not (and (stringp pattern) (not (string-empty-p pattern))))
      (format "The pattern must be a non-empty string, not %S" pattern))
     ((not predicate)
      (jf/agent-introspection--bad-kind
       kind (mapcar #'car jf/agent-introspection--apropos-kinds)))
     (t
      (condition-case err
          (let* ((names (mapcar #'symbol-name (apropos-internal pattern predicate)))
                 (shown (seq-take names jf/agent-introspection--max-names))
                 (more (- (length names) (length shown))))
            (cond
             ((null names)
              (format "No %s matches the pattern %S" (or kind "function or variable") pattern))
             ((> more 0)
              (format "%s\n... and %d more; narrow the pattern"
                      (mapconcat #'identity shown "\n") more))
             (t (mapconcat #'identity shown "\n"))))
        (invalid-regexp
         (format "The pattern %S is not a valid regexp: %s"
                 pattern (error-message-string err))))))))

(defun jf/agent-introspection-tools-register ()
  "Register the four read-only introspection gptel tools.
They are `emacs_describe_symbol', `emacs_find_definition',
`emacs_variable_value' and `emacs_apropos', in category
\"introspection\".  Requires gptel."
  (gptel-make-tool
   :name "emacs_describe_symbol"
   :function #'jf/agent-introspection-describe-symbol
   :description "Describe an Emacs Lisp symbol: for a function, its kind, usage \
line (arglist) and documentation; for a variable, its documentation.  A name \
that is neither says so.  Read-only."
   :args (list '(:name "name"
                 :type string
                 :description "The symbol's exact name, e.g. \"find-file\" or \"fill-column\"."))
   :category "introspection")
  (gptel-make-tool
   :name "emacs_find_definition"
   :function #'jf/agent-introspection-find-definition
   :description "Find where an Emacs Lisp function or variable is defined.  The \
first line of the result is FILE:LINE; the definition's source text follows.  \
Read-only: no file is opened in a buffer."
   :args (list '(:name "name"
                 :type string
                 :description "The symbol's exact name.")
               '(:name "kind"
                 :type string
                 :optional t
                 :enum ["function" "variable"]
                 :description "Which definition to find; omitted, a function definition is preferred."))
   :category "introspection")
  (gptel-make-tool
   :name "emacs_variable_value"
   :function #'jf/agent-introspection-variable-value
   :description "Return the global value of an Emacs Lisp variable, printed \
readably (as `prin1' prints it; long values are truncated).  Read-only."
   :args (list '(:name "name"
                 :type string
                 :description "The variable's exact name."))
   :category "introspection")
  (gptel-make-tool
   :name "emacs_apropos"
   :function #'jf/agent-introspection-apropos
   :description "List the Emacs Lisp symbols whose names match a regexp, one per \
line, optionally only functions, variables or interactive commands.  Read-only."
   :args (list '(:name "pattern"
                 :type string
                 :description "A regexp matched against symbol names; a plain substring works.")
               '(:name "kind"
                 :type string
                 :optional t
                 :enum ["function" "variable" "command"]
                 :description "Restrict to functions, variables or interactive commands; omitted, functions and variables."))
   :category "introspection"))

(with-eval-after-load 'gptel
  (jf/agent-introspection-tools-register))

(provide 'emacs-agent-introspection-tools)
;;; introspection-tools.el ends here
