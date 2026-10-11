;;; introspection-tools-spec.el --- The owned read-only introspection tools -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D10, D26 and
;; § Seams: seam/introspection-tools.
;;
;; The tools are driven the way an agent reaches them: by name, with JSON
;; arguments, through `jf/agent-call' on the `call' tier.  Each spec cites
;; the scenario of specs/agent-introspection-tools/spec.md it pins.
;; Pending until config/emacs-agent/introspection-tools.org exists.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defun introspection-spec--fixture-function (alpha &optional beta)
  "Fixture docstring of the introspection specs.
ALPHA and BETA are ignored."
  (ignore alpha beta))

(defun introspection-spec--probe-function ()
  "A non-interactive probe for the apropos spec."
  nil)

(defun introspection-spec--probe-command ()
  "An interactive probe for the apropos spec."
  (interactive)
  nil)

(defvar introspection-spec--probe-variable (list "a" 1 'b)
  "A bound probe variable for the variable-value and apropos specs.")

(defun introspection-spec--call (tool json)
  "Call the introspection TOOL with JSON on the call tier; return the envelope."
  (emacs-agent-test-envelope (jf/agent-call "call" tool json)))

(defun introspection-spec--result (tool json)
  "The result text of a successful call of TOOL with JSON."
  (let ((reply (introspection-spec--call tool json)))
    (expect (plist-get reply :ok) :to-be t)
    (plist-get reply :result)))


(describe "Introspection tools (seam/introspection-tools)"

  (it "registers four synchronous tools in the introspection category (seam/introspection-tools)"
    (dolist (name '("emacs_describe_symbol" "emacs_find_definition"
                    "emacs_variable_value" "emacs_apropos"))
      (let ((tool (gptel-get-tool (list "introspection" name))))
        (expect (gptel-tool-name tool) :to-equal name)
        (expect (gptel-tool-async tool) :to-be nil))))

  (it "returns the documentation and arglist of a defined function (Scenario: agent-introspection-tools § Describe a function)"
    (let ((text (introspection-spec--result
                 "emacs_describe_symbol"
                 "{\"name\":\"introspection-spec--fixture-function\"}")))
      (expect text :to-match "Fixture docstring of the introspection specs")
      (expect text :to-match "alpha")
      (expect text :to-match "&optional beta")))

  (it "returns the file and line of a function defined in the configuration (Scenario: agent-introspection-tools § Find where a function is defined)"
    (let* ((text (introspection-spec--result
                  "emacs_find_definition" "{\"name\":\"jf/resolve-module-path\"}"))
           (first-line (car (split-string text "\n"))))
      (expect first-line :to-match "\\`\\(.+\\):\\([0-9]+\\)\\'")
      (string-match "\\`\\(.+\\):\\([0-9]+\\)\\'" first-line)
      (let ((file (match-string 1 first-line))
            (line (string-to-number (match-string 2 first-line))))
        (expect (file-name-nondirectory file) :to-equal "init.el")
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (forward-line (1- line))
          (expect (looking-at-p "(defun jf/resolve-module-path") :to-be t)))))

  (it "visits no file to find a definition (seam/introspection-tools)"
    (let ((before (buffer-list)))
      (introspection-spec--result "emacs_find_definition"
                                  "{\"name\":\"jf/resolve-module-path\"}")
      (expect (seq-difference (buffer-list) before) :to-be nil)))

  (it "returns a bound variable's value as readable text and leaves it unchanged (Scenario: agent-introspection-tools § Read a variable)"
    (let* ((before introspection-spec--probe-variable)
           (copy (copy-tree before))
           (text (introspection-spec--result
                  "emacs_variable_value"
                  "{\"name\":\"introspection-spec--probe-variable\"}")))
      (expect (car (read-from-string text)) :to-equal copy)
      (expect introspection-spec--probe-variable :to-be before)
      (expect introspection-spec--probe-variable :to-equal copy)))

  (it "returns only interactive commands matching the pattern for the kind command (Scenario: agent-introspection-tools § Search symbols by pattern and kind)"
    (let ((names (split-string
                  (introspection-spec--result
                   "emacs_apropos"
                   "{\"pattern\":\"introspection-spec--probe\",\"kind\":\"command\"}")
                  "[\n ]+" t)))
      (expect names :to-equal '("introspection-spec--probe-command"))))

  (it "states that an undefined symbol is not defined, without signalling or interning it (Scenario: agent-introspection-tools § Unknown symbol)"
    (let* ((name (concat "emacs-agent-test-" "no-such-symbol-xyzzy"))
           (text (introspection-spec--result
                  "emacs_describe_symbol" (format "{\"name\":%S}" name))))
      (expect text :to-match "not defined")
      (expect (intern-soft name) :to-be nil)))

  (it "answers softly from every tool for a name that is not defined (seam/introspection-tools)"
    (let ((name (concat "emacs-agent-test-" "absent-plugh")))
      (dolist (tool '("emacs_find_definition" "emacs_variable_value"))
        (expect (introspection-spec--result tool (format "{\"name\":%S}" name))
                :to-match "not defined"))
      (expect (intern-soft name) :to-be nil))))

;;; introspection-tools-spec.el ends here
