;;; dispatcher.el --- Agent call dispatcher -*- lexical-binding: t; -*-

(require 'seq)

(declare-function gptel-tool-args "gptel-request" (tool))
(declare-function gptel-tool-async "gptel-request" (tool))
(declare-function gptel-tool-function "gptel-request" (tool))
(declare-function gptel--to-string "gptel-request" (s))
(declare-function jf/agent--tier-presets "presets" (tier))
(declare-function jf/agent-preset-tool-names "presets" (preset))

(declare-function gptel-get-tool "gptel-request" (path))
(declare-function gptel-tool-p "gptel-request" (object))

(defun jf/agent--registry-lookup (name)
  "Return the gptel tool registered under the name NAME, or nil.
NAME is a tool name string.  Nil when nothing is registered under NAME,
when NAME names a category rather than a tool, or when gptel is not
loaded.  Never signals."
  (let ((found (and (stringp name)
                    (condition-case nil
                        (gptel-get-tool name)
                      (error nil)))))
    (and found (fboundp 'gptel-tool-p) (gptel-tool-p found) found)))

(defun jf/agent--text (object)
  "Return OBJECT as the text gptel gives the model for it.
A string is itself, a symbol its name, anything else its `prin1' form."
  (cond ((fboundp 'gptel--to-string) (gptel--to-string object))
        ((stringp object) object)
        ((symbolp object) (symbol-name object))
        (t (prin1-to-string object))))

(defun jf/agent--error-text (err)
  "Return the text gptel gives the model for the error ERR.
ERR is a condition, (SYMBOL . DATA); each element becomes text and they
are joined with spaces, as `gptel--handle-tool-use' does:
\=(user-error \"Nope\") gives \"user-error Nope\"."
  (mapconcat #'jf/agent--text err " "))

(defun jf/agent--envelope (ok text)
  "Return the JSON reply for an outcome: success when OK, TEXT its text.
OK non-nil gives {\"ok\":true,\"result\":TEXT}, nil gives
{\"ok\":false,\"error\":TEXT}.  Never signals."
  (let ((encode (lambda (plist)
                  (decode-coding-string (json-serialize plist) 'utf-8))))
    (condition-case nil
        (funcall encode (if ok
                            (list :ok t :result text)
                          (list :ok :false :error text)))
      (error
       (funcall encode
                (list :ok :false
                      :error "The reply is not valid Unicode text and cannot be sent as JSON"))))))

(defun jf/agent--in-presets-p (name presets)
  "Non-nil when the tool name NAME is in one of the gptel presets PRESETS."
  (seq-some (lambda (preset) (member name (jf/agent-preset-tool-names preset)))
            presets))

(defun jf/agent--outside-tier-text (tier name)
  "Refusal text for the tool NAME, which no preset of TIER holds."
  (if (and (equal tier "call")
           (jf/agent--in-presets-p name '(emacs-agent-write)))
      (format "Tool %s changes state: it runs only on the call-write tier" name)
    (format "Tool %s is in no agent preset (emacs-agent-read, emacs-agent-write)" name)))

(define-error 'jf/agent-bad-arguments "Arguments are not a JSON object")

(defun jf/agent--decode-args (json)
  "Return the JSON object string JSON as a plist, as gptel decodes it.
Signal `jf/agent-bad-arguments' when JSON is not a string holding one
JSON object."
  (unless (and (stringp json) (string-match-p "\\`[ \t\n\r]*{" json))
    (signal 'jf/agent-bad-arguments (list (format "%S" json))))
  (condition-case err
      (json-parse-string json :object-type 'plist
                         :null-object nil :false-object :json-false)
    (json-error
     (signal 'jf/agent-bad-arguments (list (error-message-string err))))))

(defun jf/agent--arg-values (tool args)
  "Return the values of the plist ARGS for TOOL, in its argument order.
For each of TOOL's `gptel-tool-args', the value of ARGS under the
keyword :NAME, as `gptel--handle-tool-use' maps them."
  (mapcar (lambda (arg)
            (plist-get args (intern (concat ":" (plist-get arg :name)))))
          (gptel-tool-args tool)))

(defun jf/agent--apply-tool (tool json)
  "Run TOOL on the JSON arguments JSON; return (OK . TEXT)."
  (condition-case err
      (let ((values (jf/agent--arg-values tool (jf/agent--decode-args json))))
        (condition-case err
            (cons t (jf/agent--text (apply (gptel-tool-function tool) values)))
          (error (cons nil (jf/agent--error-text err)))))
    (jf/agent-bad-arguments (cons nil (error-message-string err)))))

(defun jf/agent--call-outcome (tier name json)
  "Return (OK . TEXT): the outcome of running tool NAME on TIER with JSON."
  (let* ((presets (jf/agent--tier-presets tier))
         (allowed (and presets (jf/agent--in-presets-p name presets)))
         (tool (and allowed (jf/agent--registry-lookup name))))
    (cond
     ((null presets)
      (cons nil (format "Unknown tier %S: the tiers are call and call-write" tier)))
     ((not allowed) (cons nil (jf/agent--outside-tier-text tier name)))
     ((null tool)
      (cons nil (format "Tool %s is in an agent preset but no tool of that name is registered with gptel"
                        name)))
     ((gptel-tool-async tool)
      (cons nil (format "Tool %s is asynchronous: asynchronous tools are not supported" name)))
     (t (jf/agent--apply-tool tool json)))))

(declare-function jf/agent--live-session-p "action-log" ())
(declare-function jf/agent-log-call "action-log" (tier name args outcome))

(defun jf/agent--record (tier name args outcome)
  "Log the call NAME on TIER with ARGS and OUTCOME when this is the live session.
OUTCOME is (OK . TEXT); see `jf/agent-log-call'."
  (when (jf/agent--live-session-p)
    (jf/agent-log-call tier name args (car outcome))))

(defun jf/agent-call (tier tool-name json)
  "Run the gptel tool TOOL-NAME on TIER with the JSON object JSON.
TIER is \"call\" (the `emacs-agent-read' tools) or \"call-write\" (those
and the `emacs-agent-write' tools).  JSON's members are passed to the
tool by name, as gptel passes a model's arguments.  Return the JSON
envelope: {\"ok\":true,\"result\":R} or {\"ok\":false,\"error\":E}."
  (let ((outcome (jf/agent--call-outcome tier tool-name json)))
    (jf/agent--record tier tool-name json outcome)
    (jf/agent--envelope (car outcome) (cdr outcome))))

(defun jf/agent-eval (form)
  "Read and evaluate the form string FORM; return the JSON envelope.
The result is the `prin1' form of the value; an error answers with
gptel's text for it."
  (let ((outcome (condition-case err
                     (cons t (prin1-to-string
                              (eval (car (read-from-string form)) t)))
                   (error (cons nil (jf/agent--error-text err))))))
    (jf/agent--record "eval" form nil outcome)
    (jf/agent--envelope (car outcome) (cdr outcome))))

(provide 'emacs-agent-dispatcher)
;;; dispatcher.el ends here
