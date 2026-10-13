;;; manifest.el --- The agent tool manifest -*- lexical-binding: t; -*-

(require 'seq)

(declare-function gptel-tool-name "gptel-request" (tool))
(declare-function gptel-tool-category "gptel-request" (tool))
(declare-function gptel-tool-description "gptel-request" (tool))
(declare-function gptel-tool-args "gptel-request" (tool))
(declare-function jf/agent-preset-tool-names "presets" (preset))
(declare-function jf/agent-tool-tier "presets" (name))
(declare-function jf/agent--registry-lookup "dispatcher" (name))

(defun jf/agent--manifest-text (value)
  "Return VALUE as manifest text: a string as is, a symbol its name, nil \"\"."
  (cond ((stringp value) value)
        ((null value) "")
        ((symbolp value) (symbol-name value))
        (t (format "%s" value))))

(defun jf/agent--manifest-arg (arg)
  "Return the manifest object of the gptel argument plist ARG."
  (list :name (jf/agent--manifest-text (plist-get arg :name))
        :type (jf/agent--manifest-text (plist-get arg :type))
        :optional (if (plist-get arg :optional) t :false)
        :description (jf/agent--manifest-text (plist-get arg :description))))

(defun jf/agent--manifest-entry (tool)
  "Return the manifest object of the gptel tool TOOL."
  (let ((name (gptel-tool-name tool)))
    (list :name name
          :category (or (gptel-tool-category tool) "misc")
          :tier (jf/agent-tool-tier name)
          :description (jf/agent--manifest-text (gptel-tool-description tool))
          :args (vconcat (mapcar #'jf/agent--manifest-arg (gptel-tool-args tool))))))

(defun jf/agent--manifest-names ()
  "Return the agent preset tool names: the read preset's, then the write
preset's not already listed."
  (seq-uniq (append (jf/agent-preset-tool-names 'emacs-agent-read)
                    (jf/agent-preset-tool-names 'emacs-agent-write))))

(defun jf/agent-api ()
  "Return the manifest of the tools an agent may call, as a JSON array string.
One object per agent preset tool name that gptel's registry holds, the
`emacs-agent-read' names first:
{\"name\", \"category\", \"tier\", \"description\",
 \"args\": [{\"name\", \"type\", \"optional\", \"description\"}]}.
TIER is \"read\" or \"write\" (`jf/agent-tool-tier').  A preset name no
tool is registered under is left out.  Built from the presets and the
registry at each call.  The string is `json-serialize's unibyte UTF-8,
not decoded, as the dispatcher's envelope is."
  (json-serialize
   (vconcat (delq nil (mapcar (lambda (name)
                                (let ((tool (jf/agent--registry-lookup name)))
                                  (and tool (jf/agent--manifest-entry tool))))
                              (jf/agent--manifest-names))))))

(provide 'emacs-agent-manifest)
;;; manifest.el ends here
