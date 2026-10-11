;;; dispatcher.el --- Agent call dispatcher -*- lexical-binding: t; -*-

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

(provide 'emacs-agent-dispatcher)
;;; dispatcher.el ends here
