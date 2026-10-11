;;; action-log.el --- Live-session action log -*- lexical-binding: t; -*-

(defvar server-name)

(defgroup jf/agent-log nil
  "The live session's record of agent calls."
  :group 'tools)

(defcustom jf/agent-log-args-max 200
  "Most characters an action-log entry keeps of a call's arguments.
The tool or form field is cut to the same length."
  :type 'natnum
  :group 'jf/agent-log)

(defcustom jf/agent-log-max-lines 1000
  "Most entries `*emacs-agent-log*' keeps; older ones are dropped."
  :type 'natnum
  :group 'jf/agent-log)

(defconst jf/agent-log-buffer-name "*emacs-agent-log*"
  "Name of the action log buffer.")

(defun jf/agent--live-session-p ()
  "Non-nil when this Emacs is the live session: `server-name' is \"server\"."
  (and (boundp 'server-name) (equal server-name "server")))

(defun jf/agent--log-field (object)
  "Return OBJECT as a one-line action-log field.
A string is itself, nil is \"-\", anything else its `prin1' form;
whitespace runs become one space and the text is cut to
`jf/agent-log-args-max' characters."
  (let* ((text (cond ((null object) "-")
                     ((stringp object) object)
                     (t (prin1-to-string object))))
         (line (string-trim (replace-regexp-in-string "[ \t\n\r\f]+" " " text))))
    (if (> (length line) jf/agent-log-args-max)
        (concat (substring line 0 (max 0 (1- jf/agent-log-args-max))) "…")
      line)))

(defun jf/agent--log-trim ()
  "Drop the oldest lines of the current buffer beyond `jf/agent-log-max-lines'."
  (let ((excess (- (count-lines (point-min) (point-max)) jf/agent-log-max-lines)))
    (when (> excess 0)
      (save-excursion
        (goto-char (point-min))
        (forward-line excess)
        (delete-region (point-min) (point))))))

(defun jf/agent-log-call (tier name args outcome)
  "Append one entry for a dispatched call to `*emacs-agent-log*'.
TIER is the tier string, NAME the tool name (for the eval tier, the
form), ARGS the call's arguments (the JSON string, or nil), OUTCOME
non-nil when the call answered ok and nil when it was refused or
failed.  Never displays or selects the buffer; never signals."
  (condition-case nil
      (let ((line (format "%s  %s  %s  %s  %s\n"
                          (format-time-string "%F %T")
                          (jf/agent--log-field tier)
                          (jf/agent--log-field name)
                          (jf/agent--log-field args)
                          (if outcome "ok" "error"))))
        (with-current-buffer (get-buffer-create jf/agent-log-buffer-name)
          (unless (eq buffer-undo-list t)
            (setq buffer-undo-list t
                  buffer-read-only t))
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (insert line)
              (jf/agent--log-trim)))
          (set-buffer-modified-p nil)))
    (error nil)))

(provide 'emacs-agent-action-log)
;;; action-log.el ends here
