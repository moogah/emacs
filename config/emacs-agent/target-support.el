;;; target-support.el --- Target-side observation and control helpers -*- lexical-binding: t; -*-

(defvar jf/frame-state-max-chars 20000
  "Most characters of buffer contents `jf/frame-state' returns.
Contents past this many characters are cut, and the state's buffer
`truncated' is true.")

(defvar jf/frame-state-message-lines 10
  "How many of the last lines of *Messages* `jf/frame-state' returns.")

(defun jf/frame-state--bool (value)
  "VALUE as a JSON boolean for `json-serialize'."
  (if value t :false))

(defun jf/frame-state--buffer (window)
  "The `buffer' object of `jf/frame-state' for WINDOW's buffer."
  (with-current-buffer (window-buffer window)
    (let* ((start (point-min))
           (end (min (point-max)
                     (+ start (max 0 jf/frame-state-max-chars)))))
      (list :name (buffer-name)
            :contents (buffer-substring-no-properties start end)
            :truncated (jf/frame-state--bool (< end (point-max)))
            :point (window-point window)))))

(defun jf/frame-state--windows ()
  "The `windows' array of `jf/frame-state'."
  (vconcat
   (mapcar (lambda (window)
             (list :buffer (buffer-name (window-buffer window))
                   :selected (jf/frame-state--bool (eq window (selected-window)))))
           (window-list nil nil))))

(defun jf/frame-state--minibuffer ()
  "The `minibuffer' object of `jf/frame-state'."
  (let ((window (active-minibuffer-window)))
    (list :active (jf/frame-state--bool window)
          :contents (if window
                        (with-current-buffer (window-buffer window)
                          (minibuffer-contents-no-properties))
                      :null))))

(defun jf/frame-state--messages ()
  "The `messages' array of `jf/frame-state'."
  (let ((buffer (get-buffer "*Messages*")))
    (if (not buffer)
        []
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (forward-line (- (max 0 jf/frame-state-message-lines)))
          (vconcat (split-string (buffer-substring-no-properties (point) (point-max))
                                 "\n" t)))))))

(defun jf/frame-state ()
  "Return the target's observable state as a JSON object string.
The object has:
  buffer      {name, contents, truncated, point} for the selected
              window's buffer; contents are cut to
              `jf/frame-state-max-chars' and truncated is true when cut
  windows     [{buffer, selected}] for the selected frame's windows
  minibuffer  {active, contents}, contents null when not active
  messages    the last `jf/frame-state-message-lines' lines of *Messages*
It needs no frame beyond a daemon's terminal frame.  The string is
`json-serialize's unibyte UTF-8, not decoded."
  (json-serialize
   (list :buffer (jf/frame-state--buffer (selected-window))
         :windows (jf/frame-state--windows)
         :minibuffer (jf/frame-state--minibuffer)
         :messages (jf/frame-state--messages))))

(defun jf/type-text (string)
  "Insert STRING literally at point in the selected window's buffer.
No character of STRING is read as a key: control characters, `C-x'
spelled out and backslashes all go in as written.  Return nil."
  (unless (stringp string)
    (signal 'wrong-type-argument (list #'stringp string)))
  (with-current-buffer (window-buffer (selected-window))
    (insert string))
  nil)

(defun jf/send-keys (keys)
  "Run KEYS, a key sequence in `kbd' notation, as a keyboard macro.
The keys go through the command loop as if typed, in the selected
window, with whatever keymaps and modes are active there.  Behavior
that depends on a real display (redisplay, a terminal's key decoding)
needs a frame attached first, `emacsclient -c' or `-t'.  Return nil."
  (unless (stringp keys)
    (signal 'wrong-type-argument (list #'stringp keys)))
  (execute-kbd-macro (kbd keys))
  nil)

(defun jf/run-command (name)
  "Run the interactive command NAME with `call-interactively'.
NAME is a string or a symbol.  The command runs with the selected
window's buffer current.  Signal a `user-error' when NAME does not name
an interactive command.  Return nil."
  (let ((command (if (symbolp name) name (intern-soft name))))
    (unless (and command (commandp command))
      (user-error "Not an interactive command: %s" name))
    (with-current-buffer (window-buffer (selected-window))
      (call-interactively command))
    nil))

(provide 'emacs-agent-target-support)
;;; target-support.el ends here
