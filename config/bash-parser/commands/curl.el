;;; curl.el --- Multi-domain command handlers for curl -*- lexical-binding: t; -*-

;;; Commentary:

;; Curl contributes to two semantic domains:
;; - :filesystem - output files (-o/--output), data files (-d @file), upload files (-T)
;; - :network - URL extraction for HTTP/HTTPS endpoints

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-curl--filesystem-handler (parsed-command)
  "Extract filesystem operations from curl commands.

Detects:
  -o/--output FILE → FILE is :write
  -d @FILE / --data @FILE → FILE is :read (strip @ prefix)
  -T/--upload-file FILE → FILE is :read

PARSED-COMMAND is the parsed command plist.
Returns filesystem domain result or nil if no file ops."
  (let* ((args (plist-get parsed-command :args))
         (operations nil)
         (i 0))
    (while (< i (length args))
      (let ((arg (nth i args)))
        (cond
         ;; -o / --output → next arg is write target
         ((or (equal arg "-o") (equal arg "--output"))
          (when (< (1+ i) (length args))
            (push (list :file (nth (1+ i) args)
                        :operation :write
                        :confidence :high
                        :command "curl")
                  operations)
            (setq i (1+ i))))

         ;; -d / --data with @file syntax → file is read
         ((or (equal arg "-d") (equal arg "--data"))
          (when (< (1+ i) (length args))
            (let ((val (nth (1+ i) args)))
              (when (string-prefix-p "@" val)
                (push (list :file (substring val 1)
                            :operation :read
                            :confidence :high
                            :command "curl")
                      operations)))
            (setq i (1+ i))))

         ;; -T / --upload-file → next arg is read source
         ((or (equal arg "-T") (equal arg "--upload-file"))
          (when (< (1+ i) (length args))
            (push (list :file (nth (1+ i) args)
                        :operation :read
                        :confidence :high
                        :command "curl")
                  operations)
            (setq i (1+ i))))))
      (setq i (1+ i)))

    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(defun jf/bash-command-curl--network-handler (parsed-command)
  "Extract network access from curl commands.

Finds URLs in arguments (http:// or https:// prefixes).

PARSED-COMMAND is the parsed command plist.
Returns network domain result or nil if no URLs found."
  (let* ((args (plist-get parsed-command :args))
         (operations nil))
    (dolist (arg args)
      (when (or (string-prefix-p "http://" arg)
                (string-prefix-p "https://" arg))
        (let ((protocol (if (string-prefix-p "https://" arg) :https :http)))
          (push (list :protocol protocol
                      :endpoint arg
                      :command "curl")
                operations))))
    (when operations
      (list :domain :network
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

;; Register both domain handlers
(jf/bash-register-command-handler
 :command "curl" :domain :filesystem :handler #'jf/bash-command-curl--filesystem-handler)
(jf/bash-register-command-handler
 :command "curl" :domain :network :handler #'jf/bash-command-curl--network-handler)

(provide 'bash-command-curl)
;;; curl.el ends here
