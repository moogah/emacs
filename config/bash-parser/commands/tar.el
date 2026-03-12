;;; tar.el --- Tar command handler for semantic extraction -*- lexical-binding: t; -*-

;; Filesystem handler for tar command.
;; Mode-dependent: behavior changes based on -c, -x, -t flags.

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-tar--detect-mode (flags)
  "Detect tar mode from FLAGS list.
Returns :create, :extract, or :list, or nil if no mode detected."
  (let ((mode nil))
    (dolist (flag flags)
      (cond
       ((or (equal flag "-c") (and (string-prefix-p "-" flag)
                                    (not (string-prefix-p "--" flag))
                                    (string-match-p "c" flag)))
        (setq mode :create))
       ((or (equal flag "-x") (and (string-prefix-p "-" flag)
                                    (not (string-prefix-p "--" flag))
                                    (string-match-p "x" flag)))
        (setq mode :extract))
       ((or (equal flag "-t") (and (string-prefix-p "-" flag)
                                    (not (string-prefix-p "--" flag))
                                    (string-match-p "t" flag)))
        (setq mode :list))))
    mode))

(defun jf/bash-command-tar--find-archive (args flags)
  "Find the archive file from ARGS and FLAGS.
Looks for -f flag (standalone or combined) and returns the next argument."
  (let ((archive-file nil)
        (i 0))
    (while (and (< i (length args)) (not archive-file))
      (let ((arg (nth i args)))
        (cond
         ;; Standalone -f flag
         ((equal arg "-f")
          (when (< (1+ i) (length args))
            (setq archive-file (nth (1+ i) args))))
         ;; Combined flag containing f (like -czf, -xvf)
         ((and (string-prefix-p "-" arg)
               (not (string-prefix-p "--" arg))
               (string-match-p "f" arg))
          (when (< (1+ i) (length args))
            (setq archive-file (nth (1+ i) args))))))
      (setq i (1+ i)))
    archive-file))

(defun jf/bash-command-tar--filesystem-handler (parsed-command)
  "Extract filesystem operations from tar PARSED-COMMAND.
Mode flags determine operation types:
  -c (create): archive is :write, positional args are :read
  -x (extract): archive is :read, positional args are :write
  -t (list): archive is :read only
Returns plist with :domain, :operations, :claimed-token-ids, :metadata or nil."
  (let* ((flags (plist-get parsed-command :flags))
         (positional-args (plist-get parsed-command :positional-args))
         (args (plist-get parsed-command :args))
         (command "tar")
         (mode (jf/bash-command-tar--detect-mode (or flags args)))
         (archive-file (jf/bash-command-tar--find-archive (or args '()) (or flags '())))
         (operations nil))
    (when (and mode archive-file)
      ;; Archive file operation
      (push (list :file archive-file
                  :operation (if (eq mode :create) :write :read)
                  :command command)
            operations)
      ;; Positional args (source/extracted files)
      (when (and positional-args (not (eq mode :list)))
        (dolist (file positional-args)
          (unless (equal file archive-file)
            (push (list :file file
                        :operation (if (eq mode :create) :read :write)
                        :command command)
                  operations))))
      (when operations
        (list :domain :filesystem
              :operations (nreverse operations)
              :claimed-token-ids nil
              :metadata nil)))))

(jf/bash-register-command-handler
 :command "tar"
 :domain :filesystem
 :handler #'jf/bash-command-tar--filesystem-handler)

(provide 'bash-command-tar-handler)
;;; tar.el ends here
