;;; bash-commands-index.el --- Auto-discovery for command handlers -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, commands, discovery
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Zero-registration command handler loading.  Scans the commands/
;; directory for .el files, loads each one with error isolation so that
;; a failure in one handler file does not prevent others from loading.

;;; Code:

(defun jf/bash-commands--discover-and-load ()
  "Discover and load all command handler files in the commands directory.

Scans `config/bash-parser/commands/' for .el files, skips index.el
itself, and loads each file wrapped in `condition-case' for error
isolation.  A failing file is logged via `message' but does not
prevent the remaining files from loading.

Returns a list of successfully loaded file paths."
  (let* ((commands-dir (expand-file-name "config/bash-parser/commands" jf/emacs-dir))
         (el-files (when (file-directory-p commands-dir)
                     (directory-files commands-dir t "\\.el$")))
         (loaded nil))
    (dolist (file el-files)
      (unless (string= (file-name-nondirectory file) "index.el")
        (condition-case err
            (progn
              (load file nil t)
              (push file loaded))
          (error
           (message "bash-commands-index: Error loading %s: %s"
                    (file-name-nondirectory file)
                    (error-message-string err))))))
    (nreverse loaded)))

;; Auto-trigger discovery on load
(jf/bash-commands--discover-and-load)

(provide 'bash-commands-index)
;;; bash-commands-index.el ends here
