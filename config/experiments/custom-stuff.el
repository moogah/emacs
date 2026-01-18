(defun create-narrowed-window ()
  "Create a new window with a narrowed view of the current function or heading."
  (interactive)
  (let ((filetype (file-name-extension (buffer-file-name)))
        (filename (buffer-file-name))
        (start nil)
        (end nil))
    (save-excursion
      (cond
       ;; Python
       ((string-equal filetype "py")
        (beginning-of-defun)
        (setq start (point))
        (end-of-defun)
        (setq end (point)))

       ;; JavaScript
       ((or (string-equal filetype "js") (string-equal filetype "jsx"))
        (js2-mode)
        (js2-beginning-of-defun)
        (setq start (point))
        (js2-end-of-defun)
        (setq end (point)))

       ;; Org mode
       ((string-equal filetype "org")
        (org-back-to-heading t)
        (setq start (point))
        (org-forward-heading-same-level 1 t)
        (setq end (point)))

       (t
        (error "File type not supported"))))

    ;; Create new window with narrowed view
    (clone-indirect-buffer "buffer-name" t) ; add NEWNAME t ie: my-function-name t to name the buffer
    (narrow-to-region start end)))
