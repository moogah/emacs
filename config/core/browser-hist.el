;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure browser-hist for accessing browser history
;; ===============================================================================

(use-package browser-hist
  :straight (browser-hist :type git :host github :repo "agzam/browser-hist.el")
  :config
  (defun browser-hist-create-org-roam-node (&optional force-update)
    "Create an org-roam node from browser history selection.
With FORCE-UPDATE argument, ensure that the history cache is updated."
    (interactive "P")
    (require 'org-roam)
    
    (when force-update (message "Forcing browser history update"))
    (browser-hist--make-db-copy browser-hist-default-browser force-update)
    
    (unwind-protect
        (let* ((completion-styles '(basic partial-completion))
               (selected-url
                (completing-read "Browser history: " #'browser-hist--completion-table))
               (rows-raw (browser-hist--send-query nil))
               (title (alist-get selected-url rows-raw nil nil #'string=))
               (url selected-url))
          (when (and url title)
            (org-roam-capture-
             :keys "b"
             :node (org-roam-node-create :title title)
             :info (list :ref url)
             :templates org-roam-capture-ref-templates)))
      (and browser-hist--db-connection
           (ignore-errors (sqlite-close browser-hist--db-connection))
           (setq browser-hist--db-connection nil))))

  :bind ("C-c n b" . browser-hist-create-org-roam-node))
