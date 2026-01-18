;; -*- lexical-binding: t; -*-
;; Machine-specific configuration for Mac.home

;; Configure hostname-based daily journal directory
(setq org-roam-dailies-directory "dailies/")

(setq browser-hist-db-paths
      '((safari . "/Users/jefffarr/Library/Safari/History.db")))

(setq browser-hist-default-browser 'safari)

;; Position Emacs frame on the right side of the monitor
;; Override the default left=0 positioning from core/defaults
(let ((display-width (display-pixel-width)))
  (setq default-frame-alist
        (cons `(left . ,(/ display-width 2))
              (assq-delete-all 'left default-frame-alist)))
  (setq initial-frame-alist
        (cons `(left . ,(/ display-width 2))
              (assq-delete-all 'left initial-frame-alist))))
