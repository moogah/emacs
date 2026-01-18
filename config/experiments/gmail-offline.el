;; Install notmuch package for Emacs
(use-package notmuch
  :ensure t
  :config
  ;; Configure notmuch user information
  (setq user-full-name "Your Full Name"
        user-mail-address "your.email@gmail.com"))

;; For extra functionality with mail composition
(use-package message
  :ensure nil  ;; built into Emacs
  :config
  ;; Configure mail sending
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/opt/homebrew/bin/msmtp"
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header))

;; Basic notmuch setup
(use-package notmuch
  :ensure t
  :bind (("C-c m" . notmuch)
         ("C-c M" . notmuch-mua-new-mail))
  :config
  ;; User information
  (setq user-full-name "Your Full Name"
        user-mail-address "your.email@gmail.com"
        send-mail-function 'sendmail-send-it)

  ;; Notmuch customizations
  (setq notmuch-search-oldest-first nil  ;; Newest emails first
        notmuch-show-logo nil            ;; Remove logo
        notmuch-hello-thousands-separator ","
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                notmuch-hello-insert-recent-searches
                                notmuch-hello-insert-alltags))

  ;; Setup saved searches
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "*" :key "a")
          (:name "last 7 days" :query "date:7d..today" :key "7")))

  ;; Configure sending mail from notmuch
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/opt/homebrew/bin/msmtp"
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header)

  ;; For some UI improvements
  (setq notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-20s ")
          ("subject" . "%-80s ")
          ("tags" . "(%s)")))

  ;; Save attachments to Downloads folder
  (setq mm-default-directory "~/Downloads")

  ;; Enable Evil bindings for notmuch if using Evil Mode
  (with-eval-after-load 'evil-collection
    (evil-collection-init '(notmuch))))

;; Create a function to sync mail
(defun my/sync-mail ()
  "Sync mail using Lieer and update notmuch database."
  (interactive)
  (message "Syncing mail...")
  (let ((default-directory (expand-file-name "~/Mail/gmail/")))
    (call-process "gmi" nil nil nil "sync"))
  (call-process "notmuch" nil nil nil "new")
  (message "Mail sync completed"))

;; Bind a key for syncing mail
(global-set-key (kbd "C-c m s") 'my/sync-mail)

;; Add email address completion via notmuch
(use-package notmuch-address
  :ensure nil  ;; comes with notmuch
  :config
  (setq notmuch-address-command "notmuch-addresses"
        notmuch-address-save-filename (expand-file-name "notmuch-addresses" user-emacs-directory))
  (notmuch-address-message-insinuate))

;; Render HTML emails
(use-package shr
  :ensure nil  ;; built-in
  :config
  (setq shr-color-visible-luminance-min 80
        shr-use-colors nil
        shr-use-fonts nil))

;; Configure notmuch to handle HTML emails
(use-package notmuch
  :ensure t
  :config
  (setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))
  (setq mm-text-html-renderer 'shr))
