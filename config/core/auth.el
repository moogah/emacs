;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure EasyPG
;; ===============================================================================

;; Set authentication source (use encrypted GPG file)
(setq auth-sources '("~/.authinfo.gpg"))

;; Don't ask which key to use when encrypting
(setq epa-file-select-keys nil)

;; Encrypt to this key by default
(setq epa-file-encrypt-to '("moogah@gmail.com"))

;; Fix EasyPG error in Emacs 25+
;; From https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
;; and https://www.bytedude.com/gpg-in-emacs/
(defvar epa-pinentry-mode)
(setq epa-pinentry-mode 'loopback)
