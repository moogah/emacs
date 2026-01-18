;; Machine-specific configuration for Jeffs-MBP
;; Overrides for auth settings

;; Use the MacGPG2 version which was used to encrypt the .authinfo.gpg file
(setq epg-gpg-program "/usr/local/MacGPG2/bin/gpg")

;; Allow Emacs to prompt for the passphrase
(setq epa-pinentry-mode 'loopback)

;; Ensure the exec path includes MacGPG2 for GUI Emacs
(when (file-exists-p "/Applications/Emacs.app")
  (add-to-list 'exec-path "/usr/local/MacGPG2/bin"))