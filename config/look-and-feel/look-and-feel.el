;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Look and Feel Component Loading
;; ===============================================================================

;; Load basic UI settings
(jf/load-module (expand-file-name "config/look-and-feel/base-ui.el" jf/emacs-dir))

;; Load frame and line number settings
(jf/load-module (expand-file-name "config/look-and-feel/frame-and-lines.el" jf/emacs-dir))

;; Load modeline configuration
(jf/load-module (expand-file-name "config/look-and-feel/modeline.el" jf/emacs-dir))

;; Load theme configuration
(jf/load-module (expand-file-name "config/look-and-feel/themes.el" jf/emacs-dir))

;; Load org-mode faces configuration
(jf/load-module (expand-file-name "config/look-and-feel/org-faces.el" jf/emacs-dir))

;; Load terminal emulation configuration
(jf/load-module (expand-file-name "config/look-and-feel/terminal.el" jf/emacs-dir))

;; Load tab bar styling
(jf/load-module (expand-file-name "config/look-and-feel/vim-tab.el" jf/emacs-dir))

;; Load imenu list configuration
(jf/load-module (expand-file-name "config/look-and-feel/imenu.el" jf/emacs-dir))
