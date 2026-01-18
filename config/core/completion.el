;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Modern Completion System Integration with Error Handling
;; ===============================================================================

;; Load individual completion components with error handling
(jf/load-module (expand-file-name "config/core/completion/vertico.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/core/completion/consult.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/core/completion/marginalia.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/core/completion/embark.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/core/completion/corfu-company.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/core/completion/mini-frame.el" jf/emacs-dir))
