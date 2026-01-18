;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Modern Completion System Integration with Error Handling
;; ===============================================================================

;; Load individual completion components with error handling
(jf/load-module (expand-file-name "core/completion/vertico.el" jf/emacs-dir))
(jf/load-module (expand-file-name "core/completion/consult.el" jf/emacs-dir))
(jf/load-module (expand-file-name "core/completion/marginalia.el" jf/emacs-dir))
(jf/load-module (expand-file-name "core/completion/embark.el" jf/emacs-dir))
(jf/load-module (expand-file-name "core/completion/corfu-company.el" jf/emacs-dir))
(jf/load-module (expand-file-name "core/completion/mini-frame.el" jf/emacs-dir))
