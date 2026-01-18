;; ===============================================================================
;; Configure Corfu Auto-Completion
;; ===============================================================================
(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :init)
;;  (global-corfu-mode))

(use-package mini-frame
  :straight t
  :config
  (setq mini-frame-mode t)
  (setq resize-mini-frames t))

(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 80))))

;; ===============================================================================
;; Configure Company Auto-Completion
;; ===============================================================================
;; also try out https://github.com/minad/corfu
(use-package company
  :straight t
  ;;:bind(:map company-active-map
  ;;           ([return] . nil)
  ;;           ("RET" . nil))
  :config
  (company-mode)
  (add-hook 'after-init-hook 'global-company-mode))



;; use C-RET to cancel comletion

;; ===============================================================================
;; Configure Vertico and Orderless
;; ===============================================================================

(defun enable-vertico ()
  (use-package vertico
    :straight (vertico :host github :repo "minad/vertico") ; later versions require emacs 29
    :init
    (vertico-mode)
    (setq vertico-cycle t))

  (use-package consult
    
    :straight (consult :host github :repo "minad/consult") ; later versions require emacs 29

    :bind (
	   ("C-x b" . consult-buffer)
	   ("C-s" . consult-line))
    :config
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source))

  (use-package consult-projectile
    :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

  (use-package marginalia
    
    :straight (marginalia :host github :repo "minad/consult/marginalia") ; later versions require emacs 29

    :bind (
	   :map minibuffer-local-map
		("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))

  (recentf-mode 1)

  (use-package embark
    :straight t
    :bind
    (("C-." . embark-act)) ;; @TODO this is overwritten by evil mode
    :init
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		   nill
		   (window-parameters (mode-line-format . none)))))

  (use-package embark-consult
    :straight t
    :after (embark consult)
    :demand t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))
  
  (use-package savehist
    :straight t
    :init
    (savehist-mode))
  
  (use-package orderless
    :straight t
    :init
    (setq completion-styles '(orderless basic)
          comletion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion))))))
(enable-vertico)
