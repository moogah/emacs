;; -*- lexical-binding: t; -*-

(use-package ghostel
  ;; ghostel is not in the recipe repos straight knows about (MELPA et al.),
  ;; so `:straight t' fails to resolve it. Point at the upstream repo
  ;; explicitly. The Elisp lives in a `lisp/' subdirectory, so :files is
  ;; required — straight's default only globs *.el at the repo root.
  :straight (ghostel :type git :host github :repo "dakra/ghostel"
                     :files ("lisp/*.el"))
  :commands (ghostel ghostel-compile)
  :custom
  ;; Scrollback retained per terminal, in BYTES (default ~5 MB ≈ 5k lines).
  ;; Bumped for reviewing long build/test/agent output.
  (ghostel-max-scrollback (* 20 1024 1024))
  :config
  ;; Kill the buffer automatically when the underlying shell process exits,
  ;; so finished terminals don't linger.
  (setq ghostel-kill-buffer-on-exit t))

;; Start ghostel buffers in Emacs state so keystrokes pass straight to the shell.
(with-eval-after-load 'ghostel
  (when (featurep 'evil)
    (evil-set-initial-state 'ghostel-mode 'emacs)))

(global-set-key (kbd "C-c g") #'ghostel)
