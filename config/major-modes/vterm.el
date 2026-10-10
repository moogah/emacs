;; -*- lexical-binding: t; -*-

(unless (executable-find "cmake")
  (display-warning
   'vterm
   (concat "vterm needs `cmake' to build its native module, but none was "
           "found on PATH. Install it (macOS: `brew install cmake'; "
           "Debian/Ubuntu: `sudo apt-get install cmake build-essential') "
           "before the package compiles.")
   :warning))

(use-package vterm
  :straight t
  :commands (vterm vterm-other-window)
  :custom
  ;; Lines of scrollback to retain (default 1000). Higher is handy for
  ;; reviewing long build/test output.
  (vterm-max-scrollback 10000)
  ;; Let programs that emit OSC 51 / directory-tracking sequences (configured
  ;; in the shell) update Emacs' notion of the buffer's default-directory.
  (vterm-buffer-name-string "vterm %s")
  :config
  ;; Kill the buffer automatically when the underlying shell process exits,
  ;; so finished terminals don't linger.
  (setq vterm-kill-buffer-on-exit t))

;; Start vterm buffers in Emacs state so keystrokes pass straight to the shell.
(with-eval-after-load 'vterm
  (when (featurep 'evil)
    (evil-set-initial-state 'vterm-mode 'emacs))
  (when (featurep 'evil-collection)
    (evil-collection-init 'vterm)))

(global-set-key (kbd "C-c v") #'vterm)
