;; ===============================================================================
;; Configure Hydra
;; ===============================================================================

(use-package hydra
  :straight t)

(defhydra hydra-main (:exit t)
  "
Global Mode Launcher

_P_: Projectile      _R_: rgrep
_r_: Org-Roam        _d_: dogears
_a_: Org-Agenda
_c_: Consult
"
  ("P" hydra-projectile/body nil)
  ("r" hydra-roam/body nil)
  ("a" hydra-agenda/body nil)
  ("c" hydra-consult/body nil)
  ("R" rgrep nil)
  ("d" hydra-dogears/body nil)
  ("q" nil "cancel"))

(defhydra hydra-projectile (:exit t)
  "
Projectile Mode

_t_: run tests
_k_: kill proj buffers
"
  ("t" projectile-test-project nil)
  ("k" projectile-kill-buffers nil)
  ("H" hydra-main/body "Home")
  ("q" nil "cancel"))

(defhydra hydra-roam (:exit t)
  "
Org-Roam

_r_: add ref
_t_: toggle buffer
"
  ("r" org-roam-ref-add nil)
  ("t" org-roam-buffer-toggle nil)
  ("H" hydra-main/body nil)
  ("q" nil "cancel"))

(defhydra hydra-agenda (:exit t)
  "
Org-Agenda
"
  ("H" hydra-main/body "Home")
  ("q" nil "cancel"))

(defhydra hydra-consult (:exit t)
  "
Consult

_r_: ripgrep       _w_: open buffer in another window
_i_: imenu         _f_: open buffer in another frame
_b_: bookmark
_y_: yank
"
  ("r" consult-ripgrep nil)
  ("i" consult-imenu-multi nil)
  ("b" consult-bookmark nil)
  ("y" consult-yank-from-kill-ring nil)
  ("w" consult-buffer-other-window nil)
  ("f" consult-buffer-other-frame nil)
  ("H" hydra-main/body "Home")
  ("q" nil "cancel"))

(define-key global-map (kbd "C-c h") 'hydra-main/body)

(defhydra hydra-dired (:exit t)
  "
Dired Mode

_b_: bookmarks
"
  ("b" dirvish-bookmark-jump nil)
  ("H" hydra-main/body "Home")
  ("q" nil "cancel"))

(defhydra hydra-dogears (:exit nil)
  "
Dogears

_f_: forward
_b_: back
"
  ("f" dogears-forward nil)
  ("b" dogears-back nil)
  ("H" hydra-main/body nil)
  ("q" nil "cancel"))

(define-key dired-mode-map (kbd "C-c h") 'hydra-dired/body)
;;(define-key org-mode-map (kbd "C-c h") 'hydra-roam/body) ;;
