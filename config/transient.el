(use-package transient
  :straight t  ; Use latest version instead of old v0.6.0
  :demand t)   ; Force immediate load to override built-in version

(use-package transient-showcase
  :straight '(transient-showcase :type git :host github :repo "positron-solutions/transient-showcase"))
