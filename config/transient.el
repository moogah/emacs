;; Debug: Check state before loading transient
(message "=== TRANSIENT DEBUG ===")
(message "Before: transient loaded? %s" (featurep 'transient))
(when (featurep 'transient)
  (message "Before: transient-version = %s"
           (if (boundp 'transient--version) transient--version "undefined")))

(use-package transient
  :straight t  ; Use latest version instead of old v0.6.0
  :demand t    ; Force immediate load to override built-in version
  :config
  (message "After: transient loaded from %s"
           (locate-library "transient"))
  (message "After: transient-version = %s"
           (if (boundp 'transient--version) transient--version "undefined"))
  (message "After: has transient--set-layout? %s"
           (fboundp 'transient--set-layout))
  (message "=== END TRANSIENT DEBUG ==="))

(use-package transient-showcase
  :straight '(transient-showcase :type git :host github :repo "positron-solutions/transient-showcase"))
