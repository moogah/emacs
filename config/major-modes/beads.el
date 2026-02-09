;; -*- lexical-binding: t; -*-

;; Core beads.el setup
(use-package beads
  :straight (beads :type git :host codeberg :repo "ctietze/beads.el")
  :commands (beads beads-list beads-new)
  :config
  ;; Configure beads database location if needed
  ;; By default, beads looks for .beads/beads.db in the project root
  ;; This can be customized per-project
  )

;; Additional beads configuration can go here
;; Examples:
;; - Custom keybindings
;; - Integration with projectile or other tools
;; - Custom faces or display options
