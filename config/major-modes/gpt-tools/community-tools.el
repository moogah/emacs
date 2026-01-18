;; -*- lexical-binding: t; -*-

;; Set confirmation mode to 'auto - let tools and agents decide
;; - 'auto: Respect per-tool :confirm and per-agent confirm-tool-calls settings
;; - t: Always confirm all tools
;; - nil: Never confirm any tools
(setq gptel-confirm-tool-calls 'auto)
(setq gptel-use-tools t)

(use-package gptel-got
  :straight '(gptel-got :type git
                         :host nil
                         :repo "https://codeberg.org/bajsicki/gptel-got")
  :after (gptel org-ql)
  :demand t
  :config
  ;; Integrate with org-roam and daily journal workflows
  ;; Tools will respect gptel-confirm-tool-calls for safety
  )

(use-package llm-tool-collection
  :straight '(llm-tool-collection :type git
                                   :host github
                                   :repo "skissue/llm-tool-collection")
  :after gptel
  :demand t
  :config
  ;; Confirmation enabled for destructive operations
  ;; Note: Some overlap with gptel-agent filesystem tools
  )

(use-package ragmacs
  :straight '(ragmacs :type git
                       :host github
                       :repo "positron-solutions/ragmacs")
  :after gptel
  :demand t
  :config
  ;; Enable recursive tool calling for deep introspection
  ;; Tools will respect global confirmation settings
  )
