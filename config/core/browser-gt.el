;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure browser-gt: browser bridge over WebSocket, Safari via AppleScript
;; ===============================================================================

(use-package websocket
  :straight t)

(use-package browser-gt
  :straight (browser-gt :type git :host github :repo "moogah/browser-gt"
                        :branch "safari-osa")
  :after websocket
  :config
  (require 'browser-gt-tab-manager) ; M-x browser-gt-tab-manager
  (require 'browser-gt-babel)       ; org-babel browser-gt-js blocks
  (require 'browser-gt-url-handler) ; browser-gt-browse-url + browser-gt-url-routes
  (when (eq system-type 'darwin)
    (require 'browser-gt-safari-osa)) ; Safari without an extension
  (browser-gt-start))
