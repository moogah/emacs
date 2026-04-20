;;; chat.el --- GPTEL Chat-Mode Subsystem Loader -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Loader for the gptel chat-mode subsystem.  Loads the feature
;; modules that together implement `gptel-chat-mode' and its
;; supporting parser, streaming, navigation, display, and menu
;; layers.  See `openspec/changes/gptel-chat-mode/architecture.md'
;; for the component inventory.

;;; Code:

;; Feature Module Loading

;; Load the chat-mode feature modules in dependency order. The loader
;; follows the same pattern as =config/gptel/gptel.org= — serial
;; =jf/load-module= calls against absolute paths resolved from
;; =jf/emacs-dir=.

;; Dependency notes (from architecture.md §Interfaces):

;; 1. =mode= declares the major mode and keymap — every other module
;;    assumes it exists.
;; 2. =parser= walks the buffer; it depends only on =mode=.
;; 3. =sanitize= provides the delimiter-escape helpers used by =stream=.
;; 4. =stream= builds the streaming callback; it uses =sanitize= and
;;    =parser=.
;; 5. =send= wires =parser= and =stream= to =gptel-request=.
;; 6. =nav= consumes the turn list from =parser=; loads after =send= so
;;    its commands can reuse send-path utilities for regenerate.
;; 7. =display= is presentation-only; safe to load last among the core
;;    modules.
;; 8. =menu= defines the transient prefix and the preset-application
;;    hook; it depends on =send= (for the rebound Send suffix).


;; [[file:chat.org::*Feature Module Loading][Feature Module Loading:1]]
(jf/load-module (expand-file-name "config/gptel/chat/mode.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/chat/parser.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/chat/sanitize.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/chat/stream.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/chat/send.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/chat/nav.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/chat/display.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/chat/menu.el" jf/emacs-dir))
;; Feature Module Loading:1 ends here

;; Provide


;; [[file:chat.org::*Provide][Provide:1]]
(provide 'gptel-chat)

;;; chat.el ends here
;; Provide:1 ends here
