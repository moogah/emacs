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
;; 3. =stream= builds the streaming callback; its internal
;;    =gptel-chat--sanitize-chunk= handles delimiter-collision escaping
;;    (see architecture.md §Components), and it uses =parser=.
;; 4. =send= wires =parser= and =stream= to =gptel-request=.
;; 5. =nav= loads after =send= because =regenerate= is a thin wrapper
;;    over =gptel-chat-send= — it deletes the trailing assistant block
;;    and re-invokes the send entry point. Turn navigation itself uses
;;    =re-search-forward= on block delimiters and does NOT depend on
;;    =parser= internals (architecture.md §gptel-chat-nav explicitly
;;    denies coupling to parser internals).
;; 6. =display= is presentation-only; safe to load last among the core
;;    modules.
;; 7. =menu= defines the transient prefix and the preset-application
;;    hook; it depends on =send= (for the rebound Send suffix).


;; [[file:chat.org::*Feature Module Loading][Feature Module Loading:1]]
(jf/load-module (expand-file-name "config/gptel/chat/mode.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/gptel/chat/parser.el" jf/emacs-dir))
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
