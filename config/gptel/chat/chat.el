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

;; Load order is partly a real load-time dependency and partly a
;; readability convention. Emacs resolves function symbols at *call*
;; time, so a load-time dependency only exists when a top-level form in
;; module A references a symbol defined in module B. None of the
;; chat-mode modules currently planned issue cross-module =require= or
;; =declare-function= calls, and references to sibling symbols
;; (=gptel-chat-send=, =gptel-chat--parse-buffer=, etc.) live inside
;; interactive command bodies and transient suffix bodies — i.e.
;; call-time, not load-time. The conventional order below mirrors the
;; data-flow described in architecture.md §Interfaces so the loader
;; reads top-down with the call graph.

;; Notes (from architecture.md §Components / §Interfaces):

;; 1. =mode= declares the major mode and keymap. The =mode= keymap binds
;;    commands defined in later modules; because Emacs resolves symbols
;;    at command-invocation time, those bindings do not require the
;;    target commands to exist at load time. Loading =mode= first is a
;;    readability convention, not a hard requirement.
;; 2. =parser= walks the buffer producing a turn list. Standalone — no
;;    load-time references to other chat-mode modules.
;; 3. =stream= builds the streaming callback; its internal
;;    =gptel-chat--sanitize-chunk= handles delimiter-collision escaping
;;    (see architecture.md §Components). The callback closure references
;;    =parser= helpers only at call time.
;; 4. =send= is the interactive command bound to =C-c C-c=. It calls
;;    into =parser= and =stream= from inside the command body
;;    (call-time).
;; 5. =nav= ordering after =send= is *convention*, not a load-time
;;    dependency. =gptel-chat-regenerate= invokes =gptel-chat-send=
;;    from inside its interactive body, and the navigation commands
;;    call =gptel-chat--parse-buffer= the same way. Architecture.md
;;    §=gptel-chat-nav= explicitly denies coupling to parser internals.
;;    Nav can safely load before send; the order here just mirrors the
;;    "send pipeline first, ergonomic wrappers next" reading order.
;; 6. =display= is presentation-only; safe to load last among the core
;;    modules.
;; 7. =menu= ordering after =send= is also convention. The transient
;;    prefix references =gptel-chat-send= (and the preset-application
;;    hook references parser/send symbols), but transient resolves
;;    suffix command symbols at click time. The convention keeps
;;    "things that wire up other things" after the things they wire.


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
