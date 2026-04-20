;;; mode.el --- GPTEL Chat-Mode Major Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Defines `gptel-chat-mode', a major mode derived from `org-mode'
;; for multi-turn chat buffers using symmetric #+begin_user /
;; #+begin_assistant special blocks (with nested #+begin_tool blocks
;; inside assistant blocks).  See
;; `openspec/changes/gptel-chat-mode/architecture.md' §Components and
;; `openspec/changes/gptel-chat-mode/design.md' §Decisions 6, 7, 9,
;; 10, 14 for the rationale.
;;
;; This module is intentionally minimal.  It defines the mode, the
;; keymap, and the `gptel-chat-new' command.  Parsing, streaming,
;; sending, navigation, display, and preset-wiring are separate
;; modules that bind onto the mode at call-time.

;;; Code:

;; Forward declarations

;; Keymap entries bind to commands defined in later-loading sibling
;; modules (=send=, =nav=, =display=). Emacs resolves command symbols at
;; call-time, so the bindings work even when the target functions are not
;; yet defined — but =byte-compile= and =check-declare= are happier when
;; we acknowledge the forward reference explicitly.

;; =gptel-abort= lives upstream and is always available once =gptel=
;; itself has loaded (design.md §Decision 10: the upstream abort command
;; operates on the current buffer, finds its FSM, and invokes our
;; callback with ='abort= — no chat-mode wrapper needed).


;; [[file:mode.org::*Forward declarations][Forward declarations:1]]
(declare-function gptel-chat-send "gptel-chat-send" ())
(declare-function gptel-chat-next-turn "gptel-chat-nav" ())
(declare-function gptel-chat-previous-turn "gptel-chat-nav" ())
(declare-function gptel-chat-regenerate "gptel-chat-nav" ())
(declare-function gptel-chat-toggle-display-layer "gptel-chat-display" ())
(declare-function gptel-abort "gptel" (buf))
;; Forward declarations:1 ends here

;; Keymap

;; Small, opinionated, non-shadowing (design.md §Decision 7).

;; Because chat-mode buffers MAY use org headings for organizational
;; structure (design.md §Decision 12), shadowing =C-c C-n= / =C-c C-p=
;; would remove heading navigation. Turn-nav uses the single-=C-c=
;; prefix (=C-c n= / =C-c p=) so both navigation systems remain usable.

;; =C-c C-k= is bound directly to the upstream =gptel-abort= — chat-mode
;; does not need its own abort wrapper (design.md §Decision 10).


;; [[file:mode.org::*Keymap][Keymap:1]]
(defvar gptel-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'gptel-chat-send)
    (define-key map (kbd "C-c n")   #'gptel-chat-next-turn)
    (define-key map (kbd "C-c p")   #'gptel-chat-previous-turn)
    (define-key map (kbd "C-c C-r") #'gptel-chat-regenerate)
    (define-key map (kbd "C-c C-t") #'gptel-chat-toggle-display-layer)
    (define-key map (kbd "C-c C-k") #'gptel-abort)
    map)
  "Keymap for `gptel-chat-mode'.

Bindings:
  C-c C-c  `gptel-chat-send'                 — send the buffer
  C-c n    `gptel-chat-next-turn'            — jump to next turn
  C-c p    `gptel-chat-previous-turn'        — jump to previous turn
  C-c C-r  `gptel-chat-regenerate'           — regenerate last response
  C-c C-t  `gptel-chat-toggle-display-layer' — show/hide overlays
  C-c C-k  `gptel-abort'                     — abort the in-flight request

The single-`C-c' prefix on turn navigation avoids shadowing org's
`C-c C-n' / `C-c C-p' heading navigation — see design.md §Decision 7.")
;; Keymap:1 ends here

;; Major mode definition

;; =define-derived-mode= from =org-mode= — inherit fontification, folding,
;; and inside-block editing behaviours (design.md §Decision 6). Overrides
;; are minimal by design: only the keymap (above) and
;; =org-adapt-indentation= (below).

;; The =gptel-chat-mode-hook= runs at activation per Emacs convention;
;; later tasks (display layer, preset wiring) register their own
;; activation logic by adding to the hook.


;; [[file:mode.org::*Major mode definition][Major mode definition:1]]
;;;###autoload
(define-derived-mode gptel-chat-mode org-mode "GPTEL-Chat"
  "Major mode for multi-turn gptel chat buffers.

Derived from `org-mode'.  The buffer is a sequence of turn blocks:
`#+begin_user' / `#+end_user' pairs for user messages and
`#+begin_assistant' / `#+end_assistant' pairs for model responses.
Assistant blocks may contain nested `#+begin_tool (<name> :args <sexp>)'
blocks recording tool calls and their results.  Content outside turn
blocks — headings, prose, drawers, `#+keyword:' lines — is treated as
human organization/commentary and does not participate in message
construction.

Turn-block delimiter lines are pinned to column 0; `org-adapt-indentation'
is disabled buffer-locally so the parser's `^#\\+begin_...' anchor stays
valid regardless of the user's global org settings (design.md §Decision 14).

\\{gptel-chat-mode-map}"
  (setq-local org-adapt-indentation nil))
;; Major mode definition:1 ends here

;; New-chat command

;; Minimum viable initialization (design.md §Decision 9): a fresh buffer
;; with a single empty =#+begin_user= / =#+end_user= block and point
;; positioned on the empty line between the delimiters. No model, system
;; prompt, preset, or metadata keywords are pre-populated — per-buffer
;; configuration flows through the upstream preset system
;; (=gptel--apply-preset=, =gptel-menu=) and is wired in by a later task.

;; The buffer name uses =generate-new-buffer-name= so invoking the command
;; repeatedly produces =*gptel-chat*=, =*gptel-chat*<2>=, etc.


;; [[file:mode.org::*New-chat command][New-chat command:1]]
;;;###autoload
(defun gptel-chat-new ()
  "Create a new gptel chat buffer and switch to it.

The buffer is in `gptel-chat-mode' with initial content containing
an empty `#+begin_user' / `#+end_user' block; point is positioned
on the empty line inside the block so the user can start typing
immediately.

No preset, model, system prompt, or metadata is pre-populated — see
design.md §Decision 9.  Users who want a preset invoke `gptel-menu',
save the buffer with a `:GPTEL_PRESET:' property drawer, or set
`gptel--preset' as a file-local variable."
  (interactive)
  (let ((buffer (generate-new-buffer "*gptel-chat*")))
    (switch-to-buffer buffer)
    (gptel-chat-mode)
    (insert "#+begin_user\n\n#+end_user\n")
    (goto-char (point-min))
    ;; Move past "#+begin_user\n" to the empty line inside the block.
    (forward-line 1)
    buffer))
;; New-chat command:1 ends here

;; Provide


;; [[file:mode.org::*Provide][Provide:1]]
(provide 'gptel-chat-mode)

;;; mode.el ends here
;; Provide:1 ends here
