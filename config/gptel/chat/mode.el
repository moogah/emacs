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

;; Dependencies

;; =org= is our parent mode. Require it explicitly so =gptel-chat-mode=
;; activation does not rely on a prior autoload having loaded =org= —
;; useful for minimal batch contexts.


;; [[file:mode.org::*Dependencies][Dependencies:1]]
(require 'org)
;; Dependencies:1 ends here

;; Forward declarations

;; Keymap entries bind to commands defined in later-loading sibling
;; modules (=send=, =nav=, =display=). Emacs resolves command symbols at
;; call-time, so the bindings work even when the target functions are not
;; yet defined. The single-arg =declare-function= form below suppresses
;; byte-compiler "not known to be defined" warnings without making
;; unverifiable claims about the source library (the sibling files are
;; named =send.el=, =nav.el=, =display.el=, so any library hint here
;; would need to match — simpler to omit).

;; =gptel-abort= lives upstream in =gptel.el= and is always available
;; once =gptel= itself has loaded (design.md §Decision 10: the upstream
;; abort command operates on the current buffer, finds its FSM, and
;; invokes our callback with ='abort= — no chat-mode wrapper needed).


;; [[file:mode.org::*Forward declarations][Forward declarations:1]]
(declare-function gptel-chat-send nil ())
(declare-function gptel-chat-next-turn nil ())
(declare-function gptel-chat-previous-turn nil ())
(declare-function gptel-chat-regenerate nil ())
(declare-function gptel-chat-toggle-display-layer nil ())
(declare-function gptel-chat-menu nil ())
(declare-function gptel-abort "gptel" (buf))
(declare-function gptel-chat--point-in-block-body-p nil
                  (&optional pos buffer))
;; Forward declarations:1 ends here

;; Keymap

;; Small, opinionated, non-shadowing (design.md §Decision 7).

;; Because chat-mode buffers MAY use org headings for organizational
;; structure (design.md §Decision 12), shadowing =C-c C-n= / =C-c C-p=
;; would remove heading navigation. Turn-nav uses the single-=C-c=
;; prefix (=C-c n= / =C-c p=) so both navigation systems remain usable.

;; =C-c C-k= is bound directly to the upstream =gptel-abort= — chat-mode
;; does not need its own abort wrapper (design.md §Decision 10).

;; =C-c C-,= opens =gptel-chat-menu= — the chat-mode transient that
;; mirrors =gptel-menu='s configuration layout with the Send suffix
;; rebound to =gptel-chat-send= (design.md §Decision 15, task
;; =menu-integration=). We deliberately avoid binding the /same/ key as
;; upstream =gptel-menu='s suggested binding (users who want upstream
;; behaviour can still run =M-x gptel-menu= directly, and the upstream
;; prefix remains unmutated).


;; [[file:mode.org::*Keymap][Keymap:1]]
(defvar gptel-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'gptel-chat-send)
    (define-key map (kbd "C-c n")   #'gptel-chat-next-turn)
    (define-key map (kbd "C-c p")   #'gptel-chat-previous-turn)
    (define-key map (kbd "C-c C-r") #'gptel-chat-regenerate)
    (define-key map (kbd "C-c C-t") #'gptel-chat-toggle-display-layer)
    (define-key map (kbd "C-c C-k") #'gptel-abort)
    (define-key map (kbd "C-c C-,") #'gptel-chat-menu)
    map)
  "Keymap for `gptel-chat-mode'.

Bindings:
  C-c C-c  `gptel-chat-send'                 — send the buffer
  C-c n    `gptel-chat-next-turn'            — jump to next turn
  C-c p    `gptel-chat-previous-turn'        — jump to previous turn
  C-c C-r  `gptel-chat-regenerate'           — regenerate last response
  C-c C-t  `gptel-chat-toggle-display-layer' — show/hide overlays
  C-c C-k  `gptel-abort'                     — abort the in-flight request
  C-c C-,  `gptel-chat-menu'                 — open chat-mode transient

The single-`C-c' prefix on turn navigation avoids shadowing org's
`C-c C-n' / `C-c C-p' heading navigation — see design.md §Decision 7.")
;; Keymap:1 ends here

;; Buffer content indentation

;; =gptel-chat-content-indentation= controls how many spaces of
;; heading-collision escape are applied to column-0 =*= lines inside
;; chat-block bodies. The org heading regex is =^\*+ = anchored at column
;; 0; any leading whitespace breaks it, so a single space is the minimum
;; escape that prevents the parser from absorbing the line into an
;; outline subtree (=openspec/changes/gptel-chat-heading-scoping/specs/gptel/chat-mode.md=
;; "Heading-collision escape"; =openspec/changes/gptel-chat-heading-scoping/design.md=
;; §Decision 8).

;; Default =1= minimizes visual noise. Users wanting parity with
;; =org-edit-src-content-indentation= (default =2=) can set it to =2=.
;; The parser un-escape strips *any* amount of leading whitespace before
;; a =*= line, so round-trip is robust against config changes — a buffer
;; written with =1= and re-opened under =2= is re-normalized at
;; read-migration time.

;; The defcustom lives in =mode.el= (rather than =stream.el=, =send.el=,
;; or =parser.el=) because every write/read pipeline stage that consults
;; it loads after =mode.el= in =chat.org='s feature-module load order.
;; The =:group 'gptel-chat= reference resolves at customize-time, not
;; load-time, so the forward reference to the group defined in
;; =display.el= is harmless.


;; [[file:mode.org::*Buffer content indentation][Buffer content indentation:1]]
(defcustom gptel-chat-content-indentation 1
  "Number of leading spaces used to escape column-0 `*' lines in chat blocks.

Org's heading regex is `^\\*+ ' anchored at column 0; an unescaped `*'
line inside a `#+begin_user' / `#+begin_assistant' / `#+begin_tool'
block destroys the block in `org-element-parse-buffer' (the AST loses
the special-block entirely; the heading absorbs subsequent content
into its outline subtree).  The chat-mode write pipeline (streaming
insertion, user-typed lines, paste/yank, file-read migration) prefixes
every column-0 `*' line in a chat-block body with this many spaces;
the parser's send path strips any leading whitespace before such a
line so the LLM only ever sees clean message content.

Default 1 is the minimum that breaks the heading regex.  Set to 2 for
parity with `org-edit-src-content-indentation'.  See
`openspec/changes/gptel-chat-heading-scoping/specs/gptel/chat-mode.md'
\(\"Heading-collision escape\") and design.md §Decision 8."
  :type 'integer
  :group 'gptel-chat)
;; Buffer content indentation:1 ends here

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
  (setq-local org-adapt-indentation nil)
  (add-hook 'after-change-functions
            #'gptel-chat--escape-inserted-headings nil t))
;; Major mode definition:1 ends here

;; Pure prepare helper

;; =gptel-chat--prepare-new-buffer= is the pure, side-effect-isolated
;; half of =gptel-chat-new=: it creates the buffer, activates the mode,
;; inserts the initial content, and positions point — but it does *not*
;; mutate window configuration. The interactive command is a thin
;; =switch-to-buffer= wrapper on top of this helper (§Design rationale in
;; the corresponding task file).

;; This split lets tests exercise the real initialization path without
;; the window-config side effect =switch-to-buffer= would otherwise leak
;; into the test environment, and opens the door to future variants such
;; as =gptel-chat-new-other-window= that differ only in how they display
;; the prepared buffer.


;; [[file:mode.org::*Pure prepare helper][Pure prepare helper:1]]
(defun gptel-chat--prepare-new-buffer ()
  "Create and return a fresh gptel chat buffer.

The returned buffer is in `gptel-chat-mode' with initial content
containing an empty `#+begin_user' / `#+end_user' block; point is
positioned on the empty line inside the block so typing extends the
user block rather than the delimiters.

This is the pure half of `gptel-chat-new': it does NOT mutate window
configuration.  Callers that want to display the buffer wrap this in
`switch-to-buffer' (or a variant such as `pop-to-buffer',
`display-buffer', etc.).

No preset, model, system prompt, or metadata is pre-populated — see
design.md §Decision 9."
  (let ((buffer (generate-new-buffer "*gptel-chat*")))
    (with-current-buffer buffer
      (gptel-chat-mode)
      (insert "#+begin_user\n\n#+end_user\n")
      (goto-char (point-min))
      ;; Move past "#+begin_user\n" to the empty line inside the block.
      (forward-line 1))
    buffer))
;; Pure prepare helper:1 ends here

;; Interactive switch wrapper

;; =gptel-chat-new= is the interactive entry point: prepare the buffer
;; via the pure helper, then switch to it.


;; [[file:mode.org::*Interactive switch wrapper][Interactive switch wrapper:1]]
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
`gptel--preset' as a file-local variable.

This command is an interactive wrapper over
`gptel-chat--prepare-new-buffer' — all buffer-preparation logic
lives in the helper; this command only adds the
`switch-to-buffer' window mutation."
  (interactive)
  (let ((buffer (gptel-chat--prepare-new-buffer)))
    (switch-to-buffer buffer)
    buffer))
;; Interactive switch wrapper:1 ends here

;; Effect on downstream =after-change-functions=

;; When this hook inserts the prefix it grows the buffer by N spaces per
;; escaped line. Downstream =after-change-functions= entries see the
;; buffer in its post-rewrite state but receive the *original* =BEG=,
;; =END= arguments from Emacs. Callers that do positional arithmetic on
;; =END= must be tolerant of this — the same caveat applies to any
;; =after-change-functions= entry that mutates the buffer. We do not
;; attempt to "fix up" =END= for downstream entries because Emacs does
;; not provide a documented way to do so; we accept the standard
;; contract.


;; [[file:mode.org::*Effect on downstream =after-change-functions=][Effect on downstream =after-change-functions=:1]]
(defun gptel-chat--escape-inserted-headings (beg end length)
  "Escape column-0 `*' lines in [BEG, END) when LENGTH is 0.

Intended for `after-change-functions'.  When LENGTH is 0 (pure
insertion, not a deletion or replacement), walk the inserted range
line by line.  For each line whose beginning-of-line position is
>= BEG, starts with `^\\*+ ', and whose BOL lies strictly inside a
chat-block body (per `gptel-chat--point-in-block-body-p'), insert
`gptel-chat-content-indentation' leading spaces at that BOL.

The function is idempotent: it acts only on lines that currently
start with `*' at column 0.  An already-escaped line begins with
whitespace, so re-running on previously-escaped content is a no-op.

Re-entry guard: `inhibit-modification-hooks' is bound non-nil while
the rewrite runs so this hook does not re-trigger on its own
inserts.

Boundary clipping: the per-line predicate returns nil for delimiter
lines and for positions outside any chat block, so an inserted
range that straddles a `#+end_*' line escapes only the portion that
actually falls inside the body.

Mid-line insertion: when point sits mid-line at insertion (column
> 0), the first inserted line is concatenated onto the existing
line and its BOL is < BEG.  Such a line is excluded from the scan
by the BOL >= BEG guard, matching the user's mental model that the
first segment is part of the existing line, not new content.

Downstream `after-change-functions' entries see the buffer in its
post-rewrite state but receive the original BEG/END from Emacs;
positional arithmetic on END must tolerate the buffer having grown
by the inserted prefix characters.  This caveat applies to any
mutating `after-change-functions' entry, not just this one.

See `openspec/changes/gptel-chat-heading-scoping/design.md'
§Decision 3."
  (when (and (zerop length)
             (> end beg))
    (let ((inhibit-modification-hooks t)
          (indent (max 0 (or gptel-chat-content-indentation 0))))
      (when (> indent 0)
        (save-excursion
          (save-match-data
            (let ((prefix (make-string indent ?\s))
                  ;; Use a marker so positional drift caused by our
                  ;; own inserts does not invalidate the upper bound
                  ;; of the scan.
                  (end-marker (copy-marker end t)))
              (unwind-protect
                  (progn
                    (goto-char beg)
                    ;; Advance to the first BOL >= BEG.  When BEG is
                    ;; itself at column 0, BOL = BEG and we start
                    ;; immediately; when BEG is mid-line, the first
                    ;; line is excluded (its BOL < BEG), matching
                    ;; the mid-line semantics described in the
                    ;; docstring.
                    (unless (= (point) (line-beginning-position))
                      (forward-line 1))
                    (while (and (< (point) end-marker)
                                (not (eobp)))
                      (let ((bol (point)))
                        (when (and (looking-at "\\*+ ")
                                   (gptel-chat--point-in-block-body-p
                                    bol))
                          (goto-char bol)
                          (insert prefix)))
                      (forward-line 1)))
                (set-marker end-marker nil)))))))))
;; Effect on downstream =after-change-functions=:1 ends here

;; Provide


;; [[file:mode.org::*Provide][Provide:1]]
(provide 'gptel-chat-mode)

;;; mode.el ends here
;; Provide:1 ends here
