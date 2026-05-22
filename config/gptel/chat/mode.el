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

;; Property-drawer value fontification override

;; Org applies its `/emphasis/' font-lock keyword buffer-wide, with no
;; context guard excluding `:PROPERTIES:' drawers. A drawer value such as
;; `:GPTEL_SCOPE_READ: /Users/jeff/emacs/' is a syntactically valid org
;; emphasis span — the leading `/' opens it, the trailing `/' closes it —
;; so org renders the path italic and, with `org-hide-emphasis-markers'
;; on, dims or hides the boundary `/' characters. For `:GPTEL_SCOPE_*:'
;; path values this is a legibility defect: the user cannot read the path
;; they scoped (design.md §Addendum Finding A / Decision A).

;; Drawer values are *data*, not prose, and should never carry emphasis.
;; =gptel-chat--drawer-value-matcher= is a font-lock matcher function that
;; advances point to the next property-drawer value span: it matches a
;; `:KEY: value' line and confirms — by scanning backward to the nearest
;; `:PROPERTIES:' / `:END:' boundary — that the line sits inside a
;; property drawer. Match group 1 is the value text.

;; The matcher is registered buffer-locally in =gptel-chat-mode= via
;; =font-lock-add-keywords= with =\='append= ordering and an OVERRIDE
;; facespec (the trailing =t= in =(1 \='org-property-value t)=). Append
;; ordering plus the override flag ensure our `org-property-value' face
;; wins over org's emphasis keyword, which runs inside
;; `org-font-lock-extra-keywords'. The override is scoped to
;; property-drawer value spans only — `/emphasis/' in chat-turn prose is
;; left untouched.


;; [[file:mode.org::*Property-drawer value fontification override][Property-drawer value fontification override:1]]
(defun gptel-chat--drawer-value-matcher (limit)
  "Font-lock matcher: advance point to the next property-drawer value span.

Search forward (bounded by LIMIT) for a `:KEY: value' line.  Set match
group 1 to the value text and return non-nil only when the line sits
inside a `:PROPERTIES: ... :END:' drawer — confirmed by scanning
backward to the nearest `:PROPERTIES:'/`:END:' boundary and requiring
`PROPERTIES'.  Lines that look like property entries but live outside a
drawer (or property lines with no value) are skipped.

Used by `gptel-chat-mode' with an OVERRIDE facespec so drawer values
render as plain `org-property-value' data rather than inheriting org
`/emphasis/' font-lock (design.md §Addendum Finding A / Decision A)."
  (let (found)
    (while (and (not found)
                (re-search-forward
                 "^[ \t]*:[A-Za-z][A-Za-z0-9_-]*:\\(?: \\(.*\\)\\)?$"
                 limit t))
      (when (and (match-beginning 1)
                 (save-excursion
                   (save-match-data
                     (forward-line 0)
                     (and (re-search-backward
                           "^[ \t]*:\\(PROPERTIES\\|END\\):[ \t]*$" nil t)
                          (string-equal (match-string 1) "PROPERTIES")))))
        (setq found t)))
    found))
;; Property-drawer value fontification override:1 ends here

;; Config-drawer folding

;; A persisted =session.org= opens with a file-level =:PROPERTIES:=
;; drawer at =point-min= holding the full configuration snapshot —
;; preset, model, backend, tools, temperature, and the
;; =:GPTEL_SCOPE_*:= keys (design.md §Addendum, Finding C / Decision C;
;; register entry =register/boundary/chat-mode-session-display=,
;; override C). Shown expanded, that block is the first thing in view
;; and buries the =* System Prompt= / =* Chat= headings under a wall of
;; configuration.

;; org-mode's startup visibility (=org-set-visibility-according-to-property=
;; et al.) folds drawers *under headings*, but the chat-mode config
;; drawer sits *before the first heading* — it is not reliably reached
;; by startup visibility in the derived mode. =gptel-chat-mode= folds it
;; explicitly on activation.

;; =org-fold-hide-drawer-all= (org 9.7's =org-fold= API) folds every
;; drawer in the buffer, including the pre-first-heading file-level
;; one. On a drawerless buffer — e.g. a =gptel-chat-new= scratch buffer
;; whose only content is an empty =#+begin_user= block — it finds no
;; drawer and is a silent no-op, so no guard is needed.

;; *Ordering matters.* The fold must run from =gptel-chat-mode-hook=,
;; *not* from the =define-derived-mode= body. =define-derived-mode=
;; expands so the child body executes *before* =run-mode-hooks=, while
;; =org-mode='s own fold subsystem (=org-fold-core=) is still
;; finalizing. A fold registered from the body is silently dropped by a
;; later step of mode activation — verified empirically: an
;; =org-fold-hide-drawer-all= call in the body leaves
;; =(org-fold-folded-p ... 'drawer)= = nil, whereas the identical call
;; from =gptel-chat-mode-hook= folds the drawer correctly. So the fold
;; function is attached to the hook with =add-hook= at load time;
;; =define-derived-mode= runs that hook after the body, once the fold
;; core is fully set up. This is exactly the "run the fold from the mode
;; hook after content load" resolution anticipated in the task's
;; implementation steps.


;; [[file:mode.org::*Config-drawer folding][Config-drawer folding:1]]
(defun gptel-chat--fold-config-drawer ()
  "Fold all drawers in the current buffer on `gptel-chat-mode' activation.

Runs from `gptel-chat-mode-hook'.  Folds every drawer via
`org-fold-hide-drawer-all' — a buffer-wide fold, not a targeted one.
This covers the pre-first-heading file-level configuration drawer
(the WYSIWYG-clutter target of design.md §Addendum Decision C) that
org-mode's startup visibility does not reliably reach in this derived
mode; it also folds the `* System Prompt' heading's own property
drawer, which is harmless since that heading is itself folded by its
`:VISIBILITY: folded' property.

Attached to the hook rather than called from the `define-derived-mode'
body: the body runs before `run-mode-hooks', while org-mode's
`org-fold-core' subsystem is still finalizing, and a fold registered
that early is silently dropped by a later activation step.  Running
from the hook — after the body and after fold-core setup — folds the
drawer reliably.

A no-op when the buffer has no drawer (a `gptel-chat-new' scratch
buffer): `org-fold-hide-drawer-all' simply finds nothing to fold and
raises no error.  See design.md §Addendum (Finding C / Decision C)."
  (org-fold-hide-drawer-all))
;; Config-drawer folding:1 ends here

;; Major mode definition

;; =define-derived-mode= from =org-mode= — inherit fontification, folding,
;; and inside-block editing behaviours (design.md §Decision 6). Overrides
;; are minimal by design: the keymap (above), =org-adapt-indentation= and
;; the drawer-value font-lock override (in the body, below); the
;; config-drawer fold runs from =gptel-chat-mode-hook=
;; (=gptel-chat--fold-config-drawer=, attached after the form below).

;; The =gptel-chat-mode-hook= runs at activation per Emacs convention,
;; *after* the mode body — which is why the config-drawer fold attaches
;; to it rather than living in the body. Later tasks (display layer,
;; preset wiring) register their own activation logic the same way.

;; The =font-lock-add-keywords= call installs
;; =gptel-chat--drawer-value-matcher= buffer-locally (=nil= first
;; argument). =\='append= places the keyword after org's own keywords and
;; the OVERRIDE flag =t= makes our `org-property-value' face win over
;; org's `/emphasis/' keyword for property-drawer value spans (design.md
;; §Addendum Finding A).


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

Property-drawer values are re-stamped with `org-property-value' via a
buffer-local OVERRIDE font-lock keyword so scope path values do not
inherit org `/emphasis/' (design.md §Addendum Finding A).

The file-level config `:PROPERTIES:' drawer is folded on activation by
`gptel-chat--fold-config-drawer', attached to `gptel-chat-mode-hook'
below, so a persisted `session.org' opens clean (design.md §Addendum,
Decision C).

\\{gptel-chat-mode-map}"
  (setq-local org-adapt-indentation nil)
  (font-lock-add-keywords
   nil
   '((gptel-chat--drawer-value-matcher (1 'org-property-value t)))
   'append))

(add-hook 'gptel-chat-mode-hook #'gptel-chat--fold-config-drawer)
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

;; Provide


;; [[file:mode.org::*Provide][Provide:1]]
(provide 'gptel-chat-mode)

;;; mode.el ends here
;; Provide:1 ends here
