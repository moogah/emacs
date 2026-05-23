;;; display.el --- GPTEL Chat-Mode Display Layer -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Subtle, role-distinguishing overlay layer for `gptel-chat-mode'
;; buffers.  Overlays span the BODY of each turn block (between the
;; `#+begin_<role>' and `#+end_<role>' delimiter lines) and carry the
;; `gptel-chat-display' property so they can be cleanly removed.
;;
;; Public entry points:
;;   `gptel-chat-toggle-display-layer'   — interactive toggle command
;;   `gptel-chat--refresh-overlays'      — buffer refresh (called from
;;                                          mode hook and after-change)
;;
;; Customization:
;;   `gptel-chat-user-face'              — face for user turn bodies
;;   `gptel-chat-assistant-face'         — face for assistant turn bodies
;;   `gptel-chat-display-refresh-delay'  — debounce seconds (default 0.1)
;;
;; See `openspec/changes/gptel-chat-mode/architecture.md' §Components
;; (subsection `gptel-chat-display (display)') for the module contract.

;;; Code:

(require 'cl-lib)
(require 'gptel-chat-parser)

;; Forward declarations for symbols that live in `gptel-chat-send'
;; (loaded independently at mode-activation time) and in upstream
;; gptel.  Silences byte-compiler warnings without creating a hard
;; load-order dependency; all run-time references are guarded by
;; `bound-and-true-p' or `fboundp'.
(defvar gptel-chat--lifecycle-state)
(declare-function gptel-fsm-info "gptel-request" (fsm))
(declare-function gptel-chat--body-indent "gptel-chat-mode" ())

;; Customization group

;; Reuse the parent =gptel-chat= group when it exists; otherwise create a
;; minimal local group so =M-x customize-group RET gptel-chat RET= works
;; in batch tests that load this module in isolation.


;; [[file:display.org::*Customization group][Customization group:1]]
(defgroup gptel-chat nil
  "Multi-turn gptel chat-mode buffers."
  :group 'gptel
  :prefix "gptel-chat-")
;; Customization group:1 ends here

;; Faces

;; Inherit from =org-block= so chat turn bodies pick up whatever the
;; user's active org theme uses for source/quote blocks.  This avoids
;; hard-coded colour palettes that fight the user's theme — a frequent
;; source of unreadable light-on-light or dark-on-dark renders when the
;; =(((background dark)))= matcher resolves the wrong variant (e.g. when
;; =custom-set-faces= overrides =default= after the theme loads, or when
;; =frame-parameter background-mode= disagrees with the theme family).

;; The =#+begin_user= / =#+begin_assistant= delimiter lines remain
;; visually distinct on their own; users who want extra per-role tinting
;; can customise these faces directly via =M-x customize-face=.


;; [[file:display.org::*Faces][Faces:1]]
(defface gptel-chat-user-face
  '((t :inherit org-block :extend t))
  "Face applied to the body region of `#+begin_user' blocks.
Inherits from `org-block' so chat blocks match the user's org theme.
Delimiter lines are NOT covered by this face."
  :group 'gptel-chat)

(defface gptel-chat-assistant-face
  '((t :inherit org-block :extend t))
  "Face applied to the body region of `#+begin_assistant' blocks.
Inherits from `org-block' so chat blocks match the user's org theme.
Delimiter lines are NOT covered by this face."
  :group 'gptel-chat)
;; Faces:1 ends here

;; Defcustoms


;; [[file:display.org::*Defcustoms][Defcustoms:1]]
(defcustom gptel-chat-display-refresh-delay 0.1
  "Idle-timer delay (seconds) for debounced overlay refresh.
After a buffer change, the display layer waits this many seconds of
idle time before recomputing role overlays.  Lower values feel more
responsive; higher values reduce work during rapid edits."
  :type 'number
  :group 'gptel-chat)
;; Defcustoms:1 ends here

;; Buffer-local state

;; Two buffer-local variables:

;; - =gptel-chat--display-enabled= tracks whether the layer is active in
;;   this buffer. Toggled by =gptel-chat-toggle-display-layer=. Default
;;   =t= (enabled on fresh buffers per spec scenario "Display layer is
;;   active by default").
;; - =gptel-chat--display-refresh-timer= holds the pending idle timer so
;;   rapid edits cancel the prior timer instead of stacking up.


;; [[file:display.org::*Buffer-local state][Buffer-local state:1]]
(defvar-local gptel-chat--display-enabled t
  "Non-nil when the chat-mode display layer is active in this buffer.
Toggled by `gptel-chat-toggle-display-layer'.")

(defvar-local gptel-chat--display-refresh-timer nil
  "Pending idle timer for debounced overlay refresh, or nil.")
;; Buffer-local state:1 ends here

;; Body-range helper

;; The parser returns markers pointing at the start of =#+begin_*= and
;; =#+end_*= delimiter lines. Translate those into a body range that
;; excludes the delimiters:

;; - =body-start= = position immediately after the =#+begin_*\n= line.
;; - =body-end=   = position at start of =#+end_*= line (i.e. the parser's
;;   =:end= marker).

;; Keeping this in one helper prevents off-by-one drift between the
;; user-turn and assistant-turn overlay paths.


;; [[file:display.org::*Body-range helper][Body-range helper:1]]
(defun gptel-chat--display-body-range (turn)
  "Return a (BODY-START . BODY-END) cons for TURN, or nil on an empty body.
TURN is a turn plist as returned by `gptel-chat-parse-buffer'.  The
start position skips the `#+begin_*' delimiter line; the end position
is the start of the `#+end_*' delimiter line.  Returns nil when the
resulting range is empty (which would produce a zero-width overlay)."
  (let* ((start-marker (plist-get turn :start))
         (end-marker   (plist-get turn :end))
         (body-end     (and end-marker (marker-position end-marker)))
         (body-start
          (when start-marker
            (save-excursion
              (goto-char (marker-position start-marker))
              ;; Move past the `#+begin_<role>' line (and its terminating
              ;; newline) so the overlay starts at the first body char.
              (forward-line 1)
              (point)))))
    (when (and body-start body-end (< body-start body-end))
      (cons body-start body-end))))
;; Body-range helper:1 ends here

;; Overlay installer

;; Install a single overlay for one turn. Every overlay carries the
;; =gptel-chat-display= property so =remove-overlays= can target them
;; cleanly without touching overlays owned by other packages.


;; [[file:display.org::*Overlay installer][Overlay installer:1]]
(defun gptel-chat--display-make-overlay (start end face)
  "Install a `gptel-chat-display' overlay from START to END with FACE.
Returns the new overlay.  The overlay is tagged with the
`gptel-chat-display' property (value t) for later removal."
  (let ((ov (make-overlay start end nil t nil)))
    (overlay-put ov 'gptel-chat-display t)
    (overlay-put ov 'face face)
    ;; Tag the role so external inspection / debugging is easy.
    ov))
;; Overlay installer:1 ends here

;; Overlay cleanup

;; Centralize the removal call so =toggle= and =refresh= both route
;; through the same code path.


;; [[file:display.org::*Overlay cleanup][Overlay cleanup:1]]
(defun gptel-chat--display-remove-all ()
  "Remove every `gptel-chat-display' overlay from the current buffer."
  (remove-overlays (point-min) (point-max) 'gptel-chat-display t))
;; Overlay cleanup:1 ends here

;; Tool-delimiter alignment (Path C)

;; Nested =#+begin_tool= / =#+end_tool= delimiter lines live inside an
;; assistant body. They stay at *real* column 0 — the parser anchors
;; =^#\+begin_tool= / =^#\+end_tool= there, and Path C of the
;; body-indentation design (=gptel-chat-heading-scoping= design.md
;; Decision 5) keeps them there so the parser, the streaming emitter,
;; and the block-body predicate are all unchanged.

;; The cost of column-0 tool delimiters is purely visual: they would sit
;; flush-left amid the indented prose around them.
;; =gptel-chat--display-prefix-tool-delimiters= closes that gap by
;; installing a =line-prefix= overlay — a string of =gptel-chat--body-indent=
;; spaces — on each tool delimiter line, so it *renders* aligned with the
;; indented body. The overlay is display-only: buffer text, on-disk
;; content, and parser input are all unaffected. A display-only nudge is
;; sound here — and not for body content — because tool delimiter lines
;; are never sent to the model.


;; [[file:display.org::*Tool-delimiter alignment (Path C)][Tool-delimiter alignment (Path C):1]]
(defconst gptel-chat--display-tool-delimiter-regexp
  "^#\\+\\(?:begin\\|end\\)_tool\\b"
  "Regexp matching a nested tool-block delimiter line at column 0.
Used by `gptel-chat--display-prefix-tool-delimiters' to find the
lines that receive a cosmetic `line-prefix'.")

(defun gptel-chat--display-prefix-tool-delimiters (body-start body-end)
  "Install `line-prefix' overlays on tool delimiter lines in [BODY-START, BODY-END).
For each column-0 `#+begin_tool' / `#+end_tool' line in the range,
install a `gptel-chat-display'-tagged overlay carrying a `line-prefix'
of `gptel-chat--body-indent' spaces, so the delimiter renders
visually aligned with the indented body around it (design.md
Decision 5 — Path C).  The overlays are display-only and are removed
with the role overlays by `gptel-chat--display-remove-all'."
  (let ((prefix (make-string (gptel-chat--body-indent) ?\s)))
    (save-excursion
      (save-match-data
        (goto-char body-start)
        (while (re-search-forward gptel-chat--display-tool-delimiter-regexp
                                  body-end t)
          (let ((ov (make-overlay (line-beginning-position)
                                  (line-end-position) nil t nil)))
            (overlay-put ov 'gptel-chat-display t)
            (overlay-put ov 'line-prefix prefix)))))))
;; Tool-delimiter alignment (Path C):1 ends here

;; Refresh — the main entry point

;; Walk the turn list and re-install role overlays. The refresh is
;; idempotent: it clears existing =gptel-chat-display= overlays before
;; installing new ones, so calling it repeatedly converges on the correct
;; state.

;; If =gptel-chat-parse-buffer= raises a =user-error= (the buffer is
;; structurally invalid — unclosed block, turn-inside-turn, etc.) we
;; swallow it here: partial typing of a block should not blow up a
;; silent background refresh. The refresh simply does nothing until the
;; buffer becomes parseable again.


;; [[file:display.org::*Refresh — the main entry point][Refresh — the main entry point:1]]
(defun gptel-chat--refresh-overlays (&optional buffer)
  "Refresh role overlays in BUFFER (default current).
Removes existing `gptel-chat-display' overlays and installs fresh ones
spanning the body of each user / assistant turn, plus `line-prefix'
overlays aligning the nested tool-block delimiters (Path C).  A
`user-error' from the parser (mid-edit buffer with an unclosed block)
is caught and treated as a no-op — the next refresh will reinstall
overlays once the buffer reaches a parseable state."
  (with-current-buffer (or buffer (current-buffer))
    (when gptel-chat--display-enabled
      (gptel-chat--display-remove-all)
      (condition-case _err
          (let ((turns (gptel-chat-parse-buffer)))
            (dolist (turn turns)
              (when-let* ((range (gptel-chat--display-body-range turn))
                          (face (pcase (plist-get turn :role)
                                  (`user      'gptel-chat-user-face)
                                  (`assistant 'gptel-chat-assistant-face))))
                (gptel-chat--display-make-overlay
                 (car range) (cdr range) face)
                ;; Path C: render the nested tool-block delimiter lines
                ;; aligned with the indented assistant body.
                (when (eq (plist-get turn :role) 'assistant)
                  (gptel-chat--display-prefix-tool-delimiters
                   (car range) (cdr range))))))
        (user-error nil)))))
;; Refresh — the main entry point:1 ends here

;; Debounced after-change callback

;; =after-change-functions= runs synchronously on every edit. Rather
;; than reparse on each keystroke, schedule a single idle-timer refresh
;; and cancel any prior pending timer so rapid edits collapse into one
;; reparse. The timer callback captures the buffer so it still runs
;; correctly if the user switches windows before idle kicks in.

;; **Streaming-aware refresh** (design.md Decision 5 Risks, lines
;; 418/424): during an active streaming or tool-running request the
;; upstream =gptel-request= pipeline fires an =after-change= event on
;; every chunk inserted into the buffer. A naive O(n) full-buffer
;; reparse per chunk produces visual flicker on large chats and
;; monopolises idle time. The scheduler therefore skips scheduling
;; while =gptel-chat--lifecycle-state= is =streaming= or
;; =tool-running=; a single refresh fires from the =DONE= handler
;; (=gptel-chat--display-refresh-on-done=, below) once the request
;; completes.

;; The =bound-and-true-p= check keeps display.el load-order
;; independent: if =gptel-chat-send= has not been loaded yet
;; (e.g. in a unit test that only requires the display layer), the
;; variable is unbound and the guard falls through to the normal
;; scheduling path.


;; [[file:display.org::*Debounced after-change callback][Debounced after-change callback:1]]
(defconst gptel-chat--display-streaming-states
  '(streaming tool-running)
  "Lifecycle-state symbols during which the display layer defers refresh.
Matches the values `gptel-chat--on-type' / `gptel-chat--on-tool' set
on `gptel-chat--lifecycle-state' (see send.org §Handler functions).
A single refresh is fired from `gptel-chat--display-refresh-on-done'
once the request reaches terminal state.  design.md §Decision 5
Risks (lines 418/424).")

(defun gptel-chat--display-streaming-p ()
  "Return non-nil when this buffer has a streaming/tool-running request.
Reads the buffer-local `gptel-chat--lifecycle-state' that the FSM
handlers in `send.el' write.  Returns nil when the variable is
unbound (display layer loaded standalone in a test) or holds any
non-streaming value (`waiting', `error', `aborted', nil)."
  (and (boundp 'gptel-chat--lifecycle-state)
       (memq gptel-chat--lifecycle-state
             gptel-chat--display-streaming-states)))

(defun gptel-chat--display-schedule-refresh (&rest _args)
  "Schedule a debounced overlay refresh for the current buffer.
Intended for `after-change-functions'; the three positional args are
ignored.  Cancels any pending timer so rapid edits collapse into one
refresh after `gptel-chat-display-refresh-delay' seconds of idle
time.

Skips scheduling while the buffer has a streaming or tool-running
request in flight (see `gptel-chat--display-streaming-p').  Streaming
chunks fire `after-change-functions' on every inserted token; a
full-buffer reparse per chunk is expensive and causes overlay
flicker.  The refresh is deferred until the `DONE' handler
(`gptel-chat--display-refresh-on-done') fires one final reparse."
  (when (and gptel-chat--display-enabled
             (not (gptel-chat--display-streaming-p)))
    (when (timerp gptel-chat--display-refresh-timer)
      (cancel-timer gptel-chat--display-refresh-timer))
    (let ((buf (current-buffer)))
      (setq gptel-chat--display-refresh-timer
            (run-with-idle-timer
             gptel-chat-display-refresh-delay nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq gptel-chat--display-refresh-timer nil)
                   (gptel-chat--refresh-overlays)))))))))
;; Debounced after-change callback:1 ends here

;; Post-stream refresh

;; When a streaming request finishes, the display layer needs a single
;; refresh to reflect all the content that arrived while scheduling was
;; suppressed. =gptel-chat--on-done= (defined in =send.el=) fires on
;; the terminal =DONE= transition; advising it with this helper keeps
;; the coupling one-way (display depends on send — never the reverse)
;; and load-order-independent via =with-eval-after-load=.

;; The helper reads the request buffer from the FSM's =info= plist
;; (the same contract =gptel-chat--set-lifecycle-state= uses) so a
;; dead or detached FSM is a silent no-op.


;; [[file:display.org::*Post-stream refresh][Post-stream refresh:1]]
(defun gptel-chat--display-refresh-on-done (fsm)
  "Refresh display-layer overlays once after FSM reaches DONE.
Intended as `:after' advice on `gptel-chat--on-done'.  Reads the
originating buffer from FSM's `info' plist (key `:buffer') — the
same lookup `gptel-chat--set-lifecycle-state' uses — and fires a
single overlay refresh so the buffer reflects all chunks that
arrived while `gptel-chat--display-schedule-refresh' was
suppressed during streaming.  design.md §Decision 5 Risks."
  (when-let* ((info (and (fboundp 'gptel-fsm-info)
                         (gptel-fsm-info fsm)))
              (buf  (plist-get info :buffer))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (when gptel-chat--display-enabled
        (gptel-chat--refresh-overlays)))))

(with-eval-after-load 'gptel-chat-send
  (advice-add 'gptel-chat--on-done :after
              #'gptel-chat--display-refresh-on-done))
;; Post-stream refresh:1 ends here

;; Hook install / uninstall

;; Two helpers keep =after-change-functions= membership symmetric. The
;; hook list is buffer-local so turning the layer off in one chat buffer
;; does not affect another chat buffer.


;; [[file:display.org::*Hook install / uninstall][Hook install / uninstall:1]]
(defun gptel-chat--display-install-hooks ()
  "Install the debounced-refresh hook on `after-change-functions' (buffer-local)."
  (add-hook 'after-change-functions
            #'gptel-chat--display-schedule-refresh nil t))

(defun gptel-chat--display-uninstall-hooks ()
  "Remove the debounced-refresh hook and cancel any pending timer."
  (remove-hook 'after-change-functions
               #'gptel-chat--display-schedule-refresh t)
  (when (timerp gptel-chat--display-refresh-timer)
    (cancel-timer gptel-chat--display-refresh-timer)
    (setq gptel-chat--display-refresh-timer nil)))
;; Hook install / uninstall:1 ends here

;; Mode-activation entry point

;; =gptel-chat-mode-hook= runs at activation. The entry point installs
;; the after-change hook and performs an initial pass so overlays are
;; present immediately (spec scenario "Display layer is active by
;; default") without waiting for the first edit.

;; It also registers =gptel-chat--display-uninstall-hooks= on the
;; buffer-local =kill-buffer-hook=. A pending idle timer captures the
;; buffer in its closure; if the buffer is killed before the timer
;; fires, the closure runs =buffer-live-p= and no-ops — but the timer
;; itself lingers on =timer-list= until GC. Cancelling it on kill
;; keeps =timer-list= tidy and matches the uninstall contract used by
;; the toggle command.


;; [[file:display.org::*Mode-activation entry point][Mode-activation entry point:1]]
(defun gptel-chat--display-activate ()
  "Activate the display layer in the current buffer.
Installs the after-change hook, runs a full overlay refresh, and
ensures `gptel-chat--display-enabled' starts in its default
truthy state.  Also registers `gptel-chat--display-uninstall-hooks'
on the buffer-local `kill-buffer-hook' so a pending idle timer is
cancelled (and removed from `timer-list') when the buffer is
killed — a no-op closure firing post-kill is harmless but the
timer entry lingers until GC without this cleanup."
  (unless gptel-chat--display-enabled
    (setq gptel-chat--display-enabled t))
  (gptel-chat--display-install-hooks)
  (add-hook 'kill-buffer-hook
            #'gptel-chat--display-uninstall-hooks nil t)
  (gptel-chat--refresh-overlays))

(add-hook 'gptel-chat-mode-hook #'gptel-chat--display-activate)
;; Mode-activation entry point:1 ends here

;; Toggle command

;; Flip =gptel-chat--display-enabled= and either install+refresh or
;; uninstall+remove. The command is interactive so users can bind it (the
;; mode keymap already binds =C-c C-t= to it).


;; [[file:display.org::*Toggle command][Toggle command:1]]
;;;###autoload
(defun gptel-chat-toggle-display-layer ()
  "Toggle the `gptel-chat-mode' display layer in the current buffer.

When enabled, role overlays subtly distinguish user and assistant
turn bodies (see `gptel-chat-user-face' /
`gptel-chat-assistant-face').  When disabled, overlays are removed
and the after-change refresher is uninstalled — the buffer shows
plain text.  Buffer contents are never modified by either state."
  (interactive)
  (setq gptel-chat--display-enabled (not gptel-chat--display-enabled))
  (cond
   (gptel-chat--display-enabled
    (gptel-chat--display-install-hooks)
    (gptel-chat--refresh-overlays)
    (message "gptel-chat display layer enabled"))
   (t
    (gptel-chat--display-uninstall-hooks)
    (gptel-chat--display-remove-all)
    (message "gptel-chat display layer disabled"))))
;; Toggle command:1 ends here

;; Provide


;; [[file:display.org::*Provide][Provide:1]]
(provide 'gptel-chat-display)

;;; display.el ends here
;; Provide:1 ends here
