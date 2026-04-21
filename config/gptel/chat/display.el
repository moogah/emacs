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

;; Subtle defaults: a slight background tint per role. Users who want a
;; different treatment (foreground shift, line-prefix, box, etc.)
;; customize via =M-x customize-face RET gptel-chat-user-face RET=.

;; The dark/light background variants keep contrast readable under both
;; theme families without hard-coding a specific theme's palette.


;; [[file:display.org::*Faces][Faces:1]]
(defface gptel-chat-user-face
  '((((background dark))  :background "#1a2030" :extend t)
    (((background light)) :background "#f0f4ff" :extend t))
  "Face applied to the body region of `#+begin_user' blocks.
Delimiter lines are NOT covered by this face."
  :group 'gptel-chat)

(defface gptel-chat-assistant-face
  '((((background dark))  :background "#1a2a1a" :extend t)
    (((background light)) :background "#f0fff4" :extend t))
  "Face applied to the body region of `#+begin_assistant' blocks.
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
TURN is a turn plist as returned by `gptel-chat--parse-buffer'.  The
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

;; Refresh — the main entry point

;; Walk the turn list and re-install role overlays. The refresh is
;; idempotent: it clears existing =gptel-chat-display= overlays before
;; installing new ones, so calling it repeatedly converges on the correct
;; state.

;; If =gptel-chat--parse-buffer= raises a =user-error= (the buffer is
;; structurally invalid — unclosed block, turn-inside-turn, etc.) we
;; swallow it here: partial typing of a block should not blow up a
;; silent background refresh. The refresh simply does nothing until the
;; buffer becomes parseable again.


;; [[file:display.org::*Refresh — the main entry point][Refresh — the main entry point:1]]
(defun gptel-chat--refresh-overlays (&optional buffer)
  "Refresh role overlays in BUFFER (default current).
Removes existing `gptel-chat-display' overlays and installs fresh ones
spanning the body of each user / assistant turn.  A `user-error' from
the parser (mid-edit buffer with an unclosed block) is caught and
treated as a no-op — the next refresh will reinstall overlays once
the buffer reaches a parseable state."
  (with-current-buffer (or buffer (current-buffer))
    (when gptel-chat--display-enabled
      (gptel-chat--display-remove-all)
      (condition-case _err
          (let ((turns (gptel-chat--parse-buffer)))
            (dolist (turn turns)
              (when-let* ((range (gptel-chat--display-body-range turn))
                          (face (pcase (plist-get turn :role)
                                  (`user      'gptel-chat-user-face)
                                  (`assistant 'gptel-chat-assistant-face))))
                (gptel-chat--display-make-overlay
                 (car range) (cdr range) face))))
        (user-error nil)))))
;; Refresh — the main entry point:1 ends here

;; Debounced after-change callback

;; =after-change-functions= runs synchronously on every edit. Rather
;; than reparse on each keystroke, schedule a single idle-timer refresh
;; and cancel any prior pending timer so rapid edits collapse into one
;; reparse. The timer callback captures the buffer so it still runs
;; correctly if the user switches windows before idle kicks in.


;; [[file:display.org::*Debounced after-change callback][Debounced after-change callback:1]]
(defun gptel-chat--display-schedule-refresh (&rest _args)
  "Schedule a debounced overlay refresh for the current buffer.
Intended for `after-change-functions'; the three positional args are
ignored.  Cancels any pending timer so rapid edits collapse into one
refresh after `gptel-chat-display-refresh-delay' seconds of idle
time."
  (when gptel-chat--display-enabled
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


;; [[file:display.org::*Mode-activation entry point][Mode-activation entry point:1]]
(defun gptel-chat--display-activate ()
  "Activate the display layer in the current buffer.
Installs the after-change hook, runs a full overlay refresh, and
ensures `gptel-chat--display-enabled' starts in its default
truthy state."
  (unless gptel-chat--display-enabled
    (setq gptel-chat--display-enabled t))
  (gptel-chat--display-install-hooks)
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
