;;; nav.el --- GPTEL Chat-Mode Turn Navigation -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Implements `gptel-chat-next-turn', `gptel-chat-previous-turn',
;; and `gptel-chat-regenerate' for `gptel-chat-mode' buffers.
;;
;; The commands delegate to `gptel-chat--parse-buffer' for turn-list
;; extraction rather than duplicating the parser state machine.  The
;; keymap that binds these commands lives in `mode.el'
;; (`gptel-chat-mode-map'): `C-c n' / `C-c p' for navigation and
;; `C-c C-r' for regenerate.  Single-`C-c' navigation is deliberate
;; so org's `C-c C-n' / `C-c C-p' heading navigation remains bound
;; (design.md §Decision 7).

;;; Code:

(require 'cl-lib)
(require 'gptel-chat-parser)

;; Forward declarations

;; =gptel-chat-regenerate= calls =gptel-chat-send= (the send command,
;; owned by the =send-command= task). That module may load after this
;; one, and in batch/test contexts the symbol may be stubbed via a spy.
;; =declare-function= suppresses byte-compiler warnings without making
;; claims about a specific source library.


;; [[file:nav.org::*Forward declarations][Forward declarations:1]]
(declare-function gptel-chat-send "send" ())
;; Forward declarations:1 ends here

;; Turn-position helper

;; Both navigation commands and regenerate extract turn start/end
;; positions from the parser output. The parser returns markers; for
;; point comparison we want integer positions. =marker-position= returns
;; =nil= if a marker has been cleared, which would be surprising here —
;; but a belt-and-suspenders filter keeps the comparison logic simple.


;; [[file:nav.org::*Turn-position helper][Turn-position helper:1]]
(defun gptel-chat-nav--turn-start (turn)
  "Return the integer position of TURN's `:start' marker, or nil."
  (when-let ((m (plist-get turn :start)))
    (marker-position m)))

(defun gptel-chat-nav--turn-end (turn)
  "Return the integer position of TURN's `:end' marker, or nil."
  (when-let ((m (plist-get turn :end)))
    (marker-position m)))
;; Turn-position helper:1 ends here

;; Containing-turn helper

;; Navigation has to distinguish "inside a turn" from "outside a turn"
;; because the spec requires navigation to *skip* the containing turn
;; (previous-turn from inside an assistant goes to the preceding user,
;; not to the assistant's own start).

;; A turn contains point when its =:start= ≤ point ≤ end-of-closing-line.
;; The parser's =:end= marker points at the start of the
;; =#+end_<kind>= delimiter line, so "end of the block" for containment
;; purposes is the end of that delimiter line.


;; [[file:nav.org::*Containing-turn helper][Containing-turn helper:1]]
(defun gptel-chat-nav--turn-contains-p (turn pos)
  "Return non-nil when POS lies within TURN (inclusive of delimiter lines)."
  (let ((start (gptel-chat-nav--turn-start turn))
        (body-end (gptel-chat-nav--turn-end turn)))
    (when (and start body-end)
      (let ((block-end (save-excursion
                         (goto-char body-end)
                         (line-end-position))))
        (and (<= start pos) (<= pos block-end))))))

(defun gptel-chat-nav--containing-turn (turns pos)
  "Return the element of TURNS containing POS, or nil."
  (cl-find-if (lambda (turn) (gptel-chat-nav--turn-contains-p turn pos))
              turns))
;; Containing-turn helper:1 ends here

;; Next turn

;; Parse the buffer. If point is inside a turn, the next turn is the
;; first turn whose start is strictly greater than that turn's end.
;; Otherwise (point is in prose/headings between turns), the next turn
;; is the first turn whose start is strictly greater than point. If
;; none exists, emit "No next turn" and leave point unchanged.


;; [[file:nav.org::*Next turn][Next turn:1]]
;;;###autoload
(defun gptel-chat-next-turn ()
  "Move point to the start of the next outer turn block.

Turns are identified by the chat-mode parser, so anything outside a
turn (headings, prose, drawers, `#+keyword:' lines) is skipped.
When point is inside a turn block, the command jumps *past* that
turn to the next one.  Signals a user-visible `No next turn'
message and leaves point unchanged when no further turn exists."
  (interactive)
  (let* ((here (point))
         (turns (gptel-chat--parse-buffer))
         (current (gptel-chat-nav--containing-turn turns here))
         (threshold (if current
                        (gptel-chat-nav--turn-end current)
                      here))
         (next (cl-find-if
                (lambda (turn)
                  (let ((start (gptel-chat-nav--turn-start turn)))
                    (and start (> start threshold))))
                turns)))
    (if next
        (goto-char (gptel-chat-nav--turn-start next))
      (message "No next turn"))))
;; Next turn:1 ends here

;; Previous turn

;; Symmetric to =gptel-chat-next-turn=. If point is inside a turn, the
;; previous turn is the last turn whose start is strictly less than that
;; turn's start. Otherwise (point outside any turn), the previous turn
;; is the last turn whose start is strictly less than point. Because the
;; turn list is in document order, =cl-find-if= with =:from-end t=
;; returns the closest-but-before match without building a reversed list.


;; [[file:nav.org::*Previous turn][Previous turn:1]]
;;;###autoload
(defun gptel-chat-previous-turn ()
  "Move point to the start of the previous outer turn block.

Turns are identified by the chat-mode parser, so anything outside a
turn (headings, prose, drawers, `#+keyword:' lines) is skipped.
When point is inside a turn block, the command jumps *before* that
turn to the preceding one.  Signals a user-visible `No previous
turn' message and leaves point unchanged when no earlier turn
exists."
  (interactive)
  (let* ((here (point))
         (turns (gptel-chat--parse-buffer))
         (current (gptel-chat-nav--containing-turn turns here))
         (threshold (if current
                        (gptel-chat-nav--turn-start current)
                      here))
         (prev (cl-find-if
                (lambda (turn)
                  (let ((start (gptel-chat-nav--turn-start turn)))
                    (and start (< start threshold))))
                turns
                :from-end t)))
    (if prev
        (goto-char (gptel-chat-nav--turn-start prev))
      (message "No previous turn"))))
;; Previous turn:1 ends here

;; Regenerate last response

;; Parse the buffer and inspect the last turn:

;; - Empty turn list → no response to regenerate.
;; - Last turn is a user block (no response yet) → same.
;; - Last turn is an assistant block → delete from its =:start= marker to
;;   the end of its =#+end_assistant= line (inclusive of the trailing
;;   newline, when one exists) and invoke =gptel-chat-send= which
;;   re-issues the request for the preceding user turn.

;; The parser's =:end= marker points at the start of the =#+end_assistant=
;; delimiter line. We extend the deletion range to the end of that line
;; (and the following newline, when present) so the block is removed
;; cleanly without leaving a dangling =#+end_assistant= or an empty line.


;; [[file:nav.org::*Regenerate last response][Regenerate last response:1]]
(defun gptel-chat-nav--delete-assistant-block (turn)
  "Delete the assistant-block TURN from the current buffer.

Removes the region from TURN's `:start' marker through the end of the
matching `#+end_assistant' line (including the trailing newline when
one is present)."
  (let* ((start (gptel-chat-nav--turn-start turn))
         (body-end (gptel-chat-nav--turn-end turn))
         (end-line-end
          (save-excursion
            (goto-char body-end)
            (line-end-position)))
         ;; Extend past the newline after `#+end_assistant' when present,
         ;; so the block is removed cleanly.
         (del-end (min (point-max) (1+ end-line-end))))
    (delete-region start del-end)
    (goto-char start)))

;;;###autoload
(defun gptel-chat-regenerate ()
  "Regenerate the last assistant response in the current buffer.

When the buffer's last turn is an assistant block, delete it
(including the closing `#+end_assistant' line) and re-send the
buffer so the request for the preceding user turn is re-issued.

When the buffer has no turns, or the last turn is an unanswered
user block, emit `No response to regenerate' and leave the buffer
unchanged."
  (interactive)
  (let* ((turns (gptel-chat--parse-buffer))
         (last (car (last turns))))
    (cond
     ((null last)
      (message "No response to regenerate"))
     ((eq (plist-get last :role) 'assistant)
      (gptel-chat-nav--delete-assistant-block last)
      (gptel-chat-send))
     (t
      (message "No response to regenerate")))))
;; Regenerate last response:1 ends here

;; Provide


;; [[file:nav.org::*Provide][Provide:1]]
(provide 'gptel-chat-nav)

;;; nav.el ends here
;; Provide:1 ends here
