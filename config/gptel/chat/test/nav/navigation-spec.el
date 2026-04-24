;;; navigation-spec.el --- Buttercup tests for gptel-chat turn navigation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat-next-turn' and
;; `gptel-chat-previous-turn'.
;;
;; Coverage (from openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md
;; §"Turn navigation"):
;;   - Next turn from inside a user block moves to the following assistant.
;;   - Previous turn from inside an assistant block moves to the preceding user.
;;   - Next turn at end-of-buffer emits "No next turn" and doesn't move.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/nav/;
;; two levels up is .../config/gptel/chat/, which holds `nav.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-parser)
(require 'gptel-chat-nav)


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-nav-test--two-turn-buffer
  (concat "#+begin_user\n"
          "What's the capital of France?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Paris.\n"
          "#+end_assistant\n")
  "Buffer with one completed user/assistant turn pair.")

(defconst gptel-chat-nav-test--three-turn-buffer
  (concat "#+begin_user\n"
          "First question.\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "First answer.\n"
          "#+end_assistant\n"
          "\n"
          "#+begin_user\n"
          "Second question.\n"
          "#+end_user\n")
  "Buffer with user, assistant, then trailing unanswered user.")

(defun gptel-chat-nav-test--goto-substring (needle)
  "Move point to the start of NEEDLE within the current buffer."
  (goto-char (point-min))
  (search-forward needle)
  (goto-char (match-beginning 0)))

(defmacro gptel-chat-nav-test--with-buffer (content &rest body)
  "Insert CONTENT in a fresh temp buffer and evaluate BODY there.
Point starts at `point-min'."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))


;;; Tests --------------------------------------------------------------------

(describe "gptel-chat-next-turn"

  (it "moves point from inside a user block to the following assistant block"
    (gptel-chat-nav-test--with-buffer
        gptel-chat-nav-test--two-turn-buffer
      ;; Position point in the middle of the user block's body.
      (gptel-chat-nav-test--goto-substring "capital")
      (gptel-chat-next-turn)
      (expect (looking-at "^#\\+begin_assistant\\b") :to-be-truthy)))

  (it "moves point from the first user block to the second user block via the assistant"
    ;; Two consecutive next-turn presses should visit the assistant and
    ;; then the trailing user block.
    (gptel-chat-nav-test--with-buffer
        gptel-chat-nav-test--three-turn-buffer
      (gptel-chat-nav-test--goto-substring "First question.")
      (gptel-chat-next-turn)
      (expect (looking-at "^#\\+begin_assistant\\b") :to-be-truthy)
      (gptel-chat-next-turn)
      (expect (looking-at "^#\\+begin_user\\b") :to-be-truthy)))

  (it "emits \"No next turn\" and does not move when point is after the last turn"
    (gptel-chat-nav-test--with-buffer
        gptel-chat-nav-test--two-turn-buffer
      (goto-char (point-max))
      (let* ((before (point))
             (msg nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq msg (apply #'format fmt args)))))
          (gptel-chat-next-turn))
        (expect (point) :to-equal before)
        (expect msg :to-equal "No next turn"))))

  (it "emits \"No next turn\" in a buffer with no turns"
    (gptel-chat-nav-test--with-buffer
        "* Heading\n\nJust prose, no turn blocks here.\n"
      (let* ((before (point))
             (msg nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq msg (apply #'format fmt args)))))
          (gptel-chat-next-turn))
        (expect (point) :to-equal before)
        (expect msg :to-equal "No next turn")))))


(describe "gptel-chat-previous-turn"

  (it "moves point from inside an assistant block to the preceding user block"
    (gptel-chat-nav-test--with-buffer
        gptel-chat-nav-test--two-turn-buffer
      (gptel-chat-nav-test--goto-substring "Paris.")
      (gptel-chat-previous-turn)
      (expect (looking-at "^#\\+begin_user\\b") :to-be-truthy)))

  (it "emits \"No previous turn\" and does not move when point is before the first turn"
    (gptel-chat-nav-test--with-buffer
        (concat "* Prologue\n\n" gptel-chat-nav-test--two-turn-buffer)
      ;; Put point at the very start, before any turn block.
      (goto-char (point-min))
      (let* ((before (point))
             (msg nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq msg (apply #'format fmt args)))))
          (gptel-chat-previous-turn))
        (expect (point) :to-equal before)
        (expect msg :to-equal "No previous turn"))))

  (it "walks backwards through turns in document order"
    (gptel-chat-nav-test--with-buffer
        gptel-chat-nav-test--three-turn-buffer
      ;; Start in the trailing user block.
      (gptel-chat-nav-test--goto-substring "Second question.")
      (gptel-chat-previous-turn)
      (expect (looking-at "^#\\+begin_assistant\\b") :to-be-truthy)
      (gptel-chat-previous-turn)
      (expect (looking-at "^#\\+begin_user\\b") :to-be-truthy))))


(provide 'gptel-chat-navigation-spec)

;;; navigation-spec.el ends here
