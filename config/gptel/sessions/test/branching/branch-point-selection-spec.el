;;; branch-point-selection-spec.el --- Buttercup tests for branch-point selection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `jf/gptel--branching-user-turns',
;; `jf/gptel--branching-turn-branch-point',
;; `jf/gptel--branching-turn-label', and
;; `jf/gptel--branching-select-branch-point'.
;;
;; Coverage (from
;; openspec/changes/gptel-chat-mode/specs/gptel/sessions-branching.md,
;; MODIFIED Requirement: Branch point selection):
;;   - Interactive turn selection presents a numbered list of user blocks.
;;   - Include → branch point is after #+end_user of the selected turn.
;;   - Exclude → branch point is before #+begin_user of the selected turn.
;;   - Assistant blocks, tool blocks, headings, and prose are NOT selectable.
;;   - No valid branch points → reports "no available branch points".

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load production modules under test.
(require 'gptel-chat-parser)
(require 'gptel-session-branching)

(defmacro jf-branching-test--with-buffer (content &rest body)
  "Insert CONTENT into a fresh temp buffer and evaluate BODY inside it."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Small content builders.

(defconst jf-branching-test--two-user-turns
  (concat "#+begin_user\n"
          "First question?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "First answer.\n"
          "#+end_assistant\n"
          "\n"
          "#+begin_user\n"
          "Second question?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Second answer.\n"
          "#+end_assistant\n"))

(defconst jf-branching-test--prose-and-headings
  (concat "* Section A\n"
          "\n"
          "Notes the human keeps before any turn.\n"
          "\n"
          "#+begin_user\n"
          "Only user turn.\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "An answer.\n"
          "#+end_assistant\n"
          "\n"
          "* Section B\n"
          "\n"
          "More prose.\n"))

(defconst jf-branching-test--assistant-and-tool-only
  (concat "#+begin_assistant\n"
          "Hello.\n"
          "\n"
          "#+begin_tool (run_bash_command :command \"uname\")\n"
          "{\"success\":true,\"output\":\"Darwin\\n\"}\n"
          "#+end_tool\n"
          "\n"
          "Done.\n"
          "#+end_assistant\n"))

;;; Tests.

(describe "jf/gptel--branching-user-turns"

  (it "returns all outer user turns in document order"
    (jf-branching-test--with-buffer jf-branching-test--two-user-turns
      (let ((turns (jf/gptel--branching-user-turns)))
        (expect (length turns) :to-equal 2)
        (expect (plist-get (nth 0 turns) :role) :to-equal 'user)
        (expect (plist-get (nth 1 turns) :role) :to-equal 'user)
        (expect (string-trim (plist-get (nth 0 turns) :content))
                :to-equal "First question?")
        (expect (string-trim (plist-get (nth 1 turns) :content))
                :to-equal "Second question?"))))

  (it "excludes assistant blocks, tool blocks, headings, and prose"
    ;; Assistant-only buffer with a nested tool block and prose content
    ;; must yield an empty user-turn list — assistant blocks, tool
    ;; blocks, and prose do not count.
    (jf-branching-test--with-buffer jf-branching-test--assistant-and-tool-only
      (expect (jf/gptel--branching-user-turns) :to-equal nil)))

  (it "ignores org headings and prose outside user turns"
    (jf-branching-test--with-buffer jf-branching-test--prose-and-headings
      (let ((turns (jf/gptel--branching-user-turns)))
        (expect (length turns) :to-equal 1)
        (expect (string-trim (plist-get (car turns) :content))
                :to-equal "Only user turn."))))

  (it "returns nil for an empty buffer"
    (jf-branching-test--with-buffer ""
      (expect (jf/gptel--branching-user-turns) :to-equal nil))))

(describe "jf/gptel--branching-turn-label"

  (it "uses the first non-empty line of the turn content"
    (jf-branching-test--with-buffer jf-branching-test--two-user-turns
      (let ((turns (jf/gptel--branching-user-turns)))
        (expect (jf/gptel--branching-turn-label (nth 0 turns))
                :to-equal "First question?")
        (expect (jf/gptel--branching-turn-label (nth 1 turns))
                :to-equal "Second question?"))))

  (it "truncates long lines to 60 characters with an ellipsis"
    (let* ((long-body (make-string 80 ?x))
           (turn (list :role 'user :content long-body)))
      (let ((label (jf/gptel--branching-turn-label turn)))
        (expect (length label) :to-equal 60)
        (expect (substring label 57) :to-equal "..."))))

  (it "labels a whitespace-only turn as (empty)"
    (let ((turn (list :role 'user :content "   \n\t \n")))
      (expect (jf/gptel--branching-turn-label turn) :to-equal "(empty)"))))

(describe "jf/gptel--branching-turn-branch-point"

  (it "returns the position immediately after #+end_user for INCLUDE"
    (jf-branching-test--with-buffer jf-branching-test--two-user-turns
      (let* ((turns (jf/gptel--branching-user-turns))
             (first (nth 0 turns))
             (pos (jf/gptel--branching-turn-branch-point first t)))
        ;; INCLUDE: position lies on the line right after the
        ;; `#+end_user' line — i.e., the character preceding POS is a
        ;; newline that closes the `#+end_user' line.
        (expect (save-excursion
                  (goto-char pos)
                  (beginning-of-line)
                  (point))
                :to-equal pos)
        (expect (save-excursion
                  (goto-char pos)
                  (forward-line -1)
                  (buffer-substring-no-properties
                   (point)
                   (line-end-position)))
                :to-equal "#+end_user"))))

  (it "returns the position immediately before #+begin_user for EXCLUDE"
    (jf-branching-test--with-buffer jf-branching-test--two-user-turns
      (let* ((turns (jf/gptel--branching-user-turns))
             (second (nth 1 turns))
             (pos (jf/gptel--branching-turn-branch-point second nil)))
        ;; EXCLUDE: position is the start of the `#+begin_user' line.
        (expect (save-excursion
                  (goto-char pos)
                  (buffer-substring-no-properties
                   (point)
                   (line-end-position)))
                :to-equal "#+begin_user"))))

  (it "INCLUDE position is greater than EXCLUDE position for the same turn"
    (jf-branching-test--with-buffer jf-branching-test--two-user-turns
      (let* ((turns (jf/gptel--branching-user-turns))
             (turn (nth 0 turns))
             (include-pos (jf/gptel--branching-turn-branch-point turn t))
             (exclude-pos (jf/gptel--branching-turn-branch-point turn nil)))
        (expect include-pos :to-be-greater-than exclude-pos)))))

(describe "jf/gptel--branching-select-branch-point"

  (it "signals a user-error when no outer user turns are present"
    (jf-branching-test--with-buffer jf-branching-test--assistant-and-tool-only
      (expect (jf/gptel--branching-select-branch-point)
              :to-throw 'user-error)))

  (it "signals a user-error for an empty buffer"
    (jf-branching-test--with-buffer ""
      (expect (jf/gptel--branching-select-branch-point)
              :to-throw 'user-error)))

  (it "presents a numbered list of user-turn labels to completing-read"
    (jf-branching-test--with-buffer jf-branching-test--two-user-turns
      (let (captured-choices)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (setq captured-choices choices)
                     (car choices)))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (jf/gptel--branching-select-branch-point)
          (expect captured-choices :to-equal
                  '("1. First question?" "2. Second question?"))))))

  (it "returns the INCLUDE branch point when the user answers yes"
    (jf-branching-test--with-buffer jf-branching-test--two-user-turns
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   ;; Pick the second turn.
                   (nth 1 choices)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (let* ((result (jf/gptel--branching-select-branch-point))
               (pos (car result))
               (include-p (cdr result)))
          (expect include-p :to-be-truthy)
          ;; INCLUDE lands immediately after the `#+end_user' line of
          ;; the *second* user turn.
          (expect (save-excursion
                    (goto-char pos)
                    (forward-line -1)
                    (buffer-substring-no-properties
                     (point)
                     (line-end-position)))
                  :to-equal "#+end_user")))))

  (it "returns the EXCLUDE branch point when the user answers no"
    (jf-branching-test--with-buffer jf-branching-test--two-user-turns
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (car choices)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (let* ((result (jf/gptel--branching-select-branch-point))
               (pos (car result))
               (include-p (cdr result)))
          (expect include-p :not :to-be-truthy)
          ;; EXCLUDE lands at the `#+begin_user' line of the first turn.
          (expect (save-excursion
                    (goto-char pos)
                    (buffer-substring-no-properties
                     (point)
                     (line-end-position)))
                  :to-equal "#+begin_user"))))))

(provide 'branch-point-selection-spec)
;;; branch-point-selection-spec.el ends here
