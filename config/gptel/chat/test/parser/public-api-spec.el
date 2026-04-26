;;; public-api-spec.el --- Public-contract tests for the parser API -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs that pin the public API contract for the
;; `gptel-chat-parser' subsystem, as documented in
;; openspec/changes/persistent-agent-rebuild/specs/chat-mode/spec.md
;; (delta) §"Public programmatic-send API".
;;
;; These specs treat the parser as a black box from a non-chat-mode
;; caller's perspective.  They exercise:
;;
;;   - `gptel-chat-parse-buffer'      — callable without chat-mode
;;                                       being active in the buffer.
;;   - `gptel-chat-turns-to-messages' — produces the cons-list shape
;;                                       `gptel-request' accepts as
;;                                       its `:prompt' argument.
;;   - Absence of double-dash internal aliases for both names — i.e.,
;;     the rename is complete and no `defalias' shim remains.
;;
;; Detailed parser behaviour (delimiter validation, segment shapes,
;; tool-call extraction) is covered by `buffer-format-spec.el' and
;; `message-construction-spec.el'.  This file pins only the public
;; surface contract.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared fixtures (sibling of this file's parent directory).
(let ((helpers (expand-file-name
                "../test-helpers.el"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load helpers nil t))

;; Load the module under test.
(require 'gptel-chat-parser)

;;; Tests

(describe "gptel-chat parser public API"

  (describe "gptel-chat-parse-buffer"

    (it "is callable from a non-chat-mode buffer"
      ;; Scenario: specs/chat-mode/spec.md (delta) § "Public
      ;; programmatic-send API" → "Public parse function callable
      ;; from outside chat-mode"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "Hello\n"
                  "#+end_user\n"
                  "#+begin_assistant\n"
                  "Hi\n"
                  "#+end_assistant\n")
        ;; The fixture deliberately does NOT activate `gptel-chat-mode';
        ;; the parser must work on a plain buffer for non-interactive
        ;; callers (e.g., `PersistentAgent') to drive it.
        (expect (derived-mode-p 'gptel-chat-mode) :to-be nil)
        (let ((turns (gptel-chat-parse-buffer)))
          (expect (listp turns) :to-be-truthy)
          (expect (length turns) :to-equal 2)
          ;; Turn-plist shape: each turn carries `:role', start/end
          ;; markers, and either `:content' (user) or `:segments'
          ;; (assistant).
          (let ((user (nth 0 turns))
                (asst (nth 1 turns)))
            (expect (plist-get user :role)      :to-equal 'user)
            (expect (plist-get user :content)   :to-equal "Hello\n")
            (expect (markerp (plist-get user :start)) :to-be-truthy)
            (expect (markerp (plist-get user :end))   :to-be-truthy)
            (expect (plist-get asst :role)      :to-equal 'assistant)
            (expect (listp (plist-get asst :segments)) :to-be-truthy)
            (expect (markerp (plist-get asst :start)) :to-be-truthy)
            (expect (markerp (plist-get asst :end))   :to-be-truthy)))))

    (it "produces gptel-request prompt-shaped output"
      ;; Scenario: specs/chat-mode/spec.md (delta) § "Public
      ;; programmatic-send API" → "Public turns-to-messages produces
      ;; gptel-request input"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "Hello\n"
                  "#+end_user\n"
                  "#+begin_assistant\n"
                  "Hi\n"
                  "#+end_assistant\n"
                  "#+begin_user\n"
                  "Goodbye\n"
                  "#+end_user\n")
        (let* ((turns (gptel-chat-parse-buffer))
               (messages (gptel-chat-turns-to-messages turns)))
          ;; The converter yields a cons-list — one cons per
          ;; non-empty turn / segment — in document order.  Each
          ;; entry's car is a tag symbol and its cdr is either a
          ;; string (for `prompt' / `response') or a plist (for
          ;; `tool').
          (expect (listp messages) :to-be-truthy)
          (expect (length messages) :to-equal 3)
          ;; Document-order preservation.
          (expect (car (nth 0 messages)) :to-equal 'prompt)
          (expect (car (nth 1 messages)) :to-equal 'response)
          (expect (car (nth 2 messages)) :to-equal 'prompt)
          ;; Every entry conforms to one of the documented shapes.
          (dolist (entry messages)
            (expect (consp entry) :to-be-truthy)
            (let ((tag (car entry))
                  (val (cdr entry)))
              (expect (memq tag '(prompt response tool)) :to-be-truthy)
              (cond
               ((memq tag '(prompt response))
                (expect (stringp val) :to-be-truthy))
               ((eq tag 'tool)
                ;; A tool entry carries a plist with the documented
                ;; keys; round-trip into `plist-get' must succeed.
                (expect (or (listp val) (plistp val)) :to-be-truthy))))))))

    (it "exposes no double-dash aliases after load"
      ;; Scenario: specs/chat-mode/spec.md (delta) § "Public
      ;; programmatic-send API" → "No double-dash internal aliases
      ;; remain"
      (expect (fboundp 'gptel-chat--parse-buffer)      :to-be nil)
      (expect (fboundp 'gptel-chat--turns-to-messages) :to-be nil))))

(provide 'public-api-spec)

;;; public-api-spec.el ends here
