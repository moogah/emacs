;;; public-api-spec.el --- Public-contract tests for the send API -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs that pin the public API contract for the
;; `gptel-chat-send' subsystem, as documented in
;; openspec/changes/persistent-agent-rebuild/specs/chat-mode/spec.md
;; (delta) §"Public programmatic-send API".
;;
;; These specs treat the send module as a black box from a
;; non-interactive caller's perspective.  They exercise:
;;
;;   - `gptel-chat-open-assistant-block' — opens an assistant block
;;                                          and returns a live
;;                                          advance-insertion marker.
;;   - `gptel-chat-fsm-handlers'         — variable suitable for
;;                                          passing as `:handlers' to
;;                                          `gptel-make-fsm'.
;;   - Absence of double-dash internal aliases for both names.
;;
;; Detailed send-command behaviour (in-flight guard, point resolution,
;; gptel-request invocation) is covered by `send-command-spec.el' and
;; `backend-invocation-spec.el'.  This file pins only the public
;; surface contract.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared fixtures (sibling of this file's parent directory).
(let ((helpers (expand-file-name
                "../test-helpers.el"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load helpers nil t))

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/send/;
;; two levels up is .../config/gptel/chat/.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel)
(require 'gptel-chat-parser)
(require 'gptel-chat-send)

;;; Tests

(describe "gptel-chat send public API"

  (describe "gptel-chat-open-assistant-block"

    (it "opens an assistant block and returns an advance marker"
      ;; Scenario: specs/chat-mode/spec.md (delta) § "Public
      ;; programmatic-send API" → "Public open-assistant-block
      ;; returns insertion marker"
      (gptel-chat-test--with-buffer
          "#+begin_user\nHello\n#+end_user\n"
        (let* ((turns (gptel-chat-parse-buffer))
               (user (car turns))
               (m (gptel-chat-open-assistant-block user))
               (content (buffer-substring-no-properties
                         (point-min) (point-max))))
          ;; (a) Buffer now contains a `#+begin_assistant' line right
          ;; after the user turn's `#+end_user'.  The opener inserts
          ;; a blank line between the closing user delimiter and the
          ;; new assistant header (mirrors send-command-spec).
          (expect (string-match-p
                   "#\\+end_user\n+#\\+begin_assistant\n"
                   content)
                  :to-be-truthy)
          ;; (b) The returned value is a marker.
          (expect (markerp m) :to-be-truthy)
          ;; (c) Insertion-type is t (advance marker), so streamed
          ;; inserts at the marker push it forward and successive
          ;; line inserts land in order.
          (expect (marker-insertion-type m) :to-equal t)
          ;; The marker is live in the test buffer.
          (expect (marker-buffer m) :to-equal (current-buffer))
          ;; (d) The marker is positioned at the line immediately
          ;; after `#+begin_assistant'.  Verify by checking that the
          ;; line *preceding* the marker is exactly `#+begin_assistant'.
          (save-excursion
            (goto-char m)
            (expect (bolp) :to-be-truthy)
            (forward-line -1)
            (expect (buffer-substring-no-properties
                     (point) (line-end-position))
                    :to-equal "#+begin_assistant"))))))

  (describe "gptel-chat-fsm-handlers"

    (it "fsm-handlers exposes the documented per-state chain"
      ;; Scenario: specs/chat-mode/spec.md (delta) § "Public
      ;; programmatic-send API" → "Public fsm-handlers usable with
      ;; gptel-make-fsm"
      ;;
      ;; The variable is an alist keyed by FSM state symbol.  Each
      ;; entry has the chat-mode UI handler first; for every state
      ;; except `TYPE' (UI-only), the chain ends with one of gptel's
      ;; upstream state-driving handlers.
      (expect (boundp 'gptel-chat-fsm-handlers) :to-be-truthy)
      (let ((alist gptel-chat-fsm-handlers))
        (expect (listp alist) :to-be-truthy)
        ;; All six required state keys are present.
        (dolist (key '(WAIT TYPE TOOL DONE ERRS ABRT))
          (expect (assq key alist) :not :to-be nil))
        ;; For each non-TYPE entry, the last element of the chain is
        ;; one of the three documented upstream handlers.  TYPE is
        ;; UI-only and intentionally has no upstream handler in the
        ;; chain.
        (let ((upstream-handlers
               '(gptel--handle-wait
                 gptel--handle-tool-use
                 gptel--handle-post)))
          (dolist (entry alist)
            (let ((state (car entry))
                  (chain (cdr entry)))
              (unless (eq state 'TYPE)
                (expect (memq (car (last chain)) upstream-handlers)
                        :to-be-truthy)))))))

    (it "exposes no double-dash aliases after load"
      ;; Scenario: specs/chat-mode/spec.md (delta) § "Public
      ;; programmatic-send API" → "No double-dash internal aliases
      ;; remain"
      (expect (fboundp 'gptel-chat--open-assistant-block) :to-be nil)
      ;; `gptel-chat--fsm-handlers' is a (former) variable name; check
      ;; both `boundp' and `fboundp' so the test fails on either a
      ;; defalias OR a defvaralias shim.
      (expect (boundp 'gptel-chat--fsm-handlers)  :to-be nil)
      (expect (fboundp 'gptel-chat--fsm-handlers) :to-be nil))))

(provide 'send-public-api-spec)

;;; public-api-spec.el ends here
