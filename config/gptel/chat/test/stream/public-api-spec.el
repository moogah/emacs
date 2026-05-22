;;; public-api-spec.el --- Public-contract tests for the stream API -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs that pin the public API contract for the
;; `gptel-chat-stream' subsystem, as documented in
;; openspec/changes/persistent-agent-rebuild/specs/chat-mode/spec.md
;; (delta) §"Public programmatic-send API".
;;
;; These specs treat the streaming module as a black box from a
;; non-interactive caller's perspective.  They exercise:
;;
;;   - `gptel-chat-stream-callback' — returns a callback suitable
;;                                     for passing as `gptel-request'
;;                                     `:callback'.  The callback
;;                                     inserts at the supplied
;;                                     marker and closes the
;;                                     assistant block on the `t'
;;                                     completion sentinel.
;;   - Absence of a double-dash internal alias for the name.
;;
;; Detailed sanitizer / holdback / abort / error / tool-call
;; behaviour is covered by `streaming-spec.el', `chunk-split-spec.el',
;; `tool-call-spec.el', and `multi-round-tool-use-spec.el'.  This
;; file pins only the public surface contract.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared fixtures (sibling of this file's parent directory).
(let ((helpers (expand-file-name
                "../test-helpers.el"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load helpers nil t))

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/stream/;
;; two levels up is .../config/gptel/chat/.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-parser)
(require 'gptel-chat-send)
(require 'gptel-chat-stream)

;;; Tests

(describe "gptel-chat stream public API"

  (describe "gptel-chat-stream-callback"

    (it "stream callback inserts at the supplied marker"
      ;; Scenario: specs/chat-mode/spec.md (delta) § "Public
      ;; programmatic-send API" → "Public stream-callback usable
      ;; with gptel-request"
      ;;
      ;; Set up a chat-mode buffer with one user turn, open the
      ;; assistant block via the public opener, build the callback
      ;; via the public factory, and drive it as `gptel-request'
      ;; would: a string chunk, then the `t' completion sentinel.
      ;; INFO is the plist `gptel-request' supplies on each call;
      ;; the callback consults `:tool-use' on the `t' arm — leaving
      ;; it unset signals "final round-trip", which closes the block
      ;; and appends a fresh user block.
      (let ((buf (generate-new-buffer " *gptel-chat-stream-public-api*")))
        (unwind-protect
            (with-current-buffer buf
              (insert "#+begin_user\nhi\n#+end_user\n")
              (goto-char (point-min))
              (let* ((turns (gptel-chat-parse-buffer))
                     (user (car turns))
                     (insertion (gptel-chat-open-assistant-block user))
                     (cb (gptel-chat-stream-callback insertion))
                     ;; INFO plist mirrors what `gptel-request'
                     ;; passes through to the callback.  Only
                     ;; `:tool-use' is consulted (on the `t' arm);
                     ;; the other keys are present for
                     ;; documentation and to prove the callback
                     ;; tolerates a populated INFO.
                     (info (list :buffer buf
                                 :position (marker-position insertion)
                                 :tracking-marker insertion)))
                ;; A complete-line chunk: the line-buffered inserter
                ;; commits it to the buffer at the insertion marker,
                ;; indented by the body width (design.md §Decision 1).
                (funcall cb "hello\n" info)
                ;; Assert the indented "hello" lands in the assistant
                ;; body — between the column-0 `#+begin_assistant'
                ;; header line and the next line.
                (let ((content (buffer-substring-no-properties
                                (point-min) (point-max))))
                  (expect (string-match-p
                           "#\\+begin_assistant\n  hello\n"
                           content)
                          :to-be-truthy))
                ;; The `t' completion sentinel: flush, close the
                ;; assistant block, append a fresh user block (the
                ;; post-completion append flow).
                (funcall cb t info)
                (let ((content (buffer-substring-no-properties
                                (point-min) (point-max))))
                  ;; `#+end_assistant' now appears after the inserted
                  ;; indented `"hello"' text, AND a fresh empty user
                  ;; block is appended after it.  The delimiter lines
                  ;; stay at column 0; the new user block's body line
                  ;; carries the body indent (design.md §Decision 6).
                  (expect (string-match-p
                           (concat "#\\+begin_assistant\n  hello\n"
                                   "#\\+end_assistant\n\n"
                                   "#\\+begin_user\n +\n#\\+end_user\n")
                           content)
                          :to-be-truthy))))
          (when (buffer-live-p buf) (kill-buffer buf)))))

    (it "exposes no double-dash alias after load"
      ;; Scenario: specs/chat-mode/spec.md (delta) § "Public
      ;; programmatic-send API" → "No double-dash internal aliases
      ;; remain"
      (expect (fboundp 'gptel-chat--stream-callback) :to-be nil))))

(provide 'stream-public-api-spec)

;;; public-api-spec.el ends here
