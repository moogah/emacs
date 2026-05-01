;;; escape-round-trip-spec.el --- Delimiter / heading escape inverse-pair tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs pinning the round-trip relationship between the
;; stream sanitizer (`gptel-chat--sanitize-chunk' in stream.el) and
;; the send-path un-escapers in parser.el.
;;
;; Two collision rules with separate inverse pairs:
;;
;; 1. Delimiter collision (existing).  The sanitizer prepends `,' to a
;;    line matching `^#\\+end_\\(user\\|assistant\\|tool\\)\\b' so the
;;    insertion does not prematurely close the containing block; the
;;    parser's `gptel-chat--unescape-end-delimiters' strips that `,'
;;    before the body reaches the model.
;;
;; 2. Heading collision (new — change `gptel-chat-heading-scoping',
;;    design.md §Decision 4).  The sanitizer prepends
;;    `gptel-chat-content-indentation' leading spaces to a line
;;    matching `^\\*+ ' so org's heading scanner does not absorb the
;;    line into an outline subtree.  The send-path inverse — strip
;;    leading whitespace before such a line — is added by sibling
;;    task `add-parser-heading-unescape'.  Until that task lands, the
;;    specs in this file pin the SANITIZE-SIDE of the heading-escape
;;    inverse pair only; the full parse → message round-trip for
;;    heading lines is added when the parser un-escape lands.
;;
;; Mechanical inverse proof (the spec scenario "Delimiter escape
;; round-trip" from openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md):
;;
;;   line (contains a bare delimiter)
;;     ─ sanitize ▶  ,delimiter-line
;;     ─ write to buffer as assistant body
;;     ─ parse + unescape ▶  line (original)
;;
;; We verify both directions for delimiters:
;;   - Stream side: every bare `#+end_*' line sanitizes to `,#+end_*';
;;     non-matching lines are unchanged.
;;   - Send side: every `,#+end_*' line un-escapes back to `#+end_*';
;;     non-chat `,#+end_src' and similar are preserved.
;;   - Composition: full parse → message → text → sanitize → parse
;;     preserves the message content across the cycle.
;;
;; For heading lines we currently verify the stream side only (the
;; sanitizer's escape shape and idempotence on already-escaped input);
;; the parse-side and full pipeline arrive with `add-parser-heading-unescape'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared fixtures (sibling of this file's parent directory).
(let ((helpers (expand-file-name
                "../test-helpers.el"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load helpers nil t))

;; Modules under test.
;; `gptel-chat-mode' owns the `gptel-chat-content-indentation' defcustom
;; consulted by `gptel-chat--sanitize-chunk' for the heading-collision
;; escape; load it here so heading-escape specs see the configured
;; default rather than the sanitizer's standalone fallback.
(require 'gptel-chat-mode)
(require 'gptel-chat-parser)
(require 'gptel-chat-stream)

;;; ---------------------------------------------------------------
;;; Helpers for mechanical inverse proofs
;;; ---------------------------------------------------------------

(defun gptel-chat-test--sanitize-body (body)
  "Run stream-sanitizer over BODY line-by-line.
Mirrors what `gptel-chat--make-stream-inserter' does on the write
path: split at `\\n', sanitize each complete line, re-join with
`\\n'.  Returns the sanitized string."
  (let ((lines (split-string body "\n"))
        (acc   nil))
    (dolist (line lines)
      (push (gptel-chat--sanitize-chunk line) acc))
    (mapconcat #'identity (nreverse acc) "\n")))

(defun gptel-chat-test--build-chat-buffer (user-body assistant-body)
  "Return a chat-mode buffer string wrapping USER-BODY and ASSISTANT-BODY.
Inserts the two bodies inside `#+begin_user'/`#+end_user' and
`#+begin_assistant'/`#+end_assistant' delimiters respectively.  The
bodies are expected to be newline-terminated."
  (concat "#+begin_user\n"     user-body      "#+end_user\n"
          "#+begin_assistant\n" assistant-body "#+end_assistant\n"))

;;; ---------------------------------------------------------------
;;; Line-level inverse relationship
;;; ---------------------------------------------------------------

(describe "sanitize-chunk / unescape-end-delimiters inverse pair"

  (describe "bare delimiter lines survive the write-then-read cycle"
    (dolist (delimiter '("#+end_user" "#+end_assistant" "#+end_tool"
                         "#+END_USER" "#+End_Assistant" "#+end_TOOL"))
      (it (format "round-trips %S" delimiter)
        (let* ((sanitized (gptel-chat--sanitize-chunk delimiter))
               (restored  (gptel-chat--unescape-end-delimiters sanitized)))
          (expect sanitized :to-match "\\`,#\\+end_")
          (expect restored  :to-equal delimiter)))))

  (describe "non-colliding lines are untouched both ways"
    (dolist (line '(""
                    "plain prose"
                    "#+end_src"
                    "#+begin_assistant"
                    "#+end_usernope"
                    "    #+end_user"     ; indented — not a collision
                    ",#+end_src"))
      (it (format "passes %S through both passes unchanged" line)
        (let* ((sanitized (gptel-chat--sanitize-chunk line))
               (restored  (gptel-chat--unescape-end-delimiters sanitized)))
          (expect sanitized :to-equal line)
          (expect restored  :to-equal line)))))

  (describe "un-escape is idempotent"
    (it "a single pass over an already-un-escaped string is a no-op"
      (let ((text "ordinary prose\n#+end_src\n#+end_user\nmore\n"))
        (expect (gptel-chat--unescape-end-delimiters text)
                :to-equal text))))

  (describe "sanitize is also idempotent for our delimiters"
    (it "sanitizing `,#+end_assistant' (already-escaped) is a no-op"
      ;; The sanitizer matches `^#\\+end_...' (no leading comma).  An
      ;; already-escaped line does not match, so sanitize is idempotent.
      (let ((escaped ",#+end_assistant"))
        (expect (gptel-chat--sanitize-chunk escaped)
                :to-equal escaped))))

  (describe "un-escape strips only commas the sanitizer would have added"
    (it "leaves `,,#+end_assistant' alone (never produced by the sanitizer)"
      ;; The sanitizer never writes `,,#+end_*': it only escapes lines
      ;; that /match/ `^#\\+end_\\(user\\|assistant\\|tool\\)\\b', and
      ;; `,#+end_...' does not match.  Mirroring that exactly, the
      ;; un-escaper only strips the leading `,' when the *next*
      ;; characters complete a bare `#+end_*' delimiter.  Any other
      ;; leading-comma-and-hash sequence is content and stays verbatim.
      (expect (gptel-chat--unescape-end-delimiters ",,#+end_assistant")
              :to-equal ",,#+end_assistant"))))


;;; ---------------------------------------------------------------
;;; Heading-collision escape: sanitize-side line-level shape
;;; ---------------------------------------------------------------
;;
;; Owned by task `extend-stream-sanitizer-heading-rule' (change
;; `gptel-chat-heading-scoping').  The parser un-escape that strips
;; leading whitespace before `*' lines is added by the sibling task
;; `add-parser-heading-unescape'; until then, this section pins only
;; the SANITIZE-SIDE shape and idempotence of the heading-escape rule
;; alongside the delimiter rule above.  When the parser un-escape
;; lands, a future commit can extend the full pipeline section below
;; with heading-line round-trips.

(describe "sanitize-chunk heading-collision escape (write side)"

  (describe "bare heading lines sanitize with leading whitespace"
    (dolist (heading '("* Heading"
                       "** Sub-heading"
                       "*** Deep"
                       "**** 4-deep"
                       "***** 5-deep"
                       "****** 6-deep"))
      (it (format "escapes %S with the configured prefix" heading)
        ;; The escape is independent of star count; one prefix breaks
        ;; the heading regex regardless of depth.  Use the default
        ;; defcustom value (1 leading space) for the assertion shape.
        (let ((gptel-chat-content-indentation 1))
          (let ((sanitized (gptel-chat--sanitize-chunk heading)))
            (expect sanitized :to-equal (concat " " heading))
            (expect sanitized :to-match "\\` \\*+ "))))))

  (describe "non-heading `*' shapes are not escaped"
    ;; Negative cases that must NOT pick up an indent: anything not
    ;; matching `^\\*+ ' at column 0 is content the sanitizer must
    ;; preserve verbatim.
    (dolist (line '(""
                    "*"                ; bare star, no trailing space
                    "*no-space"        ; no trailing space
                    "*emphasis*"       ; org emphasis markup
                    "  * indented"     ; indented past column 0
                    "not* a heading"   ; star not at column 0
                    " * Heading"       ; already escaped (idempotence)
                    "  ** also-indented"))
      (it (format "passes %S through unchanged" line)
        (let ((gptel-chat-content-indentation 1))
          (expect (gptel-chat--sanitize-chunk line) :to-equal line)))))

  (describe "heading escape is idempotent at the line level"
    ;; Sanitize twice on the same input must not stack two escapes.
    ;; The first pass produces ` * Heading'; the second pass sees a
    ;; line that no longer matches `^\\*+ ' (leading space) and is a
    ;; no-op.  This mirrors the `,#+end_*' idempotence guarantee.
    (it "sanitize(sanitize(`* Heading')) equals sanitize(`* Heading')"
      (let* ((gptel-chat-content-indentation 1)
             (once (gptel-chat--sanitize-chunk "* Heading"))
             (twice (gptel-chat--sanitize-chunk once)))
        (expect once :to-equal " * Heading")
        (expect twice :to-equal once)))

    (it "sanitize(sanitize(`*** Deep')) equals sanitize(`*** Deep')"
      (let* ((gptel-chat-content-indentation 1)
             (once (gptel-chat--sanitize-chunk "*** Deep"))
             (twice (gptel-chat--sanitize-chunk once)))
        (expect once :to-equal " *** Deep")
        (expect twice :to-equal once))))

  (describe "heading escape honours `gptel-chat-content-indentation'"
    (it "uses 2 leading spaces when set to 2 (org-edit-src parity)"
      (let ((gptel-chat-content-indentation 2))
        (expect (gptel-chat--sanitize-chunk "* Heading")
                :to-equal "  * Heading")))

    (it "is idempotent at width 2 as well as width 1"
      (let* ((gptel-chat-content-indentation 2)
             (once (gptel-chat--sanitize-chunk "* Heading"))
             (twice (gptel-chat--sanitize-chunk once)))
        (expect once :to-equal "  * Heading")
        ;; Already-indented `*' lines (any whitespace) no longer match
        ;; `^\\*+ ', so a second pass is a no-op regardless of width.
        (expect twice :to-equal once)))))


(describe "sanitize-body line-by-line: heading + delimiter rules together"
  ;; Mirrors the streaming inserter's per-line scan: a multi-line
  ;; payload gets each rule applied independently, the rules don't
  ;; collide (a line can't start with both `#+end_' and `*'), and
  ;; non-matching lines pass through.

  (it "applies both rules across a mixed body"
    (let* ((gptel-chat-content-indentation 1)
           (body (concat "intro\n"
                         "* Heading\n"
                         "more prose\n"
                         "#+end_assistant\n"
                         "tail\n"))
           (sanitized (gptel-chat-test--sanitize-body body)))
      (expect sanitized
              :to-equal
              (concat "intro\n"
                      " * Heading\n"
                      "more prose\n"
                      ",#+end_assistant\n"
                      "tail\n"))))

  (it "is idempotent at the body level"
    ;; Re-sanitizing an already-sanitized body must not stack escapes.
    (let* ((gptel-chat-content-indentation 1)
           (body (concat "* Heading\n"
                         "#+end_user\n"
                         "* Another\n"))
           (once (gptel-chat-test--sanitize-body body))
           (twice (gptel-chat-test--sanitize-body once)))
      (expect once
              :to-equal
              (concat " * Heading\n"
                      ",#+end_user\n"
                      " * Another\n"))
      (expect twice :to-equal once))))

;;; ---------------------------------------------------------------
;;; Full pipeline: parse → message → text → sanitize → parse
;;; ---------------------------------------------------------------

(describe "parse → message → write-back → parse round-trip"

  ;; The full cycle exercised here:
  ;;
  ;;   (1) Buffer contains assistant body with `,#+end_assistant' inside
  ;;       (the escaped form produced by the stream sanitizer).
  ;;   (2) Parse the buffer → turn list.
  ;;   (3) Convert turns → messages; the un-escaper strips the `,'.
  ;;   (4) Take the message body string and sanitize it as if it were
  ;;       being re-streamed — the `,' comes back.
  ;;   (5) Wrap the sanitized body in `#+begin_assistant'/`#+end_assistant'
  ;;       and parse again — the resulting message text equals the
  ;;       message text from step (3).
  ;;
  ;; This proves that a round-trip through the on-disk buffer format
  ;; (sanitize write-path + un-escape read-path) preserves message
  ;; content verbatim.

  (it "round-trips `,#+end_assistant' body line through the full pipeline"
    (let* ((assistant-body
            (concat "Here is a literal delimiter:\n"
                    ",#+end_assistant\n"
                    "and more text.\n"))
           (buffer-text
            (concat "#+begin_assistant\n"
                    assistant-body
                    "#+end_assistant\n"))
           ;; Step 2-3: parse + message-construct the starting buffer.
           (msgs-original
            (gptel-chat-test--with-buffer buffer-text
              (gptel-chat-turns-to-messages
               (gptel-chat-parse-buffer))))
           (message-text (cdr (car msgs-original)))
           ;; Step 4: sanitize the message text line-by-line.
           (resanitized (gptel-chat-test--sanitize-body message-text))
           ;; Step 5: embed sanitized body back in an assistant block
           ;; and reparse.
           (buffer-text-2
            (concat "#+begin_assistant\n"
                    resanitized
                    (unless (string-suffix-p "\n" resanitized) "\n")
                    "#+end_assistant\n"))
           (msgs-roundtripped
            (gptel-chat-test--with-buffer buffer-text-2
              (gptel-chat-turns-to-messages
               (gptel-chat-parse-buffer)))))
      ;; The un-escaped text has the bare delimiter.
      (expect message-text :to-match "^#\\+end_assistant$")
      ;; After re-sanitizing, the comma is back.
      (expect resanitized :to-match "^,#\\+end_assistant$")
      ;; And round-tripping yields the same message content.
      (expect msgs-roundtripped :to-equal msgs-original)))

  (it "round-trips `,#+end_user' inside a user body"
    ;; Users writing a literal `#+end_user' line in their prompt use
    ;; the `,'-escape convention; that prefix must be stripped on send.
    (let* ((user-body
            (concat "Quoting:\n"
                    ",#+end_user\n"
                    "to illustrate.\n"))
           (buffer-text (concat "#+begin_user\n"
                                user-body
                                "#+end_user\n"))
           (msgs (gptel-chat-test--with-buffer buffer-text
                   (gptel-chat-turns-to-messages
                    (gptel-chat-parse-buffer))))
           (text (cdr (car msgs))))
      (expect (car (car msgs)) :to-equal 'prompt)
      (expect text :to-match "^#\\+end_user$")
      ;; And we can re-sanitize + reparse to recover the same message.
      (let* ((resanitized (gptel-chat-test--sanitize-body text))
             (buffer-text-2
              (concat "#+begin_user\n"
                      resanitized
                      (unless (string-suffix-p "\n" resanitized) "\n")
                      "#+end_user\n"))
             (msgs-2 (gptel-chat-test--with-buffer buffer-text-2
                       (gptel-chat-turns-to-messages
                        (gptel-chat-parse-buffer)))))
        (expect msgs-2 :to-equal msgs))))

  (it "round-trips all three delimiter variants in a single assistant body"
    (let* ((body (concat
                  "prose line\n"
                  ",#+end_user\n"
                  "more prose\n"
                  ",#+end_assistant\n"
                  "yet more\n"
                  ",#+end_tool\n"
                  "tail\n"))
           (buffer-text (concat "#+begin_assistant\n"
                                body
                                "#+end_assistant\n"))
           (msgs (gptel-chat-test--with-buffer buffer-text
                   (gptel-chat-turns-to-messages
                    (gptel-chat-parse-buffer))))
           (text (cdr (car msgs))))
      (expect text :to-match "^#\\+end_user$")
      (expect text :to-match "^#\\+end_assistant$")
      (expect text :to-match "^#\\+end_tool$")
      (expect text :not :to-match "^,#")))

  (it "does not un-escape `,#+end_src' during a full parse"
    ;; `#+end_src' is not a chat-mode delimiter, so the `,' stays.
    (let* ((body (concat ",#+end_src\n"
                         "prose\n"))
           (buffer-text (concat "#+begin_assistant\n"
                                body
                                "#+end_assistant\n"))
           (msgs (gptel-chat-test--with-buffer buffer-text
                   (gptel-chat-turns-to-messages
                    (gptel-chat-parse-buffer))))
           (text (cdr (car msgs))))
      (expect text :to-match "^,#\\+end_src$"))))

(provide 'escape-round-trip-spec)

;;; escape-round-trip-spec.el ends here
