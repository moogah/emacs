;;; message-construction-spec.el --- Buttercup tests for turn-list → :prompt -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat-turns-to-messages' — the converter
;; that shapes the parser's turn list into the `gptel-request' `:prompt'
;; advanced-format message list.
;;
;; Covers every scenario in §"Message construction from buffer" of
;; openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md:
;;   - Single user-assistant turn
;;   - Assistant turn with one tool call
;;   - Delimiter escape round-trip (covered here + in escape-round-trip-spec.el)
;;   - Empty blocks are skipped
;;   - Turns distributed across org headings
;;   - User prompt composed with org structural features
;;   - User block body containing a `#+begin_assistant' literal
;;
;; The converter's target shape matches `gptel-request''s documented
;; `:prompt' contract — the advanced format dispatched by
;; `gptel--parse-list-and-insert' and the backend-specific
;; `gptel--parse-list' methods:
;;
;;   (prompt . STR)        — a user turn
;;   (response . STR)      — an assistant text segment
;;   (tool . PLIST)        — a single tool call
;;
;; The backend expands each `(tool . PLIST)' cons into the wire-level
;; tool_use / tool_result pair for the provider.  See
;; `runtime/straight/repos/gptel/gptel-anthropic.el' (Anthropic: assistant
;; role carrying `:type "tool_use"' followed by a user role carrying
;; `:type "tool_result"') and sibling backend files for the expansion
;; per provider.

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

;;; ---------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------

(describe "gptel-chat-turns-to-messages: Message construction from buffer"

  (describe "Single user-assistant turn"
    (it "emits [(prompt . user-text) (response . assistant-text)]"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "What's the capital of France?\n"
                  "#+end_user\n"
                  "#+begin_assistant\n"
                  "Paris.\n"
                  "#+end_assistant\n")
        (let* ((turns (gptel-chat-parse-buffer))
               (msgs  (gptel-chat-turns-to-messages turns)))
          (expect msgs :to-equal
                  '((prompt   . "What's the capital of France?\n")
                    (response . "Paris.\n"))))))

    (it "preserves document order across multiple pairs"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\nq1\n#+end_user\n"
                  "#+begin_assistant\na1\n#+end_assistant\n"
                  "#+begin_user\nq2\n#+end_user\n"
                  "#+begin_assistant\na2\n#+end_assistant\n")
        (let ((msgs (gptel-chat-turns-to-messages
                     (gptel-chat-parse-buffer))))
          (expect (mapcar #'car msgs)
                  :to-equal '(prompt response prompt response))
          (expect (mapcar #'cdr msgs)
                  :to-equal '("q1\n" "a1\n" "q2\n" "a2\n"))))))

  (describe "Assistant turn with one tool call"
    (it "expands (text, tool-call, text) segments into response + tool + response"
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  "Sure.\n"
                  "\n"
                  "#+begin_tool (run_bash_command :command \"uname\")\n"
                  "{\"success\":true,\"output\":\"Darwin\\n\",\"exit_code\":0}\n"
                  "#+end_tool\n"
                  "\n"
                  "You're on Darwin.\n"
                  "#+end_assistant\n")
        (let ((msgs (gptel-chat-turns-to-messages
                     (gptel-chat-parse-buffer))))
          ;; Three messages total: response, tool, response.
          (expect (length msgs) :to-equal 3)
          (expect (car (nth 0 msgs)) :to-equal 'response)
          (expect (car (nth 1 msgs)) :to-equal 'tool)
          (expect (car (nth 2 msgs)) :to-equal 'response)
          ;; Tool call carries the plist the backend consumes.
          (let ((call (cdr (nth 1 msgs))))
            (expect (plist-get call :name) :to-equal "run_bash_command")
            (expect (plist-get call :args)
                    :to-equal '(:command "uname"))
            (expect (plist-get call :result) :to-match "Darwin"))
          ;; Surrounding prose segments are preserved verbatim.
          (expect (cdr (nth 0 msgs)) :to-match "\\`Sure\\.\n")
          (expect (cdr (nth 2 msgs)) :to-match "You're on Darwin\\.\n\\'")))))

  (describe "Multiple tool calls in one assistant turn"
    (it "emits a tool cons per call, in document order, interleaved with prose"
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  "Step 1:\n"
                  "#+begin_tool (t1 :x 1)\n"
                  "r1\n"
                  "#+end_tool\n"
                  "Between.\n"
                  "#+begin_tool (t2 :x 2)\n"
                  "r2\n"
                  "#+end_tool\n"
                  "Step 3:\n"
                  "#+begin_tool (t3 :x 3)\n"
                  "r3\n"
                  "#+end_tool\n"
                  "Done.\n"
                  "#+end_assistant\n")
        (let ((msgs (gptel-chat-turns-to-messages
                     (gptel-chat-parse-buffer))))
          (expect (mapcar #'car msgs)
                  :to-equal
                  '(response tool response tool response tool response))
          (let ((tool-msgs (cl-remove-if-not
                            (lambda (m) (eq (car m) 'tool)) msgs)))
            (expect (mapcar (lambda (m) (plist-get (cdr m) :name)) tool-msgs)
                    :to-equal '("t1" "t2" "t3"))
            (expect (mapcar (lambda (m) (plist-get (cdr m) :result)) tool-msgs)
                    :to-equal '("r1" "r2" "r3")))))))

  (describe "Delimiter escape round-trip"
    (it "strips leading `,' from `,#+end_assistant' lines in assistant text"
      ;; A user editing (or a previous stream) would have written
      ;; `,#+end_assistant' as a sanitized body line; on the send path
      ;; the escape must be undone before the model sees it.
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  "Here is a literal delimiter:\n"
                  ",#+end_assistant\n"
                  "and more prose.\n"
                  "#+end_assistant\n")
        (let* ((msgs (gptel-chat-turns-to-messages
                      (gptel-chat-parse-buffer)))
               (text (cdr (car msgs))))
          (expect (length msgs) :to-equal 1)
          (expect (car (car msgs)) :to-equal 'response)
          (expect text :to-match "^#\\+end_assistant$")
          (expect text :not :to-match "^,#\\+end_assistant$"))))

    (it "strips leading `,' from `,#+end_user' lines in user body"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "Quoting this delimiter literally:\n"
                  ",#+end_user\n"
                  "to show it to the model.\n"
                  "#+end_user\n")
        (let* ((msgs (gptel-chat-turns-to-messages
                      (gptel-chat-parse-buffer)))
               (text (cdr (car msgs))))
          (expect (car (car msgs)) :to-equal 'prompt)
          (expect text :to-match "^#\\+end_user$")
          (expect text :not :to-match "^,#\\+end_user$"))))

    (it "leaves `,#+end_src' and other non-chat `,#+end_*' alone"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "Inside a src block example:\n"
                  ",#+end_src\n"
                  "#+end_user\n")
        (let* ((msgs (gptel-chat-turns-to-messages
                      (gptel-chat-parse-buffer)))
               (text (cdr (car msgs))))
          (expect text :to-match "^,#\\+end_src$"))))

    (it "un-escapes mixed-case `,#+End_Assistant' (case-insensitive)"
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  ",#+End_Assistant\n"
                  "#+end_assistant\n")
        (let* ((msgs (gptel-chat-turns-to-messages
                      (gptel-chat-parse-buffer)))
               (text (cdr (car msgs))))
          (expect text :to-match "^#\\+End_Assistant$")
          (expect text :not :to-match "^,#"))))

    (it "leaves `,,#+end_assistant' alone (sanitizer never produces double-commas)"
      ;; The stream sanitizer only escapes bare `#+end_*' lines; a line
      ;; already beginning with `,' passes through unchanged.  So a
      ;; `,,#+end_assistant' body line is not a shape the pipeline
      ;; creates — the un-escaper correctly leaves it untouched rather
      ;; than inventing a second comma-stripping rule.
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  ",,#+end_assistant\n"
                  "#+end_assistant\n")
        (let* ((msgs (gptel-chat-turns-to-messages
                      (gptel-chat-parse-buffer)))
               (text (cdr (car msgs))))
          (expect text :to-match "^,,#\\+end_assistant$")))))

  (describe "Empty blocks are skipped"
    (it "emits no message for an empty user block"
      (gptel-chat-test--with-buffer "#+begin_user\n#+end_user\n"
        (expect (gptel-chat-turns-to-messages
                 (gptel-chat-parse-buffer))
                :to-equal nil)))

    (it "emits no message for a whitespace-only user block"
      (gptel-chat-test--with-buffer "#+begin_user\n   \n\t\n#+end_user\n"
        (expect (gptel-chat-turns-to-messages
                 (gptel-chat-parse-buffer))
                :to-equal nil)))

    (it "emits no message for an empty assistant block"
      (gptel-chat-test--with-buffer "#+begin_assistant\n#+end_assistant\n"
        (expect (gptel-chat-turns-to-messages
                 (gptel-chat-parse-buffer))
                :to-equal nil)))

    (it "emits no message for a whitespace-only assistant block"
      (gptel-chat-test--with-buffer
          "#+begin_assistant\n\n  \n\n#+end_assistant\n"
        (expect (gptel-chat-turns-to-messages
                 (gptel-chat-parse-buffer))
                :to-equal nil)))

    (it "keeps a tool-call assistant turn even with blank surrounding prose"
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  "\n"
                  "#+begin_tool (t :x 1)\n"
                  "r\n"
                  "#+end_tool\n"
                  "\n"
                  "#+end_assistant\n")
        (let ((msgs (gptel-chat-turns-to-messages
                     (gptel-chat-parse-buffer))))
          (expect (length msgs) :to-equal 1)
          (expect (car (car msgs)) :to-equal 'tool))))

    (it "drops empty user turns between populated ones"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\nq1\n#+end_user\n"
                  "#+begin_user\n\n#+end_user\n"
                  "#+begin_user\nq2\n#+end_user\n")
        (expect (gptel-chat-turns-to-messages
                 (gptel-chat-parse-buffer))
                :to-equal
                '((prompt . "q1\n") (prompt . "q2\n"))))))

  (describe "Turns distributed across org headings"
    (it "yields [prompt_A, response_A, prompt_B] in document order"
      (gptel-chat-test--with-buffer
          (concat "* Section A\n"
                  "Free-form notes.\n"
                  "#+begin_user\n"
                  "First question.\n"
                  "#+end_user\n"
                  "#+begin_assistant\n"
                  "First answer.\n"
                  "#+end_assistant\n"
                  "* Section B\n"
                  "#+begin_user\n"
                  "Follow-up.\n"
                  "#+end_user\n")
        (expect (gptel-chat-turns-to-messages
                 (gptel-chat-parse-buffer))
                :to-equal
                '((prompt   . "First question.\n")
                  (response . "First answer.\n")
                  (prompt   . "Follow-up.\n")))))

    (it "does not include heading text anywhere in the message list"
      (gptel-chat-test--with-buffer
          (concat "* A heading\n"
                  "Prose that belongs to the human.\n"
                  "#+begin_user\n"
                  "Just the question.\n"
                  "#+end_user\n")
        (let ((msgs (gptel-chat-turns-to-messages
                     (gptel-chat-parse-buffer))))
          (expect msgs :to-equal
                  '((prompt . "Just the question.\n")))
          (expect (cdr (car msgs))
                  :not :to-match "A heading")
          (expect (cdr (car msgs))
                  :not :to-match "human")))))

  (describe "User prompt with org structural features preserved verbatim"
    (it "captures headings, src blocks, lists, and emphasis inside a user block"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "* Context\n"
                  "\n"
                  "Here is the problem I'm trying to solve.\n"
                  "\n"
                  "#+begin_src emacs-lisp\n"
                  "(defun hello () \"hi\")\n"
                  "#+end_src\n"
                  "\n"
                  "- item one\n"
                  "- item two\n"
                  "\n"
                  "The /emphasis/ on *bold* matters.\n"
                  "#+end_user\n")
        (let* ((msgs (gptel-chat-turns-to-messages
                      (gptel-chat-parse-buffer)))
               (text (cdr (car msgs))))
          (expect (length msgs) :to-equal 1)
          (expect (car (car msgs)) :to-equal 'prompt)
          (expect text :to-match "\\* Context")
          (expect text :to-match "#\\+begin_src emacs-lisp")
          (expect text :to-match "#\\+end_src")
          (expect text :to-match "- item one")
          (expect text :to-match "- item two")
          (expect text :to-match "/emphasis/")
          (expect text :to-match "\\*bold\\*")))))

  (describe "User block containing a #+begin_assistant literal"
    (it "includes the literal in the user prompt content"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "Consider this example:\n"
                  "#+begin_assistant\n"
                  "(that is quoted in the user's prose)\n"
                  "#+end_user\n")
        (let ((msgs (gptel-chat-turns-to-messages
                     (gptel-chat-parse-buffer))))
          (expect (length msgs) :to-equal 1)
          (expect (car (car msgs)) :to-equal 'prompt)
          (expect (cdr (car msgs))
                  :to-match "#\\+begin_assistant")
          (expect (cdr (car msgs))
                  :to-match "(that is quoted in the user's prose)")))))

  (describe "Empty input"
    (it "yields nil for an empty turn list"
      (expect (gptel-chat-turns-to-messages nil)
              :to-equal nil))))

(describe "gptel-chat-turns-to-messages: tool-call plist shape"

  ;; The plist we emit is the /compact/ advanced-format encoding that
  ;; `gptel--parse-list' (per backend) expands into tool_use / tool_result
  ;; messages on the wire.  See for example `gptel-anthropic.el':
  ;;
  ;;   (`(tool . ,call)
  ;;     (unless (plist-get call :id)
  ;;       (plist-put call :id (gptel--anthropic-format-tool-id nil)))
  ;;     (push (list :role "assistant"
  ;;                 :content `[(:type "tool_use" :id ,(plist-get call :id)
  ;;                              :name ,(plist-get call :name)
  ;;                              :input ,(plist-get call :args))])
  ;;           prompts)
  ;;     (push (gptel--parse-tool-results backend (list (cdr entry))) prompts))
  ;;
  ;; — and the `:role "user"' tool_result message is built from the same
  ;; plist's `:id' and `:result' by `gptel--parse-tool-results'.  The
  ;; parallel openai / gemini / ollama methods consume the same three
  ;; keys.  This suite pins our side of that contract.

  (it "includes :name, :args, :result on the tool plist"
    (gptel-chat-test--with-buffer
        (concat "#+begin_assistant\n"
                "#+begin_tool (my_tool :arg1 1 :arg2 \"two\")\n"
                "RESULT-STRING\n"
                "#+end_tool\n"
                "#+end_assistant\n")
      (let* ((msgs (gptel-chat-turns-to-messages
                    (gptel-chat-parse-buffer)))
             (tool-msg (car msgs))
             (call (cdr tool-msg)))
        (expect (car tool-msg) :to-equal 'tool)
        (expect (plist-get call :name)   :to-equal "my_tool")
        (expect (plist-get call :args)   :to-equal '(:arg1 1 :arg2 "two"))
        (expect (plist-get call :result) :to-equal "RESULT-STRING"))))

  (it "omits :id (upstream backend synthesises one per-provider)"
    (gptel-chat-test--with-buffer
        (concat "#+begin_assistant\n"
                "#+begin_tool (no_id :x 1)\n"
                "r\n"
                "#+end_tool\n"
                "#+end_assistant\n")
      (let* ((msgs (gptel-chat-turns-to-messages
                    (gptel-chat-parse-buffer)))
             (call (cdr (car msgs))))
        (expect (plist-member call :id) :to-equal nil))))

  (it "defaults :result to empty string when the tool block is empty"
    (gptel-chat-test--with-buffer
        (concat "#+begin_assistant\n"
                "#+begin_tool (empty_tool)\n"
                "#+end_tool\n"
                "#+end_assistant\n")
      (let* ((msgs (gptel-chat-turns-to-messages
                    (gptel-chat-parse-buffer)))
             (call (cdr (car msgs))))
        (expect (plist-get call :result) :to-equal ""))))

  (describe "tool :result delimiter un-escape (symmetry with prose)"

    ;; The stream sanitizer prepends `,' to any body line matching
    ;; `^#\+end_\(user\|assistant\|tool\)\b'.  User and assistant text
    ;; route their content through `gptel-chat--unescape-end-delimiters'
    ;; at emit time.  Tool-result content must be un-escaped the same
    ;; way so the model never sees the sanitizer's `,'-prefix artifact.

    (it "strips leading `,' from `,#+end_tool' lines in tool :result"
      ;; Build the segment plist directly so the spec pins the
      ;; converter's contract without depending on parser round-tripping
      ;; an escaped delimiter through a tool block body.
      (let* ((segment (list :type   'tool-call
                            :name   "dump_prose"
                            :args   '(:arg 1)
                            :result (concat "prose\n"
                                            ",#+end_tool\n"
                                            "more prose\n")))
             (msgs (gptel-chat--segment-to-messages segment))
             (call (cdr (car msgs))))
        (expect (car (car msgs)) :to-equal 'tool)
        (expect (plist-get call :result)
                :to-equal (concat "prose\n"
                                  "#+end_tool\n"
                                  "more prose\n"))))

    (it "is a no-op when :result contains no escaped delimiters"
      (let* ((raw "no delimiters here\njust prose\n")
             (segment (list :type   'tool-call
                            :name   "plain"
                            :args   nil
                            :result raw))
             (msgs (gptel-chat--segment-to-messages segment))
             (call (cdr (car msgs))))
        (expect (plist-get call :result) :to-equal raw)))))

(provide 'message-construction-spec)

;;; message-construction-spec.el ends here
