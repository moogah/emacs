;;; buffer-format-spec.el --- Buttercup tests for gptel-chat parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat--parse-buffer' and for the
;; `gptel-chat-mode' major-mode definition.
;;
;; Coverage (from openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md):
;;   §Mode definition and activation — interactive activation and the
;;     file-local `-*- gptel-chat -*-' cookie.
;;   §Buffer format validation — all seven scenarios.
;;   §Message construction from buffer — parser-only scenarios (role
;;     ordering, turn list in document order, tool-call segment
;;     extraction, user-block opacity, heading-distributed turns,
;;     user prompt with org structural features).
;;
;; Delimiter un-escaping and the turn-list -> message-list conversion
;; live in task `messages' and are NOT exercised here.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared fixtures (sibling of this file's parent directory).
(let ((helpers (expand-file-name
                "../test-helpers.el"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load helpers nil t))

;; Load the modules under test.
(require 'gptel-chat-parser)
(require 'gptel-chat-mode)

;;; Small content builders — keep scenarios scannable at a glance.

(defconst gptel-chat-test--single-turn-pair
  (concat "#+begin_user\n"
          "What's the capital of France?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Paris.\n"
          "#+end_assistant\n"))

(defconst gptel-chat-test--tool-call-turn
  (concat "#+begin_assistant\n"
          "Sure.\n"
          "\n"
          "#+begin_tool (run_bash_command :command \"uname\")\n"
          "{\"success\":true,\"output\":\"Darwin\\n\",\"exit_code\":0}\n"
          "#+end_tool\n"
          "\n"
          "You're on Darwin.\n"
          "#+end_assistant\n"))

(defconst gptel-chat-test--heading-distributed
  (concat "* Section A\n"
          "\n"
          "Free-form notes the human keeps for themselves.\n"
          "\n"
          "#+begin_user\n"
          "First question.\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "First answer.\n"
          "#+end_assistant\n"
          "\n"
          "* Section B\n"
          "\n"
          "#+begin_user\n"
          "Follow-up.\n"
          "#+end_user\n"))

;;; Tests

(describe "gptel-chat-mode: Mode definition and activation"

  (describe "interactive activation"
    (it "sets `major-mode' to `gptel-chat-mode'"
      (with-temp-buffer
        (gptel-chat-mode)
        (expect major-mode :to-equal 'gptel-chat-mode)))

    (it "derives from `org-mode'"
      (with-temp-buffer
        (gptel-chat-mode)
        (expect (derived-mode-p 'org-mode) :to-be-truthy)))

    (it "disables `org-adapt-indentation' buffer-locally"
      ;; Set a non-nil global value so the assertion is meaningful: the
      ;; mode must install a buffer-local nil that shadows whatever the
      ;; user has configured globally.
      (let ((org-adapt-indentation t))
        (with-temp-buffer
          (gptel-chat-mode)
          (expect (local-variable-p 'org-adapt-indentation) :to-be-truthy)
          (expect org-adapt-indentation :to-equal nil))))

    (it "installs the chat-mode keymap with `C-c C-c' bound to `gptel-chat-send'"
      (with-temp-buffer
        (gptel-chat-mode)
        ;; Active-map assertions — prove `define-derived-mode' actually
        ;; wired `gptel-chat-mode-map' onto the buffer.  A regression that
        ;; renamed the defvar or dropped the `-map'-suffix wiring would
        ;; leave the defvar-level lookups below green while `C-c C-c'
        ;; does nothing in a real chat buffer.
        (expect (eq (current-local-map) gptel-chat-mode-map)
                :to-be-truthy)
        (expect (lookup-key (current-local-map) (kbd "C-c C-c"))
                :to-equal 'gptel-chat-send)
        ;; Defvar-level assertions — exhaustively document the map's
        ;; contents so a regression that drops an individual binding is
        ;; caught with a per-key failure message.
        (expect (lookup-key gptel-chat-mode-map (kbd "C-c C-c"))
                :to-equal 'gptel-chat-send)
        (expect (lookup-key gptel-chat-mode-map (kbd "C-c n"))
                :to-equal 'gptel-chat-next-turn)
        (expect (lookup-key gptel-chat-mode-map (kbd "C-c p"))
                :to-equal 'gptel-chat-previous-turn)
        (expect (lookup-key gptel-chat-mode-map (kbd "C-c C-r"))
                :to-equal 'gptel-chat-regenerate)
        (expect (lookup-key gptel-chat-mode-map (kbd "C-c C-t"))
                :to-equal 'gptel-chat-toggle-display-layer)
        (expect (lookup-key gptel-chat-mode-map (kbd "C-c C-k"))
                :to-equal 'gptel-abort)))

    (it "runs `gptel-chat-mode-hook' on activation"
      (let ((hook-ran nil))
        (let ((gptel-chat-mode-hook (list (lambda () (setq hook-ran t)))))
          (with-temp-buffer
            (gptel-chat-mode)
            (expect hook-ran :to-be-truthy))))))

  (describe "file-local cookie activation"
    (it "activates `gptel-chat-mode' on a file whose first line has `-*- gptel-chat -*-'"
      (let ((tmpfile (make-temp-file "gptel-chat-cookie-" nil ".org")))
        (unwind-protect
            (progn
              (with-temp-file tmpfile
                (insert "# -*- gptel-chat -*-\n"
                        "#+begin_user\n"
                        "hi\n"
                        "#+end_user\n"))
              (let ((buf (find-file-noselect tmpfile)))
                (unwind-protect
                    (with-current-buffer buf
                      (expect major-mode :to-equal 'gptel-chat-mode))
                  (kill-buffer buf))))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile)))))

    ;; Stricter check: prove the cookie — not the `.org' extension — is
    ;; what activates the mode.  With a `.txt' tempfile, extension-based
    ;; auto-mode-alist would select `text-mode'; only the cookie can
    ;; switch the buffer into `gptel-chat-mode'.
    (it "activates on a non-`.org' file when the cookie is present"
      (let ((tmpfile (make-temp-file "gptel-chat-cookie-" nil ".txt")))
        (unwind-protect
            (progn
              (with-temp-file tmpfile
                (insert "# -*- gptel-chat -*-\n"
                        "#+begin_user\n"
                        "hi\n"
                        "#+end_user\n"))
              (let ((buf (find-file-noselect tmpfile)))
                (unwind-protect
                    (with-current-buffer buf
                      (expect major-mode :to-equal 'gptel-chat-mode))
                  (kill-buffer buf))))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile))))))

  (describe "gptel-chat-new"
    (it "creates a buffer in `gptel-chat-mode' with an empty user block"
      (let ((buf (gptel-chat-new)))
        (unwind-protect
            (with-current-buffer buf
              (expect major-mode :to-equal 'gptel-chat-mode)
              (expect (buffer-string)
                      :to-equal "#+begin_user\n\n#+end_user\n"))
          (kill-buffer buf))))

    (it "positions point on the empty line inside the user block"
      (let ((buf (gptel-chat-new)))
        (unwind-protect
            (with-current-buffer buf
              ;; Point is on line 2 (the empty line between delimiters).
              (expect (line-number-at-pos) :to-equal 2)
              ;; Typing here extends the user block, not the delimiters.
              (expect (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))
                      :to-equal ""))
          (kill-buffer buf))))))

(describe "gptel-chat--parse-buffer: Buffer format validation"

  (describe "empty buffer"
    (it "returns an empty turn list"
      (gptel-chat-test--with-buffer ""
        (expect (gptel-chat--parse-buffer) :to-equal nil)))

    (it "treats whitespace-only content as empty"
      (gptel-chat-test--with-buffer "\n\n   \n\t\n"
        (expect (gptel-chat--parse-buffer) :to-equal nil))))

  (describe "metadata-only buffer"
    (it "yields an empty turn list when the buffer contains only keywords"
      (gptel-chat-test--with-buffer "#+gptel-model: foo\n#+title: A thread\n\n"
        (expect (gptel-chat--parse-buffer) :to-equal nil))))

  (describe "headings and commentary outside turn blocks"
    (it "ignores heading and prose lines; yields one turn per block"
      (gptel-chat-test--with-buffer
          (concat "* A heading\n"
                  "\n"
                  "Some free-form prose the human wrote.\n"
                  "\n"
                  "#+begin_user\n"
                  "Question.\n"
                  "#+end_user\n"
                  "\n"
                  "#+begin_assistant\n"
                  "Answer.\n"
                  "#+end_assistant\n")
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (length turns) :to-equal 2)
          (expect (plist-get (nth 0 turns) :role) :to-equal 'user)
          (expect (plist-get (nth 1 turns) :role) :to-equal 'assistant))))

    (it "does not include heading text in any turn's content"
      (gptel-chat-test--with-buffer
          (concat "* A heading\n"
                  "#+begin_user\n"
                  "Just the question.\n"
                  "#+end_user\n")
        (let* ((turns (gptel-chat--parse-buffer))
               (user (nth 0 turns)))
          (expect (plist-get user :content)
                  :not :to-match "A heading")))))

  (describe "turn blocks nested under org headings"
    (it "enumerates blocks in document order regardless of heading depth"
      (gptel-chat-test--with-buffer gptel-chat-test--heading-distributed
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (length turns) :to-equal 3)
          (expect (mapcar (lambda (tn) (plist-get tn :role)) turns)
                  :to-equal '(user assistant user)))))

    (it "handles turns nested at arbitrary heading depth"
      (gptel-chat-test--with-buffer
          (concat "* A\n"
                  "** A.1\n"
                  "*** A.1.a\n"
                  "#+begin_user\n"
                  "deep-nested prompt\n"
                  "#+end_user\n")
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (length turns) :to-equal 1)
          (expect (plist-get (car turns) :role) :to-equal 'user)
          (expect (plist-get (car turns) :content)
                  :to-equal "deep-nested prompt\n")))))

  (describe "unmatched delimiter"
    (it "signals user-error for an unclosed user block"
      (gptel-chat-test--with-buffer "#+begin_user\nhello\n"
        (expect (gptel-chat--parse-buffer) :to-throw 'user-error)))

    (it "reports the opening line number in the error message"
      (gptel-chat-test--with-buffer
          (concat "intro prose\n"
                  "#+begin_user\n"
                  "incomplete\n")
        (condition-case err
            (progn (gptel-chat--parse-buffer)
                   (expect nil :to-be-truthy)) ; should not reach here
          (user-error
           (expect (error-message-string err)
                   :to-match "unclosed user block at line 2")))))

    (it "signals user-error for an unclosed assistant block"
      (gptel-chat-test--with-buffer "#+begin_assistant\nhello\n"
        (expect (gptel-chat--parse-buffer) :to-throw 'user-error)))

    (it "signals user-error for an unclosed tool block inside an assistant"
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  "#+begin_tool (foo :x 1)\n"
                  "result\n"
                  "#+end_assistant\n")
        (expect (gptel-chat--parse-buffer) :to-throw 'user-error)))

    ;; Regression: a `#+begin_user' on the buffer's final line with no
    ;; trailing newline must signal the documented unclosed-block
    ;; `user-error' rather than `args-out-of-range' from `goto-char'.
    ;; Reachable from partially-streamed or hand-edited buffers.
    (it "signals user-error (not args-out-of-range) for #+begin_user at EOF with no newline"
      (gptel-chat-test--with-buffer "#+begin_user"
        (expect (gptel-chat--parse-buffer) :to-throw 'user-error)))

    (it "reports unclosed-user at line 1 for EOF-without-newline #+begin_user"
      (gptel-chat-test--with-buffer "#+begin_user"
        (condition-case err
            (progn (gptel-chat--parse-buffer)
                   (expect nil :to-be-truthy))
          (user-error
           (expect (error-message-string err)
                   :to-match "unclosed user block at line 1")))))

    (it "signals user-error (not args-out-of-range) for #+begin_assistant at EOF with no newline"
      (gptel-chat-test--with-buffer "#+begin_assistant"
        (expect (gptel-chat--parse-buffer) :to-throw 'user-error)))

    (it "reports unclosed-assistant at line 1 for EOF-without-newline #+begin_assistant"
      (gptel-chat-test--with-buffer "#+begin_assistant"
        (condition-case err
            (progn (gptel-chat--parse-buffer)
                   (expect nil :to-be-truthy))
          (user-error
           (expect (error-message-string err)
                   :to-match "unclosed assistant block at line 1"))))))

  (describe "tool block outside an assistant block"
    (it "signals user-error when `#+begin_tool' appears at top level"
      (gptel-chat-test--with-buffer
          (concat "#+begin_tool (foo :x 1)\n"
                  "body\n"
                  "#+end_tool\n")
        (expect (gptel-chat--parse-buffer) :to-throw 'user-error)))

    (it "reports tool-block-outside-assistant at the offending line"
      (gptel-chat-test--with-buffer
          (concat "first line of prose\n"
                  "#+begin_tool (foo)\n"
                  "#+end_tool\n")
        (condition-case err
            (progn (gptel-chat--parse-buffer)
                   (expect nil :to-be-truthy))
          (user-error
           (expect (error-message-string err)
                   :to-match "tool-block-outside-assistant at line 2")))))

    (it "signals an error even when the tool block follows a closed assistant"
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  "finished\n"
                  "#+end_assistant\n"
                  "#+begin_tool (stray)\n"
                  "#+end_tool\n")
        (expect (gptel-chat--parse-buffer) :to-throw 'user-error))))

  (describe "turn nested inside another turn"
    (it "signals turn-inside-turn when #+begin_user appears in assistant body"
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  "preamble\n"
                  "#+begin_user\n"
                  "nested prompt\n"
                  "#+end_user\n"
                  "#+end_assistant\n")
        (expect (gptel-chat--parse-buffer) :to-throw 'user-error)))

    (it "reports turn-inside-turn at the offending inner line"
      (gptel-chat-test--with-buffer
          (concat "#+begin_assistant\n"
                  "line two\n"
                  "#+begin_assistant\n"
                  "#+end_assistant\n"
                  "#+end_assistant\n")
        (condition-case err
            (progn (gptel-chat--parse-buffer)
                   (expect nil :to-be-truthy))
          (user-error
           (expect (error-message-string err)
                   :to-match "turn-inside-turn at line 3"))))))

  (describe "user block body containing #+begin_assistant literal"
    (it "treats the literal as body content, not a new turn"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "Consider this example:\n"
                  "#+begin_assistant\n"
                  "(that is quoted in the user's prose)\n"
                  "#+end_user\n")
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (length turns) :to-equal 1)
          (expect (plist-get (car turns) :role) :to-equal 'user)
          (expect (plist-get (car turns) :content)
                  :to-match "#\\+begin_assistant")
          (expect (plist-get (car turns) :content)
                  :to-match "(that is quoted in the user's prose)"))))

    (it "also tolerates a literal #+begin_user line inside user body"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "Show me:\n"
                  "#+begin_user\n"
                  "nested quoting\n"
                  "#+end_user\n")
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (length turns) :to-equal 1)
          (expect (plist-get (car turns) :role) :to-equal 'user)
          (expect (plist-get (car turns) :content)
                  :to-match "nested quoting")))))

  (describe "user prompt composed with org structural features"
    (it "parses a user block with headings, src block, list, emphasis as a single turn"
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
        (let* ((turns (gptel-chat--parse-buffer))
               (user (car turns))
               (content (plist-get user :content)))
          (expect (length turns) :to-equal 1)
          (expect (plist-get user :role) :to-equal 'user)
          (expect content :to-match "\\* Context")
          (expect content :to-match "#\\+begin_src emacs-lisp")
          (expect content :to-match "#\\+end_src")
          (expect content :to-match "- item one")
          (expect content :to-match "/emphasis/"))))

    (it "does not terminate early on inner #+end_src"
      (gptel-chat-test--with-buffer
          (concat "#+begin_user\n"
                  "#+begin_src bash\n"
                  "echo hello\n"
                  "#+end_src\n"
                  "That was a shell snippet.\n"
                  "#+end_user\n")
        (let* ((turns (gptel-chat--parse-buffer))
               (user (car turns)))
          (expect (length turns) :to-equal 1)
          (expect (plist-get user :content)
                  :to-match "That was a shell snippet."))))))

(describe "gptel-chat--parse-buffer: turn list shape and ordering"

  (describe "a single user/assistant pair"
    (it "produces two turns in document order"
      (gptel-chat-test--with-buffer gptel-chat-test--single-turn-pair
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (length turns) :to-equal 2)
          (expect (plist-get (nth 0 turns) :role) :to-equal 'user)
          (expect (plist-get (nth 1 turns) :role) :to-equal 'assistant))))

    (it "captures user content verbatim (no delimiter lines, trailing newline preserved)"
      (gptel-chat-test--with-buffer gptel-chat-test--single-turn-pair
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (plist-get (nth 0 turns) :content)
                  :to-equal "What's the capital of France?\n"))))

    (it "exposes start and end as markers pointing into the buffer"
      (gptel-chat-test--with-buffer gptel-chat-test--single-turn-pair
        (let* ((turns (gptel-chat--parse-buffer))
               (user (nth 0 turns))
               (start (plist-get user :start))
               (end   (plist-get user :end)))
          (expect (markerp start) :to-be-truthy)
          (expect (markerp end) :to-be-truthy)
          (expect (marker-buffer start) :to-equal (current-buffer))
          (expect (> (marker-position end) (marker-position start))
                  :to-be-truthy)))))

  (describe "turns distributed across org headings"
    (it "emits turns in document order, independent of heading ownership"
      (gptel-chat-test--with-buffer gptel-chat-test--heading-distributed
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (mapcar (lambda (tn)
                            (cons (plist-get tn :role)
                                  (when (eq (plist-get tn :role) 'user)
                                    (plist-get tn :content))))
                          turns)
                  :to-equal
                  '((user . "First question.\n")
                    (assistant)
                    (user . "Follow-up.\n")))))))

  (describe "empty block"
    (it "emits a turn with empty content (message filtering happens downstream)"
      (gptel-chat-test--with-buffer "#+begin_user\n#+end_user\n"
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (length turns) :to-equal 1)
          (expect (plist-get (car turns) :role) :to-equal 'user)
          (expect (plist-get (car turns) :content) :to-equal ""))))

    ;; Pins the empty-assistant-turn contract (spec §"Message construction
    ;; from buffer" — Empty-turn contract): an assistant block with no body
    ;; between delimiters parses to a turn whose `:segments' is nil.  This
    ;; is one of the two shapes the downstream message-construction empty
    ;; rule keys off (the other being all-whitespace text segments).
    (it "emits an assistant turn with :segments nil for an empty block"
      (gptel-chat-test--with-buffer "#+begin_assistant\n#+end_assistant\n"
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (length turns) :to-equal 1)
          (expect (plist-get (car turns) :role) :to-equal 'assistant)
          (expect (plist-get (car turns) :segments) :to-equal nil))))

    ;; Pins the other half of the empty-assistant-turn contract (spec
    ;; §"Message construction from buffer" — Empty-turn contract): an
    ;; assistant block whose body is whitespace-only parses to a turn
    ;; whose `:segments' is a non-empty list of `text' segments whose
    ;; payload is whitespace-only.  The sibling `:segments nil' spec
    ;; above pins the fully-empty branch; this spec pins the
    ;; all-whitespace branch so a regression that silently trims
    ;; whitespace segments (collapsing them to `:segments nil' or
    ;; dropping them entirely) fails at the parser layer rather than
    ;; sliding through invisibly because the end-to-end message list is
    ;; nil either way.
    (it "emits an assistant turn with whitespace-only text segments for a whitespace-only block"
      (gptel-chat-test--with-buffer "#+begin_assistant\n   \n#+end_assistant\n"
        (let* ((turns (gptel-chat--parse-buffer))
               (assistant (car turns))
               (segs (plist-get assistant :segments)))
          (expect (length turns) :to-equal 1)
          (expect (plist-get assistant :role) :to-equal 'assistant)
          (expect (listp segs) :to-be-truthy)
          (expect (>= (length segs) 1) :to-be-truthy)
          ;; Every segment is a `text' segment.
          (expect (cl-every (lambda (s) (eq (plist-get s :type) 'text))
                            segs)
                  :to-be-truthy)
          ;; Every segment's payload is whitespace-only (no
          ;; non-whitespace characters).
          (expect (cl-every
                   (lambda (s)
                     (let ((content (plist-get s :content)))
                       (and (stringp content)
                            (not (string-match-p "[^ \t\n\r]" content)))))
                   segs)
                  :to-be-truthy)))))

  ;; The parser binds `case-fold-search t' to match org's own
  ;; case-insensitive handling of structural keywords.  This test pins
  ;; the invariant so a future refactor that replaces the regexes or
  ;; wraps them in a case-sensitive `cond' fails loudly.
  (describe "case-insensitive delimiter matching"
    (it "parses mixed-case #+BEGIN_USER / #+End_User / #+END_ASSISTANT"
      (gptel-chat-test--with-buffer
          (concat "#+BEGIN_USER\n"
                  "q\n"
                  "#+End_User\n"
                  "#+begin_assistant\n"
                  "a\n"
                  "#+END_ASSISTANT\n")
        (let ((turns (gptel-chat--parse-buffer)))
          (expect (length turns) :to-equal 2)
          (expect (plist-get (nth 0 turns) :role) :to-equal 'user)
          (expect (plist-get (nth 0 turns) :content) :to-equal "q\n")
          (expect (plist-get (nth 1 turns) :role) :to-equal 'assistant))))))

(describe "gptel-chat--parse-buffer: assistant segments with tool calls"

  (it "splits an assistant block into (text, tool-call, text) segments"
    (gptel-chat-test--with-buffer
        (concat "#+begin_user\n"
                "q\n"
                "#+end_user\n"
                gptel-chat-test--tool-call-turn)
      (let* ((turns (gptel-chat--parse-buffer))
             (assistant (nth 1 turns))
             (segs (plist-get assistant :segments)))
        (expect (plist-get assistant :role) :to-equal 'assistant)
        (expect (length segs) :to-equal 3)
        (expect (plist-get (nth 0 segs) :type) :to-equal 'text)
        (expect (plist-get (nth 1 segs) :type) :to-equal 'tool-call)
        (expect (plist-get (nth 2 segs) :type) :to-equal 'text))))

  (it "captures the tool call's name and result"
    (gptel-chat-test--with-buffer gptel-chat-test--tool-call-turn
      (let* ((turns (gptel-chat--parse-buffer))
             (assistant (car turns))
             (segs (plist-get assistant :segments))
             (tc (cl-find-if
                  (lambda (s) (eq (plist-get s :type) 'tool-call))
                  segs)))
        (expect tc :not :to-be nil)
        (expect (plist-get tc :name) :to-equal "run_bash_command")
        (expect (plist-get tc :result)
                :to-match "Darwin"))))

  (it "produces no tool-call segment for an assistant with no tool blocks"
    (gptel-chat-test--with-buffer
        (concat "#+begin_assistant\n"
                "Just prose.\n"
                "#+end_assistant\n")
      (let* ((turns (gptel-chat--parse-buffer))
             (segs  (plist-get (car turns) :segments)))
        (expect (length segs) :to-equal 1)
        (expect (plist-get (car segs) :type) :to-equal 'text))))

  ;; Regression: the outer `while' loop in
  ;; `gptel-chat--scan-assistant-body' must advance `text-from' past
  ;; each `#+end_tool' it consumes.  A bug where it fails to advance
  ;; (e.g., off-by-one on `text-from') only surfaces with >=2 tool
  ;; calls — hence this scenario mandated by spec
  ;; §"Tool-call rendering inside assistant blocks" → "Multiple tool
  ;; calls in a response".
  (it "splits an assistant with two tool calls into (text tool-call text tool-call text)"
    (gptel-chat-test--with-buffer
        (concat "#+begin_assistant\n"
                "prose1\n"
                "\n"
                "#+begin_tool (tool_a :x 1)\n"
                "result_a\n"
                "#+end_tool\n"
                "\n"
                "prose2\n"
                "\n"
                "#+begin_tool (tool_b :y 2)\n"
                "result_b\n"
                "#+end_tool\n"
                "\n"
                "prose3\n"
                "#+end_assistant\n")
      (let* ((turns (gptel-chat--parse-buffer))
             (assistant (car turns))
             (segs (plist-get assistant :segments)))
        (expect (plist-get assistant :role) :to-equal 'assistant)
        (expect (length segs) :to-equal 5)
        (expect (mapcar (lambda (s) (plist-get s :type)) segs)
                :to-equal '(text tool-call text tool-call text))
        (expect (plist-get (nth 1 segs) :name) :to-equal "tool_a")
        (expect (plist-get (nth 1 segs) :result) :to-equal "result_a")
        (expect (plist-get (nth 3 segs) :name) :to-equal "tool_b")
        (expect (plist-get (nth 3 segs) :result) :to-equal "result_b")
        (expect (plist-get (nth 0 segs) :content) :to-match "prose1")
        (expect (plist-get (nth 2 segs) :content) :to-match "prose2")
        (expect (plist-get (nth 4 segs) :content) :to-match "prose3")))))

(provide 'buffer-format-spec)

;;; buffer-format-spec.el ends here
