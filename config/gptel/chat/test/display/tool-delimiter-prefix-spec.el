;;; tool-delimiter-prefix-spec.el --- Path C tool-delimiter line-prefix overlays -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat--display-prefix-tool-delimiters' —
;; Path C of the body-indentation design.  Nested `#+begin_tool' /
;; `#+end_tool' delimiter lines stay at *real* column 0 in the buffer
;; (the parser anchors them there); the display layer installs a
;; `line-prefix' overlay of body-width spaces on each so they *render*
;; aligned with the indented assistant body around them.  See
;; `openspec/changes/gptel-chat-heading-scoping/design.md' Decision 5
;; and the `Chat-block body indentation' requirement of
;; `specs/gptel/chat-mode.md'.
;;
;; The overlay is display-only: buffer text, on-disk content, and
;; parser input are all unaffected — the tool delimiter lines still
;; start at column 0.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-mode)
(require 'gptel-chat-parser)
(require 'gptel-chat-display)
(require 'gptel-chat-send)

;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-tool-prefix-test--buffer
  (concat "#+begin_user\n"
          "  Run the search.\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "  Looking it up.\n"
          "#+begin_tool (search :q \"foo\")\n"
          "  tool result line\n"
          "#+end_tool\n"
          "  Done.\n"
          "#+end_assistant\n")
  "A buffer with one user turn and an assistant turn nesting a tool block.
Body content is already indented; the `#+begin_tool' / `#+end_tool'
delimiter lines sit at real column 0.")

(defmacro gptel-chat-tool-prefix-test--with-chat-buffer (content &rest body)
  "Create a temp buffer populated with CONTENT in `gptel-chat-mode' and run BODY.

The buffer is killed on exit.  Activation runs the real
`gptel-chat-mode-hook', which installs the display layer and runs
the initial overlay refresh synchronously — so the Path C overlays
are present before BODY runs."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (generate-new-buffer " *gptel-chat-tool-prefix-test*")))
     (unwind-protect
         (with-current-buffer buf
           (insert ,content)
           (goto-char (point-min))
           (gptel-chat-mode)
           ,@body)
       (kill-buffer buf))))

(defun gptel-chat-tool-prefix-test--line-prefix-overlays (&optional buffer)
  "Return `gptel-chat-display' overlays in BUFFER carrying a `line-prefix'."
  (with-current-buffer (or buffer (current-buffer))
    (cl-remove-if-not
     (lambda (ov) (and (overlay-get ov 'gptel-chat-display)
                       (overlay-get ov 'line-prefix)))
     (overlays-in (point-min) (point-max)))))

(defun gptel-chat-tool-prefix-test--overlay-line-text (ov)
  "Return the buffer text of the line spanned by overlay OV."
  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))

;;; Tests --------------------------------------------------------------------

(describe "gptel-chat--display-prefix-tool-delimiters"

  (it "installs a line-prefix overlay on each tool delimiter line"
    (gptel-chat-tool-prefix-test--with-chat-buffer
        gptel-chat-tool-prefix-test--buffer
      ;; One overlay on `#+begin_tool', one on `#+end_tool'.
      (let ((ovs (gptel-chat-tool-prefix-test--line-prefix-overlays)))
        (expect (length ovs) :to-equal 2))))

  (it "the line-prefix is body-width spaces"
    (gptel-chat-tool-prefix-test--with-chat-buffer
        gptel-chat-tool-prefix-test--buffer
      (let ((expected (make-string (gptel-chat--body-indent) ?\s)))
        (dolist (ov (gptel-chat-tool-prefix-test--line-prefix-overlays))
          (expect (overlay-get ov 'line-prefix) :to-equal expected)))))

  (it "the overlays land on the `#+begin_tool' / `#+end_tool' lines"
    (gptel-chat-tool-prefix-test--with-chat-buffer
        gptel-chat-tool-prefix-test--buffer
      (let ((texts (sort (mapcar #'gptel-chat-tool-prefix-test--overlay-line-text
                                 (gptel-chat-tool-prefix-test--line-prefix-overlays))
                         #'string<)))
        (expect texts :to-equal
                (list "#+begin_tool (search :q \"foo\")"
                      "#+end_tool")))))

  (it "leaves the tool delimiter buffer text at real column 0"
    ;; The overlay is display-only: the underlying text is unchanged,
    ;; so the parser's `^#\\+begin_tool' / `^#\\+end_tool' anchors hold.
    (gptel-chat-tool-prefix-test--with-chat-buffer
        gptel-chat-tool-prefix-test--buffer
      (expect (string-match-p "^#\\+begin_tool (search :q \"foo\")$"
                              (buffer-string))
              :to-be-truthy)
      (expect (string-match-p "^#\\+end_tool$" (buffer-string))
              :to-be-truthy)
      ;; And the buffer text is byte-for-byte the fixture.
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal gptel-chat-tool-prefix-test--buffer)))

  (it "tags the Path C overlays with `gptel-chat-display' for removal"
    ;; The line-prefix overlays must carry the tag so
    ;; `gptel-chat--display-remove-all' clears them with the role
    ;; overlays.
    (gptel-chat-tool-prefix-test--with-chat-buffer
        gptel-chat-tool-prefix-test--buffer
      (dolist (ov (gptel-chat-tool-prefix-test--line-prefix-overlays))
        (expect (overlay-get ov 'gptel-chat-display) :to-be-truthy))))

  (it "removes the Path C overlays when the display layer is toggled off"
    (gptel-chat-tool-prefix-test--with-chat-buffer
        gptel-chat-tool-prefix-test--buffer
      (expect (length (gptel-chat-tool-prefix-test--line-prefix-overlays))
              :to-equal 2)
      (gptel-chat-toggle-display-layer)
      (expect (length (gptel-chat-tool-prefix-test--line-prefix-overlays))
              :to-equal 0)))

  (it "installs no tool-delimiter overlays for a buffer with no tool block"
    (gptel-chat-tool-prefix-test--with-chat-buffer
        (concat "#+begin_user\n"
                "  Hello.\n"
                "#+end_user\n"
                "\n"
                "#+begin_assistant\n"
                "  Hi back.\n"
                "#+end_assistant\n")
      (expect (length (gptel-chat-tool-prefix-test--line-prefix-overlays))
              :to-equal 0))))

(provide 'gptel-chat-tool-delimiter-prefix-spec)

;;; tool-delimiter-prefix-spec.el ends here
