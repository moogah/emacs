;;; tool-block-rendering-spec.el --- Buttercup tests for nested tool-block fontification and auto-fold -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat-mode' tool-block rendering
;; (config/gptel/chat/mode.org §Tool-block rendering).
;;
;; Org's own block fontifier (`org-fontify-meta-lines-and-blocks-1')
;; does not recurse into nested special blocks: when an outer
;; `#+begin_assistant' wraps inner `#+begin_tool' / `#+end_tool'
;; blocks, the inner delimiter lines are seen as plain content and
;; never receive `org-block-begin-line' / `org-block-end-line' face.
;; `gptel-chat-mode' restores the visual cue via
;; `gptel-chat--tool-block-font-lock-keywords' and auto-folds every
;; nested tool block on mode activation via
;; `gptel-chat--fold-tool-blocks'.
;;
;; Tests drive `gptel-chat-mode' directly and call `font-lock-ensure',
;; so the hook-installed `font-lock-add-keywords' + startup-visibility
;; path is exercised end-to-end.  No mocking of font-lock or org
;; internals.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; spec-dir is .../config/gptel/chat/test/mode/; two levels up holds mode.el.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-mode)
(require 'gptel-chat-stream)


;;; Helpers ------------------------------------------------------------------

(defun gptel-chat-tool-test--faces-at (pos)
  "Return the `face' text property at POS as a normalized list of faces."
  (let ((face (get-text-property pos 'face)))
    (cond ((null face) nil)
          ((listp face) face)
          (t (list face)))))

(defun gptel-chat-tool-test--face-at-p (pos sym)
  "Return non-nil when face SYM is present in the `face' property at POS."
  (and (memq sym (gptel-chat-tool-test--faces-at pos)) t))

(defmacro gptel-chat-tool-test--with-fontified (content &rest body)
  "Run BODY in a fontified `gptel-chat-mode' buffer containing CONTENT.
The buffer is activated into `gptel-chat-mode' (which runs the mode
hook, including `gptel-chat--apply-startup-visibility') and
`font-lock-ensure'd before BODY runs; point starts at `point-min'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (gptel-chat-mode)
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-tool-test--nested-tool-turn
  (concat "#+begin_user\n"
          "  question?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "#+begin_tool (run_bash_command :command \"ls\")\n"
          "  {\"output\":\"hello\\nworld\"}\n"
          "#+end_tool\n"
          "  answer prose.\n"
          "#+end_assistant\n")
  "An assistant turn wrapping one nested `#+begin_tool' block.
Reproduces the on-disk shape `gptel-chat-mode' writes — outer turn
block, inner tool block, indented bodies.")

(defconst gptel-chat-tool-test--multi-tool-turn
  (concat "#+begin_assistant\n"
          "#+begin_tool (run_bash_command :command \"ls\")\n"
          "  {\"output\":\"a\"}\n"
          "#+end_tool\n"
          "#+begin_tool (run_bash_command :command \"pwd\")\n"
          "  {\"output\":\"/tmp\"}\n"
          "#+end_tool\n"
          "  done.\n"
          "#+end_assistant\n")
  "An assistant turn with two nested tool blocks.
Used to assert the fold pass folds every tool block, not just the first.")


;;; Specs --------------------------------------------------------------------

(describe "gptel-chat-mode tool-block rendering"

  (describe "font-lock — nested-tool delimiter fontification"

    (it "paints `#+begin_tool' with `org-block-begin-line' face even when nested inside `#+begin_assistant'"
      (gptel-chat-tool-test--with-fontified
          gptel-chat-tool-test--nested-tool-turn
        (re-search-forward "^#\\+begin_tool")
        (let ((bol (line-beginning-position)))
          (expect (gptel-chat-tool-test--face-at-p
                   bol 'org-block-begin-line)
                  :to-be-truthy))))

    (it "paints `#+end_tool' with `org-block-end-line' face even when nested inside `#+begin_assistant'"
      (gptel-chat-tool-test--with-fontified
          gptel-chat-tool-test--nested-tool-turn
        (re-search-forward "^#\\+end_tool")
        (let ((bol (line-beginning-position)))
          (expect (gptel-chat-tool-test--face-at-p
                   bol 'org-block-end-line)
                  :to-be-truthy))))

    (it "paints both delimiter lines of every nested tool block in a multi-tool turn"
      (gptel-chat-tool-test--with-fontified
          gptel-chat-tool-test--multi-tool-turn
        ;; All `#+begin_tool' lines must be styled.
        (goto-char (point-min))
        (cl-loop while (re-search-forward "^#\\+begin_tool" nil t) do
                 (expect (gptel-chat-tool-test--face-at-p
                          (line-beginning-position) 'org-block-begin-line)
                         :to-be-truthy))
        ;; All `#+end_tool' lines must be styled.
        (goto-char (point-min))
        (cl-loop while (re-search-forward "^#\\+end_tool" nil t) do
                 (expect (gptel-chat-tool-test--face-at-p
                          (line-beginning-position) 'org-block-end-line)
                         :to-be-truthy))))

    (it "leaves the outer `#+begin_assistant' delimiter's existing org-block face untouched (override is scoped to `_tool')"
      (gptel-chat-tool-test--with-fontified
          gptel-chat-tool-test--nested-tool-turn
        (re-search-forward "^#\\+begin_assistant")
        ;; Org paints this natively with `org-block-begin-line'; the
        ;; tool-block matcher must not have claimed it.
        (expect (gptel-chat-tool-test--face-at-p
                 (line-beginning-position) 'org-block-begin-line)
                :to-be-truthy))))

  (describe "auto-fold — nested tool-block visibility on mode activation"

    (it "folds the body of every nested `#+begin_tool' block on `gptel-chat-mode' activation"
      (gptel-chat-tool-test--with-fontified
          gptel-chat-tool-test--nested-tool-turn
        ;; The body line between `#+begin_tool' and `#+end_tool' must
        ;; carry `org-hide-block' invisibility installed by
        ;; `org-fold-hide-block-toggle'.
        (re-search-forward "^#\\+begin_tool")
        (forward-line 1)
        (expect (get-char-property (point) 'invisible)
                :to-be 'org-hide-block)))

    (it "leaves the outer `#+begin_assistant' body visible (turn bodies stay expanded)"
      (gptel-chat-tool-test--with-fontified
          gptel-chat-tool-test--nested-tool-turn
        ;; The trailing assistant prose line (outside any tool block)
        ;; must remain visible.
        (re-search-forward "answer prose")
        (expect (get-char-property (line-beginning-position) 'invisible)
                :to-be nil)))

    (it "leaves the outer `#+begin_user' body visible"
      (gptel-chat-tool-test--with-fontified
          gptel-chat-tool-test--nested-tool-turn
        (re-search-forward "question\\?")
        (expect (get-char-property (line-beginning-position) 'invisible)
                :to-be nil)))

    (it "folds every tool block in a multi-tool turn, not just the first"
      (gptel-chat-tool-test--with-fontified
          gptel-chat-tool-test--multi-tool-turn
        ;; Probe the body of each tool block.  Both must be folded.
        (goto-char (point-min))
        (cl-loop while (re-search-forward "^#\\+begin_tool" nil t) do
                 (forward-line 1)
                 (expect (get-char-property (point) 'invisible)
                         :to-be 'org-hide-block))))

    (it "is a no-op on a buffer with no tool blocks — mode activation succeeds on a `gptel-chat-new'-style empty user block"
      (expect
       (with-temp-buffer
         (insert "#+begin_user\n  \n#+end_user\n")
         (gptel-chat-mode))
       :not :to-throw)))

  (describe "streaming auto-fold — tool blocks fold as the result arrives"

    ;; The startup-visibility pass folds tool blocks that exist when
    ;; the mode activates.  Blocks streamed in later (during a live
    ;; assistant turn) must also end folded — otherwise the user
    ;; watches each tool result balloon to full size before any
    ;; later cleanup hides it.  `gptel-chat--stream-close-tool-block'
    ;; runs the fold right after inserting the result text (fold
    ;; timing must be post-insert — invisibility set on an empty
    ;; range does not extend to text inserted into it later).  Mirrors
    ;; upstream `gptel-mode' behavior at `gptel.el:1919-1924'.

    (it "folds a tool block whose body is filled in via `gptel-chat--stream-close-tool-block' (post-streaming fold)"
      (with-temp-buffer
        ;; Set up an empty tool block — matching the shape
        ;; `gptel-chat--stream-open-tool-block' leaves behind: header,
        ;; empty body line, closer.
        (insert "#+begin_assistant\n")
        (insert "#+begin_tool (foo :arg 1)\n")
        (let ((body-marker (copy-marker (point) t)))
          (insert "#+end_tool\n")
          (insert "#+end_assistant\n")
          ;; Activate the mode AFTER the open shape is in place so
          ;; the startup-visibility pass sees this block and folds
          ;; it — then unfold so the streaming fold is what we are
          ;; verifying.
          (gptel-chat-mode)
          (save-excursion
            (goto-char body-marker)
            (re-search-backward "^#\\+begin_tool")
            (org-fold-hide-block-toggle 'off 'no-error))
          ;; Sanity: body line is now visible before the close call.
          (expect (get-char-property body-marker 'invisible) :to-be nil)
          ;; Run the close — this inserts the result AND folds.
          (gptel-chat--stream-close-tool-block body-marker "  ok\n")
          ;; The inserted body line must end up folded.
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^#\\+begin_tool")
            (forward-line 1)
            (expect (get-char-property (point) 'invisible)
                    :to-be 'org-hide-block)))))

    (it "does not signal in a non-Org buffer — the fold is guarded for isolated-test callers"
      (expect
       (with-temp-buffer
         (insert "#+begin_tool (foo)\n")
         (let ((body-marker (copy-marker (point) t)))
           (insert "#+end_tool\n")
           ;; Fundamental-mode buffer: `org-element-at-point' would
           ;; warn-and-bail.  The guard must short-circuit the fold
           ;; without raising so existing stream tests that use raw
           ;; scratch buffers continue to work.
           (gptel-chat--stream-close-tool-block body-marker "ok")))
       :not :to-throw))))

;;; tool-block-rendering-spec.el ends here
