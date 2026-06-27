;;; tool-header-truncation-spec.el --- Display-overlay truncation of long #+begin_tool headers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat--display-truncate-tool-headers'
;; (config/gptel/chat/display.org §Tool-header truncation).
;;
;; A `#+begin_tool (...)' header carries the full model-supplied
;; argument plist as its buffer text — long `run_bash_command' calls
;; routinely push the line past 100 columns and wrap raggedly.  The
;; display layer installs a `gptel-chat-display'-tagged overlay with
;; a `display' property of ` ...)' over the overrun portion so the
;; header *renders* short while the buffer text stays full-fidelity
;; for the parser, send path, and on-disk file.
;;
;; Mirrors upstream `gptel-mode' visually (gptel.el:1879-1886) without
;; touching our header-as-source-of-truth contract.

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

(defconst gptel-chat-tool-truncate-test--long-header
  ;; A real-world-shaped header: `run_bash_command' with a verbose
  ;; command string.  Header text is 123 columns wide.
  (concat "#+begin_user\n"
          "  question\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "#+begin_tool (run_bash_command :command \"ls -la "
          "/usr/local/bin/brew /opt/homebrew/bin/brew 2>/dev/null\")\n"
          "  {\"output\":\"...\"}\n"
          "#+end_tool\n"
          "  answer.\n"
          "#+end_assistant\n")
  "A buffer with a long `#+begin_tool' header (over 100 columns).")

(defconst gptel-chat-tool-truncate-test--short-header
  (concat "#+begin_assistant\n"
          "#+begin_tool (foo :x 1)\n"
          "  ok\n"
          "#+end_tool\n"
          "#+end_assistant\n")
  "A buffer with a short `#+begin_tool' header (well below 80 columns).")


(defmacro gptel-chat-tool-truncate-test--with-chat-buffer (content &rest body)
  "Create a temp buffer populated with CONTENT in `gptel-chat-mode' and run BODY.
The buffer is killed on exit.  Activation runs the real
`gptel-chat-mode-hook', which installs the display layer and fires
the initial overlay refresh synchronously."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (generate-new-buffer " *gptel-chat-tool-truncate-test*")))
     (unwind-protect
         (with-current-buffer buf
           (insert ,content)
           (goto-char (point-min))
           (gptel-chat-mode)
           ,@body)
       (kill-buffer buf))))

(defun gptel-chat-tool-truncate-test--display-overlays (&optional buffer)
  "Return `gptel-chat-display' overlays in BUFFER carrying a `display' property."
  (with-current-buffer (or buffer (current-buffer))
    (cl-remove-if-not
     (lambda (ov) (and (overlay-get ov 'gptel-chat-display)
                       (overlay-get ov 'display)))
     (overlays-in (point-min) (point-max)))))


;;; Tests --------------------------------------------------------------------

(describe "gptel-chat--display-truncate-tool-headers"

  (it "installs a `display' overlay on a header that exceeds the budget"
    (let ((gptel-chat-display-tool-header-max-width 80))
      (gptel-chat-tool-truncate-test--with-chat-buffer
          gptel-chat-tool-truncate-test--long-header
        (let ((ovs (gptel-chat-tool-truncate-test--display-overlays)))
          (expect (length ovs) :to-equal 1)
          (expect (overlay-get (car ovs) 'display) :to-equal " ...)")))))

  (it "installs no truncation overlay when the header is within budget"
    (let ((gptel-chat-display-tool-header-max-width 80))
      (gptel-chat-tool-truncate-test--with-chat-buffer
          gptel-chat-tool-truncate-test--short-header
        (expect (length (gptel-chat-tool-truncate-test--display-overlays))
                :to-equal 0))))

  (it "covers the trailing portion of the header line (not its head)"
    ;; The overlay must start past the budget cut and extend to EOL,
    ;; so the visible portion before the ellipsis is the prefix of
    ;; the header text — not the tail.
    (let ((gptel-chat-display-tool-header-max-width 80))
      (gptel-chat-tool-truncate-test--with-chat-buffer
          gptel-chat-tool-truncate-test--long-header
        (let* ((ov (car (gptel-chat-tool-truncate-test--display-overlays)))
               (start (overlay-start ov))
               (end (overlay-end ov)))
          ;; The overlay START must be on the `#+begin_tool' line — not
          ;; before it (would clip prose) and not at EOL (zero-width).
          (save-excursion
            (goto-char start)
            (expect (buffer-substring-no-properties (line-beginning-position)
                                                    (+ (line-beginning-position) 13))
                    :to-equal "#+begin_tool ")
            ;; END is end-of-line of the same header line.
            (expect end :to-equal (line-end-position))
            ;; Visible width budget: prefix-before-overlay + display-string
            ;; must equal the budget.
            (goto-char start)
            (expect (+ (current-column) (length " ...)"))
                    :to-equal 80))))))

  (it "leaves buffer text byte-identical — truncation is display-only"
    (let ((gptel-chat-display-tool-header-max-width 80))
      (gptel-chat-tool-truncate-test--with-chat-buffer
          gptel-chat-tool-truncate-test--long-header
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal gptel-chat-tool-truncate-test--long-header))))

  (it "disables truncation when `gptel-chat-display-tool-header-max-width' is nil"
    (let ((gptel-chat-display-tool-header-max-width nil))
      (gptel-chat-tool-truncate-test--with-chat-buffer
          gptel-chat-tool-truncate-test--long-header
        (expect (length (gptel-chat-tool-truncate-test--display-overlays))
                :to-equal 0))))

  (it "respects a custom budget — narrower budget truncates more headers"
    ;; The short fixture's header is ~23 cols.  With budget = 20 the
    ;; header now exceeds budget and must get truncated.
    (let ((gptel-chat-display-tool-header-max-width 20))
      (gptel-chat-tool-truncate-test--with-chat-buffer
          gptel-chat-tool-truncate-test--short-header
        (let ((ovs (gptel-chat-tool-truncate-test--display-overlays)))
          (expect (length ovs) :to-equal 1)
          (expect (overlay-get (car ovs) 'display) :to-equal " ...)")))))

  (it "tags the truncation overlay with `gptel-chat-display' for removal"
    (let ((gptel-chat-display-tool-header-max-width 80))
      (gptel-chat-tool-truncate-test--with-chat-buffer
          gptel-chat-tool-truncate-test--long-header
        (dolist (ov (gptel-chat-tool-truncate-test--display-overlays))
          (expect (overlay-get ov 'gptel-chat-display) :to-be-truthy)))))

  (it "removes the truncation overlay when the display layer is toggled off"
    (let ((gptel-chat-display-tool-header-max-width 80))
      (gptel-chat-tool-truncate-test--with-chat-buffer
          gptel-chat-tool-truncate-test--long-header
        (expect (length (gptel-chat-tool-truncate-test--display-overlays))
                :to-equal 1)
        (gptel-chat-toggle-display-layer)
        (expect (length (gptel-chat-tool-truncate-test--display-overlays))
                :to-equal 0))))

  (it "ignores `#+begin_tool' inside `#+begin_user' (only assistant turns are walked)"
    ;; The refresh helper iterates assistant-role turns only; a tool-
    ;; shaped line in a user turn is content, not a tool block, and
    ;; must not receive a truncation overlay.
    (let ((gptel-chat-display-tool-header-max-width 20))
      (gptel-chat-tool-truncate-test--with-chat-buffer
          (concat "#+begin_user\n"
                  "  #+begin_tool (this is content, not a real tool block)\n"
                  "#+end_user\n")
        (expect (length (gptel-chat-tool-truncate-test--display-overlays))
                :to-equal 0)))))

(provide 'gptel-chat-tool-header-truncation-spec)

;;; tool-header-truncation-spec.el ends here
