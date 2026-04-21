;;; display-layer-spec.el --- Buttercup tests for gptel-chat display layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat-display' — the overlay-based role
;; distinction layer and its toggle command.
;;
;; Coverage (from openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md
;; §"Display-layer role distinction"):
;;   - Display layer is active by default (overlays on user + assistant
;;     block bodies with role-specific faces).
;;   - Display layer can be toggled off (overlays removed, buffer text
;;     unchanged).
;;   - Toggle back on → overlays reappear.
;;   - Buffer text is never modified (buffer-substring-no-properties
;;     equals the on-disk string).
;;
;; All tests drive `gptel-chat-mode' directly so the hook-installed
;; activation path (debounced after-change + initial refresh) is
;; exercised end-to-end.  No mocking of overlays or parser.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/display/;
;; two levels up is .../config/gptel/chat/, which holds `display.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-mode)
(require 'gptel-chat-parser)
(require 'gptel-chat-display)


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-display-test--two-turns
  (concat "#+begin_user\n"
          "Hello there.\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "General Kenobi.\n"
          "#+end_assistant\n")
  "Minimal buffer content with one user turn and one assistant turn.")

(defun gptel-chat-display-test--display-overlays (&optional buffer)
  "Return the list of `gptel-chat-display'-tagged overlays in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cl-remove-if-not
     (lambda (ov) (overlay-get ov 'gptel-chat-display))
     (overlays-in (point-min) (point-max)))))

(defun gptel-chat-display-test--overlays-with-face (face &optional buffer)
  "Return `gptel-chat-display' overlays in BUFFER whose `face' property equals FACE."
  (cl-remove-if-not
   (lambda (ov) (eq (overlay-get ov 'face) face))
   (gptel-chat-display-test--display-overlays buffer)))

(defmacro gptel-chat-display-test--with-chat-buffer (content &rest body)
  "Create a temp buffer populated with CONTENT in `gptel-chat-mode' and run BODY.

The buffer is killed on exit.  Activation runs the real
`gptel-chat-mode-hook', which installs the display layer's
after-change hook and runs the initial overlay refresh
synchronously."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (generate-new-buffer " *gptel-chat-display-test*")))
     (unwind-protect
         (with-current-buffer buf
           (insert ,content)
           (goto-char (point-min))
           (gptel-chat-mode)
           ,@body)
       (kill-buffer buf))))


;;; Tests --------------------------------------------------------------------

(describe "gptel-chat-display: default activation"

  (it "installs overlays on both user and assistant block bodies"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (let ((user-ovs (gptel-chat-display-test--overlays-with-face
                      'gptel-chat-user-face))
           (asst-ovs (gptel-chat-display-test--overlays-with-face
                      'gptel-chat-assistant-face)))
       (expect (length user-ovs) :to-equal 1)
       (expect (length asst-ovs) :to-equal 1))))

  (it "overlays cover block bodies but NOT delimiter lines"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (let* ((user-ov (car (gptel-chat-display-test--overlays-with-face
                           'gptel-chat-user-face)))
            (user-text (buffer-substring-no-properties
                        (overlay-start user-ov)
                        (overlay-end user-ov))))
       ;; The body text is exactly "Hello there.\n" — no delimiters.
       (expect user-text :to-equal "Hello there.\n")
       (expect user-text :not :to-match "#\\+begin_user")
       (expect user-text :not :to-match "#\\+end_user"))
     (let* ((asst-ov (car (gptel-chat-display-test--overlays-with-face
                           'gptel-chat-assistant-face)))
            (asst-text (buffer-substring-no-properties
                        (overlay-start asst-ov)
                        (overlay-end asst-ov))))
       (expect asst-text :to-equal "General Kenobi.\n")
       (expect asst-text :not :to-match "#\\+begin_assistant")
       (expect asst-text :not :to-match "#\\+end_assistant"))))

  (it "does not modify buffer text (buffer-substring-no-properties matches input)"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (expect (buffer-substring-no-properties (point-min) (point-max))
             :to-equal gptel-chat-display-test--two-turns)))

  (it "starts with `gptel-chat--display-enabled' set to t"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (expect gptel-chat--display-enabled :to-be-truthy))))


(describe "gptel-chat-display: overlay tagging"

  (it "every installed overlay carries the `gptel-chat-display' property"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (let ((ovs (overlays-in (point-min) (point-max))))
       ;; Every overlay this module installs must be tagged, so
       ;; `remove-overlays ... gptel-chat-display t' can target them.
       (dolist (ov (cl-remove-if-not
                    (lambda (o) (memq (overlay-get o 'face)
                                      '(gptel-chat-user-face
                                        gptel-chat-assistant-face)))
                    ovs))
         (expect (overlay-get ov 'gptel-chat-display) :to-be-truthy))))))


(describe "gptel-chat-toggle-display-layer"

  (it "removes all gptel-chat-display overlays when toggled off"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 2)
     (gptel-chat-toggle-display-layer)
     (expect gptel-chat--display-enabled :to-equal nil)
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 0)))

  (it "keeps buffer text unchanged when toggled off"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (gptel-chat-toggle-display-layer)
     (expect (buffer-substring-no-properties (point-min) (point-max))
             :to-equal gptel-chat-display-test--two-turns)))

  (it "reinstalls overlays when toggled back on"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     ;; off
     (gptel-chat-toggle-display-layer)
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 0)
     ;; on
     (gptel-chat-toggle-display-layer)
     (expect gptel-chat--display-enabled :to-be-truthy)
     (expect (length (gptel-chat-display-test--overlays-with-face
                      'gptel-chat-user-face))
             :to-equal 1)
     (expect (length (gptel-chat-display-test--overlays-with-face
                      'gptel-chat-assistant-face))
             :to-equal 1))))


(describe "gptel-chat--refresh-overlays: idempotence and robustness"

  (it "is idempotent — repeated calls do not stack overlays"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (gptel-chat--refresh-overlays)
     (gptel-chat--refresh-overlays)
     (gptel-chat--refresh-overlays)
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 2)))

  (it "treats a mid-edit unclosed block as a no-op (no overlays, no error)"
    ;; A buffer containing only `#+begin_user' (no close) is the kind of
    ;; state the debounced refresh can land on between keystrokes.  The
    ;; refresher must swallow the parser's `user-error' so typing is not
    ;; interrupted.
    (gptel-chat-display-test--with-chat-buffer
     "#+begin_user\nhalf-typed\n"
     (expect (gptel-chat--refresh-overlays) :not :to-throw)
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 0))))


;;; display-layer-spec.el ends here
(provide 'gptel-chat-display-layer-spec)
