;;; drawer-fontification-spec.el --- Buttercup tests for drawer value emphasis suppression -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the property-drawer value fontification override
;; installed by `gptel-chat-mode' (config/gptel/chat/mode.org).
;;
;; Coverage (from openspec/changes/gptel-drawer-as-source-of-truth/design.md
;; §Addendum Finding A / Decision A; register entry
;; register/boundary/chat-mode-session-display override A):
;;   - A trailing-slash drawer value such as `:GPTEL_SCOPE_READ:
;;     /Users/jeff/emacs/' is a syntactically valid org `/emphasis/'
;;     span; without the override org renders it italic and dims the
;;     boundary `/' characters.  The override re-stamps the value span
;;     with `org-property-value' (no italic).
;;   - The override is a buffer-local font-lock keyword carrying the
;;     OVERRIDE flag, registered with `'append' ordering so it wins over
;;     org's emphasis keyword.
;;   - Chat-turn prose `/emphasis/' is unaffected — the override is
;;     scoped to property-drawer value spans only.
;;
;; Tests drive `gptel-chat-mode' directly and call `font-lock-ensure',
;; so the hook-installed `font-lock-add-keywords' path is exercised
;; end-to-end.  No mocking of font-lock or org internals.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/display/;
;; two levels up is .../config/gptel/chat/, which holds `mode.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-mode)


;;; Helpers ------------------------------------------------------------------

(defun gptel-chat-drawer-test--faces-at (pos)
  "Return the `face' text property at POS as a normalized list of faces.
The `face' property may be a single symbol or a list; normalize so
callers can `member'-test individual faces uniformly."
  (let ((face (get-text-property pos 'face)))
    (cond ((null face) nil)
          ((listp face) face)
          (t (list face)))))

(defun gptel-chat-drawer-test--face-at-p (pos sym)
  "Return non-nil when face SYM is present in the `face' property at POS."
  (and (memq sym (gptel-chat-drawer-test--faces-at pos)) t))

(defmacro gptel-chat-drawer-test--with-fontified (content &rest body)
  "Run BODY in a fontified `gptel-chat-mode' buffer containing CONTENT.
The buffer is activated into `gptel-chat-mode' and `font-lock-ensure'd
before BODY runs; point starts at `point-min'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (gptel-chat-mode)
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-drawer-test--scope-drawer
  (concat ":PROPERTIES:\n"
          ":GPTEL_SCOPE_READ: /Users/jeff/emacs/\n"
          ":END:\n")
  "A property drawer whose value is a trailing-slash directory path.
The trailing `/' closes a valid org emphasis span opened by the
leading `/', so without the override org fontifies the value italic.")

(defconst gptel-chat-drawer-test--drawer-plus-prose
  (concat ":PROPERTIES:\n"
          ":GPTEL_SCOPE_READ: /Users/jeff/emacs/\n"
          ":END:\n"
          "\n"
          "#+begin_user\n"
          "This sentence has /emphasized/ prose.\n"
          "#+end_user\n")
  "A scope drawer followed by a chat-turn user block containing prose
emphasis.  Used to assert the override does not flatten chat-turn
`/emphasis/'.")


;;; Specs --------------------------------------------------------------------

(describe "register/boundary/chat-mode-session-display"

  (describe "override A — drawer value emphasis suppression"

    (it "renders a trailing-slash drawer value (e.g. :GPTEL_SCOPE_READ: /Users/jeff/emacs/) with face org-property-value, not org italic emphasis"
      (gptel-chat-drawer-test--with-fontified
          gptel-chat-drawer-test--scope-drawer
        ;; Locate the value text and probe both boundary slashes.
        (search-forward "/Users")
        (let ((open-slash (- (point) (length "/Users"))))
          (expect (gptel-chat-drawer-test--face-at-p
                   open-slash 'org-property-value)
                  :to-be-truthy)
          (expect (gptel-chat-drawer-test--face-at-p open-slash 'italic)
                  :not :to-be-truthy))
        (search-forward "emacs/")
        (let ((close-slash (1- (point))))
          (expect (gptel-chat-drawer-test--face-at-p
                   close-slash 'org-property-value)
                  :to-be-truthy)
          (expect (gptel-chat-drawer-test--face-at-p close-slash 'italic)
                  :not :to-be-truthy))))

    (it "re-stamps drawer value spans via a buffer-local font-lock keyword carrying the OVERRIDE flag"
      ;; The override is installed as a buffer-local font-lock keyword
      ;; whose matcher is the named function and whose facespec carries
      ;; the OVERRIDE flag (the trailing `t' in `(1 (quote
      ;; org-property-value) t)').  Without that flag our face would not
      ;; win over org's emphasis keyword.
      (gptel-chat-drawer-test--with-fontified
          gptel-chat-drawer-test--scope-drawer
        ;; `font-lock-keywords' is buffer-local; (cadr font-lock-keywords)
        ;; is the compiled keyword list.
        (let ((entry (cl-find-if
                      (lambda (kw)
                        (and (consp kw)
                             (eq (car kw)
                                 'gptel-chat--drawer-value-matcher)))
                      (cadr font-lock-keywords))))
          ;; The matcher is registered ...
          (expect entry :to-be-truthy)
          ;; ... its facespec targets match group 1 with org-property-value ...
          (let ((facespec (cadr entry)))
            (expect (nth 0 facespec) :to-equal 1)
            (expect (nth 1 facespec) :to-equal ''org-property-value)
            ;; ... and the OVERRIDE flag is set.
            (expect (nth 2 facespec) :to-be-truthy)))
        ;; `font-lock-keywords' is buffer-local, so the keyword does not
        ;; leak into the global font-lock state.
        (expect (local-variable-p 'font-lock-keywords) :to-be-truthy)))

    (it "leaves chat-turn prose /emphasis/ intact — the override is scoped to property-drawer value spans only"
      (gptel-chat-drawer-test--with-fontified
          gptel-chat-drawer-test--drawer-plus-prose
        ;; The drawer value is de-emphasized ...
        (search-forward "/Users")
        (expect (gptel-chat-drawer-test--face-at-p
                 (- (point) (length "/Users")) 'italic)
                :not :to-be-truthy)
        ;; ... but chat-turn prose emphasis keeps its italic face.
        (goto-char (point-min))
        (search-forward "/emphasized/")
        (let ((emphasis-mid (- (point) (length "phasized/"))))
          (expect (gptel-chat-drawer-test--face-at-p emphasis-mid 'italic)
                  :to-be-truthy)
          ;; And it is NOT re-stamped as a property value — the matcher
          ;; must not have claimed this span.
          (expect (gptel-chat-drawer-test--face-at-p
                   emphasis-mid 'org-property-value)
                  :not :to-be-truthy))))))

;;; drawer-fontification-spec.el ends here
