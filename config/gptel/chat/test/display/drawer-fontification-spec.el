;;; drawer-fontification-spec.el --- Buttercup tests for drawer span emphasis suppression -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the property-drawer span fontification override
;; installed by `gptel-chat-mode' (config/gptel/chat/mode.org).
;;
;; Coverage (from openspec/changes/gptel-drawer-as-source-of-truth/design.md
;; §Addendum Finding A / Decision A; register entry
;; register/boundary/chat-mode-session-display override A; cycle-8
;; cross-line follow-up):
;;   - Single-line: a trailing-slash drawer value such as
;;     `:GPTEL_SCOPE_READ: /Users/jeff/emacs/' is a syntactically
;;     valid org `/emphasis/' span; without the override org renders
;;     it italic and dims the boundary `/' characters.  The override
;;     re-stamps the value with `org-property-value' (no italic) and
;;     forces `invisible' to nil so boundary characters remain visible.
;;   - Cross-line: when line N's drawer value opens an unmatched `/'
;;     (e.g. `:GPTEL_SCOPE_READ: /**') and a later drawer value
;;     contains a closing `/' (e.g. `:GPTEL_SCOPE_DENY: **/.git/**'),
;;     org's emphasis parser pairs them across lines — italicising
;;     the line N+1 KEY text (`:GPTEL_SCOPE_DENY:').  The span-level
;;     override covers the entire `:PROPERTIES: ... :END:' interior
;;     so the key text on the closing line is NOT italic and boundary
;;     `/' characters are NOT invisible.
;;   - Chat-turn prose `/emphasis/' is unaffected — the override is
;;     scoped to drawer interiors only (override-A counter-assertion).
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

(defconst gptel-chat-drawer-test--cross-line-scope-drawer
  (concat ":PROPERTIES:\n"
          ":GPTEL_SCOPE_READ: /**\n"
          ":GPTEL_SCOPE_DENY: **/.git/** **/runtime/** **/.env\n"
          ":END:\n")
  "A property drawer reproducing the cycle-7 user-testing finding (D).
The leading `/' in `:GPTEL_SCOPE_READ:''s value has no closing `/'
on its own line; org's emphasis parser pairs it with the first `/'
in `:GPTEL_SCOPE_DENY:''s value, italicising everything between —
including the next-line key `:GPTEL_SCOPE_DENY:' — and hiding the
boundary `/' characters under `org-hide-emphasis-markers'.")

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

    (it "renders a trailing-slash drawer value (e.g. :GPTEL_SCOPE_READ: /Users/jeff/emacs/) with face org-property-value, not org italic emphasis, and keeps the boundary `/' visible"
      (gptel-chat-drawer-test--with-fontified
          gptel-chat-drawer-test--scope-drawer
        ;; Locate the value text and probe both boundary slashes.
        (search-forward "/Users")
        (let ((open-slash (- (point) (length "/Users"))))
          (expect (gptel-chat-drawer-test--face-at-p
                   open-slash 'org-property-value)
                  :to-be-truthy)
          (expect (gptel-chat-drawer-test--face-at-p open-slash 'italic)
                  :not :to-be-truthy)
          ;; The opening `/' must not be hidden by
          ;; `org-hide-emphasis-markers'.
          (expect (get-text-property open-slash 'invisible)
                  :to-be nil))
        (search-forward "emacs/")
        (let ((close-slash (1- (point))))
          (expect (gptel-chat-drawer-test--face-at-p
                   close-slash 'org-property-value)
                  :to-be-truthy)
          (expect (gptel-chat-drawer-test--face-at-p close-slash 'italic)
                  :not :to-be-truthy)
          (expect (get-text-property close-slash 'invisible)
                  :to-be nil))))

    (it "covers the cross-line case — an unmatched `/' on one drawer value paired with a `/' on the next does NOT italicise the intervening key text, and the boundary `/' stays visible (cycle-7 user-testing finding D)"
      ;; The cycle-7 fix was per-line and could not reach this case;
      ;; the span-level override owns the entire drawer interior.
      (gptel-chat-drawer-test--with-fontified
          gptel-chat-drawer-test--cross-line-scope-drawer
        ;; 1. The next-line KEY `:GPTEL_SCOPE_DENY:' — caught in
        ;;    org's cross-line italic span — must NOT carry italic.
        (search-forward ":GPTEL_SCOPE_DENY:")
        (let* ((key-end (point))
               (key-start (- key-end (length ":GPTEL_SCOPE_DENY:")))
               ;; Probe the middle of the key text, not its boundary.
               (key-mid (+ key-start
                           (/ (length ":GPTEL_SCOPE_DENY:") 2))))
          (expect (gptel-chat-drawer-test--face-at-p key-mid 'italic)
                  :not :to-be-truthy)
          (expect (get-text-property key-mid 'invisible) :to-be nil))
        ;; 2. The boundary `/' characters inside `**/.git/**' must
        ;;    remain visible — they are the closing emphasis marker
        ;;    org would otherwise hide under `org-hide-emphasis-markers'.
        (search-forward "**/.git/**")
        (let* ((segment-end (point))
               (segment-start (- segment-end (length "**/.git/**"))))
          ;; Every position in `**/.git/**' must be visible (no
          ;; `invisible' text-property) — including the `/' boundaries.
          (cl-loop for pos from segment-start below segment-end do
                   (expect (get-text-property pos 'invisible)
                           :to-be nil))
          ;; And no position should be italic.
          (cl-loop for pos from segment-start below segment-end do
                   (expect (gptel-chat-drawer-test--face-at-p pos 'italic)
                           :not :to-be-truthy)))))

    (it "leaves chat-turn prose /emphasis/ intact — the override is scoped to property-drawer interiors only"
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
