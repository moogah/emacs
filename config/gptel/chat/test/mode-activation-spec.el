;;; mode-activation-spec.el --- Content-addressed gptel-chat-mode activation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for content-addressed activation of `gptel-chat-mode'
;; via `magic-mode-alist'.  See
;; `openspec/changes/gptel-content-addressed-session-activation/design.md'
;; §Decision D1 and the `chat-mode' requirement "Mode definition and
;; activation".
;;
;; `mode.el' registers a single `magic-mode-alist' entry
;; `(jf/gptel--session-signature-p . gptel-chat-mode)'.  `set-auto-mode'
;; consults `magic-mode-alist' BEFORE `auto-mode-alist', so a
;; drawer-bearing `.org' file selects `gptel-chat-mode' even though the
;; default `.org -> org-mode' mapping would otherwise win.
;;
;; These specs exercise the wiring end-to-end through
;; `find-file-noselect' against real temp-file fixtures so `set-auto-mode'
;; runs naturally:
;;
;;   1. A `.org' whose point-min drawer carries a `:GPTEL_' key opens in
;;      `gptel-chat-mode'.
;;   2. An ordinary `.org' file (no `:GPTEL_' drawer) stays `org-mode'.
;;   3. A `.org' file that only quotes `:GPTEL_PRESET:' in PROSE (not a
;;      real point-min drawer) stays `org-mode' — precedence plus the
;;      false-match guard, end-to-end
;;      (register/invariant/signature-anchored-to-point-min-drawer).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Add the chat source dir to `load-path' so `require' below resolves.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../" spec-dir)))
  (add-to-list 'load-path chat-dir))

;; `gptel-chat-mode' (the `magic-mode-alist' value) and the migration
;; pass it runs need the chat mode + parser modules.
(require 'gptel-chat-parser)
(require 'gptel-chat-mode)

;; The signature predicate (`magic-mode-alist' key) lives in the sessions
;; filesystem module, loaded by absolute path in production (it is not on
;; `load-path').  Load it here the same way so the predicate symbol is
;; bound when `set-auto-mode' consults the alist.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (filesystem-el (expand-file-name
                       "../../sessions/filesystem.el" spec-dir)))
  (load filesystem-el nil t t))

;;; ---------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------

(defmacro gptel-chat-activation-test--with-org-file (content &rest body)
  "Write CONTENT to a temp `.org' file, open it, run BODY, then clean up.

A real temp file with the `.org' extension is used so `set-auto-mode'
runs through its full precedence chain (`magic-mode-alist' before
`auto-mode-alist') exactly as it does for a user file open.  BODY runs
with the opened buffer current; the buffer is killed and the file
deleted afterward, even on error.

Binds `magic-mode-alist' to the live value so the registration under
test is in effect, and binds `enable-local-variables' nil so a stray
file-local cookie in CONTENT cannot influence mode selection."
  (declare (indent 1) (debug (form body)))
  `(let* ((file (make-temp-file "gptel-chat-activation-" nil ".org"))
          (enable-local-variables nil)
          (buffer nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (setq buffer (find-file-noselect file))
           (with-current-buffer buffer ,@body))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (when (file-exists-p file) (delete-file file)))))

;;; ---------------------------------------------------------------
;;; Specs
;;; ---------------------------------------------------------------

(describe "Content-addressed gptel-chat-mode activation (magic-mode-alist)"

  (it "registers the signature predicate on magic-mode-alist"
    (expect (rassq 'gptel-chat-mode magic-mode-alist) :to-equal
            (cons #'jf/gptel--session-signature-p #'gptel-chat-mode)))

  (it "opens a drawer-bearing .org in gptel-chat-mode"
    (gptel-chat-activation-test--with-org-file
        (concat ":PROPERTIES:\n"
                ":GPTEL_SESSION_ID: abc123\n"
                ":GPTEL_PRESET: default\n"
                ":END:\n"
                "\n"
                "#+begin_user\n"
                "  hello\n"
                "#+end_user\n")
      (expect major-mode :to-be 'gptel-chat-mode)))

  (it "leaves an ordinary .org file in org-mode"
    (gptel-chat-activation-test--with-org-file
        (concat "#+title: Ordinary notes\n"
                "\n"
                "* A heading\n"
                "Some prose with no session drawer.\n")
      (expect major-mode :to-be 'org-mode)))

  (it "leaves an .org file that only quotes :GPTEL_PRESET: in prose in org-mode"
    ;; Precedence + false-match guard, end-to-end: the key appears only
    ;; in body prose / a src block, never in a real point-min drawer, so
    ;; the predicate must NOT match and the default .org -> org-mode
    ;; mapping must win
    ;; (register/invariant/signature-anchored-to-point-min-drawer).
    (gptel-chat-activation-test--with-org-file
        (concat "#+title: Notes about gptel\n"
                "\n"
                "* How activation works\n"
                "The drawer carries a :GPTEL_PRESET: key, for example:\n"
                "\n"
                "#+begin_src org\n"
                ":PROPERTIES:\n"
                ":GPTEL_PRESET: default\n"
                ":END:\n"
                "#+end_src\n")
      (expect major-mode :to-be 'org-mode))))

(provide 'mode-activation-spec)

;;; mode-activation-spec.el ends here
