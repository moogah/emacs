;;; resolve-module-path-spec.el --- Buttercup tests for jf/resolve-module-path -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Exercises `jf/resolve-module-path' against the three supported
;; MODULE-PATH shapes:
;;
;;   - zero separators  ("transient")
;;   - one separator    ("core/defaults")
;;   - two separators   ("gptel/chat/chat")
;;
;; All shapes resolve to an absolute .el path under `jf/emacs-dir'
;; with a fixed "config/" prefix.  The function is pure: it performs
;; only path construction, no filesystem I/O.

;;; Code:

(require 'buttercup)

;; The function under test is defined in init.el at the repo root.
;; `jf/emacs-dir' is also defined there.  Load init only if the
;; symbols are not already present so this spec works both under
;; the full test harness and in isolation.
(unless (fboundp 'jf/resolve-module-path)
  (let ((init-file (expand-file-name
                    "init.el"
                    (locate-dominating-file
                     (or load-file-name buffer-file-name default-directory)
                     "init.el"))))
    (load init-file nil t)))

(describe "jf/resolve-module-path"
  :var (original-emacs-dir)

  (before-each
    ;; Pin jf/emacs-dir to a known fixture so expected outputs are
    ;; deterministic regardless of where the test is invoked from.
    (setq original-emacs-dir jf/emacs-dir)
    (setq jf/emacs-dir "/tmp/fixture-emacs/"))

  (after-each
    (setq jf/emacs-dir original-emacs-dir))

  (it "resolves a single-segment path (e.g. \"transient\")"
    (expect (jf/resolve-module-path "transient")
            :to-equal
            "/tmp/fixture-emacs/config/transient.el"))

  (it "resolves a two-segment path (e.g. \"core/defaults\")"
    (expect (jf/resolve-module-path "core/defaults")
            :to-equal
            "/tmp/fixture-emacs/config/core/defaults.el"))

  (it "resolves a three-segment path (e.g. \"gptel/chat/chat\")"
    (expect (jf/resolve-module-path "gptel/chat/chat")
            :to-equal
            "/tmp/fixture-emacs/config/gptel/chat/chat.el")))

(provide 'resolve-module-path-spec)
;;; resolve-module-path-spec.el ends here
